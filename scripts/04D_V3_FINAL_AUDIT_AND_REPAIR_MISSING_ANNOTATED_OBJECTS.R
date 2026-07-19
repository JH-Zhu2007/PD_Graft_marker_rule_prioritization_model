# ============================================================
# 04D_V3_FINAL_AUDIT_AND_REPAIR_MISSING_ANNOTATED_OBJECTS.R
# ============================================================
# 目的：
#   接在 04D V2 后运行。
#
#   04D V2 总体完成，但日志中出现一个对象中途失败：
#     GSE233885 :: GSM7438587...
#     无法改变被锁定的联编 'classi' 的值
#
#   同时最终计数：
#     manifest 54
#     annotated 52
#     unassigned 1
#   合计 53，说明还有 1 个对象没有写入最终 manifest。
#
#   04D V3 只做最终 audit + repair：
#     1. 读取 03C main manifest
#     2. 读取 04D group_annotation_table
#     3. 读取已有 04D annotated manifest
#     4. 找出缺失对象
#     5. 用更保守的 metadata data.frame 写法修复缺失对象
#     6. 重新输出完整 manifest / summary / audit report
#
# 成功标志：
#   ✅ 04D V3 final audit and repair 完成。
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"
SAVE_RDS_COMPRESS <- FALSE


# ============================================================
# 1. 加载包
# ============================================================

cat("\n============================================================\n")
cat("04D V3：final audit and repair missing annotated objects\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 04D V3。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
  library(data.table)
})


# ============================================================
# 2. 路径
# ============================================================

tables_dir <- file.path(PROJECT_DIR, "03_tables")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_main_manifest <- file.path(tables_dir, "03C_strategy", "03C_main_analysis_object_manifest.csv")
input_group_annotation <- file.path(tables_dir, "04D_annotations", "04D_group_annotation_table.csv")
input_object_summary <- file.path(tables_dir, "04D_annotations", "04D_object_annotation_summary.csv")
input_annotated_manifest <- file.path(tables_dir, "04D_annotations", "04D_annotated_object_manifest.csv")
input_failed <- file.path(tables_dir, "04D_annotations", "04D_failed_objects.csv")

out_tables_dir <- file.path(tables_dir, "04D_annotations")
out_objects_dir <- file.path(objects_dir, "04D_annotated_objects")

object_summary_csv <- input_object_summary
annotated_manifest_csv <- input_annotated_manifest
failed_objects_csv <- input_failed
final_audit_csv <- file.path(out_tables_dir, "04D_V3_final_audit_summary.csv")
report_txt <- file.path(reports_dir, "04D_V3_final_audit_and_repair_report.txt")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 3. 工具函数
# ============================================================

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

safe_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "/", x)
  x <- basename(x)
  x <- gsub("\\.rds$|\\.csv$|\\.tsv$|\\.txt$", "", x, ignore.case = TRUE)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  ifelse(nchar(x) == 0, "unnamed", x)
}

extract_group_column_from_source <- function(group_source) {
  group_source <- as.character(group_source)

  if (is.na(group_source) || group_source == "") return(NA_character_)
  if (group_source == "object_all_no_cluster") return("object_all_no_cluster")

  group_source <- sub("^03A_", "", group_source)
  group_source <- sub("^expression_", "", group_source)

  group_source
}

ensure_annotation_columns <- function(meta) {
  if (!"annotation_04D_v1" %in% colnames(meta)) meta[["annotation_04D_v1"]] <- "unassigned_no_04B_group"
  if (!"annotation_04D_confidence" %in% colnames(meta)) meta[["annotation_04D_confidence"]] <- "none"
  if (!"annotation_04D_subtype_bias" %in% colnames(meta)) meta[["annotation_04D_subtype_bias"]] <- NA_character_
  if (!"annotation_04D_caution" %in% colnames(meta)) meta[["annotation_04D_caution"]] <- "No 04B group-level annotation available."
  if (!"annotation_04D_group_source" %in% colnames(meta)) meta[["annotation_04D_group_source"]] <- NA_character_
  if (!"annotation_04D_group_id" %in% colnames(meta)) meta[["annotation_04D_group_id"]] <- NA_character_
  if (!"annotation_04D_DA_maturation_combined" %in% colnames(meta)) meta[["annotation_04D_DA_maturation_combined"]] <- NA_real_
  if (!"annotation_04D_safety_risk_combined" %in% colnames(meta)) meta[["annotation_04D_safety_risk_combined"]] <- NA_real_
  if (!"annotation_04D_A9_minus_A10_score" %in% colnames(meta)) meta[["annotation_04D_A9_minus_A10_score"]] <- NA_real_
  meta
}

repair_one_object <- function(row, group_ann) {
  ds <- row$dataset
  oid <- row$object_id
  in_rds <- row$initial_pca_object

  out_dir <- file.path(out_objects_dir, ds)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_rds <- file.path(out_dir, paste0(safe_name(oid), "_04D_annotated.rds"))

  obj <- readRDS(in_rds)

  meta <- as.data.frame(obj@meta.data, stringsAsFactors = FALSE)
  meta <- ensure_annotation_columns(meta)

  sub_ann <- as.data.table(group_ann)[dataset == ds & object_id == oid]

  n_groups_written <- 0L

  if (nrow(sub_ann) == 0L) {
    meta[["annotation_04D_v1"]] <- "unassigned_no_04B_group"
    meta[["annotation_04D_confidence"]] <- "none"
    meta[["annotation_04D_caution"]] <- "No 04B group-level annotation available."
    meta[["annotation_04D_group_source"]] <- "no_04B_group"
    meta[["annotation_04D_group_id"]] <- "no_04B_group"
    status <- "REPAIRED_SAVED_UNASSIGNED_NO_04B"
  } else {
    for (j in seq_len(nrow(sub_ann))) {
      gs <- sub_ann$group_source[[j]]
      gid <- as.character(sub_ann$group_id[[j]])

      group_col <- extract_group_column_from_source(gs)

      if (is.na(group_col)) next

      if (group_col == "object_all_no_cluster") {
        cells_use <- rownames(meta)
      } else if (group_col %in% colnames(meta)) {
        cells_use <- rownames(meta)[as.character(meta[[group_col]]) == gid]
      } else if ("seurat_clusters" %in% colnames(meta)) {
        cells_use <- rownames(meta)[as.character(meta[["seurat_clusters"]]) == gid]
      } else {
        cells_use <- character()
      }

      if (length(cells_use) == 0L) next

      meta[cells_use, "annotation_04D_v1"] <- sub_ann$annotation_v1_conservative[[j]]
      meta[cells_use, "annotation_04D_confidence"] <- sub_ann$annotation_v1_confidence[[j]]
      meta[cells_use, "annotation_04D_subtype_bias"] <- sub_ann$annotation_v1_subtype_bias[[j]]
      meta[cells_use, "annotation_04D_caution"] <- sub_ann$annotation_v1_caution[[j]]
      meta[cells_use, "annotation_04D_group_source"] <- gs
      meta[cells_use, "annotation_04D_group_id"] <- gid
      meta[cells_use, "annotation_04D_DA_maturation_combined"] <- sub_ann$DA_maturation_combined[[j]]
      meta[cells_use, "annotation_04D_safety_risk_combined"] <- sub_ann$safety_risk_combined[[j]]
      meta[cells_use, "annotation_04D_A9_minus_A10_score"] <- sub_ann$A9_minus_A10_score[[j]]

      n_groups_written <- n_groups_written + 1L
    }

    status <- "REPAIRED_SUCCESS_ANNOTATED"
  }

  obj@meta.data <- meta

  ann_tab <- as.data.table(meta)[
    ,
    .N,
    by = annotation_04D_v1
  ][order(-N)]

  dominant_annotation <- if (nrow(ann_tab) > 0L) ann_tab$annotation_04D_v1[[1L]] else NA_character_
  dominant_n <- if (nrow(ann_tab) > 0L) ann_tab$N[[1L]] else NA_integer_

  saveRDS(obj, out_rds, compress = SAVE_RDS_COMPRESS)

  list(
    object_summary = data.frame(
      dataset = ds,
      object_id = oid,
      n_cells = ncol(obj),
      n_groups_available = nrow(sub_ann),
      n_groups_written = n_groups_written,
      dominant_annotation = dominant_annotation,
      dominant_annotation_cells = dominant_n,
      status = status,
      stringsAsFactors = FALSE
    ),
    manifest = data.frame(
      dataset = ds,
      object_id = oid,
      input_rds = in_rds,
      annotated_rds = out_rds,
      status = status,
      stringsAsFactors = FALSE
    )
  )
}


# ============================================================
# 4. 读取现有输出并找缺失
# ============================================================

stamp("读取 03C / 04D 输出。")

main_manifest <- read_csv_required(input_main_manifest)
group_ann <- read_csv_required(input_group_annotation)
object_summary <- read_csv_optional(input_object_summary)
annotated_manifest <- read_csv_optional(input_annotated_manifest)
failed_objects <- read_csv_optional(input_failed)

if (!all(c("dataset", "object_id", "initial_pca_object") %in% colnames(main_manifest))) {
  stop("03C main manifest 缺少 dataset/object_id/initial_pca_object。")
}

main_manifest <- main_manifest[file.exists(main_manifest$initial_pca_object), , drop = FALSE]

main_keys <- paste(main_manifest$dataset, main_manifest$object_id, sep = "||")

if (nrow(annotated_manifest) > 0 && all(c("dataset", "object_id", "annotated_rds") %in% colnames(annotated_manifest))) {
  ok_manifest <- annotated_manifest[
    file.exists(annotated_manifest$annotated_rds),
    ,
    drop = FALSE
  ]
  done_keys <- paste(ok_manifest$dataset, ok_manifest$object_id, sep = "||")
} else {
  done_keys <- character()
}

missing_keys <- setdiff(main_keys, done_keys)

stamp("main manifest objects：", length(main_keys))
stamp("已有 annotated rds objects：", length(done_keys))
stamp("缺失 objects：", length(missing_keys))

missing_rows <- main_manifest[main_keys %in% missing_keys, , drop = FALSE]


# ============================================================
# 5. 修复缺失对象
# ============================================================

repair_summary_list <- list()
repair_manifest_list <- list()
repair_failed_list <- list()

if (nrow(missing_rows) > 0L) {
  for (i in seq_len(nrow(missing_rows))) {
    ds <- missing_rows$dataset[[i]]
    oid <- missing_rows$object_id[[i]]

    stamp("修复缺失对象 ", i, " / ", nrow(missing_rows), "：", ds, " :: ", oid)

    res <- tryCatch({
      repair_one_object(missing_rows[i, , drop = FALSE], group_ann)
    }, error = function(e) {
      msg <- conditionMessage(e)
      stamp("  修复失败：", msg)

      repair_failed_list[[length(repair_failed_list) + 1L]] <<- data.frame(
        dataset = ds,
        object_id = oid,
        input_rds = missing_rows$initial_pca_object[[i]],
        stage = "repair_missing_object",
        message = msg,
        stringsAsFactors = FALSE
      )

      NULL
    })

    if (!is.null(res)) {
      repair_summary_list[[length(repair_summary_list) + 1L]] <- res$object_summary
      repair_manifest_list[[length(repair_manifest_list) + 1L]] <- res$manifest
    }
  }
}

repair_summary <- if (length(repair_summary_list) > 0L) rbindlist(repair_summary_list, fill = TRUE) else data.frame()
repair_manifest <- if (length(repair_manifest_list) > 0L) rbindlist(repair_manifest_list, fill = TRUE) else data.frame()
repair_failed <- if (length(repair_failed_list) > 0L) rbindlist(repair_failed_list, fill = TRUE) else data.frame()


# ============================================================
# 6. 合并并重写 manifest / summary / failed
# ============================================================

if (nrow(object_summary) > 0 && nrow(repair_summary) > 0) {
  existing_keys_summary <- paste(object_summary$dataset, object_summary$object_id, sep = "||")
  repair_keys_summary <- paste(repair_summary$dataset, repair_summary$object_id, sep = "||")
  object_summary <- object_summary[!existing_keys_summary %in% repair_keys_summary, , drop = FALSE]
  object_summary <- rbindlist(list(object_summary, repair_summary), fill = TRUE)
} else if (nrow(repair_summary) > 0) {
  object_summary <- repair_summary
}

if (nrow(annotated_manifest) > 0 && nrow(repair_manifest) > 0) {
  existing_keys_manifest <- paste(annotated_manifest$dataset, annotated_manifest$object_id, sep = "||")
  repair_keys_manifest <- paste(repair_manifest$dataset, repair_manifest$object_id, sep = "||")
  annotated_manifest <- annotated_manifest[!existing_keys_manifest %in% repair_keys_manifest, , drop = FALSE]
  annotated_manifest <- rbindlist(list(annotated_manifest, repair_manifest), fill = TRUE)
} else if (nrow(repair_manifest) > 0) {
  annotated_manifest <- repair_manifest
}

if (nrow(failed_objects) > 0 && nrow(repair_manifest) > 0 && all(c("dataset", "object_id") %in% colnames(failed_objects))) {
  repair_keys_manifest <- paste(repair_manifest$dataset, repair_manifest$object_id, sep = "||")
  failed_keys <- paste(failed_objects$dataset, failed_objects$object_id, sep = "||")
  failed_objects <- failed_objects[!failed_keys %in% repair_keys_manifest, , drop = FALSE]
}

if (nrow(repair_failed) > 0) {
  failed_objects <- rbindlist(list(failed_objects, repair_failed), fill = TRUE)
}

atomic_write_csv(as.data.frame(object_summary), object_summary_csv)
atomic_write_csv(as.data.frame(annotated_manifest), annotated_manifest_csv)
atomic_write_csv(as.data.frame(failed_objects), failed_objects_csv)


# ============================================================
# 7. 最终 audit
# ============================================================

if (nrow(annotated_manifest) > 0 && all(c("dataset", "object_id", "annotated_rds") %in% colnames(annotated_manifest))) {
  annotated_manifest$file_exists <- file.exists(annotated_manifest$annotated_rds)
  final_done_keys <- paste(
    annotated_manifest$dataset[annotated_manifest$file_exists],
    annotated_manifest$object_id[annotated_manifest$file_exists],
    sep = "||"
  )
} else {
  final_done_keys <- character()
}

final_missing_keys <- setdiff(main_keys, final_done_keys)

final_audit <- data.frame(
  metric = c(
    "main_manifest_objects",
    "annotated_rds_existing_objects",
    "missing_objects_after_repair",
    "objects_repaired_now",
    "objects_repair_failed_now",
    "failed_table_rows_after_repair"
  ),
  value = c(
    length(main_keys),
    length(final_done_keys),
    length(final_missing_keys),
    nrow(repair_manifest),
    nrow(repair_failed),
    nrow(failed_objects)
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(final_audit, final_audit_csv)


# ============================================================
# 8. 报告
# ============================================================

missing_lines <- if (length(final_missing_keys) > 0) final_missing_keys else "none"

repair_lines <- if (nrow(repair_manifest) > 0) {
  paste0(repair_manifest$dataset, " :: ", repair_manifest$object_id, " — ", repair_manifest$status)
} else {
  "none"
}

failed_lines <- if (nrow(repair_failed) > 0) {
  paste0(repair_failed$dataset, " :: ", repair_failed$object_id, " — ", repair_failed$message)
} else {
  "none"
}

report_lines <- c(
  "04D V3 final audit and repair report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Audit before repair:",
  paste0("Main manifest objects: ", length(main_keys)),
  paste0("Existing annotated RDS objects before repair: ", length(done_keys)),
  paste0("Missing objects before repair: ", length(missing_keys)),
  "",
  "Repair results:",
  repair_lines,
  "",
  "Repair failures:",
  failed_lines,
  "",
  "Final audit:",
  paste0("Annotated RDS existing objects after repair: ", length(final_done_keys)),
  paste0("Missing objects after repair: ", length(final_missing_keys)),
  "Missing keys:",
  missing_lines,
  "",
  "Output files:",
  paste0("Object summary: ", object_summary_csv),
  paste0("Annotated manifest: ", annotated_manifest_csv),
  paste0("Failed objects: ", failed_objects_csv),
  paste0("Final audit summary: ", final_audit_csv),
  "",
  "Next step:",
  "05A_DA_A9_A10_AND_PROJECTION_COMPETENCE_SCORING.R"
)

writeLines(report_lines, report_txt)


# ============================================================
# 9. 结束
# ============================================================

cat("\n============================================================\n")
cat("04D V3 final audit and repair 运行结束\n")
cat("============================================================\n\n")

cat("Main manifest objects：", length(main_keys), "\n")
cat("Annotated RDS existing objects after repair：", length(final_done_keys), "\n")
cat("Missing objects after repair：", length(final_missing_keys), "\n")
cat("Objects repaired now：", nrow(repair_manifest), "\n")
cat("Repair failed now：", nrow(repair_failed), "\n\n")

cat("输出文件：\n")
cat(object_summary_csv, "\n")
cat(annotated_manifest_csv, "\n")
cat(failed_objects_csv, "\n")
cat(final_audit_csv, "\n")
cat(report_txt, "\n\n")

if (length(final_missing_keys) == 0L && nrow(repair_failed) == 0L) {
  cat("✅ 04D V3 final audit and repair 完成。\n")
  cat("现在 04D annotated manifest 已完整，可以进入 05A。\n")
} else {
  cat("⚠️ 04D V3 完成，但仍有缺失/失败对象。请查看 04D_V3_final_audit_summary.csv。\n")
}
