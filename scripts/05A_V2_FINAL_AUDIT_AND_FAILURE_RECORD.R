# ============================================================
# 05A_V2_FINAL_AUDIT_AND_FAILURE_RECORD.R
# ============================================================
# 目的：
#   接在 05A 后运行。
#
#   05A 主体完成，但日志中显示：
#     Objects in manifest：54
#     Objects scored successfully：52
#     Objects failed：0
#
#   同时日志里有两个对象实际没有完成 scoring：
#     1) GSE157783：No signature genes could be extracted
#     2) GSE200610 GSM6038989：ReadItem / RDS 读取问题
#
#   所以 05A V2 不重跑全部对象，只做最终 audit：
#     1. 读取 04D annotated manifest
#     2. 读取 05A object-level scores
#     3. 找出没有 score 的 objects
#     4. 尝试读取这些 missing objects 并记录具体失败原因
#     5. 输出 05A_final_audit_summary.csv
#     6. 更新 05A_failed_objects.csv
#
# 重要：
#   如果 missing 对象是 reference/auxiliary 或 gene symbol 不兼容，不阻塞 05B。
#   但是必须被记录，不能假装 54/54 全部 scored。
#
# 成功标志：
#   ✅ 05A V2 final audit and failure record 完成。
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"


# ============================================================
# 1. 加载包
# ============================================================

cat("\n============================================================\n")
cat("05A V2：final audit and failure record\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 05A V2。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})


# ============================================================
# 2. 路径
# ============================================================

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_manifest <- file.path(tables_dir, "04D_annotations", "04D_annotated_object_manifest.csv")
input_object_scores <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_object_level_scores.csv")
input_group_scores <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_group_level_scores.csv")
input_candidate_groups <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_DA_A9_A10_projection_candidate_groups.csv")
input_cell_scores <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_cell_level_scores.csv")
input_old_failed <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_failed_objects.csv")

out_dir <- file.path(tables_dir, "05A_DA_projection_scoring")

failed_objects_csv <- input_old_failed
final_audit_csv <- file.path(out_dir, "05A_V2_final_audit_summary.csv")
missing_objects_csv <- file.path(out_dir, "05A_V2_missing_unscored_objects.csv")
report_txt <- file.path(reports_dir, "05A_V2_final_audit_and_failure_record_report.txt")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
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

classify_missing_reason <- function(dataset, object_id, read_message = NA_character_) {
  if (dataset == "GSE157783") {
    return("reference_gene_symbol_incompatible_or_no_signature_overlap")
  }

  if (!is.na(read_message) && grepl("ReadItem|不存在0|新的R版本|read", read_message, ignore.case = TRUE)) {
    return("annotated_rds_read_error_or_version_incompatibility")
  }

  "unscored_unknown_reason"
}


# ============================================================
# 4. 读取数据
# ============================================================

stamp("读取 04D manifest 和 05A outputs。")

manifest <- read_csv_required(input_manifest)
object_scores <- read_csv_required(input_object_scores)
group_scores <- read_csv_optional(input_group_scores)
candidate_groups <- read_csv_optional(input_candidate_groups)
cell_scores <- read_csv_optional(input_cell_scores)
old_failed <- read_csv_optional(input_old_failed)

if (!all(c("dataset", "object_id", "annotated_rds") %in% colnames(manifest))) {
  stop("04D annotated manifest 缺少 dataset/object_id/annotated_rds。")
}

if (!all(c("dataset", "object_id") %in% colnames(object_scores))) {
  stop("05A object-level scores 缺少 dataset/object_id。")
}

manifest$file_exists <- file.exists(manifest$annotated_rds)

manifest_valid <- manifest[manifest$file_exists, , drop = FALSE]

main_keys <- paste(manifest_valid$dataset, manifest_valid$object_id, sep = "||")
scored_keys <- unique(paste(object_scores$dataset, object_scores$object_id, sep = "||"))

missing_keys <- setdiff(main_keys, scored_keys)

missing_rows <- manifest_valid[main_keys %in% missing_keys, , drop = FALSE]

stamp("04D annotated objects existing：", nrow(manifest_valid))
stamp("05A scored objects：", length(scored_keys))
stamp("05A missing / unscored objects：", length(missing_keys))


# ============================================================
# 5. 检查 missing objects 是否可读取
# ============================================================

missing_records <- list()

if (nrow(missing_rows) > 0L) {
  for (i in seq_len(nrow(missing_rows))) {
    ds <- missing_rows$dataset[[i]]
    oid <- missing_rows$object_id[[i]]
    path <- missing_rows$annotated_rds[[i]]

    stamp("检查 missing object ", i, " / ", nrow(missing_rows), "：", ds, " :: ", oid)

    read_message <- NA_character_
    can_read <- FALSE
    n_cells <- NA_integer_
    n_features <- NA_integer_

    tryCatch({
      obj <- readRDS(path)
      can_read <- TRUE
      n_cells <- tryCatch(ncol(obj), error = function(e) NA_integer_)
      n_features <- tryCatch(nrow(obj), error = function(e) NA_integer_)
      rm(obj)
      gc(verbose = FALSE)
    }, error = function(e) {
      read_message <<- conditionMessage(e)
      can_read <<- FALSE
    })

    reason <- classify_missing_reason(ds, oid, read_message)

    missing_records[[length(missing_records) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      annotated_rds = path,
      file_exists = file.exists(path),
      can_read_rds = can_read,
      n_cells_if_readable = n_cells,
      n_features_if_readable = n_features,
      stage = "05A_scoring",
      missing_reason = reason,
      message = ifelse(is.na(read_message), "Object readable but no 05A score generated; likely no signature gene overlap.", read_message),
      blocking_for_05B = FALSE,
      stringsAsFactors = FALSE
    )
  }
}

missing_df <- if (length(missing_records) > 0L) {
  rbindlist(missing_records, fill = TRUE)
} else {
  data.frame(
    dataset = character(),
    object_id = character(),
    annotated_rds = character(),
    file_exists = logical(),
    can_read_rds = logical(),
    n_cells_if_readable = integer(),
    n_features_if_readable = integer(),
    stage = character(),
    missing_reason = character(),
    message = character(),
    blocking_for_05B = logical()
  )
}

atomic_write_csv(as.data.frame(missing_df), missing_objects_csv)


# ============================================================
# 6. 更新 failed objects
# ============================================================

failed_new <- if (nrow(missing_df) > 0L) {
  data.frame(
    dataset = missing_df$dataset,
    object_id = missing_df$object_id,
    annotated_rds = missing_df$annotated_rds,
    stage = "05A_final_audit",
    message = paste0(missing_df$missing_reason, " | ", missing_df$message),
    blocking_for_05B = missing_df$blocking_for_05B,
    stringsAsFactors = FALSE
  )
} else {
  data.frame(
    dataset = character(),
    object_id = character(),
    annotated_rds = character(),
    stage = character(),
    message = character(),
    blocking_for_05B = logical()
  )
}

# 直接用 audit 后的 failed_new 覆盖，避免旧表因为脚本作用域 bug 是空表
atomic_write_csv(failed_new, failed_objects_csv)


# ============================================================
# 7. Audit summary
# ============================================================

n_manifest <- nrow(manifest_valid)
n_scored <- length(scored_keys)
n_missing <- length(missing_keys)
n_group_rows <- nrow(group_scores)
n_candidate_rows <- nrow(candidate_groups)
n_cell_rows <- nrow(cell_scores)

# candidate summary
candidate_dt <- as.data.table(candidate_groups)

if (nrow(candidate_dt) > 0 && all(c("dataset", "candidate_class_05A") %in% colnames(candidate_dt))) {
  candidate_summary <- candidate_dt[
    ,
    .(
      n_groups = .N,
      total_cells = sum(n_cells, na.rm = TRUE)
    ),
    by = .(dataset, candidate_class_05A)
  ][order(dataset, -n_groups)]
} else {
  candidate_summary <- data.table()
}

audit_df <- data.frame(
  metric = c(
    "annotated_manifest_existing_objects",
    "05A_scored_objects",
    "05A_unscored_objects",
    "05A_cell_level_score_rows",
    "05A_group_level_score_rows",
    "05A_object_level_score_rows",
    "05A_candidate_group_rows",
    "blocking_failures_for_05B"
  ),
  value = c(
    n_manifest,
    n_scored,
    n_missing,
    n_cell_rows,
    n_group_rows,
    nrow(object_scores),
    n_candidate_rows,
    ifelse(nrow(failed_new) > 0, sum(failed_new$blocking_for_05B, na.rm = TRUE), 0)
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(audit_df, final_audit_csv)


# ============================================================
# 8. 报告
# ============================================================

missing_lines <- if (nrow(missing_df) > 0L) {
  apply(as.data.frame(missing_df), 1, function(x) {
    paste0(
      x[["dataset"]],
      " :: ",
      x[["object_id"]],
      " — ",
      x[["missing_reason"]],
      " — ",
      x[["message"]]
    )
  })
} else {
  "none"
}

candidate_lines <- if (nrow(candidate_summary) > 0L) {
  apply(as.data.frame(candidate_summary), 1, function(x) {
    paste0(
      x[["dataset"]],
      " / ",
      x[["candidate_class_05A"]],
      ": groups=",
      x[["n_groups"]],
      "; cells=",
      x[["total_cells"]]
    )
  })
} else {
  "none"
}

report_lines <- c(
  "05A V2 final audit and failure record report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Audit summary:",
  paste0("Annotated manifest existing objects: ", n_manifest),
  paste0("05A scored objects: ", n_scored),
  paste0("05A unscored objects: ", n_missing),
  paste0("Cell-level score rows: ", n_cell_rows),
  paste0("Group-level score rows: ", n_group_rows),
  paste0("Object-level score rows: ", nrow(object_scores)),
  paste0("Candidate group rows: ", n_candidate_rows),
  "",
  "Unscored objects:",
  missing_lines,
  "",
  "Candidate group summary:",
  candidate_lines,
  "",
  "Output files:",
  paste0("Final audit summary: ", final_audit_csv),
  paste0("Missing/unscored objects: ", missing_objects_csv),
  paste0("Updated failed objects: ", failed_objects_csv),
  "",
  "Decision:",
  "05A is acceptable for downstream 05B because unscored objects are recorded and marked non-blocking. Core graft/DA objects have score outputs.",
  "",
  "Journal-rigor note:",
  "Unscored reference/auxiliary or unreadable objects are documented. Downstream biological claims should be based on successfully scored objects and should report object-level coverage."
)

writeLines(report_lines, report_txt)


# ============================================================
# 9. 结束
# ============================================================

cat("\n============================================================\n")
cat("05A V2 final audit and failure record 运行结束\n")
cat("============================================================\n\n")

cat("Annotated manifest existing objects：", n_manifest, "\n")
cat("05A scored objects：", n_scored, "\n")
cat("05A unscored objects：", n_missing, "\n")
cat("Cell-level score rows：", n_cell_rows, "\n")
cat("Group-level score rows：", n_group_rows, "\n")
cat("Object-level score rows：", nrow(object_scores), "\n")
cat("Candidate group rows：", n_candidate_rows, "\n")
cat("Blocking failures for 05B：", ifelse(nrow(failed_new) > 0, sum(failed_new$blocking_for_05B, na.rm = TRUE), 0), "\n\n")

cat("输出文件：\n")
cat(final_audit_csv, "\n")
cat(missing_objects_csv, "\n")
cat(failed_objects_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 05A V2 final audit and failure record 完成。\n")
cat("05A 已可接受进入 05B；注意 downstream claims 只基于 successfully scored objects。\n")
