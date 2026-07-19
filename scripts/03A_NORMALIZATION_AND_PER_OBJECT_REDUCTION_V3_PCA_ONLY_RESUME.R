# ============================================================
# 03A_NORMALIZATION_AND_PER_OBJECT_REDUCTION_V3_PCA_ONLY_RESUME.R
# ============================================================
# 目的：
#   03A 安全续跑版，专门解决 Windows/RStudio 跑到一半 abort / 卡死的问题。
#
#   和 V1 的主要区别：
#     1. REBUILD_EXISTING = FALSE：已经成功保存的对象直接跳过，不从头重跑
#     2. SAVE_BASIC_PLOTS = FALSE：先不画图，避免 ggplot/DimPlot 引起 RStudio abort
#     3. 每个对象单独 tryCatch，失败不中断
#     4. 降低 PCA/UMAP dims，减少内存压力
#     5. V3 对所有未完成对象只做 Normalize/HVG/Scale/PCA，彻底跳过 UMAP/cluster
#     6. 每个对象结束后强制 gc()
#
# 成功标志：
#   ✅ 03A V3 PCA-only resume 完成。
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"

# 关键：安全续跑，不重跑已经成功的对象
REBUILD_EXISTING <- FALSE

# 关键：先不画图，避免 plot 触发 abort
SAVE_BASIC_PLOTS <- FALSE

# 降低内存压力
N_VARIABLE_FEATURES <- 2000L
MAX_NPCS <- 20L
MAX_UMAP_DIMS <- 20L
CLUSTER_RESOLUTION <- 0.5

MIN_CELLS_FOR_REDUCTION <- 50L
MIN_FEATURES_FOR_REDUCTION <- 200L

SAVE_RDS_COMPRESS <- FALSE

# V3 关键：所有未完成对象都只做到 PCA，不再跑 UMAP / FindNeighbors / FindClusters。
# 你的 RStudio abort 就发生在 RunUMAP / FindNeighbors / FindClusters 附近。
# 03A 的核心目标是完成 Normalize/HVG/Scale/PCA，为 03B/03C 做准备；
# UMAP/cluster 后面可以在合并/整合对象上统一做，不必在这里硬跑。
FORCE_PCA_ONLY_ALL_OBJECTS <- TRUE

# PCA-only 安全模式保留，但 V3 已经所有对象 PCA-only。
SAFE_LARGE_OBJECT_MODE <- TRUE
LARGE_OBJECT_CELL_CUTOFF <- 0L

# 让 R 尽量别用多线程/并行引发内存暴涨
Sys.setenv(
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1"
)

options(future.globals.maxSize = 8 * 1024^3)
options(expressions = 5e5)

if (requireNamespace("future", quietly = TRUE)) {
  future::plan("sequential")
}


# ============================================================
# 1. 加载包
# ============================================================

cat("\n============================================================\n")
cat("03A V3 PCA-only resume：normalization and per-object reduction\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "Matrix", "data.table", "ggplot2")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 03A V2。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
  library(Matrix)
  library(data.table)
  library(ggplot2)
})


# ============================================================
# 2. 路径
# ============================================================

metadata_dir <- file.path(PROJECT_DIR, "01_metadata")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_manifest_csv <- file.path(tables_dir, "02B_qc", "02B_filtered_object_manifest.csv")

out_objects_dir <- file.path(objects_dir, "03A_normalized_reduced")
out_tables_dir <- file.path(tables_dir, "03A_reduction")
out_figures_dir <- file.path(figures_dir, "03A_reduction")

dir.create(out_objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

status_csv <- file.path(metadata_dir, "03A_reduction_status.csv")
object_summary_csv <- file.path(out_tables_dir, "03A_object_reduction_summary.csv")
reduced_manifest_csv <- file.path(out_tables_dir, "03A_reduced_object_manifest.csv")
report_txt <- file.path(reports_dir, "03A_normalization_and_per_object_reduction_report.txt")


# ============================================================
# 3. 工具函数
# ============================================================

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
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

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }

  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path, "\n请先确认 02B 已成功完成。")
  }
  data.table::fread(path, data.table = FALSE)
}

object_is_readable <- function(path) {
  tryCatch({
    readRDS(path)
    TRUE
  }, error = function(e) FALSE)
}

get_assay_for_analysis <- function(obj) {
  assays <- names(obj@assays)

  if ("RNA" %in% assays) return("RNA")

  da <- tryCatch({
    SeuratObject::DefaultAssay(obj)
  }, error = function(e) NA_character_)

  if (!is.na(da) && da %in% assays) return(da)

  if (length(assays) > 0L) return(assays[[1L]])

  NA_character_
}

get_counts_matrix <- function(obj, assay) {
  mat <- tryCatch({
    SeuratObject::GetAssayData(obj, assay = assay, layer = "counts")
  }, error = function(e1) {
    tryCatch({
      SeuratObject::GetAssayData(obj, assay = assay, slot = "counts")
    }, error = function(e2) NULL)
  })

  if (is.null(mat)) stop("无法读取 counts matrix。")
  mat
}

choose_npcs <- function(n_cells, n_features) {
  npcs <- min(MAX_NPCS, n_cells - 1L, n_features - 1L)
  if (!is.finite(npcs) || npcs < 5L) return(NA_integer_)
  as.integer(npcs)
}

choose_dims <- function(npcs) {
  dims_n <- min(MAX_UMAP_DIMS, npcs)
  if (!is.finite(dims_n) || dims_n < 5L) return(NULL)
  seq_len(as.integer(dims_n))
}

append_status <- function(status_list, summary_list, manifest_list) {
  status_df <- data.table::rbindlist(status_list, fill = TRUE)

  summary_df <- if (length(summary_list) > 0L) {
    data.table::rbindlist(summary_list, fill = TRUE)
  } else {
    data.frame()
  }

  manifest_df <- if (length(manifest_list) > 0L) {
    data.table::rbindlist(manifest_list, fill = TRUE)
  } else {
    data.frame()
  }

  atomic_write_csv(status_df, status_csv)
  atomic_write_csv(summary_df, object_summary_csv)
  atomic_write_csv(manifest_df, reduced_manifest_csv)
}


# ============================================================
# 4. 读取 02B manifest
# ============================================================

stamp("读取 02B filtered object manifest。")

manifest <- read_csv_required(input_manifest_csv)

needed_cols <- c("dataset", "object_id", "filtered_rds")
if (!all(needed_cols %in% colnames(manifest))) {
  stop("02B manifest 缺少必要列：", paste(setdiff(needed_cols, colnames(manifest)), collapse = ", "))
}

manifest <- manifest[file.exists(manifest$filtered_rds), , drop = FALSE]

if (nrow(manifest) == 0L) {
  stop("02B manifest 中没有可用 filtered_rds。")
}

stamp("找到 02B filtered Seurat objects：", nrow(manifest))


# ============================================================
# 5. 主循环
# ============================================================

status_list <- list()
summary_list <- list()
reduced_manifest_list <- list()

for (i in seq_len(nrow(manifest))) {
  dataset <- manifest$dataset[[i]]
  object_id <- manifest$object_id[[i]]
  in_rds <- manifest$filtered_rds[[i]]

  out_dir_obj <- file.path(out_objects_dir, dataset)
  dir.create(out_dir_obj, recursive = TRUE, showWarnings = FALSE)

  out_rds <- file.path(out_dir_obj, paste0(object_id, "_03A_reduced.rds"))

  stamp("03A V2 处理对象 ", i, " / ", nrow(manifest), "：", dataset, " :: ", object_id)

  status_row <- data.frame(
    dataset = dataset,
    object_id = object_id,
    input_rds = in_rds,
    output_rds = out_rds,
    n_cells = NA_integer_,
    n_features = NA_integer_,
    assay_used = NA_character_,
    n_variable_features = NA_integer_,
    npcs = NA_integer_,
    dims_used = NA_character_,
    n_clusters = NA_integer_,
    large_object_safe_mode = FALSE,
    status = "PENDING",
    message = NA_character_,
    stringsAsFactors = FALSE
  )

  # 关键：如果已经有成功对象，就跳过，不从头重跑
  if (!REBUILD_EXISTING && file.exists(out_rds) && object_is_readable(out_rds)) {
    obj_tmp <- readRDS(out_rds)

    status_row$n_cells <- ncol(obj_tmp)
    status_row$n_features <- nrow(obj_tmp)
    status_row$status <- "SKIPPED_EXISTING"
    status_row$message <- "Existing 03A object is readable; skipped."

    status_list[[length(status_list) + 1L]] <- status_row

    reduced_manifest_list[[length(reduced_manifest_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      reduced_rds = out_rds,
      n_cells = ncol(obj_tmp),
      n_features = nrow(obj_tmp),
      status = "SKIPPED_EXISTING",
      stringsAsFactors = FALSE
    )

    rm(obj_tmp)
    gc(verbose = FALSE)
    next
  }

  obj <- tryCatch({
    readRDS(in_rds)
  }, error = function(e) {
    status_row$status <<- "FAILED_READ_RDS"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    next
  }

  assay <- get_assay_for_analysis(obj)

  if (is.na(assay) || !assay %in% names(obj@assays)) {
    status_row$status <- "FAILED_NO_VALID_ASSAY"
    status_row$message <- "No valid assay found."
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(obj)
    gc(verbose = FALSE)
    next
  }

  tryCatch({
    SeuratObject::DefaultAssay(obj) <- assay
  }, error = function(e) NULL)

  counts <- tryCatch({
    get_counts_matrix(obj, assay)
  }, error = function(e) {
    status_row$status <<- "FAILED_GET_COUNTS"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(counts)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(obj)
    gc(verbose = FALSE)
    next
  }

  n_cells <- ncol(counts)
  n_features <- nrow(counts)

  status_row$n_cells <- n_cells
  status_row$n_features <- n_features
  status_row$assay_used <- assay

  if (n_cells < MIN_CELLS_FOR_REDUCTION || n_features < MIN_FEATURES_FOR_REDUCTION) {
    status_row$status <- "SKIPPED_TOO_SMALL"
    status_row$message <- paste0("Too small for reduction: cells=", n_cells, "; features=", n_features)

    obj$analysis_stage <- "03A_skipped_too_small"
    saveRDS(obj, out_rds, compress = SAVE_RDS_COMPRESS)

    status_list[[length(status_list) + 1L]] <- status_row

    reduced_manifest_list[[length(reduced_manifest_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      reduced_rds = out_rds,
      n_cells = n_cells,
      n_features = n_features,
      status = status_row$status,
      stringsAsFactors = FALSE
    )

    append_status(status_list, summary_list, reduced_manifest_list)
    rm(obj, counts)
    gc(verbose = FALSE)
    next
  }

  # Normalize
  stamp("  NormalizeData")
  obj <- tryCatch({
    NormalizeData(obj, assay = assay, normalization.method = "LogNormalize", scale.factor = 10000, verbose = FALSE)
  }, error = function(e) {
    status_row$status <<- "FAILED_NORMALIZE"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(counts)
    gc(verbose = FALSE)
    next
  }

  # Variable features
  stamp("  FindVariableFeatures")
  obj <- tryCatch({
    FindVariableFeatures(obj, assay = assay, selection.method = "vst", nfeatures = min(N_VARIABLE_FEATURES, n_features), verbose = FALSE)
  }, error = function(e) {
    status_row$status <<- "FAILED_VARIABLE_FEATURES"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(counts)
    gc(verbose = FALSE)
    next
  }

  hvgs <- SeuratObject::VariableFeatures(obj, assay = assay)
  hvgs <- hvgs[hvgs %in% rownames(obj)]
  status_row$n_variable_features <- length(hvgs)

  if (length(hvgs) < 50L) {
    status_row$status <- "SKIPPED_TOO_FEW_HVGS"
    status_row$message <- paste0("Too few HVGs: ", length(hvgs))

    obj$analysis_stage <- "03A_skipped_too_few_hvgs"
    saveRDS(obj, out_rds, compress = SAVE_RDS_COMPRESS)

    status_list[[length(status_list) + 1L]] <- status_row

    reduced_manifest_list[[length(reduced_manifest_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      reduced_rds = out_rds,
      n_cells = n_cells,
      n_features = n_features,
      status = status_row$status,
      stringsAsFactors = FALSE
    )

    append_status(status_list, summary_list, reduced_manifest_list)
    rm(obj, counts)
    gc(verbose = FALSE)
    next
  }

  npcs <- choose_npcs(n_cells, length(hvgs))
  dims_use <- choose_dims(npcs)

  if (is.na(npcs) || is.null(dims_use)) {
    status_row$status <- "SKIPPED_INVALID_PCA_DIMS"
    status_row$message <- paste0("Invalid PCA dims: npcs=", npcs)

    obj$analysis_stage <- "03A_skipped_invalid_dims"
    saveRDS(obj, out_rds, compress = SAVE_RDS_COMPRESS)

    status_list[[length(status_list) + 1L]] <- status_row

    reduced_manifest_list[[length(reduced_manifest_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      reduced_rds = out_rds,
      n_cells = n_cells,
      n_features = n_features,
      status = status_row$status,
      stringsAsFactors = FALSE
    )

    append_status(status_list, summary_list, reduced_manifest_list)
    rm(obj, counts)
    gc(verbose = FALSE)
    next
  }

  status_row$npcs <- npcs
  status_row$dims_used <- paste(range(dims_use), collapse = ":")

  stamp("  ScaleData")
  obj <- tryCatch({
    ScaleData(obj, assay = assay, features = hvgs, verbose = FALSE)
  }, error = function(e) {
    status_row$status <<- "FAILED_SCALE"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(counts)
    gc(verbose = FALSE)
    next
  }

  stamp("  RunPCA")
  obj <- tryCatch({
    RunPCA(obj, assay = assay, features = hvgs, npcs = npcs, verbose = FALSE)
  }, error = function(e) {
    status_row$status <<- "FAILED_PCA"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(counts)
    gc(verbose = FALSE)
    next
  }

  # PCA-only 安全模式
  if (FORCE_PCA_ONLY_ALL_OBJECTS || (SAFE_LARGE_OBJECT_MODE && n_cells > LARGE_OBJECT_CELL_CUTOFF)) {
    stamp("  V3 PCA-only 模式：cells=", n_cells, "，跳过 UMAP/Neighbors/Clusters，保留 PCA。")

    obj$analysis_stage <- "03A_v3_pca_only_resume"
    obj$dataset_03A <- dataset
    obj$object_id_03A <- object_id

    saveRDS(obj, out_rds, compress = SAVE_RDS_COMPRESS)

    status_row$status <- "SUCCESS_03A_PCA_ONLY"
    status_row$message <- paste0("V3 PCA-only safe mode. cells=", n_cells)
    status_row$large_object_safe_mode <- TRUE

    status_list[[length(status_list) + 1L]] <- status_row

    summary_list[[length(summary_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      n_cells = n_cells,
      n_features = n_features,
      assay_used = assay,
      n_variable_features = length(hvgs),
      npcs = npcs,
      dims_used = paste(range(dims_use), collapse = ":"),
      n_clusters = NA_integer_,
      reduced_rds = out_rds,
      status = status_row$status,
      stringsAsFactors = FALSE
    )

    reduced_manifest_list[[length(reduced_manifest_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      reduced_rds = out_rds,
      n_cells = n_cells,
      n_features = n_features,
      n_variable_features = length(hvgs),
      npcs = npcs,
      dims_used = paste(range(dims_use), collapse = ":"),
      n_clusters = NA_integer_,
      status = status_row$status,
      stringsAsFactors = FALSE
    )

    append_status(status_list, summary_list, reduced_manifest_list)
    rm(obj, counts)
    gc(verbose = FALSE)
    next
  }

  # UMAP / neighbors / clusters
  stamp("  RunUMAP / FindNeighbors / FindClusters")
  obj <- tryCatch({
    obj <- RunUMAP(
      obj,
      reduction = "pca",
      dims = dims_use,
      reduction.name = "umap",
      reduction.key = "UMAP_",
      umap.method = "uwot",
      metric = "cosine",
      n.neighbors = 30L,
      min.dist = 0.3,
      verbose = FALSE
    )

    obj <- FindNeighbors(obj, reduction = "pca", dims = dims_use, verbose = FALSE)
    obj <- FindClusters(obj, resolution = CLUSTER_RESOLUTION, verbose = FALSE)

    obj
  }, error = function(e) {
    status_row$status <<- "FAILED_UMAP_CLUSTER"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    append_status(status_list, summary_list, reduced_manifest_list)
    rm(counts)
    gc(verbose = FALSE)
    next
  }

  n_clusters <- if ("seurat_clusters" %in% colnames(obj@meta.data)) {
    length(unique(as.character(obj$seurat_clusters)))
  } else {
    NA_integer_
  }

  status_row$n_clusters <- n_clusters

  obj$analysis_stage <- "03A_normalized_reduced"
  obj$dataset_03A <- dataset
  obj$object_id_03A <- object_id

  stamp("保存 03A reduced object：", out_rds)
  saveRDS(obj, out_rds, compress = SAVE_RDS_COMPRESS)

  status_row$status <- "SUCCESS_03A_REDUCED"
  status_row$message <- "Normalize/HVG/Scale/PCA/UMAP/cluster completed."

  status_list[[length(status_list) + 1L]] <- status_row

  summary_list[[length(summary_list) + 1L]] <- data.frame(
    dataset = dataset,
    object_id = object_id,
    n_cells = n_cells,
    n_features = n_features,
    assay_used = assay,
    n_variable_features = length(hvgs),
    npcs = npcs,
    dims_used = paste(range(dims_use), collapse = ":"),
    n_clusters = n_clusters,
    reduced_rds = out_rds,
    status = status_row$status,
    stringsAsFactors = FALSE
  )

  reduced_manifest_list[[length(reduced_manifest_list) + 1L]] <- data.frame(
    dataset = dataset,
    object_id = object_id,
    reduced_rds = out_rds,
    n_cells = n_cells,
    n_features = n_features,
    n_variable_features = length(hvgs),
    npcs = npcs,
    dims_used = paste(range(dims_use), collapse = ":"),
    n_clusters = n_clusters,
    status = "SUCCESS_03A_REDUCED",
    stringsAsFactors = FALSE
  )

  append_status(status_list, summary_list, reduced_manifest_list)

  rm(obj, counts)
  gc(verbose = FALSE)
}


# ============================================================
# 6. 最终输出
# ============================================================

append_status(status_list, summary_list, reduced_manifest_list)

status_df <- data.table::fread(status_csv, data.table = FALSE)
manifest_df <- data.table::fread(reduced_manifest_csv, data.table = FALSE)

n_total <- nrow(status_df)
n_success_full <- sum(status_df$status == "SUCCESS_03A_REDUCED", na.rm = TRUE)
n_success_pca_only <- sum(status_df$status %in% c("SUCCESS_03A_PCA_ONLY", "SUCCESS_03A_PCA_ONLY_LARGE_OBJECT"), na.rm = TRUE)
n_skipped_existing <- sum(status_df$status == "SKIPPED_EXISTING", na.rm = TRUE)
n_skipped_other <- sum(grepl("^SKIPPED", status_df$status) & status_df$status != "SKIPPED_EXISTING", na.rm = TRUE)
n_failed <- sum(grepl("^FAILED", status_df$status), na.rm = TRUE)

total_cells_done <- if ("n_cells" %in% colnames(manifest_df)) {
  sum(manifest_df$n_cells, na.rm = TRUE)
} else {
  0
}

report_lines <- c(
  "03A V3 PCA-only resume normalization and per-object reduction report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input:",
  paste0("02B filtered object manifest: ", input_manifest_csv),
  "",
  "Summary:",
  paste0("Total records: ", n_total),
  paste0("Full success reduced objects: ", n_success_full),
  paste0("PCA-only objects: ", n_success_pca_only),
  paste0("Skipped existing objects: ", n_skipped_existing),
  paste0("Other skipped objects: ", n_skipped_other),
  paste0("Failed objects: ", n_failed),
  paste0("Cells represented in reduced manifest: ", total_cells_done),
  "",
  "Safe settings:",
  paste0("REBUILD_EXISTING: ", REBUILD_EXISTING),
  paste0("SAVE_BASIC_PLOTS: ", SAVE_BASIC_PLOTS),
  paste0("MAX_NPCS: ", MAX_NPCS),
  paste0("MAX_UMAP_DIMS: ", MAX_UMAP_DIMS),
  paste0("FORCE_PCA_ONLY_ALL_OBJECTS: ", FORCE_PCA_ONLY_ALL_OBJECTS),
  paste0("SAFE_LARGE_OBJECT_MODE: ", SAFE_LARGE_OBJECT_MODE),
  paste0("LARGE_OBJECT_CELL_CUTOFF: ", LARGE_OBJECT_CELL_CUTOFF),
  "",
  "Output files:",
  paste0("Status CSV: ", status_csv),
  paste0("Object summary CSV: ", object_summary_csv),
  paste0("Reduced manifest CSV: ", reduced_manifest_csv),
  paste0("Reduced objects: ", out_objects_dir),
  "",
  "Next step:",
  "03B_MERGE_WITHIN_DATASET_AND_BATCH_CHECK.R",
  "",
  "Important note:",
  "Objects marked PCA-only are intentionally kept safe to avoid Windows/RStudio memory abort caused by UMAP/neighbor graph steps. UMAP/clustering can be performed later after merging/integration."
)

writeLines(report_lines, report_txt)


cat("\n============================================================\n")
cat("03A V3 PCA-only resume 运行结束\n")
cat("============================================================\n\n")

cat("总记录数：", n_total, "\n")
cat("完整 reduced 成功对象：", n_success_full, "\n")
cat("PCA-only 成功对象：", n_success_pca_only, "\n")
cat("跳过已有成功对象：", n_skipped_existing, "\n")
cat("其他跳过对象：", n_skipped_other, "\n")
cat("失败对象：", n_failed, "\n")
cat("manifest 中细胞总数：", total_cells_done, "\n\n")

cat("输出文件：\n")
cat(status_csv, "\n")
cat(object_summary_csv, "\n")
cat(reduced_manifest_csv, "\n")
cat(report_txt, "\n\n")

if (n_failed == 0L) {
  cat("✅ 03A V3 PCA-only resume 完成。\n")
  cat("下一步可以进入 03B：按 dataset 合并对象、检查 batch effect、准备 integration。\n")
} else {
  cat("⚠️ 03A V3 完成，但仍有失败对象。请查看 03A_reduction_status.csv。\n")
}
