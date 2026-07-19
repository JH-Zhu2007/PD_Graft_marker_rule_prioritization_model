
PROJECT_DIR <- "D:/PD_Graft_Project"

REBUILD_EXISTING <- TRUE

APPLY_FILTERING_AND_SAVE_OBJECTS <- TRUE

SAVE_QC_PLOTS <- TRUE

PLOT_MAX_CELLS <- 50000L

SAVE_RDS_COMPRESS <- FALSE

NFEATURE_LOW_DEFAULT <- 200L
NFEATURE_LOW_LOWDEPTH <- 100L
LOW_DEPTH_MEDIAN_NFEATURE_CUTOFF <- 500L

NFEATURE_HIGH_QUANTILE <- 0.995

NCOUNT_HIGH_QUANTILE <- 0.995

MT_ENABLE_IF_P95_GT <- 0.1
MT_MIN_CUTOFF <- 15
MT_MAX_CUTOFF <- 30
MT_QUANTILE <- 0.99

NCOUNT_LOW_DEFAULT <- 0

cat("\n============================================================\n")
cat("02B：QC visualization and filtering decision\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "Matrix", "data.table", "ggplot2")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 02B。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
  library(Matrix)
  library(data.table)
  library(ggplot2)
})

metadata_dir <- file.path(PROJECT_DIR, "01_metadata")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_status_csv <- file.path(metadata_dir, "02A_object_integrity_status.csv")
input_manifest_csv <- file.path(tables_dir, "02A_qc", "02A_cell_qc_metrics_manifest.csv")
input_summary_csv <- file.path(tables_dir, "02A_qc", "02A_seurat_object_qc_summary.csv")

out_objects_dir <- file.path(objects_dir, "02B_qc_filtered")
out_tables_dir <- file.path(tables_dir, "02B_qc")
out_figures_dir <- file.path(figures_dir, "02B_qc")

dir.create(out_objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

threshold_csv <- file.path(out_tables_dir, "02B_auto_qc_thresholds.csv")
cell_filter_summary_csv <- file.path(out_tables_dir, "02B_cell_filtering_summary.csv")
filtered_manifest_csv <- file.path(out_tables_dir, "02B_filtered_object_manifest.csv")
failed_csv <- file.path(out_tables_dir, "02B_failed_objects.csv")
report_txt <- file.path(reports_dir, "02B_QC_visualization_and_filtering_decision_report.txt")

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
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

safe_quantile <- function(x, prob) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path, "\n请先确认 02A V2 已成功完成。")
  }

  data.table::fread(path, data.table = FALSE)
}

make_threshold_for_object <- function(cell_qc, dataset, object_id) {
  nf <- as.numeric(cell_qc$nFeature_RNA)
  nc <- as.numeric(cell_qc$nCount_RNA)
  mt <- as.numeric(cell_qc$percent.mt)

  nf_median <- stats::median(nf, na.rm = TRUE)
  nc_median <- stats::median(nc, na.rm = TRUE)

  nf_min <- ifelse(
    is.finite(nf_median) && nf_median < LOW_DEPTH_MEDIAN_NFEATURE_CUTOFF,
    NFEATURE_LOW_LOWDEPTH,
    NFEATURE_LOW_DEFAULT
  )

  nf_max <- ceiling(safe_quantile(nf, NFEATURE_HIGH_QUANTILE))
  nc_min <- NCOUNT_LOW_DEFAULT
  nc_max <- ceiling(safe_quantile(nc, NCOUNT_HIGH_QUANTILE))

  mt_p95 <- safe_quantile(mt, 0.95)
  mt_p99 <- safe_quantile(mt, MT_QUANTILE)

  use_mt <- is.finite(mt_p95) && mt_p95 > MT_ENABLE_IF_P95_GT

  if (use_mt) {
    mt_max <- min(MT_MAX_CUTOFF, max(MT_MIN_CUTOFF, ceiling(mt_p99)))
  } else {
    mt_max <- 100
  }

  data.frame(
    dataset = dataset,
    object_id = object_id,
    n_cells_before = nrow(cell_qc),
    nFeature_min = as.numeric(nf_min),
    nFeature_max = as.numeric(nf_max),
    nCount_min = as.numeric(nc_min),
    nCount_max = as.numeric(nc_max),
    percent_mt_max = as.numeric(mt_max),
    use_percent_mt_filter = use_mt,
    nFeature_median = nf_median,
    nFeature_p01 = safe_quantile(nf, 0.01),
    nFeature_p05 = safe_quantile(nf, 0.05),
    nFeature_p95 = safe_quantile(nf, 0.95),
    nFeature_p995 = safe_quantile(nf, 0.995),
    nCount_median = nc_median,
    nCount_p995 = safe_quantile(nc, 0.995),
    percent_mt_median = stats::median(mt, na.rm = TRUE),
    percent_mt_p95 = mt_p95,
    percent_mt_p99 = mt_p99,
    stringsAsFactors = FALSE
  )
}

apply_filter_to_cell_qc <- function(cell_qc, thr) {
  keep <- rep(TRUE, nrow(cell_qc))

  keep <- keep & as.numeric(cell_qc$nFeature_RNA) >= thr$nFeature_min
  keep <- keep & as.numeric(cell_qc$nFeature_RNA) <= thr$nFeature_max
  keep <- keep & as.numeric(cell_qc$nCount_RNA) >= thr$nCount_min
  keep <- keep & as.numeric(cell_qc$nCount_RNA) <= thr$nCount_max

  if (isTRUE(thr$use_percent_mt_filter)) {
    keep <- keep & as.numeric(cell_qc$percent.mt) <= thr$percent_mt_max
  }

  keep[is.na(keep)] <- FALSE
  keep
}

plot_overall_retention <- function(summary_df, out_dir) {
  if (!SAVE_QC_PLOTS || nrow(summary_df) == 0L) return(invisible(NULL))

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  p <- ggplot(summary_df, aes(x = reorder(object_id, retention_rate), y = retention_rate)) +
    geom_col() +
    coord_flip() +
    theme_bw(base_size = 10) +
    labs(
      title = "02B cell retention rate after conservative QC filtering",
      x = "Object",
      y = "Retention rate"
    )

  ggsave(
    filename = file.path(out_dir, "02B_overall_retention_rate.pdf"),
    plot = p,
    width = 10,
    height = max(6, 0.18 * nrow(summary_df)),
    limitsize = FALSE
  )
}

plot_object_qc_before_after <- function(cell_qc, keep, dataset, object_id, out_dir) {
  if (!SAVE_QC_PLOTS) return(invisible(NULL))

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  plot_df <- cell_qc

  if (nrow(plot_df) > PLOT_MAX_CELLS) {
    set.seed(20260714)
    idx <- sample(seq_len(nrow(plot_df)), PLOT_MAX_CELLS)
    plot_df <- plot_df[idx, , drop = FALSE]
    keep_plot <- keep[idx]
  } else {
    keep_plot <- keep
  }

  plot_df$qc_keep_02B <- ifelse(keep_plot, "kept", "removed")

  p1 <- ggplot(plot_df, aes(x = nFeature_RNA, fill = qc_keep_02B)) +
    geom_histogram(bins = 80, alpha = 0.65, position = "identity") +
    theme_bw(base_size = 10) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "nFeature_RNA before/after QC decision",
      x = "nFeature_RNA",
      y = "Cell count"
    )

  p2 <- ggplot(plot_df, aes(x = percent.mt, fill = qc_keep_02B)) +
    geom_histogram(bins = 80, alpha = 0.65, position = "identity") +
    theme_bw(base_size = 10) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "percent.mt before/after QC decision",
      x = "percent.mt",
      y = "Cell count"
    )

  p3 <- ggplot(plot_df, aes(x = nCount_RNA, y = nFeature_RNA, color = qc_keep_02B)) +
    geom_point(alpha = 0.35, size = 0.3) +
    theme_bw(base_size = 10) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "nCount_RNA vs nFeature_RNA",
      x = "nCount_RNA",
      y = "nFeature_RNA"
    )

  file_pdf <- file.path(
    out_dir,
    paste0(safe_name(paste(dataset, object_id, sep = "__")), "_02B_qc_before_after.pdf")
  )

  grDevices::pdf(file_pdf, width = 9, height = 7)
  print(p1)
  print(p2)
  print(p3)
  grDevices::dev.off()

  invisible(file_pdf)
}

stamp("读取 02A V2 输出。")

status_df <- read_csv_required(input_status_csv)
manifest_df <- read_csv_required(input_manifest_csv)
summary_df_02A <- read_csv_required(input_summary_csv)

success_df <- status_df[
  status_df$status == "SUCCESS_SEURAT_QC_METRICS" &
    !is.na(status_df$saved_path) &
    file.exists(status_df$saved_path),
  ,
  drop = FALSE
]

if (nrow(success_df) == 0L) {
  stop("02A status 中没有找到成功的 Seurat QC 对象。请确认 02A V2 已完成。")
}

stamp("02A 成功 Seurat 对象数量：", nrow(success_df))

threshold_list <- list()
failed_list <- list()

for (i in seq_len(nrow(manifest_df))) {
  dataset <- manifest_df$dataset[[i]]
  object_id <- manifest_df$object_id[[i]]
  cell_qc_csv <- manifest_df$cell_qc_csv[[i]]

  if (!file.exists(cell_qc_csv)) {
    failed_list[[length(failed_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      stage = "threshold",
      status = "FAILED_MISSING_CELL_QC_CSV",
      message = cell_qc_csv,
      stringsAsFactors = FALSE
    )
    next
  }

  cell_qc <- tryCatch({
    data.table::fread(cell_qc_csv, data.table = FALSE)
  }, error = function(e) {
    failed_list[[length(failed_list) + 1L]] <<- data.frame(
      dataset = dataset,
      object_id = object_id,
      stage = "threshold",
      status = "FAILED_READ_CELL_QC_CSV",
      message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
    NULL
  })

  if (is.null(cell_qc)) next

  needed <- c("nFeature_RNA", "nCount_RNA", "percent.mt")
  if (!all(needed %in% colnames(cell_qc))) {
    failed_list[[length(failed_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      stage = "threshold",
      status = "FAILED_MISSING_QC_COLUMNS",
      message = paste(setdiff(needed, colnames(cell_qc)), collapse = ","),
      stringsAsFactors = FALSE
    )
    next
  }

  threshold_list[[length(threshold_list) + 1L]] <- make_threshold_for_object(
    cell_qc = cell_qc,
    dataset = dataset,
    object_id = object_id
  )
}

threshold_df <- data.table::rbindlist(threshold_list, fill = TRUE)
atomic_write_csv(threshold_df, threshold_csv)

stamp("已生成 QC threshold 表：", threshold_csv)

filter_summary_list <- list()
filtered_manifest_list <- list()

for (i in seq_len(nrow(success_df))) {
  dataset <- success_df$dataset[[i]]
  object_id <- success_df$object_id[[i]]
  saved_path_02A <- success_df$saved_path[[i]]

  stamp("02B 过滤对象 ", i, " / ", nrow(success_df), "：", dataset, " :: ", object_id)

  thr <- threshold_df[
    threshold_df$dataset == dataset & threshold_df$object_id == object_id,
    ,
    drop = FALSE
  ]

  if (nrow(thr) == 0L) {
    failed_list[[length(failed_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      stage = "filter",
      status = "FAILED_NO_THRESHOLD",
      message = "没有找到该对象的 threshold。",
      stringsAsFactors = FALSE
    )
    next
  }

  thr <- thr[1, , drop = FALSE]

  obj <- tryCatch({
    readRDS(saved_path_02A)
  }, error = function(e) {
    failed_list[[length(failed_list) + 1L]] <<- data.frame(
      dataset = dataset,
      object_id = object_id,
      stage = "filter",
      status = "FAILED_READ_02A_OBJECT",
      message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
    NULL
  })

  if (is.null(obj)) next

  meta <- obj@meta.data
  meta$cell_barcode <- rownames(meta)

  needed <- c("nFeature_RNA", "nCount_RNA", "percent.mt")
  if (!all(needed %in% colnames(meta))) {
    failed_list[[length(failed_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      stage = "filter",
      status = "FAILED_MISSING_META_QC_COLUMNS",
      message = paste(setdiff(needed, colnames(meta)), collapse = ","),
      stringsAsFactors = FALSE
    )
    rm(obj)
    gc(verbose = FALSE)
    next
  }

  keep <- apply_filter_to_cell_qc(meta, thr)
  keep_cells <- rownames(meta)[keep]

  n_before <- ncol(obj)
  n_after <- length(keep_cells)
  retention_rate <- ifelse(n_before > 0, n_after / n_before, NA_real_)

  obj$qc_keep_02B <- keep
  obj$qc_reason_02B <- ifelse(keep, "kept", "removed_by_conservative_qc")
  obj$qc_stage <- "02B_qc_filtered"

  tryCatch({
    plot_object_qc_before_after(
      cell_qc = meta,
      keep = keep,
      dataset = dataset,
      object_id = object_id,
      out_dir = file.path(out_figures_dir, dataset)
    )
  }, error = function(e) {
    stamp("02B QC plot 失败但不中断：", dataset, " :: ", object_id, "；", conditionMessage(e))
  })

  out_dir_obj <- file.path(out_objects_dir, dataset)
  dir.create(out_dir_obj, recursive = TRUE, showWarnings = FALSE)

  out_rds <- file.path(out_dir_obj, paste0(object_id, "_02B_filtered.rds"))

  if (APPLY_FILTERING_AND_SAVE_OBJECTS) {
    if (!REBUILD_EXISTING && file.exists(out_rds)) {
      stamp("已存在 02B filtered object，跳过保存：", out_rds)
    } else {
      obj_filtered <- subset(obj, cells = keep_cells)
      saveRDS(obj_filtered, out_rds, compress = SAVE_RDS_COMPRESS)
      rm(obj_filtered)
    }
  }

  filter_summary_list[[length(filter_summary_list) + 1L]] <- data.frame(
    dataset = dataset,
    object_id = object_id,
    n_cells_before = n_before,
    n_cells_after = n_after,
    n_cells_removed = n_before - n_after,
    retention_rate = retention_rate,
    nFeature_min = thr$nFeature_min,
    nFeature_max = thr$nFeature_max,
    nCount_min = thr$nCount_min,
    nCount_max = thr$nCount_max,
    percent_mt_max = thr$percent_mt_max,
    use_percent_mt_filter = thr$use_percent_mt_filter,
    stringsAsFactors = FALSE
  )

  filtered_manifest_list[[length(filtered_manifest_list) + 1L]] <- data.frame(
    dataset = dataset,
    object_id = object_id,
    filtered_rds = out_rds,
    n_cells_before = n_before,
    n_cells_after = n_after,
    retention_rate = retention_rate,
    stringsAsFactors = FALSE
  )

  rm(obj, meta)
  gc(verbose = FALSE)
}

filter_summary_df <- data.table::rbindlist(filter_summary_list, fill = TRUE)
filtered_manifest_df <- data.table::rbindlist(filtered_manifest_list, fill = TRUE)

atomic_write_csv(filter_summary_df, cell_filter_summary_csv)
atomic_write_csv(filtered_manifest_df, filtered_manifest_csv)

if (length(failed_list) > 0L) {
  failed_df <- data.table::rbindlist(failed_list, fill = TRUE)
} else {
  failed_df <- data.frame()
}
atomic_write_csv(failed_df, failed_csv)

tryCatch({
  plot_overall_retention(filter_summary_df, out_figures_dir)
}, error = function(e) {
  stamp("总体 retention plot 失败但不中断：", conditionMessage(e))
})

n_objects <- nrow(success_df)
n_filtered_objects <- nrow(filtered_manifest_df)
n_failed <- nrow(failed_df)

total_before <- if ("n_cells_before" %in% colnames(filter_summary_df)) {
  sum(filter_summary_df$n_cells_before, na.rm = TRUE)
} else {
  0
}

total_after <- if ("n_cells_after" %in% colnames(filter_summary_df)) {
  sum(filter_summary_df$n_cells_after, na.rm = TRUE)
} else {
  0
}

overall_retention <- ifelse(total_before > 0, total_after / total_before, NA_real_)

report_lines <- c(
  "02B QC visualization and filtering decision report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input:",
  paste0("02A status: ", input_status_csv),
  paste0("02A manifest: ", input_manifest_csv),
  paste0("02A summary: ", input_summary_csv),
  "",
  "Summary:",
  paste0("02A successful Seurat objects used: ", n_objects),
  paste0("Filtered objects saved: ", n_filtered_objects),
  paste0("Failed objects/tasks: ", n_failed),
  paste0("Total cells before QC filtering: ", total_before),
  paste0("Total cells after QC filtering: ", total_after),
  paste0("Overall retention rate: ", round(overall_retention * 100, 3), "%"),
  "",
  "Filtering rule:",
  paste0("nFeature min default: ", NFEATURE_LOW_DEFAULT, " or ", NFEATURE_LOW_LOWDEPTH, " for low-depth objects"),
  paste0("nFeature max quantile: ", NFEATURE_HIGH_QUANTILE),
  paste0("nCount max quantile: ", NCOUNT_HIGH_QUANTILE),
  paste0("percent.mt filter enabled when p95 > ", MT_ENABLE_IF_P95_GT),
  paste0("percent.mt max: max(", MT_MIN_CUTOFF, ", p99), capped at ", MT_MAX_CUTOFF),
  "",
  "Output files:",
  paste0("Thresholds: ", threshold_csv),
  paste0("Cell filtering summary: ", cell_filter_summary_csv),
  paste0("Filtered object manifest: ", filtered_manifest_csv),
  paste0("Failed objects/tasks: ", failed_csv),
  paste0("Filtered Seurat objects: ", out_objects_dir),
  paste0("QC figures: ", out_figures_dir),
  "",
  "Next step:",
  "03A_NORMALIZATION_AND_PER_DATASET_REDUCTION.R",
  "",
  "Important note:",
  "02B uses conservative automatic thresholds. Before manuscript-level claims, inspect threshold and retention tables."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("02B QC visualization and filtering decision 运行结束\n")
cat("============================================================\n\n")

cat("02A 成功 Seurat 对象数量：", n_objects, "\n")
cat("已保存 filtered objects 数量：", n_filtered_objects, "\n")
cat("失败任务数量：", n_failed, "\n")
cat("过滤前总细胞数：", total_before, "\n")
cat("过滤后总细胞数：", total_after, "\n")
cat("总体保留率：", round(overall_retention * 100, 3), "%\n\n")

cat("输出文件：\n")
cat(threshold_csv, "\n")
cat(cell_filter_summary_csv, "\n")
cat(filtered_manifest_csv, "\n")
cat(failed_csv, "\n")
cat(report_txt, "\n\n")

if (n_failed == 0L) {
  cat("✅ 02B QC visualization and filtering decision 完成。\n")
  cat("下一步可以进入 03A：Normalize / variable features / PCA / UMAP 初步降维。\n")
} else {
  cat("⚠️ 02B 完成，但存在失败任务。请查看 failed CSV。\n")
}
