# ============================================================
# 02A_OBJECT_INTEGRITY_AND_QC_METRICS_V2.R
# ============================================================
# 目的：
#   01A / 01B 完成后，进入正式 QC 前的第一步。
#
#   本脚本只做：
#     1. 检查所有已导入对象是否可读
#     2. 区分 Seurat single-cell 对象与 bulk / 非 Seurat 对象
#     3. 给 Seurat 对象计算基础 QC 指标
#        - nFeature_RNA
#        - nCount_RNA
#        - percent.mt
#        - percent.ribo
#        - percent.hb
#     4. 保存带 QC 指标的新对象副本
#     5. 输出每个对象/样本的 QC summary 表
#     6. 输出 02A 报告
#
# 注意：
#   02A 不做过滤。
#   02A 不做 Normalize。
#   02A 不做 integration。
#   02A 不做 clustering。
#
# 成功标志：
#   最后显示：
#   ✅ 02A V2 object integrity and QC metrics 完成。
#
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"

REBUILD_EXISTING <- TRUE

# 是否保存加入 QC 指标后的 Seurat 对象
SAVE_UPDATED_SEURAT_OBJECTS <- TRUE

# 是否输出简单 QC 分布图
SAVE_BASIC_QC_PLOTS <- TRUE

# 绘图最多抽样多少细胞，避免超大对象画图太慢
PLOT_MAX_CELLS <- 50000L

# 保存 RDS 是否压缩
# FALSE 更快但更占硬盘；TRUE 更慢但更省空间
SAVE_RDS_COMPRESS <- FALSE


# ============================================================
# 1. 加载包
# ============================================================
# V2 关键修正：
#   02A V1 读入 GSE204795 的 DESeqDataSet 后，Bioconductor 的
#   SummarizedExperiment 会 mask Seurat/SeuratObject 的 Assays()。
#   这会导致后面的 Seurat 对象被误判/失败。
#   V2 不再使用裸 Assays()，改用 obj@assays 和 SeuratObject::GetAssayData，
#   避免 namespace 冲突。


cat("\n============================================================\n")
cat("02A V2：object integrity and QC metrics\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "Matrix", "data.table", "ggplot2")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 02A。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(Matrix)
  library(data.table)
  library(ggplot2)
})


# ============================================================
# 2. 路径设置
# ============================================================

objects_root <- file.path(PROJECT_DIR, "02_objects")
metadata_dir <- file.path(PROJECT_DIR, "01_metadata")
tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_objects_dir <- file.path(objects_root, "02A_qc_metrics")
out_tables_dir <- file.path(tables_dir, "02A_qc")
out_figures_dir <- file.path(figures_dir, "02A_qc")

dir.create(out_objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

status_csv <- file.path(metadata_dir, "02A_object_integrity_status.csv")
summary_csv <- file.path(out_tables_dir, "02A_seurat_object_qc_summary.csv")
cell_manifest_csv <- file.path(out_tables_dir, "02A_cell_qc_metrics_manifest.csv")
sample_summary_csv <- file.path(out_tables_dir, "02A_sample_level_qc_summary.csv")
non_seurat_csv <- file.path(out_tables_dir, "02A_non_seurat_objects.csv")
duplicate_cells_csv <- file.path(out_tables_dir, "02A_duplicate_cell_names_across_objects.csv")
report_txt <- file.path(reports_dir, "02A_object_integrity_and_qc_metrics_report.txt")


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
  x <- gsub("\\.rds$", "", x, ignore.case = TRUE)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  ifelse(nchar(x) == 0, "unnamed", x)
}

infer_dataset <- function(path) {
  p <- gsub("\\\\", "/", path)
  parts <- strsplit(p, "/", fixed = TRUE)[[1]]

  idx <- which(parts == "01A_standardized")
  if (length(idx) > 0 && length(parts) >= idx[1] + 1) {
    return(parts[idx[1] + 1])
  }

  if (grepl("01B_GSE178265_DA", p, ignore.case = TRUE)) {
    return("GSE178265_DA_01B")
  }

  m <- regmatches(p, regexpr("GSE[0-9]+", p))
  if (length(m) > 0 && nchar(m) > 0) {
    return(m[1])
  }

  "UNKNOWN_DATASET"
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

object_is_readable <- function(path) {
  tryCatch({
    readRDS(path)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

is_seurat_object <- function(obj) {
  inherits(obj, "Seurat")
}

is_bulk_like_object <- function(obj) {
  cls <- class(obj)
  any(grepl("DESeqDataSet|SummarizedExperiment|ExpressionSet|DGEList", cls))
}

get_assay_for_qc <- function(obj) {
  # V2：不用 Assays(obj)，避免 SummarizedExperiment::Assays mask Seurat::Assays。
  assays <- names(obj@assays)

  if ("RNA" %in% assays) {
    return("RNA")
  }

  da <- tryCatch({
    SeuratObject::DefaultAssay(obj)
  }, error = function(e) {
    NA_character_
  })

  if (!is.na(da) && da %in% assays) {
    return(da)
  }

  if (length(assays) > 0L) {
    return(assays[[1L]])
  }

  NA_character_
}

get_counts_matrix <- function(obj, assay) {
  # Seurat v5 优先 layer；Seurat v4 用 slot。
  mat <- tryCatch({
    SeuratObject::GetAssayData(obj, assay = assay, layer = "counts")
  }, error = function(e1) {
    tryCatch({
      SeuratObject::GetAssayData(obj, assay = assay, slot = "counts")
    }, error = function(e2) {
      NULL
    })
  })

  if (is.null(mat)) {
    stop("无法从 assay=", assay, " 读取 counts。")
  }

  mat
}

calc_percent_by_pattern <- function(counts, pattern) {
  genes <- rownames(counts)
  hit <- grep(pattern, genes, value = TRUE)

  total <- Matrix::colSums(counts)
  total[total == 0] <- NA_real_

  if (length(hit) == 0L) {
    return(rep(0, ncol(counts)))
  }

  pct <- Matrix::colSums(counts[hit, , drop = FALSE]) / total * 100
  pct[is.na(pct)] <- 0
  as.numeric(pct)
}

safe_quantile <- function(x, probs) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(rep(NA_real_, length(probs)))
  }
  as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
}

make_summary_row <- function(meta, dataset, object_id, object_path, assay, saved_path) {
  q_nf <- safe_quantile(meta$nFeature_RNA, c(0.01, 0.05, 0.50, 0.95, 0.99))
  q_nc <- safe_quantile(meta$nCount_RNA, c(0.01, 0.05, 0.50, 0.95, 0.99))
  q_mt <- safe_quantile(meta$percent.mt, c(0.01, 0.05, 0.50, 0.95, 0.99))
  q_rb <- safe_quantile(meta$percent.ribo, c(0.01, 0.05, 0.50, 0.95, 0.99))

  data.frame(
    dataset = dataset,
    object_id = object_id,
    object_path = object_path,
    saved_path = saved_path,
    assay_used = assay,
    n_cells = nrow(meta),
    nFeature_p01 = q_nf[1],
    nFeature_p05 = q_nf[2],
    nFeature_median = q_nf[3],
    nFeature_p95 = q_nf[4],
    nFeature_p99 = q_nf[5],
    nCount_p01 = q_nc[1],
    nCount_p05 = q_nc[2],
    nCount_median = q_nc[3],
    nCount_p95 = q_nc[4],
    nCount_p99 = q_nc[5],
    percent_mt_p01 = q_mt[1],
    percent_mt_p05 = q_mt[2],
    percent_mt_median = q_mt[3],
    percent_mt_p95 = q_mt[4],
    percent_mt_p99 = q_mt[5],
    percent_ribo_p01 = q_rb[1],
    percent_ribo_p05 = q_rb[2],
    percent_ribo_median = q_rb[3],
    percent_ribo_p95 = q_rb[4],
    percent_ribo_p99 = q_rb[5],
    stringsAsFactors = FALSE
  )
}

make_sample_summary <- function(cell_qc) {
  sample_col <- NULL

  candidate_cols <- c(
    "sample_id", "sample", "orig.ident", "donor", "condition",
    "dataset", "object_id"
  )

  for (cc in candidate_cols) {
    if (cc %in% colnames(cell_qc)) {
      sample_col <- cc
      break
    }
  }

  if (is.null(sample_col)) {
    cell_qc$sample_group_for_qc <- cell_qc$object_id
    sample_col <- "sample_group_for_qc"
  }

  dt <- data.table::as.data.table(cell_qc)
  dt[, sample_group_for_qc := as.character(get(sample_col))]

  dt[
    ,
    .(
      n_cells = .N,
      nFeature_median = median(nFeature_RNA, na.rm = TRUE),
      nFeature_p05 = as.numeric(quantile(nFeature_RNA, 0.05, na.rm = TRUE)),
      nFeature_p95 = as.numeric(quantile(nFeature_RNA, 0.95, na.rm = TRUE)),
      nCount_median = median(nCount_RNA, na.rm = TRUE),
      nCount_p05 = as.numeric(quantile(nCount_RNA, 0.05, na.rm = TRUE)),
      nCount_p95 = as.numeric(quantile(nCount_RNA, 0.95, na.rm = TRUE)),
      percent_mt_median = median(percent.mt, na.rm = TRUE),
      percent_mt_p95 = as.numeric(quantile(percent.mt, 0.95, na.rm = TRUE)),
      percent_ribo_median = median(percent.ribo, na.rm = TRUE),
      percent_ribo_p95 = as.numeric(quantile(percent.ribo, 0.95, na.rm = TRUE))
    ),
    by = .(dataset, object_id, sample_group_for_qc)
  ]
}

plot_qc_basic <- function(cell_qc, dataset, object_id, out_dir) {
  if (!SAVE_BASIC_QC_PLOTS) {
    return(invisible(NULL))
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (nrow(cell_qc) > PLOT_MAX_CELLS) {
    set.seed(20260714)
    cell_qc <- cell_qc[sample(seq_len(nrow(cell_qc)), PLOT_MAX_CELLS), , drop = FALSE]
  }

  plot_base <- safe_name(paste(dataset, object_id, sep = "__"))

  p1 <- ggplot(cell_qc, aes(x = nFeature_RNA)) +
    geom_histogram(bins = 80) +
    theme_bw(base_size = 11) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "nFeature_RNA distribution",
      x = "nFeature_RNA",
      y = "Cell count"
    )

  p2 <- ggplot(cell_qc, aes(x = nCount_RNA)) +
    geom_histogram(bins = 80) +
    theme_bw(base_size = 11) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "nCount_RNA distribution",
      x = "nCount_RNA",
      y = "Cell count"
    )

  p3 <- ggplot(cell_qc, aes(x = percent.mt)) +
    geom_histogram(bins = 80) +
    theme_bw(base_size = 11) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "percent.mt distribution",
      x = "percent.mt",
      y = "Cell count"
    )

  p4 <- ggplot(cell_qc, aes(x = nCount_RNA, y = nFeature_RNA)) +
    geom_point(alpha = 0.25, size = 0.25) +
    theme_bw(base_size = 11) +
    labs(
      title = paste0(dataset, " / ", object_id),
      subtitle = "nCount_RNA vs nFeature_RNA",
      x = "nCount_RNA",
      y = "nFeature_RNA"
    )

  png_file <- file.path(out_dir, paste0(plot_base, "_basic_qc.png"))
  pdf_file <- file.path(out_dir, paste0(plot_base, "_basic_qc.pdf"))

  grDevices::png(png_file, width = 2800, height = 2200, res = 220)
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  grDevices::dev.off()

  grDevices::pdf(pdf_file, width = 9, height = 7)
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  grDevices::dev.off()

  invisible(c(png_file, pdf_file))
}


# ============================================================
# 4. 收集对象路径
# ============================================================

stamp("收集 01A / 01B 对象。")

paths_01A <- character()

dir_01A <- file.path(objects_root, "01A_standardized")

if (dir.exists(dir_01A)) {
  paths_01A <- list.files(
    dir_01A,
    pattern = "\\.rds$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
}

path_01B <- file.path(
  objects_root,
  "01B_GSE178265_DA",
  "01B_GSE178265_DA_seurat.rds"
)

object_paths <- unique(c(paths_01A, if (file.exists(path_01B)) path_01B else character()))

object_paths <- object_paths[file.exists(object_paths)]

if (length(object_paths) == 0L) {
  stop("没有找到任何 01A/01B RDS 对象。请确认 01A 和 01B 已完成。")
}

stamp("找到 RDS 对象数量：", length(object_paths))


# ============================================================
# 5. 主循环：对象检查 + QC 指标计算
# ============================================================

status_list <- list()
summary_list <- list()
cell_manifest_files <- list()
sample_summary_list <- list()
non_seurat_list <- list()
all_cell_names_index <- list()

for (idx in seq_along(object_paths)) {
  path <- object_paths[[idx]]
  dataset <- infer_dataset(path)
  object_id <- safe_name(path)

  stamp("处理对象 ", idx, " / ", length(object_paths), "：", dataset, " :: ", object_id)

  object_out_dir <- file.path(out_objects_dir, dataset)
  object_table_dir <- file.path(out_tables_dir, "per_object_cell_qc")
  object_fig_dir <- file.path(out_figures_dir, dataset)

  dir.create(object_out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(object_table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(object_fig_dir, recursive = TRUE, showWarnings = FALSE)

  saved_path <- file.path(object_out_dir, paste0(object_id, "_02A_qc.rds"))
  cell_qc_csv <- file.path(object_table_dir, paste0(dataset, "__", object_id, "__cell_qc.csv"))

  status_row <- data.frame(
    dataset = dataset,
    object_id = object_id,
    object_path = path,
    object_class = NA_character_,
    is_seurat = FALSE,
    is_bulk_like = FALSE,
    n_features = NA_integer_,
    n_cells = NA_integer_,
    assay_used = NA_character_,
    saved_path = NA_character_,
    status = "PENDING",
    message = NA_character_,
    stringsAsFactors = FALSE
  )

  obj <- tryCatch({
    readRDS(path)
  }, error = function(e) {
    status_row$status <<- "FAILED_READ_RDS"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(obj)) {
    status_list[[length(status_list) + 1L]] <- status_row
    next
  }

  status_row$object_class <- paste(class(obj), collapse = " / ")

  if (!is_seurat_object(obj)) {
    status_row$is_bulk_like <- is_bulk_like_object(obj)
    status_row$status <- ifelse(status_row$is_bulk_like, "VALID_NON_SEURAT_BULK_LIKE_SKIPPED_QC", "VALID_NON_SEURAT_SKIPPED_QC")
    status_row$message <- "不是 Seurat 对象；02A 只对 single-cell Seurat 对象计算 QC 指标。"

    non_seurat_list[[length(non_seurat_list) + 1L]] <- data.frame(
      dataset = dataset,
      object_id = object_id,
      object_path = path,
      object_class = status_row$object_class,
      is_bulk_like = status_row$is_bulk_like,
      status = status_row$status,
      stringsAsFactors = FALSE
    )

    status_list[[length(status_list) + 1L]] <- status_row
    rm(obj)
    gc(verbose = FALSE)
    next
  }

  status_row$is_seurat <- TRUE

  assay <- tryCatch({
    get_assay_for_qc(obj)
  }, error = function(e) {
    NA_character_
  })

  if (is.na(assay) || !assay %in% names(obj@assays)) {
    status_row$status <- "FAILED_NO_VALID_ASSAY"
    status_row$message <- "找不到可用于 QC 的 assay。"
    status_list[[length(status_list) + 1L]] <- status_row
    rm(obj)
    gc(verbose = FALSE)
    next
  }

  tryCatch({ SeuratObject::DefaultAssay(obj) <- assay }, error = function(e) NULL)
  status_row$assay_used <- assay

  counts <- tryCatch({
    get_counts_matrix(obj, assay)
  }, error = function(e) {
    status_row$status <<- "FAILED_GET_COUNTS"
    status_row$message <<- conditionMessage(e)
    NULL
  })

  if (is.null(counts)) {
    status_list[[length(status_list) + 1L]] <- status_row
    rm(obj)
    gc(verbose = FALSE)
    next
  }

  status_row$n_features <- nrow(counts)
  status_row$n_cells <- ncol(counts)

  # 保证基础 QC 指标存在且与当前 counts 一致
  obj$nCount_RNA <- as.numeric(Matrix::colSums(counts))
  obj$nFeature_RNA <- as.numeric(Matrix::colSums(counts > 0))

  # 线粒体 / ribosomal / hemoglobin
  obj$percent.mt <- calc_percent_by_pattern(
    counts,
    pattern = "^MT-|^mt-|^Mt-"
  )

  obj$percent.ribo <- calc_percent_by_pattern(
    counts,
    pattern = "^RPL|^RPS|^Rpl|^Rps|^rpl|^rps"
  )

  obj$percent.hb <- calc_percent_by_pattern(
    counts,
    pattern = "^HBA|^HBB|^HBM|^HBQ|^HBZ|^Hba|^Hbb|^Hbm|^Hbq|^Hbz"
  )

  # 标准 metadata 字段
  obj$dataset_02A <- dataset
  obj$object_id_02A <- object_id
  obj$qc_stage <- "02A_qc_metrics"

  meta <- obj@meta.data
  meta$cell_barcode <- rownames(meta)
  meta$dataset <- dataset
  meta$object_id <- object_id
  meta$object_path <- path

  # 输出 per-object cell QC 表
  cell_qc_cols <- unique(c(
    "cell_barcode",
    "dataset",
    "object_id",
    "orig.ident",
    "sample",
    "sample_id",
    "condition",
    "nCount_RNA",
    "nFeature_RNA",
    "percent.mt",
    "percent.ribo",
    "percent.hb",
    "object_path"
  ))

  cell_qc_cols <- cell_qc_cols[cell_qc_cols %in% colnames(meta)]

  cell_qc <- meta[, cell_qc_cols, drop = FALSE]
  atomic_write_csv(cell_qc, cell_qc_csv)

  cell_manifest_files[[length(cell_manifest_files) + 1L]] <- data.frame(
    dataset = dataset,
    object_id = object_id,
    n_cells = nrow(cell_qc),
    cell_qc_csv = cell_qc_csv,
    stringsAsFactors = FALSE
  )

  # sample-level summary
  sample_summary_list[[length(sample_summary_list) + 1L]] <- as.data.frame(
    make_sample_summary(cell_qc)
  )

  # object-level summary
  summary_list[[length(summary_list) + 1L]] <- make_summary_row(
    meta = meta,
    dataset = dataset,
    object_id = object_id,
    object_path = path,
    assay = assay,
    saved_path = ifelse(SAVE_UPDATED_SEURAT_OBJECTS, saved_path, NA_character_)
  )

  # duplicate cell name 检查用
  all_cell_names_index[[length(all_cell_names_index) + 1L]] <- data.frame(
    dataset = dataset,
    object_id = object_id,
    cell_barcode = colnames(obj),
    stringsAsFactors = FALSE
  )

  # plot
  tryCatch({
    plot_qc_basic(cell_qc, dataset, object_id, object_fig_dir)
  }, error = function(e) {
    stamp("QC plot 失败但不中断：", dataset, " :: ", object_id, "；", conditionMessage(e))
  })

  # 保存更新后的 Seurat 对象
  if (SAVE_UPDATED_SEURAT_OBJECTS) {
    if (!REBUILD_EXISTING && file.exists(saved_path) && object_is_readable(saved_path)) {
      stamp("已存在可读 02A 对象，跳过保存：", saved_path)
    } else {
      stamp("保存 02A QC Seurat object：", saved_path)
      saveRDS(obj, saved_path, compress = SAVE_RDS_COMPRESS)
    }

    status_row$saved_path <- saved_path
  }

  status_row$status <- "SUCCESS_SEURAT_QC_METRICS"
  status_row$message <- "Seurat 对象可读；QC metrics 已计算。"

  status_list[[length(status_list) + 1L]] <- status_row

  rm(obj, counts, meta, cell_qc)
  gc(verbose = FALSE)
}


# ============================================================
# 6. 汇总输出
# ============================================================

status_df <- data.table::rbindlist(status_list, fill = TRUE)
atomic_write_csv(status_df, status_csv)

if (length(summary_list) > 0L) {
  summary_df <- data.table::rbindlist(summary_list, fill = TRUE)
} else {
  summary_df <- data.frame()
}
atomic_write_csv(summary_df, summary_csv)

if (length(cell_manifest_files) > 0L) {
  cell_manifest_df <- data.table::rbindlist(cell_manifest_files, fill = TRUE)
} else {
  cell_manifest_df <- data.frame()
}
atomic_write_csv(cell_manifest_df, cell_manifest_csv)

if (length(sample_summary_list) > 0L) {
  sample_summary_df <- data.table::rbindlist(sample_summary_list, fill = TRUE)
} else {
  sample_summary_df <- data.frame()
}
atomic_write_csv(sample_summary_df, sample_summary_csv)

if (length(non_seurat_list) > 0L) {
  non_seurat_df <- data.table::rbindlist(non_seurat_list, fill = TRUE)
} else {
  non_seurat_df <- data.frame()
}
atomic_write_csv(non_seurat_df, non_seurat_csv)

# duplicate cell names across objects
if (length(all_cell_names_index) > 0L) {
  cell_index_df <- data.table::rbindlist(all_cell_names_index, fill = TRUE)
  dup_df <- cell_index_df[
    duplicated(cell_index_df$cell_barcode) |
      duplicated(cell_index_df$cell_barcode, fromLast = TRUE),
    ,
    drop = FALSE
  ]

  if (nrow(dup_df) > 0L) {
    dup_df <- dup_df[order(dup_df$cell_barcode), , drop = FALSE]
  }
} else {
  dup_df <- data.frame()
}

atomic_write_csv(dup_df, duplicate_cells_csv)


# ============================================================
# 7. 写报告
# ============================================================

n_total <- nrow(status_df)
n_success_seurat <- sum(status_df$status == "SUCCESS_SEURAT_QC_METRICS", na.rm = TRUE)
n_non_seurat <- sum(grepl("NON_SEURAT", status_df$status), na.rm = TRUE)
n_failed <- sum(grepl("^FAILED", status_df$status), na.rm = TRUE)
n_cells_total <- if ("n_cells" %in% colnames(summary_df)) sum(summary_df$n_cells, na.rm = TRUE) else 0
n_duplicate_cells <- nrow(dup_df)

report_lines <- c(
  "02A V2 object integrity and QC metrics report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("Total RDS objects scanned: ", n_total),
  paste0("Successful Seurat QC objects: ", n_success_seurat),
  paste0("Non-Seurat / bulk-like skipped objects: ", n_non_seurat),
  paste0("Failed objects: ", n_failed),
  paste0("Total Seurat cells summarized: ", n_cells_total),
  paste0("Duplicate cell-name rows across objects: ", n_duplicate_cells),
  "",
  "Output files:",
  paste0("Status CSV: ", status_csv),
  paste0("Object QC summary CSV: ", summary_csv),
  paste0("Cell QC manifest CSV: ", cell_manifest_csv),
  paste0("Sample-level QC summary CSV: ", sample_summary_csv),
  paste0("Non-Seurat objects CSV: ", non_seurat_csv),
  paste0("Duplicate cell names CSV: ", duplicate_cells_csv),
  paste0("02A updated Seurat objects: ", out_objects_dir),
  paste0("Basic QC figures: ", out_figures_dir),
  "",
  "Interpretation:",
  "02A only calculates and records QC metrics. It does not remove cells.",
  "02B should inspect these metrics and decide filtering thresholds.",
  "",
  "Next step:",
  "02B_QC_VISUALIZATION_AND_FILTERING_DECISION.R"
)

writeLines(report_lines, report_txt)


# ============================================================
# 8. 结束提示
# ============================================================

cat("\n============================================================\n")
cat("02A V2 object integrity and QC metrics 运行结束\n")
cat("============================================================\n\n")

cat("总 RDS 对象数量：", n_total, "\n")
cat("成功计算 QC 的 Seurat 对象数量：", n_success_seurat, "\n")
cat("跳过的 non-Seurat / bulk-like 对象数量：", n_non_seurat, "\n")
cat("失败对象数量：", n_failed, "\n")
cat("Seurat 细胞总数：", n_cells_total, "\n")
cat("跨对象重复 cell name 行数：", n_duplicate_cells, "\n\n")

cat("输出文件：\n")
cat(status_csv, "\n")
cat(summary_csv, "\n")
cat(cell_manifest_csv, "\n")
cat(sample_summary_csv, "\n")
cat(non_seurat_csv, "\n")
cat(duplicate_cells_csv, "\n")
cat(report_txt, "\n\n")

if (n_failed == 0L) {
  cat("✅ 02A V2 object integrity and QC metrics 完成。\n")
  cat("下一步可以进入 02B：QC 可视化、阈值判断和过滤策略。\n")
} else {
  cat("⚠️ 02A 完成，但存在失败对象。请先查看 02A_object_integrity_status.csv。\n")
}
