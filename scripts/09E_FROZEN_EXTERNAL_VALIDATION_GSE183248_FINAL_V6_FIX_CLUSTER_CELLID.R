
PROJECT_DIR <- "D:/PD_Graft_Project"

EXTERNAL_GSE_ID <- "GSE183248"

SEED <- 20260715

DOWNLOAD_SUPPLEMENTARY <- TRUE

MAX_SUPPLEMENTARY_FILE_MB <- 5000

MAX_MATRICES_TO_IMPORT <- 20

MIN_MARKER_OVERLAP_PER_SIGNATURE <- 2

PDF_WIDTH <- 11.5
PDF_HEIGHT <- 7.5

PREFERRED_GROUP_FIELDS <- c(
  "condition",
  "external_condition",
  "sample",
  "orig.ident",
  "timepoint",
  "day",
  "treatment",
  "cell_type",
  "seurat_clusters"
)

cat("\n============================================================\n")
cat("09E：Frozen external validation application on GSE183248\n")
cat("============================================================\n\n")

options(stringsAsFactors = FALSE)
options(timeout = 60000)

required_pkgs <- c("data.table", "Matrix")

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop("缺少 R 包，请先手动安装：", paste(missing_pkgs, collapse = ", "))
}

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
})

HAS_SEURAT <- requireNamespace("Seurat", quietly = TRUE)
HAS_RANDOMFOREST <- requireNamespace("randomForest", quietly = TRUE)

if (HAS_SEURAT) {
  suppressPackageStartupMessages(library(Seurat))
} else {
  message("未检测到 Seurat：09E 会用 Matrix/base R 导入和打分；UMAP/Seurat object 输出会跳过。")
}

if (HAS_RANDOMFOREST) {
  suppressPackageStartupMessages(library(randomForest))
}

set.seed(SEED)

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
raw_dir <- file.path(PROJECT_DIR, "00_raw_data")
temp_dir <- file.path(PROJECT_DIR, "05_temp")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(
  tables_dir,
  "09E_frozen_external_validation_GSE183248_FINAL_V6_FIX_CLUSTER_CELLID"
)

out_figures_dir <- file.path(
  figures_dir,
  "09E_frozen_external_validation_GSE183248_FINAL_V6_FIX_CLUSTER_CELLID_pdf"
)

out_objects_dir <- file.path(
  objects_dir,
  "09E_external_GSE183248"
)

external_raw_dir <- file.path(
  raw_dir,
  "09E_external_GSE183248"
)

external_extract_dir <- file.path(
  temp_dir,
  "09E_GSE183248_extracted"
)

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(external_raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(external_extract_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

input_09D_eligibility <- file.path(
  tables_dir,
  "09D_external_validation_dataset_audit_V8_PUBLICATION_POLISH",
  "09D_external_dataset_eligibility_audit.csv"
)

input_09D_decision <- file.path(
  tables_dir,
  "09D_external_validation_dataset_audit_V8_PUBLICATION_POLISH",
  "09D_external_validation_dataset_decision_report.txt"
)

input_ideal_reduced <- file.path(
  tables_dir,
  "09B_ML_ready_dataset_and_leakage_audit_V3",
  "09B_ideal_like_training_reduced_non_direct_features.csv"
)

input_safety_reduced <- file.path(
  tables_dir,
  "09B_ML_ready_dataset_and_leakage_audit_V3",
  "09B_safety_risk_training_reduced_non_direct_features.csv"
)

external_metadata_audit_csv <- file.path(out_tables_dir, "09E_external_GSE183248_metadata_and_download_audit.csv")
external_file_inventory_csv <- file.path(out_tables_dir, "09E_external_GSE183248_file_inventory.csv")
external_matrix_inventory_csv <- file.path(out_tables_dir, "09E_external_GSE183248_matrix_inventory.csv")
external_import_audit_csv <- file.path(out_tables_dir, "09E_external_GSE183248_import_audit.csv")
marker_panel_audit_csv <- file.path(out_tables_dir, "09E_frozen_04A_marker_panel_audit.csv")
gene_overlap_audit_csv <- file.path(out_tables_dir, "09E_external_gene_overlap_audit.csv")
normalization_audit_csv <- file.path(out_tables_dir, "09E_external_normalization_decision_audit.csv")
cell_score_csv <- file.path(out_tables_dir, "09E_external_cell_signature_scores.csv")
cell_metadata_csv <- file.path(out_tables_dir, "09E_external_cell_metadata.csv")
group_score_summary_csv <- file.path(out_tables_dir, "09E_external_group_score_summary.csv")
external_feature_table_csv <- file.path(out_tables_dir, "09E_external_reduced_feature_table_for_prediction.csv")
ml_feature_alignment_csv <- file.path(out_tables_dir, "09E_external_ML_feature_alignment_audit.csv")
external_prediction_csv <- file.path(out_tables_dir, "09E_external_frozen_predictor_probabilities.csv")
method_note_txt <- file.path(out_tables_dir, "09E_method_and_claim_boundary_note.txt")
next_step_plan_txt <- file.path(out_tables_dir, "09E_to_09F_external_validation_figure_plan.txt")
session_info_txt <- file.path(out_tables_dir, "09E_sessionInfo.txt")
output_check_csv <- file.path(out_tables_dir, "09E_output_verification.csv")
report_txt <- file.path(reports_dir, "09E_frozen_external_validation_GSE183248_report.txt")

external_matrix_rds <- file.path(out_objects_dir, "09E_GSE183248_external_expression_matrix.rds")
external_seurat_rds <- file.path(out_objects_dir, "09E_GSE183248_external_seurat_object_optional.rds")

fig_score_heatmap_pdf <- file.path(out_figures_dir, "09E_external_signature_score_heatmap.pdf")
fig_prediction_pdf <- file.path(out_figures_dir, "09E_external_frozen_predictor_probability_barplot.pdf")
fig_gene_overlap_pdf <- file.path(out_figures_dir, "09E_external_gene_overlap_audit_barplot.pdf")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

num <- function(x) suppressWarnings(as.numeric(x))

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }

  if (file.exists(path)) unlink(path, force = TRUE)

  data.table::fwrite(df, path)

  if (!file.exists(path)) stop("CSV 未生成：", path)

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes <= 0) {
    stop("CSV 已创建但为空或无效：", path)
  }

  invisible(path)
}

read_required_csv <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = TRUE, showProgress = FALSE)
}

safe_pdf <- function(path, width = PDF_WIDTH, height = PDF_HEIGHT) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(path)) {
    removed <- file.remove(path)
    if (!isTRUE(removed)) {
      stop(
        "旧 PDF 正在被占用，无法覆盖：", path,
        "\n请关闭 Edge/Adobe/RStudio Viewer/文件资源管理器预览窗口后重跑。"
      )
    }
  }

  while (grDevices::dev.cur() > 1) {
    try(grDevices::dev.off(), silent = TRUE)
    if (grDevices::dev.cur() <= 1) break
  }

  grDevices::pdf(path, width = width, height = height, useDingbats = FALSE, onefile = TRUE)
}

finish_pdf <- function(path) {
  try(grDevices::dev.off(), silent = TRUE)

  if (!file.exists(path)) stop("PDF 未生成：", path)

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes < 1000) {
    stop("PDF 已创建但文件过小或无效：", path, "；size = ", size_bytes)
  }

  message("已保存 PDF：", normalizePath(path, winslash = "/", mustWork = TRUE),
          " | size = ", round(size_bytes / 1024, 1), " KB")
}

plot_empty_pdf <- function(path, title, message) {
  safe_pdf(path)
  plot.new()
  title(main = title)
  text(0.5, 0.5, message, cex = 0.95)
  finish_pdf(path)
}

safe_read_lines_url <- function(url) {
  tryCatch({
    readLines(url, warn = FALSE)
  }, error = function(e) {
    character()
  })
}

extract_href <- function(html_lines) {
  x <- unlist(regmatches(html_lines, gregexpr("href=\"[^\"]+\"", html_lines)))
  x <- gsub("^href=\"|\"$", "", x)
  x <- x[!grepl("^\\.\\./?$", x)]
  unique(x)
}

gse_series_dir <- function(gse_id) {
  sub("[0-9]{3}$", "nnn", toupper(gse_id), perl = TRUE)
}

gse_base_url <- function(gse_id) {
  paste0(
    "https://ftp.ncbi.nlm.nih.gov/geo/series/",
    gse_series_dir(gse_id),
    "/",
    toupper(gse_id)
  )
}

gse_supp_url <- function(gse_id) {
  paste0(gse_base_url(gse_id), "/suppl/")
}

gse_soft_url <- function(gse_id) {
  paste0(gse_base_url(gse_id), "/soft/", toupper(gse_id), "_family.soft.gz")
}

download_file_safe <- function(url, dest, max_mb = MAX_SUPPLEMENTARY_FILE_MB) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(dest) && file.info(dest)$size > 0) {
    return(data.table(
      url = url,
      dest = dest,
      status = "exists_skipped",
      size_bytes = file.info(dest)$size,
      message = NA_character_
    ))
  }

  res <- tryCatch({
    utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
    data.table(
      url = url,
      dest = dest,
      status = "downloaded",
      size_bytes = ifelse(file.exists(dest), file.info(dest)$size, NA_real_),
      message = NA_character_
    )
  }, error = function(e) {
    data.table(
      url = url,
      dest = dest,
      status = "failed",
      size_bytes = ifelse(file.exists(dest), file.info(dest)$size, NA_real_),
      message = conditionMessage(e)
    )
  })

  if (file.exists(dest) && file.info(dest)$size <= 0) {
    unlink(dest, force = TRUE)
  }

  res
}

extract_archive_safe <- function(path, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  status <- "not_archive_or_skipped"
  message <- NA_character_

  lower <- tolower(path)

  if (grepl("\\.zip$", lower)) {
    status <- tryCatch({
      utils::unzip(path, exdir = out_dir)
      "extracted_zip"
    }, error = function(e) {
      message <<- conditionMessage(e)
      "failed_zip"
    })
  } else if (grepl("\\.tar$|\\.tar\\.gz$|\\.tgz$", lower)) {
    status <- tryCatch({
      utils::untar(path, exdir = out_dir)
      "extracted_tar"
    }, error = function(e) {
      message <<- conditionMessage(e)
      "failed_tar"
    })
  }

  data.table(
    file = path,
    extracted_to = out_dir,
    extraction_status = status,
    message = message
  )
}

is_metadata_like_external_file <- function(path) {
  fn <- tolower(basename(as.character(path)))
  grepl(
    "metadata|meta[_-]?data|sample[_-]?info|sample[_-]?annotation|annotation|phenotype|pheno|clinical|protocol|readme|supplementary[_-]?table|gsm.*meta",
    fn,
    perl = TRUE
  )
}

is_expression_like_external_file <- function(path) {
  fn <- tolower(basename(as.character(path)))
  if (is_metadata_like_external_file(path)) return(FALSE)

  grepl(
    "raw[_-]?data|rawdata|count|counts|matrix|expression|expr|exprs|normalized|normalised",
    fn,
    perl = TRUE
  )
}

read_tsv_any <- function(path, header = FALSE) {
  con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, "rt") else file(path, "rt")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  utils::read.delim(con, header = header, stringsAsFactors = FALSE, check.names = FALSE)
}

find_marker_panel_file <- function() {
  files <- list.files(tables_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  files <- files[grepl("04A|marker|panel|signature", files, ignore.case = TRUE)]

  if (length(files) == 0) return(NA_character_)

  score_file <- function(f) {
    dt <- tryCatch(data.table::fread(f, nrows = 50, showProgress = FALSE), error = function(e) data.table())
    if (ncol(dt) == 0) return(0)
    cn <- tolower(names(dt))
    has_gene <- any(cn %in% c("gene", "genes", "symbol", "gene_symbol", "marker_gene", "external_gene_name"))
    has_cat <- any(cn %in% c("category", "signature", "module", "marker_category", "program", "gene_set", "cell_type", "class"))
    as.integer(has_gene) + as.integer(has_cat) + as.integer(grepl("04A", f, ignore.case = TRUE))
  }

  scores <- vapply(files, score_file, numeric(1))
  files[which.max(scores)]
}

standardize_marker_panel <- function(path) {
  dt <- data.table::fread(path, data.table = TRUE, showProgress = FALSE)
  original_names <- names(dt)
  cn <- tolower(names(dt))

  gene_candidates <- c("gene", "genes", "symbol", "gene_symbol", "marker_gene", "external_gene_name")
  category_candidates <- c("category", "signature", "module", "marker_category", "program", "gene_set", "cell_type", "class")

  gene_col <- original_names[match(intersect(gene_candidates, cn)[1], cn)]
  category_col <- original_names[match(intersect(category_candidates, cn)[1], cn)]

  if (is.na(gene_col) || is.na(category_col)) {
    stop("无法从 frozen marker panel 识别 gene/category 列：", path)
  }

  out <- data.table(
    category = as.character(dt[[category_col]]),
    gene = toupper(as.character(dt[[gene_col]]))
  )

  out <- out[!is.na(category) & category != "" & !is.na(gene) & gene != ""]
  out[, gene := gsub("\\s+", "", gene)]
  out <- unique(out)

  out
}

sanitize_feature <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

infer_condition_from_name <- function(x) {
  x0 <- as.character(x)
  xl <- tolower(x0)

  treatment <- ifelse(grepl("pink", xl), "PINK1",
                      ifelse(grepl("control|ctrl", xl), "Control", "Unknown"))

  day <- ifelse(grepl("d0?6|day.?0?6|_06|d6", xl), "D06",
                ifelse(grepl("d10|day.?10", xl), "D10",
                       ifelse(grepl("d15|day.?15", xl), "D15",
                              ifelse(grepl("d21|day.?21", xl), "D21",
                                     ifelse(grepl("ipsc|ips", xl), "iPSC", "Unknown")))))

  paste(treatment, day, sep = "_")
}

normalize_counts_log1p_cpm <- function(counts) {

  x <- counts@x
  finite_x <- x[is.finite(x)]

  if (length(finite_x) == 0) {
    audit <- data.table(
      normalization_mode = "empty_or_nonfinite_matrix",
      min_value = NA_real_,
      max_value = NA_real_,
      fraction_negative_nonzero = NA_real_,
      fraction_integer_like_nonzero = NA_real_,
      reason = "No finite non-zero values detected; returned matrix unchanged."
    )
    attr(counts, "normalization_audit") <- audit
    return(counts)
  }

  min_x <- min(finite_x, na.rm = TRUE)
  max_x <- max(finite_x, na.rm = TRUE)
  frac_neg <- mean(finite_x < 0, na.rm = TRUE)
  frac_integer_like <- mean(abs(finite_x - round(finite_x)) < 1e-8, na.rm = TRUE)

  if (is.finite(min_x) && min_x < 0) {
    audit <- data.table(
      normalization_mode = "used_as_processed_expression_negative_values_detected",
      min_value = min_x,
      max_value = max_x,
      fraction_negative_nonzero = frac_neg,
      fraction_integer_like_nonzero = frac_integer_like,
      reason = "Negative values detected; matrix was treated as processed/log/normalized expression and was not log1p-transformed."
    )
    attr(counts, "normalization_audit") <- audit
    return(counts)
  }

  if (is.finite(max_x) && max_x <= 50 && frac_integer_like < 0.80) {
    audit <- data.table(
      normalization_mode = "used_as_processed_expression_nonnegative_loglike",
      min_value = min_x,
      max_value = max_x,
      fraction_negative_nonzero = frac_neg,
      fraction_integer_like_nonzero = frac_integer_like,
      reason = "Values were non-negative but log-like/non-integer; matrix was treated as processed expression and was not log1p CPM transformed."
    )
    attr(counts, "normalization_audit") <- audit
    return(counts)
  }

  cs <- Matrix::colSums(counts)
  cs[!is.finite(cs) | cs <= 0] <- 1
  norm <- Matrix::t(Matrix::t(counts) / cs) * 10000
  norm@x <- log1p(pmax(norm@x, 0))

  audit <- data.table(
    normalization_mode = "log1p_CPM_from_count_like_matrix",
    min_value = min_x,
    max_value = max_x,
    fraction_negative_nonzero = frac_neg,
    fraction_integer_like_nonzero = frac_integer_like,
    reason = "Non-negative count-like matrix detected; applied log1p(CPM+1) normalization."
  )
  attr(norm, "normalization_audit") <- audit
  norm
}

matrix_row_mean_sparse <- function(mat, genes) {
  genes <- intersect(genes, rownames(mat))
  if (length(genes) == 0) return(rep(NA_real_, ncol(mat)))
  as.numeric(Matrix::colMeans(mat[genes, , drop = FALSE]))
}

get_feature_cols <- function(dt) {
  exclude <- c(
    "task",
    "weak_label",
    "dataset",
    "object_id",
    "group_id",
    "group_key",
    "safety_contrast_class_05B",
    "n_cells",
    "sample_weight_equal",
    "sample_weight_sqrt_cells",
    "row_id",
    "fold",
    "predicted_probability"
  )
  numeric_cols <- names(dt)[vapply(dt, function(z) is.numeric(z) || is.integer(z), logical(1))]
  setdiff(numeric_cols, exclude)
}

prep_fit <- function(train_dt, feature_cols) {
  med <- sapply(feature_cols, function(fc) {
    x <- num(train_dt[[fc]])
    if (all(is.na(x))) return(0)
    median(x, na.rm = TRUE)
  })

  sdv <- sapply(feature_cols, function(fc) {
    x <- num(train_dt[[fc]])
    x[is.na(x)] <- med[[fc]]
    s <- sd(x, na.rm = TRUE)
    if (!is.finite(s) || s == 0) s <- 1
    s
  })

  list(median = med, sd = sdv)
}

prep_apply <- function(dt, feature_cols, prep, scale = TRUE) {
  mat <- matrix(NA_real_, nrow = nrow(dt), ncol = length(feature_cols))
  colnames(mat) <- make.names(feature_cols, unique = TRUE)

  for (j in seq_along(feature_cols)) {
    fc <- feature_cols[[j]]
    x <- num(dt[[fc]])
    x[is.na(x)] <- prep$median[[fc]]
    if (scale) {
      x <- (x - prep$median[[fc]]) / prep$sd[[fc]]
    }
    mat[, j] <- x
  }

  as.data.frame(mat, check.names = FALSE)
}

fit_full_logistic_predict <- function(train_dt, external_dt, feature_cols) {
  if (nrow(train_dt) < 5 || length(unique(train_dt$weak_label)) < 2) {
    return(list(success = FALSE, message = "training data too small or one class only", probs = rep(NA_real_, nrow(external_dt))))
  }

  prep <- prep_fit(train_dt, feature_cols)
  x_train <- prep_apply(train_dt, feature_cols, prep, scale = TRUE)
  x_external <- prep_apply(external_dt, feature_cols, prep, scale = TRUE)

  train_df <- data.frame(
    weak_label = as.integer(train_dt$weak_label),
    x_train,
    check.names = FALSE
  )

  formula_txt <- paste("weak_label ~", paste(colnames(x_train), collapse = " + "))

  fit <- tryCatch({
    suppressWarnings(stats::glm(as.formula(formula_txt), data = train_df, family = stats::binomial()))
  }, error = function(e) e)

  if (inherits(fit, "error")) {
    return(list(success = FALSE, message = conditionMessage(fit), probs = rep(NA_real_, nrow(external_dt))))
  }

  probs <- tryCatch({
    as.numeric(stats::predict(fit, newdata = x_external, type = "response"))
  }, error = function(e) rep(NA_real_, nrow(external_dt)))

  list(success = TRUE, message = "ok", probs = probs)
}

fit_full_rf_predict <- function(train_dt, external_dt, feature_cols) {
  if (!HAS_RANDOMFOREST) {
    return(list(success = FALSE, message = "randomForest not installed", probs = rep(NA_real_, nrow(external_dt))))
  }

  if (nrow(train_dt) < 10 || length(unique(train_dt$weak_label)) < 2) {
    return(list(success = FALSE, message = "training data too small or one class only", probs = rep(NA_real_, nrow(external_dt))))
  }

  prep <- prep_fit(train_dt, feature_cols)
  x_train <- prep_apply(train_dt, feature_cols, prep, scale = FALSE)
  x_external <- prep_apply(external_dt, feature_cols, prep, scale = FALSE)

  y_train <- factor(as.integer(train_dt$weak_label), levels = c(0, 1))

  fit <- tryCatch({
    randomForest::randomForest(
      x = x_train,
      y = y_train,
      ntree = 500,
      mtry = max(1, floor(sqrt(ncol(x_train)))),
      importance = TRUE
    )
  }, error = function(e) e)

  if (inherits(fit, "error")) {
    return(list(success = FALSE, message = conditionMessage(fit), probs = rep(NA_real_, nrow(external_dt))))
  }

  probs <- tryCatch({
    pr <- stats::predict(fit, newdata = x_external, type = "prob")
    if ("1" %in% colnames(pr)) as.numeric(pr[, "1"]) else as.numeric(pr[, ncol(pr)])
  }, error = function(e) rep(NA_real_, nrow(external_dt)))

  list(success = TRUE, message = "ok", probs = probs)
}

stamp("读取 09D V8 eligibility audit。")

elig09D <- read_required_csv(input_09D_eligibility)

primary09D <- elig09D[
  gse_id == EXTERNAL_GSE_ID &
    external_validation_tier == "tier1_primary_frozen_validation_candidate"
]

if (nrow(primary09D) == 0) {
  stop("09D V8 没有把 ", EXTERNAL_GSE_ID, " 标记为 tier1 primary candidate，停止 09E。")
}

decision_audit <- data.table(
  gse_id = EXTERNAL_GSE_ID,
  confirmed_primary_candidate = TRUE,
  role = primary09D$recommended_validation_role_after_audit[1],
  eligibility_score = primary09D$eligibility_score[1],
  selection_basis = if ("final_selection_basis" %in% names(primary09D)) primary09D$final_selection_basis[1] else NA_character_,
  decision_file = input_09D_decision
)

atomic_write_csv(as.data.frame(decision_audit), external_metadata_audit_csv)

stamp("获取 GSE183248 GEO supplementary file list。")

supp_url <- gse_supp_url(EXTERNAL_GSE_ID)
supp_html <- safe_read_lines_url(supp_url)
hrefs <- extract_href(supp_html)

supp_files <- hrefs[!grepl("/$", hrefs)]
supp_files <- supp_files[grepl(EXTERNAL_GSE_ID, supp_files, ignore.case = TRUE) | grepl("\\.(tar|gz|zip|txt|tsv|csv|rds|rda|h5|hdf5|mtx|loom)$", supp_files, ignore.case = TRUE)]

if (length(supp_files) == 0) {

  supp_files <- paste0(EXTERNAL_GSE_ID, "_RAW.tar")
}

supp_dt <- data.table(
  gse_id = EXTERNAL_GSE_ID,
  supp_url = supp_url,
  file_name = basename(supp_files),
  file_url = ifelse(grepl("^https?://", supp_files), supp_files, paste0(supp_url, supp_files))
)

if (DOWNLOAD_SUPPLEMENTARY) {
  stamp("下载 supplementary files。")

  dl_list <- list()
  ext_list <- list()

  for (i in seq_len(nrow(supp_dt))) {
    dest <- file.path(external_raw_dir, supp_dt$file_name[i])
    dl <- download_file_safe(supp_dt$file_url[i], dest)
    dl_list[[length(dl_list) + 1L]] <- dl

    if (dl$status %in% c("downloaded", "exists_skipped") && file.exists(dest)) {
      ext <- extract_archive_safe(dest, external_extract_dir)
      ext_list[[length(ext_list) + 1L]] <- ext
    }
  }

  download_audit <- rbindlist(dl_list, fill = TRUE)
  extraction_audit <- rbindlist(ext_list, fill = TRUE)

  supp_dt <- merge(
    supp_dt,
    download_audit[, .(file_url = url, local_path = dest, download_status = status, size_bytes, download_message = message)],
    by = "file_url",
    all.x = TRUE
  )

  if (nrow(extraction_audit) > 0) {
    supp_dt <- merge(
      supp_dt,
      extraction_audit[, .(local_path = file, extracted_to, extraction_status, extraction_message = message)],
      by = "local_path",
      all.x = TRUE
    )
  }
} else {
  supp_dt[, local_path := NA_character_]
  supp_dt[, download_status := "download_skipped_by_user_setting"]
  supp_dt[, size_bytes := NA_real_]
}

atomic_write_csv(as.data.frame(supp_dt), external_file_inventory_csv)

stamp("扫描 external files / matrix candidates。")

scan_roots <- unique(c(external_raw_dir, external_extract_dir))
all_files <- unique(unlist(lapply(scan_roots, function(d) {
  if (dir.exists(d)) list.files(d, recursive = TRUE, full.names = TRUE) else character()
})))

file_inventory <- data.table(
  path = all_files,
  file_name = basename(all_files),
  dir = dirname(all_files),
  size_bytes = ifelse(file.exists(all_files), file.info(all_files)$size, NA_real_),
  ext = tolower(tools::file_ext(gsub("\\.gz$", "", all_files, ignore.case = TRUE)))
)

file_inventory[, file_type_guess := fifelse(
  grepl("matrix\\.mtx(\\.gz)?$", file_name, ignore.case = TRUE),
  "10x_matrix_mtx",
  fifelse(
    grepl("(barcodes|barcode).*\\.tsv(\\.gz)?$", file_name, ignore.case = TRUE),
    "10x_barcodes",
    fifelse(
      grepl("(features|genes).*\\.tsv(\\.gz)?$", file_name, ignore.case = TRUE),
      "10x_features_genes",
      fifelse(
        grepl("\\.(rds|rda)$", file_name, ignore.case = TRUE),
        "R_object",
        fifelse(
          grepl("\\.(csv|tsv|txt)(\\.gz)?$", file_name, ignore.case = TRUE),
          "text_table",
          fifelse(
            grepl("\\.(h5|hdf5)$", file_name, ignore.case = TRUE),
            "h5_candidate",
            "other"
          )
        )
      )
    )
  )
)]

atomic_write_csv(as.data.frame(file_inventory), external_file_inventory_csv)

mtx_files <- file_inventory[file_type_guess == "10x_matrix_mtx"]$path

tenx_dirs <- unique(dirname(mtx_files))

tenx_candidates <- rbindlist(lapply(tenx_dirs, function(d) {
  files_d <- list.files(d, full.names = TRUE)
  fn <- basename(files_d)

  barcode <- files_d[grepl("(barcodes|barcode).*\\.tsv(\\.gz)?$", fn, ignore.case = TRUE)][1]
  feature <- files_d[grepl("(features|genes).*\\.tsv(\\.gz)?$", fn, ignore.case = TRUE)][1]
  matrix <- files_d[grepl("matrix\\.mtx(\\.gz)?$", fn, ignore.case = TRUE)][1]

  data.table(
    matrix_type = "10x_mtx",
    matrix_path = matrix,
    feature_path = feature,
    barcode_path = barcode,
    source_dir = d,
    sample_id = basename(d),
    import_priority = 1L,
    ready_to_import = !is.na(matrix) & !is.na(feature) & !is.na(barcode)
  )
}), fill = TRUE)

r_object_candidates <- file_inventory[file_type_guess == "R_object", .(
  matrix_type = "R_object",
  matrix_path = path,
  feature_path = NA_character_,
  barcode_path = NA_character_,
  source_dir = dir,
  sample_id = tools::file_path_sans_ext(file_name),
  import_priority = 2L,
  ready_to_import = TRUE
)]

text_candidates <- file_inventory[
  file_type_guess == "text_table" &
    vapply(path, is_expression_like_external_file, logical(1)),
  .(
    matrix_type = "text_table",
    matrix_path = path,
    feature_path = NA_character_,
    barcode_path = NA_character_,
    source_dir = dir,
    sample_id = tools::file_path_sans_ext(gsub("\\.gz$", "", file_name, ignore.case = TRUE)),
    import_priority = 3L,
    ready_to_import = TRUE
  )
]

matrix_candidates <- rbindlist(list(tenx_candidates, r_object_candidates, text_candidates), fill = TRUE)
matrix_candidates <- matrix_candidates[ready_to_import == TRUE]

if (nrow(matrix_candidates) > 0) {
  matrix_candidates[, metadata_like_excluded := vapply(matrix_path, is_metadata_like_external_file, logical(1))]
  matrix_candidates[, expression_like_candidate := vapply(matrix_path, is_expression_like_external_file, logical(1))]
  matrix_candidates <- matrix_candidates[
    matrix_type != "text_table" | metadata_like_excluded == FALSE
  ]
}

setorder(matrix_candidates, import_priority, matrix_path)

atomic_write_csv(as.data.frame(matrix_candidates), external_matrix_inventory_csv)

if (nrow(matrix_candidates) == 0) {
  plot_empty_pdf(fig_gene_overlap_pdf, "09E gene overlap audit", "No importable external matrix detected.")
  plot_empty_pdf(fig_score_heatmap_pdf, "09E external signature score heatmap", "No importable external matrix detected.")
  plot_empty_pdf(fig_prediction_pdf, "09E external frozen predictor probabilities", "No importable external matrix detected.")

  writeLines(c(
    "09E stopped before scoring because no importable matrix was detected.",
    "This is a data-accessibility failure, not a biological validation failure.",
    "Check 09E_external_GSE183248_file_inventory.csv and manually inspect supplementary files."
  ), method_note_txt)

  stop("09E 没有检测到可导入的 external expression matrix。请查看 file inventory。")
}

stamp("导入 external expression matrix。")

import_audits <- list()
counts_list <- list()
metadata_list <- list()

import_one_10x <- function(row) {
  mat <- Matrix::readMM(row$matrix_path)

  genes <- read_tsv_any(row$feature_path, header = FALSE)
  barcodes <- read_tsv_any(row$barcode_path, header = FALSE)

  gene_symbols <- if (ncol(genes) >= 2) genes[[2]] else genes[[1]]
  gene_symbols <- toupper(as.character(gene_symbols))
  gene_symbols[is.na(gene_symbols) | gene_symbols == ""] <- paste0("GENE_", seq_len(length(gene_symbols)))[is.na(gene_symbols) | gene_symbols == ""]

  cell_ids <- as.character(barcodes[[1]])
  cell_ids <- paste(row$sample_id, cell_ids, sep = "_")

  rownames(mat) <- make.unique(gene_symbols)
  colnames(mat) <- make.unique(cell_ids)

  mat <- as(mat, "dgCMatrix")

  meta <- data.table(
    cell_id = colnames(mat),
    sample = row$sample_id,
    source_dir = row$source_dir,
    external_condition = infer_condition_from_name(row$sample_id)
  )

  list(counts = mat, meta = meta)
}

import_one_text <- function(row) {
  dt <- data.table::fread(row$matrix_path, data.table = FALSE, showProgress = FALSE)

  if (ncol(dt) < 3) stop("text matrix has <3 columns: ", row$matrix_path)

  gene_col <- dt[[1]]
  expr <- dt[, -1, drop = FALSE]
  expr[] <- lapply(expr, function(x) suppressWarnings(as.numeric(x)))

  mat <- as.matrix(expr)
  rownames(mat) <- make.unique(toupper(as.character(gene_col)))
  colnames(mat) <- make.unique(colnames(expr))

  mat[is.na(mat)] <- 0
  mat <- Matrix::Matrix(mat, sparse = TRUE)

  meta <- data.table(
    cell_id = colnames(mat),
    sample = row$sample_id,
    source_dir = row$source_dir,
    external_condition = infer_condition_from_name(colnames(mat))
  )

  list(counts = mat, meta = meta)
}

import_one_r_object <- function(row) {
  obj <- readRDS(row$matrix_path)

  if (HAS_SEURAT && inherits(obj, "Seurat")) {
    assay_use <- if ("RNA" %in% names(obj@assays)) "RNA" else names(obj@assays)[1]
    mat <- tryCatch({
      Seurat::GetAssayData(obj, assay = assay_use, slot = "counts")
    }, error = function(e) {
      Seurat::GetAssayData(obj, assay = assay_use, slot = "data")
    })

    mat <- as(mat, "dgCMatrix")

    meta <- as.data.table(obj@meta.data, keep.rownames = "cell_id")
    if (!"sample" %in% names(meta)) meta[, sample := row$sample_id]
    if (!"external_condition" %in% names(meta)) meta[, external_condition := infer_condition_from_name(sample)]

    return(list(counts = mat, meta = meta))
  }

  if (inherits(obj, "dgCMatrix") || inherits(obj, "dgTMatrix") || inherits(obj, "matrix")) {
    mat <- as(obj, "dgCMatrix")
    meta <- data.table(
      cell_id = colnames(mat),
      sample = row$sample_id,
      source_dir = row$source_dir,
      external_condition = infer_condition_from_name(colnames(mat))
    )
    return(list(counts = mat, meta = meta))
  }

  stop("R object is not Seurat or matrix-like: ", row$matrix_path)
}

n_import <- min(nrow(matrix_candidates), MAX_MATRICES_TO_IMPORT)

for (i in seq_len(n_import)) {
  row <- matrix_candidates[i]

  if (row$matrix_type == "text_table" && is_metadata_like_external_file(row$matrix_path)) {
    import_audits[[length(import_audits) + 1L]] <- data.table(
      matrix_path = row$matrix_path,
      matrix_type = row$matrix_type,
      sample_id = row$sample_id,
      import_status = "skipped_metadata_like_file",
      message = "Excluded by V5 metadata-like file filter; not an expression matrix.",
      n_genes = NA_integer_,
      n_cells = NA_integer_
    )
    next
  }

  res <- tryCatch({
    if (row$matrix_type == "10x_mtx") {
      import_one_10x(row)
    } else if (row$matrix_type == "text_table") {
      import_one_text(row)
    } else if (row$matrix_type == "R_object") {
      import_one_r_object(row)
    } else {
      stop("unsupported matrix_type: ", row$matrix_type)
    }
  }, error = function(e) {
    e
  })

  if (inherits(res, "error")) {
    import_audits[[length(import_audits) + 1L]] <- data.table(
      matrix_path = row$matrix_path,
      matrix_type = row$matrix_type,
      sample_id = row$sample_id,
      import_status = "failed",
      message = conditionMessage(res),
      n_genes = NA_integer_,
      n_cells = NA_integer_
    )
  } else {
    mat <- res$counts
    meta <- res$meta

    if (row$matrix_type == "text_table" && is_metadata_like_external_file(row$sample_id)) {
      import_audits[[length(import_audits) + 1L]] <- data.table(
        matrix_path = row$matrix_path,
        matrix_type = row$matrix_type,
        sample_id = row$sample_id,
        import_status = "skipped_after_import_metadata_like_sample",
        message = "Imported object looked metadata-like by sample_id; excluded before merge.",
        n_genes = nrow(mat),
        n_cells = ncol(mat)
      )
      next
    }

    if (!"cell_id" %in% names(meta)) meta[, cell_id := colnames(mat)]
    meta <- meta[match(colnames(mat), cell_id)]
    meta[is.na(cell_id), cell_id := colnames(mat)[is.na(cell_id)]]

    counts_list[[length(counts_list) + 1L]] <- mat
    metadata_list[[length(metadata_list) + 1L]] <- meta

    import_audits[[length(import_audits) + 1L]] <- data.table(
      matrix_path = row$matrix_path,
      matrix_type = row$matrix_type,
      sample_id = row$sample_id,
      import_status = "success",
      message = NA_character_,
      n_genes = nrow(mat),
      n_cells = ncol(mat)
    )
  }
}

import_audit <- rbindlist(import_audits, fill = TRUE)
atomic_write_csv(as.data.frame(import_audit), external_import_audit_csv)

if (length(counts_list) == 0) {
  stop("所有 external matrix 导入失败。请查看 09E_external_GSE183248_import_audit.csv")
}

stamp("合并 external matrices。")

all_genes <- sort(unique(unlist(lapply(counts_list, rownames))))

align_matrix <- function(mat, genes) {
  missing <- setdiff(genes, rownames(mat))
  if (length(missing) > 0) {
    zero <- Matrix::Matrix(0, nrow = length(missing), ncol = ncol(mat), sparse = TRUE)
    rownames(zero) <- missing
    colnames(zero) <- colnames(mat)
    mat <- rbind(mat, zero)
  }
  mat[genes, , drop = FALSE]
}

counts_aligned <- lapply(counts_list, align_matrix, genes = all_genes)
external_counts <- do.call(Matrix::cbind2, counts_aligned)
external_meta <- rbindlist(metadata_list, fill = TRUE)

external_meta <- external_meta[match(colnames(external_counts), cell_id)]
external_meta[is.na(cell_id), cell_id := colnames(external_counts)[is.na(cell_id)]]

if (any(duplicated(rownames(external_counts)))) {
  stamp("检测到重复 gene symbols：按 gene symbol 合并求和。")
  gene_factor <- factor(rownames(external_counts), levels = unique(rownames(external_counts)))
  external_counts <- rowsum(as.matrix(external_counts), group = gene_factor, reorder = FALSE)
  external_counts <- Matrix::Matrix(external_counts, sparse = TRUE)
}

saveRDS(external_counts, external_matrix_rds)

atomic_write_csv(as.data.frame(external_meta), cell_metadata_csv)

if (HAS_SEURAT) {
  seu <- tryCatch({
    obj <- Seurat::CreateSeuratObject(counts = external_counts, meta.data = as.data.frame(external_meta))
    obj <- Seurat::NormalizeData(obj, verbose = FALSE)
    obj
  }, error = function(e) e)

  if (!inherits(seu, "error")) {
    saveRDS(seu, external_seurat_rds)
  }
}

stamp("External matrix genes：", nrow(external_counts))
stamp("External matrix cells：", ncol(external_counts))

stamp("定位并读取 frozen 04A marker panel。")

marker_file <- find_marker_panel_file()

if (is.na(marker_file) || !file.exists(marker_file)) {
  stop("未找到 frozen 04A marker panel CSV。请确认 03_tables 下存在 04A marker panel 输出。")
}

marker_panel <- standardize_marker_panel(marker_file)

marker_audit <- data.table(
  marker_panel_file = marker_file,
  n_marker_rows = nrow(marker_panel),
  n_categories = uniqueN(marker_panel$category),
  n_unique_genes = uniqueN(marker_panel$gene)
)

atomic_write_csv(as.data.frame(marker_audit), marker_panel_audit_csv)

stamp("计算 external frozen signature scores。")

external_norm <- normalize_counts_log1p_cpm(external_counts)

normalization_audit <- attr(external_norm, "normalization_audit")
if (is.null(normalization_audit)) {
  normalization_audit <- data.table(
    normalization_mode = "unknown",
    min_value = NA_real_,
    max_value = NA_real_,
    fraction_negative_nonzero = NA_real_,
    fraction_integer_like_nonzero = NA_real_,
    reason = "Normalization audit attribute was not available."
  )
}
atomic_write_csv(as.data.frame(normalization_audit), normalization_audit_csv)

categories <- sort(unique(marker_panel$category))

score_mat <- matrix(NA_real_, nrow = ncol(external_norm), ncol = length(categories))
rownames(score_mat) <- colnames(external_norm)
colnames(score_mat) <- paste0("score_", sanitize_feature(categories))

overlap_audit <- rbindlist(lapply(categories, function(cat) {
  genes <- unique(marker_panel[category == cat]$gene)
  present <- intersect(genes, rownames(external_norm))

  score_mat[, paste0("score_", sanitize_feature(cat))] <<- matrix_row_mean_sparse(external_norm, present)

  data.table(
    category = cat,
    score_column = paste0("score_", sanitize_feature(cat)),
    n_marker_genes = length(genes),
    n_overlap_genes = length(present),
    overlap_fraction = ifelse(length(genes) > 0, length(present) / length(genes), NA_real_),
    enough_overlap = length(present) >= MIN_MARKER_OVERLAP_PER_SIGNATURE,
    missing_genes = paste(setdiff(genes, present), collapse = ";")
  )
}), fill = TRUE)

score_dt <- as.data.table(score_mat, keep.rownames = "cell_id")
score_dt <- merge(external_meta, score_dt, by = "cell_id", all.y = TRUE)

atomic_write_csv(as.data.frame(overlap_audit), gene_overlap_audit_csv)
atomic_write_csv(as.data.frame(score_dt), cell_score_csv)

stamp("生成 external group-level score summary。")

available_group_fields <- intersect(PREFERRED_GROUP_FIELDS, names(score_dt))
if (length(available_group_fields) == 0) {
  score_dt[, external_group := "GSE183248_all_cells"]
} else {
  group_field <- available_group_fields[1]
  score_dt[, external_group := as.character(get(group_field))]
  score_dt[is.na(external_group) | external_group == "", external_group := "Unknown"]
}

score_cols <- names(score_dt)[grepl("^score_", names(score_dt))]

group_summary <- score_dt[, c(
  list(
    dataset = EXTERNAL_GSE_ID,
    n_cells = .N
  ),
  lapply(.SD, function(x) mean(num(x), na.rm = TRUE))
), by = external_group, .SDcols = score_cols]

for (sc in score_cols) {
  med_sc <- median(num(score_dt[[sc]]), na.rm = TRUE)
  tmp <- score_dt[, .(pct_gt_global_median = mean(num(get(sc)) > med_sc, na.rm = TRUE)), by = external_group]
  setnames(tmp, "pct_gt_global_median", paste0("pct_cells_", sc, "_gt_global_median"))
  group_summary <- merge(group_summary, tmp, by = "external_group", all.x = TRUE)
}

atomic_write_csv(as.data.frame(group_summary), group_score_summary_csv)

stamp("构建 external reduced feature table。")

external_features <- copy(group_summary)
external_features[, group_id := external_group]
external_features[, object_id := paste0(EXTERNAL_GSE_ID, "_external")]
external_features[, dataset := EXTERNAL_GSE_ID]

for (sc in score_cols) {
  base <- sub("^score_", "", sc)
  marker_name <- paste0("marker_", base)
  if (!marker_name %in% names(external_features)) {
    external_features[, (marker_name) := get(sc)]
  }
}

a9_cols <- names(external_features)[grepl("A9", names(external_features), ignore.case = TRUE)]
a10_cols <- names(external_features)[grepl("A10", names(external_features), ignore.case = TRUE)]

if (length(a9_cols) > 0 && length(a10_cols) > 0) {
  a9_use <- a9_cols[1]
  a10_use <- a10_cols[1]
  external_features[, A9_minus_A10_score_05A := num(get(a9_use)) - num(get(a10_use))]
  external_features[, pct_cells_A9_minus_A10_score_05A_gt0 := as.numeric(A9_minus_A10_score_05A > 0)]
}

atomic_write_csv(as.data.frame(external_features), external_feature_table_csv)

stamp("应用 discovery-only frozen predictor（如果特征可对齐）。")

prediction_outputs <- list()
alignment_outputs <- list()

if (file.exists(input_ideal_reduced) && file.exists(input_safety_reduced)) {
  train_list <- list(
    ideal_like_classifier = read_required_csv(input_ideal_reduced),
    safety_risk_classifier = read_required_csv(input_safety_reduced)
  )

  for (task_name in names(train_list)) {
    train_dt <- copy(train_list[[task_name]])
    train_dt[, weak_label := as.integer(weak_label)]

    feature_cols <- get_feature_cols(train_dt)

    missing_external <- setdiff(feature_cols, names(external_features))
    available_external <- intersect(feature_cols, names(external_features))

    alignment_outputs[[length(alignment_outputs) + 1L]] <- data.table(
      task_name = task_name,
      n_required_features = length(feature_cols),
      n_available_external_features = length(available_external),
      n_missing_external_features = length(missing_external),
      feature_alignment_fraction = ifelse(length(feature_cols) > 0, length(available_external) / length(feature_cols), NA_real_),
      missing_features = paste(missing_external, collapse = ";"),
      prediction_ready = length(missing_external) == 0
    )

    if (length(missing_external) == 0 && length(feature_cols) > 0) {
      glm_res <- fit_full_logistic_predict(train_dt, external_features, feature_cols)
      rf_res <- fit_full_rf_predict(train_dt, external_features, feature_cols)

      pred_dt <- data.table(
        dataset = EXTERNAL_GSE_ID,
        external_group = external_features$external_group,
        group_id = external_features$group_id,
        object_id = external_features$object_id,
        task_name = task_name,
        logistic_success = glm_res$success,
        logistic_message = glm_res$message,
        logistic_predicted_probability = glm_res$probs,
        random_forest_success = rf_res$success,
        random_forest_message = rf_res$message,
        random_forest_predicted_probability = rf_res$probs
      )

      prediction_outputs[[length(prediction_outputs) + 1L]] <- pred_dt
    }
  }
} else {
  alignment_outputs[[length(alignment_outputs) + 1L]] <- data.table(
    task_name = "all",
    n_required_features = NA_integer_,
    n_available_external_features = NA_integer_,
    n_missing_external_features = NA_integer_,
    feature_alignment_fraction = NA_real_,
    missing_features = "09B reduced feature training tables not found",
    prediction_ready = FALSE
  )
}

alignment_audit <- rbindlist(alignment_outputs, fill = TRUE)
atomic_write_csv(as.data.frame(alignment_audit), ml_feature_alignment_csv)

if (length(prediction_outputs) > 0) {
  pred_all <- rbindlist(prediction_outputs, fill = TRUE)

  pred_wide <- dcast(
    pred_all,
    dataset + external_group + group_id + object_id ~ task_name,
    value.var = c("logistic_predicted_probability", "random_forest_predicted_probability")
  )

  ideal_col <- "logistic_predicted_probability_ideal_like_classifier"
  safety_col <- "logistic_predicted_probability_safety_risk_classifier"

  if (all(c(ideal_col, safety_col) %in% names(pred_wide))) {
    pred_wide[, exploratory_external_class := fifelse(
      get(ideal_col) >= 0.5 & get(safety_col) < 0.5,
      "external_ideal_like_probability_high_safety_low_like",
      fifelse(
        get(safety_col) >= 0.5,
        "external_safety_risk_probability_high",
        "external_mixed_or_uncertain"
      )
    )]
  }

  atomic_write_csv(as.data.frame(pred_wide), external_prediction_csv)
} else {
  pred_wide <- data.table(
    message = "Frozen predictor probabilities were not generated because external features did not fully align with 09B reduced features."
  )
  atomic_write_csv(as.data.frame(pred_wide), external_prediction_csv)
}

stamp("生成 09E PDF figures。")

if (nrow(overlap_audit) > 0) {
  ov <- copy(overlap_audit)
  ov <- ov[order(overlap_fraction)]
  ov[, label := paste0(category, " (", n_overlap_genes, "/", n_marker_genes, ")")]

  safe_pdf(fig_gene_overlap_pdf, width = 12.5, height = max(7.5, 0.32 * nrow(ov) + 2.8))
  par(mar = c(5.2, 16.5, 4.2, 2.0))

  barplot(
    ov$overlap_fraction,
    names.arg = ov$label,
    horiz = TRUE,
    las = 1,
    xlab = "External gene overlap fraction",
    main = "09E frozen marker gene overlap in GSE183248",
    col = ifelse(ov$enough_overlap, "grey55", "grey85"),
    border = "grey25",
    cex.names = 0.58,
    cex.axis = 0.90,
    xlim = c(0, 1)
  )

  finish_pdf(fig_gene_overlap_pdf)
} else {
  plot_empty_pdf(fig_gene_overlap_pdf, "09E gene overlap audit", "No marker overlap results.")
}

score_plot_cols <- score_cols[colSums(is.na(as.matrix(score_dt[, ..score_cols]))) < nrow(score_dt)]
if (nrow(group_summary) > 0 && length(score_plot_cols) > 0) {
  heat_dt <- copy(group_summary)
  mat <- as.matrix(heat_dt[, ..score_plot_cols])
  rownames(mat) <- heat_dt$external_group

  mat_scaled <- scale(mat)
  mat_scaled[is.na(mat_scaled)] <- 0
  mat_scaled[mat_scaled > 2] <- 2
  mat_scaled[mat_scaled < -2] <- -2

  safe_pdf(fig_score_heatmap_pdf, width = 12.5, height = max(6.8, 0.35 * nrow(mat_scaled) + 2.5))
  par(mar = c(9.5, 12.5, 4.0, 2.0))

  image(
    x = seq_len(ncol(mat_scaled)),
    y = seq_len(nrow(mat_scaled)),
    z = t(mat_scaled[nrow(mat_scaled):1, , drop = FALSE]),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = "09E external frozen signature score heatmap"
  )
  axis(1, at = seq_len(ncol(mat_scaled)), labels = gsub("^score_", "", colnames(mat_scaled)), las = 2, cex.axis = 0.58)
  axis(2, at = seq_len(nrow(mat_scaled)), labels = rev(rownames(mat_scaled)), las = 1, cex.axis = 0.70)
  box()

  finish_pdf(fig_score_heatmap_pdf)
} else {
  plot_empty_pdf(fig_score_heatmap_pdf, "09E external signature score heatmap", "No score matrix available.")
}

if (exists("pred_wide") && nrow(pred_wide) > 0 && !("message" %in% names(pred_wide))) {
  prob_cols <- names(pred_wide)[grepl("predicted_probability", names(pred_wide))]
  if (length(prob_cols) > 0) {
    pred_long <- melt(
      pred_wide,
      id.vars = c("external_group"),
      measure.vars = prob_cols,
      variable.name = "model_probability",
      value.name = "probability"
    )
    pred_long <- pred_long[!is.na(probability)]

    if (nrow(pred_long) > 0) {
      pred_long[, label := paste(external_group, model_probability, sep = " | ")]
      pred_long <- pred_long[order(probability)]

      safe_pdf(fig_prediction_pdf, width = 12.5, height = max(7.2, 0.28 * nrow(pred_long) + 2.8))
      par(mar = c(5.2, 18.5, 4.2, 2.0))

      barplot(
        pred_long$probability,
        names.arg = pred_long$label,
        horiz = TRUE,
        las = 1,
        xlab = "Predicted probability",
        main = "09E frozen predictor probabilities in GSE183248",
        col = "grey60",
        border = "grey25",
        cex.names = 0.50,
        xlim = c(0, 1)
      )

      abline(v = 0.5, lty = 2, col = "grey45")

      finish_pdf(fig_prediction_pdf)
    } else {
      plot_empty_pdf(fig_prediction_pdf, "09E external frozen predictor probabilities", "No finite probabilities.")
    }
  } else {
    plot_empty_pdf(fig_prediction_pdf, "09E external frozen predictor probabilities", "Prediction columns not available.")
  }
} else {
  plot_empty_pdf(fig_prediction_pdf, "09E external frozen predictor probabilities", "Prediction skipped due to feature mismatch.")
}

method_lines <- c(
  "09E frozen external validation method and claim-boundary note",
  "",
  "Method-ready wording:",
  paste0(
    "After selecting GSE183248 as the primary external validation candidate in 09D, we applied the frozen transcriptomic prioritization framework to the external dataset. ",
    "The marker panel and feature definitions were not modified using external data. ",
    "External expression matrices were imported from GEO supplementary files where available, normalized using a fixed log1p CPM procedure, and scored using the frozen marker categories. ",
    "Group-level score summaries were generated for external validation. ",
    "Where reduced non-direct features could be matched exactly, discovery-trained marker-rule-derived classifiers were applied to estimate ideal-like and safety-risk-associated probabilities."
  ),
  "",
  "Claim boundary:",
  "09E supports external transcriptomic reproducibility if score patterns are biologically consistent.",
  "09E does not prove anatomical projection, graft integration, therapeutic efficacy, tumorigenicity, or clinical safety.",
  "External data are not used to retrain the model or tune thresholds.",
  "If feature alignment is incomplete, ML prediction is reported as skipped rather than forced."
)

writeLines(method_lines, method_note_txt)

plan_lines <- c(
  "09E to 09F external validation figure plan",
  "",
  "09F should polish the following outputs if 09E succeeds:",
  "1. External marker gene overlap audit figure.",
  "2. External frozen signature score heatmap.",
  "3. External group-level score barplots for DA-like / A9-A10 / projection-associated / safety-risk modules.",
  "4. Frozen predictor probability plots if feature alignment is complete.",
  "5. A concise external validation summary panel.",
  "",
  "Do not change the frozen 04A/05B/09C framework based on 09E results."
)

writeLines(plan_lines, next_step_plan_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

report_lines <- c(
  "09E frozen external validation GSE183248 report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Decision audit:",
  capture.output(print(decision_audit)),
  "",
  "Import audit:",
  capture.output(print(import_audit)),
  "",
  "Marker audit:",
  capture.output(print(marker_audit)),
  "",
  "Normalization audit:",
  capture.output(print(normalization_audit)),
  "",
  "Gene overlap summary:",
  capture.output(print(overlap_audit[, .(category, n_marker_genes, n_overlap_genes, overlap_fraction, enough_overlap)])),
  "",
  "ML feature alignment:",
  capture.output(print(alignment_audit)),
  "",
  "Output directories:",
  out_tables_dir,
  out_figures_dir,
  out_objects_dir
)

writeLines(report_lines, report_txt)

required_output_files <- c(
  external_metadata_audit_csv,
  external_file_inventory_csv,
  external_matrix_inventory_csv,
  external_import_audit_csv,
  marker_panel_audit_csv,
  gene_overlap_audit_csv,
  normalization_audit_csv,
  cell_score_csv,
  cell_metadata_csv,
  group_score_summary_csv,
  external_feature_table_csv,
  ml_feature_alignment_csv,
  external_prediction_csv,
  method_note_txt,
  next_step_plan_txt,
  session_info_txt,
  report_txt,
  fig_score_heatmap_pdf,
  fig_prediction_pdf,
  fig_gene_overlap_pdf,
  external_matrix_rds
)

output_check <- data.table(
  file = required_output_files,
  exists = file.exists(required_output_files),
  size_bytes = ifelse(file.exists(required_output_files), file.info(required_output_files)$size, NA_real_)
)

atomic_write_csv(as.data.frame(output_check), output_check_csv)

bad_outputs <- output_check[!exists | is.na(size_bytes) | size_bytes <= 0]
if (nrow(bad_outputs) > 0) {
  print(bad_outputs)
  stop("09E 输出验证失败。")
}

cat("\n============================================================\n")
cat("09E frozen external validation GSE183248 FINAL V6 FIX CLUSTER CELLID 运行结束\n")
cat("============================================================\n\n")

cat("External GSE：", EXTERNAL_GSE_ID, "\n")
cat("External genes：", nrow(external_counts), "\n")
cat("External cells：", ncol(external_counts), "\n")
cat("Marker categories：", uniqueN(marker_panel$category), "\n")
cat("Group summaries：", nrow(group_summary), "\n")
cat("ML prediction ready tasks：", sum(alignment_audit$prediction_ready == TRUE, na.rm = TRUE), "\n\n")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n")
cat(out_objects_dir, "\n\n")

cat("关键输出：\n")
cat(gene_overlap_audit_csv, "\n")
cat(normalization_audit_csv, "\n")
cat(cell_score_csv, "\n")
cat(group_score_summary_csv, "\n")
cat(external_feature_table_csv, "\n")
cat(ml_feature_alignment_csv, "\n")
cat(external_prediction_csv, "\n")
cat(method_note_txt, "\n\n")

cat("PDF 图：\n")
cat(fig_gene_overlap_pdf, "\n")
cat(fig_score_heatmap_pdf, "\n")
cat(fig_prediction_pdf, "\n\n")

cat("✅ 09E frozen external validation GSE183248 FINAL V6 FIX CLUSTER CELLID 完成。\n")

PROJECT_DIR <- "D:/PD_Graft_Project"

SEED <- 20260715

EXTERNAL_GSE_ID <- "GSE183248"

DEFAULT_K_CLUSTERS <- 8

N_HVG_FOR_EXTERNAL_CLUSTERING <- 2000

N_PCS <- 30

MIN_CLUSTER_CELLS <- 30

PDF_WIDTH <- 12
PDF_HEIGHT <- 7.5

cat("\n============================================================\n")
cat("09E3：GSE183248 external grouping and cluster-level recovery\n")
cat("============================================================\n\n")

options(stringsAsFactors = FALSE)

required_pkgs <- c("data.table", "Matrix")

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop("缺少 R 包，请先手动安装：", paste(missing_pkgs, collapse = ", "))
}

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
})

HAS_IRLBA <- requireNamespace("irlba", quietly = TRUE)
HAS_RANDOMFOREST <- requireNamespace("randomForest", quietly = TRUE)

if (HAS_RANDOMFOREST) {
  suppressPackageStartupMessages(library(randomForest))
}

set.seed(SEED)

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

v2_tables_dir <- file.path(
  tables_dir,
  "09E_frozen_external_validation_GSE183248_FINAL_V6_FIX_CLUSTER_CELLID"
)

v2_objects_dir <- file.path(
  objects_dir,
  "09E_external_GSE183248"
)

out_tables_dir <- file.path(
  tables_dir,
  "09E_frozen_external_validation_GSE183248_FINAL_V6_FIX_CLUSTER_CELLID"
)

out_figures_dir <- file.path(
  figures_dir,
  "09E_frozen_external_validation_GSE183248_FINAL_V6_FIX_CLUSTER_CELLID_pdf"
)

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

input_external_matrix_rds <- file.path(
  v2_objects_dir,
  "09E_GSE183248_external_expression_matrix.rds"
)

input_cell_scores <- file.path(
  v2_tables_dir,
  "09E_external_cell_signature_scores.csv"
)

input_cell_metadata <- file.path(
  v2_tables_dir,
  "09E_external_cell_metadata.csv"
)

input_normalization_audit <- file.path(
  v2_tables_dir,
  "09E_external_normalization_decision_audit.csv"
)

input_v2_predictions <- file.path(
  v2_tables_dir,
  "09E_external_frozen_predictor_probabilities.csv"
)

input_ideal_reduced <- file.path(
  tables_dir,
  "09B_ML_ready_dataset_and_leakage_audit_V3",
  "09B_ideal_like_training_reduced_non_direct_features.csv"
)

input_safety_reduced <- file.path(
  tables_dir,
  "09B_ML_ready_dataset_and_leakage_audit_V3",
  "09B_safety_risk_training_reduced_non_direct_features.csv"
)

grouping_audit_csv <- file.path(out_tables_dir, "09E3_grouping_recovery_audit.csv")
cell_grouping_csv <- file.path(out_tables_dir, "09E3_external_cell_grouping_assignments.csv")
cluster_score_summary_csv <- file.path(out_tables_dir, "09E3_external_cluster_score_summary.csv")
cluster_feature_table_csv <- file.path(out_tables_dir, "09E3_external_cluster_reduced_feature_table_for_prediction.csv")
cluster_ml_alignment_csv <- file.path(out_tables_dir, "09E3_external_cluster_ML_feature_alignment_audit.csv")
cluster_prediction_csv <- file.path(out_tables_dir, "09E3_external_cluster_frozen_predictor_probabilities.csv")
cluster_marker_profile_csv <- file.path(out_tables_dir, "09E3_external_cluster_marker_profile_summary.csv")
method_note_txt <- file.path(out_tables_dir, "09E3_method_and_claim_boundary_note.txt")
session_info_txt <- file.path(out_tables_dir, "09E3_sessionInfo.txt")
output_check_csv <- file.path(out_tables_dir, "09E3_output_verification.csv")
report_txt <- file.path(reports_dir, "09E3_external_grouping_cluster_recovery_report.txt")

fig_cluster_size_pdf <- file.path(out_figures_dir, "09E3_external_cluster_size_barplot.pdf")
fig_cluster_score_heatmap_pdf <- file.path(out_figures_dir, "09E3_external_cluster_signature_score_heatmap.pdf")
fig_cluster_prediction_pdf <- file.path(out_figures_dir, "09E3_external_cluster_frozen_predictor_probability_barplot.pdf")
fig_cluster_priority_pdf <- file.path(out_figures_dir, "09E3_external_cluster_priority_scatter.pdf")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

num <- function(x) suppressWarnings(as.numeric(x))

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }

  if (file.exists(path)) unlink(path, force = TRUE)

  data.table::fwrite(df, path)

  if (!file.exists(path)) stop("CSV 未生成：", path)

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes <= 0) {
    stop("CSV 已创建但为空或无效：", path)
  }

  invisible(path)
}

read_required_csv <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = TRUE, showProgress = FALSE)
}

safe_pdf <- function(path, width = PDF_WIDTH, height = PDF_HEIGHT) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(path)) {
    removed <- file.remove(path)
    if (!isTRUE(removed)) {
      stop(
        "旧 PDF 正在被占用，无法覆盖：", path,
        "\n请关闭 Edge/Adobe/RStudio Viewer/文件资源管理器预览窗口后重跑。"
      )
    }
  }

  while (grDevices::dev.cur() > 1) {
    try(grDevices::dev.off(), silent = TRUE)
    if (grDevices::dev.cur() <= 1) break
  }

  grDevices::pdf(path, width = width, height = height, useDingbats = FALSE, onefile = TRUE)
}

finish_pdf <- function(path) {
  try(grDevices::dev.off(), silent = TRUE)

  if (!file.exists(path)) stop("PDF 未生成：", path)

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes < 1000) {
    stop("PDF 已创建但文件过小或无效：", path, "；size = ", size_bytes)
  }

  message("已保存 PDF：", normalizePath(path, winslash = "/", mustWork = TRUE),
          " | size = ", round(size_bytes / 1024, 1), " KB")
}

plot_empty_pdf <- function(path, title, message) {
  safe_pdf(path)
  plot.new()
  title(main = title)
  text(0.5, 0.5, message, cex = 0.95)
  finish_pdf(path)
}

sanitize_feature <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

make_clean_group <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == "" | tolower(x) %in% c("unknown", "unknown_unknown", "na", "none")] <- NA_character_
  x
}

infer_condition_from_text <- function(x) {
  x <- as.character(x)
  xl <- tolower(x)

  treatment <- ifelse(
    grepl("pink1|pink", xl), "PINK1",
    ifelse(grepl("control|ctrl|wildtype|wt", xl), "Control", "Unknown")
  )

  day <- ifelse(
    grepl("d0?6|day.?0?6|[^0-9]06[^0-9]|d6", xl), "D06",
    ifelse(
      grepl("d10|day.?10", xl), "D10",
      ifelse(
        grepl("d15|day.?15", xl), "D15",
        ifelse(
          grepl("d21|day.?21", xl), "D21",
          ifelse(grepl("ipsc|ips", xl), "iPSC", "Unknown")
        )
      )
    )
  )

  paste(treatment, day, sep = "_")
}

is_meaningful_group <- function(x) {
  x <- as.character(x)
  !is.na(x) & x != "" & !tolower(x) %in% c("unknown", "unknown_unknown", "na", "none")
}

get_feature_cols <- function(dt) {
  exclude <- c(
    "task",
    "weak_label",
    "dataset",
    "object_id",
    "group_id",
    "group_key",
    "safety_contrast_class_05B",
    "n_cells",
    "sample_weight_equal",
    "sample_weight_sqrt_cells",
    "row_id",
    "fold",
    "predicted_probability"
  )
  numeric_cols <- names(dt)[vapply(dt, function(z) is.numeric(z) || is.integer(z), logical(1))]
  setdiff(numeric_cols, exclude)
}

prep_fit <- function(train_dt, feature_cols) {
  med <- sapply(feature_cols, function(fc) {
    x <- num(train_dt[[fc]])
    if (all(is.na(x))) return(0)
    median(x, na.rm = TRUE)
  })

  sdv <- sapply(feature_cols, function(fc) {
    x <- num(train_dt[[fc]])
    x[is.na(x)] <- med[[fc]]
    s <- sd(x, na.rm = TRUE)
    if (!is.finite(s) || s == 0) s <- 1
    s
  })

  list(median = med, sd = sdv)
}

prep_apply <- function(dt, feature_cols, prep, scale = TRUE) {
  mat <- matrix(NA_real_, nrow = nrow(dt), ncol = length(feature_cols))
  colnames(mat) <- make.names(feature_cols, unique = TRUE)

  for (j in seq_along(feature_cols)) {
    fc <- feature_cols[[j]]
    x <- num(dt[[fc]])
    x[is.na(x)] <- prep$median[[fc]]
    if (scale) {
      x <- (x - prep$median[[fc]]) / prep$sd[[fc]]
    }
    mat[, j] <- x
  }

  as.data.frame(mat, check.names = FALSE)
}

fit_full_logistic_predict <- function(train_dt, external_dt, feature_cols) {
  if (nrow(train_dt) < 5 || length(unique(train_dt$weak_label)) < 2) {
    return(list(success = FALSE, message = "training data too small or one class only", probs = rep(NA_real_, nrow(external_dt))))
  }

  prep <- prep_fit(train_dt, feature_cols)
  x_train <- prep_apply(train_dt, feature_cols, prep, scale = TRUE)
  x_external <- prep_apply(external_dt, feature_cols, prep, scale = TRUE)

  train_df <- data.frame(
    weak_label = as.integer(train_dt$weak_label),
    x_train,
    check.names = FALSE
  )

  formula_txt <- paste("weak_label ~", paste(colnames(x_train), collapse = " + "))

  fit <- tryCatch({
    suppressWarnings(stats::glm(as.formula(formula_txt), data = train_df, family = stats::binomial()))
  }, error = function(e) e)

  if (inherits(fit, "error")) {
    return(list(success = FALSE, message = conditionMessage(fit), probs = rep(NA_real_, nrow(external_dt))))
  }

  probs <- tryCatch({
    as.numeric(stats::predict(fit, newdata = x_external, type = "response"))
  }, error = function(e) rep(NA_real_, nrow(external_dt)))

  list(success = TRUE, message = "ok", probs = probs)
}

fit_full_rf_predict <- function(train_dt, external_dt, feature_cols) {
  if (!HAS_RANDOMFOREST) {
    return(list(success = FALSE, message = "randomForest not installed", probs = rep(NA_real_, nrow(external_dt))))
  }

  if (nrow(train_dt) < 10 || length(unique(train_dt$weak_label)) < 2) {
    return(list(success = FALSE, message = "training data too small or one class only", probs = rep(NA_real_, nrow(external_dt))))
  }

  prep <- prep_fit(train_dt, feature_cols)
  x_train <- prep_apply(train_dt, feature_cols, prep, scale = FALSE)
  x_external <- prep_apply(external_dt, feature_cols, prep, scale = FALSE)

  y_train <- factor(as.integer(train_dt$weak_label), levels = c(0, 1))

  fit <- tryCatch({
    randomForest::randomForest(
      x = x_train,
      y = y_train,
      ntree = 500,
      mtry = max(1, floor(sqrt(ncol(x_train)))),
      importance = TRUE
    )
  }, error = function(e) e)

  if (inherits(fit, "error")) {
    return(list(success = FALSE, message = conditionMessage(fit), probs = rep(NA_real_, nrow(external_dt))))
  }

  probs <- tryCatch({
    pr <- stats::predict(fit, newdata = x_external, type = "prob")
    if ("1" %in% colnames(pr)) as.numeric(pr[, "1"]) else as.numeric(pr[, ncol(pr)])
  }, error = function(e) rep(NA_real_, nrow(external_dt)))

  list(success = TRUE, message = "ok", probs = probs)
}

row_var_sparse <- function(mat) {
  mu <- Matrix::rowMeans(mat)
  mu2 <- Matrix::rowMeans(mat ^ 2)
  v <- mu2 - mu ^ 2
  v[!is.finite(v)] <- 0
  v
}

make_cluster_from_matrix <- function(mat, n_hvg = N_HVG_FOR_EXTERNAL_CLUSTERING, n_pcs = N_PCS, k = DEFAULT_K_CLUSTERS) {
  stamp("开始 external unsupervised cluster recovery。")

  if (!inherits(mat, "dgCMatrix")) {
    mat <- as(mat, "dgCMatrix")
  }

  if (is.null(colnames(mat))) {
    stop("external matrix 缺少 colnames，无法生成 cluster cell_id。")
  }

  cell_ids <- make.unique(as.character(colnames(mat)))
  cell_ids[is.na(cell_ids) | cell_ids == ""] <- paste0("ExternalCell_", seq_along(cell_ids))[is.na(cell_ids) | cell_ids == ""]
  colnames(mat) <- cell_ids

  rv <- row_var_sparse(mat)
  genes_use <- names(sort(rv, decreasing = TRUE))[seq_len(min(n_hvg, sum(rv > 0)))]
  genes_use <- genes_use[!is.na(genes_use)]

  if (length(genes_use) < 50) {
    stop("可用于 clustering 的高变基因太少：", length(genes_use))
  }

  dense <- as.matrix(t(mat[genes_use, , drop = FALSE]))
  dense[!is.finite(dense)] <- 0

  rownames(dense) <- cell_ids

  dense <- scale(dense)
  dense[!is.finite(dense)] <- 0
  rownames(dense) <- cell_ids

  n_pcs_use <- min(n_pcs, ncol(dense) - 1, nrow(dense) - 1)
  if (n_pcs_use < 2) stop("可用 PCA 维度太少。")

  if (HAS_IRLBA) {
    pca <- irlba::prcomp_irlba(dense, n = n_pcs_use, center = FALSE, scale. = FALSE)
    pcs <- pca$x
    pca_method <- "irlba_prcomp_irlba"
  } else {
    pca <- stats::prcomp(dense, center = FALSE, scale. = FALSE, rank. = n_pcs_use)
    pcs <- pca$x[, seq_len(n_pcs_use), drop = FALSE]
    pca_method <- "stats_prcomp"
  }

  if (is.null(rownames(pcs)) || length(rownames(pcs)) != length(cell_ids)) {
    rownames(pcs) <- cell_ids
  }

  k_use <- min(k, max(2, floor(nrow(pcs) / MIN_CLUSTER_CELLS)))
  if (k_use < 2) k_use <- 2

  km <- stats::kmeans(pcs, centers = k_use, nstart = 50, iter.max = 100)

  cluster_dt <- data.table(
    cell_id = rownames(pcs),
    external_cluster = paste0("ExternalCluster_", sprintf("%02d", km$cluster)),
    pca_method = pca_method,
    n_hvg_used = length(genes_use),
    n_pcs_used = n_pcs_use,
    k_clusters = k_use
  )

  if (!"cell_id" %in% names(cluster_dt)) {
    stop("cluster_dt 缺少 cell_id；V6 hard validation failed.")
  }
  if (any(is.na(cluster_dt$cell_id)) || any(cluster_dt$cell_id == "")) {
    stop("cluster_dt cell_id 存在 NA/empty；V6 hard validation failed.")
  }

  cluster_dt
}

stamp("读取 09E base section 输出 / 内存对象。")

if (exists("external_counts") && inherits(external_counts, "Matrix")) {
  stamp("使用 Part 1 内存中的 external_counts，不重新 readRDS。")
  external_mat <- external_counts
} else if (exists("external_counts")) {
  stamp("使用 Part 1 内存中的 external_counts，并转换为 sparse Matrix。")
  external_mat <- Matrix::Matrix(external_counts, sparse = TRUE)
} else {
  if (!file.exists(input_external_matrix_rds)) stop("找不到 09E external matrix RDS：", input_external_matrix_rds)
  external_mat <- tryCatch({
    readRDS(input_external_matrix_rds)
  }, error = function(e) {
    stop(
      "读取 external matrix RDS 失败：", input_external_matrix_rds,
      "\n原因：", conditionMessage(e),
      "\n建议：运行完整 09E V4 脚本，让 Part 2 直接使用 Part 1 内存对象；",
      "或者删除旧 RDS 后重新运行完整脚本。"
    )
  })
}

if (exists("score_dt") && is.data.frame(score_dt) && "cell_id" %in% names(score_dt)) {
  stamp("使用 Part 1 内存中的 score_dt，不重新读取 cell score CSV。")
  cell_scores <- data.table::as.data.table(score_dt)
} else {
  cell_scores <- read_required_csv(input_cell_scores)
}

if (exists("external_meta") && is.data.frame(external_meta) && "cell_id" %in% names(external_meta)) {
  stamp("使用 Part 1 内存中的 external_meta，不重新读取 metadata CSV。")
  cell_meta <- data.table::as.data.table(external_meta)
} else {
  cell_meta <- read_required_csv(input_cell_metadata)
}

if (!"cell_id" %in% names(cell_scores)) stop("09E cell score table 缺少 cell_id。")
if (!"cell_id" %in% names(cell_meta)) stop("09E metadata table 缺少 cell_id。")

score_cols <- names(cell_scores)[grepl("^score_", names(cell_scores))]
if (length(score_cols) == 0) stop("09E cell score table 没有 score_ 列。")

stamp("External cells in matrix：", ncol(external_mat))
stamp("External cells in score table：", nrow(cell_scores))
stamp("Score columns：", length(score_cols))

stamp("尝试从 metadata / cell_id 恢复 biological group。")

meta_dt <- copy(cell_meta)
score_dt <- copy(cell_scores)

meta_dt <- unique(meta_dt, by = "cell_id")
score_dt <- unique(score_dt, by = "cell_id")

merged_dt <- merge(
  score_dt,
  meta_dt,
  by = "cell_id",
  all.x = TRUE,
  suffixes = c("", "_meta")
)

candidate_cols <- intersect(
  c("external_condition", "condition", "sample", "orig.ident", "source_dir", "external_group", "cell_type", "seurat_clusters"),
  names(merged_dt)
)

group_candidate_summary <- rbindlist(lapply(candidate_cols, function(cc) {
  x <- make_clean_group(merged_dt[[cc]])
  data.table(
    candidate_column = cc,
    n_unique_nonmissing = uniqueN(x[!is.na(x)]),
    n_cells_nonmissing = sum(!is.na(x)),
    fraction_nonmissing = mean(!is.na(x)),
    example_values = paste(head(unique(x[!is.na(x)]), 8), collapse = ";")
  )
}), fill = TRUE)

if (length(candidate_cols) == 0) {
  group_candidate_summary <- data.table(
    candidate_column = "none_detected",
    n_unique_nonmissing = 0,
    n_cells_nonmissing = 0,
    fraction_nonmissing = 0,
    example_values = NA_character_
  )
}

text_fields <- unique(c("cell_id", candidate_cols))
text_blob <- do.call(paste, c(merged_dt[, ..text_fields], sep = " | "))
inferred_group <- infer_condition_from_text(text_blob)
inferred_group[!is_meaningful_group(inferred_group)] <- NA_character_

n_inferred <- uniqueN(inferred_group[!is.na(inferred_group)])
frac_inferred <- mean(!is.na(inferred_group))

use_biological_group <- FALSE
selected_group_col <- NA_character_
selected_group_reason <- NA_character_

valid_candidates <- group_candidate_summary[
  n_unique_nonmissing >= 2 &
    fraction_nonmissing >= 0.80
]

if (nrow(valid_candidates) > 0) {
  selected_group_col <- valid_candidates$candidate_column[1]
  merged_dt[, recovered_biological_group := make_clean_group(get(selected_group_col))]
  use_biological_group <- TRUE
  selected_group_reason <- paste0("Used existing metadata column: ", selected_group_col)
} else if (n_inferred >= 2 && frac_inferred >= 0.80) {
  merged_dt[, recovered_biological_group := inferred_group]
  use_biological_group <- TRUE
  selected_group_col <- "inferred_from_text_fields"
  selected_group_reason <- "Used inferred grouping from cell_id/metadata text patterns."
} else {
  merged_dt[, recovered_biological_group := NA_character_]
  selected_group_reason <- "No reliable biological grouping recovered; proceeding to unsupervised cluster-level recovery."
}

grouping_audit <- rbind(
  group_candidate_summary,
  data.table(
    candidate_column = "inferred_from_text_fields",
    n_unique_nonmissing = n_inferred,
    n_cells_nonmissing = sum(!is.na(inferred_group)),
    fraction_nonmissing = frac_inferred,
    example_values = paste(head(unique(inferred_group[!is.na(inferred_group)]), 8), collapse = ";")
  ),
  fill = TRUE
)

grouping_audit[, selected_for_primary_external_subgroup := candidate_column == selected_group_col]
grouping_audit[, final_decision := selected_group_reason]

atomic_write_csv(as.data.frame(grouping_audit), grouping_audit_csv)

if (use_biological_group) {
  stamp("已恢复 biological grouping：", selected_group_col)
  merged_dt[, final_external_group := recovered_biological_group]
  merged_dt[, final_grouping_source := selected_group_col]
  cluster_assignment <- data.table(
    cell_id = merged_dt$cell_id,
    final_external_group = merged_dt$final_external_group,
    recovered_biological_group = merged_dt$recovered_biological_group,
    external_cluster = NA_character_,
    final_grouping_source = selected_group_col
  )
} else {
  stamp("未恢复可靠 biological grouping，执行 unsupervised cluster-level recovery。")

  cluster_dt <- make_cluster_from_matrix(
    external_mat,
    n_hvg = N_HVG_FOR_EXTERNAL_CLUSTERING,
    n_pcs = N_PCS,
    k = DEFAULT_K_CLUSTERS
  )

  merged_dt <- merge(
    merged_dt,
    cluster_dt,
    by = "cell_id",
    all.x = TRUE
  )

  merged_dt[is.na(external_cluster), external_cluster := "ExternalCluster_unassigned"]
  merged_dt[, final_external_group := external_cluster]
  merged_dt[, final_grouping_source := "unsupervised_external_cluster"]

  cluster_assignment <- merged_dt[, .(
    cell_id,
    final_external_group,
    recovered_biological_group,
    external_cluster,
    final_grouping_source,
    pca_method = if ("pca_method" %in% names(merged_dt)) pca_method else NA_character_,
    n_hvg_used = if ("n_hvg_used" %in% names(merged_dt)) n_hvg_used else NA_integer_,
    n_pcs_used = if ("n_pcs_used" %in% names(merged_dt)) n_pcs_used else NA_integer_,
    k_clusters = if ("k_clusters" %in% names(merged_dt)) k_clusters else NA_integer_
  )]
}

atomic_write_csv(as.data.frame(cluster_assignment), cell_grouping_csv)

stamp("生成 external subgroup / cluster score summary。")

cluster_score_summary <- merged_dt[, c(
  list(
    dataset = EXTERNAL_GSE_ID,
    n_cells = .N,
    grouping_source = unique(final_grouping_source)[1],
    small_group_flag = .N < MIN_CLUSTER_CELLS
  ),
  lapply(.SD, function(x) mean(num(x), na.rm = TRUE))
), by = final_external_group, .SDcols = score_cols]

setnames(cluster_score_summary, "final_external_group", "external_group")

marker_profile <- rbindlist(lapply(seq_len(nrow(cluster_score_summary)), function(i) {
  row <- cluster_score_summary[i]
  vals <- unlist(row[, ..score_cols], use.names = TRUE)
  data.table(
    external_group = row$external_group,
    score_column = names(vals),
    mean_score = as.numeric(vals)
  )[order(-mean_score)]
}), fill = TRUE)

marker_profile[, rank_within_group := seq_len(.N), by = external_group]

atomic_write_csv(as.data.frame(cluster_score_summary), cluster_score_summary_csv)
atomic_write_csv(as.data.frame(marker_profile), cluster_marker_profile_csv)

stamp("构建 external cluster-level reduced feature table。")

external_features <- copy(cluster_score_summary)
external_features[, group_id := external_group]
external_features[, object_id := paste0(EXTERNAL_GSE_ID, "_external_cluster_recovery")]
external_features[, dataset := EXTERNAL_GSE_ID]

for (sc in score_cols) {
  base <- sub("^score_", "", sc)
  marker_name <- paste0("marker_", base)
  if (!marker_name %in% names(external_features)) {
    external_features[, (marker_name) := get(sc)]
  }
}

a9_cols <- names(external_features)[grepl("A9", names(external_features), ignore.case = TRUE)]
a10_cols <- names(external_features)[grepl("A10", names(external_features), ignore.case = TRUE)]

if (length(a9_cols) > 0 && length(a10_cols) > 0) {
  a9_use <- a9_cols[1]
  a10_use <- a10_cols[1]
  external_features[, A9_minus_A10_score_05A := num(get(a9_use)) - num(get(a10_use))]
  external_features[, pct_cells_A9_minus_A10_score_05A_gt0 := as.numeric(A9_minus_A10_score_05A > 0)]
}

atomic_write_csv(as.data.frame(external_features), cluster_feature_table_csv)

stamp("应用 discovery-only frozen predictor 到 external clusters。")

prediction_outputs <- list()
alignment_outputs <- list()

if (file.exists(input_ideal_reduced) && file.exists(input_safety_reduced)) {
  train_list <- list(
    ideal_like_classifier = read_required_csv(input_ideal_reduced),
    safety_risk_classifier = read_required_csv(input_safety_reduced)
  )

  for (task_name in names(train_list)) {
    train_dt <- copy(train_list[[task_name]])
    train_dt[, weak_label := as.integer(weak_label)]

    feature_cols <- get_feature_cols(train_dt)

    missing_external <- setdiff(feature_cols, names(external_features))
    available_external <- intersect(feature_cols, names(external_features))

    alignment_outputs[[length(alignment_outputs) + 1L]] <- data.table(
      task_name = task_name,
      n_required_features = length(feature_cols),
      n_available_external_features = length(available_external),
      n_missing_external_features = length(missing_external),
      feature_alignment_fraction = ifelse(length(feature_cols) > 0, length(available_external) / length(feature_cols), NA_real_),
      missing_features = paste(missing_external, collapse = ";"),
      prediction_ready = length(missing_external) == 0
    )

    if (length(missing_external) == 0 && length(feature_cols) > 0) {
      glm_res <- fit_full_logistic_predict(train_dt, external_features, feature_cols)
      rf_res <- fit_full_rf_predict(train_dt, external_features, feature_cols)

      pred_dt <- data.table(
        dataset = EXTERNAL_GSE_ID,
        external_group = external_features$external_group,
        group_id = external_features$group_id,
        object_id = external_features$object_id,
        n_cells = external_features$n_cells,
        small_group_flag = external_features$small_group_flag,
        task_name = task_name,
        logistic_success = glm_res$success,
        logistic_message = glm_res$message,
        logistic_predicted_probability = glm_res$probs,
        random_forest_success = rf_res$success,
        random_forest_message = rf_res$message,
        random_forest_predicted_probability = rf_res$probs
      )

      prediction_outputs[[length(prediction_outputs) + 1L]] <- pred_dt
    }
  }
} else {
  alignment_outputs[[length(alignment_outputs) + 1L]] <- data.table(
    task_name = "all",
    n_required_features = NA_integer_,
    n_available_external_features = NA_integer_,
    n_missing_external_features = NA_integer_,
    feature_alignment_fraction = NA_real_,
    missing_features = "09B reduced feature training tables not found",
    prediction_ready = FALSE
  )
}

alignment_audit <- rbindlist(alignment_outputs, fill = TRUE)
atomic_write_csv(as.data.frame(alignment_audit), cluster_ml_alignment_csv)

if (length(prediction_outputs) > 0) {
  pred_all <- rbindlist(prediction_outputs, fill = TRUE)

  pred_wide <- dcast(
    pred_all,
    dataset + external_group + group_id + object_id + n_cells + small_group_flag ~ task_name,
    value.var = c("logistic_predicted_probability", "random_forest_predicted_probability")
  )

  ideal_col <- "logistic_predicted_probability_ideal_like_classifier"
  safety_col <- "logistic_predicted_probability_safety_risk_classifier"

  if (all(c(ideal_col, safety_col) %in% names(pred_wide))) {
    pred_wide[, exploratory_external_class := fifelse(
      get(ideal_col) >= 0.5 & get(safety_col) < 0.5,
      "external_ideal_like_probability_high_safety_low_like",
      fifelse(
        get(safety_col) >= 0.5,
        "external_safety_risk_probability_high",
        "external_mixed_or_uncertain"
      )
    )]

    pred_wide[, external_priority_index_logistic := get(ideal_col) - get(safety_col)]
  }

  rf_ideal_col <- "random_forest_predicted_probability_ideal_like_classifier"
  rf_safety_col <- "random_forest_predicted_probability_safety_risk_classifier"

  if (all(c(rf_ideal_col, rf_safety_col) %in% names(pred_wide))) {
    pred_wide[, external_priority_index_random_forest := get(rf_ideal_col) - get(rf_safety_col)]
  }

  atomic_write_csv(as.data.frame(pred_wide), cluster_prediction_csv)
} else {
  pred_wide <- data.table(
    message = "Frozen predictor probabilities were not generated because external cluster features did not fully align with 09B reduced features."
  )
  atomic_write_csv(as.data.frame(pred_wide), cluster_prediction_csv)
}

stamp("生成 09E3 PDF figures。")

size_dt <- cluster_score_summary[order(n_cells)]
safe_pdf(fig_cluster_size_pdf, width = 10.8, height = max(6.5, 0.42 * nrow(size_dt) + 2.6))
par(mar = c(5.2, 10.5, 4.2, 2.0))
barplot(
  size_dt$n_cells,
  names.arg = size_dt$external_group,
  horiz = TRUE,
  las = 1,
  xlab = "Number of cells",
  main = "09E3 external subgroup / cluster size",
  col = ifelse(size_dt$small_group_flag, "grey85", "grey55"),
  border = "grey25",
  cex.names = 0.74
)
finish_pdf(fig_cluster_size_pdf)

heat_dt <- copy(cluster_score_summary)
mat <- as.matrix(heat_dt[, ..score_cols])
rownames(mat) <- heat_dt$external_group

if (nrow(mat) > 1) {
  mat_scaled <- scale(mat)
} else {
  mat_scaled <- mat
}
mat_scaled[!is.finite(mat_scaled)] <- 0
mat_scaled[mat_scaled > 2] <- 2
mat_scaled[mat_scaled < -2] <- -2

safe_pdf(fig_cluster_score_heatmap_pdf, width = 13.5, height = max(7.5, 0.45 * nrow(mat_scaled) + 3.0))
par(mar = c(10.5, 10.5, 4.0, 2.0))

image(
  x = seq_len(ncol(mat_scaled)),
  y = seq_len(nrow(mat_scaled)),
  z = t(mat_scaled[nrow(mat_scaled):1, , drop = FALSE]),
  axes = FALSE,
  xlab = "",
  ylab = "",
  main = "09E3 external cluster frozen signature score heatmap"
)

axis(1, at = seq_len(ncol(mat_scaled)), labels = gsub("^score_", "", colnames(mat_scaled)), las = 2, cex.axis = 0.60)
axis(2, at = seq_len(nrow(mat_scaled)), labels = rev(rownames(mat_scaled)), las = 1, cex.axis = 0.75)
box()

finish_pdf(fig_cluster_score_heatmap_pdf)

if (exists("pred_wide") && nrow(pred_wide) > 0 && !("message" %in% names(pred_wide))) {
  prob_cols <- names(pred_wide)[grepl("predicted_probability", names(pred_wide))]
  pred_long <- melt(
    pred_wide,
    id.vars = c("external_group", "n_cells"),
    measure.vars = prob_cols,
    variable.name = "model_probability",
    value.name = "probability"
  )
  pred_long <- pred_long[!is.na(probability)]
  pred_long[, label := paste0(external_group, " | ", gsub("predicted_probability_", "", model_probability), " | n=", n_cells)]
  pred_long <- pred_long[order(probability)]

  if (nrow(pred_long) > 0) {
    safe_pdf(fig_cluster_prediction_pdf, width = 13.5, height = max(7.5, 0.22 * nrow(pred_long) + 3.0))
    par(mar = c(5.2, 20.5, 4.2, 2.0))
    barplot(
      pred_long$probability,
      names.arg = pred_long$label,
      horiz = TRUE,
      las = 1,
      xlab = "Predicted probability",
      main = "09E3 external cluster frozen predictor probabilities",
      col = "grey60",
      border = "grey25",
      cex.names = 0.52,
      xlim = c(0, 1)
    )
    abline(v = 0.5, lty = 2, col = "grey45")
    finish_pdf(fig_cluster_prediction_pdf)
  } else {
    plot_empty_pdf(fig_cluster_prediction_pdf, "09E3 external cluster probabilities", "No finite probabilities.")
  }
} else {
  plot_empty_pdf(fig_cluster_prediction_pdf, "09E3 external cluster probabilities", "Prediction skipped due to feature mismatch.")
}

if (exists("pred_wide") && nrow(pred_wide) > 0 && !("message" %in% names(pred_wide)) &&
    all(c("logistic_predicted_probability_ideal_like_classifier",
          "logistic_predicted_probability_safety_risk_classifier") %in% names(pred_wide))) {

  safe_pdf(fig_cluster_priority_pdf, width = 8.8, height = 7.5)
  par(mar = c(5.2, 5.2, 4.2, 2.0))

  x <- pred_wide$logistic_predicted_probability_ideal_like_classifier
  y <- pred_wide$logistic_predicted_probability_safety_risk_classifier

  plot(
    x, y,
    xlim = c(0, 1),
    ylim = c(0, 1),
    pch = 19,
    cex = pmax(0.8, sqrt(pred_wide$n_cells) / 18),
    xlab = "Ideal-like probability (logistic)",
    ylab = "Safety-risk probability (logistic)",
    main = "09E3 external cluster prioritization scatter"
  )
  abline(v = 0.5, lty = 2, col = "grey55")
  abline(h = 0.5, lty = 2, col = "grey55")
  text(x, y, labels = pred_wide$external_group, pos = 3, cex = 0.70)

  finish_pdf(fig_cluster_priority_pdf)
} else {
  plot_empty_pdf(fig_cluster_priority_pdf, "09E3 external cluster prioritization scatter", "Required logistic probability columns unavailable.")
}

method_lines <- c(
  "09E3 method and claim-boundary note",
  "",
  "Purpose:",
  "09E3 was performed because 09E base section successfully imported and scored GSE183248 but recovered only one external group (Unknown_Unknown).",
  "",
  "Method:",
  "The script first attempted to recover biological grouping from available metadata and cell identifiers.",
  "If no reliable biological grouping was available, unsupervised external cluster-level recovery was performed using high-variance genes, PCA, and k-means clustering.",
  "Frozen 09E cell-level marker scores were then summarized by recovered groups/clusters.",
  "Discovery-trained marker-rule-derived predictors were applied to external groups only when all reduced non-direct features matched.",
  "",
  "Claim boundary:",
  "09E3 provides exploratory external subgroup-level transcriptomic validation.",
  "Cluster recovery is unsupervised and is not equivalent to manual biological cell-type annotation.",
  "External data were not used to train the predictor or tune thresholds.",
  "Predicted probabilities are transcriptomic prioritization scores, not clinical outcome or graft safety predictions.",
  "This analysis does not prove anatomical projection, functional integration, therapeutic efficacy, or clinical safety."
)

writeLines(method_lines, method_note_txt)

report_lines <- c(
  "09E3 external grouping and cluster-level recovery report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Grouping decision:",
  selected_group_reason,
  "",
  "Grouping audit:",
  capture.output(print(grouping_audit)),
  "",
  "Cluster score summary:",
  capture.output(print(cluster_score_summary)),
  "",
  "ML alignment:",
  capture.output(print(alignment_audit)),
  "",
  "Prediction summary:",
  capture.output(print(pred_wide)),
  "",
  "Output directories:",
  out_tables_dir,
  out_figures_dir
)

writeLines(report_lines, report_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

required_output_files <- c(
  grouping_audit_csv,
  cell_grouping_csv,
  cluster_score_summary_csv,
  cluster_feature_table_csv,
  cluster_ml_alignment_csv,
  cluster_prediction_csv,
  cluster_marker_profile_csv,
  method_note_txt,
  session_info_txt,
  report_txt,
  fig_cluster_size_pdf,
  fig_cluster_score_heatmap_pdf,
  fig_cluster_prediction_pdf,
  fig_cluster_priority_pdf
)

output_check <- data.table(
  file = required_output_files,
  exists = file.exists(required_output_files),
  size_bytes = ifelse(file.exists(required_output_files), file.info(required_output_files)$size, NA_real_)
)

atomic_write_csv(as.data.frame(output_check), output_check_csv)

bad_outputs <- output_check[!exists | is.na(size_bytes) | size_bytes <= 0]
if (nrow(bad_outputs) > 0) {
  print(bad_outputs)
  stop("09E3 输出验证失败。")
}

cat("\n============================================================\n")
cat("09E external grouping and cluster-level recovery FINAL V6 FULL 运行结束\n")
cat("============================================================\n\n")

cat("External GSE：", EXTERNAL_GSE_ID, "\n")
cat("Grouping decision：", selected_group_reason, "\n")
cat("External groups/clusters：", nrow(cluster_score_summary), "\n")
cat("External cells：", nrow(cluster_assignment), "\n")
cat("ML prediction ready tasks：", sum(alignment_audit$prediction_ready == TRUE, na.rm = TRUE), "\n\n")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("关键输出：\n")
cat(grouping_audit_csv, "\n")
cat(cell_grouping_csv, "\n")
cat(cluster_score_summary_csv, "\n")
cat(cluster_feature_table_csv, "\n")
cat(cluster_ml_alignment_csv, "\n")
cat(cluster_prediction_csv, "\n")
cat(method_note_txt, "\n\n")

cat("PDF 图：\n")
cat(fig_cluster_size_pdf, "\n")
cat(fig_cluster_score_heatmap_pdf, "\n")
cat(fig_cluster_prediction_pdf, "\n")
cat(fig_cluster_priority_pdf, "\n\n")

cat("✅ 09E external grouping and cluster-level recovery FINAL V6 FULL 完成。\n")
