
PROJECT_DIR <- "D:/PD_Graft_Project"

SEED <- 20260715

CONTEXT_GSE_IDS <- c("GSE184950", "GSE243639")

LOCAL_MANUAL_FILE_PLAN <- data.frame(
  gse_id = c("GSE243639"),
  file_name = c("GSE243639_Filtered_count_table.csv.gz"),
  source_url = c("https://ftp.ncbi.nlm.nih.gov/geo/series/GSE243nnn/GSE243639/suppl/GSE243639_Filtered_count_table.csv.gz"),
  expected_local_path = c("D:/PD_Graft_Project/00_raw_data/09I_disease_context_validation/GSE243639/GSE243639_Filtered_count_table.csv.gz"),
  expected_min_size_bytes = c(220000000),
  stringsAsFactors = FALSE
)

RESUME_FROM_EXISTING_CHECKPOINTS <- TRUE

PREVIOUS_V8_TABLES_DIR <- "D:/PD_Graft_Project/03_tables/09I_disease_context_validation_V8_MARKER_TARGETED_LOCAL_IMPORT"
PREVIOUS_V8_OBJECTS_DIR <- "D:/PD_Graft_Project/02_objects/09I_disease_context_validation_V8_MARKER_TARGETED_LOCAL_IMPORT"

TARGET_IMPORT_CHUNK_N_GENES <- 25

DEFAULT_K_CLUSTERS <- 8
MIN_CLUSTER_CELLS <- 30

PDF_WIDTH <- 11.5
PDF_HEIGHT <- 7.5

cat("\n============================================================\n")
cat("09I：Disease-context validation V9 checkpoint resume + safe figures\n")
cat("============================================================\n\n")

options(stringsAsFactors = FALSE)

required_pkgs <- c("data.table", "Matrix", "ggplot2")

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop("缺少 R 包，请先手动安装：", paste(missing_pkgs, collapse = ", "))
}

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(ggplot2)
})

HAS_RANDOMFOREST <- requireNamespace("randomForest", quietly = TRUE)
if (HAS_RANDOMFOREST) {
  suppressPackageStartupMessages(library(randomForest))
}

set.seed(SEED)

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(tables_dir, "09I_disease_context_validation_V9_CHECKPOINT_RESUME_SAFE_FIGURES")
out_figures_dir <- file.path(figures_dir, "09I_disease_context_validation_V9_CHECKPOINT_RESUME_SAFE_FIGURES_pdf")
out_objects_dir <- file.path(objects_dir, "09I_disease_context_validation_V9_CHECKPOINT_RESUME_SAFE_FIGURES")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_objects_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

known_marker_path <- file.path(
  tables_dir,
  "04A_annotation_prep",
  "04A_marker_panel_alias_long.csv"
)

input_ideal_training <- file.path(
  tables_dir,
  "09B_ML_ready_dataset_and_leakage_audit_V4_FULL_FIXED_LAYOUT",
  "09B_ideal_like_training_reduced_non_direct_features.csv"
)

input_safety_training <- file.path(
  tables_dir,
  "09B_ML_ready_dataset_and_leakage_audit_V4_FULL_FIXED_LAYOUT",
  "09B_safety_risk_training_reduced_non_direct_features.csv"
)

dataset_plan_csv <- file.path(out_tables_dir, "09I_V9_disease_context_dataset_plan.csv")
manual_download_plan_csv <- file.path(out_tables_dir, "09I_V9_manual_download_file_plan.csv")
marker_audit_csv <- file.path(out_tables_dir, "09I_V9_marker_panel_audit.csv")
target_import_audit_csv <- file.path(out_tables_dir, "09I_V9_marker_targeted_import_audit.csv")
target_gene_overlap_csv <- file.path(out_tables_dir, "09I_V9_gene_overlap_by_dataset.csv")
cell_score_csv <- file.path(out_tables_dir, "09I_V9_cell_signature_scores.csv")
cluster_assignment_csv <- file.path(out_tables_dir, "09I_V9_cell_context_cluster_assignments.csv")
cluster_score_summary_csv <- file.path(out_tables_dir, "09I_V9_context_cluster_score_summary.csv")
cluster_feature_table_csv <- file.path(out_tables_dir, "09I_V9_context_cluster_reduced_feature_table_for_prediction.csv")
ml_alignment_csv <- file.path(out_tables_dir, "09I_V9_context_cluster_ML_feature_alignment_audit.csv")
prediction_csv <- file.path(out_tables_dir, "09I_V9_context_cluster_frozen_predictor_probabilities.csv")
priority_summary_csv <- file.path(out_tables_dir, "09I_V9_context_cluster_priority_summary.csv")
key_findings_csv <- file.path(out_tables_dir, "09I_V9_key_findings_summary.csv")
method_note_txt <- file.path(out_tables_dir, "09I_V9_method_and_claim_boundary_note.txt")
session_info_txt <- file.path(out_tables_dir, "09I_V9_sessionInfo.txt")
output_check_csv <- file.path(out_tables_dir, "09I_V9_output_verification.csv")
report_txt <- file.path(reports_dir, "09I_disease_context_validation_V9_report.txt")

target_counts_rds <- file.path(out_objects_dir, "09I_V9_marker_target_counts_sparse.rds")
target_norm_rds <- file.path(out_objects_dir, "09I_V9_marker_target_log1pCPM_sparse.rds")

fig_import_pdf <- file.path(out_figures_dir, "09I_V9_marker_targeted_import_summary.pdf")
fig_gene_overlap_pdf <- file.path(out_figures_dir, "09I_V9_gene_overlap_by_dataset.pdf")
fig_cluster_size_pdf <- file.path(out_figures_dir, "09I_V9_context_cluster_size_barplot.pdf")
fig_signature_heatmap_pdf <- file.path(out_figures_dir, "09I_V9_context_cluster_signature_heatmap.pdf")
fig_probability_pdf <- file.path(out_figures_dir, "09I_V9_context_cluster_predictor_probability.pdf")
fig_priority_pdf <- file.path(out_figures_dir, "09I_V9_context_cluster_priority_index_barplot.pdf")
fig_summary_pdf <- file.path(out_figures_dir, "09I_V9_disease_context_validation_summary_panel.pdf")

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

save_pdf_plot <- function(plot_obj, path, width = PDF_WIDTH, height = PDF_HEIGHT) {
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

  ggplot2::ggsave(
    filename = path,
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    device = grDevices::cairo_pdf,
    limitsize = FALSE
  )

  if (!file.exists(path)) stop("PDF 未生成：", path)

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes < 1000) {
    stop("PDF 已创建但文件过小或无效：", path, "；size = ", size_bytes)
  }

  message("已保存 PDF：", normalizePath(path, winslash = "/", mustWork = TRUE),
          " | size = ", round(size_bytes / 1024, 1), " KB")
}

theme_pub <- function(base_size = 11) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = base_size + 2),
      plot.subtitle = element_text(hjust = 0.5, size = base_size - 1, color = "grey25"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      legend.title = element_text(face = "bold"),
      legend.position = "right",
      strip.background = element_rect(fill = "grey90", color = "grey40"),
      strip.text = element_text(face = "bold"),
      plot.margin = ggplot2::margin(8, 12, 8, 12)
    )
}

sanitize_feature <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  make.unique(x)
}

gene_key <- function(x) {
  toupper(gsub("-", "_", as.character(x)))
}

short_signature_label <- function(x) {
  x <- as.character(x)
  x <- gsub("^score_", "", x)
  x <- gsub("^marker_", "", x)
  x <- gsub("_", " ", x)
  x <- gsub("extracellular matrix", "ECM", x, ignore.case = TRUE)
  x <- gsub("microglia macrophage immune", "immune", x, ignore.case = TRUE)
  x <- gsub("midbrain floor plate progenitor", "floor-plate prog.", x, ignore.case = TRUE)
  x <- gsub("neuronal maturation synapse", "neuronal maturation", x, ignore.case = TRUE)
  x <- gsub("pluripotency immature risk", "pluripotency risk", x, ignore.case = TRUE)
  x <- gsub("progenitor neuroepithelial", "neuroepithelial prog.", x, ignore.case = TRUE)
  x <- gsub("stress apoptosis response", "stress/apoptosis", x, ignore.case = TRUE)
  x <- gsub("vascular pericyte meningeal", "vascular/pericyte", x, ignore.case = TRUE)
  x
}

split_gene_string <- function(x) {
  x <- as.character(x)
  x <- gsub("\\[|\\]|\\(|\\)|\\{|\\}|\"", " ", x)
  parts <- unlist(strsplit(x, "[;,/|[:space:]]+", perl = TRUE))
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  parts <- parts[grepl("^[A-Za-z0-9._-]+$", parts)]
  unique(parts)
}

standardize_marker_panel_from_dt <- function(dt, source_path) {
  original_names <- names(dt)
  clean_names <- tolower(gsub("[^A-Za-z0-9]+", "_", original_names))

  category_candidates <- c(
    "category", "marker_category", "signature", "signature_category",
    "module", "module_name", "program", "program_name", "marker_set",
    "marker_group", "group", "class", "cell_state", "state", "annotation"
  )

  gene_candidates <- c(
    "gene", "genes", "symbol", "symbols", "gene_symbol", "gene_symbols",
    "marker", "markers", "marker_gene", "marker_genes",
    "gene_name", "gene_names", "official_symbol", "hgnc_symbol"
  )

  cat_idx <- which(clean_names %in% category_candidates)[1]
  gene_idx <- which(clean_names %in% gene_candidates)[1]

  if (!is.na(cat_idx) && !is.na(gene_idx)) {
    raw <- data.table(
      category = as.character(dt[[original_names[cat_idx]]]),
      gene_raw = as.character(dt[[original_names[gene_idx]]])
    )

    out <- raw[
      !is.na(category) & category != "" &
        !is.na(gene_raw) & gene_raw != ""
    ][
      ,
      .(gene = split_gene_string(gene_raw)),
      by = category
    ]

    out[, source_file := source_path]
    out[, source_format := "long_category_gene"]
    out <- unique(out[!is.na(gene) & gene != "" & !is.na(category) & category != ""])

    if (nrow(out) >= 10 && uniqueN(out$category) >= 2) return(out)
  }

  stop("cannot standardize marker panel from known 04A file")
}

read_csv_header_cells <- function(path) {
  con <- if (grepl("\\.gz$", tolower(path))) gzfile(path, open = "rt") else file(path, open = "rt")
  on.exit(close(con), add = TRUE)

  header_line <- readLines(con, n = 1, warn = FALSE)
  if (length(header_line) == 0) stop("empty file, no header: ", path)

  header_dt <- data.table::fread(
    text = header_line,
    header = FALSE,
    data.table = FALSE,
    showProgress = FALSE,
    fill = TRUE
  )

  header_vals <- as.character(unlist(header_dt[1, ], use.names = FALSE))
  if (length(header_vals) < 3) stop("header has too few columns: ", path)

  cell_names <- header_vals[-1]
  cell_names[is.na(cell_names) | cell_names == ""] <- paste0("Cell_", seq_along(cell_names))[is.na(cell_names) | cell_names == ""]
  make.unique(cell_names)
}

safe_read_lines <- function(con, n) {
  warned <- FALSE
  msg <- NA_character_

  lines <- withCallingHandlers(
    tryCatch({
      readLines(con, n = n, warn = FALSE)
    }, error = function(e) {
      msg <<- conditionMessage(e)
      character()
    }),
    warning = function(w) {
      warned <<- TRUE
      msg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  list(lines = lines, warned = warned, message = msg)
}

read_marker_target_counts_with_library <- function(path, target_genes, chunk_n_genes = TARGET_IMPORT_CHUNK_N_GENES) {
  stamp("V9 marker-targeted local import：", basename(path))

  target_keys <- unique(gene_key(target_genes))
  cell_names <- read_csv_header_cells(path)
  n_cells <- length(cell_names)

  con <- if (grepl("\\.gz$", tolower(path))) gzfile(path, open = "rt") else file(path, open = "rt")
  on.exit(close(con), add = TRUE)

  header_line <- readLines(con, n = 1, warn = FALSE)
  if (length(header_line) == 0) stop("empty file after opening: ", path)

  i_all <- integer()
  j_all <- integer()
  x_all <- numeric()
  kept_genes <- character()
  lib_size <- numeric(n_cells)

  scanned_genes <- 0L
  parsed_chunks <- 0L
  incomplete_gzip_warning <- FALSE
  incomplete_gzip_message <- NA_character_

  repeat {
    rr <- safe_read_lines(con, n = chunk_n_genes)

    if (isTRUE(rr$warned)) {
      incomplete_gzip_warning <- TRUE
      incomplete_gzip_message <- rr$message
    }

    lines <- rr$lines

    if (length(lines) == 0) {
      break
    }

    parsed_chunks <- parsed_chunks + 1L

    dt <- tryCatch({
      data.table::fread(
        text = paste(c(header_line, lines), collapse = "\n"),
        header = TRUE,
        data.table = FALSE,
        showProgress = FALSE,
        fill = TRUE
      )
    }, error = function(e) NULL)

    if (is.null(dt) || nrow(dt) == 0 || ncol(dt) < 3) {
      if (isTRUE(rr$warned)) break
      next
    }

    if (ncol(dt) < n_cells + 1) {
      missing_n <- n_cells + 1 - ncol(dt)
      for (mm in seq_len(missing_n)) {
        dt[[paste0("V9_missing_col_", mm)]] <- 0
      }
    }

    if (ncol(dt) > n_cells + 1) {
      dt <- dt[, seq_len(n_cells + 1), drop = FALSE]
    }

    genes <- as.character(dt[[1]])
    valid_gene <- !is.na(genes) & genes != ""
    if (!any(valid_gene)) {
      if (isTRUE(rr$warned)) break
      next
    }

    dt <- dt[valid_gene, , drop = FALSE]
    genes <- genes[valid_gene]

    value_df <- dt[, -1, drop = FALSE]

    mat_chunk <- suppressWarnings(as.matrix(sapply(value_df, as.numeric)))
    if (is.null(dim(mat_chunk))) {
      if (isTRUE(rr$warned)) break
      next
    }

    if (ncol(mat_chunk) != n_cells) {
      if (ncol(mat_chunk) > n_cells) {
        mat_chunk <- mat_chunk[, seq_len(n_cells), drop = FALSE]
      } else {
        pad <- matrix(0, nrow = nrow(mat_chunk), ncol = n_cells - ncol(mat_chunk))
        mat_chunk <- cbind(mat_chunk, pad)
      }
    }

    mat_chunk[!is.finite(mat_chunk)] <- 0
    mat_chunk[mat_chunk < 0] <- 0

    lib_size <- lib_size + colSums(mat_chunk)

    scanned_genes <- scanned_genes + length(genes)

    gkey <- gene_key(genes)
    keep <- gkey %in% target_keys

    if (any(keep)) {
      kept_block <- mat_chunk[keep, , drop = FALSE]
      kept_gene_names <- genes[keep]

      nz <- which(kept_block != 0, arr.ind = TRUE)

      if (nrow(nz) > 0) {
        i_all <- c(i_all, as.integer(length(kept_genes) + nz[, 1]))
        j_all <- c(j_all, as.integer(nz[, 2]))
        x_all <- c(x_all, as.numeric(kept_block[nz]))
      }

      kept_genes <- c(kept_genes, kept_gene_names)
    }

    if (parsed_chunks %% 100 == 0) {
      stamp(
        "targeted import progress: chunks=", parsed_chunks,
        " scanned_genes=", scanned_genes,
        " kept_marker_rows=", length(kept_genes),
        " marker_nnz=", length(x_all)
      )
    }

    if (isTRUE(rr$warned)) {
      break
    }
  }

  if (scanned_genes < 1000) {
    stop("Too few genes scanned from count table: ", scanned_genes)
  }

  if (length(kept_genes) == 0) {
    stop("No target marker genes found in count table.")
  }

  kept_genes_sanitized <- sanitize_feature(kept_genes)

  target_counts <- Matrix::sparseMatrix(
    i = i_all,
    j = j_all,
    x = x_all,
    dims = c(length(kept_genes_sanitized), n_cells),
    dimnames = list(kept_genes_sanitized, cell_names)
  )

  original_gene_clean <- gsub("\\.[0-9]+$", "", kept_genes_sanitized)
  if (any(duplicated(original_gene_clean))) {
    group <- factor(original_gene_clean, levels = unique(original_gene_clean))
    row_map <- Matrix::sparseMatrix(
      i = as.integer(group),
      j = seq_along(original_gene_clean),
      x = 1,
      dims = c(length(levels(group)), length(original_gene_clean)),
      dimnames = list(levels(group), kept_genes_sanitized)
    )
    target_counts <- row_map %*% target_counts
    target_counts <- as(target_counts, "dgCMatrix")
  }

  lib_size[!is.finite(lib_size) | lib_size <= 0] <- 1

  stamp(
    "V9 marker-targeted import complete：scanned_genes=", scanned_genes,
    " marker_genes_kept=", nrow(target_counts),
    " cells=", ncol(target_counts),
    " marker_nnz=", length(target_counts@x),
    " incomplete_gzip_warning=", incomplete_gzip_warning
  )

  list(
    target_counts = target_counts,
    library_size = lib_size,
    cell_names = cell_names,
    audit = data.table(
      import_mode = "marker_targeted_local_import",
      file_path = path,
      file_size_bytes = file.info(path)$size,
      scanned_genes = scanned_genes,
      parsed_chunks = parsed_chunks,
      target_marker_rows_kept = nrow(target_counts),
      cells = ncol(target_counts),
      marker_nnz = length(target_counts@x),
      incomplete_gzip_warning = incomplete_gzip_warning,
      incomplete_gzip_message = incomplete_gzip_message
    )
  )
}

normalize_target_counts_log1pCPM <- function(target_counts, library_size) {
  library_size[!is.finite(library_size) | library_size <= 0] <- 1
  norm <- Matrix::t(Matrix::t(target_counts) / library_size) * 10000
  norm@x <- log1p(pmax(norm@x, 0))
  as(norm, "dgCMatrix")
}

score_markers_from_target_norm <- function(mat_norm, marker_panel, dataset_id) {
  gene_map <- data.table(
    matrix_gene = rownames(mat_norm),
    gene_upper = gene_key(rownames(mat_norm))
  )

  panel <- copy(marker_panel)
  panel[, gene_upper := gene_key(gene)]

  categories <- sort(unique(panel$category))
  score_list <- list()
  overlap_list <- list()

  for (catg in categories) {
    genes <- unique(panel[category == catg]$gene_upper)
    hit <- gene_map[gene_upper %in% genes]$matrix_gene
    hit <- unique(hit)

    overlap_list[[length(overlap_list) + 1L]] <- data.table(
      dataset = dataset_id,
      category = catg,
      n_marker_genes = length(genes),
      n_overlap_genes = length(hit),
      overlap_fraction = ifelse(length(genes) > 0, length(hit) / length(genes), NA_real_),
      overlap_genes = paste(hit, collapse = ";")
    )

    if (length(hit) == 0) {
      score_vec <- rep(NA_real_, ncol(mat_norm))
    } else {
      score_vec <- Matrix::colMeans(mat_norm[hit, , drop = FALSE])
    }

    score_list[[catg]] <- score_vec
  }

  score_dt <- data.table(
    dataset = dataset_id,
    cell_id = colnames(mat_norm)
  )

  for (catg in categories) {
    score_dt[[paste0("score_", catg)]] <- score_list[[catg]]
  }

  list(
    score_dt = score_dt,
    overlap_dt = rbindlist(overlap_list, fill = TRUE)
  )
}

cluster_from_signature_scores <- function(score_dt, dataset_id, k = DEFAULT_K_CLUSTERS) {
  score_cols <- names(score_dt)[grepl("^score_", names(score_dt))]

  x <- as.matrix(score_dt[, ..score_cols])
  rownames(x) <- score_dt$cell_id

  for (j in seq_len(ncol(x))) {
    med <- median(x[, j], na.rm = TRUE)
    if (!is.finite(med)) med <- 0
    x[!is.finite(x[, j]), j] <- med
  }

  x <- scale(x)
  x[!is.finite(x)] <- 0

  k_use <- min(k, max(2, floor(nrow(x) / MIN_CLUSTER_CELLS)))
  if (k_use < 2) k_use <- 2

  km <- stats::kmeans(x, centers = k_use, nstart = 50, iter.max = 100)

  data.table(
    dataset = dataset_id,
    cell_id = rownames(x),
    context_cluster = paste0(dataset_id, "_SignatureCluster_", sprintf("%02d", km$cluster)),
    clustering_space = "frozen_signature_score_space",
    k_clusters = k_use
  )
}

get_feature_cols <- function(dt) {
  exclude <- c(
    "task", "weak_label", "dataset", "object_id", "group_id", "group_key",
    "safety_contrast_class_05B", "n_cells", "sample_weight_equal",
    "sample_weight_sqrt_cells", "row_id", "fold", "predicted_probability"
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
    if (scale) x <- (x - prep$median[[fc]]) / prep$sd[[fc]]
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

stamp("读取 04A / 09B frozen inputs。")

manual_plan <- as.data.table(LOCAL_MANUAL_FILE_PLAN)
manual_plan[, exists := file.exists(expected_local_path)]
manual_plan[, size_bytes := ifelse(exists, file.info(expected_local_path)$size, NA_real_)]
manual_plan[, size_valid := exists & is.finite(size_bytes) & size_bytes >= expected_min_size_bytes]
manual_plan[, status := fifelse(
  size_valid,
  "local_file_ready",
  fifelse(exists, "local_file_exists_but_too_small_or_partial", "local_file_missing")
)]

atomic_write_csv(as.data.frame(manual_plan), manual_download_plan_csv)

if (any(manual_plan$status != "local_file_ready")) {
  print(manual_plan)
  stop("09I V9 stopped: local manual file missing or partial.")
}

if (!file.exists(known_marker_path)) {
  stop("找不到已确认 04A marker panel：", known_marker_path)
}

raw_marker_dt <- data.table::fread(known_marker_path, data.table = FALSE, showProgress = FALSE)
marker_panel_full <- standardize_marker_panel_from_dt(raw_marker_dt, known_marker_path)
marker_panel <- unique(marker_panel_full[, .(category = as.character(category), gene = as.character(gene))])
marker_panel <- marker_panel[!is.na(category) & category != "" & !is.na(gene) & gene != ""]

marker_audit <- marker_panel[
  ,
  .(n_genes = uniqueN(gene), genes = paste(unique(gene), collapse = ";")),
  by = category
]

atomic_write_csv(as.data.frame(marker_audit), marker_audit_csv)

if (!file.exists(input_ideal_training)) stop("找不到 09B ideal training table：", input_ideal_training)
if (!file.exists(input_safety_training)) stop("找不到 09B safety training table：", input_safety_training)

ideal_train <- data.table::fread(input_ideal_training, data.table = TRUE, showProgress = FALSE)
safety_train <- data.table::fread(input_safety_training, data.table = TRUE, showProgress = FALSE)

ideal_train[, weak_label := as.integer(weak_label)]
safety_train[, weak_label := as.integer(weak_label)]

ideal_feature_cols <- get_feature_cols(ideal_train)
safety_feature_cols <- get_feature_cols(safety_train)

dataset_plan <- data.table(
  gse_id = c("GSE184950", "GSE243639"),
  planned_role = c("raw_only_skipped_in_09I_V9", "disease_context_marker_targeted_validation"),
  use_for_primary_claim = FALSE,
  claim_boundary = c(
    "GSE184950 has no processed matrix selected; RAW.tar not processed in V9.",
    "GSE243639 processed count table used for marker-targeted disease-context transcriptomic validation."
  )
)

atomic_write_csv(as.data.frame(dataset_plan), dataset_plan_csv)

stamp("Marker categories：", uniqueN(marker_panel$category))
stamp("Marker genes：", uniqueN(marker_panel$gene))

stamp("执行 / 恢复 V9 marker-targeted local import checkpoint。")

target_genes <- unique(marker_panel$gene)
local_path <- manual_plan$expected_local_path[1]

prev_target_counts_rds <- file.path(PREVIOUS_V8_OBJECTS_DIR, "09I_V8_marker_target_counts_sparse.rds")
prev_target_norm_rds <- file.path(PREVIOUS_V8_OBJECTS_DIR, "09I_V8_marker_target_log1pCPM_sparse.rds")
prev_target_import_audit_csv <- file.path(PREVIOUS_V8_TABLES_DIR, "09I_V8_marker_targeted_import_audit.csv")

checkpoint_loaded <- FALSE

if (isTRUE(RESUME_FROM_EXISTING_CHECKPOINTS) &&
    file.exists(target_counts_rds) &&
    file.exists(target_norm_rds) &&
    file.exists(target_import_audit_csv)) {

  stamp("V9 checkpoint detected：读取 V9 target_counts / target_norm，不重新导入。")

  target_counts <- readRDS(target_counts_rds)
  target_norm <- readRDS(target_norm_rds)
  target_import <- list(audit = data.table::fread(target_import_audit_csv, data.table = TRUE, showProgress = FALSE))

  checkpoint_loaded <- TRUE
}

if (!checkpoint_loaded &&
    isTRUE(RESUME_FROM_EXISTING_CHECKPOINTS) &&
    file.exists(prev_target_counts_rds) &&
    file.exists(prev_target_norm_rds) &&
    file.exists(prev_target_import_audit_csv)) {

  stamp("V8 checkpoint detected：复制并读取 V8 heavy-import outputs，不重新导入。")

  file.copy(prev_target_counts_rds, target_counts_rds, overwrite = TRUE)
  file.copy(prev_target_norm_rds, target_norm_rds, overwrite = TRUE)

  prev_audit <- data.table::fread(prev_target_import_audit_csv, data.table = TRUE, showProgress = FALSE)
  prev_audit[, checkpoint_source := "reused_from_V8"]
  atomic_write_csv(as.data.frame(prev_audit), target_import_audit_csv)

  target_counts <- readRDS(target_counts_rds)
  target_norm <- readRDS(target_norm_rds)
  target_import <- list(audit = prev_audit)

  checkpoint_loaded <- TRUE
}

if (!checkpoint_loaded) {
  stamp("No checkpoint detected：开始 V9 marker-targeted local import。")

  target_import <- read_marker_target_counts_with_library(
    path = local_path,
    target_genes = target_genes,
    chunk_n_genes = TARGET_IMPORT_CHUNK_N_GENES
  )

  target_counts <- target_import$target_counts
  library_size <- target_import$library_size

  target_import$audit[, checkpoint_source := "fresh_V9_import"]
  atomic_write_csv(as.data.frame(target_import$audit), target_import_audit_csv)

  saveRDS(target_counts, target_counts_rds)

  target_norm <- normalize_target_counts_log1pCPM(target_counts, library_size)
  saveRDS(target_norm, target_norm_rds)
}

stamp(
  "V9 marker-targeted checkpoint ready：marker_genes=",
  nrow(target_counts),
  " cells=",
  ncol(target_counts),
  " nnz=",
  length(target_counts@x)
)

stamp("计算 frozen marker-category scores。")

score_res <- score_markers_from_target_norm(
  mat_norm = target_norm,
  marker_panel = marker_panel,
  dataset_id = "GSE243639"
)

cell_scores <- score_res$score_dt
gene_overlap <- score_res$overlap_dt

atomic_write_csv(as.data.frame(gene_overlap), target_gene_overlap_csv)

stamp("执行 frozen signature-space context clustering。")

cluster_dt <- cluster_from_signature_scores(
  score_dt = cell_scores,
  dataset_id = "GSE243639",
  k = DEFAULT_K_CLUSTERS
)

cell_scores <- merge(cell_scores, cluster_dt, by = c("dataset", "cell_id"), all.x = TRUE)

atomic_write_csv(as.data.frame(cell_scores), cell_score_csv)
atomic_write_csv(as.data.frame(cluster_dt), cluster_assignment_csv)

stamp("生成 cluster-level feature table。")

score_cols <- names(cell_scores)[grepl("^score_", names(cell_scores))]

a9_score_cols <- score_cols[grepl("A9", score_cols, ignore.case = TRUE)]
a10_score_cols <- score_cols[grepl("A10", score_cols, ignore.case = TRUE)]

if (length(a9_score_cols) > 0 && length(a10_score_cols) > 0) {
  cell_scores[, A9_minus_A10_score_05A_cell := num(get(a9_score_cols[1])) - num(get(a10_score_cols[1]))]
} else {
  cell_scores[, A9_minus_A10_score_05A_cell := NA_real_]
}

cluster_summary <- cell_scores[
  ,
  c(
    list(
      n_cells = .N,
      clustering_space = unique(clustering_space)[1],
      k_clusters = unique(k_clusters)[1],
      pct_cells_A9_minus_A10_score_05A_gt0 = mean(A9_minus_A10_score_05A_cell > 0, na.rm = TRUE),
      A9_minus_A10_score_05A = mean(A9_minus_A10_score_05A_cell, na.rm = TRUE)
    ),
    lapply(.SD, function(x) mean(num(x), na.rm = TRUE))
  ),
  by = .(dataset, context_cluster),
  .SDcols = score_cols
]

cluster_summary[!is.finite(pct_cells_A9_minus_A10_score_05A_gt0), pct_cells_A9_minus_A10_score_05A_gt0 := NA_real_]
cluster_summary[!is.finite(A9_minus_A10_score_05A), A9_minus_A10_score_05A := NA_real_]

atomic_write_csv(as.data.frame(cluster_summary), cluster_score_summary_csv)

feature_dt <- copy(cluster_summary)
feature_dt[, group_id := context_cluster]
feature_dt[, object_id := paste0(dataset, "_09I_V9_marker_targeted_context")]

for (sc in score_cols) {
  base <- sub("^score_", "", sc)
  marker_name <- paste0("marker_", base)
  if (!marker_name %in% names(feature_dt)) {
    feature_dt[, (marker_name) := get(sc)]
  }
}

atomic_write_csv(as.data.frame(feature_dt), cluster_feature_table_csv)

stamp("应用 09B V4 frozen predictors。")

alignment_list <- list()
prediction_list <- list()

train_list <- list(
  ideal_like_classifier = ideal_train,
  safety_risk_classifier = safety_train
)

feature_list <- list(
  ideal_like_classifier = ideal_feature_cols,
  safety_risk_classifier = safety_feature_cols
)

for (task_name in names(train_list)) {
  train_dt <- copy(train_list[[task_name]])
  feature_cols <- feature_list[[task_name]]

  missing_external <- setdiff(feature_cols, names(feature_dt))
  available_external <- intersect(feature_cols, names(feature_dt))

  alignment_list[[length(alignment_list) + 1L]] <- data.table(
    task_name = task_name,
    n_required_features = length(feature_cols),
    n_available_external_features = length(available_external),
    n_missing_external_features = length(missing_external),
    feature_alignment_fraction = ifelse(length(feature_cols) > 0, length(available_external) / length(feature_cols), NA_real_),
    missing_features = paste(missing_external, collapse = ";"),
    prediction_ready = length(missing_external) == 0
  )

  if (length(missing_external) == 0 && length(feature_cols) > 0) {
    glm_res <- fit_full_logistic_predict(train_dt, feature_dt, feature_cols)
    rf_res <- fit_full_rf_predict(train_dt, feature_dt, feature_cols)

    pred_dt <- data.table(
      dataset = feature_dt$dataset,
      context_cluster = feature_dt$context_cluster,
      group_id = feature_dt$group_id,
      object_id = feature_dt$object_id,
      n_cells = feature_dt$n_cells,
      task_name = task_name,
      logistic_success = glm_res$success,
      logistic_message = glm_res$message,
      logistic_predicted_probability = glm_res$probs,
      random_forest_success = rf_res$success,
      random_forest_message = rf_res$message,
      random_forest_predicted_probability = rf_res$probs
    )

    prediction_list[[length(prediction_list) + 1L]] <- pred_dt
  }
}

ml_alignment <- rbindlist(alignment_list, fill = TRUE)
atomic_write_csv(as.data.frame(ml_alignment), ml_alignment_csv)

if (length(prediction_list) > 0) {
  pred_all <- rbindlist(prediction_list, fill = TRUE)

  pred_wide <- dcast(
    pred_all,
    dataset + context_cluster + group_id + object_id + n_cells ~ task_name,
    value.var = c("logistic_predicted_probability", "random_forest_predicted_probability")
  )

  il <- "logistic_predicted_probability_ideal_like_classifier"
  sl <- "logistic_predicted_probability_safety_risk_classifier"
  ir <- "random_forest_predicted_probability_ideal_like_classifier"
  sr <- "random_forest_predicted_probability_safety_risk_classifier"

  if (all(c(il, sl) %in% names(pred_wide))) {
    pred_wide[, priority_index_logistic := num(get(il)) - num(get(sl))]
    pred_wide[, context_prediction_class_logistic := fifelse(
      num(get(sl)) >= 0.5,
      "safety_risk_like_or_lower_priority_context",
      fifelse(num(get(il)) >= 0.5 & num(get(sl)) < 0.5,
              "ideal_like_context",
              "mixed_or_uncertain_context")
    )]
  }

  if (all(c(ir, sr) %in% names(pred_wide))) {
    pred_wide[, priority_index_random_forest := num(get(ir)) - num(get(sr))]
  }

  pred_wide[, priority_index_consensus := rowMeans(
    cbind(
      if ("priority_index_logistic" %in% names(pred_wide)) priority_index_logistic else NA_real_,
      if ("priority_index_random_forest" %in% names(pred_wide)) priority_index_random_forest else NA_real_
    ),
    na.rm = TRUE
  )]

  atomic_write_csv(as.data.frame(pred_wide), prediction_csv)
  atomic_write_csv(as.data.frame(pred_wide), priority_summary_csv)
} else {
  pred_wide <- data.table(message = "Prediction skipped due to feature mismatch.")
  atomic_write_csv(as.data.frame(pred_wide), prediction_csv)
  atomic_write_csv(as.data.frame(pred_wide), priority_summary_csv)
}

stamp("生成 09I V9 PDF figures。")

import_dt <- copy(target_import$audit)
import_dt[, dataset := "GSE243639"]

p_import <- ggplot(import_dt, aes(x = dataset, y = cells)) +
  geom_col(fill = "grey55", color = "grey25", linewidth = 0.25, width = 0.68) +
  geom_text(
    aes(label = paste0(
      "cells=", cells,
      "\nscanned genes=", scanned_genes,
      "\nmarker genes=", target_marker_rows_kept
    )),
    vjust = -0.25,
    size = 3.2
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(
    title = "09I V9 marker-targeted local import summary",
    subtitle = "Only frozen marker genes were retained; library size was computed from all parsed genes",
    x = NULL,
    y = "Cells"
  ) +
  theme_pub(base_size = 10.5)

save_pdf_plot(p_import, fig_import_pdf, width = 9.5, height = 5.8)

overlap_plot <- copy(gene_overlap)
overlap_plot[, category_label := paste0(short_signature_label(category), " (", n_overlap_genes, "/", n_marker_genes, ")")]
overlap_plot <- overlap_plot[order(overlap_fraction)]
overlap_plot[, category_label := factor(category_label, levels = category_label)]

p_overlap <- ggplot(overlap_plot, aes(x = category_label, y = overlap_fraction)) +
  geom_col(fill = "grey55", color = "grey25", linewidth = 0.18, width = 0.68) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Frozen marker-gene overlap in GSE243639",
    x = NULL,
    y = "Overlap fraction"
  ) +
  theme_pub(base_size = 9.5) +
  theme(axis.text.y = element_text(size = 7.5))

save_pdf_plot(p_overlap, fig_gene_overlap_pdf, width = 12.5, height = 7.8)

size_dt <- copy(cluster_summary)
size_dt[, cluster_short := gsub("^GSE243639_SignatureCluster_", "SC", context_cluster)]
size_dt[, cluster_short := factor(cluster_short, levels = size_dt[order(n_cells)]$cluster_short)]

p_size <- ggplot(size_dt, aes(x = cluster_short, y = n_cells)) +
  geom_col(fill = "grey55", color = "grey25", linewidth = 0.18, width = 0.68) +
  geom_text(aes(label = n_cells), hjust = -0.10, size = 3.0) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "GSE243639 disease-context signature-space cluster sizes",
    x = NULL,
    y = "Number of cells"
  ) +
  theme_pub(base_size = 10.5) +
  theme(plot.margin = ggplot2::margin(8, 35, 8, 8))

save_pdf_plot(p_size, fig_cluster_size_pdf, width = 9.8, height = 6.2)

heat_mat <- as.matrix(cluster_summary[, ..score_cols])
rownames(heat_mat) <- gsub("^GSE243639_SignatureCluster_", "SC", cluster_summary$context_cluster)

if (nrow(heat_mat) > 1) {
  heat_scaled <- scale(heat_mat)
} else {
  heat_scaled <- heat_mat
}
heat_scaled[!is.finite(heat_scaled)] <- 0
heat_scaled[heat_scaled > 2] <- 2
heat_scaled[heat_scaled < -2] <- -2

heat_long <- as.data.table(heat_scaled, keep.rownames = "cluster_label")
heat_long <- melt(
  heat_long,
  id.vars = "cluster_label",
  variable.name = "signature",
  value.name = "z_score",
  variable.factor = FALSE,
  value.factor = FALSE
)

heat_long[, signature_label := short_signature_label(signature)]
heat_long[, cluster_label := factor(cluster_label, levels = rev(unique(cluster_label)))]

p_heat <- ggplot(heat_long, aes(x = signature_label, y = cluster_label, fill = z_score)) +
  geom_tile(color = "white", linewidth = 0.25) +
  scale_fill_gradient2(
    low = "navy",
    mid = "white",
    high = "firebrick",
    midpoint = 0,
    limits = c(-2, 2),
    name = "Scaled\nscore"
  ) +
  labs(
    title = "Frozen signature scores across GSE243639 disease-context clusters",
    subtitle = "Column-scaled scores for visualization only",
    x = NULL,
    y = NULL
  ) +
  theme_pub(base_size = 9.5) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 8.5)
  )

save_pdf_plot(p_heat, fig_signature_heatmap_pdf, width = 14.8, height = 6.8)

if (!("message" %in% names(pred_wide))) {
  prob_cols <- names(pred_wide)[grepl("predicted_probability", names(pred_wide))]
  prob_long <- melt(
    pred_wide,
    id.vars = c("dataset", "context_cluster", "n_cells"),
    measure.vars = prob_cols,
    variable.name = "model_probability",
    value.name = "probability",
    variable.factor = FALSE,
    value.factor = FALSE
  )

  prob_long[, model := fifelse(grepl("^logistic", model_probability), "Logistic", "Random forest")]
  prob_long[, task := fifelse(grepl("ideal_like", model_probability), "Ideal-like", "Safety-risk")]
  prob_long[, cluster_short := gsub("^GSE243639_SignatureCluster_", "SC", context_cluster)]
  prob_long[, cluster_short := factor(cluster_short, levels = unique(cluster_short))]

  p_prob <- ggplot(prob_long, aes(x = cluster_short, y = probability, fill = task)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.62, color = "grey25", linewidth = 0.15) +
    geom_hline(yintercept = 0.5, linetype = "dashed", linewidth = 0.35, color = "grey45") +
    facet_wrap(~ model, ncol = 1) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.03))) +
    labs(
      title = "Frozen predictor probabilities in GSE243639 disease-context clusters",
      subtitle = "Disease-context marker-targeted validation only; not graft validation",
      x = "Signature-space context cluster",
      y = "Predicted probability",
      fill = "Prediction task"
    ) +
    theme_pub(base_size = 10) +
    theme(axis.text.x = element_text(size = 8.5, face = "bold"))
} else {
  p_prob <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "Prediction skipped due to feature mismatch.", size = 5) +
    theme_void()
}

save_pdf_plot(p_prob, fig_probability_pdf, width = 10.8, height = 7.3)

if (!("message" %in% names(pred_wide)) && "priority_index_consensus" %in% names(pred_wide)) {
  priority_dt <- copy(pred_wide)
  priority_dt[, cluster_short := gsub("^GSE243639_SignatureCluster_", "SC", context_cluster)]
  priority_dt[, cluster_short := factor(cluster_short, levels = priority_dt[order(priority_index_consensus)]$cluster_short)]

  p_priority <- ggplot(priority_dt, aes(x = cluster_short, y = priority_index_consensus)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.35, color = "grey45") +
    geom_col(fill = "grey55", color = "grey25", linewidth = 0.18, width = 0.68) +
    coord_flip() +
    labs(
      title = "GSE243639 disease-context cluster priority index",
      subtitle = "Consensus priority index = mean ideal-like probability − safety-risk probability",
      x = NULL,
      y = "Consensus priority index"
    ) +
    theme_pub(base_size = 10.5)
} else {
  p_priority <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "Priority index unavailable.", size = 5) +
    theme_void()
}

save_pdf_plot(p_priority, fig_priority_pdf, width = 9.8, height = 6.3)

n_cells_total <- nrow(cell_scores)
n_clusters_total <- nrow(cluster_summary)
n_ready_tasks <- sum(ml_alignment$prediction_ready == TRUE, na.rm = TRUE)

if (!("message" %in% names(pred_wide)) && "context_prediction_class_logistic" %in% names(pred_wide)) {
  n_safety_like <- sum(pred_wide$context_prediction_class_logistic == "safety_risk_like_or_lower_priority_context", na.rm = TRUE)
  n_ideal_like <- sum(pred_wide$context_prediction_class_logistic == "ideal_like_context", na.rm = TRUE)
  n_mixed <- sum(pred_wide$context_prediction_class_logistic == "mixed_or_uncertain_context", na.rm = TRUE)
} else {
  n_safety_like <- NA_integer_
  n_ideal_like <- NA_integer_
  n_mixed <- NA_integer_
}

summary_dt <- data.table(
  Metric = c(
    "Context dataset used",
    "Skipped context dataset",
    "Imported cells",
    "Parsed genes for library size",
    "Retained marker genes",
    "Recovered signature-space clusters",
    "ML tasks ready",
    "Logistic safety-risk-like clusters",
    "Logistic ideal-like clusters",
    "Mixed/uncertain clusters",
    "Interpretation boundary"
  ),
  Value = c(
    "GSE243639 processed count table",
    "GSE184950 raw-only candidate",
    as.character(n_cells_total),
    as.character(target_import$audit$scanned_genes[1]),
    as.character(nrow(target_counts)),
    as.character(n_clusters_total),
    as.character(n_ready_tasks),
    as.character(n_safety_like),
    as.character(n_ideal_like),
    as.character(n_mixed),
    "Disease-context marker-targeted validation only"
  )
)

summary_dt[, row_id := seq_len(.N)]
summary_dt[, y := rev(row_id)]

p_summary <- ggplot(summary_dt, aes(y = y)) +
  annotate(
    "text",
    x = 0,
    y = max(summary_dt$y) + 1.0,
    label = "09I V9 disease-context validation summary",
    hjust = 0,
    fontface = "bold",
    size = 5.0
  ) +
  geom_text(aes(x = 0.02, label = Metric), hjust = 0, fontface = "bold", size = 3.55) +
  geom_text(aes(x = 0.58, label = Value), hjust = 0, size = 3.55) +
  annotate(
    "text",
    x = 0.02,
    y = 0.35,
    label = paste(
      "Claim boundary:",
      "marker-targeted disease-context transcriptomic comparison;",
      "not primary graft validation, clinical safety, treatment efficacy, or projection evidence."
    ),
    hjust = 0,
    size = 3.05
  ) +
  xlim(0, 1.65) +
  ylim(0, max(summary_dt$y) + 1.6) +
  theme_void()

save_pdf_plot(p_summary, fig_summary_pdf, width = 12.8, height = 7.2)

stamp("写出 09I V9 key findings / method note / report。")

key_findings <- data.table(
  item = c(
    "context_dataset_used",
    "context_dataset_skipped",
    "local_file_size_bytes",
    "scanned_genes_for_library_size",
    "retained_marker_genes",
    "imported_cells",
    "context_clusters",
    "marker_categories",
    "ml_prediction_ready_tasks",
    "logistic_safety_risk_like_context_clusters",
    "logistic_ideal_like_context_clusters",
    "mixed_or_uncertain_context_clusters",
    "incomplete_gzip_warning_recorded",
    "claim_boundary"
  ),
  value = c(
    "GSE243639",
    "GSE184950 raw-only candidate skipped in marker-targeted V9",
    as.character(manual_plan$size_bytes[1]),
    as.character(target_import$audit$scanned_genes[1]),
    as.character(nrow(target_counts)),
    as.character(n_cells_total),
    as.character(n_clusters_total),
    as.character(uniqueN(marker_panel$category)),
    as.character(n_ready_tasks),
    as.character(n_safety_like),
    as.character(n_ideal_like),
    as.character(n_mixed),
    as.character(target_import$audit$incomplete_gzip_warning[1]),
    "Disease-context marker-targeted validation only; not primary graft validation and not clinical safety/efficacy validation."
  )
)

atomic_write_csv(as.data.frame(key_findings), key_findings_csv)

method_lines <- c(
  "09I V9 disease-context validation method and claim-boundary note",
  "",
  "Purpose:",
  "09I V9 applies the frozen 04A marker scoring and 09B reduced-feature predictors to a disease-context processed count table from GSE243639.",
  "",
  "Reason for V9 marker-targeted import:",
  "The available GSE243639 processed count table was large and produced an incomplete gzip warning near the terminal rows.",
  "Full dense import failed due to memory allocation, and full sparse import was unnecessary for frozen marker-score validation.",
  "Therefore, V9 scanned the count table in chunks, computed library sizes using all parsed genes, and retained only frozen marker genes for log1p(CPM) scoring.",
  "",
  "Method:",
  "No internet download was performed.",
  "The local GSE243639_Filtered_count_table.csv.gz file was checked for size before import.",
  "Frozen 04A marker genes were retained, normalized by library size computed across all parsed genes, and summarized into marker-category scores.",
  "Disease-context clusters were recovered in frozen signature-score space.",
  "Reduced non-direct marker-rule-derived predictors from 09B V4 were applied without retraining.",
  "",
  "Claim boundary:",
  "09I V9 is a disease-context marker-targeted transcriptomic validation module.",
  "It does not replace 09E primary external validation.",
  "It does not prove graft function, anatomical projection, host integration, clinical safety, tumorigenicity, treatment response, or therapeutic efficacy."
)

writeLines(method_lines, method_note_txt)

report_lines <- c(
  "09I V9 disease-context validation report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Manual local file plan:",
  capture.output(print(manual_plan)),
  "",
  "Import audit:",
  capture.output(print(target_import$audit)),
  "",
  "Marker audit:",
  capture.output(print(marker_audit)),
  "",
  "Gene overlap:",
  capture.output(print(gene_overlap)),
  "",
  "ML alignment:",
  capture.output(print(ml_alignment)),
  "",
  "Key findings:",
  capture.output(print(key_findings)),
  "",
  "Output directories:",
  out_tables_dir,
  out_figures_dir
)

writeLines(report_lines, report_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

required_outputs <- c(
  dataset_plan_csv,
  manual_download_plan_csv,
  marker_audit_csv,
  target_import_audit_csv,
  target_gene_overlap_csv,
  cell_score_csv,
  cluster_assignment_csv,
  cluster_score_summary_csv,
  cluster_feature_table_csv,
  ml_alignment_csv,
  prediction_csv,
  priority_summary_csv,
  key_findings_csv,
  method_note_txt,
  session_info_txt,
  report_txt,
  target_counts_rds,
  target_norm_rds,
  fig_import_pdf,
  fig_gene_overlap_pdf,
  fig_cluster_size_pdf,
  fig_signature_heatmap_pdf,
  fig_probability_pdf,
  fig_priority_pdf,
  fig_summary_pdf
)

output_check <- data.table(
  file = required_outputs,
  exists = file.exists(required_outputs),
  size_bytes = ifelse(file.exists(required_outputs), file.info(required_outputs)$size, NA_real_)
)

atomic_write_csv(as.data.frame(output_check), output_check_csv)

bad <- output_check[!exists | is.na(size_bytes) | size_bytes <= 0]
if (nrow(bad) > 0) {
  print(bad)
  stop("09I V9 输出验证失败。")
}

cat("\n============================================================\n")
cat("09I disease-context validation FINAL V9 CHECKPOINT RESUME SAFE FIGURES 运行结束\n")
cat("============================================================\n\n")

cat("Context dataset used：GSE243639\n")
cat("Context dataset skipped：GSE184950 raw-only candidate\n")
cat("Local file size bytes：", manual_plan$size_bytes[1], "\n")
cat("Scanned genes for library size：", target_import$audit$scanned_genes[1], "\n")
cat("Retained marker genes：", nrow(target_counts), "\n")
cat("Imported cells：", n_cells_total, "\n")
cat("Context clusters：", n_clusters_total, "\n")
cat("Marker categories：", uniqueN(marker_panel$category), "\n")
cat("ML prediction ready tasks：", n_ready_tasks, "\n")
cat("Logistic safety-risk-like context clusters：", n_safety_like, "\n")
cat("Logistic ideal-like context clusters：", n_ideal_like, "\n")
cat("Mixed/uncertain context clusters：", n_mixed, "\n")
cat("Incomplete gzip warning recorded：", target_import$audit$incomplete_gzip_warning[1], "\n\n")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n")
cat(out_objects_dir, "\n\n")

cat("关键输出：\n")
cat(target_import_audit_csv, "\n")
cat(target_gene_overlap_csv, "\n")
cat(cluster_score_summary_csv, "\n")
cat(ml_alignment_csv, "\n")
cat(prediction_csv, "\n")
cat(priority_summary_csv, "\n")
cat(key_findings_csv, "\n")
cat(method_note_txt, "\n\n")

cat("PDF 图：\n")
cat(fig_import_pdf, "\n")
cat(fig_gene_overlap_pdf, "\n")
cat(fig_cluster_size_pdf, "\n")
cat(fig_signature_heatmap_pdf, "\n")
cat(fig_probability_pdf, "\n")
cat(fig_priority_pdf, "\n")
cat(fig_summary_pdf, "\n\n")

cat("✅ 09I disease-context validation FINAL V9 CHECKPOINT RESUME SAFE FIGURES 完成。\n")
