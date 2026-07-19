
cat("\n[11I FINAL] Starting module-score correlation...\n")
cat("[11I FINAL] Mode: complete standalone 11I rebuild; no previous 11I dependency; no internet; no 00-10P rerun.\n")
cat("[11I FINAL] Claim boundary: module-score correlation only; no clinical prediction or clinical biomarker claim.\n")
cat("[11I FINAL] Figure style: Nature-style color palette, not grayscale-only.\n")

graphics.off()

project_root <- "D:/PD_Graft_Project"

object_root <- file.path(project_root, "02_objects")
table_root <- file.path(project_root, "03_tables")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "11I_module_score_correlation_FINAL_COMPLETE_STANDALONE"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "11I_module_score_correlation_FINAL_COMPLETE_STANDALONE_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "11I_module_score_correlation_FINAL_COMPLETE_STANDALONE"
)

dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_text_dir, recursive = TRUE, showWarnings = FALSE)

safe_chr <- function(value_obj) {
  out <- as.character(value_obj)
  out[is.na(out)] <- ""
  out
}

safe_num <- function(value_obj) {
  suppressWarnings(as.numeric(value_obj))
}

clean_space <- function(value_obj) {
  out <- safe_chr(value_obj)
  out <- gsub("^\\s+|\\s+$", "", out)
  out <- gsub("[\r\n\t]+", " ", out)
  out <- gsub("\\s+", " ", out)
  out
}

clean_gene_symbol <- function(value_obj) {
  out <- toupper(clean_space(value_obj))
  out <- gsub("[^A-Z0-9.-]", "", out)
  out[out %in% c("", "NA", "NAN", "NULL", "NONE", "GENE", "SYMBOL")] <- ""
  out
}

safe_bind_rows <- function(list_value) {
  if (length(list_value) < 1) return(data.frame(stringsAsFactors = FALSE))
  keep_vec <- rep(FALSE, length(list_value))
  for (idx_value in seq_along(list_value)) {
    keep_vec[idx_value] <- is.data.frame(list_value[[idx_value]]) && nrow(list_value[[idx_value]]) > 0
  }
  list_value <- list_value[keep_vec]
  if (length(list_value) < 1) return(data.frame(stringsAsFactors = FALSE))
  all_cols <- unique(unlist(lapply(list_value, colnames), use.names = FALSE))
  fixed_list <- list()
  for (idx_value in seq_along(list_value)) {
    data_value <- list_value[[idx_value]]
    missing_cols <- setdiff(all_cols, colnames(data_value))
    if (length(missing_cols) > 0) {
      for (col_value in missing_cols) data_value[[col_value]] <- NA
    }
    fixed_list[[idx_value]] <- data_value[, all_cols, drop = FALSE]
  }
  do.call(base::rbind, fixed_list)
}

write_csv_safe <- function(data_value, file_value) {
  utils::write.csv(data_value, file_value, row.names = FALSE, na = "")
  cat("[11I FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[11I FINAL] Wrote:", file_value, "\n")
}

read_table_safe <- function(file_value) {
  if (!file.exists(file_value)) return(data.frame(stringsAsFactors = FALSE))
  ext_value <- tolower(tools::file_ext(file_value))
  out <- data.frame(stringsAsFactors = FALSE)
  tryCatch({
    if (ext_value %in% c("tsv", "txt")) {
      out <- utils::read.table(
        file_value,
        sep = "\t",
        header = TRUE,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        quote = "",
        comment.char = "",
        fill = TRUE
      )
    } else {
      out <- utils::read.csv(file_value, stringsAsFactors = FALSE, check.names = FALSE)
    }
  }, error = function(err_obj) {
    out <<- data.frame(stringsAsFactors = FALSE)
  })
  if (!is.data.frame(out)) out <- data.frame(stringsAsFactors = FALSE)
  out
}

open_pdf_safe <- function(filename, width_value = 10, height_value = 6) {
  file_primary <- file.path(out_fig_dir, filename)
  if (file.exists(file_primary)) suppressWarnings(try(file.remove(file_primary), silent = TRUE))
  ok_value <- TRUE
  tryCatch({
    grDevices::pdf(
      file_primary,
      width = width_value,
      height = height_value,
      onefile = FALSE,
      useDingbats = FALSE,
      paper = "special"
    )
  }, error = function(err_obj) {
    ok_value <<- FALSE
  })
  if (!ok_value) {
    alt_name <- paste0(sub("\\.pdf$", "", filename), "_ALT_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    file_alt <- file.path(out_fig_dir, alt_name)
    grDevices::pdf(
      file_alt,
      width = width_value,
      height = height_value,
      onefile = FALSE,
      useDingbats = FALSE,
      paper = "special"
    )
    cat("[11I FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
    return(file_alt)
  }
  file_primary
}

new_canvas <- function() {
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", xpd = FALSE)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
}

draw_title <- function(title_value, subtitle_value = "") {
  text(0.5, 0.965, title_value, cex = 0.98, font = 2, adj = c(0.5, 0.5))
  if (nchar(subtitle_value) > 0) {
    text(0.5, 0.928, subtitle_value, cex = 0.50, col = nature_palette$muted, adj = c(0.5, 0.5))
  }
}

nature_palette <- list(
  ink = "#1D1D1F",
  muted = "#5F6368",
  grid = "#E6E8EB",
  border = "#2F3A45",
  navy = "#3B4992",
  blue = "#4DBBD5",
  teal = "#00A087",
  green = "#3C5488",
  orange = "#E64B35",
  red = "#B2182B",
  purple = "#7E6148",
  gold = "#F39B7F",
  pale_blue = "#EAF2F8",
  pale_orange = "#FDE9DF",
  pale_green = "#E8F3EF",
  white = "#FFFFFF"
)

blend_color <- function(color_low, color_high, fraction_value) {
  fraction_value <- safe_num(fraction_value)
  fraction_value[!is.finite(fraction_value)] <- 0
  fraction_value[fraction_value < 0] <- 0
  fraction_value[fraction_value > 1] <- 1
  low_rgb <- grDevices::col2rgb(color_low) / 255
  high_rgb <- grDevices::col2rgb(color_high) / 255
  out_colors <- character(length(fraction_value))
  for (idx_color in seq_along(fraction_value)) {
    mixed_rgb <- low_rgb[, 1] * (1 - fraction_value[idx_color]) + high_rgb[, 1] * fraction_value[idx_color]
    out_colors[idx_color] <- grDevices::rgb(mixed_rgb[1], mixed_rgb[2], mixed_rgb[3])
  }
  out_colors
}

nature_continuous_color <- function(value_obj, max_obj, low_color = nature_palette$pale_blue, high_color = nature_palette$navy) {
  value_num <- safe_num(value_obj)
  max_num <- max(safe_num(max_obj), na.rm = TRUE)
  if (!is.finite(max_num) || max_num <= 0) max_num <- 1
  fraction_value <- value_num / max_num
  fraction_value[!is.finite(fraction_value)] <- 0
  fraction_value[fraction_value < 0] <- 0
  fraction_value[fraction_value > 1] <- 1
  blend_color(low_color, high_color, fraction_value)
}

value_to_gray <- function(value_obj, max_obj) {
  nature_continuous_color(value_obj, max_obj, nature_palette$pale_blue, nature_palette$navy)
}

corr_to_nature <- function(value_obj) {
  value_num <- safe_num(value_obj)
  value_num[!is.finite(value_num)] <- 0
  value_num[value_num < -1] <- -1
  value_num[value_num > 1] <- 1
  out_colors <- character(length(value_num))
  neg_idx <- value_num < 0
  pos_idx <- value_num >= 0
  if (any(neg_idx)) {
    out_colors[neg_idx] <- blend_color(nature_palette$navy, nature_palette$white, (value_num[neg_idx] + 1))
  }
  if (any(pos_idx)) {
    out_colors[pos_idx] <- blend_color(nature_palette$white, nature_palette$red, value_num[pos_idx])
  }
  out_colors
}

corr_to_gray <- function(value_obj) {
  corr_to_nature(value_obj)
}

balance_to_nature <- function(value_obj) {
  value_num <- safe_num(value_obj)
  min_num <- min(value_num, na.rm = TRUE)
  max_num <- max(value_num, na.rm = TRUE)
  if (!is.finite(min_num) || !is.finite(max_num) || abs(max_num - min_num) < 1e-12) {
    return(rep(nature_palette$teal, length(value_num)))
  }
  fraction_value <- (value_num - min_num) / (max_num - min_num)
  blend_color(nature_palette$blue, nature_palette$orange, fraction_value)
}

pair_color_from_rho <- function(rho_value) {
  rho_num <- safe_num(rho_value)
  rho_num[!is.finite(rho_num)] <- 0
  out_colors <- character(length(rho_num))
  pos_idx <- rho_num >= 0
  neg_idx <- rho_num < 0
  if (any(pos_idx)) out_colors[pos_idx] <- blend_color(nature_palette$pale_orange, nature_palette$orange, pmin(abs(rho_num[pos_idx]), 1))
  if (any(neg_idx)) out_colors[neg_idx] <- blend_color(nature_palette$pale_blue, nature_palette$navy, pmin(abs(rho_num[neg_idx]), 1))
  out_colors
}

module_family_color <- function(module_name_value) {
  module_text <- tolower(safe_chr(module_name_value))
  out_colors <- rep(nature_palette$navy, length(module_text))
  out_colors[grepl("da_core|a9|a10|neuronal|synaptic|axon|projection", module_text)] <- nature_palette$teal
  out_colors[grepl("risk|stress|inflammatory|off_target|cell_cycle", module_text)] <- nature_palette$orange
  out_colors[grepl("pd_genetic", module_text)] <- nature_palette$purple
  out_colors
}

extract_dataset_from_path <- function(file_value) {
  file_value <- safe_chr(file_value)
  out <- regmatches(file_value, regexpr("GSE[0-9]+", file_value))
  if (length(out) < 1 || out == "-1") return("unknown_dataset")
  out
}

extract_sample_from_path <- function(file_value) {
  base_value <- basename(file_value)
  out <- sub("\\.rds$", "", base_value, ignore.case = TRUE)
  out <- gsub("[^A-Za-z0-9._-]+", "_", out)
  out
}

module_gene_list <- list(
  DA_core = c("TH", "DDC", "SLC6A3", "SLC18A2", "NR4A2", "FOXA2", "LMX1A", "LMX1B", "EN1", "PITX3", "ALDH1A1"),
  A9_like = c("SOX6", "ALDH1A1", "KCNJ6", "DCC", "GIRK2", "VGF", "SLC10A4", "DAB1", "LMO3"),
  A10_like = c("CALB1", "OTX2", "SOX6", "SLC17A6", "VIP", "NTS", "GRP", "CRHBP"),
  projection_competence = c("DCC", "ROBO1", "ROBO2", "SLIT1", "SLIT2", "NTN1", "SEMA3A", "SEMA3C", "PLXNA4", "EPHA4", "EPHB1", "EFNA5", "NCAM1", "L1CAM"),
  axon_guidance = c("GAP43", "NEFL", "NEFM", "NEFH", "CNTN2", "DCX", "TUBB3", "MAP1B", "STMN2", "DPYSL2", "DPYSL3"),
  synaptic_maturation = c("SYN1", "SYP", "SNAP25", "STX1A", "VAMP2", "DLG4", "SHANK2", "SYT1", "RIMS1", "UNC13A"),
  neuronal_maturation = c("MAP2", "RBFOX3", "TUBB3", "DCX", "STMN2", "NEFL", "NEFM", "CAMK2D", "GAP43"),
  cell_cycle_proliferation_risk = c("MKI67", "TOP2A", "HMGB2", "CCNB1", "CCNB2", "CDK1", "PCNA", "MCM2", "MCM5", "TYMS"),
  off_target_non_DA_risk = c("GFAP", "AQP4", "OLIG1", "OLIG2", "PDGFRA", "COL1A1", "COL3A1", "PECAM1", "VWF", "EPCAM"),
  stress_p53_apoptosis = c("TP53", "BBC3", "BAX", "BCL2L11", "CASP2", "CASP3", "CASP9", "DDIT3", "FOS", "JUN", "HSPA1A", "HSP90AA1"),
  inflammatory_NFkB = c("NFKB1", "RELA", "TNF", "IL1B", "IL6", "CXCL8", "CCL2", "TLR4", "IRF1", "STAT1"),
  PD_genetic_context_limited = c("CAMK2D", "LRRK2", "PARK7", "GCH1", "FYN", "BCKDK", "FBXO7", "KANSL1", "NUCKS1")
)

module_direction_df <- data.frame(
  module_name = names(module_gene_list),
  module_direction = ifelse(grepl("risk|stress|inflammatory|off_target|cell_cycle", names(module_gene_list), ignore.case = TRUE),
                            "risk_associated", "favorable_or_identity_associated"),
  n_genes_defined = as.integer(vapply(module_gene_list, length, integer(1))),
  stringsAsFactors = FALSE
)
write_csv_safe(module_direction_df, file.path(out_table_dir, "11I_FINAL_module_signature_catalog.csv"))

find_latest_file <- function(term_values, root_value = table_root, max_n = 1) {
  files <- list.files(root_value, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
  if (length(files) < 1) return(character(0))
  info <- file.info(files)
  files <- files[is.finite(info$size) & info$size > 0 & info$size < 80 * 1024 * 1024]
  files <- files[!grepl("11I_module_score_correlation", files, ignore.case = TRUE)]
  path_lower <- tolower(files)
  keep <- rep(TRUE, length(files))
  for (term_value in tolower(safe_chr(term_values))) keep <- keep & grepl(term_value, path_lower, fixed = TRUE)
  hits <- files[keep]
  if (length(hits) < 1) return(character(0))
  hit_info <- file.info(hits)
  hits <- hits[order(hit_info$mtime, decreasing = TRUE)]
  unique(hits)[seq_len(min(max_n, length(unique(hits))))]
}

file_11h_marker <- find_latest_file(c("11h", "candidate_transcriptomic_marker_signature_table"), max_n = 1)
marker_11h_df <- data.frame(stringsAsFactors = FALSE)
if (length(file_11h_marker) > 0) {
  marker_11h_df <- read_table_safe(file_11h_marker[1])
}
write_csv_safe(
  data.frame(
    detected_11H_marker_table = ifelse(length(file_11h_marker) > 0, file_11h_marker[1], ""),
    rows_detected = nrow(marker_11h_df),
    stringsAsFactors = FALSE
  ),
  file.path(out_table_dir, "11I_FINAL_optional_11H_marker_table_detection.csv")
)

get_matrix_from_object <- function(obj_value) {

  mat_value <- NULL

  if (inherits(obj_value, "Seurat")) {
    tryCatch({
      if (requireNamespace("Seurat", quietly = TRUE)) {
        assay_names <- tryCatch(names(obj_value@assays), error = function(err_obj) character(0))
        assay_use <- ifelse("RNA" %in% assay_names, "RNA", ifelse(length(assay_names) > 0, assay_names[1], ""))
        if (assay_use != "") {
          mat_value <- tryCatch(
            Seurat::GetAssayData(obj_value, assay = assay_use, slot = "data"),
            error = function(err_obj) NULL
          )
          if (is.null(mat_value) || nrow(mat_value) < 1) {
            mat_value <- tryCatch(
              Seurat::GetAssayData(obj_value, assay = assay_use, slot = "counts"),
              error = function(err_obj) NULL
            )
          }
        }
      }
    }, error = function(err_obj) {
      mat_value <<- NULL
    })
  }

  if (is.null(mat_value)) {
    tryCatch({
      if (requireNamespace("SummarizedExperiment", quietly = TRUE) && inherits(obj_value, "SummarizedExperiment")) {
        assay_names <- SummarizedExperiment::assayNames(obj_value)
        assay_use <- ifelse("logcounts" %in% assay_names, "logcounts", ifelse("counts" %in% assay_names, "counts", assay_names[1]))
        mat_value <- SummarizedExperiment::assay(obj_value, assay_use)
      }
    }, error = function(err_obj) {
      mat_value <<- NULL
    })
  }

  if (is.null(mat_value) && (is.matrix(obj_value) || inherits(obj_value, "Matrix"))) {
    mat_value <- obj_value
  }

  if (is.null(mat_value) && is.list(obj_value)) {
    candidate_names <- c("data", "counts", "exprs", "expression", "matrix", "mat", "assay", "RNA")
    for (name_value in candidate_names) {
      if (!is.null(mat_value)) next
      if (name_value %in% names(obj_value)) {
        candidate_obj <- obj_value[[name_value]]
        if (is.matrix(candidate_obj) || inherits(candidate_obj, "Matrix")) mat_value <- candidate_obj
        if (is.list(candidate_obj) && is.null(mat_value)) {
          for (inner_name in c("data", "counts", "matrix", "mat")) {
            if (!is.null(mat_value)) next
            if (inner_name %in% names(candidate_obj)) {
              inner_obj <- candidate_obj[[inner_name]]
              if (is.matrix(inner_obj) || inherits(inner_obj, "Matrix")) mat_value <- inner_obj
            }
          }
        }
      }
    }
  }

  if (is.null(mat_value)) return(NULL)
  if (is.null(rownames(mat_value)) || is.null(colnames(mat_value))) return(NULL)
  if (nrow(mat_value) < 5 || ncol(mat_value) < 5) return(NULL)
  mat_value
}

get_metadata_from_object <- function(obj_value, cell_names) {
  meta_value <- data.frame(row.names = cell_names, cell_id = cell_names, stringsAsFactors = FALSE)

  if (inherits(obj_value, "Seurat")) {
    tryCatch({
      md <- obj_value@meta.data
      if (is.data.frame(md) && nrow(md) > 0) {
        common_cells <- intersect(cell_names, rownames(md))
        if (length(common_cells) > 0) {
          meta_value <- md[cell_names, , drop = FALSE]
          meta_value$cell_id <- rownames(meta_value)
        }
      }
    }, error = function(err_obj) {
      meta_value <<- data.frame(row.names = cell_names, cell_id = cell_names, stringsAsFactors = FALSE)
    })
  }

  if (!inherits(obj_value, "Seurat")) {
    tryCatch({
      if (requireNamespace("SummarizedExperiment", quietly = TRUE) && inherits(obj_value, "SummarizedExperiment")) {
        cd <- as.data.frame(SummarizedExperiment::colData(obj_value))
        if (is.data.frame(cd) && nrow(cd) > 0) {
          common_cells <- intersect(cell_names, rownames(cd))
          if (length(common_cells) > 0) {
            meta_value <- cd[cell_names, , drop = FALSE]
            meta_value$cell_id <- rownames(meta_value)
          }
        }
      }
    }, error = function(err_obj) {
      meta_value <<- data.frame(row.names = cell_names, cell_id = cell_names, stringsAsFactors = FALSE)
    })
  }

  if (!is.data.frame(meta_value) || nrow(meta_value) != length(cell_names)) {
    meta_value <- data.frame(row.names = cell_names, cell_id = cell_names, stringsAsFactors = FALSE)
  }
  meta_value
}

pick_state_column <- function(meta_value) {
  if (!is.data.frame(meta_value) || ncol(meta_value) < 1) return("")
  col_names <- colnames(meta_value)
  col_lower <- tolower(col_names)
  priority_terms <- c(
    "cell_state", "state", "cluster", "seurat_clusters", "integrated_snn_res",
    "annotation", "celltype", "cell_type", "ident", "subtype", "class"
  )
  for (term_value in priority_terms) {
    hits <- col_names[grepl(term_value, col_lower, fixed = TRUE)]
    if (length(hits) > 0) {
      for (hit_value in hits) {
        score_value_vec <- safe_chr(meta_value[[hit_value]])
        n_unique <- length(unique(score_value_vec[score_value_vec != ""]))
        if (n_unique >= 2 && n_unique <= max(120, floor(nrow(meta_value) * 0.75))) return(hit_value)
      }
    }
  }
  ""
}

row_means_safe <- function(mat_value) {

  if (inherits(mat_value, "Matrix") && requireNamespace("Matrix", quietly = TRUE)) {
    return(as.numeric(Matrix::rowMeans(mat_value)))
  }
  as.numeric(rowMeans(as.matrix(mat_value), na.rm = TRUE))
}

col_means_safe <- function(mat_value) {
  if (inherits(mat_value, "Matrix") && requireNamespace("Matrix", quietly = TRUE)) {
    return(as.numeric(Matrix::colMeans(mat_value)))
  }
  as.numeric(colMeans(as.matrix(mat_value), na.rm = TRUE))
}

compute_module_scores <- function(mat_value, module_list) {

  gene_upper <- clean_gene_symbol(rownames(mat_value))
  keep_gene <- gene_upper != ""
  mat_value <- mat_value[keep_gene, , drop = FALSE]
  gene_upper <- gene_upper[keep_gene]

  first_idx <- !duplicated(gene_upper)
  mat_value <- mat_value[first_idx, , drop = FALSE]
  gene_upper <- gene_upper[first_idx]
  rownames(mat_value) <- gene_upper

  out <- data.frame(cell_id = colnames(mat_value), stringsAsFactors = FALSE)

  for (module_name in names(module_list)) {
    module_genes <- clean_gene_symbol(module_list[[module_name]])
    module_genes <- unique(module_genes[module_genes != ""])
    hit_genes <- intersect(module_genes, rownames(mat_value))
    if (length(hit_genes) < 1) {
      out[[module_name]] <- rep(NA_real_, ncol(mat_value))
    } else {
      sub_mat <- mat_value[hit_genes, , drop = FALSE]
      out[[module_name]] <- col_means_safe(sub_mat)
    }
  }
  out
}

aggregate_state_scores <- function(score_df, meta_df, state_col, dataset_value, sample_value, file_value) {
  if (!is.data.frame(score_df) || nrow(score_df) < 1) return(data.frame(stringsAsFactors = FALSE))
  if (!is.data.frame(meta_df) || nrow(meta_df) < 1 || state_col == "" || !(state_col %in% colnames(meta_df))) {
    state_values <- rep("all_cells", nrow(score_df))
  } else {
    meta_use <- meta_df[score_df$cell_id, , drop = FALSE]
    state_values <- safe_chr(meta_use[[state_col]])
    state_values[state_values == ""] <- "unlabeled"
  }

  modules <- setdiff(colnames(score_df), "cell_id")
  state_unique <- sort(unique(state_values))
  out_list <- list()

  for (state_value in state_unique) {
    idx <- which(state_values == state_value)
    if (length(idx) < 5) next
    row_out <- data.frame(
      dataset = dataset_value,
      sample_id = sample_value,
      object_file = file_value,
      state_column = state_col,
      state_label = state_value,
      compact_state_label = paste(dataset_value, state_value, sep = " "),
      n_cells = length(idx),
      stringsAsFactors = FALSE
    )
    for (module_name in modules) {
      score_value_vec <- safe_num(score_df[[module_name]][idx])
      row_out[[module_name]] <- ifelse(sum(is.finite(score_value_vec)) > 0, mean(score_value_vec, na.rm = TRUE), NA_real_)
    }
    out_list[[length(out_list) + 1]] <- row_out
  }
  safe_bind_rows(out_list)
}

cat("[11I FINAL] Scanning RDS objects...\n")
rds_files <- character(0)
if (dir.exists(object_root)) {
  rds_files <- list.files(object_root, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
}
rds_files <- rds_files[!grepl("backup|old|tmp|temp|archive|failed", rds_files, ignore.case = TRUE)]
rds_files <- rds_files[file.exists(rds_files)]
rds_files <- unique(rds_files)

if (length(rds_files) > 180) {
  preferred <- rds_files[grepl("04D|annotated|02B|processed|seurat|GSE178265|GSE200610|GSE204796|GSE183248|GSE132758|GSE233885", rds_files, ignore.case = TRUE)]
  if (length(preferred) >= 20) rds_files <- preferred
}
if (length(rds_files) > 140) rds_files <- rds_files[seq_len(140)]

cat("[11I FINAL] Candidate RDS detected:", length(rds_files), "\n")

object_audit_list <- list()
state_score_list <- list()

attempted_count <- 0
readable_count <- 0
scorable_count <- 0
state_row_count <- 0

if (length(rds_files) > 0) {
  for (idx_file in seq_along(rds_files)) {
    file_value <- rds_files[idx_file]
    attempted_count <- attempted_count + 1
    if (attempted_count %% 20 == 0) {
      cat("[11I FINAL] Processing RDS ", attempted_count, "/", length(rds_files), "\n", sep = "")
    }

    obj_value <- NULL
    read_ok <- FALSE
    tryCatch({
      obj_value <- readRDS(file_value)
      read_ok <- TRUE
    }, error = function(err_obj) {
      read_ok <<- FALSE
    })

    if (!read_ok || is.null(obj_value)) {
      object_audit_list[[length(object_audit_list) + 1]] <- data.frame(
        file_path = file_value,
        dataset = extract_dataset_from_path(file_value),
        readable = FALSE,
        scorable = FALSE,
        n_genes = NA_integer_,
        n_cells = NA_integer_,
        state_column = "",
        n_state_rows = 0,
        note = "readRDS_failed",
        stringsAsFactors = FALSE
      )
      next
    }

    readable_count <- readable_count + 1
    mat_value <- get_matrix_from_object(obj_value)
    if (is.null(mat_value)) {
      object_audit_list[[length(object_audit_list) + 1]] <- data.frame(
        file_path = file_value,
        dataset = extract_dataset_from_path(file_value),
        readable = TRUE,
        scorable = FALSE,
        n_genes = NA_integer_,
        n_cells = NA_integer_,
        state_column = "",
        n_state_rows = 0,
        note = "no_matrix_extracted",
        stringsAsFactors = FALSE
      )
      rm(obj_value)
      gc(verbose = FALSE)
      next
    }

    if (ncol(mat_value) > 9000) {
      set.seed(1109)
      keep_cells <- sort(sample(seq_len(ncol(mat_value)), 9000))
      mat_value <- mat_value[, keep_cells, drop = FALSE]
    }

    meta_value <- get_metadata_from_object(obj_value, colnames(mat_value))
    state_col <- pick_state_column(meta_value)

    score_df <- compute_module_scores(mat_value, module_gene_list)
    if (!is.data.frame(score_df) || nrow(score_df) < 5) {
      object_audit_list[[length(object_audit_list) + 1]] <- data.frame(
        file_path = file_value,
        dataset = extract_dataset_from_path(file_value),
        readable = TRUE,
        scorable = FALSE,
        n_genes = nrow(mat_value),
        n_cells = ncol(mat_value),
        state_column = state_col,
        n_state_rows = 0,
        note = "module_score_failed",
        stringsAsFactors = FALSE
      )
      rm(obj_value, mat_value)
      gc(verbose = FALSE)
      next
    }

    dataset_value <- extract_dataset_from_path(file_value)
    sample_value <- extract_sample_from_path(file_value)
    state_scores <- aggregate_state_scores(score_df, meta_value, state_col, dataset_value, sample_value, file_value)

    if (nrow(state_scores) > 0) {
      scorable_count <- scorable_count + 1
      state_row_count <- state_row_count + nrow(state_scores)
      state_score_list[[length(state_score_list) + 1]] <- state_scores
    }

    object_audit_list[[length(object_audit_list) + 1]] <- data.frame(
      file_path = file_value,
      dataset = dataset_value,
      readable = TRUE,
      scorable = nrow(state_scores) > 0,
      n_genes = nrow(mat_value),
      n_cells = ncol(mat_value),
      state_column = state_col,
      n_state_rows = nrow(state_scores),
      note = ifelse(nrow(state_scores) > 0, "scored", "no_state_rows"),
      stringsAsFactors = FALSE
    )

    rm(obj_value, mat_value, meta_value, score_df, state_scores)
    gc(verbose = FALSE)
  }
}

object_audit_df <- safe_bind_rows(object_audit_list)
state_score_df <- safe_bind_rows(state_score_list)

write_csv_safe(object_audit_df, file.path(out_table_dir, "11I_FINAL_object_scan_and_scoring_audit.csv"))

if (nrow(state_score_df) < 3) {
  cat("[11I FINAL] WARNING: insufficient state-level rows from RDS scan. Attempting fallback from upstream module-score tables...\n")

  fallback_files <- c(
    find_latest_file(c("11f", "projection_evidence_tier_table_for_11h_dedup"), max_n = 5),
    find_latest_file(c("11c", "target"), max_n = 5),
    find_latest_file(c("11e", "state"), max_n = 5)
  )
  fallback_list <- list()
  for (file_value in fallback_files) {
    tmp <- read_table_safe(file_value)
    if (nrow(tmp) < 1) next
    numeric_cols <- colnames(tmp)[vapply(tmp, function(col_value) {
      vals <- safe_num(col_value)
      sum(is.finite(vals)) >= max(3, floor(0.25 * length(vals)))
    }, logical(1))]
    label_values <- label_from_possible_cols(tmp, c("compact", "state", "label", "cluster"), "fallback_state")
    if (length(numeric_cols) > 1) {
      out_tmp <- data.frame(
        dataset = dataset_from_label(label_values),
        sample_id = basename(file_value),
        object_file = file_value,
        state_column = "fallback_table",
        state_label = label_values,
        compact_state_label = label_values,
        n_cells = 1,
        stringsAsFactors = FALSE
      )
      for (module_name in names(module_gene_list)) out_tmp[[module_name]] <- NA_real_
      for (col_value in numeric_cols) {
        target_name <- names(module_gene_list)[tolower(names(module_gene_list)) %in% tolower(col_value)]
        if (length(target_name) > 0) out_tmp[[target_name[1]]] <- safe_num(tmp[[col_value]])
      }
      fallback_list[[length(fallback_list) + 1]] <- out_tmp
    }
  }
  fallback_df <- safe_bind_rows(fallback_list)
  if (nrow(fallback_df) > 0) state_score_df <- fallback_df
}

if (nrow(state_score_df) < 3) {
  stop("[11I FINAL] Insufficient state-score rows for correlation. Need at least 3 rows.", call. = FALSE)
}

dedup_key <- paste(state_score_df$dataset, state_score_df$sample_id, state_score_df$state_label, sep = "||")
state_score_df <- state_score_df[!duplicated(dedup_key), , drop = FALSE]

module_names <- names(module_gene_list)
available_module_names <- module_names[module_names %in% colnames(state_score_df)]
module_non_na <- rep(0, nrow(state_score_df))
for (idx_row in seq_len(nrow(state_score_df))) {
  vals <- safe_num(state_score_df[idx_row, available_module_names, drop = TRUE])
  module_non_na[idx_row] <- sum(is.finite(vals))
}
state_score_df <- state_score_df[module_non_na >= 2, , drop = FALSE]

write_csv_safe(state_score_df, file.path(out_table_dir, "11I_FINAL_state_level_module_score_table.csv"))
write_tsv_safe(state_score_df, file.path(out_table_dir, "11I_FINAL_state_level_module_score_table.tsv"))

score_mat <- as.matrix(state_score_df[, available_module_names, drop = FALSE])
storage.mode(score_mat) <- "numeric"

keep_modules <- rep(FALSE, ncol(score_mat))
for (idx_col in seq_len(ncol(score_mat))) {
  vals <- score_mat[, idx_col]
  finite_vals <- vals[is.finite(vals)]
  keep_modules[idx_col] <- length(finite_vals) >= 5 && length(unique(round(finite_vals, 8))) >= 2
}
score_mat <- score_mat[, keep_modules, drop = FALSE]
available_module_names <- colnames(score_mat)

if (ncol(score_mat) < 2) {
  stop("[11I FINAL] Fewer than two variable modules available for correlation.", call. = FALSE)
}

cor_mat <- stats::cor(score_mat, use = "pairwise.complete.obs", method = "spearman")
cor_mat[!is.finite(cor_mat)] <- 0
diag(cor_mat) <- 1

write_csv_safe(
  data.frame(module_name = rownames(cor_mat), cor_mat, check.names = FALSE, stringsAsFactors = FALSE),
  file.path(out_table_dir, "11I_FINAL_spearman_module_correlation_matrix.csv")
)

pair_list <- list()
for (idx_i in seq_len(ncol(score_mat))) {
  for (idx_j in seq_len(ncol(score_mat))) {
    if (idx_j <= idx_i) next
    v1 <- score_mat[, idx_i]
    v2 <- score_mat[, idx_j]
    ok <- is.finite(v1) & is.finite(v2)
    n_pair <- sum(ok)
    rho <- NA_real_
    p_val <- NA_real_
    if (n_pair >= 5) {
      test_value <- tryCatch(stats::cor.test(v1[ok], v2[ok], method = "spearman", exact = FALSE), error = function(err_obj) NULL)
      if (!is.null(test_value)) {
        rho <- safe_num(test_value$estimate[1])
        p_val <- safe_num(test_value$p.value)
      } else {
        rho <- suppressWarnings(stats::cor(v1[ok], v2[ok], method = "spearman"))
        p_val <- NA_real_
      }
    }
    pair_list[[length(pair_list) + 1]] <- data.frame(
      module_1 = colnames(score_mat)[idx_i],
      module_2 = colnames(score_mat)[idx_j],
      spearman_rho = rho,
      n_pairwise_states = n_pair,
      nominal_p = p_val,
      abs_rho = abs(rho),
      direction = ifelse(is.finite(rho) & rho >= 0, "positive", "negative"),
      stringsAsFactors = FALSE
    )
  }
}
pair_df <- safe_bind_rows(pair_list)
pair_df$BH_adjusted_p <- stats::p.adjust(safe_num(pair_df$nominal_p), method = "BH")
pair_df$correlation_strength <- "weak_or_unresolved"
pair_df$correlation_strength[is.finite(pair_df$abs_rho) & pair_df$abs_rho >= 0.30] <- "moderate"
pair_df$correlation_strength[is.finite(pair_df$abs_rho) & pair_df$abs_rho >= 0.50] <- "strong"
pair_df$correlation_strength[is.finite(pair_df$abs_rho) & pair_df$abs_rho >= 0.70] <- "very_strong"
pair_df <- pair_df[order(pair_df$abs_rho, decreasing = TRUE), , drop = FALSE]

write_csv_safe(pair_df, file.path(out_table_dir, "11I_FINAL_pairwise_module_correlation_table.csv"))
write_tsv_safe(pair_df, file.path(out_table_dir, "11I_FINAL_pairwise_module_correlation_table.tsv"))

favorable_modules <- available_module_names[!grepl("risk|stress|inflammatory|off_target|cell_cycle", available_module_names, ignore.case = TRUE)]
risk_modules <- available_module_names[grepl("risk|stress|inflammatory|off_target|cell_cycle", available_module_names, ignore.case = TRUE)]

state_axis_df <- state_score_df[, c("dataset", "sample_id", "state_label", "compact_state_label", "n_cells"), drop = FALSE]
if (length(favorable_modules) > 0) {
  state_axis_df$favorable_identity_axis_score <- rowMeans(score_mat[, favorable_modules, drop = FALSE], na.rm = TRUE)
} else {
  state_axis_df$favorable_identity_axis_score <- NA_real_
}
if (length(risk_modules) > 0) {
  state_axis_df$risk_safety_axis_score <- rowMeans(score_mat[, risk_modules, drop = FALSE], na.rm = TRUE)
} else {
  state_axis_df$risk_safety_axis_score <- NA_real_
}
state_axis_df$priority_balance_axis_score <- state_axis_df$favorable_identity_axis_score - state_axis_df$risk_safety_axis_score

axis_cor <- NA_real_
axis_p <- NA_real_
ok_axis <- is.finite(state_axis_df$favorable_identity_axis_score) & is.finite(state_axis_df$risk_safety_axis_score)
if (sum(ok_axis) >= 5) {
  test_axis <- tryCatch(
    stats::cor.test(state_axis_df$favorable_identity_axis_score[ok_axis], state_axis_df$risk_safety_axis_score[ok_axis], method = "spearman", exact = FALSE),
    error = function(err_obj) NULL
  )
  if (!is.null(test_axis)) {
    axis_cor <- safe_num(test_axis$estimate[1])
    axis_p <- safe_num(test_axis$p.value)
  }
}
write_csv_safe(state_axis_df, file.path(out_table_dir, "11I_FINAL_state_level_identity_risk_axis_scores.csv"))

axis_summary_df <- data.frame(
  comparison = "favorable_identity_axis_vs_risk_safety_axis",
  spearman_rho = axis_cor,
  nominal_p = axis_p,
  n_pairwise_states = sum(ok_axis),
  interpretation = "transcriptomic identity-risk module relationship only; not clinical safety prediction",
  stringsAsFactors = FALSE
)
write_csv_safe(axis_summary_df, file.path(out_table_dir, "11I_FINAL_identity_risk_axis_correlation_summary.csv"))

marker_module_list <- list()
if (nrow(marker_11h_df) > 0) {
  gene_col <- colnames(marker_11h_df)[grepl("gene_symbol|gene$|symbol", tolower(colnames(marker_11h_df)))]
  if (length(gene_col) > 0) {
    marker_genes <- clean_gene_symbol(marker_11h_df[[gene_col[1]]])
    marker_genes <- unique(marker_genes[marker_genes != ""])
    for (module_name in names(module_gene_list)) {
      hits <- intersect(clean_gene_symbol(module_gene_list[[module_name]]), marker_genes)
      marker_module_list[[length(marker_module_list) + 1]] <- data.frame(
        module_name = module_name,
        n_module_genes = length(unique(clean_gene_symbol(module_gene_list[[module_name]]))),
        n_11H_candidate_marker_genes = length(hits),
        overlapping_11H_candidate_marker_genes = paste(sort(hits), collapse = ";"),
        stringsAsFactors = FALSE
      )
    }
  }
}
marker_module_df <- safe_bind_rows(marker_module_list)
if (nrow(marker_module_df) < 1) {
  marker_module_df <- data.frame(
    module_name = names(module_gene_list),
    n_module_genes = as.integer(vapply(module_gene_list, length, integer(1))),
    n_11H_candidate_marker_genes = 0,
    overlapping_11H_candidate_marker_genes = "",
    stringsAsFactors = FALSE
  )
}
write_csv_safe(marker_module_df, file.path(out_table_dir, "11I_FINAL_module_overlap_with_11H_candidate_marker_signatures.csv"))

fig_a <- open_pdf_safe("11I_FINAL_FigA_object_scan_and_claim_boundary.pdf", 11.2, 6.4)
new_canvas()
draw_title("11I module-score correlation input audit", "Complete RDS rescan; module-score correlation only; conservative claim boundary retained.")

audit_items <- data.frame(
  label = c(
    "Candidate RDS",
    "Attempted",
    "Readable",
    "Scorable",
    "State rows",
    "Variable modules"
  ),
  value = c(
    length(rds_files),
    attempted_count,
    readable_count,
    scorable_count,
    nrow(state_score_df),
    ncol(score_mat)
  ),
  stringsAsFactors = FALSE
)
max_val <- max(safe_num(audit_items$value), na.rm = TRUE)
if (!is.finite(max_val) || max_val < 1) max_val <- 1
y_pos <- seq(0.78, 0.44, length.out = nrow(audit_items))
bar_x0 <- 0.30
bar_x1 <- 0.72
for (idx_value in seq_len(nrow(audit_items))) {
  yy <- y_pos[idx_value]
  score_value_vec <- safe_num(audit_items$value[idx_value])
  width_val <- score_value_vec / max_val
  text(bar_x0 - 0.018, yy, audit_items$label[idx_value], cex = 0.62, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.024, bar_x0 + width_val * (bar_x1 - bar_x0), yy + 0.024,
       col = nature_continuous_color(score_value_vec, max_val, nature_palette$pale_green, nature_palette$teal), border = nature_palette$border, lwd = 0.5)
  text(bar_x0 + width_val * (bar_x1 - bar_x0) + 0.012, yy, as.character(score_value_vec), cex = 0.56, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.08, 0.24, "Allowed", cex = 0.68, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
text(0.18, 0.24, "Module-score correlation; identity-risk axis structure; candidate marker signature context.", cex = 0.50, adj = c(0, 0.5), col = nature_palette$muted)
text(0.08, 0.17, "Prohibited", cex = 0.68, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
text(0.18, 0.17, "Clinical prediction, validated biomarker, causal projection/lineage/graft-safety claim.", cex = 0.50, adj = c(0, 0.5), col = nature_palette$muted)
dev.off()
cat("[11I FINAL] Wrote figure:", fig_a, "\n")

module_order <- available_module_names
if (length(available_module_names) >= 3) {
  dist_value <- as.dist(1 - abs(cor_mat))
  hc <- tryCatch(stats::hclust(dist_value, method = "average"), error = function(err_obj) NULL)
  if (!is.null(hc)) module_order <- available_module_names[hc$order]
}
cor_ordered <- cor_mat[module_order, module_order, drop = FALSE]

fig_b <- open_pdf_safe("11I_FINAL_FigB_module_correlation_heatmap.pdf", 9.8, 8.6)
new_canvas()
draw_title("Module-score correlation landscape", "Spearman correlation across state-level module scores; Nature-style blue-white-red scale.")
nr <- nrow(cor_ordered)
nc <- ncol(cor_ordered)
hm_x0 <- 0.24
hm_x1 <- 0.82
hm_y0 <- 0.18
hm_y1 <- 0.80
cell_w <- (hm_x1 - hm_x0) / nc
cell_h <- (hm_y1 - hm_y0) / nr
for (row_idx in seq_len(nr)) {
  for (col_idx in seq_len(nc)) {
    score_value_vec <- cor_ordered[row_idx, col_idx]
    rect(
      hm_x0 + (col_idx - 1) * cell_w,
      hm_y1 - row_idx * cell_h,
      hm_x0 + col_idx * cell_w,
      hm_y1 - (row_idx - 1) * cell_h,
      col = corr_to_gray(score_value_vec),
      border = "white",
      lwd = 0.35
    )
    if (abs(score_value_vec) >= 0.5 || row_idx == col_idx) {
      text(
        hm_x0 + (col_idx - 0.5) * cell_w,
        hm_y1 - (row_idx - 0.5) * cell_h,
        sprintf("%.2f", score_value_vec),
        cex = 0.34,
        col = ifelse(score_value_vec > 0.45, "white", "gray20")
      )
    }
  }
}
rect(hm_x0, hm_y0, hm_x1, hm_y1, border = nature_palette$border, lwd = 0.7)
for (row_idx in seq_len(nr)) {
  yy <- hm_y1 - (row_idx - 0.5) * cell_h
  text(hm_x0 - 0.012, yy, rownames(cor_ordered)[row_idx], cex = 0.40, adj = c(1, 0.5), col = nature_palette$ink)
}
for (col_idx in seq_len(nc)) {
  xx <- hm_x0 + (col_idx - 0.5) * cell_w
  text(xx, 0.105, colnames(cor_ordered)[col_idx], cex = 0.40, srt = 90, adj = c(0.5, 0.5), col = nature_palette$ink)
}

legend_x0 <- 0.87
legend_x1 <- 0.895
legend_y0 <- 0.25
legend_y1 <- 0.75
n_legend <- 60
for (idx_legend in seq_len(n_legend)) {
  yy0 <- legend_y0 + (idx_legend - 1) / n_legend * (legend_y1 - legend_y0)
  yy1 <- legend_y0 + idx_legend / n_legend * (legend_y1 - legend_y0)
  score_value_vec <- -1 + 2 * (idx_legend - 0.5) / n_legend
  rect(legend_x0, yy0, legend_x1, yy1, col = corr_to_gray(score_value_vec), border = NA)
}
rect(legend_x0, legend_y0, legend_x1, legend_y1, border = nature_palette$border, lwd = 0.5)
text(legend_x1 + 0.015, legend_y1, "+1", cex = 0.42, adj = c(0, 0.5), col = nature_palette$ink)
text(legend_x1 + 0.015, (legend_y0 + legend_y1) / 2, "0", cex = 0.42, adj = c(0, 0.5), col = nature_palette$ink)
text(legend_x1 + 0.015, legend_y0, "-1", cex = 0.42, adj = c(0, 0.5), col = nature_palette$ink)
text(0.925, 0.50, "Spearman rho", cex = 0.42, srt = 90, col = nature_palette$muted)
dev.off()
cat("[11I FINAL] Wrote figure:", fig_b, "\n")

fig_c <- open_pdf_safe("11I_FINAL_FigC_identity_risk_axis_landscape.pdf", 10.8, 6.8)
new_canvas()
draw_title("Identity-risk module axis landscape", "State-level favorable identity versus risk/safety-context module scores; color indicates priority balance.")
risk_axis_values <- state_axis_df$risk_safety_axis_score
identity_axis_values <- state_axis_df$favorable_identity_axis_score
ok <- is.finite(risk_axis_values) & is.finite(identity_axis_values)
if (sum(ok) >= 3) {
  risk_axis_ok <- risk_axis_values[ok]
  identity_axis_ok <- identity_axis_values[ok]
  x_min <- min(risk_axis_ok, na.rm = TRUE)
  x_max <- max(risk_axis_ok, na.rm = TRUE)
  y_min <- min(identity_axis_ok, na.rm = TRUE)
  y_max <- max(identity_axis_ok, na.rm = TRUE)
  if (abs(x_max - x_min) < 1e-12) { x_min <- x_min - 0.5; x_max <- x_max + 0.5 }
  if (abs(y_max - y_min) < 1e-12) { y_min <- y_min - 0.5; y_max <- y_max + 0.5 }
  pad_x <- 0.05 * (x_max - x_min)
  pad_y <- 0.05 * (y_max - y_min)
  xlim <- c(x_min - pad_x, x_max + pad_x)
  ylim <- c(y_min - pad_y, y_max + pad_y)
  plot_x0 <- 0.16
  plot_x1 <- 0.76
  plot_y0 <- 0.16
  plot_y1 <- 0.82

  map_x <- function(vals) plot_x0 + (vals - xlim[1]) / (xlim[2] - xlim[1]) * (plot_x1 - plot_x0)
  map_y <- function(vals) plot_y0 + (vals - ylim[1]) / (ylim[2] - ylim[1]) * (plot_y1 - plot_y0)

  rect(plot_x0, plot_y0, plot_x1, plot_y1, border = nature_palette$border, col = NA, lwd = 0.6)
  for (tick_frac in seq(0, 1, by = 0.25)) {
    xx <- plot_x0 + tick_frac * (plot_x1 - plot_x0)
    yy <- plot_y0 + tick_frac * (plot_y1 - plot_y0)
    segments(xx, plot_y0, xx, plot_y1, col = nature_palette$grid, lwd = 0.5)
    segments(plot_x0, yy, plot_x1, yy, col = nature_palette$grid, lwd = 0.5)
  }

  balance <- state_axis_df$priority_balance_axis_score[ok]
  point_sizes <- sqrt(pmax(safe_num(state_axis_df$n_cells[ok]), 1))
  point_sizes <- 0.45 + 1.2 * point_sizes / max(point_sizes, na.rm = TRUE)
  points(map_x(risk_axis_ok), map_y(identity_axis_ok), pch = 21, bg = value_to_gray(balance - min(balance, na.rm = TRUE), max(balance - min(balance, na.rm = TRUE), na.rm = TRUE)), col = nature_palette$muted, cex = point_sizes, lwd = 0.35)

  label_order <- order(balance, decreasing = TRUE)
  top_n <- min(8, length(label_order))
  label_idx <- label_order[seq_len(top_n)]
  rail_x <- 0.80
  rail_y <- seq(0.78, 0.38, length.out = top_n)
  for (idx_label in seq_len(top_n)) {
    i <- label_idx[idx_label]
    px <- map_x(risk_axis_ok[i])
    py <- map_y(identity_axis_ok[i])
    segments(px, py, rail_x - 0.01, rail_y[idx_label], col = nature_palette$blue, lwd = 0.55)
    text(rail_x, rail_y[idx_label], state_axis_df$compact_state_label[which(ok)[i]], cex = 0.38, adj = c(0, 0.5), col = nature_palette$ink)
  }

  axis(1, at = seq(plot_x0, plot_x1, length.out = 5), labels = sprintf("%.2f", seq(xlim[1], xlim[2], length.out = 5)), pos = plot_y0, cex.axis = 0.45, lwd = 0.4, tck = -0.01)
  axis(2, at = seq(plot_y0, plot_y1, length.out = 5), labels = sprintf("%.2f", seq(ylim[1], ylim[2], length.out = 5)), pos = plot_x0, cex.axis = 0.45, lwd = 0.4, tck = -0.01)
  text((plot_x0 + plot_x1) / 2, 0.07, "Risk / safety-context axis score", cex = 0.60, col = nature_palette$ink)
  text(0.055, (plot_y0 + plot_y1) / 2, "Favorable identity axis score", cex = 0.60, srt = 90, col = nature_palette$ink)
  text(0.80, 0.85, paste0("Spearman rho = ", sprintf("%.3f", axis_cor)), cex = 0.52, adj = c(0, 0.5), col = nature_palette$ink)
} else {
  text(0.5, 0.5, "Insufficient states for identity-risk landscape.", cex = 0.8)
}
dev.off()
cat("[11I FINAL] Wrote figure:", fig_c, "\n")

fig_d <- open_pdf_safe("11I_FINAL_FigD_top_module_correlation_pairs.pdf", 10.8, 6.4)
new_canvas()
draw_title("Top module-score correlation pairs", "Warm bars indicate positive correlations; cool bars indicate negative correlations.")
plot_pairs <- pair_df[is.finite(pair_df$abs_rho), , drop = FALSE]
plot_pairs <- plot_pairs[order(plot_pairs$abs_rho, decreasing = TRUE), , drop = FALSE]
plot_pairs <- plot_pairs[seq_len(min(16, nrow(plot_pairs))), , drop = FALSE]
if (nrow(plot_pairs) > 0) {
  y_pos <- seq(0.80, 0.22, length.out = nrow(plot_pairs))
  max_abs <- max(plot_pairs$abs_rho, na.rm = TRUE)
  if (!is.finite(max_abs) || max_abs < 1e-6) max_abs <- 1
  bar_x0 <- 0.42
  bar_x1 <- 0.82
  for (idx_value in seq_len(nrow(plot_pairs))) {
    yy <- y_pos[idx_value]
    score_value_vec <- plot_pairs$abs_rho[idx_value]
    width_val <- score_value_vec / max_abs
    label_value <- paste(plot_pairs$module_1[idx_value], "vs", plot_pairs$module_2[idx_value])
    text(bar_x0 - 0.018, yy, label_value, cex = 0.44, adj = c(1, 0.5), col = nature_palette$ink)
    rect(bar_x0, yy - 0.018, bar_x0 + width_val * (bar_x1 - bar_x0), yy + 0.018,
         col = pair_color_from_rho(plot_pairs$spearman_rho[idx_value]), border = nature_palette$border, lwd = 0.5)
    rho_label <- paste0(ifelse(plot_pairs$spearman_rho[idx_value] >= 0, "+", ""), sprintf("%.2f", plot_pairs$spearman_rho[idx_value]))
    text(bar_x0 + width_val * (bar_x1 - bar_x0) + 0.012, yy, rho_label, cex = 0.46, adj = c(0, 0.5), col = nature_palette$ink)
  }
  text(0.5, 0.10, "Absolute Spearman rho; sign shown at bar end", cex = 0.55, col = nature_palette$muted)
} else {
  text(0.5, 0.5, "No pairwise correlation rows available.", cex = 0.8)
}
dev.off()
cat("[11I FINAL] Wrote figure:", fig_d, "\n")

claim_boundary_df <- data.frame(
  category = c(
    "allowed",
    "allowed",
    "allowed",
    "allowed",
    "prohibited",
    "prohibited",
    "prohibited",
    "prohibited",
    "prohibited"
  ),
  statement = c(
    "Module-score correlation across transcriptomic state-level module scores",
    "Favorable identity versus risk/safety-context axis relationship",
    "Candidate transcriptomic marker signature context",
    "Supportive evidence for downstream 11J ML audit and 12H/12I manuscript integration",
    "Clinical prediction model",
    "Diagnostic/prognostic/therapeutic-response biomarker validation",
    "Causal proof of graft efficacy or safety",
    "True anatomical projection or lineage tracing validation",
    "Functional integration proof"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(claim_boundary_df, file.path(out_table_dir, "11I_FINAL_claim_boundary.csv"))

summary_df <- data.frame(
  item = c(
    "candidate_RDS_detected",
    "RDS_attempted",
    "readable_objects",
    "scorable_objects",
    "state_level_module_score_rows",
    "variable_modules_for_correlation",
    "pairwise_module_correlations",
    "strong_or_very_strong_pairs_abs_rho_ge_0.50",
    "identity_risk_axis_spearman_rho",
    "identity_risk_axis_pairwise_states",
    "candidate_marker_signature_module_rows",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(length(rds_files)),
    as.character(attempted_count),
    as.character(readable_count),
    as.character(scorable_count),
    as.character(nrow(state_score_df)),
    as.character(ncol(score_mat)),
    as.character(nrow(pair_df)),
    as.character(sum(pair_df$abs_rho >= 0.50, na.rm = TRUE)),
    as.character(round(axis_cor, 6)),
    as.character(sum(ok_axis)),
    as.character(nrow(marker_module_df)),
    "4",
    "INPUT_READY_FOR_11J_ML_AUDIT_AND_FEATURE_IMPORTANCE_REVIEW"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "11I_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "11I_FINAL_execution_summary.tsv"))

report_lines <- c(
  "11I FINAL report",
  "================",
  "Module: module-score correlation",
  "Mode: complete standalone 11I rebuild; no previous 11I output dependency; no internet; no 00-10P rerun.",
  "",
  paste0("Candidate RDS detected: ", length(rds_files)),
  paste0("RDS attempted: ", attempted_count),
  paste0("Readable objects: ", readable_count),
  paste0("Scorable objects: ", scorable_count),
  paste0("State-level module-score rows: ", nrow(state_score_df)),
  paste0("Variable modules for correlation: ", ncol(score_mat)),
  paste0("Pairwise module correlations: ", nrow(pair_df)),
  paste0("Strong or very strong pairs abs(rho) >= 0.50: ", sum(pair_df$abs_rho >= 0.50, na.rm = TRUE)),
  paste0("Identity-risk axis Spearman rho: ", round(axis_cor, 6)),
  paste0("Identity-risk axis pairwise states: ", sum(ok_axis)),
  "",
  "Main outputs:",
  paste0("- ", file.path(out_table_dir, "11I_FINAL_state_level_module_score_table.csv")),
  paste0("- ", file.path(out_table_dir, "11I_FINAL_spearman_module_correlation_matrix.csv")),
  paste0("- ", file.path(out_table_dir, "11I_FINAL_pairwise_module_correlation_table.csv")),
  paste0("- ", file.path(out_table_dir, "11I_FINAL_state_level_identity_risk_axis_scores.csv")),
  paste0("- ", file.path(out_table_dir, "11I_FINAL_module_overlap_with_11H_candidate_marker_signatures.csv")),
  "",
  "Allowed interpretation:",
  "- Transcriptomic module-score correlation structure.",
  "- Favorable identity versus risk/safety-context module relationship.",
  "- Candidate marker signature context.",
  "",
  "Prohibited interpretation:",
  "- No clinical prediction.",
  "- No validated biomarker claim.",
  "- No causal graft efficacy/safety claim.",
  "- No true projection/lineage/functional integration proof.",
  "",
  "Decision: INPUT_READY_FOR_11J_ML_AUDIT_AND_FEATURE_IMPORTANCE_REVIEW"
)
report_file <- file.path(out_text_dir, "11I_FINAL_module_score_correlation_report.txt")
writeLines(report_lines, report_file)
cat("[11I FINAL] Wrote:", report_file, "\n")

cat("\n[11I FINAL] Completed module-score correlation.\n")
cat("[11I FINAL] Candidate RDS detected:", length(rds_files), "\n")
cat("[11I FINAL] RDS attempted:", attempted_count, "\n")
cat("[11I FINAL] Readable objects:", readable_count, "\n")
cat("[11I FINAL] Scorable objects:", scorable_count, "\n")
cat("[11I FINAL] State-level module-score rows:", nrow(state_score_df), "\n")
cat("[11I FINAL] Variable modules for correlation:", ncol(score_mat), "\n")
cat("[11I FINAL] Pairwise module correlations:", nrow(pair_df), "\n")
cat("[11I FINAL] Strong/very strong pairs abs(rho)>=0.50:", sum(pair_df$abs_rho >= 0.50, na.rm = TRUE), "\n")
cat("[11I FINAL] Identity-risk axis Spearman rho:", round(axis_cor, 6), "\n")
cat("[11I FINAL] Figures written: 4\n")
cat("[11I FINAL] Decision: INPUT_READY_FOR_11J_ML_AUDIT_AND_FEATURE_IMPORTANCE_REVIEW\n")
cat("[11I FINAL] Output tables:", out_table_dir, "\n")
cat("[11I FINAL] Output figs  :", out_fig_dir, "\n")
cat("[11I FINAL] Output text  :", out_text_dir, "\n")
cat("[11I FINAL] Next         : review 11I FINAL PDFs; if accepted, proceed to 11J ML audit / feature importance.\n")
