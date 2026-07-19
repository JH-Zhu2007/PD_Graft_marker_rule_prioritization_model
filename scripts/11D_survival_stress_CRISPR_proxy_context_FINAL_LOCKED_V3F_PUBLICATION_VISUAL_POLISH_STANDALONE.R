
cat("\n[11D FINAL V3F] Starting publication visual polish from locked validated 11D tables...\n")
cat("[11D FINAL V3F] Mode: table-to-figure only; no raw re-analysis; no Excel reparse; no 00-10P rerun.\n")
cat("[11D FINAL V3F] Claim boundary: CRISPR = manual-column proxy overlap support only; not validated survival-hit genes.\n")

project_root <- "D:/PD_Graft_Project"

v3e_tbl_dir <- file.path(project_root, "03_tables", "11D_survival_perturbation_CRISPR_validation_FINAL_LOCKED_V3E_REGENERATE_FIGURES_FROM_VALIDATED_TABLES")
v1_tbl_dir  <- file.path(project_root, "03_tables", "11D_survival_perturbation_CRISPR_validation_V1")
v8_tbl_dir  <- file.path(project_root, "03_tables", "11D_survival_perturbation_CRISPR_validation_V8_EXCLUDE_SAFEHARBOR_REPAIR_TABLES")

out_tag <- "11D_survival_perturbation_CRISPR_validation_FINAL_LOCKED_V3F_PUBLICATION_VISUAL_POLISH"
out_tbl_dir <- file.path(project_root, "03_tables", out_tag)
out_fig_dir <- file.path(project_root, "04_figures", paste0(out_tag, "_pdf"))
out_txt_dir <- file.path(project_root, "09_manuscript", out_tag)

dir.create(out_tbl_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_txt_dir, recursive = TRUE, showWarnings = FALSE)

cat("[11D FINAL V3F] V3E tables  :", v3e_tbl_dir, "\n")
cat("[11D FINAL V3F] V1 tables   :", v1_tbl_dir, "\n")
cat("[11D FINAL V3F] V8 tables   :", v8_tbl_dir, "\n")
cat("[11D FINAL V3F] Output figs :", out_fig_dir, "\n")

clean_text <- function(txt_value) {
  val <- as.character(txt_value)
  val[is.na(val)] <- ""
  val <- gsub("\u2212", "-", val, fixed = TRUE)
  val <- gsub("\u2013", "-", val, fixed = TRUE)
  val <- gsub("\u2014", "-", val, fixed = TRUE)
  val <- gsub('"', "", val, fixed = TRUE)
  val <- gsub("\r", " ", val, fixed = TRUE)
  val <- gsub("\n", " ", val, fixed = TRUE)
  val <- gsub("\t", " ", val, fixed = TRUE)
  val <- gsub("  +", " ", val)
  val <- gsub("^ +", "", val)
  val <- gsub(" +$", "", val)
  val
}

safe_num <- function(num_value) {
  suppressWarnings(as.numeric(num_value))
}

fmt_num <- function(num_value, digits_value = 3) {
  vals <- suppressWarnings(as.numeric(num_value))
  out <- rep("", length(vals))
  ok <- is.finite(vals)
  out[ok] <- formatC(vals[ok], format = "f", digits = digits_value)
  out
}

safe_write_tsv <- function(df_value, path_value) {
  if (is.null(df_value)) {
    writeLines("empty", path_value, useBytes = TRUE)
    return(invisible(FALSE))
  }
  df2 <- as.data.frame(df_value, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(df2) == 0) {
    writeLines("empty", path_value, useBytes = TRUE)
    return(invisible(FALSE))
  }
  cn <- clean_text(colnames(df2))
  cn[cn == ""] <- paste0("col", seq_len(length(cn)))[cn == ""]
  colnames(df2) <- cn
  lines <- character(nrow(df2) + 1)
  lines[1] <- paste(cn, collapse = "\t")
  if (nrow(df2) > 0) {
    for (rr in seq_len(nrow(df2))) {
      vals <- clean_text(as.character(df2[rr, , drop = TRUE]))
      lines[rr + 1] <- paste(vals, collapse = "\t")
    }
  }
  writeLines(lines, path_value, useBytes = TRUE)
  invisible(TRUE)
}

read_table_auto <- function(path_value) {
  if (!file.exists(path_value)) {
    return(NULL)
  }
  ext <- tolower(tools::file_ext(path_value))
  if (ext == "csv") {
    df <- tryCatch(
      utils::read.csv(path_value, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (!is.null(df)) {
      return(df)
    }
  }
  df <- tryCatch(
    utils::read.delim(path_value, stringsAsFactors = FALSE, check.names = FALSE, quote = "", sep = "\t"),
    error = function(e) NULL
  )
  df
}

find_first_existing <- function(path_vector) {
  for (pp in path_vector) {
    if (file.exists(pp)) return(pp)
  }
  return(NA_character_)
}

copy_table_to_output <- function(src_path, dst_name) {
  if (!is.na(src_path) && file.exists(src_path)) {
    dst <- file.path(out_tbl_dir, dst_name)
    ok <- tryCatch(file.copy(src_path, dst, overwrite = TRUE), error = function(e) FALSE)
    if (isTRUE(ok)) return(dst)
  }
  return(NA_character_)
}

bulk_path <- find_first_existing(c(
  file.path(v3e_tbl_dir, "11D_FINAL_V3E_GSE216363_bulk_sample_module_scores_REUSED_FROM_V1.csv"),
  file.path(v1_tbl_dir, "11D_V1_GSE216363_bulk_sample_module_scores.csv")
))

sc_path <- find_first_existing(c(
  file.path(v3e_tbl_dir, "11D_FINAL_V3E_GSE216364_scRNA_sample_module_scores_REUSED_FROM_V1.csv"),
  file.path(v1_tbl_dir, "11D_V1_GSE216364_scRNA_sample_module_scores.csv")
))

status_path <- find_first_existing(c(
  file.path(v3e_tbl_dir, "11D_FINAL_V3E_survival_perturbation_evidence_status_REUSED_FROM_V8.tsv"),
  file.path(v8_tbl_dir, "11D_V8_survival_perturbation_evidence_status_SOURCE.tsv")
))

cand_path <- find_first_existing(c(
  file.path(v3e_tbl_dir, "11D_FINAL_V3E_biological_CRISPR_proxy_candidate_genes_SAFEHARBOR_EXCLUDED_REUSED_FROM_V8.tsv"),
  file.path(v8_tbl_dir, "11D_V8_CRISPR_manual_column_biological_candidate_proxy_genes_SAFEHARBOR_EXCLUDED.tsv")
))

ctrl_path <- find_first_existing(c(
  file.path(v3e_tbl_dir, "11D_FINAL_V3E_control_loci_SAFEHARBOR_EXCLUDED_FROM_CANDIDATES_REUSED_FROM_V8.tsv"),
  file.path(v8_tbl_dir, "11D_V8_CRISPR_manual_column_control_loci_SAFEHARBOR_EXCLUDED_FROM_CANDIDATES.tsv")
))

overlap_path <- find_first_existing(c(
  file.path(v3e_tbl_dir, "11D_FINAL_V3E_biological_CRISPR_candidate_overlap_with_survival_modules_REUSED_FROM_V8.tsv"),
  file.path(v8_tbl_dir, "11D_V8_CRISPR_manual_column_overlap_BIOLOGICAL_CANDIDATES_ONLY.tsv")
))

audit_path <- find_first_existing(c(
  file.path(v3e_tbl_dir, "11D_FINAL_V3E_manual_metric_source_audit_REUSED_FROM_V8.tsv"),
  file.path(v8_tbl_dir, "11D_V8_manual_metric_source_audit_DISPLAY_CLEANED.tsv")
))

component_presence <- data.frame(
  component = c("bulk_V1_table", "scRNA_V1_table", "status_V8_table", "biological_candidate_V8_table",
                "control_loci_V8_table", "overlap_V8_table", "metric_source_audit_V8_table"),
  path = c(bulk_path, sc_path, status_path, cand_path, ctrl_path, overlap_path, audit_path),
  present = c(!is.na(bulk_path), !is.na(sc_path), !is.na(status_path), !is.na(cand_path),
              !is.na(ctrl_path), !is.na(overlap_path), !is.na(audit_path)),
  stringsAsFactors = FALSE
)
safe_write_tsv(component_presence, file.path(out_tbl_dir, "11D_FINAL_V3F_input_component_presence.tsv"))

bulk_df <- read_table_auto(bulk_path)
sc_df <- read_table_auto(sc_path)
status_df <- read_table_auto(status_path)
cand_df <- read_table_auto(cand_path)
ctrl_df <- read_table_auto(ctrl_path)
overlap_df <- read_table_auto(overlap_path)
audit_df <- read_table_auto(audit_path)

copy_table_to_output(bulk_path, "11D_FINAL_V3F_GSE216363_bulk_sample_module_scores_REUSED_FROM_LOCKED_SOURCE.csv")
copy_table_to_output(sc_path, "11D_FINAL_V3F_GSE216364_scRNA_sample_module_scores_REUSED_FROM_LOCKED_SOURCE.csv")
copy_table_to_output(status_path, "11D_FINAL_V3F_survival_perturbation_evidence_status_REUSED_FROM_LOCKED_SOURCE.tsv")
copy_table_to_output(cand_path, "11D_FINAL_V3F_biological_CRISPR_proxy_candidate_genes_SAFEHARBOR_EXCLUDED_REUSED_FROM_LOCKED_SOURCE.tsv")
copy_table_to_output(ctrl_path, "11D_FINAL_V3F_control_loci_SAFEHARBOR_EXCLUDED_FROM_CANDIDATES_REUSED_FROM_LOCKED_SOURCE.tsv")
copy_table_to_output(overlap_path, "11D_FINAL_V3F_biological_CRISPR_candidate_overlap_with_survival_modules_REUSED_FROM_LOCKED_SOURCE.tsv")
copy_table_to_output(audit_path, "11D_FINAL_V3F_manual_metric_source_audit_REUSED_FROM_LOCKED_SOURCE.tsv")

first_col_by_names <- function(df_value, name_candidates) {
  if (is.null(df_value)) return(NA_character_)
  lower_names <- tolower(colnames(df_value))
  for (needle in name_candidates) {
    hit <- which(lower_names == tolower(needle))
    if (length(hit) > 0) return(colnames(df_value)[hit[1]])
  }
  for (needle in name_candidates) {
    hit <- grep(tolower(needle), lower_names, fixed = TRUE)
    if (length(hit) > 0) return(colnames(df_value)[hit[1]])
  }
  NA_character_
}

format_gene_list <- function(gene_value) {
  val <- clean_text(gene_value)
  val <- gsub(";", "; ", val, fixed = TRUE)
  val
}

module_label <- function(module_value) {
  val <- clean_text(module_value)
  map_keys <- c("priority_DA_projection", "risk_survival_stress_proxy", "p53_apoptosis",
                "TNF_NFkB_inflammatory", "ER_UPR_stress", "proliferation_cell_cycle",
                "ferroptosis_redox", "immune_innate_stress", "safety_risk_proxy",
                "mitochondrial_apoptosis_balance", "DA_identity", "A9_projection_maturation")
  map_vals <- c("Priority DA/projection", "Risk/survival stress", "p53 apoptosis",
                "TNF-NFkB inflammatory", "ER/UPR stress", "Proliferation/cell cycle",
                "Ferroptosis/redox", "Innate immune stress", "Safety-risk proxy",
                "Mitochondrial apoptosis balance", "DA identity", "A9/projection maturation")
  out <- val
  for (ii in seq_along(map_keys)) {
    out[out == map_keys[ii]] <- map_vals[ii]
  }
  out <- gsub("_", " ", out, fixed = TRUE)
  out
}

short_sample_label <- function(sample_value) {
  val <- clean_text(sample_value)
  val <- gsub("__.*$", "", val)
  val <- gsub("^X", "", val)
  val
}

make_matrix_from_scores <- function(df_value) {
  if (is.null(df_value) || nrow(df_value) == 0) return(NULL)
  df <- as.data.frame(df_value, stringsAsFactors = FALSE, check.names = FALSE)
  colnames(df) <- clean_text(colnames(df))

  numeric_flags <- rep(FALSE, ncol(df))
  for (jj in seq_len(ncol(df))) {
    vals <- safe_num(df[[jj]])
    numeric_flags[jj] <- sum(is.finite(vals)) >= max(2, floor(nrow(df) * 0.5))
  }
  lower_names <- tolower(colnames(df))
  sample_col <- NA_character_
  module_col <- NA_character_
  score_col <- NA_character_

  for (jj in seq_len(ncol(df))) {
    vals <- clean_text(df[[jj]])
    if (sum(grepl("GSM", vals, fixed = TRUE)) >= 2) {
      sample_col <- colnames(df)[jj]
      break
    }
  }
  if (is.na(sample_col)) {
    sample_col <- first_col_by_names(df, c("sample", "sample_id", "sample_name", "gsm", "group", "group_id"))
  }

  for (jj in seq_len(ncol(df))) {
    vals <- clean_text(df[[jj]])
    mod_hits <- sum(grepl("apoptosis", vals, ignore.case = TRUE) |
                    grepl("stress", vals, ignore.case = TRUE) |
                    grepl("projection", vals, ignore.case = TRUE) |
                    grepl("cell_cycle", vals, ignore.case = TRUE) |
                    grepl("identity", vals, ignore.case = TRUE) |
                    grepl("TNF", vals, ignore.case = TRUE))
    if (mod_hits >= 2) {
      module_col <- colnames(df)[jj]
      break
    }
  }
  if (is.na(module_col)) {
    module_col <- first_col_by_names(df, c("module", "module_name", "program", "signature", "gene_set"))
  }

  score_candidates <- c("module_score", "mean_module_score", "mean_score", "score", "value", "zscore", "z_score")
  for (needle in score_candidates) {
    hit <- grep(needle, lower_names, fixed = TRUE)
    if (length(hit) > 0) {
      for (hh in hit) {
        vals <- safe_num(df[[hh]])
        if (sum(is.finite(vals)) >= 2) {
          score_col <- colnames(df)[hh]
          break
        }
      }
    }
    if (!is.na(score_col)) break
  }
  if (is.na(score_col)) {
    numeric_cols <- which(numeric_flags)
    if (length(numeric_cols) > 0) score_col <- colnames(df)[numeric_cols[length(numeric_cols)]]
  }

  if (!is.na(sample_col) && !is.na(module_col) && !is.na(score_col)) {
    samples <- short_sample_label(df[[sample_col]])
    modules <- clean_text(df[[module_col]])
    scores <- safe_num(df[[score_col]])
    keep <- samples != "" & modules != "" & is.finite(scores)
    if (sum(keep) < 2) return(NULL)
    samples <- samples[keep]
    modules <- modules[keep]
    scores <- scores[keep]
    us <- unique(samples)
    um <- unique(modules)
    mat <- matrix(NA_real_, nrow = length(um), ncol = length(us))
    rownames(mat) <- um
    colnames(mat) <- us
    for (ii in seq_along(um)) {
      for (jj in seq_along(us)) {
        idx <- which(modules == um[ii] & samples == us[jj])
        if (length(idx) > 0) {
          vals <- scores[idx]
          mat[ii, jj] <- mean(vals[is.finite(vals)])
        }
      }
    }
    return(mat)
  }

  numeric_cols <- which(numeric_flags)
  if (length(numeric_cols) >= 2) {
    label_col <- setdiff(seq_len(ncol(df)), numeric_cols)
    row_labels <- NULL
    if (length(label_col) > 0) row_labels <- clean_text(df[[label_col[1]]])
    mat <- as.matrix(as.data.frame(lapply(df[numeric_cols], safe_num), check.names = FALSE))
    if (!is.null(row_labels) && length(row_labels) == nrow(mat)) rownames(mat) <- row_labels
    colnames(mat) <- short_sample_label(colnames(df)[numeric_cols])
    return(mat)
  }
  NULL
}

row_zscore_matrix <- function(mat_value) {
  if (is.null(mat_value)) return(NULL)
  mat <- mat_value
  zout <- matrix(NA_real_, nrow = nrow(mat), ncol = ncol(mat))
  rownames(zout) <- rownames(mat)
  colnames(zout) <- colnames(mat)
  for (ii in seq_len(nrow(mat))) {
    vals <- safe_num(mat[ii, ])
    ok <- is.finite(vals)
    if (sum(ok) < 2) {
      zout[ii, ok] <- 0
    } else {
      mu <- mean(vals[ok])
      sdv <- sqrt(sum((vals[ok] - mu)^2) / max(1, sum(ok) - 1))
      if (!is.finite(sdv) || sdv == 0) {
        zout[ii, ok] <- 0
      } else {
        zout[ii, ok] <- (vals[ok] - mu) / sdv
      }
    }
  }
  zout
}

pdf_start <- function(path_value, width_value = 12, height_value = 7) {
  grDevices::pdf(path_value, width = width_value, height = height_value, onefile = FALSE, useDingbats = FALSE)
}

draw_wrapped_text <- function(label_value, x_left, x_right, y_center, cex_value = 0.75, font_value = 1, align_value = "left") {
  val <- clean_text(label_value)
  width_frac <- max(0.05, x_right - x_left)
  wrap_n <- max(8, floor(width_frac * 95))
  lines <- strwrap(val, width = wrap_n)
  if (length(lines) == 0) lines <- ""
  line_step <- 0.024 * (cex_value / 0.75)
  start_y <- y_center + (length(lines) - 1) * line_step / 2
  if (align_value == "center") {
    x_pos <- (x_left + x_right) / 2
    adj_value <- c(0.5, 0.5)
  } else {
    x_pos <- x_left + 0.006
    adj_value <- c(0, 0.5)
  }
  for (ll in seq_along(lines)) {
    graphics::text(x_pos, start_y - (ll - 1) * line_step, labels = lines[ll],
                   adj = adj_value, cex = cex_value, font = font_value, xpd = NA)
  }
}

draw_publication_table <- function(data_value, title_value, subtitle_value, footnote_value, path_value,
                                   col_widths, font_cex = 0.72, header_cex = 0.70,
                                   width_value = 12.5, height_value = 7.0) {
  df <- as.data.frame(data_value, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(df) == 0) {
    df <- data.frame(note = "No rows available", stringsAsFactors = FALSE)
    col_widths <- 1
  }
  for (jj in seq_len(ncol(df))) df[[jj]] <- clean_text(df[[jj]])
  colnames(df) <- clean_text(colnames(df))
  col_widths <- as.numeric(col_widths)
  col_widths <- col_widths / sum(col_widths)

  pdf_start(path_value, width_value, height_value)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")

  graphics::text(0.02, 0.965, clean_text(title_value), adj = c(0, 0.5), cex = 1.35, font = 2)
  graphics::text(0.02, 0.925, clean_text(subtitle_value), adj = c(0, 0.5), cex = 0.78)

  left <- 0.02
  right <- 0.98
  top <- 0.865
  bottom <- 0.105
  n_rows <- nrow(df)
  header_h <- 0.095
  body_h <- (top - bottom - header_h) / max(1, n_rows)
  body_h <- min(body_h, 0.105)
  total_h <- header_h + body_h * n_rows
  bottom2 <- top - total_h
  if (bottom2 < 0.13) {
    bottom2 <- 0.13
    body_h <- (top - bottom2 - header_h) / max(1, n_rows)
  }

  x_edges <- c(left, left + cumsum(col_widths) * (right - left))

  graphics::rect(left, top - header_h, right, top, col = "#E8E8E8", border = "#CFCFCF")
  for (jj in seq_len(ncol(df))) {
    graphics::rect(x_edges[jj], top - header_h, x_edges[jj + 1], top, border = "#CFCFCF")
    draw_wrapped_text(colnames(df)[jj], x_edges[jj], x_edges[jj + 1], top - header_h / 2,
                      cex_value = header_cex, font_value = 2)
  }

  for (rr in seq_len(n_rows)) {
    y_top <- top - header_h - (rr - 1) * body_h
    y_bottom <- y_top - body_h
    fill <- if (rr %% 2 == 0) "#FBFBFB" else "white"
    graphics::rect(left, y_bottom, right, y_top, col = fill, border = "#D9D9D9")
    for (jj in seq_len(ncol(df))) {
      graphics::rect(x_edges[jj], y_bottom, x_edges[jj + 1], y_top, border = "#D9D9D9")
      draw_wrapped_text(df[rr, jj], x_edges[jj], x_edges[jj + 1], (y_top + y_bottom) / 2,
                        cex_value = font_cex, font_value = 1)
    }
  }

  graphics::text(0.02, 0.045, clean_text(footnote_value), adj = c(0, 0.5), cex = 0.72)
  invisible(TRUE)
}

draw_heatmap_panel <- function(score_df, title_value, subtitle_value, footnote_value, path_value) {
  mat0 <- make_matrix_from_scores(score_df)
  pdf_start(path_value, 12.5, 7.0)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  graphics::text(0.50, 0.955, clean_text(title_value), adj = c(0.5, 0.5), cex = 1.35, font = 2)
  graphics::text(0.50, 0.915, clean_text(subtitle_value), adj = c(0.5, 0.5), cex = 0.78)

  if (is.null(mat0) || nrow(mat0) == 0 || ncol(mat0) == 0) {
    graphics::rect(0.08, 0.22, 0.92, 0.80, col = "#F4F4F4", border = "#BBBBBB")
    graphics::text(0.50, 0.51, "No finite module scores available in locked table", cex = 1.0)
    graphics::text(0.50, 0.055, clean_text(footnote_value), adj = c(0.5, 0.5), cex = 0.72)
    return(invisible(FALSE))
  }

  mat <- row_zscore_matrix(mat0)

  desired_order <- c("A9_projection_maturation", "DA_identity", "ER_UPR_stress",
                     "mitochondrial_apoptosis_balance", "p53_apoptosis",
                     "proliferation_cell_cycle", "safety_risk_proxy",
                     "TNF_NFkB_inflammatory",
                     "priority_DA_projection", "risk_survival_stress_proxy",
                     "ferroptosis_redox", "immune_innate_stress")
  row_names_clean <- clean_text(rownames(mat))
  order_idx <- match(desired_order, row_names_clean)
  order_idx <- order_idx[is.finite(order_idx)]
  remaining <- setdiff(seq_len(nrow(mat)), order_idx)
  if (length(order_idx) > 0) {
    mat <- mat[c(order_idx, remaining), , drop = FALSE]
  }

  vals <- as.vector(mat)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) {
    graphics::rect(0.08, 0.22, 0.92, 0.80, col = "#F4F4F4", border = "#BBBBBB")
    graphics::text(0.50, 0.51, "No finite module scores available in locked table", cex = 1.0)
    graphics::text(0.50, 0.055, clean_text(footnote_value), adj = c(0.5, 0.5), cex = 0.72)
    return(invisible(FALSE))
  }

  mat[mat > 2] <- 2
  mat[mat < -2] <- -2
  pal <- grDevices::colorRampPalette(c("#2C7BB6", "#F7F7F7", "#D7191C"))(101)

  left <- 0.19
  right <- 0.88
  bottom <- 0.22
  top <- 0.84
  nr <- nrow(mat)
  nc <- ncol(mat)
  cell_w <- (right - left) / nc
  cell_h <- (top - bottom) / nr

  for (ii in seq_len(nr)) {
    for (jj in seq_len(nc)) {
      val <- mat[ii, jj]
      if (is.finite(val)) {
        idx <- round((val + 2) / 4 * 100) + 1
        idx <- max(1, min(101, idx))
        fill <- pal[idx]
      } else {
        fill <- "#EEEEEE"
      }
      x0 <- left + (jj - 1) * cell_w
      x1 <- left + jj * cell_w
      y1 <- top - (ii - 1) * cell_h
      y0 <- top - ii * cell_h
      graphics::rect(x0, y0, x1, y1, col = fill, border = "white")
    }
  }
  graphics::rect(left, bottom, right, top, border = "#777777", lwd = 0.7)

  row_labs <- module_label(rownames(mat))
  for (ii in seq_len(nr)) {
    yy <- top - (ii - 0.5) * cell_h
    graphics::text(left - 0.012, yy, labels = row_labs[ii], adj = c(1, 0.5), cex = 0.70)
  }
  col_labs <- short_sample_label(colnames(mat))
  for (jj in seq_len(nc)) {
    xx <- left + (jj - 0.5) * cell_w
    graphics::text(xx, bottom - 0.030, labels = col_labs[jj], srt = 45, adj = c(1, 1), cex = 0.62)
  }

  leg_left <- 0.915
  leg_right <- 0.932
  leg_bottom <- 0.30
  leg_top <- 0.77
  nleg <- 80
  for (kk in seq_len(nleg)) {
    y0 <- leg_bottom + (kk - 1) / nleg * (leg_top - leg_bottom)
    y1 <- leg_bottom + kk / nleg * (leg_top - leg_bottom)
    idx <- max(1, min(101, round(kk / nleg * 100) + 1))
    graphics::rect(leg_left, y0, leg_right, y1, col = pal[idx], border = NA)
  }
  graphics::rect(leg_left, leg_bottom, leg_right, leg_top, border = "#777777", lwd = 0.6)
  graphics::text(leg_right + 0.012, leg_bottom, "-2", adj = c(0, 0.5), cex = 0.62)
  graphics::text(leg_right + 0.012, (leg_bottom + leg_top) / 2, "0", adj = c(0, 0.5), cex = 0.62)
  graphics::text(leg_right + 0.012, leg_top, "2", adj = c(0, 0.5), cex = 0.62)
  graphics::text(leg_right + 0.030, (leg_bottom + leg_top) / 2, "row z-score", srt = 90, adj = c(0.5, 0.5), cex = 0.66)

  graphics::text(0.50, 0.055, clean_text(footnote_value), adj = c(0.5, 0.5), cex = 0.72)
  invisible(TRUE)
}

bulk_rows <- if (!is.null(bulk_df)) nrow(bulk_df) else 0
sc_rows <- if (!is.null(sc_df)) nrow(sc_df) else 0
cand_rows <- if (!is.null(cand_df)) nrow(cand_df) else 0
ctrl_rows <- if (!is.null(ctrl_df)) nrow(ctrl_df) else 0

overlap_total <- 0
nonzero_modules <- 0
if (!is.null(overlap_df) && nrow(overlap_df) > 0) {
  oc <- first_col_by_names(overlap_df, c("overlap_count"))
  if (!is.na(oc)) {
    ov <- safe_num(overlap_df[[oc]])
    overlap_total <- sum(ov[is.finite(ov)])
    nonzero_modules <- sum(ov[is.finite(ov)] > 0)
  }
}

status_display <- data.frame(
  Component = c("GSE216363 bulk", "GSE216364 scRNA", "GSE217131 CRISPR", "GSE217131 SafeHarbor/control loci"),
  Evidence = c(
    paste0(bulk_rows, " bulk module-score rows"),
    paste0(sc_rows, " single-cell module-score rows"),
    paste0(cand_rows, " biological manual-column proxy genes; ", overlap_total, " overlap events across ", nonzero_modules, " module(s)"),
    paste0(ctrl_rows, " control loci excluded from candidate count")
  ),
  Role = c(
    "survival/stress perturbation module support",
    "single-cell survival/stress module support",
    "manual-column CRISPR proxy overlap support only",
    "control/background audit only"
  ),
  Allowed_wording = c(
    "bulk survival/stress module support",
    "single-cell survival/stress module support",
    "CRISPR proxy overlap support; not validated survival-hit genes",
    "SafeHarbor/control loci excluded from biological candidate count"
  ),
  stringsAsFactors = FALSE
)
safe_write_tsv(status_display, file.path(out_tbl_dir, "11D_FINAL_V3F_evidence_status_DISPLAY.tsv"))

cand_display <- data.frame()
if (!is.null(cand_df) && nrow(cand_df) > 0) {
  gene_col <- first_col_by_names(cand_df, c("gene_symbol", "gene"))
  valid_col <- first_col_by_names(cand_df, c("valid_row_n", "guide_or_row_n"))
  source_col <- first_col_by_names(cand_df, c("metric_source_n"))
  mean_col <- first_col_by_names(cand_df, c("mean_effect_score"))
  median_col <- first_col_by_names(cand_df, c("median_effect_score"))
  max_col <- first_col_by_names(cand_df, c("max_abs_effect_score"))
  gene_vals <- if (!is.na(gene_col)) clean_text(cand_df[[gene_col]]) else paste0("gene_", seq_len(nrow(cand_df)))
  cand_display <- data.frame(
    Gene = gene_vals,
    Valid_rows = if (!is.na(valid_col)) fmt_num(cand_df[[valid_col]], 0) else "",
    Metric_sources = if (!is.na(source_col)) fmt_num(cand_df[[source_col]], 0) else "",
    Mean_effect = if (!is.na(mean_col)) fmt_num(cand_df[[mean_col]], 4) else "",
    Median_effect = if (!is.na(median_col)) fmt_num(cand_df[[median_col]], 4) else "",
    Max_abs_effect = if (!is.na(max_col)) fmt_num(cand_df[[max_col]], 4) else "",
    stringsAsFactors = FALSE
  )
}
safe_write_tsv(cand_display, file.path(out_tbl_dir, "11D_FINAL_V3F_biological_CRISPR_proxy_candidate_genes_DISPLAY.tsv"))

ctrl_display <- data.frame()
if (!is.null(ctrl_df) && nrow(ctrl_df) > 0) {
  gene_col <- first_col_by_names(ctrl_df, c("gene_symbol", "gene"))
  source_col <- first_col_by_names(ctrl_df, c("metric_source_n"))
  mean_col <- first_col_by_names(ctrl_df, c("mean_effect_score"))
  median_col <- first_col_by_names(ctrl_df, c("median_effect_score"))
  max_col <- first_col_by_names(ctrl_df, c("max_abs_effect_score"))
  ctrl_display <- data.frame(
    Control_locus = if (!is.na(gene_col)) clean_text(ctrl_df[[gene_col]]) else paste0("control_", seq_len(nrow(ctrl_df))),
    Metric_sources = if (!is.na(source_col)) fmt_num(ctrl_df[[source_col]], 0) else "",
    Mean_effect = if (!is.na(mean_col)) fmt_num(ctrl_df[[mean_col]], 4) else "",
    Median_effect = if (!is.na(median_col)) fmt_num(ctrl_df[[median_col]], 4) else "",
    Max_abs_effect = if (!is.na(max_col)) fmt_num(ctrl_df[[max_col]], 4) else "",
    stringsAsFactors = FALSE
  )
}
safe_write_tsv(ctrl_display, file.path(out_tbl_dir, "11D_FINAL_V3F_control_loci_excluded_DISPLAY.tsv"))

overlap_display <- data.frame()
if (!is.null(overlap_df) && nrow(overlap_df) > 0) {
  module_col <- first_col_by_names(overlap_df, c("module"))
  candidate_count_col <- first_col_by_names(overlap_df, c("biological_candidate_proxy_gene_count", "candidate_proxy_gene_count"))
  module_count_col <- first_col_by_names(overlap_df, c("module_gene_count"))
  overlap_count_col <- first_col_by_names(overlap_df, c("overlap_count"))
  overlap_genes_col <- first_col_by_names(overlap_df, c("overlap_genes"))
  ctrl_count_col <- first_col_by_names(overlap_df, c("control_loci_excluded_from_candidate_count"))
  overlap_display <- data.frame(
    Module = if (!is.na(module_col)) module_label(overlap_df[[module_col]]) else paste0("module_", seq_len(nrow(overlap_df))),
    Candidate_genes = if (!is.na(candidate_count_col)) fmt_num(overlap_df[[candidate_count_col]], 0) else "",
    Module_genes = if (!is.na(module_count_col)) fmt_num(overlap_df[[module_count_col]], 0) else "",
    Overlap = if (!is.na(overlap_count_col)) fmt_num(overlap_df[[overlap_count_col]], 0) else "",
    Overlap_genes = if (!is.na(overlap_genes_col)) format_gene_list(overlap_df[[overlap_genes_col]]) else "",
    Controls_excluded = if (!is.na(ctrl_count_col)) fmt_num(overlap_df[[ctrl_count_col]], 0) else "",
    stringsAsFactors = FALSE
  )
}
safe_write_tsv(overlap_display, file.path(out_tbl_dir, "11D_FINAL_V3F_CRISPR_overlap_DISPLAY.tsv"))

audit_display <- data.frame()
if (!is.null(audit_df) && nrow(audit_df) > 0) {
  source_col <- first_col_by_names(audit_df, c("source", "spec_id"))
  sheet_col <- first_col_by_names(audit_df, c("sheet", "sheet_name"))
  gene_col <- first_col_by_names(audit_df, c("gene_col", "gene_col_name"))
  effect_col <- first_col_by_names(audit_df, c("effect_col", "effect_col_name"))
  raw_col <- first_col_by_names(audit_df, c("raw_rows"))
  valid_col <- first_col_by_names(audit_df, c("valid_rows", "valid_gene_effect_rows"))
  unique_col <- first_col_by_names(audit_df, c("unique_genes", "unique_valid_genes"))
  audit_display <- data.frame(
    Source = if (!is.na(source_col)) clean_text(audit_df[[source_col]]) else paste0("source_", seq_len(nrow(audit_df))),
    Sheet = if (!is.na(sheet_col)) clean_text(audit_df[[sheet_col]]) else "",
    Gene_col = if (!is.na(gene_col)) clean_text(audit_df[[gene_col]]) else "",
    Effect_col = if (!is.na(effect_col)) clean_text(audit_df[[effect_col]]) else "",
    Raw_rows = if (!is.na(raw_col)) fmt_num(audit_df[[raw_col]], 0) else "",
    Valid_rows = if (!is.na(valid_col)) fmt_num(audit_df[[valid_col]], 0) else "",
    Unique_genes = if (!is.na(unique_col)) fmt_num(audit_df[[unique_col]], 0) else "",
    stringsAsFactors = FALSE
  )
}
safe_write_tsv(audit_display, file.path(out_tbl_dir, "11D_FINAL_V3F_manual_column_metric_source_audit_DISPLAY.tsv"))

fig_paths <- character()

fig_A <- file.path(out_fig_dir, "11D_FINAL_V3F_panel_A_evidence_status_PUBLICATION_POLISH.pdf")
draw_publication_table(
  status_display,
  "11D survival/stress perturbation evidence status",
  "Bulk/scRNA module evidence retained; CRISPR interpreted as manual-column proxy overlap support only.",
  "Do not claim clinical outcome prediction, graft efficacy prediction, or validated CRISPR survival-hit genes.",
  fig_A,
  col_widths = c(0.20, 0.28, 0.25, 0.27),
  font_cex = 0.70,
  header_cex = 0.70,
  width_value = 13.5,
  height_value = 7.0
)
fig_paths <- c(fig_paths, fig_A)

fig_B <- file.path(out_fig_dir, "11D_FINAL_V3F_panel_B_biological_CRISPR_proxy_genes_PUBLICATION_POLISH.pdf")
draw_publication_table(
  cand_display,
  "GSE217131 biological CRISPR proxy candidate genes",
  "V5D-guided manual-column parsing; SafeHarbor/control loci excluded; proxy-level evidence only.",
  "Candidate genes are not manually confirmed survival-hit genes. Use as supplementary/supportive perturbation audit.",
  fig_B,
  col_widths = c(0.18, 0.15, 0.16, 0.17, 0.17, 0.17),
  font_cex = 0.66,
  header_cex = 0.66,
  width_value = 13.5,
  height_value = 7.2
)
fig_paths <- c(fig_paths, fig_B)

fig_B2 <- file.path(out_fig_dir, "11D_FINAL_V3F_panel_B2_control_loci_excluded_PUBLICATION_POLISH.pdf")
draw_publication_table(
  ctrl_display,
  "SafeHarbor/control loci excluded from CRISPR candidate count",
  "Control loci are retained for audit but not treated as biological proxy candidate genes.",
  "This panel documents why SafeHarbor/control loci are excluded from biological candidate counts.",
  fig_B2,
  col_widths = c(0.24, 0.18, 0.20, 0.20, 0.18),
  font_cex = 0.78,
  header_cex = 0.72,
  width_value = 12.5,
  height_value = 6.4
)
fig_paths <- c(fig_paths, fig_B2)

fig_C <- file.path(out_fig_dir, "11D_FINAL_V3F_panel_C_CRISPR_overlap_PUBLICATION_POLISH.pdf")
draw_publication_table(
  overlap_display,
  "Biological manual-column CRISPR proxy overlap with survival/stress modules",
  "Diagnostic table; SafeHarbor/control loci excluded; proxy-level support only, not validated survival-hit evidence.",
  "No negative count axis is used. Full source table is saved in final output tables.",
  fig_C,
  col_widths = c(0.24, 0.13, 0.13, 0.10, 0.30, 0.10),
  font_cex = 0.64,
  header_cex = 0.62,
  width_value = 14.0,
  height_value = 7.0
)
fig_paths <- c(fig_paths, fig_C)

fig_D <- file.path(out_fig_dir, "11D_FINAL_V3F_panel_D_manual_column_source_audit_PUBLICATION_POLISH.pdf")
draw_publication_table(
  audit_display,
  "Manual-column CRISPR metric-source audit",
  "Readable provenance of manually selected workbook/sheet/effect columns contributing to the proxy analysis.",
  "This audit panel documents column provenance and prevents overinterpretation of automatically parsed CRISPR tables.",
  fig_D,
  col_widths = c(0.18, 0.20, 0.12, 0.24, 0.09, 0.09, 0.08),
  font_cex = 0.62,
  header_cex = 0.62,
  width_value = 14.0,
  height_value = 7.0
)
fig_paths <- c(fig_paths, fig_D)

fig_E <- file.path(out_fig_dir, "11D_FINAL_V3F_panel_E_GSE216363_bulk_heatmap_PUBLICATION_POLISH.pdf")
draw_heatmap_panel(
  bulk_df,
  "GSE216363 bulk survival/stress module scores",
  "Row-wise z-score heatmap regenerated from the validated 11D V1 bulk module-score table.",
  "Module-score heatmap supports survival/stress perturbation evidence; not clinical outcome prediction.",
  fig_E
)
fig_paths <- c(fig_paths, fig_E)

fig_F <- file.path(out_fig_dir, "11D_FINAL_V3F_panel_F_GSE216364_scRNA_heatmap_PUBLICATION_POLISH.pdf")
draw_heatmap_panel(
  sc_df,
  "GSE216364 scRNA survival/stress module scores",
  "Sample-level mean single-cell module-score heatmap regenerated from the validated 11D V1 table.",
  "Single-cell module-score heatmap supports survival/stress perturbation evidence; not clinical outcome prediction.",
  fig_F
)
fig_paths <- c(fig_paths, fig_F)

fig_manifest <- data.frame(
  panel = c("A", "B", "B2", "C", "D", "E", "F"),
  figure_path = fig_paths,
  exists = file.exists(fig_paths),
  stringsAsFactors = FALSE
)
safe_write_tsv(fig_manifest, file.path(out_tbl_dir, "11D_FINAL_V3F_figure_manifest.tsv"))

execution_summary <- data.frame(
  metric = c(
    "bulk_module_score_rows_reused_from_V1",
    "scRNA_module_score_rows_reused_from_V1",
    "biological_manual_column_CRISPR_proxy_candidate_genes",
    "SafeHarbor_control_loci_excluded",
    "survival_stress_module_overlap_events",
    "nonzero_overlap_modules",
    "figures_written",
    "figures_missing",
    "claim_level",
    "decision"
  ),
  value = c(
    as.character(bulk_rows),
    as.character(sc_rows),
    as.character(cand_rows),
    as.character(ctrl_rows),
    as.character(overlap_total),
    as.character(nonzero_modules),
    as.character(sum(file.exists(fig_paths))),
    as.character(sum(!file.exists(fig_paths))),
    "manual-column CRISPR proxy overlap support only; SafeHarbor/control loci excluded",
    if (sum(!file.exists(fig_paths)) == 0) "FINAL_LOCKED_11D_VISUAL_POLISH_READY_FOR_11H_INTEGRATION" else "FINAL_LOCKED_11D_VISUAL_POLISH_INCOMPLETE_REVIEW_MANIFEST"
  ),
  stringsAsFactors = FALSE
)
safe_write_tsv(execution_summary, file.path(out_tbl_dir, "11D_FINAL_V3F_execution_summary.tsv"))

report_lines <- c(
  "11D FINAL V3F publication visual polish report",
  "================================================",
  paste0("Bulk module-score rows reused from V1: ", bulk_rows),
  paste0("scRNA module-score rows reused from V1: ", sc_rows),
  paste0("Biological manual-column CRISPR proxy candidate genes: ", cand_rows),
  paste0("SafeHarbor/control loci excluded: ", ctrl_rows),
  paste0("Survival/stress module overlap events: ", overlap_total),
  paste0("Nonzero overlap modules: ", nonzero_modules),
  paste0("Figures written: ", sum(file.exists(fig_paths))),
  paste0("Figures missing: ", sum(!file.exists(fig_paths))),
  "",
  "Claim boundary:",
  "Use: survival/stress perturbation module support with manual-column CRISPR proxy overlap evidence.",
  "Do not use: validated CRISPR survival-hit genes; clinical outcome prediction; graft efficacy prediction; clinical safety prediction.",
  "",
  paste0("Output figures: ", out_fig_dir),
  paste0("Output tables : ", out_tbl_dir)
)
writeLines(report_lines, file.path(out_txt_dir, "11D_FINAL_V3F_publication_visual_polish_report.txt"), useBytes = TRUE)

cat("\n[11D FINAL V3F] Completed publication visual polish from locked validated 11D tables.\n")
cat("[11D FINAL V3F] Bulk module-score rows reused from V1:", bulk_rows, "\n")
cat("[11D FINAL V3F] scRNA module-score rows reused from V1:", sc_rows, "\n")
cat("[11D FINAL V3F] Biological CRISPR proxy candidate genes:", cand_rows, "\n")
cat("[11D FINAL V3F] SafeHarbor/control loci excluded:", ctrl_rows, "\n")
cat("[11D FINAL V3F] Survival/stress module overlap events:", overlap_total, "\n")
cat("[11D FINAL V3F] Nonzero overlap modules:", nonzero_modules, "\n")
cat("[11D FINAL V3F] Figures written:", sum(file.exists(fig_paths)), "\n")
cat("[11D FINAL V3F] Figures missing:", sum(!file.exists(fig_paths)), "\n")
cat("[11D FINAL V3F] Decision:", execution_summary$value[execution_summary$metric == "decision"], "\n")
cat("[11D FINAL V3F] Output figures:", out_fig_dir, "\n")
cat("[11D FINAL V3F] Output tables :", out_tbl_dir, "\n")
cat("[11D FINAL V3F] Next          : 11E barcode lineage tracing validation, or 11H after all 11 modules.\n")
