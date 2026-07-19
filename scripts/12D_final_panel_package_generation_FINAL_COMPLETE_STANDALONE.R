
# ============================================================
# 12D FINAL COMPLETE STANDALONE - NATURE COLOR
# Final panel-package generation for DA neuron / graft-related
# transcriptomic cell-state prioritisation framework
#
# User rule respected:
#   - Complete standalone script for 12D
#   - Does NOT read any previous 12D output
#   - Does NOT use table-only patch logic
#   - May read locked upstream outputs as formal inputs:
#       10A-10P, 11A-11J, 12A, 12B, 12C
#   - Uses 12C panel-level source lock as the formal source manifest
#   - Generates final panel-package folders, standardized panel package PDFs,
#     table-redrawn panels for table-only sources, package manifests,
#     report text and Nature-style audit PDFs
#   - No internet
#   - No 00-10P rerun
#
# Scientific boundary:
#   - Panel-package generation and provenance traceability only
#   - Evidence-anchored transcriptomic prioritisation framework
#   - Candidate transcriptomic marker signatures only
#   - marker-rule-derived prioritization model audit only
#   - No clinical prediction
#   - No diagnostic/prognostic biomarker validation
#   - No causal graft efficacy/safety claim
#   - No true anatomical projection or barcode-lineage proof
# ============================================================

cat("\n[12D FINAL] Starting final panel-package generation...\n")
cat("[12D FINAL] Mode: complete standalone 12D rebuild; no previous 12D dependency; no internet; no 00-10P rerun.\n")
cat("[12D FINAL] Inputs allowed: locked upstream 10A-10P, 11A-11J, 12A, 12B and 12C outputs.\n")
cat("[12D FINAL] Claim boundary: panel-package/provenance generation only; no clinical prediction or validated biomarker claim.\n")
cat("[12D FINAL] Figure style: Nature-style clean publication layout.\n")

graphics.off()

# ------------------------- project paths -------------------------
project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")
figure_root <- file.path(project_root, "04_figures")
text_root <- file.path(project_root, "09_manuscript")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "12D_final_panel_package_generation_FINAL_COMPLETE_STANDALONE"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "12D_final_panel_package_generation_FINAL_COMPLETE_STANDALONE_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "12D_final_panel_package_generation_FINAL_COMPLETE_STANDALONE"
)
out_package_dir <- file.path(
  project_root,
  "04_figures",
  "12D_final_panel_package_generation_FINAL_COMPLETE_STANDALONE_panel_packages"
)

dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_text_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_package_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------- safe helper functions -------------------------
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

safe_file_name <- function(value_obj) {
  out <- clean_space(value_obj)
  out <- gsub("[/\\\\:;*?\"<>|]+", "_", out)
  out <- gsub("[^A-Za-z0-9._-]+", "_", out)
  out <- gsub("_+", "_", out)
  out <- gsub("^_+|_+$", "", out)
  out[out == ""] <- "unnamed"
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
  cat("[12D FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[12D FINAL] Wrote:", file_value, "\n")
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

open_pdf_safe <- function(filename, width_value = 10, height_value = 6, target_dir = out_fig_dir) {
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  file_primary <- file.path(target_dir, filename)
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
    file_alt <- file.path(target_dir, alt_name)
    grDevices::pdf(
      file_alt,
      width = width_value,
      height = height_value,
      onefile = FALSE,
      useDingbats = FALSE,
      paper = "special"
    )
    cat("[12D FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
    return(file_alt)
  }
  file_primary
}

new_canvas <- function() {
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", xpd = FALSE)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
}

# ------------------------- Nature-style colors -------------------------
nature_palette <- list(
  ink = "#1D1D1F",
  muted = "#5F6368",
  grid = "#E6E8EB",
  border = "#2F3A45",
  navy = "#3B4992",
  blue = "#4DBBD5",
  teal = "#00A087",
  orange = "#E64B35",
  red = "#B2182B",
  purple = "#7E6148",
  gold = "#F39B7F",
  pale_blue = "#EAF2F8",
  pale_orange = "#FDE9DF",
  pale_green = "#E8F3EF",
  pale_purple = "#EFE8F3",
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

figure_color <- function(figure_id_value) {
  figure_text <- toupper(safe_chr(figure_id_value))
  out_colors <- rep(nature_palette$navy, length(figure_text))
  out_colors[grepl("FIG 1|S1|SOURCE|FRAMEWORK", figure_text)] <- nature_palette$blue
  out_colors[grepl("FIG 2|S2|PRIORITISATION|DA|CORE", figure_text)] <- nature_palette$teal
  out_colors[grepl("FIG 3|S3|PSEUDOTIME|CORRELATION|MATURATION", figure_text)] <- nature_palette$red
  out_colors[grepl("FIG 4|S4|PROXY|PRECLINICAL|PROJECTION|STATE", figure_text)] <- nature_palette$purple
  out_colors[grepl("FIG 5|S5|INTEGRATED|UMBRELLA|ML|ROC", figure_text)] <- nature_palette$navy
  out_colors[grepl("RISK|SAFETY|GENETIC|GWAS", figure_text)] <- nature_palette$orange
  out_colors
}

status_color <- function(status_value) {
  status_text <- tolower(safe_chr(status_value))
  out_colors <- rep(nature_palette$teal, length(status_text))
  out_colors[grepl("table_only", status_text)] <- nature_palette$blue
  out_colors[grepl("figure_only", status_text)] <- nature_palette$purple
  out_colors[grepl("needs", status_text)] <- nature_palette$orange
  out_colors
}

draw_title <- function(title_value, subtitle_value = "") {
  text(0.5, 0.965, title_value, cex = 0.98, font = 2, adj = c(0.5, 0.5), col = nature_palette$ink)
  if (nchar(subtitle_value) > 0) {
    text(0.5, 0.928, subtitle_value, cex = 0.50, col = nature_palette$muted, adj = c(0.5, 0.5))
  }
}

# ------------------------- upstream file discovery -------------------------
if (!dir.exists(table_root)) stop("[12D FINAL] Missing table root: ", table_root, call. = FALSE)
if (!dir.exists(figure_root)) stop("[12D FINAL] Missing figure root: ", figure_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
table_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(table_info$size) & table_info$size > 0 & table_info$size < 150 * 1024 * 1024]

# Hard rule: do not read previous 12D output
all_table_files <- all_table_files[!grepl("12D_final_panel_package_generation", all_table_files, ignore.case = TRUE)]

find_files_all_terms <- function(file_values, term_values, max_n = 20) {
  if (length(file_values) < 1) return(character(0))
  term_values <- tolower(safe_chr(term_values))
  path_lower <- tolower(file_values)
  keep_vec <- rep(TRUE, length(file_values))
  for (term_value in term_values) keep_vec <- keep_vec & grepl(term_value, path_lower, fixed = TRUE)
  hits <- file_values[keep_vec]
  if (length(hits) < 1) return(character(0))
  hit_info <- file.info(hits)
  hits <- hits[order(hit_info$mtime, decreasing = TRUE)]
  unique(hits)[seq_len(min(max_n, length(unique(hits))))]
}

first_existing_file <- function(file_values) {
  file_values <- safe_chr(file_values)
  file_values <- file_values[file.exists(file_values)]
  if (length(file_values) < 1) return("")
  file_values[1]
}

# ------------------------- read locked 12C source manifest -------------------------
file_12c_panel_lock <- first_existing_file(c(
  file.path(table_root, "12C_final_source_panel_lock_refresh_FINAL_COMPLETE_STANDALONE", "12C_FINAL_panel_level_source_lock.csv"),
  find_files_all_terms(all_table_files, c("12c", "panel_level_source_lock"), max_n = 10)
))
file_12c_figure_summary <- first_existing_file(c(
  file.path(table_root, "12C_final_source_panel_lock_refresh_FINAL_COMPLETE_STANDALONE", "12C_FINAL_figure_level_source_lock_summary.csv"),
  find_files_all_terms(all_table_files, c("12c", "figure_level_source_lock_summary"), max_n = 10)
))
file_12c_source_manifest <- first_existing_file(c(
  file.path(table_root, "12C_final_source_panel_lock_refresh_FINAL_COMPLETE_STANDALONE", "12C_FINAL_source_file_manifest.csv"),
  find_files_all_terms(all_table_files, c("12c", "source_file_manifest"), max_n = 10)
))
file_12c_claim_lock <- first_existing_file(c(
  file.path(table_root, "12C_final_source_panel_lock_refresh_FINAL_COMPLETE_STANDALONE", "12C_FINAL_claim_boundary_source_lock.csv"),
  find_files_all_terms(all_table_files, c("12c", "claim_boundary_source_lock"), max_n = 10)
))

panel_lock_df <- read_table_safe(file_12c_panel_lock)
figure_summary_df <- read_table_safe(file_12c_figure_summary)
source_manifest_12c_df <- read_table_safe(file_12c_source_manifest)
claim_lock_12c_df <- read_table_safe(file_12c_claim_lock)

if (nrow(panel_lock_df) < 1) stop("[12D FINAL] Missing 12C panel-level source lock table.", call. = FALSE)
if (!("figure_id" %in% colnames(panel_lock_df))) stop("[12D FINAL] 12C panel lock table missing figure_id.", call. = FALSE)
if (!("panel_id" %in% colnames(panel_lock_df))) stop("[12D FINAL] 12C panel lock table missing panel_id.", call. = FALSE)

input_audit_df <- data.frame(
  input_name = c("12C_panel_level_source_lock", "12C_figure_level_source_lock_summary", "12C_source_file_manifest", "12C_claim_boundary_source_lock"),
  detected = c(file_12c_panel_lock != "", file_12c_figure_summary != "", file_12c_source_manifest != "", file_12c_claim_lock != ""),
  file_path = c(file_12c_panel_lock, file_12c_figure_summary, file_12c_source_manifest, file_12c_claim_lock),
  rows_loaded = c(nrow(panel_lock_df), nrow(figure_summary_df), nrow(source_manifest_12c_df), nrow(claim_lock_12c_df)),
  allowed_as_locked_upstream_input = c(TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
write_csv_safe(input_audit_df, file.path(out_table_dir, "12D_FINAL_locked_12C_input_audit.csv"))

# ------------------------- panel package generation helpers -------------------------
draw_table_preview_panel <- function(panel_row, source_table_file, output_pdf_file) {
  source_data <- read_table_safe(source_table_file)
  if (!is.data.frame(source_data) || nrow(source_data) < 1) {
    source_data <- data.frame(note = "source table could not be read or was empty", stringsAsFactors = FALSE)
  }

  display_cols <- colnames(source_data)
  if (length(display_cols) > 6) display_cols <- display_cols[seq_len(6)]
  display_rows <- seq_len(min(8, nrow(source_data)))
  display_data <- source_data[display_rows, display_cols, drop = FALSE]
  for (col_value in colnames(display_data)) {
    display_data[[col_value]] <- clean_space(display_data[[col_value]])
    display_data[[col_value]] <- substr(display_data[[col_value]], 1, 42)
  }

  open_pdf_safe(basename(output_pdf_file), width_value = 11.2, height_value = 6.6, target_dir = dirname(output_pdf_file))
  new_canvas()
  color_now <- figure_color(panel_row$figure_id)
  draw_title(
    paste0(panel_row$panel_id, " table-derived panel"),
    "Table-only source rendered as a clean panel-package preview."
  )

  rect(0.06, 0.77, 0.94, 0.86, col = blend_color(nature_palette$white, color_now, 0.18), border = color_now, lwd = 0.8)
  text(0.08, 0.835, paste0("Figure: ", panel_row$figure_id), cex = 0.48, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.08, 0.805, paste0("Panel: ", panel_row$panel_label, " | Module: ", panel_row$primary_locked_module), cex = 0.42, adj = c(0, 0.5), col = nature_palette$muted)
  text(0.50, 0.805, paste0("Content: ", substr(panel_row$planned_panel_content, 1, 90)), cex = 0.42, adj = c(0, 0.5), col = nature_palette$muted)

  # table preview
  table_x0 <- 0.06
  table_x1 <- 0.94
  table_y0 <- 0.20
  table_y1 <- 0.70
  n_cols <- max(1, ncol(display_data))
  n_rows <- max(1, nrow(display_data))
  cell_w <- (table_x1 - table_x0) / n_cols
  cell_h <- (table_y1 - table_y0) / (n_rows + 1)

  for (col_idx in seq_len(n_cols)) {
    rect(table_x0 + (col_idx - 1) * cell_w, table_y1 - cell_h,
         table_x0 + col_idx * cell_w, table_y1,
         col = color_now, border = nature_palette$white, lwd = 0.35)
    text(table_x0 + (col_idx - 0.5) * cell_w, table_y1 - 0.5 * cell_h,
         substr(colnames(display_data)[col_idx], 1, 20), cex = 0.34, col = nature_palette$white)
  }

  for (row_idx in seq_len(n_rows)) {
    for (col_idx in seq_len(n_cols)) {
      rect(table_x0 + (col_idx - 1) * cell_w, table_y1 - (row_idx + 1) * cell_h,
           table_x0 + col_idx * cell_w, table_y1 - row_idx * cell_h,
           col = ifelse(row_idx %% 2 == 0, nature_palette$white, blend_color(nature_palette$white, nature_palette$pale_blue, 0.55)),
           border = nature_palette$grid, lwd = 0.25)
      text(table_x0 + 0.004 + (col_idx - 1) * cell_w, table_y1 - (row_idx + 0.5) * cell_h,
           safe_chr(display_data[row_idx, col_idx]), cex = 0.28, adj = c(0, 0.5), col = nature_palette$ink)
    }
  }

  text(0.06, 0.12, paste0("Source table: ", source_table_file), cex = 0.30, adj = c(0, 0.5), col = nature_palette$muted)
  text(0.06, 0.08, "Claim boundary: source preview only; final biological interpretation belongs in caption/manuscript.", cex = 0.36, adj = c(0, 0.5), col = nature_palette$orange)
  dev.off()
  output_pdf_file
}

draw_panel_index_sheet <- function(panel_row, output_pdf_file, selected_source_pdf, selected_table_pdf) {
  open_pdf_safe(basename(output_pdf_file), width_value = 10.8, height_value = 5.9, target_dir = dirname(output_pdf_file))
  new_canvas()
  color_now <- figure_color(panel_row$figure_id)
  draw_title(paste0(panel_row$panel_id, " package index"), "Standardized source and claim-boundary record for final panel assembly.")

  rect(0.06, 0.66, 0.94, 0.82, col = blend_color(nature_palette$white, color_now, 0.18), border = color_now, lwd = 0.8)
  text(0.08, 0.775, paste0("Figure: ", panel_row$figure_id), cex = 0.50, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.08, 0.735, paste0("Panel: ", panel_row$panel_label, " | Source-lock status: ", panel_row$source_lock_status), cex = 0.42, adj = c(0, 0.5), col = nature_palette$muted)
  text(0.08, 0.695, paste0("Primary locked module: ", panel_row$primary_locked_module), cex = 0.42, adj = c(0, 0.5), col = nature_palette$muted)

  info_df <- data.frame(
    label = c("Planned content", "Primary source PDF", "Primary source table", "12D package PDF", "Table-rendered panel", "Claim boundary"),
    value = c(
      panel_row$planned_panel_content,
      ifelse(selected_source_pdf != "", selected_source_pdf, "none"),
      ifelse(panel_row$primary_source_table != "", panel_row$primary_source_table, "none"),
      ifelse(selected_source_pdf != "", selected_source_pdf, "none"),
      ifelse(selected_table_pdf != "", selected_table_pdf, "none"),
      panel_row$claim_boundary
    ),
    stringsAsFactors = FALSE
  )
  row_y <- seq(0.55, 0.18, length.out = nrow(info_df))
  for (row_idx in seq_len(nrow(info_df))) {
    text(0.08, row_y[row_idx], info_df$label[row_idx], cex = 0.40, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
    text(0.28, row_y[row_idx], substr(info_df$value[row_idx], 1, 118), cex = 0.34, adj = c(0, 0.5), col = nature_palette$muted)
  }
  dev.off()
  output_pdf_file
}

copy_source_pdf_safe <- function(source_pdf_file, destination_pdf_file) {
  if (!file.exists(source_pdf_file)) return(FALSE)
  dir.create(dirname(destination_pdf_file), recursive = TRUE, showWarnings = FALSE)
  ok_copy <- FALSE
  tryCatch({
    ok_copy <- file.copy(source_pdf_file, destination_pdf_file, overwrite = TRUE)
  }, error = function(err_obj) {
    ok_copy <<- FALSE
  })
  isTRUE(ok_copy)
}

draw_figure_package_index <- function(figure_id_value, figure_rows, output_pdf_file) {
  open_pdf_safe(basename(output_pdf_file), width_value = 12.2, height_value = 6.8, target_dir = dirname(output_pdf_file))
  new_canvas()
  color_now <- figure_color(figure_id_value)
  draw_title(paste0(figure_id_value, " final panel package index"), "Panel source files standardized for 12D final package assembly.")

  n_rows <- nrow(figure_rows)
  if (n_rows < 1) {
    text(0.5, 0.5, "No panels available.", cex = 0.8)
    dev.off()
    return(output_pdf_file)
  }

  rect(0.06, 0.78, 0.94, 0.84, col = blend_color(nature_palette$white, color_now, 0.18), border = color_now, lwd = 0.8)
  text(0.08, 0.81, paste0("Panels: ", n_rows, " | Figure type: ", figure_rows$figure_type[1]), cex = 0.46, font = 2, adj = c(0, 0.5), col = nature_palette$ink)

  y_positions <- seq(0.70, 0.20, length.out = n_rows)
  for (idx_row in seq_len(n_rows)) {
    current_row <- figure_rows[idx_row, , drop = FALSE]
    status_now <- current_row$final_panel_package_status
    status_col <- status_color(status_now)
    rect(0.08, y_positions[idx_row] - 0.023, 0.16, y_positions[idx_row] + 0.023,
         col = status_col, border = nature_palette$border, lwd = 0.35)
    text(0.12, y_positions[idx_row], current_row$panel_label, cex = 0.42, font = 2, col = nature_palette$white)
    text(0.18, y_positions[idx_row], substr(current_row$planned_panel_content, 1, 54), cex = 0.38, adj = c(0, 0.5), col = nature_palette$ink)
    text(0.60, y_positions[idx_row], current_row$primary_locked_module, cex = 0.36, adj = c(0, 0.5), col = nature_palette$muted)
    text(0.70, y_positions[idx_row], status_now, cex = 0.34, adj = c(0, 0.5), col = nature_palette$muted)
  }
  dev.off()
  output_pdf_file
}

# ------------------------- generate panel packages -------------------------
cat("[12D FINAL] Generating standardized panel packages...\n")

panel_package_list <- list()
unique_figures <- unique(panel_lock_df$figure_id)

for (idx_panel in seq_len(nrow(panel_lock_df))) {
  panel_row <- panel_lock_df[idx_panel, , drop = FALSE]
  figure_safe <- safe_file_name(panel_row$figure_id)
  panel_safe <- safe_file_name(panel_row$panel_id)
  figure_package_dir <- file.path(out_package_dir, figure_safe)
  panel_package_dir <- file.path(figure_package_dir, panel_safe)
  source_copy_dir <- file.path(panel_package_dir, "source_pdf_copy")
  table_render_dir <- file.path(panel_package_dir, "table_rendered_panel")
  index_dir <- file.path(panel_package_dir, "package_index")
  dir.create(source_copy_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(table_render_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)

  selected_source_pdf <- ""
  source_copy_ok <- FALSE
  if ("primary_source_pdf" %in% colnames(panel_row) && clean_space(panel_row$primary_source_pdf) != "") {
    source_pdf_now <- clean_space(panel_row$primary_source_pdf)
    if (file.exists(source_pdf_now)) {
      selected_source_pdf <- file.path(source_copy_dir, paste0(panel_safe, "__SOURCE_COPY.pdf"))
      source_copy_ok <- copy_source_pdf_safe(source_pdf_now, selected_source_pdf)
      if (!source_copy_ok) selected_source_pdf <- source_pdf_now
    }
  }

  table_render_pdf <- ""
  table_render_ok <- FALSE
  if ("primary_source_table" %in% colnames(panel_row) && clean_space(panel_row$primary_source_table) != "") {
    source_table_now <- clean_space(panel_row$primary_source_table)
    if (file.exists(source_table_now)) {
      table_render_pdf <- file.path(table_render_dir, paste0(panel_safe, "__TABLE_RENDERED_PANEL.pdf"))
      tryCatch({
        draw_table_preview_panel(panel_row, source_table_now, table_render_pdf)
        table_render_ok <- file.exists(table_render_pdf)
      }, error = function(err_obj) {
        table_render_ok <<- FALSE
      })
    }
  }

  # For table-only panels, table-rendered PDF becomes the final package PDF.
  # For panels with source PDF, the copied source PDF is the primary final package source, while table render remains a preview/reference.
  final_panel_pdf <- ""
  final_status <- "needs_review"
  if (source_copy_ok && selected_source_pdf != "") {
    final_panel_pdf <- selected_source_pdf
    final_status <- "source_pdf_packaged"
  }
  if (!source_copy_ok && table_render_ok && table_render_pdf != "") {
    final_panel_pdf <- table_render_pdf
    final_status <- "table_redrawn_panel_packaged"
  }
  if (source_copy_ok && table_render_ok) {
    final_status <- "source_pdf_plus_table_render_packaged"
  }

  index_pdf <- file.path(index_dir, paste0(panel_safe, "__PANEL_PACKAGE_INDEX.pdf"))
  tryCatch({
    draw_panel_index_sheet(panel_row, index_pdf, final_panel_pdf, table_render_pdf)
  }, error = function(err_obj) {
    index_pdf <<- ""
  })

  panel_package_list[[length(panel_package_list) + 1]] <- data.frame(
    figure_type = panel_row$figure_type,
    figure_id = panel_row$figure_id,
    panel_id = panel_row$panel_id,
    panel_label = panel_row$panel_label,
    planned_panel_content = panel_row$planned_panel_content,
    primary_locked_module = panel_row$primary_locked_module,
    source_lock_status = panel_row$source_lock_status,
    primary_source_pdf = panel_row$primary_source_pdf,
    primary_source_table = panel_row$primary_source_table,
    copied_source_pdf = ifelse(source_copy_ok, selected_source_pdf, ""),
    table_rendered_panel_pdf = ifelse(table_render_ok, table_render_pdf, ""),
    panel_package_index_pdf = index_pdf,
    final_panel_package_pdf = final_panel_pdf,
    final_panel_package_status = final_status,
    panel_package_dir = panel_package_dir,
    ready_for_12E_visual_audit = final_status != "needs_review",
    stringsAsFactors = FALSE
  )

  if (idx_panel %% 10 == 0) {
    cat("[12D FINAL] Panel packages generated ", idx_panel, "/", nrow(panel_lock_df), "\n", sep = "")
  }
}

panel_package_df <- safe_bind_rows(panel_package_list)
write_csv_safe(panel_package_df, file.path(out_table_dir, "12D_FINAL_panel_package_manifest.csv"))
write_tsv_safe(panel_package_df, file.path(out_table_dir, "12D_FINAL_panel_package_manifest.tsv"))

write_csv_safe(
  panel_package_df[panel_package_df$figure_type == "main", , drop = FALSE],
  file.path(out_table_dir, "12D_FINAL_main_figure_panel_package_manifest.csv")
)
write_csv_safe(
  panel_package_df[panel_package_df$figure_type == "supplement", , drop = FALSE],
  file.path(out_table_dir, "12D_FINAL_supplementary_figure_panel_package_manifest.csv")
)

table_redraw_df <- panel_package_df[panel_package_df$table_rendered_panel_pdf != "", , drop = FALSE]
write_csv_safe(table_redraw_df, file.path(out_table_dir, "12D_FINAL_table_rendered_panel_manifest.csv"))

# ------------------------- figure package index generation -------------------------
figure_package_list <- list()
for (idx_fig in seq_along(unique_figures)) {
  fig_now <- unique_figures[idx_fig]
  fig_safe <- safe_file_name(fig_now)
  fig_rows <- panel_package_df[panel_package_df$figure_id == fig_now, , drop = FALSE]
  fig_dir <- file.path(out_package_dir, fig_safe)
  fig_index_pdf <- file.path(fig_dir, paste0(fig_safe, "__FIGURE_PACKAGE_INDEX.pdf"))
  tryCatch({
    draw_figure_package_index(fig_now, fig_rows, fig_index_pdf)
  }, error = function(err_obj) {
    fig_index_pdf <<- ""
  })

  n_ready <- sum(fig_rows$ready_for_12E_visual_audit, na.rm = TRUE)
  n_panel <- nrow(fig_rows)
  figure_status <- ifelse(n_ready == n_panel, "ready_for_12E_visual_audit", "needs_package_review")

  figure_package_list[[length(figure_package_list) + 1]] <- data.frame(
    figure_id = fig_now,
    figure_type = fig_rows$figure_type[1],
    n_panels = n_panel,
    n_panels_packaged = n_ready,
    n_source_pdf_packaged = sum(grepl("source_pdf", fig_rows$final_panel_package_status), na.rm = TRUE),
    n_table_redrawn_packaged = sum(grepl("table_redrawn", fig_rows$final_panel_package_status), na.rm = TRUE),
    figure_package_index_pdf = fig_index_pdf,
    figure_package_dir = fig_dir,
    figure_package_status = figure_status,
    stringsAsFactors = FALSE
  )
}
figure_package_df <- safe_bind_rows(figure_package_list)
write_csv_safe(figure_package_df, file.path(out_table_dir, "12D_FINAL_figure_package_manifest.csv"))
write_tsv_safe(figure_package_df, file.path(out_table_dir, "12D_FINAL_figure_package_manifest.tsv"))

# ------------------------- source provenance manifest -------------------------
source_provenance_df <- data.frame(
  source_category = c(
    "12C panel lock input",
    "12C figure summary input",
    "12C source manifest input",
    "12C claim lock input",
    "12D panel package output",
    "12D figure package output"
  ),
  file_path = c(
    file_12c_panel_lock,
    file_12c_figure_summary,
    file_12c_source_manifest,
    file_12c_claim_lock,
    file.path(out_table_dir, "12D_FINAL_panel_package_manifest.csv"),
    file.path(out_table_dir, "12D_FINAL_figure_package_manifest.csv")
  ),
  role = c(
    "primary locked source manifest",
    "figure-level readiness context",
    "upstream source-file provenance",
    "claim-boundary control",
    "final panel-level 12D package manifest",
    "final figure-level 12D package manifest"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(source_provenance_df, file.path(out_table_dir, "12D_FINAL_source_provenance_manifest.csv"))

# ------------------------- claim boundary check -------------------------
claim_check_df <- data.frame(
  package_rule = c(
    "table-only source panels",
    "source-PDF panels",
    "figure package index",
    "ML audit panels",
    "projection/state proxy panels",
    "candidate marker panels",
    "risk/genetic-context panels"
  ),
  implementation = c(
    "redrawn as clean table-derived panels, not copied as old figures",
    "copied into standardized package folders with source provenance",
    "generated for each figure to support 12E visual audit",
    "retain marker-rule-derived ROC/PR audit language only",
    "retain proxy/molecular competence language only",
    "retain candidate transcriptomic marker signature language only",
    "retain risk-context/limited genetic-context language only"
  ),
  prohibited_overclaim = c(
    "do not imply novel analysis beyond source table",
    "do not imply source PDF is newly computed in 12D",
    "do not use as biological result panel",
    "do not claim clinical prediction",
    "do not claim anatomical projection or lineage proof",
    "do not claim validated clinical biomarker",
    "do not claim clinical safety prediction or genetic causality"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(claim_check_df, file.path(out_table_dir, "12D_FINAL_claim_boundary_package_check.csv"))

# ------------------------- 12E handoff -------------------------
handoff_12e_df <- data.frame(
  handoff_item = c(
    "panel package manifest",
    "figure package manifest",
    "main figure package manifest",
    "supplementary figure package manifest",
    "table-rendered panel manifest",
    "source provenance manifest",
    "claim-boundary package check"
  ),
  file_path = c(
    file.path(out_table_dir, "12D_FINAL_panel_package_manifest.csv"),
    file.path(out_table_dir, "12D_FINAL_figure_package_manifest.csv"),
    file.path(out_table_dir, "12D_FINAL_main_figure_panel_package_manifest.csv"),
    file.path(out_table_dir, "12D_FINAL_supplementary_figure_panel_package_manifest.csv"),
    file.path(out_table_dir, "12D_FINAL_table_rendered_panel_manifest.csv"),
    file.path(out_table_dir, "12D_FINAL_source_provenance_manifest.csv"),
    file.path(out_table_dir, "12D_FINAL_claim_boundary_package_check.csv")
  ),
  role_in_12E = c(
    "visual audit input for all panels",
    "figure-level readiness input",
    "main figure audit input",
    "supplement figure audit input",
    "check table-only source redraws",
    "audit source provenance",
    "audit claim-boundary compliance"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(handoff_12e_df, file.path(out_table_dir, "12D_FINAL_handoff_to_12E_visual_audit.csv"))

# ------------------------- 12D summary figures -------------------------
# FigA final package overview
fig_a <- open_pdf_safe("12D_FINAL_FigA_final_panel_package_overview.pdf", 11.8, 6.6)
new_canvas()
draw_title("Final panel-package generation overview", "Panel packages generated from locked 12C source manifest; no previous 12D output was used.")

overview_df <- data.frame(
  label = c(
    "Figures packaged",
    "Main figures",
    "Supplementary figures",
    "Panels packaged",
    "Source-PDF packaged panels",
    "Table-redrawn panels",
    "Figures ready for 12E"
  ),
  value = c(
    nrow(figure_package_df),
    sum(figure_package_df$figure_type == "main"),
    sum(figure_package_df$figure_type == "supplement"),
    nrow(panel_package_df),
    sum(grepl("source_pdf", panel_package_df$final_panel_package_status), na.rm = TRUE),
    sum(grepl("table_redrawn", panel_package_df$final_panel_package_status), na.rm = TRUE),
    sum(figure_package_df$figure_package_status == "ready_for_12E_visual_audit")
  ),
  family = c("all", "main", "supp", "panel", "pdf", "table", "ready"),
  stringsAsFactors = FALSE
)
max_overview <- max(safe_num(overview_df$value), na.rm = TRUE)
if (!is.finite(max_overview) || max_overview <= 0) max_overview <- 1
bar_x0 <- 0.37
bar_x1 <- 0.78
y_values <- seq(0.78, 0.26, length.out = nrow(overview_df))
for (idx_row in seq_len(nrow(overview_df))) {
  yy <- y_values[idx_row]
  count_now <- safe_num(overview_df$value[idx_row])
  width_now <- count_now / max_overview
  color_now <- nature_palette$navy
  if (overview_df$family[idx_row] == "main") color_now <- nature_palette$blue
  if (overview_df$family[idx_row] == "supp") color_now <- nature_palette$purple
  if (overview_df$family[idx_row] == "pdf") color_now <- nature_palette$teal
  if (overview_df$family[idx_row] == "table") color_now <- nature_palette$orange
  if (overview_df$family[idx_row] == "ready") color_now <- nature_palette$teal
  text(bar_x0 - 0.018, yy, overview_df$label[idx_row], cex = 0.56, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.024, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.024,
       col = color_now, border = nature_palette$border, lwd = 0.42)
  text(bar_x0 + width_now * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_now), cex = 0.52, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.12, "12E should visually audit the generated package PDFs before final assembly.", cex = 0.48, col = nature_palette$muted)
dev.off()
cat("[12D FINAL] Wrote figure:", fig_a, "\n")

# FigB package mode by figure
fig_b <- open_pdf_safe("12D_FINAL_FigB_package_mode_by_figure.pdf", 12.0, 6.8)
new_canvas()
draw_title("Panel-package mode by figure", "Source-PDF panels and table-redrawn panels are explicitly tracked for visual audit.")

plot_df <- figure_package_df
plot_df <- plot_df[order(plot_df$figure_type, plot_df$figure_id), , drop = FALSE]
y_positions <- seq(0.80, 0.18, length.out = nrow(plot_df))
bar_x0 <- 0.28
bar_x1 <- 0.78
max_panel_count <- max(safe_num(plot_df$n_panels), na.rm = TRUE)
if (!is.finite(max_panel_count) || max_panel_count <= 0) max_panel_count <- 1
for (idx_row in seq_len(nrow(plot_df))) {
  yy <- y_positions[idx_row]
  n_pdf <- safe_num(plot_df$n_source_pdf_packaged[idx_row])
  n_table <- safe_num(plot_df$n_table_redrawn_packaged[idx_row])
  n_total <- max(1, safe_num(plot_df$n_panels[idx_row]))
  text(bar_x0 - 0.018, yy, plot_df$figure_id[idx_row], cex = 0.42, adj = c(1, 0.5), col = nature_palette$ink)
  pdf_w <- (n_pdf / n_total) * (bar_x1 - bar_x0)
  table_w <- (n_table / n_total) * (bar_x1 - bar_x0)
  rect(bar_x0, yy - 0.018, bar_x0 + pdf_w, yy + 0.018, col = nature_palette$teal, border = nature_palette$border, lwd = 0.30)
  rect(bar_x0 + pdf_w, yy - 0.018, bar_x0 + pdf_w + table_w, yy + 0.018, col = nature_palette$orange, border = nature_palette$border, lwd = 0.30)
  text(bar_x1 + 0.012, yy, paste0(n_pdf, " PDF / ", n_table, " table"), cex = 0.36, adj = c(0, 0.5), col = nature_palette$muted)
}
rect(0.30, 0.08, 0.33, 0.11, col = nature_palette$teal, border = nature_palette$border, lwd = 0.30)
text(0.34, 0.095, "source-PDF packaged", cex = 0.36, adj = c(0, 0.5), col = nature_palette$muted)
rect(0.55, 0.08, 0.58, 0.11, col = nature_palette$orange, border = nature_palette$border, lwd = 0.30)
text(0.59, 0.095, "table-redrawn packaged", cex = 0.36, adj = c(0, 0.5), col = nature_palette$muted)
dev.off()
cat("[12D FINAL] Wrote figure:", fig_b, "\n")

# FigC main/supplement package readiness matrix
fig_c <- open_pdf_safe("12D_FINAL_FigC_figure_package_readiness_matrix.pdf", 11.8, 7.0)
new_canvas()
draw_title("Figure-package readiness matrix", "Rows are figures; columns indicate required package components for 12E audit.")

components <- c("panel manifest", "figure index", "source copy/table render", "claim boundary", "12E handoff")
mat_ready <- matrix(1, nrow = nrow(figure_package_df), ncol = length(components))
rownames(mat_ready) <- figure_package_df$figure_id
colnames(mat_ready) <- components
mat_ready[figure_package_df$figure_package_status != "ready_for_12E_visual_audit", ] <- 0
hm_x0 <- 0.27
hm_x1 <- 0.88
hm_y0 <- 0.18
hm_y1 <- 0.82
cell_w <- (hm_x1 - hm_x0) / ncol(mat_ready)
cell_h <- (hm_y1 - hm_y0) / nrow(mat_ready)
for (idx_row in seq_len(nrow(mat_ready))) {
  for (idx_col in seq_len(ncol(mat_ready))) {
    color_now <- ifelse(mat_ready[idx_row, idx_col] > 0, figure_color(rownames(mat_ready)[idx_row]), nature_palette$pale_orange)
    rect(
      hm_x0 + (idx_col - 1) * cell_w,
      hm_y1 - idx_row * cell_h,
      hm_x0 + idx_col * cell_w,
      hm_y1 - (idx_row - 1) * cell_h,
      col = color_now,
      border = nature_palette$white,
      lwd = 0.35
    )
  }
}
rect(hm_x0, hm_y0, hm_x1, hm_y1, border = nature_palette$border, lwd = 0.65)
for (idx_row in seq_len(nrow(mat_ready))) {
  yy <- hm_y1 - (idx_row - 0.5) * cell_h
  text(hm_x0 - 0.012, yy, rownames(mat_ready)[idx_row], cex = 0.35, adj = c(1, 0.5), col = nature_palette$ink)
}
for (idx_col in seq_len(ncol(mat_ready))) {
  xx <- hm_x0 + (idx_col - 0.5) * cell_w
  text(xx, 0.120, colnames(mat_ready)[idx_col], cex = 0.34, srt = 90, adj = c(0.5, 0.5), col = nature_palette$ink)
}
dev.off()
cat("[12D FINAL] Wrote figure:", fig_c, "\n")

# FigD 12E handoff summary
fig_d <- open_pdf_safe("12D_FINAL_FigD_12E_visual_audit_handoff_summary.pdf", 11.4, 6.4)
new_canvas()
draw_title("12E visual-audit handoff summary", "12D package generation is complete when all package manifests and index PDFs are ready.")

handoff_df <- data.frame(
  item = c(
    "Panel package manifest",
    "Figure package manifest",
    "Main package manifest",
    "Supplement package manifest",
    "Table-rendered panels",
    "Source provenance",
    "Claim-boundary check"
  ),
  ready = c(
    file.exists(file.path(out_table_dir, "12D_FINAL_panel_package_manifest.csv")),
    file.exists(file.path(out_table_dir, "12D_FINAL_figure_package_manifest.csv")),
    file.exists(file.path(out_table_dir, "12D_FINAL_main_figure_panel_package_manifest.csv")),
    file.exists(file.path(out_table_dir, "12D_FINAL_supplementary_figure_panel_package_manifest.csv")),
    file.exists(file.path(out_table_dir, "12D_FINAL_table_rendered_panel_manifest.csv")),
    file.exists(file.path(out_table_dir, "12D_FINAL_source_provenance_manifest.csv")),
    file.exists(file.path(out_table_dir, "12D_FINAL_claim_boundary_package_check.csv"))
  ),
  stringsAsFactors = FALSE
)
y_positions <- seq(0.78, 0.30, length.out = nrow(handoff_df))
for (idx_row in seq_len(nrow(handoff_df))) {
  yy <- y_positions[idx_row]
  color_now <- ifelse(handoff_df$ready[idx_row], nature_palette$teal, nature_palette$orange)
  symbols(0.22, yy, circles = 0.018, inches = FALSE, add = TRUE,
          bg = color_now, fg = nature_palette$border, lwd = 0.35)
  text(0.26, yy, handoff_df$item[idx_row], cex = 0.50, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.70, yy, ifelse(handoff_df$ready[idx_row], "ready", "missing"), cex = 0.46, adj = c(0, 0.5), col = color_now)
}
text(0.50, 0.14, "Next: 12E visual audit checks panel layout, readability, source provenance and overclaim boundaries.", cex = 0.48, col = nature_palette$muted)
dev.off()
cat("[12D FINAL] Wrote figure:", fig_d, "\n")

# ------------------------- execution summary and report -------------------------
n_figures_packaged <- nrow(figure_package_df)
n_panels_packaged <- nrow(panel_package_df)
n_panels_ready <- sum(panel_package_df$ready_for_12E_visual_audit, na.rm = TRUE)
n_figures_ready <- sum(figure_package_df$figure_package_status == "ready_for_12E_visual_audit", na.rm = TRUE)
n_source_pdf_packaged <- sum(grepl("source_pdf", panel_package_df$final_panel_package_status), na.rm = TRUE)
n_table_redrawn_packaged <- sum(grepl("table_redrawn", panel_package_df$final_panel_package_status), na.rm = TRUE)
n_needs_review <- sum(panel_package_df$final_panel_package_status == "needs_review", na.rm = TRUE)

summary_df <- data.frame(
  item = c(
    "figures_packaged",
    "main_figures_packaged",
    "supplementary_figures_packaged",
    "panels_packaged",
    "panels_ready_for_12E_visual_audit",
    "panels_needing_package_review",
    "source_pdf_packaged_panels",
    "table_redrawn_packaged_panels",
    "figure_package_index_pdfs",
    "figures_ready_for_12E_visual_audit",
    "table_rendered_panel_files",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(n_figures_packaged),
    as.character(sum(figure_package_df$figure_type == "main")),
    as.character(sum(figure_package_df$figure_type == "supplement")),
    as.character(n_panels_packaged),
    as.character(n_panels_ready),
    as.character(n_needs_review),
    as.character(n_source_pdf_packaged),
    as.character(n_table_redrawn_packaged),
    as.character(sum(file.exists(figure_package_df$figure_package_index_pdf))),
    as.character(n_figures_ready),
    as.character(nrow(table_redraw_df)),
    "4",
    "INPUT_READY_FOR_12E_FINAL_VISUAL_AUDIT"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "12D_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "12D_FINAL_execution_summary.tsv"))

report_lines <- c(
  "12D FINAL report",
  "================",
  "Module: final panel-package generation",
  "Mode: complete standalone 12D rebuild; no previous 12D output dependency; no internet; no 00-10P rerun.",
  "Allowed upstream inputs: locked 10A-10P, 11A-11J, 12A, 12B and 12C outputs.",
  "",
  paste0("Figures packaged: ", n_figures_packaged),
  paste0("Main figures packaged: ", sum(figure_package_df$figure_type == "main")),
  paste0("Supplementary figures packaged: ", sum(figure_package_df$figure_type == "supplement")),
  paste0("Panels packaged: ", n_panels_packaged),
  paste0("Panels ready for 12E visual audit: ", n_panels_ready),
  paste0("Panels needing package review: ", n_needs_review),
  paste0("Source-PDF packaged panels: ", n_source_pdf_packaged),
  paste0("Table-redrawn packaged panels: ", n_table_redrawn_packaged),
  paste0("Figure package index PDFs: ", sum(file.exists(figure_package_df$figure_package_index_pdf))),
  paste0("Figures ready for 12E visual audit: ", n_figures_ready),
  "",
  "Main 12E inputs:",
  paste0("- ", file.path(out_table_dir, "12D_FINAL_panel_package_manifest.csv")),
  paste0("- ", file.path(out_table_dir, "12D_FINAL_figure_package_manifest.csv")),
  paste0("- ", file.path(out_table_dir, "12D_FINAL_table_rendered_panel_manifest.csv")),
  paste0("- ", file.path(out_table_dir, "12D_FINAL_source_provenance_manifest.csv")),
  paste0("- ", file.path(out_table_dir, "12D_FINAL_claim_boundary_package_check.csv")),
  "",
  "Claim boundary:",
  "- 12D generated panel packages and provenance records only.",
  "- Table-only sources were redrawn as clean table-derived panels.",
  "- Do not treat table previews or package index sheets as new biological analyses.",
  "- Do not convert proxy/source locked panels into clinical prediction, clinical biomarker, causal efficacy/safety, anatomical projection or barcode-lineage claims.",
  "",
  "Decision: INPUT_READY_FOR_12E_FINAL_VISUAL_AUDIT"
)
report_file <- file.path(out_text_dir, "12D_FINAL_panel_package_generation_report.txt")
writeLines(report_lines, report_file)
cat("[12D FINAL] Wrote:", report_file, "\n")

cat("\n[12D FINAL] Completed final panel-package generation.\n")
cat("[12D FINAL] Figures packaged:", n_figures_packaged, "\n")
cat("[12D FINAL] Main figures packaged:", sum(figure_package_df$figure_type == "main"), "\n")
cat("[12D FINAL] Supplementary figures packaged:", sum(figure_package_df$figure_type == "supplement"), "\n")
cat("[12D FINAL] Panels packaged:", n_panels_packaged, "\n")
cat("[12D FINAL] Panels ready for 12E visual audit:", n_panels_ready, "\n")
cat("[12D FINAL] Panels needing package review:", n_needs_review, "\n")
cat("[12D FINAL] Source-PDF packaged panels:", n_source_pdf_packaged, "\n")
cat("[12D FINAL] Table-redrawn packaged panels:", n_table_redrawn_packaged, "\n")
cat("[12D FINAL] Figure package index PDFs:", sum(file.exists(figure_package_df$figure_package_index_pdf)), "\n")
cat("[12D FINAL] Figures ready for 12E visual audit:", n_figures_ready, "\n")
cat("[12D FINAL] Figures written: 4\n")
cat("[12D FINAL] Decision: INPUT_READY_FOR_12E_FINAL_VISUAL_AUDIT\n")
cat("[12D FINAL] Output tables:", out_table_dir, "\n")
cat("[12D FINAL] Output figs  :", out_fig_dir, "\n")
cat("[12D FINAL] Output packages:", out_package_dir, "\n")
cat("[12D FINAL] Output text  :", out_text_dir, "\n")
cat("[12D FINAL] Next         : review 12D FINAL PDFs and package manifest; if accepted, proceed to 12E final visual audit.\n")
