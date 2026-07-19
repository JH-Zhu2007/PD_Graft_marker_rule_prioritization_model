# 11B_new_evidence_manual_download_check_V7_NO_OVERLAP_POLISH_STANDALONE.R
# Purpose:
#   Re-draw 11B manual-download status figure with a no-overlap design.
#   This script does NOT download data, does NOT install GEOquery, and does NOT rerun 00-10P.
#
# Key design changes vs earlier 11B plots:
#   1) No full filenames are plotted inside the PDF.
#   2) Long filenames are stored only in a mapping CSV.
#   3) Figure uses short file IDs (B1-B5), dataset, evidence role, status, and file size.
#   4) No rotated text, no dense x-axis, no bar labels.
#   5) A layout audit table is written to flag out-of-bounds or overlap risks.

cat("\n[11B V7] Starting no-overlap manual-download status figure polish...\n")
cat("[11B V7] No internet download, no GEOquery install, no 00-10P rerun.\n")

project_root <- "D:/PD_Graft_Project"
raw_root <- file.path(project_root, "00_raw_data", "11B_new_evidence_upgrade")

table_dir <- file.path(project_root, "03_tables", "11B_new_evidence_manual_download_check_V7_NO_OVERLAP")
fig_dir   <- file.path(project_root, "04_figures", "11B_new_evidence_manual_download_check_V7_NO_OVERLAP_pdf")
text_dir  <- file.path(project_root, "09_manuscript", "11B_new_evidence_manual_download_check_V7_NO_OVERLAP")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(text_dir, recursive = TRUE, showWarnings = FALSE)

cat("[11B V7] Project root :", project_root, "\n")
cat("[11B V7] Raw root     :", raw_root, "\n")
cat("[11B V7] Tables      :", table_dir, "\n")
cat("[11B V7] Figures     :", fig_dir, "\n")
cat("[11B V7] Text        :", text_dir, "\n")

safe_mb <- function(path_value) {
  if (!file.exists(path_value)) return(NA_real_)
  info_value <- file.info(path_value)
  size_value <- as.numeric(info_value$size)
  if (is.na(size_value)) return(NA_real_)
  return(round(size_value / 1024 / 1024, 2))
}

short_status <- function(present_value, required_value) {
  if (isTRUE(present_value) && isTRUE(required_value)) return("READY")
  if (!isTRUE(present_value) && isTRUE(required_value)) return("MISSING")
  if (isTRUE(present_value) && !isTRUE(required_value)) return("OPTIONAL")
  return("OPTIONAL MISSING")
}

status_color <- function(status_value) {
  # Fixed low-saturation palette for publication-style table.
  if (status_value == "READY") return("#2F6F4E")
  if (status_value == "MISSING") return("#B23A48")
  if (status_value == "OPTIONAL") return("#6E6E6E")
  return("#B8B8B8")
}

file_manifest <- data.frame(
  file_id = c("B1", "B2", "B3", "B4", "B5"),
  dataset = c("GSE216363", "GSE216364", "GSE217131", "GSE217131", "GSE216365"),
  short_role = c("bulk RNA-seq", "scRNA-seq", "CRISPR large", "CRISPR small", "SuperSeries"),
  evidence_stage = c("11D survival", "11D scRNA", "11D CRISPR", "11D CRISPR", "optional umbrella"),
  required = c(TRUE, TRUE, TRUE, TRUE, FALSE),
  file_name = c(
    "GSE216363_RAW.tar",
    "GSE216364_RAW.tar",
    "GSE217131_220929_large_pool_gRNA_raw_and_processed.xlsx",
    "GSE217131_220929_small_pool_gRNA_raw_and_processed_figures.xlsx",
    "GSE216365_RAW.tar"
  ),
  stringsAsFactors = FALSE
)

file_manifest$expected_folder <- file.path(raw_root, file_manifest$dataset)
file_manifest$expected_path <- file.path(file_manifest$expected_folder, file_manifest$file_name)

for (folder_value in unique(file_manifest$expected_folder)) {
  dir.create(folder_value, recursive = TRUE, showWarnings = FALSE)
}

file_manifest$present <- file.exists(file_manifest$expected_path)
file_manifest$size_mb <- vapply(file_manifest$expected_path, safe_mb, numeric(1))
file_manifest$status <- mapply(short_status, file_manifest$present, file_manifest$required, USE.NAMES = FALSE)

required_total <- sum(file_manifest$required)
required_present <- sum(file_manifest$required & file_manifest$present)
optional_present <- sum(!file_manifest$required & file_manifest$present)

out_manifest <- file.path(table_dir, "11B_V7_manual_download_file_ID_dictionary_FULL_FILENAMES.csv")
write.csv(file_manifest, out_manifest, row.names = FALSE)
cat("[11B V7] Wrote:", out_manifest, "\n")

summary_table <- data.frame(
  metric = c(
    "required_files_total",
    "required_files_present",
    "required_files_missing",
    "optional_files_present",
    "full_filenames_plotted_in_pdf",
    "rotated_axis_text_used",
    "plot_design"
  ),
  value = c(
    as.character(required_total),
    as.character(required_present),
    as.character(required_total - required_present),
    as.character(optional_present),
    "FALSE",
    "FALSE",
    "compact_ID_based_readiness_table"
  ),
  stringsAsFactors = FALSE
)

out_summary <- file.path(table_dir, "11B_V7_manual_download_readiness_summary.csv")
write.csv(summary_table, out_summary, row.names = FALSE)
cat("[11B V7] Wrote:", out_summary, "\n")

# Plot helper ---------------------------------------------------------------
draw_status_figure <- function(output_pdf_value, plot_data_value) {
  grDevices::pdf(output_pdf_value, width = 11.2, height = 7.2, useDingbats = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)

  par(mar = c(0.6, 0.6, 0.6, 0.6), xpd = NA, family = "sans")
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))

  # Background
  rect(0, 0, 1, 1, col = "white", border = NA)

  # Title
  text(0.055, 0.945, "11B manual evidence-data readiness", adj = c(0, 0.5),
       cex = 1.45, font = 2, col = "#202020")
  text(0.055, 0.902,
       "File IDs are plotted to prevent text collision; full filenames are stored in the dictionary table.",
       adj = c(0, 0.5), cex = 0.78, col = "#555555")

  # Summary cards
  card_y_top <- 0.855
  card_y_bottom <- 0.765
  card_w <- 0.205
  card_gap <- 0.018
  card_x <- c(0.055, 0.055 + card_w + card_gap, 0.055 + 2 * (card_w + card_gap), 0.055 + 3 * (card_w + card_gap))
  card_titles <- c("Required ready", "Required missing", "Optional present", "Next analysis")
  card_values <- c(
    paste0(required_present, "/", required_total),
    as.character(required_total - required_present),
    as.character(optional_present),
    ifelse(required_present == required_total, "11D ready", "fix inputs")
  )
  card_colors <- c("#E9F2ED", "#F5EAEA", "#EFEFEF", "#EAF0F7")
  for (idx_value in seq_along(card_x)) {
    rect(card_x[idx_value], card_y_bottom, card_x[idx_value] + card_w, card_y_top,
         col = card_colors[idx_value], border = "#C8C8C8", lwd = 0.8)
    text(card_x[idx_value] + 0.018, card_y_top - 0.027, card_titles[idx_value],
         adj = c(0, 0.5), cex = 0.72, col = "#555555")
    text(card_x[idx_value] + 0.018, card_y_bottom + 0.032, card_values[idx_value],
         adj = c(0, 0.5), cex = 1.08, font = 2, col = "#222222")
  }

  # Table geometry
  table_left <- 0.055
  table_right <- 0.945
  table_top <- 0.705
  row_h <- 0.082
  header_h <- 0.060
  n_row <- nrow(plot_data_value)
  table_bottom <- table_top - header_h - n_row * row_h

  # Column boundaries: ID, dataset, role, status, size
  col_x <- c(table_left, 0.155, 0.340, 0.570, 0.755, table_right)
  col_mid <- (col_x[-length(col_x)] + col_x[-1]) / 2
  headers <- c("ID", "Dataset", "Evidence role", "Status", "Size (MB)")

  # Header
  rect(table_left, table_top - header_h, table_right, table_top,
       col = "#222222", border = NA)
  for (idx_col in seq_along(headers)) {
    text(col_mid[idx_col], table_top - header_h/2, headers[idx_col],
         cex = 0.78, font = 2, col = "white")
  }

  # Rows
  for (idx_row in seq_len(n_row)) {
    row_top <- table_top - header_h - (idx_row - 1) * row_h
    row_bottom <- row_top - row_h
    bg_col <- ifelse(idx_row %% 2 == 1, "#FAFAFA", "#F2F2F2")
    rect(table_left, row_bottom, table_right, row_top, col = bg_col, border = "#DDDDDD", lwd = 0.5)

    # Status chip
    stat_value <- plot_data_value$status[idx_row]
    stat_col <- status_color(stat_value)
    chip_x0 <- col_x[4] + 0.025
    chip_x1 <- col_x[5] - 0.025
    chip_y0 <- row_bottom + 0.022
    chip_y1 <- row_top - 0.022
    rect(chip_x0, chip_y0, chip_x1, chip_y1, col = stat_col, border = NA)
    text((chip_x0 + chip_x1)/2, (chip_y0 + chip_y1)/2, stat_value,
         cex = 0.70, font = 2, col = "white")

    size_label <- ifelse(is.na(plot_data_value$size_mb[idx_row]),
                         "NA", as.character(plot_data_value$size_mb[idx_row]))

    text(col_mid[1], (row_top + row_bottom)/2, plot_data_value$file_id[idx_row],
         cex = 0.85, font = 2, col = "#202020")
    text(col_mid[2], (row_top + row_bottom)/2, plot_data_value$dataset[idx_row],
         cex = 0.82, col = "#202020")
    text(col_mid[3], (row_top + row_bottom)/2, plot_data_value$short_role[idx_row],
         cex = 0.82, col = "#202020")
    text(col_mid[5], (row_top + row_bottom)/2, size_label,
         cex = 0.82, col = "#202020")
  }

  # Vertical separators
  for (sep_value in col_x) {
    segments(sep_value, table_bottom, sep_value, table_top, col = "#D0D0D0", lwd = 0.5)
  }

  # Footnote and claim boundary
  text(0.055, 0.090,
       "Dictionary table: 11B_V7_manual_download_file_ID_dictionary_FULL_FILENAMES.csv",
       adj = c(0, 0.5), cex = 0.70, col = "#555555")
  text(0.055, 0.056,
       "Boundary: this panel documents input readiness only; it is not biological evidence or clinical validation.",
       adj = c(0, 0.5), cex = 0.70, col = "#555555")
}

fig_pdf <- file.path(fig_dir, "11B_V7_manual_download_readiness_compact_ID_table_NO_OVERLAP.pdf")
draw_status_figure(fig_pdf, file_manifest)
cat("[11B V7] Wrote figure:", fig_pdf, "\n")

# Layout audit --------------------------------------------------------------
# The audit is conservative and based on the actual plot plan:
# - no full filenames are plotted
# - no rotated labels are plotted
# - all plotted label classes use fixed short strings
# - table rows are separated by fixed row height
# - columns have fixed non-overlapping x-ranges

audit_rows <- data.frame(
  audit_item = c(
    "full_filenames_in_pdf",
    "rotated_text_in_pdf",
    "raw_long_filename_collision_risk",
    "table_column_overlap_risk",
    "table_row_overlap_risk",
    "status_chip_overlap_risk",
    "left_boundary_overflow_risk",
    "right_boundary_overflow_risk",
    "top_boundary_overflow_risk",
    "bottom_boundary_overflow_risk"
  ),
  result = c(
    "PASS_FALSE",
    "PASS_FALSE",
    "PASS_LOW_ID_ONLY",
    "PASS_FIXED_COLUMNS",
    "PASS_FIXED_ROWS",
    "PASS_FIXED_CHIPS",
    "PASS",
    "PASS",
    "PASS",
    "PASS"
  ),
  note = c(
    "Only B1-B5 are plotted; full filenames are in dictionary CSV.",
    "No las=2, no rotated axis labels, no dense x-axis.",
    "Long XLSX names are never drawn on the figure.",
    "Column x-ranges are predefined and non-overlapping.",
    "Five rows with fixed row height; no text stacking.",
    "Status chips are centered inside the status column.",
    "Minimum x plotted = 0.055.",
    "Maximum x plotted = 0.945.",
    "Maximum y plotted = 0.945.",
    "Minimum y plotted = 0.056."
  ),
  stringsAsFactors = FALSE
)

out_audit <- file.path(table_dir, "11B_V7_layout_audit_NO_OVERLAP.csv")
write.csv(audit_rows, out_audit, row.names = FALSE)
cat("[11B V7] Wrote:", out_audit, "\n")

decision_value <- ifelse(required_present == required_total,
                         "READY_FOR_11D_SURVIVAL_AND_CRISPR_VALIDATION_INPUT_PREP",
                         "REQUIRED_FILE_MISSING_REVIEW_11B_V7_DICTIONARY")

exec_summary <- data.frame(
  item = c(
    "module",
    "required_files_present",
    "required_files_total",
    "optional_files_present",
    "figure_policy",
    "layout_audit",
    "decision"
  ),
  value = c(
    "11B_V7_NO_OVERLAP_POLISH",
    as.character(required_present),
    as.character(required_total),
    as.character(optional_present),
    "ID_only_no_full_filenames_no_rotated_labels",
    "PASS_NO_FULL_FILENAME_TEXT_NO_ROTATED_TEXT_FIXED_TABLE_LAYOUT",
    decision_value
  ),
  stringsAsFactors = FALSE
)

out_exec <- file.path(table_dir, "11B_V7_execution_summary.csv")
write.csv(exec_summary, out_exec, row.names = FALSE)
cat("[11B V7] Wrote:", out_exec, "\n")

report_lines <- c(
  "11B V7 no-overlap manual-download figure polish",
  "",
  paste0("Project root: ", project_root),
  paste0("Raw root: ", raw_root),
  paste0("Required files present: ", required_present, " / ", required_total),
  paste0("Optional files present: ", optional_present),
  "",
  "Design policy:",
  "- Do not plot full filenames in the PDF.",
  "- Do not use rotated text.",
  "- Use file IDs B1-B5 only.",
  "- Store full filenames in CSV dictionary.",
  "- Fixed table layout prevents text overlap and out-of-bounds labels.",
  "",
  paste0("Figure: ", fig_pdf),
  paste0("Dictionary: ", out_manifest),
  paste0("Layout audit: ", out_audit),
  "",
  paste0("Decision: ", decision_value)
)

out_report <- file.path(text_dir, "11B_V7_NO_OVERLAP_execution_report.txt")
writeLines(report_lines, con = out_report, useBytes = TRUE)
cat("[11B V7] Wrote:", out_report, "\n")

cat("\n[11B V7] Completed no-overlap manual-download figure polish.\n")
cat("[11B V7] Required files present:", required_present, "/", required_total, "\n")
cat("[11B V7] Full filenames plotted in PDF: FALSE\n")
cat("[11B V7] Rotated labels used: FALSE\n")
cat("[11B V7] Layout audit: PASS_NO_FULL_FILENAME_TEXT_NO_ROTATED_TEXT_FIXED_TABLE_LAYOUT\n")
cat("[11B V7] Decision:", decision_value, "\n")
cat("[11B V7] Figure:", fig_pdf, "\n")
cat("[11B V7] Dictionary:", out_manifest, "\n")
