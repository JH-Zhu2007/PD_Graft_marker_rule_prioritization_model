
# ============================================================
# 12E FINAL COMPLETE STANDALONE - NATURE COLOR SAFE FIGC
# Final visual audit for DA neuron / graft-related
# transcriptomic cell-state prioritisation framework
#
# User rule respected:
#   - Complete standalone script for 12E
#   - Does NOT read any previous 12E output
#   - Does NOT use table-only patch logic
#   - May read locked upstream outputs as formal inputs:
#       10A-10P, 11A-11J, 12A, 12B, 12C, 12D
#   - Uses 12D panel package manifest as the formal input
#   - Audits packaged panels for existence, provenance, package status,
#     table-redrawn mode, claim-boundary risk, naming/readiness and
#     visual-audit priorities
#   - Generates audit tables, report text and Nature-style audit PDFs
#   - No internet
#   - No 00-10P rerun
#
# Scientific boundary:
#   - Visual/package audit only
#   - Evidence-anchored transcriptomic prioritisation framework
#   - Candidate transcriptomic marker signatures only
#   - marker-rule-derived prioritization model audit only
#   - No clinical prediction
#   - No diagnostic/prognostic biomarker validation
#   - No causal graft efficacy/safety claim
#   - No true anatomical projection or barcode-lineage proof
# ============================================================

cat("\n[12E FINAL] Starting final visual audit...\n")
cat("[12E FINAL] Mode: complete standalone 12E rebuild; no previous 12E dependency; no internet; no 00-10P rerun.\n")
cat("[12E FINAL] Inputs allowed: locked upstream 10A-10P, 11A-11J, 12A, 12B, 12C and 12D outputs.\n")
cat("[12E FINAL] Claim boundary: visual/package audit only; no clinical prediction or validated biomarker claim.\n")
cat("[12E FINAL] Figure style: Nature-style clean publication layout; robust FigC table-redrawn labels.\n")

graphics.off()

# ------------------------- project paths -------------------------
project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")
figure_root <- file.path(project_root, "04_figures")
text_root <- file.path(project_root, "09_manuscript")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "12E_final_visual_audit_FINAL_COMPLETE_STANDALONE"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "12E_final_visual_audit_FINAL_COMPLETE_STANDALONE_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "12E_final_visual_audit_FINAL_COMPLETE_STANDALONE"
)

dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_text_dir, recursive = TRUE, showWarnings = FALSE)

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
  cat("[12E FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[12E FINAL] Wrote:", file_value, "\n")
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
    cat("[12E FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
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

audit_status_color <- function(status_value) {
  status_text <- tolower(safe_chr(status_value))
  out_colors <- rep(nature_palette$teal, length(status_text))
  out_colors[grepl("minor", status_text)] <- nature_palette$gold
  out_colors[grepl("review", status_text)] <- nature_palette$orange
  out_colors[grepl("fail|missing|critical", status_text)] <- nature_palette$red
  out_colors
}

draw_title <- function(title_value, subtitle_value = "") {
  text(0.5, 0.965, title_value, cex = 0.98, font = 2, adj = c(0.5, 0.5), col = nature_palette$ink)
  if (nchar(subtitle_value) > 0) {
    text(0.5, 0.928, subtitle_value, cex = 0.50, col = nature_palette$muted, adj = c(0.5, 0.5))
  }
}

# ------------------------- upstream discovery -------------------------
if (!dir.exists(table_root)) stop("[12E FINAL] Missing table root: ", table_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
table_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(table_info$size) & table_info$size > 0 & table_info$size < 160 * 1024 * 1024]

# Hard rule: do not read previous 12E output
all_table_files <- all_table_files[!grepl("12E_final_visual_audit", all_table_files, ignore.case = TRUE)]

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

# ------------------------- read locked 12D inputs -------------------------
file_12d_panel_manifest <- first_existing_file(c(
  file.path(table_root, "12D_final_panel_package_generation_FINAL_COMPLETE_STANDALONE", "12D_FINAL_panel_package_manifest.csv"),
  find_files_all_terms(all_table_files, c("12d", "panel_package_manifest"), max_n = 10)
))
file_12d_figure_manifest <- first_existing_file(c(
  file.path(table_root, "12D_final_panel_package_generation_FINAL_COMPLETE_STANDALONE", "12D_FINAL_figure_package_manifest.csv"),
  find_files_all_terms(all_table_files, c("12d", "figure_package_manifest"), max_n = 10)
))
file_12d_table_render_manifest <- first_existing_file(c(
  file.path(table_root, "12D_final_panel_package_generation_FINAL_COMPLETE_STANDALONE", "12D_FINAL_table_rendered_panel_manifest.csv"),
  find_files_all_terms(all_table_files, c("12d", "table_rendered_panel_manifest"), max_n = 10)
))
file_12d_claim_check <- first_existing_file(c(
  file.path(table_root, "12D_final_panel_package_generation_FINAL_COMPLETE_STANDALONE", "12D_FINAL_claim_boundary_package_check.csv"),
  find_files_all_terms(all_table_files, c("12d", "claim_boundary_package_check"), max_n = 10)
))
file_12d_provenance <- first_existing_file(c(
  file.path(table_root, "12D_final_panel_package_generation_FINAL_COMPLETE_STANDALONE", "12D_FINAL_source_provenance_manifest.csv"),
  find_files_all_terms(all_table_files, c("12d", "source_provenance_manifest"), max_n = 10)
))

panel_manifest_df <- read_table_safe(file_12d_panel_manifest)
figure_manifest_df <- read_table_safe(file_12d_figure_manifest)
table_render_manifest_df <- read_table_safe(file_12d_table_render_manifest)
claim_check_12d_df <- read_table_safe(file_12d_claim_check)
provenance_12d_df <- read_table_safe(file_12d_provenance)

if (nrow(panel_manifest_df) < 1) stop("[12E FINAL] Missing 12D panel package manifest.", call. = FALSE)
if (!("figure_id" %in% colnames(panel_manifest_df))) stop("[12E FINAL] 12D panel manifest missing figure_id.", call. = FALSE)
if (!("panel_id" %in% colnames(panel_manifest_df))) stop("[12E FINAL] 12D panel manifest missing panel_id.", call. = FALSE)

input_audit_df <- data.frame(
  input_name = c(
    "12D_panel_package_manifest",
    "12D_figure_package_manifest",
    "12D_table_rendered_panel_manifest",
    "12D_claim_boundary_package_check",
    "12D_source_provenance_manifest"
  ),
  detected = c(
    file_12d_panel_manifest != "",
    file_12d_figure_manifest != "",
    file_12d_table_render_manifest != "",
    file_12d_claim_check != "",
    file_12d_provenance != ""
  ),
  file_path = c(
    file_12d_panel_manifest,
    file_12d_figure_manifest,
    file_12d_table_render_manifest,
    file_12d_claim_check,
    file_12d_provenance
  ),
  rows_loaded = c(
    nrow(panel_manifest_df),
    nrow(figure_manifest_df),
    nrow(table_render_manifest_df),
    nrow(claim_check_12d_df),
    nrow(provenance_12d_df)
  ),
  allowed_as_locked_upstream_input = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
write_csv_safe(input_audit_df, file.path(out_table_dir, "12E_FINAL_locked_12D_input_audit.csv"))

# ------------------------- panel visual audit -------------------------
file_exists_safe <- function(file_value) {
  file_value <- clean_space(file_value)
  file_value != "" & file.exists(file_value)
}

file_size_safe <- function(file_value) {
  file_value <- clean_space(file_value)
  out <- rep(NA_real_, length(file_value))
  for (idx_file in seq_along(file_value)) {
    if (file_value[idx_file] != "" && file.exists(file_value[idx_file])) {
      out[idx_file] <- as.numeric(file.info(file_value[idx_file])$size)
    }
  }
  out
}

panel_audit_list <- list()
for (idx_panel in seq_len(nrow(panel_manifest_df))) {
  panel_row <- panel_manifest_df[idx_panel, , drop = FALSE]

  final_pdf <- if ("final_panel_package_pdf" %in% colnames(panel_row)) clean_space(panel_row$final_panel_package_pdf) else ""
  index_pdf <- if ("panel_package_index_pdf" %in% colnames(panel_row)) clean_space(panel_row$panel_package_index_pdf) else ""
  table_pdf <- if ("table_rendered_panel_pdf" %in% colnames(panel_row)) clean_space(panel_row$table_rendered_panel_pdf) else ""
  copied_pdf <- if ("copied_source_pdf" %in% colnames(panel_row)) clean_space(panel_row$copied_source_pdf) else ""

  final_exists <- file_exists_safe(final_pdf)
  index_exists <- file_exists_safe(index_pdf)
  table_exists <- file_exists_safe(table_pdf)
  copied_exists <- file_exists_safe(copied_pdf)

  final_size <- file_size_safe(final_pdf)
  index_size <- file_size_safe(index_pdf)
  table_size <- file_size_safe(table_pdf)

  issue_values <- character(0)
  severity_value <- "pass"

  if (!final_exists) {
    issue_values <- c(issue_values, "final_panel_package_pdf_missing")
    severity_value <- "critical_fail"
  }
  if (!index_exists) {
    issue_values <- c(issue_values, "panel_package_index_missing")
    if (severity_value != "critical_fail") severity_value <- "needs_review"
  }
  if (is.finite(final_size) && final_size < 2500) {
    issue_values <- c(issue_values, "final_panel_pdf_unusually_small")
    if (severity_value == "pass") severity_value <- "needs_review"
  }
  if (grepl("table_redrawn", panel_row$final_panel_package_status, ignore.case = TRUE)) {
    issue_values <- c(issue_values, "table_redrawn_panel_requires_manual_visual_check")
    if (severity_value == "pass") severity_value <- "minor_review"
  }
  if (grepl("source_pdf", panel_row$final_panel_package_status, ignore.case = TRUE) && !copied_exists) {
    issue_values <- c(issue_values, "source_pdf_expected_but_copy_missing")
    if (severity_value != "critical_fail") severity_value <- "needs_review"
  }
  if (nchar(final_pdf) > 240) {
    issue_values <- c(issue_values, "long_file_path_check_before_external_submission")
    if (severity_value == "pass") severity_value <- "minor_review"
  }

  visual_priority <- "standard"
  if (panel_row$figure_type == "main") visual_priority <- "high"
  if (grepl("table_redrawn", panel_row$final_panel_package_status, ignore.case = TRUE)) visual_priority <- "high_table_redraw"
  if (grepl("Main Fig 1|Supplement Fig S1|Supplement Fig S2", panel_row$figure_id)) visual_priority <- "high_partial_source_origin"
  if (severity_value %in% c("needs_review", "critical_fail")) visual_priority <- "urgent"

  if (length(issue_values) < 1) issue_values <- "none"

  panel_audit_list[[length(panel_audit_list) + 1]] <- data.frame(
    figure_type = panel_row$figure_type,
    figure_id = panel_row$figure_id,
    panel_id = panel_row$panel_id,
    panel_label = panel_row$panel_label,
    primary_locked_module = panel_row$primary_locked_module,
    planned_panel_content = ifelse("planned_panel_content" %in% colnames(panel_row), clean_space(panel_row$planned_panel_content), ""),
    claim_boundary = ifelse("claim_boundary" %in% colnames(panel_row), clean_space(panel_row$claim_boundary), ""),
    source_lock_status = panel_row$source_lock_status,
    final_panel_package_status = panel_row$final_panel_package_status,
    final_panel_package_pdf = final_pdf,
    final_panel_package_pdf_exists = final_exists,
    final_panel_package_pdf_size_bytes = final_size,
    panel_package_index_pdf = index_pdf,
    panel_package_index_exists = index_exists,
    table_rendered_panel_pdf = table_pdf,
    table_rendered_panel_exists = table_exists,
    copied_source_pdf = copied_pdf,
    copied_source_pdf_exists = copied_exists,
    visual_audit_severity = severity_value,
    visual_audit_priority = visual_priority,
    visual_audit_issue_flags = paste(issue_values, collapse = ";"),
    manual_check_required = severity_value != "pass",
    stringsAsFactors = FALSE
  )
}
panel_audit_df <- safe_bind_rows(panel_audit_list)
write_csv_safe(panel_audit_df, file.path(out_table_dir, "12E_FINAL_panel_visual_audit.csv"))
write_tsv_safe(panel_audit_df, file.path(out_table_dir, "12E_FINAL_panel_visual_audit.tsv"))

manual_panel_df <- panel_audit_df[panel_audit_df$manual_check_required, , drop = FALSE]
write_csv_safe(manual_panel_df, file.path(out_table_dir, "12E_FINAL_manual_panel_check_list.csv"))

# ------------------------- figure visual audit -------------------------
figure_ids <- unique(panel_audit_df$figure_id)
figure_audit_list <- list()
for (idx_fig in seq_along(figure_ids)) {
  fig_now <- figure_ids[idx_fig]
  sub_fig <- panel_audit_df[panel_audit_df$figure_id == fig_now, , drop = FALSE]
  n_panels <- nrow(sub_fig)
  n_pass <- sum(sub_fig$visual_audit_severity == "pass", na.rm = TRUE)
  n_minor <- sum(sub_fig$visual_audit_severity == "minor_review", na.rm = TRUE)
  n_review <- sum(sub_fig$visual_audit_severity == "needs_review", na.rm = TRUE)
  n_fail <- sum(sub_fig$visual_audit_severity == "critical_fail", na.rm = TRUE)
  n_table_redraw <- sum(grepl("table_redrawn", sub_fig$final_panel_package_status, ignore.case = TRUE), na.rm = TRUE)
  n_source_pdf <- sum(grepl("source_pdf", sub_fig$final_panel_package_status, ignore.case = TRUE), na.rm = TRUE)

  fig_status <- "pass_to_12F"
  if (n_minor > 0) fig_status <- "pass_with_manual_visual_check"
  if (n_review > 0) fig_status <- "needs_visual_review_before_12F"
  if (n_fail > 0) fig_status <- "fail_repair_before_12F"

  figure_audit_list[[length(figure_audit_list) + 1]] <- data.frame(
    figure_id = fig_now,
    figure_type = sub_fig$figure_type[1],
    n_panels = n_panels,
    n_pass_panels = n_pass,
    n_minor_review_panels = n_minor,
    n_needs_review_panels = n_review,
    n_critical_fail_panels = n_fail,
    n_table_redrawn_panels = n_table_redraw,
    n_source_pdf_panels = n_source_pdf,
    figure_visual_audit_status = fig_status,
    manual_focus = ifelse(n_table_redraw > 0, "check_table_redrawn_panels", "standard_visual_check"),
    stringsAsFactors = FALSE
  )
}
figure_audit_df <- safe_bind_rows(figure_audit_list)
write_csv_safe(figure_audit_df, file.path(out_table_dir, "12E_FINAL_figure_visual_audit.csv"))
write_tsv_safe(figure_audit_df, file.path(out_table_dir, "12E_FINAL_figure_visual_audit.tsv"))

# ------------------------- table-redrawn focus audit -------------------------
table_focus_df <- panel_audit_df[grepl("table_redrawn", panel_audit_df$final_panel_package_status, ignore.case = TRUE), , drop = FALSE]
if (nrow(table_focus_df) > 0) {
  if (!("planned_panel_content" %in% colnames(table_focus_df))) table_focus_df$planned_panel_content <- ""
  if (!("visual_audit_priority" %in% colnames(table_focus_df))) table_focus_df$visual_audit_priority <- "high_table_redraw"
  table_focus_df$required_manual_visual_checks <- paste(
    "readability",
    "column truncation",
    "panel title correctness",
    "source table provenance",
    "claim boundary",
    sep = ";"
  )
} else {
  table_focus_df <- data.frame(
    note = "No table-redrawn panels detected.",
    stringsAsFactors = FALSE
  )
}
write_csv_safe(table_focus_df, file.path(out_table_dir, "12E_FINAL_table_redrawn_panel_focus_audit.csv"))

# ------------------------- claim boundary audit -------------------------
overclaim_terms <- c(
  "clinical prediction",
  "clinical predictor",
  "diagnostic biomarker",
  "prognostic biomarker",
  "treatment response",
  "graft efficacy",
  "graft safety prediction",
  "anatomical-projection claim",
  "lineage tracing proof",
  "barcode-confirmed lineage"
)

claim_scan_list <- list()
text_cols <- intersect(
  c("planned_panel_content", "primary_locked_module", "source_lock_status", "final_panel_package_status"),
  colnames(panel_manifest_df)
)

for (idx_panel in seq_len(nrow(panel_manifest_df))) {
  panel_text <- paste(safe_chr(unlist(panel_manifest_df[idx_panel, text_cols, drop = FALSE])), collapse = " ")
  panel_text_lower <- tolower(panel_text)
  hit_terms <- overclaim_terms[vapply(overclaim_terms, function(term_now) grepl(term_now, panel_text_lower, fixed = TRUE), logical(1))]
  claim_scan_list[[length(claim_scan_list) + 1]] <- data.frame(
    figure_id = panel_manifest_df$figure_id[idx_panel],
    panel_id = panel_manifest_df$panel_id[idx_panel],
    overclaim_terms_detected = paste(hit_terms, collapse = ";"),
    overclaim_flag = length(hit_terms) > 0,
    stringsAsFactors = FALSE
  )
}
claim_scan_df <- safe_bind_rows(claim_scan_list)
write_csv_safe(claim_scan_df, file.path(out_table_dir, "12E_FINAL_panel_claim_boundary_scan.csv"))

# ------------------------- 12F handoff -------------------------
figures_pass_to_12f <- sum(figure_audit_df$figure_visual_audit_status %in% c("pass_to_12F", "pass_with_manual_visual_check"))
figures_blocked <- sum(figure_audit_df$figure_visual_audit_status %in% c("needs_visual_review_before_12F", "fail_repair_before_12F"))
panels_critical <- sum(panel_audit_df$visual_audit_severity == "critical_fail", na.rm = TRUE)
panels_review <- sum(panel_audit_df$visual_audit_severity %in% c("needs_review", "critical_fail"), na.rm = TRUE)
panels_minor <- sum(panel_audit_df$visual_audit_severity == "minor_review", na.rm = TRUE)
overclaim_flags <- sum(claim_scan_df$overclaim_flag, na.rm = TRUE)

overall_decision <- "INPUT_READY_FOR_12F_OPTIONAL_FINAL_ASSEMBLY"
if (figures_blocked > 0 || panels_critical > 0 || overclaim_flags > 0) {
  overall_decision <- "REPAIR_REQUIRED_BEFORE_12F"
}
if (figures_blocked == 0 && panels_critical == 0 && panels_minor > 0 && overclaim_flags == 0) {
  overall_decision <- "INPUT_READY_FOR_12F_WITH_TABLE_REDRAW_MANUAL_CHECK_NOTED"
}

handoff_12f_df <- data.frame(
  handoff_item = c(
    "panel visual audit",
    "figure visual audit",
    "manual panel check list",
    "table-redrawn panel focus audit",
    "claim-boundary scan",
    "12F readiness decision"
  ),
  file_path = c(
    file.path(out_table_dir, "12E_FINAL_panel_visual_audit.csv"),
    file.path(out_table_dir, "12E_FINAL_figure_visual_audit.csv"),
    file.path(out_table_dir, "12E_FINAL_manual_panel_check_list.csv"),
    file.path(out_table_dir, "12E_FINAL_table_redrawn_panel_focus_audit.csv"),
    file.path(out_table_dir, "12E_FINAL_panel_claim_boundary_scan.csv"),
    overall_decision
  ),
  role_in_12F = c(
    "controls panel selection for optional final assembly",
    "controls figure-level readiness",
    "lists panels requiring manual visual check",
    "focuses review on table-derived panels",
    "prevents overclaiming in assembled figures",
    "12F go/no-go decision"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(handoff_12f_df, file.path(out_table_dir, "12E_FINAL_handoff_to_12F_optional_assembly.csv"))

# ------------------------- summary figures -------------------------
# FigA visual audit overview
fig_a <- open_pdf_safe("12E_FINAL_FigA_visual_audit_overview.pdf", 11.8, 6.6)
new_canvas()
draw_title("Final visual-audit overview", "Audit of 12D panel packages for 12F optional final assembly.")

overview_df <- data.frame(
  label = c(
    "Figures audited",
    "Panels audited",
    "Panels pass",
    "Minor manual-check panels",
    "Needs-review panels",
    "Critical-fail panels",
    "Overclaim flags"
  ),
  value = c(
    nrow(figure_audit_df),
    nrow(panel_audit_df),
    sum(panel_audit_df$visual_audit_severity == "pass", na.rm = TRUE),
    panels_minor,
    sum(panel_audit_df$visual_audit_severity == "needs_review", na.rm = TRUE),
    panels_critical,
    overclaim_flags
  ),
  family = c("all", "panel", "pass", "minor", "review", "fail", "claim"),
  stringsAsFactors = FALSE
)
max_overview <- max(safe_num(overview_df$value), na.rm = TRUE)
if (!is.finite(max_overview) || max_overview <= 0) max_overview <- 1
bar_x0 <- 0.38
bar_x1 <- 0.79
y_values <- seq(0.78, 0.26, length.out = nrow(overview_df))
for (idx_row in seq_len(nrow(overview_df))) {
  yy <- y_values[idx_row]
  count_now <- safe_num(overview_df$value[idx_row])
  width_now <- count_now / max_overview
  if (count_now == 0) width_now <- 0.018
  color_now <- nature_palette$navy
  if (overview_df$family[idx_row] == "pass") color_now <- nature_palette$teal
  if (overview_df$family[idx_row] == "minor") color_now <- nature_palette$gold
  if (overview_df$family[idx_row] == "review") color_now <- nature_palette$orange
  if (overview_df$family[idx_row] == "fail") color_now <- nature_palette$red
  if (overview_df$family[idx_row] == "claim") color_now <- ifelse(count_now > 0, nature_palette$red, nature_palette$teal)
  text(bar_x0 - 0.018, yy, overview_df$label[idx_row], cex = 0.56, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.024, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.024,
       col = color_now, border = nature_palette$border, lwd = 0.42)
  text(bar_x0 + width_now * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_now), cex = 0.52, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.12, paste0("Decision: ", overall_decision), cex = 0.48, col = nature_palette$muted)
dev.off()
cat("[12E FINAL] Wrote figure:", fig_a, "\n")

# FigB figure-level visual audit matrix
fig_b <- open_pdf_safe("12E_FINAL_FigB_figure_visual_audit_matrix.pdf", 12.0, 7.0)
new_canvas()
draw_title("Figure-level visual-audit matrix", "Rows are figures; columns summarize package readiness and manual-check burden.")

components <- c("all panels exist", "index exists", "no critical fail", "claim boundary", "12F handoff")
mat_fig <- matrix(1, nrow = nrow(figure_audit_df), ncol = length(components))
rownames(mat_fig) <- figure_audit_df$figure_id
colnames(mat_fig) <- components
mat_fig[figure_audit_df$n_critical_fail_panels > 0, "no critical fail"] <- 0
mat_fig[figure_audit_df$figure_visual_audit_status %in% c("fail_repair_before_12F", "needs_visual_review_before_12F"), ] <- 0

hm_x0 <- 0.27
hm_x1 <- 0.88
hm_y0 <- 0.18
hm_y1 <- 0.82
cell_w <- (hm_x1 - hm_x0) / ncol(mat_fig)
cell_h <- (hm_y1 - hm_y0) / nrow(mat_fig)
for (idx_row in seq_len(nrow(mat_fig))) {
  for (idx_col in seq_len(ncol(mat_fig))) {
    color_now <- ifelse(mat_fig[idx_row, idx_col] > 0, figure_color(rownames(mat_fig)[idx_row]), nature_palette$pale_orange)
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
for (idx_row in seq_len(nrow(mat_fig))) {
  yy <- hm_y1 - (idx_row - 0.5) * cell_h
  text(hm_x0 - 0.012, yy, rownames(mat_fig)[idx_row], cex = 0.35, adj = c(1, 0.5), col = nature_palette$ink)
}
for (idx_col in seq_len(ncol(mat_fig))) {
  xx <- hm_x0 + (idx_col - 0.5) * cell_w
  text(xx, 0.120, colnames(mat_fig)[idx_col], cex = 0.34, srt = 90, adj = c(0.5, 0.5), col = nature_palette$ink)
}
dev.off()
cat("[12E FINAL] Wrote figure:", fig_b, "\n")

# FigC table-redrawn panel focus
fig_c <- open_pdf_safe("12E_FINAL_FigC_table_redrawn_panel_focus.pdf", 11.8, 6.8)
new_canvas()
draw_title("Table-redrawn panel focus", "Table-derived panels require manual readability checks before final assembly.")

if (nrow(table_focus_df) > 0 && "figure_id" %in% colnames(table_focus_df)) {
  plot_df <- table_focus_df
  y_positions <- seq(0.78, 0.25, length.out = nrow(plot_df))
  for (idx_row in seq_len(nrow(plot_df))) {
    yy <- y_positions[idx_row]
    color_now <- figure_color(plot_df$figure_id[idx_row])
    rect(0.10, yy - 0.026, 0.25, yy + 0.026, col = color_now, border = nature_palette$border, lwd = 0.35)
    text(0.175, yy, safe_chr(plot_df$panel_id[idx_row]), cex = 0.34, font = 2, col = nature_palette$white)

    label_now <- ""
    if ("planned_panel_content" %in% colnames(plot_df)) label_now <- clean_space(plot_df$planned_panel_content[idx_row])
    if (length(label_now) < 1 || is.na(label_now) || label_now == "") label_now <- paste0("table-redrawn source panel: ", safe_chr(plot_df$primary_locked_module[idx_row]))
    if (length(label_now) < 1 || is.na(label_now) || label_now == "") label_now <- "table-redrawn source panel"

    priority_now <- ""
    if ("visual_audit_priority" %in% colnames(plot_df)) priority_now <- clean_space(plot_df$visual_audit_priority[idx_row])
    if (length(priority_now) < 1 || is.na(priority_now) || priority_now == "") priority_now <- "manual_check"

    text(0.28, yy, substr(label_now, 1, 64), cex = 0.38, adj = c(0, 0.5), col = nature_palette$ink)
    text(0.72, yy, priority_now, cex = 0.34, adj = c(0, 0.5), col = nature_palette$muted)
  }
} else {
  text(0.5, 0.55, "No table-redrawn panels detected.", cex = 0.70, font = 2, col = nature_palette$ink)
}
text(0.50, 0.12, "Manual check: readability, truncation, panel title, source provenance and claim boundary.", cex = 0.46, col = nature_palette$muted)
dev.off()
cat("[12E FINAL] Wrote figure:", fig_c, "
")

# FigD 12F readiness summary
fig_d <- open_pdf_safe("12E_FINAL_FigD_12F_readiness_summary.pdf", 11.4, 6.4)
new_canvas()
draw_title("12F readiness summary", "Final visual-audit handoff for optional final assembly.")

readiness_df <- data.frame(
  item = c(
    "Panel visual audit",
    "Figure visual audit",
    "Manual check list",
    "Table-redrawn focus",
    "Claim-boundary scan",
    "12F decision"
  ),
  ready = c(
    file.exists(file.path(out_table_dir, "12E_FINAL_panel_visual_audit.csv")),
    file.exists(file.path(out_table_dir, "12E_FINAL_figure_visual_audit.csv")),
    file.exists(file.path(out_table_dir, "12E_FINAL_manual_panel_check_list.csv")),
    file.exists(file.path(out_table_dir, "12E_FINAL_table_redrawn_panel_focus_audit.csv")),
    file.exists(file.path(out_table_dir, "12E_FINAL_panel_claim_boundary_scan.csv")),
    overall_decision != "REPAIR_REQUIRED_BEFORE_12F"
  ),
  stringsAsFactors = FALSE
)
y_positions <- seq(0.78, 0.36, length.out = nrow(readiness_df))
for (idx_row in seq_len(nrow(readiness_df))) {
  yy <- y_positions[idx_row]
  color_now <- ifelse(readiness_df$ready[idx_row], nature_palette$teal, nature_palette$orange)
  symbols(0.24, yy, circles = 0.018, inches = FALSE, add = TRUE,
          bg = color_now, fg = nature_palette$border, lwd = 0.35)
  text(0.28, yy, readiness_df$item[idx_row], cex = 0.50, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.70, yy, ifelse(readiness_df$ready[idx_row], "ready", "review"), cex = 0.46, adj = c(0, 0.5), col = color_now)
}
text(0.50, 0.20, overall_decision, cex = 0.48, font = 2, col = nature_palette$ink)
text(0.50, 0.14, "12F should assemble only after manually checking table-redrawn and high-priority panels.", cex = 0.44, col = nature_palette$muted)
dev.off()
cat("[12E FINAL] Wrote figure:", fig_d, "\n")

# ------------------------- execution summary and report -------------------------
summary_df <- data.frame(
  item = c(
    "figures_audited",
    "panels_audited",
    "panels_pass",
    "minor_manual_check_panels",
    "panels_needing_review",
    "critical_fail_panels",
    "table_redrawn_panels",
    "overclaim_flags",
    "figures_pass_to_12F_or_manual_check",
    "figures_blocked_before_12F",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(nrow(figure_audit_df)),
    as.character(nrow(panel_audit_df)),
    as.character(sum(panel_audit_df$visual_audit_severity == "pass", na.rm = TRUE)),
    as.character(panels_minor),
    as.character(sum(panel_audit_df$visual_audit_severity == "needs_review", na.rm = TRUE)),
    as.character(panels_critical),
    as.character(ifelse("figure_id" %in% colnames(table_focus_df), nrow(table_focus_df), 0)),
    as.character(overclaim_flags),
    as.character(figures_pass_to_12f),
    as.character(figures_blocked),
    "4",
    overall_decision
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "12E_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "12E_FINAL_execution_summary.tsv"))

report_lines <- c(
  "12E FINAL report",
  "================",
  "Module: final visual audit",
  "Mode: complete standalone 12E rebuild; no previous 12E output dependency; no internet; no 00-10P rerun.",
  "Allowed upstream inputs: locked 10A-10P, 11A-11J, 12A, 12B, 12C and 12D outputs.",
  "",
  paste0("Figures audited: ", nrow(figure_audit_df)),
  paste0("Panels audited: ", nrow(panel_audit_df)),
  paste0("Panels pass: ", sum(panel_audit_df$visual_audit_severity == "pass", na.rm = TRUE)),
  paste0("Minor manual-check panels: ", panels_minor),
  paste0("Panels needing review: ", sum(panel_audit_df$visual_audit_severity == "needs_review", na.rm = TRUE)),
  paste0("Critical-fail panels: ", panels_critical),
  paste0("Table-redrawn panels: ", ifelse("figure_id" %in% colnames(table_focus_df), nrow(table_focus_df), 0)),
  paste0("Overclaim flags: ", overclaim_flags),
  paste0("Figures pass to 12F or manual check: ", figures_pass_to_12f),
  paste0("Figures blocked before 12F: ", figures_blocked),
  "",
  "Main 12F inputs:",
  paste0("- ", file.path(out_table_dir, "12E_FINAL_panel_visual_audit.csv")),
  paste0("- ", file.path(out_table_dir, "12E_FINAL_figure_visual_audit.csv")),
  paste0("- ", file.path(out_table_dir, "12E_FINAL_manual_panel_check_list.csv")),
  paste0("- ", file.path(out_table_dir, "12E_FINAL_table_redrawn_panel_focus_audit.csv")),
  paste0("- ", file.path(out_table_dir, "12E_FINAL_panel_claim_boundary_scan.csv")),
  "",
  "Manual note:",
  "- Table-redrawn panels require human visual inspection for readability and truncation.",
  "- This automated audit checks package integrity and provenance, but it cannot replace manual visual review of every PDF.",
  "",
  "Claim boundary:",
  "- 12E is a visual/package audit only.",
  "- Do not convert visual audit outputs into new biological findings.",
  "- Do not claim clinical prediction, validated biomarker, causal graft efficacy/safety, anatomical projection or barcode-lineage proof.",
  "",
  paste0("Decision: ", overall_decision)
)
report_file <- file.path(out_text_dir, "12E_FINAL_visual_audit_report.txt")
writeLines(report_lines, report_file)
cat("[12E FINAL] Wrote:", report_file, "\n")

cat("\n[12E FINAL] Completed final visual audit.\n")
cat("[12E FINAL] Figures audited:", nrow(figure_audit_df), "\n")
cat("[12E FINAL] Panels audited:", nrow(panel_audit_df), "\n")
cat("[12E FINAL] Panels pass:", sum(panel_audit_df$visual_audit_severity == "pass", na.rm = TRUE), "\n")
cat("[12E FINAL] Minor manual-check panels:", panels_minor, "\n")
cat("[12E FINAL] Panels needing review:", sum(panel_audit_df$visual_audit_severity == "needs_review", na.rm = TRUE), "\n")
cat("[12E FINAL] Critical-fail panels:", panels_critical, "\n")
cat("[12E FINAL] Table-redrawn panels:", ifelse("figure_id" %in% colnames(table_focus_df), nrow(table_focus_df), 0), "\n")
cat("[12E FINAL] Overclaim flags:", overclaim_flags, "\n")
cat("[12E FINAL] Figures pass to 12F/manual check:", figures_pass_to_12f, "\n")
cat("[12E FINAL] Figures blocked before 12F:", figures_blocked, "\n")
cat("[12E FINAL] Figures written: 4\n")
cat("[12E FINAL] Decision:", overall_decision, "\n")
cat("[12E FINAL] Output tables:", out_table_dir, "\n")
cat("[12E FINAL] Output figs  :", out_fig_dir, "\n")
cat("[12E FINAL] Output text  :", out_text_dir, "\n")
cat("[12E FINAL] Next         : review 12E FINAL PDFs and manual check list; if accepted, proceed to 12F optional final assembly.\n")
