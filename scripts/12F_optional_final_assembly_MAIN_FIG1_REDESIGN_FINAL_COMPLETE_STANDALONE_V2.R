
# ============================================================
# 12F FINAL COMPLETE STANDALONE V2 - MAIN FIG 1 REDESIGN OVERRIDE + SUPP TABLE COUNT FIX
# Optional final assembly-ready package for DA neuron / graft-related
# transcriptomic cell-state prioritisation framework
#
# User rule respected:
#   - Complete standalone script for 12F
#   - Does NOT read any previous 12F output
#   - Does NOT patch old 12F tables or figures
#   - May read locked upstream outputs as formal inputs:
#       10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E
#   - Uses 12E visual audit + 12D panel package manifest as formal input
#   - Does NOT directly use Main Fig 1A-D table-preview panels as final main panels
#   - Redesigns Main Fig 1A-D as publication-style schematic panels
#   - Keeps table-redrawn supplement source-detail panels as supplement/reference panels
#   - Generates final assembly-ready manifest and 12G handoff
#   - No internet
#   - No 00-10P rerun
#
# Scientific boundary:
#   - Optional final assembly-ready structure only
#   - Evidence-anchored transcriptomic prioritisation framework
#   - Candidate transcriptomic marker signatures only
#   - marker-rule-derived prioritization model audit only
#   - No clinical prediction
#   - No diagnostic/prognostic biomarker validation
#   - No causal graft efficacy/safety claim
#   - No true anatomical projection or barcode-lineage proof
# ============================================================

cat("\n[12F FINAL V2] Starting optional final assembly with Main Fig 1 redesign override and supplementary table-count fix...\n")
cat("[12F FINAL] Mode: complete standalone 12F rebuild; no previous 12F dependency; no internet; no 00-10P rerun.\n")
cat("[12F FINAL] Inputs allowed: locked upstream 10A-10P, 11A-11J, 12A, 12B, 12C, 12D and 12E outputs.\n")
cat("[12F FINAL] Main Fig 1 rule: do NOT directly assemble 12D table-preview panels; redraw as publication-style schematic panels.\n")
cat("[12F FINAL] Claim boundary: assembly-ready package only; no clinical prediction or validated biomarker claim.\n")

graphics.off()

# ------------------------- project paths -------------------------
project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")
figure_root <- file.path(project_root, "04_figures")
text_root <- file.path(project_root, "09_manuscript")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "12F_optional_final_assembly_MAIN_FIG1_REDESIGN_FINAL_COMPLETE_STANDALONE_V2"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "12F_optional_final_assembly_MAIN_FIG1_REDESIGN_FINAL_COMPLETE_STANDALONE_V2_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "12F_optional_final_assembly_MAIN_FIG1_REDESIGN_FINAL_COMPLETE_STANDALONE_V2"
)
out_assembly_dir <- file.path(
  project_root,
  "04_figures",
  "12F_optional_final_assembly_MAIN_FIG1_REDESIGN_FINAL_COMPLETE_STANDALONE_V2_assembly_package"
)

dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_text_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_assembly_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------- safe helpers -------------------------
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
  cat("[12F FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[12F FINAL] Wrote:", file_value, "\n")
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
    cat("[12F FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
    return(file_alt)
  }
  file_primary
}

new_canvas <- function() {
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", xpd = FALSE)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
}

file_exists_safe <- function(file_value) {
  file_value <- clean_space(file_value)
  file_value != "" & file.exists(file_value)
}

copy_file_safe <- function(source_file, dest_file) {
  source_file <- clean_space(source_file)
  dest_file <- clean_space(dest_file)
  if (source_file == "" || !file.exists(source_file)) return(FALSE)
  dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)
  ok_copy <- FALSE
  tryCatch({
    ok_copy <- file.copy(source_file, dest_file, overwrite = TRUE)
  }, error = function(err_obj) {
    ok_copy <<- FALSE
  })
  isTRUE(ok_copy)
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
  pale_gray = "#F3F5F7",
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

draw_title <- function(title_value, subtitle_value = "") {
  text(0.5, 0.965, title_value, cex = 1.02, font = 2, adj = c(0.5, 0.5), col = nature_palette$ink)
  if (nchar(subtitle_value) > 0) {
    text(0.5, 0.928, subtitle_value, cex = 0.50, col = nature_palette$muted, adj = c(0.5, 0.5))
  }
}

draw_label <- function(label_value, x_left, y_mid, color_value = nature_palette$navy) {
  rect(x_left, y_mid - 0.026, x_left + 0.047, y_mid + 0.026, col = color_value, border = color_value, lwd = 0.4)
  text(x_left + 0.0235, y_mid, label_value, cex = 0.60, font = 2, col = nature_palette$white)
}

draw_box <- function(x0, y0, x1, y1, title_value, body_value = "", fill_value = nature_palette$pale_blue,
                     border_value = nature_palette$blue, title_cex = 0.48, body_cex = 0.36) {
  rect(x0, y0, x1, y1, col = fill_value, border = border_value, lwd = 0.75)
  text((x0 + x1) / 2, y1 - 0.022, title_value, cex = title_cex, font = 2, col = nature_palette$ink)
  if (nchar(body_value) > 0) {
    wrapped_text <- unlist(strwrap(body_value, width = 35))
    if (length(wrapped_text) > 4) wrapped_text <- wrapped_text[seq_len(4)]
    y_start <- y1 - 0.055
    for (idx_line in seq_along(wrapped_text)) {
      text((x0 + x1) / 2, y_start - (idx_line - 1) * 0.022, wrapped_text[idx_line],
           cex = body_cex, col = nature_palette$muted)
    }
  }
}

draw_arrow <- function(x0, y0, x1, y1, color_value = nature_palette$muted) {
  arrows(x0, y0, x1, y1, length = 0.055, angle = 20, lwd = 1.0, col = color_value)
}

draw_badge <- function(x_mid, y_mid, label_value, color_value, cex_value = 0.40) {
  rect(x_mid - 0.055, y_mid - 0.022, x_mid + 0.055, y_mid + 0.022, col = color_value, border = color_value, lwd = 0.35)
  text(x_mid, y_mid, label_value, cex = cex_value, col = nature_palette$white, font = 2)
}

# ------------------------- upstream discovery -------------------------
if (!dir.exists(table_root)) stop("[12F FINAL] Missing table root: ", table_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
table_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(table_info$size) & table_info$size > 0 & table_info$size < 180 * 1024 * 1024]

# Hard rule: do not read previous 12F output
all_table_files <- all_table_files[!grepl("12F_optional_final_assembly", all_table_files, ignore.case = TRUE)]

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

# ------------------------- read locked 12E and 12D inputs -------------------------
file_12e_panel_audit <- first_existing_file(c(
  file.path(table_root, "12E_final_visual_audit_FINAL_COMPLETE_STANDALONE", "12E_FINAL_panel_visual_audit.csv"),
  find_files_all_terms(all_table_files, c("12e", "panel_visual_audit"), max_n = 10)
))
file_12e_figure_audit <- first_existing_file(c(
  file.path(table_root, "12E_final_visual_audit_FINAL_COMPLETE_STANDALONE", "12E_FINAL_figure_visual_audit.csv"),
  find_files_all_terms(all_table_files, c("12e", "figure_visual_audit"), max_n = 10)
))
file_12e_claim_scan <- first_existing_file(c(
  file.path(table_root, "12E_final_visual_audit_FINAL_COMPLETE_STANDALONE", "12E_FINAL_panel_claim_boundary_scan.csv"),
  find_files_all_terms(all_table_files, c("12e", "panel_claim_boundary_scan"), max_n = 10)
))
file_12e_manual_list <- first_existing_file(c(
  file.path(table_root, "12E_final_visual_audit_FINAL_COMPLETE_STANDALONE", "12E_FINAL_manual_panel_check_list.csv"),
  find_files_all_terms(all_table_files, c("12e", "manual_panel_check_list"), max_n = 10)
))
file_12d_panel_manifest <- first_existing_file(c(
  file.path(table_root, "12D_final_panel_package_generation_FINAL_COMPLETE_STANDALONE", "12D_FINAL_panel_package_manifest.csv"),
  find_files_all_terms(all_table_files, c("12d", "panel_package_manifest"), max_n = 10)
))
file_10h_role_summary <- first_existing_file(c(
  file.path(table_root, "10H_dataset_role_and_model_scope_freeze_V1", "10H_V1_dataset_role_summary.csv"),
  find_files_all_terms(all_table_files, c("10h", "dataset_role_summary"), max_n = 10)
))

panel_audit_df <- read_table_safe(file_12e_panel_audit)
figure_audit_df <- read_table_safe(file_12e_figure_audit)
claim_scan_df <- read_table_safe(file_12e_claim_scan)
manual_list_df <- read_table_safe(file_12e_manual_list)
panel_manifest_12d_df <- read_table_safe(file_12d_panel_manifest)
role_summary_df <- read_table_safe(file_10h_role_summary)

if (nrow(panel_audit_df) < 1) stop("[12F FINAL] Missing 12E panel visual audit table.", call. = FALSE)
if (!("figure_id" %in% colnames(panel_audit_df))) stop("[12F FINAL] 12E panel audit missing figure_id.", call. = FALSE)
if (!("panel_id" %in% colnames(panel_audit_df))) stop("[12F FINAL] 12E panel audit missing panel_id.", call. = FALSE)

input_audit_df <- data.frame(
  input_name = c(
    "12E_panel_visual_audit",
    "12E_figure_visual_audit",
    "12E_panel_claim_boundary_scan",
    "12E_manual_panel_check_list",
    "12D_panel_package_manifest",
    "10H_dataset_role_summary_for_MainFig1B"
  ),
  detected = c(
    file_12e_panel_audit != "",
    file_12e_figure_audit != "",
    file_12e_claim_scan != "",
    file_12e_manual_list != "",
    file_12d_panel_manifest != "",
    file_10h_role_summary != ""
  ),
  file_path = c(
    file_12e_panel_audit,
    file_12e_figure_audit,
    file_12e_claim_scan,
    file_12e_manual_list,
    file_12d_panel_manifest,
    file_10h_role_summary
  ),
  rows_loaded = c(
    nrow(panel_audit_df),
    nrow(figure_audit_df),
    nrow(claim_scan_df),
    nrow(manual_list_df),
    nrow(panel_manifest_12d_df),
    nrow(role_summary_df)
  ),
  allowed_as_locked_upstream_input = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
write_csv_safe(input_audit_df, file.path(out_table_dir, "12F_FINAL_locked_input_audit.csv"))

# ------------------------- role summary for Main Fig 1B -------------------------
role_plot_df <- data.frame(
  category = c(
    "Total locked accessions",
    "Core 09C training/model development",
    "Independent external validation",
    "Marker-targeted context validation",
    "Bulk support, not scRNA training",
    "Manual/background/non-core"
  ),
  n = c(12, 5, 1, 1, 1, 4),
  accessions = c(
    "12 locked GEO accessions",
    "GSE132758, GSE178265, GSE200610, GSE204796, GSE233885",
    "GSE183248",
    "GSE243639",
    "GSE204795",
    "GSE128040, GSE148434, GSE157783, GSE184950"
  ),
  stringsAsFactors = FALSE
)

if (nrow(role_summary_df) > 0) {
  category_col <- ""
  n_col <- ""
  accessions_col <- ""
  if ("category" %in% colnames(role_summary_df)) category_col <- "category"
  if ("n" %in% colnames(role_summary_df)) n_col <- "n"
  if ("accessions" %in% colnames(role_summary_df)) accessions_col <- "accessions"
  if (category_col != "" && n_col != "") {
    raw_cat <- clean_space(role_summary_df[[category_col]])
    raw_n <- safe_num(role_summary_df[[n_col]])
    raw_acc <- rep("", length(raw_cat))
    if (accessions_col != "") raw_acc <- clean_space(role_summary_df[[accessions_col]])

    get_role_row <- function(key_value, label_value) {
      hit_idx <- which(grepl(key_value, raw_cat, ignore.case = TRUE))
      if (length(hit_idx) < 1) return(data.frame(category = label_value, n = NA_real_, accessions = "", stringsAsFactors = FALSE))
      data.frame(category = label_value, n = raw_n[hit_idx[1]], accessions = raw_acc[hit_idx[1]], stringsAsFactors = FALSE)
    }

    rebuilt_role <- safe_bind_rows(list(
      get_role_row("total_locked", "Total locked accessions"),
      get_role_row("core_09C|core.*training|model_development", "Core 09C training/model development"),
      get_role_row("external_validation", "Independent external validation"),
      get_role_row("marker_targeted", "Marker-targeted context validation"),
      get_role_row("bulk_support", "Bulk support, not scRNA training"),
      get_role_row("manual_review|background|not_core", "Manual/background/non-core")
    ))
    if (sum(is.finite(rebuilt_role$n)) >= 4) {
      role_plot_df <- rebuilt_role
      role_plot_df$n[!is.finite(role_plot_df$n)] <- 0
      role_plot_df$accessions[is.na(role_plot_df$accessions)] <- ""
    }
  }
}
write_csv_safe(role_plot_df, file.path(out_table_dir, "12F_FINAL_Main_Fig_1B_dataset_role_summary_used.csv"))

# ------------------------- Main Fig 1 redesigned panels -------------------------
draw_main_fig1_panel_A <- function(output_file) {
  open_pdf_safe(basename(output_file), width_value = 12.2, height_value = 5.4, target_dir = dirname(output_file))
  new_canvas()
  draw_title(
    "Main Fig 1A redesigned workflow schematic",
    "Source-traceable transcriptomic prioritisation framework; table-preview panel replaced by publication-style schematic."
  )
  draw_label("A", 0.055, 0.865, nature_palette$blue)

  steps <- data.frame(
    title = c("Locked multi-source\ntranscriptomic inputs", "DA neuron / graft\ncell-state evidence", "Marker-rule-derived\nprioritisation", "Maturation trajectory\nand module coupling", "Proxy/external\nevidence layers", "Integrated evidence tier\nand ML audit"),
    body = c("scRNA, bulk and context datasets", "DA identity, A9/A10-like state, projection competence", "Internal transcriptomic prioritisation tasks", "Pseudotime and module-score correlations", "Preclinical, projection, state, risk and genetic context", "Evidence-tier summary with conservative claim boundary"),
    color = c(nature_palette$blue, nature_palette$teal, nature_palette$navy, nature_palette$red, nature_palette$purple, nature_palette$orange),
    stringsAsFactors = FALSE
  )

  x_centers <- seq(0.12, 0.88, length.out = nrow(steps))
  for (idx_step in seq_len(nrow(steps))) {
    x0 <- x_centers[idx_step] - 0.065
    x1 <- x_centers[idx_step] + 0.065
    draw_box(
      x0, 0.46, x1, 0.75,
      steps$title[idx_step],
      steps$body[idx_step],
      fill_value = blend_color(nature_palette$white, steps$color[idx_step], 0.16),
      border_value = steps$color[idx_step],
      title_cex = 0.39,
      body_cex = 0.28
    )
    if (idx_step < nrow(steps)) {
      draw_arrow(x1 + 0.010, 0.605, x_centers[idx_step + 1] - 0.078, 0.605, nature_palette$muted)
    }
  }

  rect(0.10, 0.23, 0.90, 0.34, col = nature_palette$pale_gray, border = nature_palette$grid, lwd = 0.55)
  text(0.50, 0.295, "Output: candidate transcriptomic cell-state prioritisation framework", cex = 0.50, font = 2, col = nature_palette$ink)
  text(0.50, 0.255, "Boundary: hypothesis-generating computational framework, not clinical prediction or candidate therapeutic-marker context.", cex = 0.38, col = nature_palette$muted)
  dev.off()
  output_file
}

draw_main_fig1_panel_B <- function(output_file, role_data) {
  open_pdf_safe(basename(output_file), width_value = 10.8, height_value = 6.2, target_dir = dirname(output_file))
  new_canvas()
  draw_title(
    "Main Fig 1B redesigned dataset/source role map",
    "Locked accessions are summarized by role rather than shown as internal file-index tables."
  )
  draw_label("B", 0.055, 0.865, nature_palette$teal)

  role_data$n <- safe_num(role_data$n)
  role_data$n[!is.finite(role_data$n)] <- 0
  max_n <- max(role_data$n, na.rm = TRUE)
  if (!is.finite(max_n) || max_n <= 0) max_n <- 1

  y_positions <- seq(0.74, 0.28, length.out = nrow(role_data))
  for (idx_role in seq_len(nrow(role_data))) {
    yy <- y_positions[idx_role]
    count_now <- role_data$n[idx_role]
    color_now <- nature_palette$teal
    if (idx_role == 1) color_now <- nature_palette$blue
    if (grepl("external|marker", role_data$category[idx_role], ignore.case = TRUE)) color_now <- nature_palette$purple
    if (grepl("bulk|manual", role_data$category[idx_role], ignore.case = TRUE)) color_now <- nature_palette$orange

    text(0.11, yy, role_data$category[idx_role], cex = 0.44, adj = c(0, 0.5), col = nature_palette$ink)
    rect(0.43, yy - 0.020, 0.43 + 0.35 * count_now / max_n, yy + 0.020,
         col = color_now, border = nature_palette$border, lwd = 0.35)
    text(0.43 + 0.35 * count_now / max_n + 0.018, yy, as.character(count_now), cex = 0.44, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
    text(0.11, yy - 0.037, substr(role_data$accessions[idx_role], 1, 95), cex = 0.26, adj = c(0, 0.5), col = nature_palette$muted)
  }

  text(0.50, 0.12, "Training/model-development, external validation, context validation and background roles are separated to prevent source-role overclaiming.", cex = 0.38, col = nature_palette$muted)
  dev.off()
  output_file
}

draw_main_fig1_panel_C <- function(output_file) {
  open_pdf_safe(basename(output_file), width_value = 11.0, height_value = 6.4, target_dir = dirname(output_file))
  new_canvas()
  draw_title(
    "Main Fig 1C redesigned domain and claim-boundary map",
    "Main-figure replacement for table-derived domain-boundary preview."
  )
  draw_label("C", 0.055, 0.865, nature_palette$orange)

  rect(0.08, 0.20, 0.47, 0.78, col = nature_palette$pale_green, border = nature_palette$teal, lwd = 0.8)
  rect(0.53, 0.20, 0.92, 0.78, col = nature_palette$pale_orange, border = nature_palette$orange, lwd = 0.8)
  text(0.275, 0.735, "Used for", cex = 0.58, font = 2, col = nature_palette$teal)
  text(0.725, 0.735, "Not used for", cex = 0.58, font = 2, col = nature_palette$orange)

  allowed_items <- c(
    "Transcriptomic prioritisation",
    "DA/graft-related cell-state interpretation",
    "Source-traceable evidence integration",
    "Candidate transcriptomic signatures",
    "Hypothesis generation for follow-up validation"
  )
  prohibited_items <- c(
    "Clinical prediction",
    "Validated diagnostic/prognostic biomarker claim",
    "Graft efficacy or safety prediction",
    "Anatomical projection proof",
    "Barcode-confirmed lineage tracing proof"
  )

  y_allowed <- seq(0.65, 0.34, length.out = length(allowed_items))
  for (idx_item in seq_along(allowed_items)) {
    symbols(0.13, y_allowed[idx_item], circles = 0.012, inches = FALSE, add = TRUE,
            bg = nature_palette$teal, fg = nature_palette$teal)
    text(0.16, y_allowed[idx_item], allowed_items[idx_item], cex = 0.42, adj = c(0, 0.5), col = nature_palette$ink)
  }

  y_prohibited <- seq(0.65, 0.34, length.out = length(prohibited_items))
  for (idx_item in seq_along(prohibited_items)) {
    text(0.58, y_prohibited[idx_item], "X", cex = 0.58, font = 2, col = nature_palette$orange)
    text(0.61, y_prohibited[idx_item], prohibited_items[idx_item], cex = 0.42, adj = c(0, 0.5), col = nature_palette$ink)
  }

  text(0.50, 0.12, "This panel makes the computational scope explicit before presenting downstream results.", cex = 0.38, col = nature_palette$muted)
  dev.off()
  output_file
}

draw_main_fig1_panel_D <- function(output_file) {
  open_pdf_safe(basename(output_file), width_value = 11.2, height_value = 6.2, target_dir = dirname(output_file))
  new_canvas()
  draw_title(
    "Main Fig 1D redesigned source-to-figure traceability map",
    "Locked upstream modules are used as source anchors for final figure assembly."
  )
  draw_label("D", 0.055, 0.865, nature_palette$navy)

  module_df <- data.frame(
    module = c("10C", "10D", "10G", "10H", "10P", "11A/11B", "12C-12E"),
    role = c(
      "source manifest",
      "figure file index",
      "domain audit",
      "dataset role freeze",
      "source-panel package",
      "evidence upgrade audit",
      "source lock and visual audit"
    ),
    color = c(nature_palette$blue, nature_palette$teal, nature_palette$orange, nature_palette$purple, nature_palette$navy, nature_palette$gold, nature_palette$red),
    stringsAsFactors = FALSE
  )

  x_positions <- c(0.14, 0.38, 0.62, 0.86, 0.26, 0.50, 0.74)
  y_positions <- c(0.66, 0.66, 0.66, 0.66, 0.42, 0.42, 0.42)

  for (idx_mod in seq_len(nrow(module_df))) {
    draw_box(
      x_positions[idx_mod] - 0.075, y_positions[idx_mod] - 0.060,
      x_positions[idx_mod] + 0.075, y_positions[idx_mod] + 0.060,
      module_df$module[idx_mod],
      module_df$role[idx_mod],
      fill_value = blend_color(nature_palette$white, module_df$color[idx_mod], 0.16),
      border_value = module_df$color[idx_mod],
      title_cex = 0.46,
      body_cex = 0.30
    )
    draw_arrow(x_positions[idx_mod], y_positions[idx_mod] - 0.074, 0.50, 0.26, nature_palette$muted)
  }

  rect(0.30, 0.13, 0.70, 0.25, col = nature_palette$pale_blue, border = nature_palette$blue, lwd = 0.8)
  text(0.50, 0.205, "Main Fig 1 source-traceable framework", cex = 0.50, font = 2, col = nature_palette$ink)
  text(0.50, 0.165, "Every final panel is linked to locked upstream tables or packaged source PDFs.", cex = 0.34, col = nature_palette$muted)
  dev.off()
  output_file
}

draw_main_fig1_full <- function(output_file, role_data) {
  open_pdf_safe(basename(output_file), width_value = 13.5, height_value = 9.6, target_dir = dirname(output_file))
  new_canvas()
  draw_title(
    "Main Fig 1. Source-traceable transcriptomic prioritisation framework",
    "Redesigned final candidate; 12D table-preview panels are retained only as source-package references."
  )

  # Panel A area
  draw_label("A", 0.055, 0.825, nature_palette$blue)
  steps <- c("Locked datasets", "DA/graft evidence", "Marker-rule-derived priority", "Pseudotime + modules", "Proxy evidence", "Evidence tier + ML audit")
  step_cols <- c(nature_palette$blue, nature_palette$teal, nature_palette$navy, nature_palette$red, nature_palette$purple, nature_palette$orange)
  x_centers <- seq(0.15, 0.85, length.out = length(steps))
  for (idx_step in seq_along(steps)) {
    draw_box(x_centers[idx_step] - 0.052, 0.72, x_centers[idx_step] + 0.052, 0.80,
             steps[idx_step], "", fill_value = blend_color(nature_palette$white, step_cols[idx_step], 0.18),
             border_value = step_cols[idx_step], title_cex = 0.32, body_cex = 0.24)
    if (idx_step < length(steps)) draw_arrow(x_centers[idx_step] + 0.058, 0.76, x_centers[idx_step + 1] - 0.058, 0.76, nature_palette$muted)
  }

  # Panel B area
  draw_label("B", 0.055, 0.610, nature_palette$teal)
  role_data$n <- safe_num(role_data$n)
  role_data$n[!is.finite(role_data$n)] <- 0
  max_n <- max(role_data$n, na.rm = TRUE)
  if (!is.finite(max_n) || max_n <= 0) max_n <- 1
  y_roles <- seq(0.62, 0.43, length.out = nrow(role_data))
  for (idx_role in seq_len(nrow(role_data))) {
    yy <- y_roles[idx_role]
    text(0.13, yy, role_data$category[idx_role], cex = 0.28, adj = c(0, 0.5), col = nature_palette$ink)
    rect(0.42, yy - 0.010, 0.42 + 0.24 * role_data$n[idx_role] / max_n, yy + 0.010,
         col = ifelse(idx_role == 1, nature_palette$blue, nature_palette$teal), border = nature_palette$border, lwd = 0.25)
    text(0.42 + 0.24 * role_data$n[idx_role] / max_n + 0.010, yy, as.character(role_data$n[idx_role]), cex = 0.28, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
  }

  # Panel C area
  draw_label("C", 0.055, 0.360, nature_palette$orange)
  rect(0.12, 0.17, 0.47, 0.36, col = nature_palette$pale_green, border = nature_palette$teal, lwd = 0.65)
  rect(0.53, 0.17, 0.88, 0.36, col = nature_palette$pale_orange, border = nature_palette$orange, lwd = 0.65)
  text(0.295, 0.335, "Used for", cex = 0.38, font = 2, col = nature_palette$teal)
  text(0.705, 0.335, "Not used for", cex = 0.38, font = 2, col = nature_palette$orange)
  text(0.295, 0.285, "Transcriptomic prioritisation\nDA/graft cell-state interpretation\nCandidate signatures", cex = 0.29, col = nature_palette$ink)
  text(0.705, 0.285, "Clinical prediction\nValidated biomarker\nEfficacy/safety proof\nProjection/lineage proof", cex = 0.29, col = nature_palette$ink)

  # Panel D area
  draw_label("D", 0.055, 0.115, nature_palette$navy)
  mod_labels <- c("10C", "10D", "10G", "10H", "10P", "11A/B", "12C-E")
  mod_x <- seq(0.15, 0.85, length.out = length(mod_labels))
  for (idx_mod in seq_along(mod_labels)) {
    draw_badge(mod_x[idx_mod], 0.12, mod_labels[idx_mod], step_cols[((idx_mod - 1) %% length(step_cols)) + 1], cex_value = 0.28)
    draw_arrow(mod_x[idx_mod], 0.095, 0.50, 0.045, nature_palette$muted)
  }
  rect(0.35, 0.015, 0.65, 0.055, col = nature_palette$pale_blue, border = nature_palette$blue, lwd = 0.5)
  text(0.50, 0.035, "source-traceable final assembly", cex = 0.32, font = 2, col = nature_palette$ink)

  dev.off()
  output_file
}

mainfig1_panel_dir <- file.path(out_fig_dir, "Main_Fig_1_redesigned_panels")
dir.create(mainfig1_panel_dir, recursive = TRUE, showWarnings = FALSE)

main_fig1_A <- file.path(mainfig1_panel_dir, "12F_FINAL_Main_Fig_1A_workflow_schematic_REDRAWN.pdf")
main_fig1_B <- file.path(mainfig1_panel_dir, "12F_FINAL_Main_Fig_1B_dataset_source_role_map_REDRAWN.pdf")
main_fig1_C <- file.path(mainfig1_panel_dir, "12F_FINAL_Main_Fig_1C_domain_claim_boundary_map_REDRAWN.pdf")
main_fig1_D <- file.path(mainfig1_panel_dir, "12F_FINAL_Main_Fig_1D_source_traceability_map_REDRAWN.pdf")
main_fig1_full <- file.path(out_fig_dir, "12F_FINAL_FigA_Main_Fig_1_redesigned_framework.pdf")

draw_main_fig1_panel_A(main_fig1_A)
draw_main_fig1_panel_B(main_fig1_B, role_plot_df)
draw_main_fig1_panel_C(main_fig1_C)
draw_main_fig1_panel_D(main_fig1_D)
draw_main_fig1_full(main_fig1_full, role_plot_df)

mainfig1_override_df <- data.frame(
  figure_id = rep("Main Fig 1", 4),
  panel_id = c("Main Fig 1A", "Main Fig 1B", "Main Fig 1C", "Main Fig 1D"),
  panel_label = c("A", "B", "C", "D"),
  old_12D_table_preview_role = c(
    "source backup only; not final panel",
    "source backup only; not final panel",
    "source backup only; not final panel",
    "source backup only; not final panel"
  ),
  final_12F_action = c(
    "redesigned_workflow_schematic",
    "redesigned_dataset_source_role_map",
    "redesigned_domain_claim_boundary_map",
    "redesigned_source_traceability_map"
  ),
  final_12F_panel_pdf = c(main_fig1_A, main_fig1_B, main_fig1_C, main_fig1_D),
  final_12F_full_figure_pdf = main_fig1_full,
  stringsAsFactors = FALSE
)
write_csv_safe(mainfig1_override_df, file.path(out_table_dir, "12F_FINAL_Main_Fig_1_redesign_override_manifest.csv"))

# ------------------------- final assembly package -------------------------
cat("[12F FINAL] Building final assembly-ready panel manifest...\n")

required_cols <- c("figure_type", "figure_id", "panel_id", "panel_label", "primary_locked_module",
                   "planned_panel_content", "final_panel_package_status", "final_panel_package_pdf",
                   "visual_audit_severity", "visual_audit_priority", "visual_audit_issue_flags")
for (col_value in required_cols) {
  if (!(col_value %in% colnames(panel_audit_df))) panel_audit_df[[col_value]] <- ""
}

assembly_list <- list()
for (idx_panel in seq_len(nrow(panel_audit_df))) {
  row_now <- panel_audit_df[idx_panel, , drop = FALSE]
  fig_id <- clean_space(row_now$figure_id)
  panel_id <- clean_space(row_now$panel_id)
  panel_label <- clean_space(row_now$panel_label)

  final_source_pdf <- clean_space(row_now$final_panel_package_pdf)
  assembly_action <- "use_locked_12D_package_panel"
  assembly_note <- "12E-passed package panel used as assembly source"
  table_preview_replaced <- FALSE

  if (fig_id == "Main Fig 1" && panel_label %in% c("A", "B", "C", "D")) {
    override_hit <- mainfig1_override_df[mainfig1_override_df$panel_label == panel_label, , drop = FALSE]
    if (nrow(override_hit) > 0) {
      final_source_pdf <- override_hit$final_12F_panel_pdf[1]
      assembly_action <- "use_12F_redesigned_main_fig1_panel"
      assembly_note <- "12D table-preview panel replaced by 12F publication-style schematic"
      table_preview_replaced <- TRUE
    }
  }

  fig_safe <- safe_file_name(fig_id)
  panel_safe <- safe_file_name(panel_id)
  dest_dir <- file.path(out_assembly_dir, fig_safe)
  dest_file <- file.path(dest_dir, paste0(panel_safe, "__12F_ASSEMBLY_SOURCE.pdf"))
  copy_ok <- copy_file_safe(final_source_pdf, dest_file)
  final_exists <- file_exists_safe(final_source_pdf)

  assembly_status <- "ready_for_final_assembly"
  if (!final_exists) assembly_status <- "missing_final_source_pdf"
  if (final_exists && !copy_ok) assembly_status <- "ready_but_copy_failed_uses_original_path"

  assembly_list[[length(assembly_list) + 1]] <- data.frame(
    figure_type = row_now$figure_type,
    figure_id = fig_id,
    panel_id = panel_id,
    panel_label = panel_label,
    primary_locked_module = row_now$primary_locked_module,
    planned_panel_content = row_now$planned_panel_content,
    previous_12D_or_12E_panel_status = row_now$final_panel_package_status,
    previous_12E_visual_audit_severity = row_now$visual_audit_severity,
    previous_12E_visual_audit_priority = row_now$visual_audit_priority,
    assembly_action = assembly_action,
    table_preview_replaced_by_redesign = table_preview_replaced,
    final_assembly_source_pdf = final_source_pdf,
    final_assembly_source_pdf_exists = final_exists,
    copied_assembly_pdf = ifelse(copy_ok, dest_file, ""),
    copied_assembly_pdf_exists = copy_ok,
    final_assembly_status = assembly_status,
    assembly_note = assembly_note,
    stringsAsFactors = FALSE
  )
}
assembly_df <- safe_bind_rows(assembly_list)
write_csv_safe(assembly_df, file.path(out_table_dir, "12F_FINAL_panel_assembly_manifest.csv"))
write_tsv_safe(assembly_df, file.path(out_table_dir, "12F_FINAL_panel_assembly_manifest.tsv"))

main_assembly_df <- assembly_df[assembly_df$figure_type == "main", , drop = FALSE]
supp_assembly_df <- assembly_df[assembly_df$figure_type == "supplement", , drop = FALSE]
write_csv_safe(main_assembly_df, file.path(out_table_dir, "12F_FINAL_main_figure_assembly_manifest.csv"))
write_csv_safe(supp_assembly_df, file.path(out_table_dir, "12F_FINAL_supplementary_figure_assembly_manifest.csv"))

# ------------------------- figure assembly manifest -------------------------
figure_ids <- unique(assembly_df$figure_id)
figure_assembly_list <- list()
for (idx_fig in seq_along(figure_ids)) {
  fig_now <- figure_ids[idx_fig]
  sub_fig <- assembly_df[assembly_df$figure_id == fig_now, , drop = FALSE]
  n_panels <- nrow(sub_fig)
  n_ready <- sum(sub_fig$final_assembly_status %in% c("ready_for_final_assembly", "ready_but_copy_failed_uses_original_path"))
  n_missing <- sum(sub_fig$final_assembly_status == "missing_final_source_pdf")
  n_redesigned <- sum(sub_fig$assembly_action == "use_12F_redesigned_main_fig1_panel")
  n_reused <- sum(sub_fig$assembly_action == "use_locked_12D_package_panel")
  n_supp_table_retained_here <- sum(grepl("table_redrawn", sub_fig$previous_12D_or_12E_panel_status, ignore.case = TRUE) & fig_now != "Main Fig 1")
  fig_status <- ifelse(n_missing == 0, "assembly_ready", "repair_required")
  if (fig_now == "Main Fig 1" && n_redesigned == 4 && n_missing == 0) fig_status <- "assembly_ready_with_12F_mainfig1_redesign"

  figure_assembly_list[[length(figure_assembly_list) + 1]] <- data.frame(
    figure_id = fig_now,
    figure_type = sub_fig$figure_type[1],
    n_panels = n_panels,
    n_panels_ready = n_ready,
    n_missing_source_panels = n_missing,
    n_12F_redesigned_panels = n_redesigned,
    n_locked_12D_reused_panels = n_reused,
    n_supp_table_redrawn_retained_panels = n_supp_table_retained_here,
    figure_assembly_status = fig_status,
    assembly_package_dir = file.path(out_assembly_dir, safe_file_name(fig_now)),
    stringsAsFactors = FALSE
  )
}
figure_assembly_df <- safe_bind_rows(figure_assembly_list)
write_csv_safe(figure_assembly_df, file.path(out_table_dir, "12F_FINAL_figure_assembly_manifest.csv"))
write_tsv_safe(figure_assembly_df, file.path(out_table_dir, "12F_FINAL_figure_assembly_manifest.tsv"))

# ------------------------- claim boundary assembly check -------------------------
claim_terms <- c(
  "clinical prediction",
  "clinical predictor",
  "diagnostic biomarker",
  "prognostic biomarker",
  "graft efficacy",
  "graft safety prediction",
  "treatment response",
  "anatomical-projection claim",
  "lineage tracing proof",
  "barcode-confirmed lineage"
)

claim_list <- list()
for (idx_panel in seq_len(nrow(assembly_df))) {
  text_now <- paste(
    assembly_df$planned_panel_content[idx_panel],
    assembly_df$assembly_action[idx_panel],
    assembly_df$assembly_note[idx_panel],
    sep = " "
  )
  text_lower <- tolower(text_now)
  hit_terms <- character(0)
  for (term_now in claim_terms) {
    if (grepl(term_now, text_lower, fixed = TRUE)) hit_terms <- c(hit_terms, term_now)
  }
  claim_list[[length(claim_list) + 1]] <- data.frame(
    figure_id = assembly_df$figure_id[idx_panel],
    panel_id = assembly_df$panel_id[idx_panel],
    final_assembly_action = assembly_df$assembly_action[idx_panel],
    prohibited_claim_terms_detected = paste(hit_terms, collapse = ";"),
    overclaim_flag = length(hit_terms) > 0,
    stringsAsFactors = FALSE
  )
}
claim_check_df <- safe_bind_rows(claim_list)
write_csv_safe(claim_check_df, file.path(out_table_dir, "12F_FINAL_claim_boundary_assembly_check.csv"))

manual_check_df <- assembly_df[
  assembly_df$figure_id == "Main Fig 1" |
    grepl("table_redrawn", assembly_df$previous_12D_or_12E_panel_status, ignore.case = TRUE) |
    assembly_df$final_assembly_status != "ready_for_final_assembly",
  ,
  drop = FALSE
]
manual_check_df$manual_check_reason <- "main_fig1_redesign_or_table_redrawn_supplement_or_nonstandard_copy_status"
write_csv_safe(manual_check_df, file.path(out_table_dir, "12F_FINAL_manual_assembly_checklist.csv"))

# ------------------------- overview figures -------------------------
# FigB assembly strategy overview
fig_b <- open_pdf_safe("12F_FINAL_FigB_assembly_strategy_overview.pdf", 11.8, 6.6)
new_canvas()
draw_title("12F assembly strategy overview", "Main Fig 1 table previews are replaced; other 12E-passed package panels are reused.")

strategy_df <- data.frame(
  label = c(
    "Total figures",
    "Total panels",
    "12F redesigned Main Fig 1 panels",
    "Locked 12D package panels reused",
    "Supplement table-redrawn panels retained",
    "Missing source panels",
    "Overclaim flags"
  ),
  value = c(
    nrow(figure_assembly_df),
    nrow(assembly_df),
    sum(assembly_df$assembly_action == "use_12F_redesigned_main_fig1_panel"),
    sum(assembly_df$assembly_action == "use_locked_12D_package_panel"),
    sum(grepl("table_redrawn", assembly_df$previous_12D_or_12E_panel_status, ignore.case = TRUE) &
          assembly_df$figure_id != "Main Fig 1"),
    sum(assembly_df$final_assembly_status == "missing_final_source_pdf"),
    sum(claim_check_df$overclaim_flag, na.rm = TRUE)
  ),
  family = c("all", "panel", "redesign", "reuse", "table", "missing", "claim"),
  stringsAsFactors = FALSE
)
max_value <- max(safe_num(strategy_df$value), na.rm = TRUE)
if (!is.finite(max_value) || max_value <= 0) max_value <- 1
bar_x0 <- 0.39
bar_x1 <- 0.80
y_positions <- seq(0.78, 0.26, length.out = nrow(strategy_df))
for (idx_row in seq_len(nrow(strategy_df))) {
  yy <- y_positions[idx_row]
  count_now <- safe_num(strategy_df$value[idx_row])
  width_now <- count_now / max_value
  if (count_now == 0) width_now <- 0.018
  color_now <- nature_palette$navy
  if (strategy_df$family[idx_row] == "redesign") color_now <- nature_palette$blue
  if (strategy_df$family[idx_row] == "reuse") color_now <- nature_palette$teal
  if (strategy_df$family[idx_row] == "table") color_now <- nature_palette$gold
  if (strategy_df$family[idx_row] == "missing") color_now <- ifelse(count_now > 0, nature_palette$red, nature_palette$teal)
  if (strategy_df$family[idx_row] == "claim") color_now <- ifelse(count_now > 0, nature_palette$red, nature_palette$teal)
  text(bar_x0 - 0.018, yy, strategy_df$label[idx_row], cex = 0.52, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.024, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.024,
       col = color_now, border = nature_palette$border, lwd = 0.42)
  text(bar_x0 + width_now * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_now), cex = 0.50, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.12, "12F creates assembly-ready sources; final publication arrangement still requires human layout review.", cex = 0.44, col = nature_palette$muted)
dev.off()
cat("[12F FINAL] Wrote figure:", fig_b, "\n")

# FigC main figure assembly readiness
fig_c <- open_pdf_safe("12F_FINAL_FigC_main_figure_assembly_readiness.pdf", 11.8, 6.8)
new_canvas()
draw_title("Main figure assembly readiness", "Main Fig 1 is redesigned; Main Fig 2-5 reuse locked source-panel packages.")

main_fig_df <- figure_assembly_df[figure_assembly_df$figure_type == "main", , drop = FALSE]
main_fig_df <- main_fig_df[order(main_fig_df$figure_id), , drop = FALSE]
y_positions <- seq(0.74, 0.30, length.out = nrow(main_fig_df))
for (idx_fig in seq_len(nrow(main_fig_df))) {
  yy <- y_positions[idx_fig]
  color_now <- figure_color(main_fig_df$figure_id[idx_fig])
  rect(0.12, yy - 0.035, 0.27, yy + 0.035, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.195, yy, main_fig_df$figure_id[idx_fig], cex = 0.40, font = 2, col = nature_palette$white)
  text(0.32, yy + 0.015, paste0("panels ready: ", main_fig_df$n_panels_ready[idx_fig], "/", main_fig_df$n_panels[idx_fig]),
       cex = 0.40, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.32, yy - 0.018, paste0("redesigned: ", main_fig_df$n_12F_redesigned_panels[idx_fig],
                                " | reused: ", main_fig_df$n_locked_12D_reused_panels[idx_fig]),
       cex = 0.34, adj = c(0, 0.5), col = nature_palette$muted)
  text(0.72, yy, main_fig_df$figure_assembly_status[idx_fig], cex = 0.36, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12F FINAL] Wrote figure:", fig_c, "\n")

# FigD supplementary figure assembly readiness
fig_d <- open_pdf_safe("12F_FINAL_FigD_supplementary_figure_assembly_readiness.pdf", 12.0, 7.2)
new_canvas()
draw_title("Supplementary figure assembly readiness", "Supplement source-detail panels remain as supplement/reference, not main figure substitutes.")

supp_fig_df <- figure_assembly_df[figure_assembly_df$figure_type == "supplement", , drop = FALSE]
supp_fig_df <- supp_fig_df[order(supp_fig_df$figure_id), , drop = FALSE]
y_positions <- seq(0.78, 0.20, length.out = nrow(supp_fig_df))
for (idx_fig in seq_len(nrow(supp_fig_df))) {
  yy <- y_positions[idx_fig]
  color_now <- figure_color(supp_fig_df$figure_id[idx_fig])
  rect(0.10, yy - 0.024, 0.29, yy + 0.024, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.195, yy, supp_fig_df$figure_id[idx_fig], cex = 0.34, font = 2, col = nature_palette$white)
  text(0.33, yy, paste0("ready ", supp_fig_df$n_panels_ready[idx_fig], "/", supp_fig_df$n_panels[idx_fig]),
       cex = 0.36, adj = c(0, 0.5), col = nature_palette$ink)
  table_retained_now <- 0
  if ("n_supp_table_redrawn_retained_panels" %in% colnames(supp_fig_df)) {
    table_retained_now <- safe_num(supp_fig_df$n_supp_table_redrawn_retained_panels[idx_fig])
    if (!is.finite(table_retained_now)) table_retained_now <- 0
  }
  text(0.52, yy, paste0("table-redrawn source panels retained: ", table_retained_now),
       cex = 0.32, adj = c(0, 0.5), col = nature_palette$muted)
  text(0.76, yy, supp_fig_df$figure_assembly_status[idx_fig], cex = 0.32, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12F FINAL] Wrote figure:", fig_d, "\n")

# FigE 12G handoff summary
fig_e <- open_pdf_safe("12F_FINAL_FigE_12G_handoff_summary.pdf", 11.4, 6.4)
new_canvas()
draw_title("12G handoff summary", "Assembly-ready sources and redesigned Main Fig 1 are ready for legend/caption refresh.")

handoff_items <- data.frame(
  item = c(
    "Main Fig 1 redesigned framework",
    "Panel assembly manifest",
    "Figure assembly manifest",
    "Main figure manifest",
    "Supplement manifest",
    "Claim-boundary assembly check",
    "Manual assembly checklist"
  ),
  ready = c(
    file.exists(main_fig1_full),
    file.exists(file.path(out_table_dir, "12F_FINAL_panel_assembly_manifest.csv")),
    file.exists(file.path(out_table_dir, "12F_FINAL_figure_assembly_manifest.csv")),
    file.exists(file.path(out_table_dir, "12F_FINAL_main_figure_assembly_manifest.csv")),
    file.exists(file.path(out_table_dir, "12F_FINAL_supplementary_figure_assembly_manifest.csv")),
    file.exists(file.path(out_table_dir, "12F_FINAL_claim_boundary_assembly_check.csv")),
    file.exists(file.path(out_table_dir, "12F_FINAL_manual_assembly_checklist.csv"))
  ),
  stringsAsFactors = FALSE
)
y_positions <- seq(0.78, 0.32, length.out = nrow(handoff_items))
for (idx_row in seq_len(nrow(handoff_items))) {
  yy <- y_positions[idx_row]
  color_now <- ifelse(handoff_items$ready[idx_row], nature_palette$teal, nature_palette$orange)
  symbols(0.22, yy, circles = 0.018, inches = FALSE, add = TRUE,
          bg = color_now, fg = nature_palette$border, lwd = 0.35)
  text(0.26, yy, handoff_items$item[idx_row], cex = 0.48, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.72, yy, ifelse(handoff_items$ready[idx_row], "ready", "review"), cex = 0.44, adj = c(0, 0.5), col = color_now)
}
text(0.50, 0.18, "Next module: 12G final legends/caption refresh using the 12F assembly manifest.", cex = 0.42, col = nature_palette$muted)
dev.off()
cat("[12F FINAL] Wrote figure:", fig_e, "\n")

# ------------------------- handoff and summary -------------------------
n_figures <- nrow(figure_assembly_df)
n_panels <- nrow(assembly_df)
n_redesigned <- sum(assembly_df$assembly_action == "use_12F_redesigned_main_fig1_panel")
n_reused <- sum(assembly_df$assembly_action == "use_locked_12D_package_panel")
n_missing <- sum(assembly_df$final_assembly_status == "missing_final_source_pdf")
n_copy_failed <- sum(assembly_df$final_assembly_status == "ready_but_copy_failed_uses_original_path")
n_claim_flags <- sum(claim_check_df$overclaim_flag, na.rm = TRUE)
n_supp_table_retained <- sum(grepl("table_redrawn", assembly_df$previous_12D_or_12E_panel_status, ignore.case = TRUE) &
                               assembly_df$figure_id != "Main Fig 1")

decision_value <- "INPUT_READY_FOR_12G_FINAL_LEGEND_CAPTION_REFRESH"
if (n_missing > 0 || n_claim_flags > 0) {
  decision_value <- "REPAIR_REQUIRED_BEFORE_12G"
}
if (n_missing == 0 && n_claim_flags == 0 && n_copy_failed > 0) {
  decision_value <- "INPUT_READY_FOR_12G_WITH_ORIGINAL_PATH_COPY_WARNING"
}

handoff_12g_df <- data.frame(
  handoff_item = c(
    "redesigned Main Fig 1 full candidate",
    "redesigned Main Fig 1 panel manifest",
    "final panel assembly manifest",
    "final figure assembly manifest",
    "manual assembly checklist",
    "claim-boundary assembly check",
    "12G readiness decision"
  ),
  file_path = c(
    main_fig1_full,
    file.path(out_table_dir, "12F_FINAL_Main_Fig_1_redesign_override_manifest.csv"),
    file.path(out_table_dir, "12F_FINAL_panel_assembly_manifest.csv"),
    file.path(out_table_dir, "12F_FINAL_figure_assembly_manifest.csv"),
    file.path(out_table_dir, "12F_FINAL_manual_assembly_checklist.csv"),
    file.path(out_table_dir, "12F_FINAL_claim_boundary_assembly_check.csv"),
    decision_value
  ),
  role_in_12G = c(
    "Main Fig 1 caption/legend target",
    "records replacement of 12D table previews",
    "panel-level source for all figure legends",
    "figure-level source for all figure legends",
    "manual check before manuscript export",
    "claim-boundary text control",
    "12G go/no-go decision"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(handoff_12g_df, file.path(out_table_dir, "12F_FINAL_handoff_to_12G_legends_and_captions.csv"))

summary_df <- data.frame(
  item = c(
    "figures_registered",
    "panels_registered",
    "main_figures_registered",
    "supplementary_figures_registered",
    "main_fig1_redesigned_panels",
    "locked_12D_package_panels_reused",
    "supplement_table_redrawn_panels_retained",
    "missing_source_panels",
    "copy_warning_panels",
    "overclaim_flags",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(n_figures),
    as.character(n_panels),
    as.character(sum(figure_assembly_df$figure_type == "main")),
    as.character(sum(figure_assembly_df$figure_type == "supplement")),
    as.character(n_redesigned),
    as.character(n_reused),
    as.character(n_supp_table_retained),
    as.character(n_missing),
    as.character(n_copy_failed),
    as.character(n_claim_flags),
    "5 plus 4 redesigned Main Fig 1 panel PDFs",
    decision_value
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "12F_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "12F_FINAL_execution_summary.tsv"))

report_lines <- c(
  "12F FINAL report",
  "================",
  "Module: optional final assembly with Main Fig 1 redesign override",
  "Mode: complete standalone 12F rebuild; no previous 12F output dependency; no internet; no 00-10P rerun.",
  "Allowed upstream inputs: locked 10A-10P, 11A-11J, 12A, 12B, 12C, 12D and 12E outputs.",
  "",
  paste0("Figures registered: ", n_figures),
  paste0("Panels registered: ", n_panels),
  paste0("Main figures registered: ", sum(figure_assembly_df$figure_type == "main")),
  paste0("Supplementary figures registered: ", sum(figure_assembly_df$figure_type == "supplement")),
  paste0("Main Fig 1 redesigned panels: ", n_redesigned),
  paste0("Locked 12D package panels reused: ", n_reused),
  paste0("Supplement table-redrawn panels retained: ", n_supp_table_retained),
  paste0("Missing source panels: ", n_missing),
  paste0("Copy warning panels: ", n_copy_failed),
  paste0("Overclaim flags: ", n_claim_flags),
  "",
  "Main Fig 1 rule:",
  "- 12D table-preview panels Main Fig 1A-D are not used as final main panels.",
  "- 12F generated redesigned vector schematic panels for Main Fig 1A-D.",
  "- 12D table-preview files remain source-package references only.",
  "",
  "12G inputs:",
  paste0("- ", main_fig1_full),
  paste0("- ", file.path(out_table_dir, "12F_FINAL_panel_assembly_manifest.csv")),
  paste0("- ", file.path(out_table_dir, "12F_FINAL_figure_assembly_manifest.csv")),
  paste0("- ", file.path(out_table_dir, "12F_FINAL_Main_Fig_1_redesign_override_manifest.csv")),
  paste0("- ", file.path(out_table_dir, "12F_FINAL_claim_boundary_assembly_check.csv")),
  "",
  "Claim boundary:",
  "- 12F is an assembly-ready packaging/redesign step only.",
  "- Do not convert assembly outputs into new biological findings.",
  "- Do not claim clinical prediction, validated biomarker, causal graft efficacy/safety, anatomical projection or barcode-lineage proof.",
  "",
  paste0("Decision: ", decision_value)
)
report_file <- file.path(out_text_dir, "12F_FINAL_optional_final_assembly_report.txt")
writeLines(report_lines, report_file)
cat("[12F FINAL] Wrote:", report_file, "\n")

cat("\n[12F FINAL] Completed optional final assembly with Main Fig 1 redesign override.\n")
cat("[12F FINAL] Figures registered:", n_figures, "\n")
cat("[12F FINAL] Panels registered:", n_panels, "\n")
cat("[12F FINAL] Main figures registered:", sum(figure_assembly_df$figure_type == "main"), "\n")
cat("[12F FINAL] Supplementary figures registered:", sum(figure_assembly_df$figure_type == "supplement"), "\n")
cat("[12F FINAL] Main Fig 1 redesigned panels:", n_redesigned, "\n")
cat("[12F FINAL] Locked 12D package panels reused:", n_reused, "\n")
cat("[12F FINAL] Supplement table-redrawn panels retained:", n_supp_table_retained, "\n")
cat("[12F FINAL] Missing source panels:", n_missing, "\n")
cat("[12F FINAL] Copy warning panels:", n_copy_failed, "\n")
cat("[12F FINAL] Overclaim flags:", n_claim_flags, "\n")
cat("[12F FINAL] Figures written: 5 plus 4 redesigned Main Fig 1 panel PDFs\n")
cat("[12F FINAL] Decision:", decision_value, "\n")
cat("[12F FINAL] Output tables:", out_table_dir, "\n")
cat("[12F FINAL] Output figs  :", out_fig_dir, "\n")
cat("[12F FINAL] Output assembly package:", out_assembly_dir, "\n")
cat("[12F FINAL] Output text  :", out_text_dir, "\n")
cat("[12F FINAL] Next         : review 12F PDFs; if accepted, proceed to 12G final legends/caption refresh.\n")
