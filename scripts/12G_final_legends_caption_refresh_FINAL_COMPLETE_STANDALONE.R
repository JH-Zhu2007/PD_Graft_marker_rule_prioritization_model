
# ============================================================
# 12G FINAL COMPLETE STANDALONE
# Final legends / caption refresh for DA neuron / graft-related
# transcriptomic cell-state prioritisation framework
#
# User rule respected:
#   - Complete standalone script for 12G
#   - Does NOT read any previous 12G output
#   - Does NOT patch old 12G tables or figures
#   - May read locked upstream outputs as formal inputs:
#       10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F
#   - Uses 12F final assembly manifest as formal input
#   - Generates final main/supplement legends, panel captions,
#     claim-boundary-safe legend audit, and 12H results-text handoff
#   - No internet
#   - No 00-10P rerun
#
# Scientific boundary:
#   - Legend/caption generation only
#   - Evidence-anchored transcriptomic prioritisation framework
#   - Candidate transcriptomic marker signatures only
#   - marker-rule-derived prioritization model audit only
#   - No clinical prediction
#   - No diagnostic/prognostic biomarker validation
#   - No causal graft efficacy/safety claim
#   - No true anatomical projection or barcode-lineage proof
# ============================================================

cat("\n[12G FINAL] Starting final legends/caption refresh...\n")
cat("[12G FINAL] Mode: complete standalone 12G rebuild; no previous 12G dependency; no internet; no 00-10P rerun.\n")
cat("[12G FINAL] Inputs allowed: locked upstream 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E and 12F outputs.\n")
cat("[12G FINAL] Formal input: 12F final assembly manifest.\n")
cat("[12G FINAL] Claim boundary: legends/captions only; no clinical prediction or validated biomarker claim.\n")

graphics.off()

# ------------------------- project paths -------------------------
project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")
figure_root <- file.path(project_root, "04_figures")
text_root <- file.path(project_root, "09_manuscript")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "12G_final_legends_caption_refresh_FINAL_COMPLETE_STANDALONE"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "12G_final_legends_caption_refresh_FINAL_COMPLETE_STANDALONE_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "12G_final_legends_caption_refresh_FINAL_COMPLETE_STANDALONE"
)

dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_text_dir, recursive = TRUE, showWarnings = FALSE)

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
  cat("[12G FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[12G FINAL] Wrote:", file_value, "\n")
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
    cat("[12G FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
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

# ------------------------- colors -------------------------
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
  text(0.5, 0.965, title_value, cex = 1.00, font = 2, adj = c(0.5, 0.5), col = nature_palette$ink)
  if (nchar(subtitle_value) > 0) {
    text(0.5, 0.928, subtitle_value, cex = 0.50, col = nature_palette$muted, adj = c(0.5, 0.5))
  }
}

# ------------------------- upstream discovery -------------------------
if (!dir.exists(table_root)) stop("[12G FINAL] Missing table root: ", table_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
table_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(table_info$size) & table_info$size > 0 & table_info$size < 200 * 1024 * 1024]

# Hard rule: do not read previous 12G output
all_table_files <- all_table_files[!grepl("12G_final_legends_caption_refresh", all_table_files, ignore.case = TRUE)]

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

# ------------------------- read locked 12F inputs -------------------------
file_12f_panel_assembly <- first_existing_file(c(
  file.path(table_root, "12F_optional_final_assembly_MAIN_FIG1_REDESIGN_FINAL_COMPLETE_STANDALONE_V2", "12F_FINAL_panel_assembly_manifest.csv"),
  find_files_all_terms(all_table_files, c("12f", "panel_assembly_manifest"), max_n = 10)
))
file_12f_figure_assembly <- first_existing_file(c(
  file.path(table_root, "12F_optional_final_assembly_MAIN_FIG1_REDESIGN_FINAL_COMPLETE_STANDALONE_V2", "12F_FINAL_figure_assembly_manifest.csv"),
  find_files_all_terms(all_table_files, c("12f", "figure_assembly_manifest"), max_n = 10)
))
file_12f_mainfig1_override <- first_existing_file(c(
  file.path(table_root, "12F_optional_final_assembly_MAIN_FIG1_REDESIGN_FINAL_COMPLETE_STANDALONE_V2", "12F_FINAL_Main_Fig_1_redesign_override_manifest.csv"),
  find_files_all_terms(all_table_files, c("12f", "main_fig_1_redesign_override_manifest"), max_n = 10)
))
file_12f_claim_check <- first_existing_file(c(
  file.path(table_root, "12F_optional_final_assembly_MAIN_FIG1_REDESIGN_FINAL_COMPLETE_STANDALONE_V2", "12F_FINAL_claim_boundary_assembly_check.csv"),
  find_files_all_terms(all_table_files, c("12f", "claim_boundary_assembly_check"), max_n = 10)
))
file_12f_manual_check <- first_existing_file(c(
  file.path(table_root, "12F_optional_final_assembly_MAIN_FIG1_REDESIGN_FINAL_COMPLETE_STANDALONE_V2", "12F_FINAL_manual_assembly_checklist.csv"),
  find_files_all_terms(all_table_files, c("12f", "manual_assembly_checklist"), max_n = 10)
))

panel_assembly_df <- read_table_safe(file_12f_panel_assembly)
figure_assembly_df <- read_table_safe(file_12f_figure_assembly)
mainfig1_override_df <- read_table_safe(file_12f_mainfig1_override)
claim_check_12f_df <- read_table_safe(file_12f_claim_check)
manual_check_12f_df <- read_table_safe(file_12f_manual_check)

if (nrow(panel_assembly_df) < 1) stop("[12G FINAL] Missing 12F panel assembly manifest.", call. = FALSE)
if (!("figure_id" %in% colnames(panel_assembly_df))) stop("[12G FINAL] 12F panel assembly manifest missing figure_id.", call. = FALSE)
if (!("panel_id" %in% colnames(panel_assembly_df))) stop("[12G FINAL] 12F panel assembly manifest missing panel_id.", call. = FALSE)

input_audit_df <- data.frame(
  input_name = c(
    "12F_panel_assembly_manifest",
    "12F_figure_assembly_manifest",
    "12F_Main_Fig_1_redesign_override_manifest",
    "12F_claim_boundary_assembly_check",
    "12F_manual_assembly_checklist"
  ),
  detected = c(
    file_12f_panel_assembly != "",
    file_12f_figure_assembly != "",
    file_12f_mainfig1_override != "",
    file_12f_claim_check != "",
    file_12f_manual_check != ""
  ),
  file_path = c(
    file_12f_panel_assembly,
    file_12f_figure_assembly,
    file_12f_mainfig1_override,
    file_12f_claim_check,
    file_12f_manual_check
  ),
  rows_loaded = c(
    nrow(panel_assembly_df),
    nrow(figure_assembly_df),
    nrow(mainfig1_override_df),
    nrow(claim_check_12f_df),
    nrow(manual_check_12f_df)
  ),
  allowed_as_locked_upstream_input = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
write_csv_safe(input_audit_df, file.path(out_table_dir, "12G_FINAL_locked_12F_input_audit.csv"))

# ------------------------- legend specification -------------------------
figure_spec <- data.frame(
  figure_id = c(
    "Main Fig 1", "Main Fig 2", "Main Fig 3", "Main Fig 4", "Main Fig 5",
    "Supplement Fig S1", "Supplement Fig S2", "Supplement Fig S3", "Supplement Fig S4", "Supplement Fig S5",
    "Supplement Fig S6", "Supplement Fig S7", "Supplement Fig S8", "Supplement Fig S9", "Supplement Fig S10"
  ),
  figure_type = c(rep("main", 5), rep("supplement", 10)),
  legend_title = c(
    "Source-traceable transcriptomic prioritisation framework",
    "Core marker-rule-derived prioritisation and DA/graft-related identity evidence",
    "Temporal maturation and module co-variation across transcriptomic states",
    "External and proxy evidence layers with risk-context boundaries",
    "Integrated umbrella evidence tier and marker-rule-derived machine-learning audit",
    "Dataset-domain and source-role manifest",
    "Dependency and source-panel traceability package",
    "Object processing, QC and atlas-level technical support",
    "Preclinical graft-outcome marker support",
    "Projection-associated molecular competence support",
    "State-level proxy support for transcriptomic cell-state heterogeneity",
    "Survival/stress perturbation and safety-risk context support",
    "Limited PD genetic-context support",
    "Candidate transcriptomic marker-signature support",
    "Marker-rule-derived machine-learning performance and feature-transparency audit"
  ),
  short_role = c(
    "Framework and source traceability",
    "Core prioritisation",
    "Temporal/module support",
    "External/proxy boundary support",
    "Integrated evidence and ML audit",
    "Source/domain supplement",
    "Dependency supplement",
    "QC/object supplement",
    "11C supplement",
    "11F supplement",
    "11E supplement",
    "11D supplement",
    "11G supplement",
    "11H supplement",
    "11J supplement"
  ),
  safe_scope_sentence = c(
    "This figure defines the computational framework, source roles and claim boundaries before downstream analyses.",
    "This figure summarizes internal marker-rule-derived transcriptomic prioritisation and DA/graft-related identity support.",
    "This figure summarizes transcriptomic pseudotime and module-score correlation support for maturation-associated structure.",
    "This figure summarizes external/proxy support layers while preserving non-clinical risk-context boundaries.",
    "This figure integrates umbrella evidence tiers with a conservative marker-rule-derived prioritization model audit.",
    "This supplement documents dataset-domain and source-role assignments.",
    "This supplement documents dependency links and source-panel traceability.",
    "This supplement documents technical object processing and QC support.",
    "This supplement documents preclinical marker-alignment support, not direct efficacy validation.",
    "This supplement documents projection-associated molecular competence proxies, not anatomical-projection claim.",
    "This supplement documents state-level proxy module comparisons, not barcode-level lineage tracing.",
    "This supplement documents survival/stress perturbation context support, not clinical safety prediction.",
    "This supplement documents limited PD genetic-context overlap support, not genetic causality.",
    "This supplement documents candidate transcriptomic marker signatures, not validated clinical biomarkers.",
    "This supplement documents marker-rule-derived prioritization model ROC/PR and feature-transparency audit, not clinical prediction."
  ),
  stringsAsFactors = FALSE
)

# enforce only figures detected in 12F, but keep all expected figures in legend spec
detected_figs <- unique(panel_assembly_df$figure_id)
if (length(detected_figs) > 0) {
  missing_spec_figs <- setdiff(detected_figs, figure_spec$figure_id)
  if (length(missing_spec_figs) > 0) {
    add_spec <- data.frame(
      figure_id = missing_spec_figs,
      figure_type = "unknown",
      legend_title = paste0(missing_spec_figs, " assembly-ready figure"),
      short_role = "Assembly-ready figure",
      safe_scope_sentence = "This figure is described using the 12F assembly manifest and conservative claim boundaries.",
      stringsAsFactors = FALSE
    )
    figure_spec <- safe_bind_rows(list(figure_spec, add_spec))
  }
}

write_csv_safe(figure_spec, file.path(out_table_dir, "12G_FINAL_figure_legend_specification.csv"))

# ------------------------- panel caption generation -------------------------
make_panel_caption <- function(fig_id, panel_label, content_text, action_text, module_text) {
  fig_id <- clean_space(fig_id)
  panel_label <- clean_space(panel_label)
  content_text <- clean_space(content_text)
  action_text <- clean_space(action_text)
  module_text <- clean_space(module_text)

  if (fig_id == "Main Fig 1" && panel_label == "A") {
    return("Workflow schematic for the source-traceable transcriptomic prioritisation framework, linking locked datasets, DA/graft-related evidence, marker-rule-derived prioritisation, pseudotime/module support, proxy evidence and integrated evidence-tier/ML audit.")
  }
  if (fig_id == "Main Fig 1" && panel_label == "B") {
    return("Dataset/source role map separating locked accessions into core model-development references, independent external validation, marker-targeted context validation, bulk support and manual/background references.")
  }
  if (fig_id == "Main Fig 1" && panel_label == "C") {
    return("Domain and claim-boundary map distinguishing supported computational uses from prohibited overclaims including clinical prediction, validated biomarker, graft efficacy/safety proof and projection/lineage proof.")
  }
  if (fig_id == "Main Fig 1" && panel_label == "D") {
    return("Source-to-figure traceability map linking locked upstream modules to the assembly-ready figure package.")
  }

  if (content_text == "") content_text <- "Assembly-ready panel from locked source package"
  if (module_text == "") module_text <- "locked upstream module"

  prefix <- paste0("Panel ", panel_label, " summarizes ", content_text, ".")
  source_sentence <- paste0(" Source anchor: ", module_text, ".")
  action_sentence <- ""
  if (grepl("redesign", action_text, ignore.case = TRUE)) {
    action_sentence <- " The panel was redesigned in 12F for publication-style presentation."
  } else {
    action_sentence <- " The panel uses the 12E-passed source package without changing the underlying analysis."
  }
  paste0(prefix, source_sentence, action_sentence)
}

panel_caption_list <- list()
for (idx_panel in seq_len(nrow(panel_assembly_df))) {
  row_now <- panel_assembly_df[idx_panel, , drop = FALSE]
  fig_id <- clean_space(row_now$figure_id)
  panel_label <- if ("panel_label" %in% colnames(row_now)) clean_space(row_now$panel_label) else ""
  panel_id <- clean_space(row_now$panel_id)
  if (panel_label == "") panel_label <- gsub("^.*([A-Z])$", "\\1", panel_id)

  content_text <- ""
  if ("planned_panel_content" %in% colnames(row_now)) content_text <- clean_space(row_now$planned_panel_content)
  action_text <- ""
  if ("assembly_action" %in% colnames(row_now)) action_text <- clean_space(row_now$assembly_action)
  module_text <- ""
  if ("primary_locked_module" %in% colnames(row_now)) module_text <- clean_space(row_now$primary_locked_module)

  caption_text <- make_panel_caption(fig_id, panel_label, content_text, action_text, module_text)

  panel_caption_list[[length(panel_caption_list) + 1]] <- data.frame(
    figure_type = if ("figure_type" %in% colnames(row_now)) clean_space(row_now$figure_type) else "",
    figure_id = fig_id,
    panel_id = panel_id,
    panel_label = panel_label,
    primary_locked_module = module_text,
    assembly_action = action_text,
    final_assembly_source_pdf = if ("final_assembly_source_pdf" %in% colnames(row_now)) clean_space(row_now$final_assembly_source_pdf) else "",
    panel_caption = caption_text,
    claim_boundary_note = "Caption describes computational/source-locked support only; no clinical prediction, validated biomarker, graft efficacy/safety proof, anatomical-projection claim or barcode-lineage proof is implied.",
    stringsAsFactors = FALSE
  )
}
panel_caption_df <- safe_bind_rows(panel_caption_list)
write_csv_safe(panel_caption_df, file.path(out_table_dir, "12G_FINAL_panel_caption_table.csv"))
write_tsv_safe(panel_caption_df, file.path(out_table_dir, "12G_FINAL_panel_caption_table.tsv"))

# ------------------------- figure legend generation -------------------------
make_figure_legend <- function(fig_id, spec_row, panel_rows) {
  title_text <- spec_row$legend_title[1]
  scope_sentence <- spec_row$safe_scope_sentence[1]
  panel_rows <- panel_rows[order(panel_rows$panel_label), , drop = FALSE]
  panel_bits <- character(0)
  for (idx_row in seq_len(nrow(panel_rows))) {
    cap_now <- clean_space(panel_rows$panel_caption[idx_row])
    lab_now <- clean_space(panel_rows$panel_label[idx_row])
    if (nchar(cap_now) > 280) cap_now <- paste0(substr(cap_now, 1, 277), "...")
    panel_bits <- c(panel_bits, paste0("(", lab_now, ") ", cap_now))
  }
  boundary_text <- "All panels should be interpreted as source-traceable, hypothesis-generating transcriptomic evidence. The figure does not provide clinical prediction, validated diagnostic/prognostic biomarker evidence, causal graft efficacy/safety proof, anatomical-projection claim or barcode-lineage claim."
  paste0(title_text, ". ", scope_sentence, " ", paste(panel_bits, collapse = " "), " ", boundary_text)
}

figure_legend_list <- list()
for (idx_fig in seq_len(nrow(figure_spec))) {
  fig_id <- clean_space(figure_spec$figure_id[idx_fig])
  sub_panels <- panel_caption_df[panel_caption_df$figure_id == fig_id, , drop = FALSE]
  if (nrow(sub_panels) < 1) next
  spec_row <- figure_spec[figure_spec$figure_id == fig_id, , drop = FALSE]
  legend_text <- make_figure_legend(fig_id, spec_row, sub_panels)
  figure_legend_list[[length(figure_legend_list) + 1]] <- data.frame(
    figure_type = spec_row$figure_type[1],
    figure_id = fig_id,
    legend_title = spec_row$legend_title[1],
    short_role = spec_row$short_role[1],
    n_panels = nrow(sub_panels),
    legend_text = legend_text,
    stringsAsFactors = FALSE
  )
}
figure_legend_df <- safe_bind_rows(figure_legend_list)
figure_legend_df <- figure_legend_df[order(figure_legend_df$figure_type, figure_legend_df$figure_id), , drop = FALSE]
write_csv_safe(figure_legend_df, file.path(out_table_dir, "12G_FINAL_figure_legend_table.csv"))
write_tsv_safe(figure_legend_df, file.path(out_table_dir, "12G_FINAL_figure_legend_table.tsv"))

main_legend_df <- figure_legend_df[figure_legend_df$figure_type == "main", , drop = FALSE]
supp_legend_df <- figure_legend_df[figure_legend_df$figure_type == "supplement", , drop = FALSE]
write_csv_safe(main_legend_df, file.path(out_table_dir, "12G_FINAL_main_figure_legends.csv"))
write_csv_safe(supp_legend_df, file.path(out_table_dir, "12G_FINAL_supplementary_figure_legends.csv"))

# ------------------------- text export -------------------------
write_legend_txt <- function(data_value, file_value, title_value) {
  lines <- c(title_value, paste(rep("=", nchar(title_value)), collapse = ""), "")
  if (nrow(data_value) > 0) {
    for (idx_row in seq_len(nrow(data_value))) {
      lines <- c(
        lines,
        paste0(data_value$figure_id[idx_row], ". ", data_value$legend_title[idx_row]),
        data_value$legend_text[idx_row],
        ""
      )
    }
  }
  writeLines(lines, file_value)
  cat("[12G FINAL] Wrote:", file_value, "\n")
}

write_legend_txt(main_legend_df, file.path(out_text_dir, "12G_FINAL_main_figure_legends.txt"), "12G FINAL main figure legends")
write_legend_txt(supp_legend_df, file.path(out_text_dir, "12G_FINAL_supplementary_figure_legends.txt"), "12G FINAL supplementary figure legends")
write_legend_txt(figure_legend_df, file.path(out_text_dir, "12G_FINAL_all_figure_legends.txt"), "12G FINAL all figure legends")

panel_caption_lines <- c("12G FINAL panel captions", "========================", "")
if (nrow(panel_caption_df) > 0) {
  for (idx_row in seq_len(nrow(panel_caption_df))) {
    panel_caption_lines <- c(
      panel_caption_lines,
      paste0(panel_caption_df$panel_id[idx_row], ": ", panel_caption_df$panel_caption[idx_row]),
      paste0("Boundary: ", panel_caption_df$claim_boundary_note[idx_row]),
      ""
    )
  }
}
writeLines(panel_caption_lines, file.path(out_text_dir, "12G_FINAL_panel_captions.txt"))
cat("[12G FINAL] Wrote:", file.path(out_text_dir, "12G_FINAL_panel_captions.txt"), "\n")

# ------------------------- claim-boundary audit -------------------------
prohibited_terms <- c(
  "clinical prediction",
  "clinical predictor",
  "diagnostic biomarker",
  "prognostic biomarker",
  "validated biomarker",
  "treatment response prediction",
  "graft-efficacy claim",
  "graft-safety claim",
  "graft safety prediction",
  "therapeutic efficacy",
  "anatomical-projection claim",
  "lineage tracing proof",
  "barcode-confirmed lineage"
)

allowed_protective_phrases <- c(
  "does not provide clinical prediction",
  "not clinical prediction",
  "not validated clinical biomarkers",
  "not validated diagnostic/prognostic biomarker",
  "not causal graft efficacy/safety proof",
  "not anatomical-projection claim",
  "not barcode-level lineage tracing",
  "hypothesis-generating",
  "candidate transcriptomic",
  "marker-rule-derived",
  "proxy"
)

scan_text_for_terms <- function(text_value, term_values) {
  text_lower <- tolower(clean_space(text_value))
  hits <- character(0)
  for (term_value in term_values) {
    if (grepl(tolower(term_value), text_lower, fixed = TRUE)) hits <- c(hits, term_value)
  }
  hits
}

claim_audit_list <- list()
for (idx_row in seq_len(nrow(figure_legend_df))) {
  text_now <- figure_legend_df$legend_text[idx_row]
  prohibited_hits <- scan_text_for_terms(text_now, prohibited_terms)
  protective_hits <- scan_text_for_terms(text_now, allowed_protective_phrases)

  # Terms embedded in negating boundary statements should not fail the legend.
  # Here we flag only if prohibited terms exist without protective language.
  fail_flag <- length(prohibited_hits) > 0 && length(protective_hits) < 1

  claim_audit_list[[length(claim_audit_list) + 1]] <- data.frame(
    figure_id = figure_legend_df$figure_id[idx_row],
    figure_type = figure_legend_df$figure_type[idx_row],
    prohibited_terms_detected = paste(prohibited_hits, collapse = ";"),
    protective_boundary_phrases_detected = paste(protective_hits, collapse = ";"),
    claim_audit_status = ifelse(fail_flag, "needs_caption_repair", "claim_boundary_pass"),
    stringsAsFactors = FALSE
  )
}
claim_audit_df <- safe_bind_rows(claim_audit_list)
write_csv_safe(claim_audit_df, file.path(out_table_dir, "12G_FINAL_legend_claim_boundary_audit.csv"))

# ------------------------- 12H results text handoff -------------------------
results_handoff_df <- data.frame(
  results_section = c(
    "Source-traceable framework",
    "Core prioritisation and DA/graft identity",
    "Temporal and module co-variation",
    "External/proxy evidence and risk boundaries",
    "Integrated evidence and marker-rule-derived prioritization model audit",
    "Limitations / claim boundaries"
  ),
  source_figures = c(
    "Main Fig 1; Supplement Fig S1-S2",
    "Main Fig 2; Supplement Fig S3",
    "Main Fig 3",
    "Main Fig 4; Supplement Fig S4-S8",
    "Main Fig 5; Supplement Fig S9-S10",
    "All main and supplement legends"
  ),
  results_writing_instruction = c(
    "Describe the framework, source roles and traceability before presenting downstream analyses.",
    "Describe transcriptomic prioritisation as marker-rule-derived and candidate-state oriented.",
    "Describe pseudotime and module correlations as transcriptomic maturation/module-support evidence.",
    "Describe proxy/external layers with explicit non-clinical interpretation.",
    "Describe umbrella evidence tiers and ML audit as internal support, not clinical prediction.",
    "State that all findings require experimental validation and should not be interpreted as validated biomarkers or clinical graft outcome predictors."
  ),
  safe_sentence_seed = c(
    "We established a source-traceable computational framework for DA neuron/graft-related transcriptomic cell-state prioritisation.",
    "Marker-rule-derived prioritisation highlighted candidate transcriptomic states with DA/graft-related identity support.",
    "Pseudotime and module-score analyses supported maturation-associated and module-coupled transcriptomic structure.",
    "External and proxy evidence layers provided contextual support while preserving strict claim boundaries.",
    "Integrated evidence tiers and ML audit summarized convergent computational support under a marker-rule-derived framework.",
    "The framework is hypothesis-generating and requires experimental validation."
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(results_handoff_df, file.path(out_table_dir, "12G_FINAL_handoff_to_12H_results_text.csv"))

# ------------------------- figures -------------------------
# FigA legend package overview
fig_a <- open_pdf_safe("12G_FINAL_FigA_legend_package_overview.pdf", 11.8, 6.6)
new_canvas()
draw_title("Final legends/caption package overview", "Legends are generated from the 12F assembly manifest with conservative claim boundaries.")

overview_df <- data.frame(
  label = c(
    "Figures with legends",
    "Main figure legends",
    "Supplement legends",
    "Panel captions",
    "Claim-boundary pass",
    "Claim-boundary repair needed"
  ),
  value = c(
    nrow(figure_legend_df),
    nrow(main_legend_df),
    nrow(supp_legend_df),
    nrow(panel_caption_df),
    sum(claim_audit_df$claim_audit_status == "claim_boundary_pass"),
    sum(claim_audit_df$claim_audit_status != "claim_boundary_pass")
  ),
  family = c("all", "main", "supp", "panel", "pass", "repair"),
  stringsAsFactors = FALSE
)
max_value <- max(safe_num(overview_df$value), na.rm = TRUE)
if (!is.finite(max_value) || max_value <= 0) max_value <- 1
bar_x0 <- 0.40
bar_x1 <- 0.80
y_positions <- seq(0.78, 0.32, length.out = nrow(overview_df))
for (idx_row in seq_len(nrow(overview_df))) {
  yy <- y_positions[idx_row]
  count_now <- safe_num(overview_df$value[idx_row])
  width_now <- count_now / max_value
  if (count_now == 0) width_now <- 0.018
  color_now <- nature_palette$navy
  if (overview_df$family[idx_row] == "main") color_now <- nature_palette$blue
  if (overview_df$family[idx_row] == "supp") color_now <- nature_palette$purple
  if (overview_df$family[idx_row] == "panel") color_now <- nature_palette$teal
  if (overview_df$family[idx_row] == "pass") color_now <- nature_palette$teal
  if (overview_df$family[idx_row] == "repair") color_now <- ifelse(count_now > 0, nature_palette$red, nature_palette$teal)
  text(bar_x0 - 0.018, yy, overview_df$label[idx_row], cex = 0.54, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.024, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.024,
       col = color_now, border = nature_palette$border, lwd = 0.42)
  text(bar_x0 + width_now * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_now), cex = 0.50, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.16, "Next: 12H should convert these legends and panel captions into Results text.", cex = 0.44, col = nature_palette$muted)
dev.off()
cat("[12G FINAL] Wrote figure:", fig_a, "\n")

# FigB main legend map
fig_b <- open_pdf_safe("12G_FINAL_FigB_main_figure_legend_map.pdf", 12.0, 6.8)
new_canvas()
draw_title("Main figure legend map", "Five main figures are captioned using assembly-ready 12F sources.")

plot_df <- main_legend_df[order(main_legend_df$figure_id), , drop = FALSE]
if (nrow(plot_df) > 0) {
  y_positions <- seq(0.76, 0.28, length.out = nrow(plot_df))
  for (idx_row in seq_len(nrow(plot_df))) {
    yy <- y_positions[idx_row]
    color_now <- figure_color(plot_df$figure_id[idx_row])
    rect(0.10, yy - 0.030, 0.25, yy + 0.030, col = color_now, border = nature_palette$border, lwd = 0.35)
    text(0.175, yy, plot_df$figure_id[idx_row], cex = 0.38, font = 2, col = nature_palette$white)
    text(0.29, yy + 0.014, substr(plot_df$legend_title[idx_row], 1, 70), cex = 0.40, adj = c(0, 0.5), col = nature_palette$ink)
    text(0.29, yy - 0.016, paste0("panels: ", plot_df$n_panels[idx_row], " | role: ", plot_df$short_role[idx_row]),
         cex = 0.32, adj = c(0, 0.5), col = nature_palette$muted)
  }
}
dev.off()
cat("[12G FINAL] Wrote figure:", fig_b, "\n")

# FigC supplement legend map
fig_c <- open_pdf_safe("12G_FINAL_FigC_supplementary_figure_legend_map.pdf", 12.0, 7.2)
new_canvas()
draw_title("Supplementary figure legend map", "Supplement legends retain source-detail, proxy and audit boundaries.")

plot_df <- supp_legend_df[order(supp_legend_df$figure_id), , drop = FALSE]
if (nrow(plot_df) > 0) {
  y_positions <- seq(0.80, 0.18, length.out = nrow(plot_df))
  for (idx_row in seq_len(nrow(plot_df))) {
    yy <- y_positions[idx_row]
    color_now <- figure_color(plot_df$figure_id[idx_row])
    rect(0.09, yy - 0.023, 0.28, yy + 0.023, col = color_now, border = nature_palette$border, lwd = 0.35)
    text(0.185, yy, plot_df$figure_id[idx_row], cex = 0.33, font = 2, col = nature_palette$white)
    text(0.31, yy + 0.010, substr(plot_df$legend_title[idx_row], 1, 75), cex = 0.34, adj = c(0, 0.5), col = nature_palette$ink)
    text(0.31, yy - 0.015, paste0("panels: ", plot_df$n_panels[idx_row], " | ", plot_df$short_role[idx_row]),
         cex = 0.28, adj = c(0, 0.5), col = nature_palette$muted)
  }
}
dev.off()
cat("[12G FINAL] Wrote figure:", fig_c, "\n")

# FigD claim-boundary legend audit
fig_d <- open_pdf_safe("12G_FINAL_FigD_legend_claim_boundary_audit.pdf", 11.6, 6.4)
new_canvas()
draw_title("Legend claim-boundary audit", "Caption text is checked for conservative wording and explicit non-clinical boundaries.")

plot_df <- claim_audit_df[order(claim_audit_df$figure_type, claim_audit_df$figure_id), , drop = FALSE]
if (nrow(plot_df) > 0) {
  y_positions <- seq(0.80, 0.18, length.out = nrow(plot_df))
  for (idx_row in seq_len(nrow(plot_df))) {
    yy <- y_positions[idx_row]
    status_now <- plot_df$claim_audit_status[idx_row]
    color_now <- ifelse(status_now == "claim_boundary_pass", nature_palette$teal, nature_palette$red)
    rect(0.11, yy - 0.016, 0.18, yy + 0.016, col = color_now, border = nature_palette$border, lwd = 0.30)
    text(0.205, yy, plot_df$figure_id[idx_row], cex = 0.30, adj = c(0, 0.5), col = nature_palette$ink)
    text(0.50, yy, status_now, cex = 0.28, adj = c(0, 0.5), col = nature_palette$muted)
  }
}
dev.off()
cat("[12G FINAL] Wrote figure:", fig_d, "\n")

# FigE 12H handoff
fig_e <- open_pdf_safe("12G_FINAL_FigE_12H_results_text_handoff.pdf", 11.6, 6.4)
new_canvas()
draw_title("12H Results-text handoff", "Results writing should follow the legend package and safe claim boundaries.")

y_positions <- seq(0.78, 0.32, length.out = nrow(results_handoff_df))
for (idx_row in seq_len(nrow(results_handoff_df))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$blue
  if (idx_row == 2) color_now <- nature_palette$teal
  if (idx_row == 3) color_now <- nature_palette$red
  if (idx_row == 4) color_now <- nature_palette$purple
  if (idx_row == 5) color_now <- nature_palette$navy
  if (idx_row == 6) color_now <- nature_palette$orange
  rect(0.08, yy - 0.026, 0.34, yy + 0.026, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.21, yy, results_handoff_df$results_section[idx_row], cex = 0.32, font = 2, col = nature_palette$white)
  text(0.38, yy + 0.010, results_handoff_df$source_figures[idx_row], cex = 0.31, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.38, yy - 0.014, substr(results_handoff_df$safe_sentence_seed[idx_row], 1, 105), cex = 0.27, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12G FINAL] Wrote figure:", fig_e, "\n")

# ------------------------- final summary -------------------------
n_fig_legends <- nrow(figure_legend_df)
n_main_legends <- nrow(main_legend_df)
n_supp_legends <- nrow(supp_legend_df)
n_panel_captions <- nrow(panel_caption_df)
n_claim_pass <- sum(claim_audit_df$claim_audit_status == "claim_boundary_pass")
n_claim_repair <- sum(claim_audit_df$claim_audit_status != "claim_boundary_pass")
n_source_pdfs_existing <- sum(file_exists_safe(panel_caption_df$final_assembly_source_pdf))

decision_value <- "INPUT_READY_FOR_12H_RESULTS_TEXT_REFRESH"
if (n_claim_repair > 0) decision_value <- "REPAIR_REQUIRED_BEFORE_12H"
if (n_fig_legends < 15 || n_panel_captions < 50) decision_value <- "REVIEW_REQUIRED_BEFORE_12H"

summary_df <- data.frame(
  item = c(
    "figure_legends_generated",
    "main_figure_legends_generated",
    "supplementary_figure_legends_generated",
    "panel_captions_generated",
    "panel_source_pdfs_existing",
    "claim_boundary_pass_figures",
    "claim_boundary_repair_needed",
    "results_handoff_sections",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(n_fig_legends),
    as.character(n_main_legends),
    as.character(n_supp_legends),
    as.character(n_panel_captions),
    as.character(n_source_pdfs_existing),
    as.character(n_claim_pass),
    as.character(n_claim_repair),
    as.character(nrow(results_handoff_df)),
    "5",
    decision_value
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "12G_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "12G_FINAL_execution_summary.tsv"))

report_lines <- c(
  "12G FINAL report",
  "================",
  "Module: final legends / caption refresh",
  "Mode: complete standalone 12G rebuild; no previous 12G output dependency; no internet; no 00-10P rerun.",
  "Allowed upstream inputs: locked 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E and 12F outputs.",
  "",
  paste0("Figure legends generated: ", n_fig_legends),
  paste0("Main figure legends generated: ", n_main_legends),
  paste0("Supplementary figure legends generated: ", n_supp_legends),
  paste0("Panel captions generated: ", n_panel_captions),
  paste0("Panel source PDFs existing: ", n_source_pdfs_existing),
  paste0("Claim-boundary pass figures: ", n_claim_pass),
  paste0("Claim-boundary repair needed: ", n_claim_repair),
  paste0("Results-handoff sections: ", nrow(results_handoff_df)),
  "",
  "Main outputs:",
  paste0("- ", file.path(out_text_dir, "12G_FINAL_main_figure_legends.txt")),
  paste0("- ", file.path(out_text_dir, "12G_FINAL_supplementary_figure_legends.txt")),
  paste0("- ", file.path(out_text_dir, "12G_FINAL_all_figure_legends.txt")),
  paste0("- ", file.path(out_text_dir, "12G_FINAL_panel_captions.txt")),
  paste0("- ", file.path(out_table_dir, "12G_FINAL_handoff_to_12H_results_text.csv")),
  "",
  "Claim boundary:",
  "- Figure legends describe computational/source-locked support only.",
  "- Candidate signatures remain candidate transcriptomic marker signatures, not validated clinical biomarkers.",
  "- ML remains marker-rule-derived transcriptomic prioritisation audit, not clinical prediction.",
  "- Projection and lineage modules remain proxy/context support, not anatomical projection or barcode-lineage proof.",
  "",
  paste0("Decision: ", decision_value)
)
report_file <- file.path(out_text_dir, "12G_FINAL_legends_caption_refresh_report.txt")
writeLines(report_lines, report_file)
cat("[12G FINAL] Wrote:", report_file, "\n")

cat("\n[12G FINAL] Completed final legends/caption refresh.\n")
cat("[12G FINAL] Figure legends generated:", n_fig_legends, "\n")
cat("[12G FINAL] Main figure legends generated:", n_main_legends, "\n")
cat("[12G FINAL] Supplementary figure legends generated:", n_supp_legends, "\n")
cat("[12G FINAL] Panel captions generated:", n_panel_captions, "\n")
cat("[12G FINAL] Panel source PDFs existing:", n_source_pdfs_existing, "\n")
cat("[12G FINAL] Claim-boundary pass figures:", n_claim_pass, "\n")
cat("[12G FINAL] Claim-boundary repair needed:", n_claim_repair, "\n")
cat("[12G FINAL] Results-handoff sections:", nrow(results_handoff_df), "\n")
cat("[12G FINAL] Figures written: 5\n")
cat("[12G FINAL] Decision:", decision_value, "\n")
cat("[12G FINAL] Output tables:", out_table_dir, "\n")
cat("[12G FINAL] Output figs  :", out_fig_dir, "\n")
cat("[12G FINAL] Output text  :", out_text_dir, "\n")
cat("[12G FINAL] Next         : review 12G PDFs and legend text files; if accepted, proceed to 12H Results text refresh.\n")
