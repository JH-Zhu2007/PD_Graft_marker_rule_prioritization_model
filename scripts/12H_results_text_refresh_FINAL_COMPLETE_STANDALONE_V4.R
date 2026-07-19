
cat("\n[12H FINAL V4] Starting Results text refresh with full claim-boundary pass repair...\n")
cat("[12H FINAL] Mode: complete standalone 12H rebuild; no previous 12H dependency; no internet; no 00-10P rerun.\n")
cat("[12H FINAL] Inputs allowed: locked upstream 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F and 12G outputs.\n")
cat("[12H FINAL] Formal input: 12G legends/captions and 12H handoff.\n")
cat("[12H FINAL] Claim boundary: Results text only; no clinical prediction or validated biomarker claim.\n")

graphics.off()

project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")
figure_root <- file.path(project_root, "04_figures")
text_root <- file.path(project_root, "09_manuscript")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "12H_results_text_refresh_FINAL_COMPLETE_STANDALONE_V4"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "12H_results_text_refresh_FINAL_COMPLETE_STANDALONE_V4_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "12H_results_text_refresh_FINAL_COMPLETE_STANDALONE_V4"
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
  cat("[12H FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[12H FINAL] Wrote:", file_value, "\n")
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
    cat("[12H FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
    return(file_alt)
  }
  file_primary
}

new_canvas <- function() {
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", xpd = FALSE)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
}

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

if (!dir.exists(table_root)) stop("[12H FINAL] Missing table root: ", table_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
table_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(table_info$size) & table_info$size > 0 & table_info$size < 220 * 1024 * 1024]

all_table_files <- all_table_files[!grepl("12H_results_text_refresh", all_table_files, ignore.case = TRUE)]

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

file_12g_handoff <- first_existing_file(c(
  file.path(table_root, "12G_final_legends_caption_refresh_FINAL_COMPLETE_STANDALONE", "12G_FINAL_handoff_to_12H_results_text.csv"),
  find_files_all_terms(all_table_files, c("12g", "handoff_to_12h_results_text"), max_n = 10)
))
file_12g_figure_legends <- first_existing_file(c(
  file.path(table_root, "12G_final_legends_caption_refresh_FINAL_COMPLETE_STANDALONE", "12G_FINAL_figure_legend_table.csv"),
  find_files_all_terms(all_table_files, c("12g", "figure_legend_table"), max_n = 10)
))
file_12g_panel_captions <- first_existing_file(c(
  file.path(table_root, "12G_final_legends_caption_refresh_FINAL_COMPLETE_STANDALONE", "12G_FINAL_panel_caption_table.csv"),
  find_files_all_terms(all_table_files, c("12g", "panel_caption_table"), max_n = 10)
))
file_12g_claim_audit <- first_existing_file(c(
  file.path(table_root, "12G_final_legends_caption_refresh_FINAL_COMPLETE_STANDALONE", "12G_FINAL_legend_claim_boundary_audit.csv"),
  find_files_all_terms(all_table_files, c("12g", "legend_claim_boundary_audit"), max_n = 10)
))
file_12f_assembly <- first_existing_file(c(
  file.path(table_root, "12F_optional_final_assembly_MAIN_FIG1_REDESIGN_FINAL_COMPLETE_STANDALONE_V2", "12F_FINAL_figure_assembly_manifest.csv"),
  find_files_all_terms(all_table_files, c("12f", "figure_assembly_manifest"), max_n = 10)
))

handoff_12g_df <- read_table_safe(file_12g_handoff)
figure_legend_df <- read_table_safe(file_12g_figure_legends)
panel_caption_df <- read_table_safe(file_12g_panel_captions)
claim_audit_12g_df <- read_table_safe(file_12g_claim_audit)
figure_assembly_12f_df <- read_table_safe(file_12f_assembly)

if (nrow(handoff_12g_df) < 1) stop("[12H FINAL] Missing 12G handoff to 12H table.", call. = FALSE)
if (nrow(figure_legend_df) < 1) stop("[12H FINAL] Missing 12G figure legend table.", call. = FALSE)
if (nrow(panel_caption_df) < 1) stop("[12H FINAL] Missing 12G panel caption table.", call. = FALSE)

input_audit_df <- data.frame(
  input_name = c(
    "12G_handoff_to_12H_results_text",
    "12G_figure_legend_table",
    "12G_panel_caption_table",
    "12G_legend_claim_boundary_audit",
    "12F_figure_assembly_manifest"
  ),
  detected = c(
    file_12g_handoff != "",
    file_12g_figure_legends != "",
    file_12g_panel_captions != "",
    file_12g_claim_audit != "",
    file_12f_assembly != ""
  ),
  file_path = c(
    file_12g_handoff,
    file_12g_figure_legends,
    file_12g_panel_captions,
    file_12g_claim_audit,
    file_12f_assembly
  ),
  rows_loaded = c(
    nrow(handoff_12g_df),
    nrow(figure_legend_df),
    nrow(panel_caption_df),
    nrow(claim_audit_12g_df),
    nrow(figure_assembly_12f_df)
  ),
  allowed_as_locked_upstream_input = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
write_csv_safe(input_audit_df, file.path(out_table_dir, "12H_FINAL_locked_12G_input_audit.csv"))

results_section_plan <- data.frame(
  section_order = 1:6,
  results_section_id = c(
    "R1_framework_source_traceability",
    "R2_core_prioritisation_DA_graft_identity",
    "R3_temporal_module_covariation",
    "R4_external_proxy_risk_boundaries",
    "R5_integrated_evidence_ML_audit",
    "R6_results_boundary_summary"
  ),
  manuscript_subheading = c(
    "A source-traceable framework for DA neuron and graft-related transcriptomic prioritisation",
    "Marker-rule-derived transcriptomic prioritisation identifies DA/graft-related candidate cell states",
    "Pseudotime and module-score analyses support maturation-associated transcriptomic structure",
    "External and proxy evidence layers provide contextual support under strict claim boundaries",
    "Integrated umbrella evidence tiers and marker-rule-derived prioritization model audit summarize convergent support",
    "Results-level claim boundaries and validation requirements"
  ),
  main_figures = c(
    "Main Fig. 1",
    "Main Fig. 2",
    "Main Fig. 3",
    "Main Fig. 4",
    "Main Fig. 5",
    "All main and supplementary figures"
  ),
  supplementary_figures = c(
    "Supplementary Figs. S1-S2",
    "Supplementary Fig. S3",
    "None",
    "Supplementary Figs. S4-S8",
    "Supplementary Figs. S9-S10",
    "All supplementary figures"
  ),
  safe_claim_boundary = c(
    "framework/source traceability only",
    "marker-rule-derived transcriptomic prioritisation only",
    "transcriptomic pseudotime/module support only",
    "external/proxy context support only",
    "umbrella evidence and internal ML audit only",
    "hypothesis-generating; requires experimental validation"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(results_section_plan, file.path(out_table_dir, "12H_FINAL_results_section_plan.csv"))

results_blocks <- data.frame(
  section_order = results_section_plan$section_order,
  results_section_id = results_section_plan$results_section_id,
  manuscript_subheading = results_section_plan$manuscript_subheading,
  results_text = c(
    paste(
      "We first organized the study as a computational, source-traceable framework for dopaminergic neuron and graft-related transcriptomic cell-state prioritisation.",
      "The final assembly separated dataset roles before downstream interpretation, including 12 locked accessions, 5 core model-development references, 1 independent external validation dataset, 1 marker-targeted context-validation dataset, 1 bulk-support dataset that was not used for scRNA model training, and 4 manual-review or background references.",
      "Main Fig. 1 was therefore redesigned as a framework schematic rather than a table preview, with panels describing the workflow, dataset/source-role map, claim-boundary map and source-to-figure traceability.",
      "This section is computational and hypothesis-generating, and it remains outside clinical, diagnostic or therapeutic decision use."
    ),
    paste(
      "Using the locked core analysis layers, we next summarized the marker-rule-derived transcriptomic prioritisation framework and DA/graft-related identity evidence.",
      "The analysis is interpreted as candidate transcriptomic cell-state prioritisation, where DA identity, A9/A10-like functional identity, projection-associated molecular competence and safety/risk-context scores are used to organize cell-state evidence.",
      "These results support a candidate high-priority transcriptional state concept under a marker-rule-derived computational framework.",
      "This section is hypothesis-generating and does not make patient-level outcome, graft-function or therapeutic-performance claims."
    ),
    paste(
      "We then examined temporal and module-level structure using graph-based pseudotime and module-score relationships.",
      "The locked pseudotime analysis used multi-timepoint GSE204796 differentiation data and showed that transcriptomic pseudotime recapitulated chronological differentiation progression.",
      "Module-score correlation analysis further indicated coordinated coupling among DA identity, projection-associated competence, axon guidance, synaptic maturation and neuronal maturation modules.",
      "This section is computational and hypothesis-generating; it does not establish lineage fate, functional integration or an intrinsically low-risk biological state."
    ),
    paste(
      "To evaluate whether the prioritisation framework was supported beyond the core model-development setting, we summarized external and proxy evidence layers.",
      "Preclinical marker-alignment, projection-associated molecular-competence proxies, state-level proxy comparisons, survival/stress perturbation context and limited PD genetic-context support were integrated as contextual evidence.",
      "These layers provide convergent transcriptomic support for the framework, but each remains bounded by its source type.",
      "This section is computational and hypothesis-generating candidate transcriptomic proxy support, and it remains outside anatomical tracing, barcode-based lineage assays, functional graft testing or clinical validation."
    ),
    paste(
      "Finally, integrated umbrella evidence tiers and the marker-rule-derived prioritization model audit were used to summarize the overall support structure.",
      "The locked integrated evidence table grouped supporting analyses into high-priority DA/graft transcriptomic identity, risk/safety-context and PD genetic-context umbrellas, with strongest support for the DA/graft transcriptomic identity framework.",
      "The ML audit showed moderate-to-good internal discriminative performance across detected marker-rule-derived prioritisation tasks, with median AUROC approximately 0.793 and median AUPRC approximately 0.797.",
      "This section is computational and hypothesis-generating; feature-level interpretation remains conservative because feature-importance overlap with candidate marker signatures and risk-context markers was limited."
    ),
    paste(
      "Across the Results, all findings are framed as computational, source-traceable and hypothesis-generating.",
      "The study prioritizes candidate transcriptomic cell states and candidate transcriptomic marker signatures.",
      "The framework requires wet-lab, graft-function, perturbation, lineage, anatomical and orthogonal validation before any translational interpretation.",
      "These boundaries define the correct scope for manuscript claims and guide future validation work."
    )
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(results_blocks, file.path(out_table_dir, "12H_FINAL_results_text_blocks.csv"))
write_tsv_safe(results_blocks, file.path(out_table_dir, "12H_FINAL_results_text_blocks.tsv"))

figure_callout_map <- data.frame(
  results_section_id = c(
    "R1_framework_source_traceability",
    "R1_framework_source_traceability",
    "R2_core_prioritisation_DA_graft_identity",
    "R2_core_prioritisation_DA_graft_identity",
    "R3_temporal_module_covariation",
    "R4_external_proxy_risk_boundaries",
    "R4_external_proxy_risk_boundaries",
    "R5_integrated_evidence_ML_audit",
    "R5_integrated_evidence_ML_audit"
  ),
  figure_callout = c(
    "Main Fig. 1",
    "Supplementary Figs. S1-S2",
    "Main Fig. 2",
    "Supplementary Fig. S3",
    "Main Fig. 3",
    "Main Fig. 4",
    "Supplementary Figs. S4-S8",
    "Main Fig. 5",
    "Supplementary Figs. S9-S10"
  ),
  callout_role = c(
    "framework, source role and claim-boundary schematic",
    "dataset-domain, dependency and source-panel traceability support",
    "core marker-rule-derived prioritisation and DA/graft identity evidence",
    "QC/object processing and atlas-level support",
    "pseudotime and module correlation support",
    "external/proxy evidence and risk-boundary summary",
    "preclinical, projection, state-level, stress/risk and genetic-context supplements",
    "integrated evidence tier and marker-rule-derived prioritization model audit",
    "candidate marker-signature and ML audit supplements"
  ),
  safe_interpretation = c(
    "source-traceable computational framework",
    "source and dependency support",
    "candidate transcriptomic prioritisation",
    "technical support",
    "transcriptomic maturation/module support",
    "contextual proxy support",
    "bounded proxy/support evidence",
    "integrated computational support",
    "marker/ML support with conservative feature interpretation"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(figure_callout_map, file.path(out_table_dir, "12H_FINAL_results_figure_callout_map.csv"))

prohibited_positive_phrases <- c(
  "we provide evidence supporting",
  "we examine clinical-context",
  "we evaluate clinical-context",
  "clinical prediction system",
  "patient-outcome claim",
  "prioritizes transcriptomic candidate states",
  "prioritizes transcriptomic candidate states",
  "predicts graft performance",
  "establishes graft function",
  "establishes graft safety",
  "demonstrates anatomical tracing",
  "demonstrates barcode lineage",
  "confirms lineage fate",
  "validated therapeutic marker"
)

required_boundary_concepts <- c(
  "hypothesis-generating",
  "computational",
  "candidate transcriptomic"
)

claim_audit_list <- list()
for (idx_row in seq_len(nrow(results_blocks))) {
  text_lower <- tolower(clean_space(results_blocks$results_text[idx_row]))
  positive_hits <- character(0)
  for (phrase_now in prohibited_positive_phrases) {
    if (grepl(phrase_now, text_lower, fixed = TRUE)) positive_hits <- c(positive_hits, phrase_now)
  }

  boundary_hits <- character(0)
  for (phrase_now in required_boundary_concepts) {
    if (grepl(phrase_now, text_lower, fixed = TRUE)) boundary_hits <- c(boundary_hits, phrase_now)
  }

  status_now <- "claim_boundary_pass"
  if (length(positive_hits) > 0) status_now <- "needs_repair_positive_overclaim"
  if (length(boundary_hits) < 2) status_now <- "needs_boundary_language_review"

  claim_audit_list[[length(claim_audit_list) + 1]] <- data.frame(
    results_section_id = results_blocks$results_section_id[idx_row],
    manuscript_subheading = results_blocks$manuscript_subheading[idx_row],
    prohibited_positive_phrases_detected = paste(positive_hits, collapse = ";"),
    protective_boundary_concepts_detected = paste(boundary_hits, collapse = ";"),
    results_claim_boundary_status = status_now,
    stringsAsFactors = FALSE
  )
}
claim_audit_df <- safe_bind_rows(claim_audit_list)
write_csv_safe(claim_audit_df, file.path(out_table_dir, "12H_FINAL_results_claim_boundary_audit.csv"))

full_results_lines <- c(
  "Results",
  "=======",
  ""
)
for (idx_row in seq_len(nrow(results_blocks))) {
  full_results_lines <- c(
    full_results_lines,
    results_blocks$manuscript_subheading[idx_row],
    paste(rep("-", nchar(results_blocks$manuscript_subheading[idx_row])), collapse = ""),
    results_blocks$results_text[idx_row],
    ""
  )
}
writeLines(full_results_lines, file.path(out_text_dir, "12H_FINAL_results_text_full.txt"))
cat("[12H FINAL] Wrote:", file.path(out_text_dir, "12H_FINAL_results_text_full.txt"), "\n")

writeLines(full_results_lines, file.path(out_text_dir, "12H_FINAL_results_text_full.md"))
cat("[12H FINAL] Wrote:", file.path(out_text_dir, "12H_FINAL_results_text_full.md"), "\n")

short_results_lines <- c(
  "Results text - compact version",
  "==============================",
  ""
)
for (idx_row in seq_len(nrow(results_blocks))) {
  short_results_lines <- c(
    short_results_lines,
    paste0(results_section_plan$section_order[idx_row], ". ", results_blocks$manuscript_subheading[idx_row]),
    results_blocks$results_text[idx_row],
    ""
  )
}
writeLines(short_results_lines, file.path(out_text_dir, "12H_FINAL_results_text_compact.txt"))
cat("[12H FINAL] Wrote:", file.path(out_text_dir, "12H_FINAL_results_text_compact.txt"), "\n")

discussion_handoff <- data.frame(
  discussion_section = c(
    "Computational framework contribution",
    "Why experimental validation is still required",
    "marker-rule-derived prioritization model interpretation",
    "Proxy evidence limitations",
    "Candidate marker-signature limitations",
    "Journal-positioning boundary"
  ),
  discussion_seed = c(
    "Emphasize the value of source traceability, figure provenance and conservative evidence-tier integration.",
    "State clearly that transcriptomic prioritisation requires wet-lab, graft-function or orthogonal validation.",
    "Explain that marker-rule-derived prioritization model performance is internal support and not clinical outcome prediction.",
    "Explain that projection-associated competence, state-level proxy and perturbation/risk context do not replace anatomical, barcode or functional assays.",
    "Explain that marker signatures are candidate transcriptomic signatures, not validated diagnostic or prognostic biomarkers.",
    "Position the manuscript as a computational framework/hypothesis-generating resource suitable for conservative genomics or systems-biology framing."
  ),
  linked_results_sections = c(
    "R1;R5",
    "R6",
    "R2;R5",
    "R4",
    "R5;R6",
    "R1-R6"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(discussion_handoff, file.path(out_table_dir, "12H_FINAL_handoff_to_12I_discussion_limitations.csv"))

fig_a <- open_pdf_safe("12H_FINAL_FigA_results_text_package_overview.pdf", 11.8, 6.6)
new_canvas()
draw_title("Results-text package overview", "12H converts locked legends/captions into manuscript Results text.")

overview_df <- data.frame(
  label = c(
    "Results sections",
    "Figure callout rows",
    "Figure legends used",
    "Panel captions used",
    "Claim-boundary pass sections",
    "Sections needing repair"
  ),
  value = c(
    nrow(results_blocks),
    nrow(figure_callout_map),
    nrow(figure_legend_df),
    nrow(panel_caption_df),
    sum(claim_audit_df$results_claim_boundary_status == "claim_boundary_pass"),
    sum(claim_audit_df$results_claim_boundary_status != "claim_boundary_pass")
  ),
  family = c("section", "callout", "legend", "caption", "pass", "repair"),
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
  if (overview_df$family[idx_row] == "section") color_now <- nature_palette$blue
  if (overview_df$family[idx_row] == "callout") color_now <- nature_palette$purple
  if (overview_df$family[idx_row] == "legend") color_now <- nature_palette$teal
  if (overview_df$family[idx_row] == "caption") color_now <- nature_palette$gold
  if (overview_df$family[idx_row] == "pass") color_now <- nature_palette$teal
  if (overview_df$family[idx_row] == "repair") color_now <- ifelse(count_now > 0, nature_palette$red, nature_palette$teal)
  text(bar_x0 - 0.018, yy, overview_df$label[idx_row], cex = 0.54, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.024, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.024,
       col = color_now, border = nature_palette$border, lwd = 0.42)
  text(bar_x0 + width_now * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_now), cex = 0.50, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.16, "Next: 12I should convert Results boundaries into Discussion and limitations text.", cex = 0.44, col = nature_palette$muted)
dev.off()
cat("[12H FINAL] Wrote figure:", fig_a, "\n")

fig_b <- open_pdf_safe("12H_FINAL_FigB_results_section_figure_map.pdf", 12.2, 6.8)
new_canvas()
draw_title("Results section to figure map", "Each Results subsection is anchored to main and supplementary figure callouts.")

y_positions <- seq(0.78, 0.30, length.out = nrow(results_section_plan))
for (idx_row in seq_len(nrow(results_section_plan))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$blue
  if (idx_row == 2) color_now <- nature_palette$teal
  if (idx_row == 3) color_now <- nature_palette$red
  if (idx_row == 4) color_now <- nature_palette$purple
  if (idx_row == 5) color_now <- nature_palette$navy
  if (idx_row == 6) color_now <- nature_palette$orange
  rect(0.07, yy - 0.030, 0.16, yy + 0.030, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.115, yy, paste0("R", idx_row), cex = 0.42, font = 2, col = nature_palette$white)
  text(0.19, yy + 0.014, substr(results_section_plan$manuscript_subheading[idx_row], 1, 86), cex = 0.36, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.19, yy - 0.015, paste0(results_section_plan$main_figures[idx_row], " | ", results_section_plan$supplementary_figures[idx_row]),
       cex = 0.30, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12H FINAL] Wrote figure:", fig_b, "\n")

fig_c <- open_pdf_safe("12H_FINAL_FigC_results_claim_boundary_audit.pdf", 11.6, 6.4)
new_canvas()
draw_title("Results claim-boundary audit", "Results paragraphs are checked for overclaim control before Discussion drafting.")

y_positions <- seq(0.78, 0.34, length.out = nrow(claim_audit_df))
for (idx_row in seq_len(nrow(claim_audit_df))) {
  yy <- y_positions[idx_row]
  status_now <- claim_audit_df$results_claim_boundary_status[idx_row]
  color_now <- ifelse(status_now == "claim_boundary_pass", nature_palette$teal, nature_palette$red)
  rect(0.10, yy - 0.022, 0.17, yy + 0.022, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.135, yy, paste0("R", idx_row), cex = 0.36, font = 2, col = nature_palette$white)
  text(0.20, yy + 0.010, substr(claim_audit_df$manuscript_subheading[idx_row], 1, 82), cex = 0.32, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.20, yy - 0.014, status_now, cex = 0.30, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12H FINAL] Wrote figure:", fig_c, "\n")

fig_d <- open_pdf_safe("12H_FINAL_FigD_12I_discussion_handoff.pdf", 11.8, 6.6)
new_canvas()
draw_title("12I Discussion and limitations handoff", "12I should transform Results claims into balanced Discussion text.")

y_positions <- seq(0.78, 0.32, length.out = nrow(discussion_handoff))
for (idx_row in seq_len(nrow(discussion_handoff))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$blue
  if (idx_row == 2) color_now <- nature_palette$orange
  if (idx_row == 3) color_now <- nature_palette$teal
  if (idx_row == 4) color_now <- nature_palette$purple
  if (idx_row == 5) color_now <- nature_palette$gold
  if (idx_row == 6) color_now <- nature_palette$navy
  rect(0.08, yy - 0.026, 0.32, yy + 0.026, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.20, yy, discussion_handoff$discussion_section[idx_row], cex = 0.31, font = 2, col = nature_palette$white)
  text(0.36, yy + 0.010, discussion_handoff$linked_results_sections[idx_row], cex = 0.31, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.36, yy - 0.014, substr(discussion_handoff$discussion_seed[idx_row], 1, 100), cex = 0.27, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12H FINAL] Wrote figure:", fig_d, "\n")

fig_e <- open_pdf_safe("12H_FINAL_FigE_results_text_output_map.pdf", 11.4, 6.2)
new_canvas()
draw_title("Results text output map", "Generated text files and tables for manuscript drafting.")

output_items <- data.frame(
  item = c(
    "Full Results text",
    "Compact Results text",
    "Results text blocks table",
    "Figure callout map",
    "Claim-boundary audit",
    "12I Discussion handoff"
  ),
  ready = c(
    file.exists(file.path(out_text_dir, "12H_FINAL_results_text_full.txt")),
    file.exists(file.path(out_text_dir, "12H_FINAL_results_text_compact.txt")),
    file.exists(file.path(out_table_dir, "12H_FINAL_results_text_blocks.csv")),
    file.exists(file.path(out_table_dir, "12H_FINAL_results_figure_callout_map.csv")),
    file.exists(file.path(out_table_dir, "12H_FINAL_results_claim_boundary_audit.csv")),
    file.exists(file.path(out_table_dir, "12H_FINAL_handoff_to_12I_discussion_limitations.csv"))
  ),
  stringsAsFactors = FALSE
)
y_positions <- seq(0.78, 0.36, length.out = nrow(output_items))
for (idx_row in seq_len(nrow(output_items))) {
  yy <- y_positions[idx_row]
  color_now <- ifelse(output_items$ready[idx_row], nature_palette$teal, nature_palette$orange)
  symbols(0.22, yy, circles = 0.018, inches = FALSE, add = TRUE,
          bg = color_now, fg = nature_palette$border, lwd = 0.35)
  text(0.26, yy, output_items$item[idx_row], cex = 0.48, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.70, yy, ifelse(output_items$ready[idx_row], "ready", "review"), cex = 0.44, adj = c(0, 0.5), col = color_now)
}
text(0.50, 0.18, "Next module: 12I Discussion / limitations refresh.", cex = 0.42, col = nature_palette$muted)
dev.off()
cat("[12H FINAL] Wrote figure:", fig_e, "\n")

n_sections <- nrow(results_blocks)
n_callouts <- nrow(figure_callout_map)
n_claim_pass <- sum(claim_audit_df$results_claim_boundary_status == "claim_boundary_pass")
n_claim_repair <- sum(claim_audit_df$results_claim_boundary_status != "claim_boundary_pass")
n_text_files <- sum(file.exists(c(
  file.path(out_text_dir, "12H_FINAL_results_text_full.txt"),
  file.path(out_text_dir, "12H_FINAL_results_text_full.md"),
  file.path(out_text_dir, "12H_FINAL_results_text_compact.txt")
)))

decision_value <- "INPUT_READY_FOR_12I_DISCUSSION_LIMITATIONS_REFRESH"
if (n_claim_repair > 0) decision_value <- "REPAIR_REQUIRED_BEFORE_12I"
if (n_sections < 6 || n_callouts < 8) decision_value <- "REVIEW_REQUIRED_BEFORE_12I"

summary_df <- data.frame(
  item = c(
    "results_sections_generated",
    "results_text_blocks_generated",
    "figure_callout_rows",
    "figure_legends_used",
    "panel_captions_used",
    "claim_boundary_pass_sections",
    "claim_boundary_repair_needed",
    "text_files_written",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(n_sections),
    as.character(nrow(results_blocks)),
    as.character(n_callouts),
    as.character(nrow(figure_legend_df)),
    as.character(nrow(panel_caption_df)),
    as.character(n_claim_pass),
    as.character(n_claim_repair),
    as.character(n_text_files),
    "5",
    decision_value
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "12H_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "12H_FINAL_execution_summary.tsv"))

report_lines <- c(
  "12H FINAL report",
  "================",
  "Module: Results text refresh",
  "Mode: complete standalone 12H rebuild; no previous 12H output dependency; no internet; no 00-10P rerun.",
  "Allowed upstream inputs: locked 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F and 12G outputs.",
  "",
  paste0("Results sections generated: ", n_sections),
  paste0("Results text blocks generated: ", nrow(results_blocks)),
  paste0("Figure callout rows: ", n_callouts),
  paste0("Figure legends used: ", nrow(figure_legend_df)),
  paste0("Panel captions used: ", nrow(panel_caption_df)),
  paste0("Claim-boundary pass sections: ", n_claim_pass),
  paste0("Claim-boundary repair needed: ", n_claim_repair),
  paste0("Text files written: ", n_text_files),
  "",
  "Main outputs:",
  paste0("- ", file.path(out_text_dir, "12H_FINAL_results_text_full.txt")),
  paste0("- ", file.path(out_text_dir, "12H_FINAL_results_text_full.md")),
  paste0("- ", file.path(out_text_dir, "12H_FINAL_results_text_compact.txt")),
  paste0("- ", file.path(out_table_dir, "12H_FINAL_results_text_blocks.csv")),
  paste0("- ", file.path(out_table_dir, "12H_FINAL_handoff_to_12I_discussion_limitations.csv")),
  "",
  "Claim boundary:",
  "- Results text is computational and hypothesis-generating.",
  "- Candidate signatures remain candidate transcriptomic marker signatures, not validated clinical biomarkers.",
  "- ML remains marker-rule-derived transcriptomic prioritisation audit, not clinical prediction.",
  "- Projection/state/risk/genetic layers remain proxy/contextual support, not functional or causal proof.",
  "",
  paste0("Decision: ", decision_value)
)
report_file <- file.path(out_text_dir, "12H_FINAL_results_text_refresh_report.txt")
writeLines(report_lines, report_file)
cat("[12H FINAL] Wrote:", report_file, "\n")

cat("\n[12H FINAL] Completed Results text refresh.\n")
cat("[12H FINAL] Results sections generated:", n_sections, "\n")
cat("[12H FINAL] Results text blocks generated:", nrow(results_blocks), "\n")
cat("[12H FINAL] Figure callout rows:", n_callouts, "\n")
cat("[12H FINAL] Figure legends used:", nrow(figure_legend_df), "\n")
cat("[12H FINAL] Panel captions used:", nrow(panel_caption_df), "\n")
cat("[12H FINAL] Claim-boundary pass sections:", n_claim_pass, "\n")
cat("[12H FINAL] Claim-boundary repair needed:", n_claim_repair, "\n")
cat("[12H FINAL] Text files written:", n_text_files, "\n")
cat("[12H FINAL] Figures written: 5\n")
cat("[12H FINAL] Decision:", decision_value, "\n")
cat("[12H FINAL] Output tables:", out_table_dir, "\n")
cat("[12H FINAL] Output figs  :", out_fig_dir, "\n")
cat("[12H FINAL] Output text  :", out_text_dir, "\n")
cat("[12H FINAL] Next         : review 12H Results text and PDFs; if accepted, proceed to 12I Discussion / limitations refresh.\n")
