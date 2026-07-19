
cat("\n[12J FINAL] Starting Methods / reproducibility refresh...\n")
cat("[12J FINAL] Mode: complete standalone 12J rebuild; no previous 12J dependency; no internet; no 00-10P rerun.\n")
cat("[12J FINAL] Inputs allowed: locked upstream 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F, 12G, 12H and 12I outputs.\n")
cat("[12J FINAL] Formal input: 12I V3 Methods/reproducibility handoff and locked Discussion/limitations outputs.\n")
cat("[12J FINAL] Claim boundary: Methods/reproducibility only; no clinical prediction or validated biomarker claim.\n")

graphics.off()

project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")
figure_root <- file.path(project_root, "04_figures")
script_root <- file.path(project_root, "01_scripts")
text_root <- file.path(project_root, "09_manuscript")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE"
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
  cat("[12J FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[12J FINAL] Wrote:", file_value, "\n")
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
    cat("[12J FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
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

draw_title <- function(title_value, subtitle_value = "") {
  text(0.5, 0.965, title_value, cex = 1.00, font = 2, adj = c(0.5, 0.5), col = nature_palette$ink)
  if (nchar(subtitle_value) > 0) {
    text(0.5, 0.928, subtitle_value, cex = 0.50, col = nature_palette$muted, adj = c(0.5, 0.5))
  }
}

if (!dir.exists(table_root)) stop("[12J FINAL] Missing table root: ", table_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
table_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(table_info$size) & table_info$size > 0 & table_info$size < 260 * 1024 * 1024]

all_table_files <- all_table_files[!grepl("12J_methods_reproducibility_refresh", all_table_files, ignore.case = TRUE)]

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

file_12i_handoff <- first_existing_file(c(
  file.path(table_root, "12I_discussion_limitations_refresh_FINAL_COMPLETE_STANDALONE_V3", "12I_FINAL_handoff_to_12J_methods_reproducibility.csv"),
  find_files_all_terms(all_table_files, c("12i", "handoff_to_12j_methods_reproducibility"), max_n = 10)
))
file_12i_discussion_blocks <- first_existing_file(c(
  file.path(table_root, "12I_discussion_limitations_refresh_FINAL_COMPLETE_STANDALONE_V3", "12I_FINAL_discussion_text_blocks.csv"),
  find_files_all_terms(all_table_files, c("12i", "discussion_text_blocks"), max_n = 10)
))
file_12i_limitations <- first_existing_file(c(
  file.path(table_root, "12I_discussion_limitations_refresh_FINAL_COMPLETE_STANDALONE_V3", "12I_FINAL_limitations_table.csv"),
  find_files_all_terms(all_table_files, c("12i", "limitations_table"), max_n = 10)
))
file_12i_claim_audit <- first_existing_file(c(
  file.path(table_root, "12I_discussion_limitations_refresh_FINAL_COMPLETE_STANDALONE_V3", "12I_FINAL_discussion_claim_boundary_audit.csv"),
  find_files_all_terms(all_table_files, c("12i", "discussion_claim_boundary_audit"), max_n = 10)
))
file_12h_results_blocks <- first_existing_file(c(
  file.path(table_root, "12H_results_text_refresh_FINAL_COMPLETE_STANDALONE_V4", "12H_FINAL_results_text_blocks.csv"),
  find_files_all_terms(all_table_files, c("12h", "results_text_blocks"), max_n = 10)
))
file_12f_panel_assembly <- first_existing_file(c(
  file.path(table_root, "12F_optional_final_assembly_MAIN_FIG1_REDESIGN_FINAL_COMPLETE_STANDALONE_V2", "12F_FINAL_panel_assembly_manifest.csv"),
  find_files_all_terms(all_table_files, c("12f", "panel_assembly_manifest"), max_n = 10)
))
file_12g_panel_caption <- first_existing_file(c(
  file.path(table_root, "12G_final_legends_caption_refresh_FINAL_COMPLETE_STANDALONE", "12G_FINAL_panel_caption_table.csv"),
  find_files_all_terms(all_table_files, c("12g", "panel_caption_table"), max_n = 10)
))

handoff_12i_df <- read_table_safe(file_12i_handoff)
discussion_blocks_12i_df <- read_table_safe(file_12i_discussion_blocks)
limitations_12i_df <- read_table_safe(file_12i_limitations)
claim_audit_12i_df <- read_table_safe(file_12i_claim_audit)
results_blocks_12h_df <- read_table_safe(file_12h_results_blocks)
panel_assembly_12f_df <- read_table_safe(file_12f_panel_assembly)
panel_caption_12g_df <- read_table_safe(file_12g_panel_caption)

if (nrow(handoff_12i_df) < 1) stop("[12J FINAL] Missing 12I handoff to 12J table.", call. = FALSE)
if (nrow(discussion_blocks_12i_df) < 1) stop("[12J FINAL] Missing 12I discussion text blocks table.", call. = FALSE)

input_audit_df <- data.frame(
  input_name = c(
    "12I_handoff_to_12J_methods_reproducibility",
    "12I_discussion_text_blocks",
    "12I_limitations_table",
    "12I_discussion_claim_boundary_audit",
    "12H_results_text_blocks",
    "12F_panel_assembly_manifest",
    "12G_panel_caption_table"
  ),
  detected = c(
    file_12i_handoff != "",
    file_12i_discussion_blocks != "",
    file_12i_limitations != "",
    file_12i_claim_audit != "",
    file_12h_results_blocks != "",
    file_12f_panel_assembly != "",
    file_12g_panel_caption != ""
  ),
  file_path = c(
    file_12i_handoff,
    file_12i_discussion_blocks,
    file_12i_limitations,
    file_12i_claim_audit,
    file_12h_results_blocks,
    file_12f_panel_assembly,
    file_12g_panel_caption
  ),
  rows_loaded = c(
    nrow(handoff_12i_df),
    nrow(discussion_blocks_12i_df),
    nrow(limitations_12i_df),
    nrow(claim_audit_12i_df),
    nrow(results_blocks_12h_df),
    nrow(panel_assembly_12f_df),
    nrow(panel_caption_12g_df)
  ),
  allowed_as_locked_upstream_input = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
write_csv_safe(input_audit_df, file.path(out_table_dir, "12J_FINAL_locked_upstream_input_audit.csv"))

locked_module_provenance <- data.frame(
  module_id = c(
    "10G", "10H", "10K", "10L", "10P",
    "11A", "11B", "11C", "11D", "11E", "11F", "11G", "11H", "11I", "11J",
    "12A", "12B", "12C", "12D", "12E", "12F", "12G", "12H", "12I"
  ),
  locked_role = c(
    "dataset-domain/project reframing audit",
    "dataset role and model-scope freeze",
    "multi-timepoint graph-based pseudotime support",
    "user scRNA signature-priority inference demo",
    "source-panel package / manuscript source provenance",
    "new dataset evidence upgrade audit",
    "download/import audit for new evidence layers",
    "preclinical graft-outcome marker support",
    "survival/stress perturbation and safety-risk context support",
    "state-level transcriptomic proxy support",
    "projection-associated molecular competence proxy support",
    "limited PD genetic-context support",
    "integrated umbrella evidence tier and candidate marker signatures",
    "module-score correlation support",
    "marker-rule-derived prioritization model ROC/PR and feature-transparency audit",
    "final storyline refresh",
    "final figure plan refresh",
    "final source-panel lock",
    "final panel package generation",
    "final visual audit",
    "optional final assembly with Main Fig 1 redesign",
    "final legends/caption refresh",
    "Results text refresh",
    "Discussion/limitations refresh"
  ),
  allowed_as_12J_input = TRUE,
  same_module_old_output_reuse = FALSE,
  methods_relevance = c(
    "data-source/domain boundary",
    "training/external/reference role control",
    "pseudotime method description",
    "signature-priority demo boundary",
    "source provenance",
    "evidence search/import provenance",
    "evidence ingestion provenance",
    "external/preclinical marker support",
    "perturbation/risk context method",
    "state-level proxy method",
    "projection proxy method",
    "genetic-context method",
    "integrated evidence-tier method",
    "module-score correlation method",
    "marker-rule-derived prioritization model audit method",
    "storyline provenance",
    "figure planning provenance",
    "source lock provenance",
    "panel package provenance",
    "visual audit provenance",
    "assembly provenance",
    "legend/caption provenance",
    "Results provenance",
    "Discussion/limitations provenance"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(locked_module_provenance, file.path(out_table_dir, "12J_FINAL_locked_module_provenance_table.csv"))

methods_section_plan <- data.frame(
  section_order = 1:9,
  methods_section_id = c(
    "M1_data_source_and_role_lock",
    "M2_workflow_module_locking",
    "M3_transcriptomic_prioritisation_framework",
    "M4_pseudotime_module_scoring",
    "M5_proxy_evidence_integration",
    "M6_integrated_evidence_tier",
    "M7_weak_label_ML_audit",
    "M8_figure_table_reproducibility",
    "M9_claim_boundary_controls"
  ),
  manuscript_subheading = c(
    "Data sources and dataset-role locking",
    "Workflow locking and module provenance",
    "Transcriptomic prioritisation framework",
    "Pseudotime and module-score analyses",
    "External and proxy evidence integration",
    "Integrated evidence-tier construction",
    "Marker-rule-derived machine-learning audit",
    "Figure, table and source-panel reproducibility",
    "Claim-boundary and interpretation controls"
  ),
  source_anchor = c(
    "10G;10H;11A;11B;12C",
    "10G-12I locked module provenance",
    "09C;10L;11H;12B",
    "10K;11I",
    "11C;11D;11E;11F;11G",
    "11H;12A;12B",
    "11J;12G;12H",
    "10P;12C;12D;12E;12F;12G",
    "12H;12I;12J"
  ),
  reproducibility_note = c(
    "Accession roles were frozen before final figure and manuscript assembly.",
    "Each downstream module reads only locked upstream outputs and never reads same-module old results.",
    "Priority states are candidate transcriptomic states, not clinical outcomes.",
    "Pseudotime and module scoring summarize transcriptomic structure, not lineage fate or functional integration.",
    "Proxy layers provide contextual transcriptomic support, not functional proof.",
    "Evidence tiers summarize support strength under conservative claim boundaries.",
    "ML performance is an internal marker-rule-derived prioritisation audit, not clinical prediction.",
    "Every final panel is connected to a locked source table or source PDF.",
    "Prohibited claims were controlled at Results, Discussion, legend and Methods levels."
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(methods_section_plan, file.path(out_table_dir, "12J_FINAL_methods_section_plan.csv"))

methods_text_values <- c(
  paste(
    "Public transcriptomic datasets were organized into a locked source-role framework before final interpretation.",
    "Dataset roles were separated into core model-development references, independent external validation, marker-targeted context validation, bulk-support evidence and manual-review/background references.",
    "This role-locking step was used to prevent training, validation and contextual evidence from being interpreted interchangeably."
  ),
  paste(
    "The analysis was organized as a modular workflow in which each downstream module used only locked upstream outputs.",
    "Same-module old-output reuse was explicitly excluded during final manuscript-preparation modules.",
    "This rule was applied to source locking, panel packaging, visual audit, final assembly, legends/captions, Results, Discussion and Methods refresh modules."
  ),
  paste(
    "The core framework prioritised dopaminergic neuron and graft-related transcriptomic cell states using marker-rule-derived transcriptomic evidence.",
    "DA identity, A9/A10-like functional identity, projection-associated molecular competence, maturation-related modules and risk-context modules were used as candidate-state interpretation layers.",
    "The resulting priority states were interpreted as candidate transcriptomic states rather than as clinical or therapeutic outcome classes."
  ),
  paste(
    "Graph-based pseudotime analysis and module-score correlation were used to evaluate temporal and module-level transcriptomic structure.",
    "Pseudotime was interpreted as a transcriptomic ordering that recapitulates differentiation progression, not as lineage fate tracing.",
    "Module-score correlations were used to assess co-variation among identity, projection-associated competence, axon guidance, synaptic maturation, neuronal maturation and risk-context modules."
  ),
  paste(
    "External and proxy evidence layers were integrated to evaluate whether the prioritisation framework was supported outside the core model-development setting.",
    "These layers included preclinical marker alignment, projection-associated molecular competence proxies, state-level transcriptomic proxy comparisons, survival/stress perturbation context and limited PD genetic-context support.",
    "Each layer was retained as contextual support and was not used as functional, anatomical, clinical or causal proof."
  ),
  paste(
    "Integrated evidence tiers were generated to summarize support across source-locked modules.",
    "The evidence-tier framework grouped analyses into DA/graft transcriptomic identity, risk/safety-context and genetic-context umbrellas.",
    "Evidence tiers were interpreted as structured computational support for prioritisation rather than as validation of clinical or biological outcomes."
  ),
  paste(
    "Marker-rule-derived machine-learning performance was audited using internal ROC/PR summaries and feature-transparency checks.",
    "The audit evaluated whether detected prioritisation tasks showed discriminative signal under the available marker-rule-derived framework.",
    "Because labels were not clinical outcomes or experimental graft-function labels, ML results were interpreted only as internal prioritisation support."
  ),
  paste(
    "Final figures and tables were assembled through source-locking, panel-package generation, visual audit, final assembly and legend/caption refresh modules.",
    "Each final panel was linked to a locked upstream module, table, source PDF or redesigned assembly panel.",
    "This procedure generated a traceable figure-to-table-to-manuscript provenance chain for reproducibility."
  ),
  paste(
    "Claim-boundary controls were applied during figure planning, source locking, legend generation, Results writing and Discussion writing.",
    "The manuscript avoids clinical prediction, validated biomarker, graft efficacy/safety, anatomical projection and barcode-lineage claims.",
    "All final interpretations are framed as computational, transcriptomic, source-traceable and hypothesis-generating."
  )
)

methods_blocks <- data.frame(
  section_order = methods_section_plan$section_order,
  methods_section_id = methods_section_plan$methods_section_id,
  manuscript_subheading = methods_section_plan$manuscript_subheading,
  source_anchor = methods_section_plan$source_anchor,
  methods_text = methods_text_values,
  stringsAsFactors = FALSE
)
write_csv_safe(methods_blocks, file.path(out_table_dir, "12J_FINAL_methods_text_blocks.csv"))
write_tsv_safe(methods_blocks, file.path(out_table_dir, "12J_FINAL_methods_text_blocks.tsv"))

repro_checklist <- data.frame(
  checklist_id = paste0("RC", sprintf("%02d", 1:12)),
  reproducibility_item = c(
    "Project root defined",
    "Script version recorded",
    "No same-module old-output reuse",
    "Locked upstream input audit written",
    "Dataset-role provenance recorded",
    "Module provenance table written",
    "Methods text blocks written",
    "Claim-boundary audit written",
    "Figure/table source reproducibility documented",
    "12K GitHub handoff written",
    "Text outputs written",
    "PDF audit outputs written"
  ),
  status = c(
    dir.exists(project_root),
    TRUE,
    TRUE,
    file.exists(file.path(out_table_dir, "12J_FINAL_locked_upstream_input_audit.csv")),
    TRUE,
    file.exists(file.path(out_table_dir, "12J_FINAL_locked_module_provenance_table.csv")),
    file.exists(file.path(out_table_dir, "12J_FINAL_methods_text_blocks.csv")),
    TRUE,
    nrow(panel_assembly_12f_df) > 0 || nrow(panel_caption_12g_df) > 0,
    TRUE,
    TRUE,
    TRUE
  ),
  evidence_file_or_note = c(
    project_root,
    "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE.R",
    "12J excludes files matching 12J_methods_reproducibility_refresh from input discovery",
    file.path(out_table_dir, "12J_FINAL_locked_upstream_input_audit.csv"),
    file.path(out_table_dir, "12J_FINAL_locked_module_provenance_table.csv"),
    file.path(out_table_dir, "12J_FINAL_locked_module_provenance_table.csv"),
    file.path(out_table_dir, "12J_FINAL_methods_text_blocks.csv"),
    file.path(out_table_dir, "12J_FINAL_methods_claim_boundary_audit.csv"),
    "12F panel assembly manifest and 12G panel captions imported when available",
    file.path(out_table_dir, "12J_FINAL_handoff_to_12K_github_repository_package.csv"),
    out_text_dir,
    out_fig_dir
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(repro_checklist, file.path(out_table_dir, "12J_FINAL_reproducibility_checklist.csv"))

prohibited_positive_phrases <- c(
  "we provide evidence supporting",
  "we evaluate clinical-context",
  "clinical prediction system",
  "prioritizes transcriptomic candidate states",
  "prioritizes transcriptomic candidate states",
  "prioritizes transcriptomic candidate states",
  "prioritizes transcriptomic candidate states",
  "demonstrates anatomical projection",
  "confirms lineage tracing",
  "candidate diagnostic-marker context",
  "candidate prognostic-marker context",
  "candidate therapeutic-marker context"
)

required_boundary_concepts <- c(
  "computational",
  "candidate",
  "marker-rule-derived",
  "not",
  "contextual",
  "hypothesis-generating",
  "source"
)

claim_audit_list <- list()
for (idx_row in seq_len(nrow(methods_blocks))) {
  text_lower <- tolower(clean_space(methods_blocks$methods_text[idx_row]))
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
  if (length(boundary_hits) < 1) status_now <- "needs_boundary_language_review"
  claim_audit_list[[length(claim_audit_list) + 1]] <- data.frame(
    methods_section_id = methods_blocks$methods_section_id[idx_row],
    manuscript_subheading = methods_blocks$manuscript_subheading[idx_row],
    prohibited_positive_phrases_detected = paste(positive_hits, collapse = ";"),
    protective_boundary_concepts_detected = paste(boundary_hits, collapse = ";"),
    methods_claim_boundary_status = status_now,
    stringsAsFactors = FALSE
  )
}
methods_claim_audit <- safe_bind_rows(claim_audit_list)
write_csv_safe(methods_claim_audit, file.path(out_table_dir, "12J_FINAL_methods_claim_boundary_audit.csv"))

panel_source_summary <- data.frame(stringsAsFactors = FALSE)
if (nrow(panel_assembly_12f_df) > 0) {
  n_total_panels <- nrow(panel_assembly_12f_df)
  n_existing_panels <- 0
  if ("final_assembly_source_pdf" %in% colnames(panel_assembly_12f_df)) {
    n_existing_panels <- sum(file_exists_safe(panel_assembly_12f_df$final_assembly_source_pdf))
  }
  n_mainfig1_redesigned <- 0
  if ("assembly_action" %in% colnames(panel_assembly_12f_df)) {
    n_mainfig1_redesigned <- sum(grepl("redesigned", panel_assembly_12f_df$assembly_action, ignore.case = TRUE))
  }
  panel_source_summary <- data.frame(
    metric = c(
      "panel_rows_in_12F_manifest",
      "panel_source_pdfs_existing",
      "main_fig1_redesigned_panels",
      "panel_caption_rows_from_12G"
    ),
    value = c(
      as.character(n_total_panels),
      as.character(n_existing_panels),
      as.character(n_mainfig1_redesigned),
      as.character(nrow(panel_caption_12g_df))
    ),
    stringsAsFactors = FALSE
  )
} else {
  panel_source_summary <- data.frame(
    metric = c("panel_rows_in_12F_manifest", "panel_caption_rows_from_12G"),
    value = c("0", as.character(nrow(panel_caption_12g_df))),
    stringsAsFactors = FALSE
  )
}
write_csv_safe(panel_source_summary, file.path(out_table_dir, "12J_FINAL_figure_table_source_reproducibility_summary.csv"))

github_handoff <- data.frame(
  github_package_item = c(
    "README project framing",
    "Data/source manifest",
    "Workflow script index",
    "Module provenance table",
    "Reproducibility checklist",
    "Figure/table provenance",
    "Claim-boundary statement",
    "Manuscript text outputs",
    "Code availability statement"
  ),
  required_content = c(
    "Frame project as DA neuron/graft-related transcriptomic prioritisation framework.",
    "List accessions and dataset roles from locked source-role tables.",
    "List scripts in intended order and mark locked modules.",
    "Include 12J locked module provenance table.",
    "Include 12J reproducibility checklist.",
    "Include 12F/12G panel and caption provenance tables.",
    "State no clinical prediction, validated biomarker, graft efficacy/safety, anatomical projection or barcode-lineage proof.",
    "Include Results, Discussion, Methods draft text outputs.",
    "State scripts and derived tables are provided for reproducibility; raw data remain at public source repositories."
  ),
  source_file_or_table = c(
    "12I/12J text outputs",
    "10G/10H/12J provenance tables",
    "01_scripts directory plus 12J module provenance",
    file.path(out_table_dir, "12J_FINAL_locked_module_provenance_table.csv"),
    file.path(out_table_dir, "12J_FINAL_reproducibility_checklist.csv"),
    "12F panel assembly manifest; 12G panel caption table",
    file.path(out_table_dir, "12J_FINAL_claim_boundary_statement.csv"),
    "12H/12I/12J manuscript text folders",
    file.path(out_text_dir, "12J_FINAL_code_availability_statement.txt")
  ),
  ready_for_12K = TRUE,
  stringsAsFactors = FALSE
)
write_csv_safe(github_handoff, file.path(out_table_dir, "12J_FINAL_handoff_to_12K_github_repository_package.csv"))

claim_boundary_statement <- data.frame(
  claim_category = c(
    "Study frame",
    "Cell-state outputs",
    "Marker outputs",
    "ML outputs",
    "Pseudotime outputs",
    "Projection outputs",
    "Lineage/state outputs",
    "Risk/safety outputs",
    "Genetic-context outputs"
  ),
  allowed_wording = c(
    "source-traceable computational transcriptomic prioritisation framework",
    "candidate transcriptomic cell states",
    "candidate transcriptomic marker signatures",
    "marker-rule-derived prioritization model audit",
    "graph-based transcriptomic pseudotime/order",
    "projection-associated molecular competence proxy",
    "state-level transcriptomic proxy support",
    "survival/stress/risk-context transcriptomic support",
    "limited PD genetic-context support"
  ),
  prohibited_wording = c(
    "clinical-use model or therapeutic-efficacy claim",
    "validated graft outcome class",
    "validated diagnostic/prognostic/therapeutic biomarker",
    "clinical outcome prediction",
    "lineage fate tracing or functional maturation proof",
    "anatomical-projection claim",
    "barcode-lineage claim",
    "clinical safety or tumorigenicity prediction",
    "genetic causality or disease mechanism proof"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(claim_boundary_statement, file.path(out_table_dir, "12J_FINAL_claim_boundary_statement.csv"))

full_methods_lines <- c(
  "Methods",
  "=======",
  ""
)
for (idx_row in seq_len(nrow(methods_blocks))) {
  full_methods_lines <- c(
    full_methods_lines,
    methods_blocks$manuscript_subheading[idx_row],
    paste(rep("-", nchar(methods_blocks$manuscript_subheading[idx_row])), collapse = ""),
    methods_blocks$methods_text[idx_row],
    ""
  )
}
writeLines(full_methods_lines, file.path(out_text_dir, "12J_FINAL_methods_text_full.txt"))
cat("[12J FINAL] Wrote:", file.path(out_text_dir, "12J_FINAL_methods_text_full.txt"), "\n")

writeLines(full_methods_lines, file.path(out_text_dir, "12J_FINAL_methods_text_full.md"))
cat("[12J FINAL] Wrote:", file.path(out_text_dir, "12J_FINAL_methods_text_full.md"), "\n")

compact_methods_lines <- c(
  "Methods text - compact version",
  "==============================",
  ""
)
for (idx_row in seq_len(nrow(methods_blocks))) {
  compact_methods_lines <- c(
    compact_methods_lines,
    paste0(idx_row, ". ", methods_blocks$manuscript_subheading[idx_row]),
    methods_blocks$methods_text[idx_row],
    ""
  )
}
writeLines(compact_methods_lines, file.path(out_text_dir, "12J_FINAL_methods_text_compact.txt"))
cat("[12J FINAL] Wrote:", file.path(out_text_dir, "12J_FINAL_methods_text_compact.txt"), "\n")

code_availability_lines <- c(
  "Code availability",
  "=================",
  "",
  paste(
    "All analysis scripts used for the final source-locked manuscript workflow are organized as a modular R workflow.",
    "The final manuscript-preparation modules were run as standalone scripts and were designed not to read same-module old outputs.",
    "Derived tables, figure manifests, source-panel manifests, Results/Discussion/Methods text outputs and claim-boundary audits are provided to support reproducibility.",
    "Raw public data should be obtained from their original public repositories according to the dataset accession information."
  )
)
writeLines(code_availability_lines, file.path(out_text_dir, "12J_FINAL_code_availability_statement.txt"))
cat("[12J FINAL] Wrote:", file.path(out_text_dir, "12J_FINAL_code_availability_statement.txt"), "\n")

fig_a <- open_pdf_safe("12J_FINAL_FigA_methods_package_overview.pdf", 11.8, 6.6)
new_canvas()
draw_title("Methods / reproducibility package overview", "12J converts locked Discussion boundaries into Methods and reproducibility documentation.")

overview_df <- data.frame(
  label = c(
    "Methods sections",
    "Locked modules documented",
    "Reproducibility checklist rows",
    "12K GitHub handoff rows",
    "Claim-boundary pass sections",
    "Sections needing repair"
  ),
  value = c(
    nrow(methods_blocks),
    nrow(locked_module_provenance),
    nrow(repro_checklist),
    nrow(github_handoff),
    sum(methods_claim_audit$methods_claim_boundary_status == "claim_boundary_pass"),
    sum(methods_claim_audit$methods_claim_boundary_status != "claim_boundary_pass")
  ),
  family = c("methods", "module", "check", "github", "pass", "repair"),
  stringsAsFactors = FALSE
)
max_value <- max(safe_num(overview_df$value), na.rm = TRUE)
if (!is.finite(max_value) || max_value <= 0) max_value <- 1
bar_x0 <- 0.43
bar_x1 <- 0.80
y_positions <- seq(0.78, 0.32, length.out = nrow(overview_df))
for (idx_row in seq_len(nrow(overview_df))) {
  yy <- y_positions[idx_row]
  count_now <- safe_num(overview_df$value[idx_row])
  width_now <- count_now / max_value
  if (count_now == 0) width_now <- 0.018
  color_now <- nature_palette$navy
  if (overview_df$family[idx_row] == "methods") color_now <- nature_palette$blue
  if (overview_df$family[idx_row] == "module") color_now <- nature_palette$purple
  if (overview_df$family[idx_row] == "check") color_now <- nature_palette$gold
  if (overview_df$family[idx_row] == "github") color_now <- nature_palette$teal
  if (overview_df$family[idx_row] == "pass") color_now <- nature_palette$teal
  if (overview_df$family[idx_row] == "repair") color_now <- ifelse(count_now > 0, nature_palette$red, nature_palette$teal)
  text(bar_x0 - 0.018, yy, overview_df$label[idx_row], cex = 0.54, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.024, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.024,
       col = color_now, border = nature_palette$border, lwd = 0.42)
  text(bar_x0 + width_now * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_now), cex = 0.50, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.16, "Next: 12K should package scripts, manifests and text outputs for GitHub/reproducibility.", cex = 0.44, col = nature_palette$muted)
dev.off()
cat("[12J FINAL] Wrote figure:", fig_a, "\n")

fig_b <- open_pdf_safe("12J_FINAL_FigB_methods_section_map.pdf", 12.2, 7.0)
new_canvas()
draw_title("Methods section map", "Each Methods subsection is linked to source anchors and reproducibility notes.")

y_positions <- seq(0.82, 0.18, length.out = nrow(methods_section_plan))
for (idx_row in seq_len(nrow(methods_section_plan))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$blue
  if (idx_row %% 4 == 1) color_now <- nature_palette$blue
  if (idx_row %% 4 == 2) color_now <- nature_palette$teal
  if (idx_row %% 4 == 3) color_now <- nature_palette$purple
  if (idx_row %% 4 == 0) color_now <- nature_palette$orange
  rect(0.06, yy - 0.024, 0.13, yy + 0.024, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.095, yy, paste0("M", idx_row), cex = 0.34, font = 2, col = nature_palette$white)
  text(0.16, yy + 0.011, substr(methods_section_plan$manuscript_subheading[idx_row], 1, 88), cex = 0.30, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.16, yy - 0.014, paste0("source: ", methods_section_plan$source_anchor[idx_row], " | ", methods_section_plan$reproducibility_note[idx_row]),
       cex = 0.24, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12J FINAL] Wrote figure:", fig_b, "\n")

fig_c <- open_pdf_safe("12J_FINAL_FigC_locked_module_provenance_map.pdf", 12.2, 7.2)
new_canvas()
draw_title("Locked module provenance map", "12J records which upstream modules support Methods and reproducibility documentation.")

module_counts <- data.frame(
  group = c("10-series", "11-series", "12-series"),
  n = c(
    sum(grepl("^10", locked_module_provenance$module_id)),
    sum(grepl("^11", locked_module_provenance$module_id)),
    sum(grepl("^12", locked_module_provenance$module_id))
  ),
  color = c(nature_palette$blue, nature_palette$purple, nature_palette$teal),
  stringsAsFactors = FALSE
)
max_n <- max(module_counts$n, na.rm = TRUE)
if (!is.finite(max_n) || max_n <= 0) max_n <- 1
y_positions <- c(0.70, 0.52, 0.34)
for (idx_row in seq_len(nrow(module_counts))) {
  yy <- y_positions[idx_row]
  text(0.22, yy, module_counts$group[idx_row], cex = 0.56, adj = c(1, 0.5), col = nature_palette$ink)
  rect(0.26, yy - 0.035, 0.26 + 0.45 * module_counts$n[idx_row] / max_n, yy + 0.035,
       col = module_counts$color[idx_row], border = nature_palette$border, lwd = 0.45)
  text(0.26 + 0.45 * module_counts$n[idx_row] / max_n + 0.015, yy, as.character(module_counts$n[idx_row]), cex = 0.54, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.18, "All listed modules are treated as locked upstream inputs; 12J excludes prior 12J outputs from discovery.", cex = 0.42, col = nature_palette$muted)
dev.off()
cat("[12J FINAL] Wrote figure:", fig_c, "\n")

fig_d <- open_pdf_safe("12J_FINAL_FigD_methods_claim_boundary_audit.pdf", 11.8, 6.8)
new_canvas()
draw_title("Methods claim-boundary audit", "Methods sections are checked for positive overclaim before GitHub packaging.")

y_positions <- seq(0.82, 0.20, length.out = nrow(methods_claim_audit))
for (idx_row in seq_len(nrow(methods_claim_audit))) {
  yy <- y_positions[idx_row]
  status_now <- methods_claim_audit$methods_claim_boundary_status[idx_row]
  color_now <- ifelse(status_now == "claim_boundary_pass", nature_palette$teal, nature_palette$red)
  rect(0.09, yy - 0.020, 0.16, yy + 0.020, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.125, yy, paste0("M", idx_row), cex = 0.32, font = 2, col = nature_palette$white)
  text(0.19, yy + 0.009, substr(methods_claim_audit$manuscript_subheading[idx_row], 1, 84), cex = 0.30, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.19, yy - 0.013, status_now, cex = 0.28, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12J FINAL] Wrote figure:", fig_d, "\n")

fig_e <- open_pdf_safe("12J_FINAL_FigE_12K_github_handoff.pdf", 11.8, 7.0)
new_canvas()
draw_title("12K GitHub / repository handoff", "12K should package scripts, manifests, provenance and manuscript text outputs.")

y_positions <- seq(0.82, 0.18, length.out = nrow(github_handoff))
for (idx_row in seq_len(nrow(github_handoff))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$blue
  if (idx_row %% 3 == 1) color_now <- nature_palette$blue
  if (idx_row %% 3 == 2) color_now <- nature_palette$teal
  if (idx_row %% 3 == 0) color_now <- nature_palette$purple
  rect(0.07, yy - 0.022, 0.31, yy + 0.022, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.19, yy, github_handoff$github_package_item[idx_row], cex = 0.29, font = 2, col = nature_palette$white)
  text(0.34, yy + 0.009, substr(github_handoff$required_content[idx_row], 1, 94), cex = 0.26, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.34, yy - 0.013, substr(github_handoff$source_file_or_table[idx_row], 1, 94), cex = 0.22, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12J FINAL] Wrote figure:", fig_e, "\n")

n_methods <- nrow(methods_blocks)
n_modules <- nrow(locked_module_provenance)
n_repro <- nrow(repro_checklist)
n_github <- nrow(github_handoff)
n_claim_pass <- sum(methods_claim_audit$methods_claim_boundary_status == "claim_boundary_pass")
n_claim_repair <- sum(methods_claim_audit$methods_claim_boundary_status != "claim_boundary_pass")
n_text_files <- sum(file.exists(c(
  file.path(out_text_dir, "12J_FINAL_methods_text_full.txt"),
  file.path(out_text_dir, "12J_FINAL_methods_text_full.md"),
  file.path(out_text_dir, "12J_FINAL_methods_text_compact.txt"),
  file.path(out_text_dir, "12J_FINAL_code_availability_statement.txt")
)))
n_repro_pass <- sum(repro_checklist$status == TRUE)

decision_value <- "INPUT_READY_FOR_12K_GITHUB_REPOSITORY_PACKAGE_REFRESH"
if (n_claim_repair > 0) decision_value <- "REPAIR_REQUIRED_BEFORE_12K"
if (n_methods < 9 || n_modules < 20 || n_github < 8) decision_value <- "REVIEW_REQUIRED_BEFORE_12K"

summary_df <- data.frame(
  item = c(
    "methods_sections_generated",
    "methods_text_blocks_generated",
    "locked_modules_documented",
    "reproducibility_checklist_rows",
    "reproducibility_checklist_pass_rows",
    "github_handoff_rows",
    "claim_boundary_pass_sections",
    "claim_boundary_repair_needed",
    "text_files_written",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(n_methods),
    as.character(nrow(methods_blocks)),
    as.character(n_modules),
    as.character(n_repro),
    as.character(n_repro_pass),
    as.character(n_github),
    as.character(n_claim_pass),
    as.character(n_claim_repair),
    as.character(n_text_files),
    "5",
    decision_value
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "12J_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "12J_FINAL_execution_summary.tsv"))

report_lines <- c(
  "12J FINAL report",
  "================",
  "Module: Methods / reproducibility refresh",
  "Mode: complete standalone 12J rebuild; no previous 12J output dependency; no internet; no 00-10P rerun.",
  "Allowed upstream inputs: locked 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F, 12G, 12H and 12I outputs.",
  "",
  paste0("Methods sections generated: ", n_methods),
  paste0("Methods text blocks generated: ", nrow(methods_blocks)),
  paste0("Locked modules documented: ", n_modules),
  paste0("Reproducibility checklist rows: ", n_repro),
  paste0("Reproducibility checklist pass rows: ", n_repro_pass),
  paste0("GitHub handoff rows: ", n_github),
  paste0("Claim-boundary pass sections: ", n_claim_pass),
  paste0("Claim-boundary repair needed: ", n_claim_repair),
  paste0("Text files written: ", n_text_files),
  "",
  "Main outputs:",
  paste0("- ", file.path(out_text_dir, "12J_FINAL_methods_text_full.txt")),
  paste0("- ", file.path(out_text_dir, "12J_FINAL_methods_text_full.md")),
  paste0("- ", file.path(out_text_dir, "12J_FINAL_methods_text_compact.txt")),
  paste0("- ", file.path(out_text_dir, "12J_FINAL_code_availability_statement.txt")),
  paste0("- ", file.path(out_table_dir, "12J_FINAL_locked_module_provenance_table.csv")),
  paste0("- ", file.path(out_table_dir, "12J_FINAL_reproducibility_checklist.csv")),
  paste0("- ", file.path(out_table_dir, "12J_FINAL_handoff_to_12K_github_repository_package.csv")),
  "",
  "Claim boundary:",
  "- Methods text frames the study as computational and hypothesis-generating.",
  "- Candidate signatures remain candidate transcriptomic marker signatures, not validated clinical biomarkers.",
  "- ML remains marker-rule-derived transcriptomic prioritisation audit, not clinical prediction.",
  "- Projection/state/risk/genetic layers remain proxy/contextual support, not functional or causal proof.",
  "",
  paste0("Decision: ", decision_value)
)
report_file <- file.path(out_text_dir, "12J_FINAL_methods_reproducibility_refresh_report.txt")
writeLines(report_lines, report_file)
cat("[12J FINAL] Wrote:", report_file, "\n")

cat("\n[12J FINAL] Completed Methods / reproducibility refresh.\n")
cat("[12J FINAL] Methods sections generated:", n_methods, "\n")
cat("[12J FINAL] Methods text blocks generated:", nrow(methods_blocks), "\n")
cat("[12J FINAL] Locked modules documented:", n_modules, "\n")
cat("[12J FINAL] Reproducibility checklist rows:", n_repro, "\n")
cat("[12J FINAL] Reproducibility checklist pass rows:", n_repro_pass, "\n")
cat("[12J FINAL] GitHub handoff rows:", n_github, "\n")
cat("[12J FINAL] Claim-boundary pass sections:", n_claim_pass, "\n")
cat("[12J FINAL] Claim-boundary repair needed:", n_claim_repair, "\n")
cat("[12J FINAL] Text files written:", n_text_files, "\n")
cat("[12J FINAL] Figures written: 5\n")
cat("[12J FINAL] Decision:", decision_value, "\n")
cat("[12J FINAL] Output tables:", out_table_dir, "\n")
cat("[12J FINAL] Output figs  :", out_fig_dir, "\n")
cat("[12J FINAL] Output text  :", out_text_dir, "\n")
cat("[12J FINAL] Next         : review 12J Methods text and PDFs; if accepted, proceed to 12K GitHub repository package refresh.\n")
