
cat("\n[12I FINAL V3] Starting Discussion / limitations refresh with D1 claim-boundary repair...\n")
cat("[12I FINAL] Mode: complete standalone 12I rebuild; no previous 12I dependency; no internet; no 00-10P rerun.\n")
cat("[12I FINAL] Inputs allowed: locked upstream 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F, 12G and 12H outputs.\n")
cat("[12I FINAL] Formal input: 12H V4 Results text and 12I discussion handoff.\n")
cat("[12I FINAL] Claim boundary: Discussion/limitations only; no clinical prediction or validated biomarker claim.\n")

graphics.off()

project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")
figure_root <- file.path(project_root, "04_figures")
text_root <- file.path(project_root, "09_manuscript")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "12I_discussion_limitations_refresh_FINAL_COMPLETE_STANDALONE_V3"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "12I_discussion_limitations_refresh_FINAL_COMPLETE_STANDALONE_V3_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "12I_discussion_limitations_refresh_FINAL_COMPLETE_STANDALONE_V3"
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
  cat("[12I FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[12I FINAL] Wrote:", file_value, "\n")
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
    cat("[12I FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
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

draw_title <- function(title_value, subtitle_value = "") {
  text(0.5, 0.965, title_value, cex = 1.00, font = 2, adj = c(0.5, 0.5), col = nature_palette$ink)
  if (nchar(subtitle_value) > 0) {
    text(0.5, 0.928, subtitle_value, cex = 0.50, col = nature_palette$muted, adj = c(0.5, 0.5))
  }
}

if (!dir.exists(table_root)) stop("[12I FINAL] Missing table root: ", table_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
table_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(table_info$size) & table_info$size > 0 & table_info$size < 240 * 1024 * 1024]

all_table_files <- all_table_files[!grepl("12I_discussion_limitations_refresh", all_table_files, ignore.case = TRUE)]

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

file_12h_discussion_handoff <- first_existing_file(c(
  file.path(table_root, "12H_results_text_refresh_FINAL_COMPLETE_STANDALONE_V4", "12H_FINAL_handoff_to_12I_discussion_limitations.csv"),
  find_files_all_terms(all_table_files, c("12h", "handoff_to_12i_discussion_limitations"), max_n = 10)
))
file_12h_results_blocks <- first_existing_file(c(
  file.path(table_root, "12H_results_text_refresh_FINAL_COMPLETE_STANDALONE_V4", "12H_FINAL_results_text_blocks.csv"),
  find_files_all_terms(all_table_files, c("12h", "results_text_blocks"), max_n = 10)
))
file_12h_section_plan <- first_existing_file(c(
  file.path(table_root, "12H_results_text_refresh_FINAL_COMPLETE_STANDALONE_V4", "12H_FINAL_results_section_plan.csv"),
  find_files_all_terms(all_table_files, c("12h", "results_section_plan"), max_n = 10)
))
file_12h_claim_audit <- first_existing_file(c(
  file.path(table_root, "12H_results_text_refresh_FINAL_COMPLETE_STANDALONE_V4", "12H_FINAL_results_claim_boundary_audit.csv"),
  find_files_all_terms(all_table_files, c("12h", "results_claim_boundary_audit"), max_n = 10)
))
file_12g_figure_legends <- first_existing_file(c(
  file.path(table_root, "12G_final_legends_caption_refresh_FINAL_COMPLETE_STANDALONE", "12G_FINAL_figure_legend_table.csv"),
  find_files_all_terms(all_table_files, c("12g", "figure_legend_table"), max_n = 10)
))

discussion_handoff_12h_df <- read_table_safe(file_12h_discussion_handoff)
results_blocks_12h_df <- read_table_safe(file_12h_results_blocks)
results_section_plan_12h_df <- read_table_safe(file_12h_section_plan)
claim_audit_12h_df <- read_table_safe(file_12h_claim_audit)
figure_legend_12g_df <- read_table_safe(file_12g_figure_legends)

if (nrow(discussion_handoff_12h_df) < 1) stop("[12I FINAL] Missing 12H handoff to 12I discussion table.", call. = FALSE)
if (nrow(results_blocks_12h_df) < 1) stop("[12I FINAL] Missing 12H Results text blocks table.", call. = FALSE)

input_audit_df <- data.frame(
  input_name = c(
    "12H_handoff_to_12I_discussion_limitations",
    "12H_results_text_blocks",
    "12H_results_section_plan",
    "12H_results_claim_boundary_audit",
    "12G_figure_legend_table"
  ),
  detected = c(
    file_12h_discussion_handoff != "",
    file_12h_results_blocks != "",
    file_12h_section_plan != "",
    file_12h_claim_audit != "",
    file_12g_figure_legends != ""
  ),
  file_path = c(
    file_12h_discussion_handoff,
    file_12h_results_blocks,
    file_12h_section_plan,
    file_12h_claim_audit,
    file_12g_figure_legends
  ),
  rows_loaded = c(
    nrow(discussion_handoff_12h_df),
    nrow(results_blocks_12h_df),
    nrow(results_section_plan_12h_df),
    nrow(claim_audit_12h_df),
    nrow(figure_legend_12g_df)
  ),
  allowed_as_locked_upstream_input = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
write_csv_safe(input_audit_df, file.path(out_table_dir, "12I_FINAL_locked_12H_input_audit.csv"))

discussion_section_plan <- data.frame(
  section_order = 1:6,
  discussion_section_id = c(
    "D1_framework_contribution",
    "D2_validation_requirement",
    "D3_weak_label_ML_interpretation",
    "D4_proxy_evidence_limitations",
    "D5_marker_signature_limitations",
    "D6_journal_positioning_boundary"
  ),
  manuscript_subheading = c(
    "A source-traceable computational framework for DA neuron and graft-related cell-state prioritisation",
    "Experimental validation remains required before biological or translational interpretation",
    "Marker-rule-derived machine-learning performance should be interpreted as internal prioritisation support",
    "Proxy evidence layers strengthen context but do not replace functional assays",
    "Candidate marker signatures require orthogonal validation",
    "Manuscript positioning as a conservative computational framework"
  ),
  linked_results_sections = c("R1;R5", "R6", "R2;R5", "R4", "R5;R6", "R1-R6"),
  primary_claim_boundary = c(
    "framework contribution, not clinical prediction",
    "validation requirement",
    "marker-rule-derived internal ML audit only",
    "proxy/context support only",
    "candidate transcriptomic signatures only",
    "journal-positioning and scope boundary"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(discussion_section_plan, file.path(out_table_dir, "12I_FINAL_discussion_section_plan.csv"))

discussion_blocks <- data.frame(
  section_order = discussion_section_plan$section_order,
  discussion_section_id = discussion_section_plan$discussion_section_id,
  manuscript_subheading = discussion_section_plan$manuscript_subheading,
  discussion_text = c(
    paste(
      "This study establishes a source-traceable computational framework for prioritising dopaminergic neuron and graft-related transcriptomic cell states.",
      "A major contribution is not a single new assay or a single isolated marker, but a structured framework that connects dataset-role auditing, marker-rule-derived prioritisation, temporal/module support, proxy evidence layers, umbrella evidence tiers and figure-level provenance.",
      "This organization is particularly important for pure computational studies because it separates what the data can support from what remains outside the current evidence base.",
      "The framework should therefore be interpreted as a transparent computational transcriptomic prioritisation resource that remains outside clinical decision use."
    ),
    paste(
      "The central limitation is the absence of direct experimental validation within the present analysis.",
      "Although the transcriptomic framework identifies candidate cell states and candidate marker signatures, these outputs require wet-lab, graft-function, perturbation, lineage, anatomical and orthogonal validation before biological causality or translational relevance can be inferred.",
      "In future work, the highest-priority transcriptomic states should be tested using independent differentiation systems, perturbation assays, graft survival/function assays and spatial or anatomical validation where appropriate.",
      "This validation requirement does not negate the computational framework, but it defines the correct level of evidence for the manuscript."
    ),
    paste(
      "The machine-learning results should be interpreted within a marker-rule-derived framework.",
      "The internal ROC/PR audit supports discriminative signal across detected prioritisation tasks, but the tasks do not represent clinical outcome labels, graft efficacy labels or patient-level therapeutic response labels.",
      "Accordingly, the ML component is best described as an internal transcriptomic prioritisation audit that helps evaluate the coherence of the computational framework.",
      "Feature-level interpretation should remain conservative, especially where feature-importance overlap with candidate marker signatures or risk-context markers is limited."
    ),
    paste(
      "Several evidence layers provide useful contextual support while remaining proxy-based.",
      "Projection-associated molecular competence does not demonstrate anatomical projection, state-level proxy comparisons do not establish barcode-lineage claim, survival/stress perturbation context does not predict clinical safety, and PD genetic-context overlap does not imply genetic causality.",
      "These layers are valuable because they test whether the candidate-prioritisation framework is directionally consistent with independent biological contexts.",
      "However, each layer should be discussed as contextual transcriptomic support rather than as functional proof."
    ),
    paste(
      "The candidate marker signatures generated by the integrated framework should be treated as transcriptomic candidates rather than validated biomarkers.",
      "They may be useful for ranking cell states, selecting genes for follow-up assays, or designing orthogonal validation experiments.",
      "However, they should not be described as diagnostic, prognostic or therapeutic biomarkers without independent validation in appropriate biological or clinical settings.",
      "This distinction is essential for maintaining a conservative and publishable claim structure."
    ),
    paste(
      "The manuscript is best positioned as a computational transcriptomic prioritisation framework and hypothesis-generating resource.",
      "Its strengths are source traceability, multi-layer evidence integration, conservative claim boundaries and a complete figure-to-results-to-discussion provenance chain.",
      "Its limitations are the lack of direct wet-lab validation, the marker-rule-derived nature of the ML tasks and the proxy nature of several support layers.",
      "This positioning supports a conservative systems-biology, genomics or computational-transcriptomics submission strategy rather than a clinical or therapeutic-efficacy framing."
    )
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(discussion_blocks, file.path(out_table_dir, "12I_FINAL_discussion_text_blocks.csv"))
write_tsv_safe(discussion_blocks, file.path(out_table_dir, "12I_FINAL_discussion_text_blocks.tsv"))

limitations_table <- data.frame(
  limitation_id = c("L1", "L2", "L3", "L4", "L5", "L6", "L7"),
  limitation_domain = c(
    "Experimental validation",
    "marker-rule-derived prioritization model",
    "Projection evidence",
    "Lineage/state evidence",
    "Safety/risk context",
    "Genetic context",
    "Marker signatures"
  ),
  what_current_study_supports = c(
    "source-traceable transcriptomic prioritisation",
    "internal discriminative performance for prioritisation tasks",
    "projection-associated molecular competence proxy",
    "state-level transcriptomic proxy comparisons",
    "survival/stress/risk-context transcriptomic support",
    "limited PD genetic-context overlap",
    "candidate transcriptomic marker signatures"
  ),
  what_current_study_does_not_prove = c(
    "direct biological causality or graft function",
    "clinical outcome prediction or therapeutic-response prediction",
    "anatomical projection",
    "barcode-lineage claim",
    "clinical safety or tumorigenicity prediction",
    "genetic causality or disease mechanism",
    "validated diagnostic, prognostic or therapeutic biomarkers"
  ),
  recommended_future_validation = c(
    "wet-lab differentiation, graft-function and orthogonal assays",
    "external labels, prospective validation or experimentally grounded labels",
    "retrograde tracing, anatomical mapping or functional connectivity assays",
    "barcode lineage tracing or time-resolved clonal assays",
    "functional perturbation, long-term graft safety and proliferation assays",
    "fine-mapping, perturbation or mechanistic genetics assays",
    "qPCR, immunostaining, perturbation and independent cohort validation"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(limitations_table, file.path(out_table_dir, "12I_FINAL_limitations_table.csv"))

journal_positioning <- data.frame(
  positioning_axis = c(
    "Best-fit framing",
    "Avoided framing",
    "Realistic article type",
    "Likely strength",
    "Likely reviewer concern",
    "Recommended response"
  ),
  recommendation = c(
    "Computational transcriptomic prioritisation framework / source-traceable candidate-state resource",
    "Clinical prediction model, validated biomarker paper, graft efficacy/safety proof, anatomical-projection claim",
    "Computational genomics, systems biology, transcriptomics resource or hypothesis-generating framework",
    "Multi-layer source traceability, integrated evidence-tier structure, conservative claim boundary",
    "Absence of direct experimental validation",
    "Emphasize claim boundaries, provide reproducible source manifests and frame follow-up validation as required"
  ),
  manuscript_use = c(
    "Title/Abstract/Discussion",
    "Claim-boundary audit",
    "Cover letter and journal selection",
    "Discussion first paragraph",
    "Limitations paragraph",
    "Reviewer-response preparation"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(journal_positioning, file.path(out_table_dir, "12I_FINAL_journal_positioning_table.csv"))

prohibited_positive_phrases <- c(
  "we provide evidence supporting",
  "we evaluate clinical-context",
  "patient outcome prediction system",
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
  "validation",
  "not",
  "proxy",
  "marker-rule-derived",
  "hypothesis-generating"
)

claim_audit_list <- list()
for (idx_row in seq_len(nrow(discussion_blocks))) {
  text_lower <- tolower(clean_space(discussion_blocks$discussion_text[idx_row]))
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
    discussion_section_id = discussion_blocks$discussion_section_id[idx_row],
    manuscript_subheading = discussion_blocks$manuscript_subheading[idx_row],
    prohibited_positive_phrases_detected = paste(positive_hits, collapse = ";"),
    protective_boundary_concepts_detected = paste(boundary_hits, collapse = ";"),
    discussion_claim_boundary_status = status_now,
    stringsAsFactors = FALSE
  )
}
discussion_claim_audit <- safe_bind_rows(claim_audit_list)
write_csv_safe(discussion_claim_audit, file.path(out_table_dir, "12I_FINAL_discussion_claim_boundary_audit.csv"))

full_discussion_lines <- c(
  "Discussion",
  "==========",
  ""
)
for (idx_row in seq_len(nrow(discussion_blocks))) {
  full_discussion_lines <- c(
    full_discussion_lines,
    discussion_blocks$manuscript_subheading[idx_row],
    paste(rep("-", nchar(discussion_blocks$manuscript_subheading[idx_row])), collapse = ""),
    discussion_blocks$discussion_text[idx_row],
    ""
  )
}
writeLines(full_discussion_lines, file.path(out_text_dir, "12I_FINAL_discussion_text_full.txt"))
cat("[12I FINAL] Wrote:", file.path(out_text_dir, "12I_FINAL_discussion_text_full.txt"), "\n")

writeLines(full_discussion_lines, file.path(out_text_dir, "12I_FINAL_discussion_text_full.md"))
cat("[12I FINAL] Wrote:", file.path(out_text_dir, "12I_FINAL_discussion_text_full.md"), "\n")

compact_lines <- c(
  "Discussion text - compact version",
  "=================================",
  ""
)
for (idx_row in seq_len(nrow(discussion_blocks))) {
  compact_lines <- c(
    compact_lines,
    paste0(idx_row, ". ", discussion_blocks$manuscript_subheading[idx_row]),
    discussion_blocks$discussion_text[idx_row],
    ""
  )
}
writeLines(compact_lines, file.path(out_text_dir, "12I_FINAL_discussion_text_compact.txt"))
cat("[12I FINAL] Wrote:", file.path(out_text_dir, "12I_FINAL_discussion_text_compact.txt"), "\n")

limitations_lines <- c(
  "Limitations paragraph",
  "=====================",
  "",
  paste(
    "This study has several limitations.",
    "First, the framework is computational and requires wet-lab, graft-function and orthogonal validation.",
    "Second, machine-learning performance reflects marker-rule-derived transcriptomic prioritisation rather than clinical outcome prediction.",
    "Third, projection-associated competence, state-level proxy evidence, survival/stress context and PD genetic context are proxy or contextual support layers rather than functional or causal proof.",
    "Finally, candidate marker signatures require independent biological validation before any diagnostic, prognostic or therapeutic use."
  )
)
writeLines(limitations_lines, file.path(out_text_dir, "12I_FINAL_limitations_paragraph.txt"))
cat("[12I FINAL] Wrote:", file.path(out_text_dir, "12I_FINAL_limitations_paragraph.txt"), "\n")

handoff_12j <- data.frame(
  methods_reproducibility_item = c(
    "Source traceability",
    "Locked module provenance",
    "marker-rule-derived prioritization model description",
    "Pseudotime and module scoring",
    "Proxy evidence modules",
    "Claim-boundary controls",
    "Figure/table reproducibility"
  ),
  required_12J_content = c(
    "Describe dataset accession roles and source-locking process.",
    "List locked modules used from 10A-12I and clarify no same-module old-output reuse.",
    "Explain marker-rule-derived task construction and internal ROC/PR audit scope.",
    "Describe graph-based pseudotime and module-score correlation methods.",
    "Describe preclinical marker alignment, projection competence proxy, state-level proxy, perturbation/risk context and PD genetic-context support.",
    "Document prohibited claims and safe wording used in Results/Discussion.",
    "Record output directories, script names, figure manifests and source-panel manifests."
  ),
  linked_12I_sections = c("D1;D6", "D1;D6", "D3", "D1;D2", "D4", "D2-D6", "D1;D6"),
  stringsAsFactors = FALSE
)
write_csv_safe(handoff_12j, file.path(out_table_dir, "12I_FINAL_handoff_to_12J_methods_reproducibility.csv"))

fig_a <- open_pdf_safe("12I_FINAL_FigA_discussion_package_overview.pdf", 11.8, 6.6)
new_canvas()
draw_title("Discussion / limitations package overview", "12I converts locked Results boundaries into balanced Discussion text.")

overview_df <- data.frame(
  label = c(
    "Discussion sections",
    "Limitations rows",
    "Journal-positioning rows",
    "12J handoff rows",
    "Claim-boundary pass sections",
    "Sections needing repair"
  ),
  value = c(
    nrow(discussion_blocks),
    nrow(limitations_table),
    nrow(journal_positioning),
    nrow(handoff_12j),
    sum(discussion_claim_audit$discussion_claim_boundary_status == "claim_boundary_pass"),
    sum(discussion_claim_audit$discussion_claim_boundary_status != "claim_boundary_pass")
  ),
  family = c("discussion", "limitations", "journal", "handoff", "pass", "repair"),
  stringsAsFactors = FALSE
)
max_value <- max(safe_num(overview_df$value), na.rm = TRUE)
if (!is.finite(max_value) || max_value <= 0) max_value <- 1
bar_x0 <- 0.42
bar_x1 <- 0.80
y_positions <- seq(0.78, 0.32, length.out = nrow(overview_df))
for (idx_row in seq_len(nrow(overview_df))) {
  yy <- y_positions[idx_row]
  count_now <- safe_num(overview_df$value[idx_row])
  width_now <- count_now / max_value
  if (count_now == 0) width_now <- 0.018
  color_now <- nature_palette$navy
  if (overview_df$family[idx_row] == "discussion") color_now <- nature_palette$blue
  if (overview_df$family[idx_row] == "limitations") color_now <- nature_palette$orange
  if (overview_df$family[idx_row] == "journal") color_now <- nature_palette$purple
  if (overview_df$family[idx_row] == "handoff") color_now <- nature_palette$gold
  if (overview_df$family[idx_row] == "pass") color_now <- nature_palette$teal
  if (overview_df$family[idx_row] == "repair") color_now <- ifelse(count_now > 0, nature_palette$red, nature_palette$teal)
  text(bar_x0 - 0.018, yy, overview_df$label[idx_row], cex = 0.54, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.024, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.024,
       col = color_now, border = nature_palette$border, lwd = 0.42)
  text(bar_x0 + width_now * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_now), cex = 0.50, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.16, "Next: 12J should convert this into Methods and reproducibility documentation.", cex = 0.44, col = nature_palette$muted)
dev.off()
cat("[12I FINAL] Wrote figure:", fig_a, "\n")

fig_b <- open_pdf_safe("12I_FINAL_FigB_discussion_section_map.pdf", 12.0, 6.8)
new_canvas()
draw_title("Discussion section map", "Each Discussion section is linked to Results sections and a claim-boundary role.")

y_positions <- seq(0.78, 0.30, length.out = nrow(discussion_section_plan))
for (idx_row in seq_len(nrow(discussion_section_plan))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$blue
  if (idx_row == 2) color_now <- nature_palette$orange
  if (idx_row == 3) color_now <- nature_palette$teal
  if (idx_row == 4) color_now <- nature_palette$purple
  if (idx_row == 5) color_now <- nature_palette$gold
  if (idx_row == 6) color_now <- nature_palette$navy
  rect(0.07, yy - 0.030, 0.16, yy + 0.030, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.115, yy, paste0("D", idx_row), cex = 0.42, font = 2, col = nature_palette$white)
  text(0.19, yy + 0.014, substr(discussion_section_plan$manuscript_subheading[idx_row], 1, 92), cex = 0.35, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.19, yy - 0.015, paste0("linked Results: ", discussion_section_plan$linked_results_sections[idx_row], " | ", discussion_section_plan$primary_claim_boundary[idx_row]),
       cex = 0.29, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12I FINAL] Wrote figure:", fig_b, "\n")

fig_c <- open_pdf_safe("12I_FINAL_FigC_limitations_boundary_map.pdf", 12.2, 7.0)
new_canvas()
draw_title("Limitations and boundary map", "Each support layer is paired with what it does not prove and what validation is needed.")

y_positions <- seq(0.80, 0.18, length.out = nrow(limitations_table))
for (idx_row in seq_len(nrow(limitations_table))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$orange
  if (idx_row %% 3 == 1) color_now <- nature_palette$blue
  if (idx_row %% 3 == 2) color_now <- nature_palette$purple
  rect(0.06, yy - 0.022, 0.13, yy + 0.022, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.095, yy, limitations_table$limitation_id[idx_row], cex = 0.36, font = 2, col = nature_palette$white)
  text(0.16, yy + 0.012, paste0(limitations_table$limitation_domain[idx_row], ": ", limitations_table$what_current_study_supports[idx_row]),
       cex = 0.29, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.16, yy - 0.014, paste0("Does not prove: ", limitations_table$what_current_study_does_not_prove[idx_row]),
       cex = 0.26, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12I FINAL] Wrote figure:", fig_c, "\n")

fig_d <- open_pdf_safe("12I_FINAL_FigD_discussion_claim_boundary_audit.pdf", 11.6, 6.4)
new_canvas()
draw_title("Discussion claim-boundary audit", "Discussion sections are checked for positive overclaim before Methods handoff.")

y_positions <- seq(0.78, 0.34, length.out = nrow(discussion_claim_audit))
for (idx_row in seq_len(nrow(discussion_claim_audit))) {
  yy <- y_positions[idx_row]
  status_now <- discussion_claim_audit$discussion_claim_boundary_status[idx_row]
  color_now <- ifelse(status_now == "claim_boundary_pass", nature_palette$teal, nature_palette$red)
  rect(0.10, yy - 0.022, 0.17, yy + 0.022, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.135, yy, paste0("D", idx_row), cex = 0.36, font = 2, col = nature_palette$white)
  text(0.20, yy + 0.010, substr(discussion_claim_audit$manuscript_subheading[idx_row], 1, 82), cex = 0.32, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.20, yy - 0.014, status_now, cex = 0.30, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12I FINAL] Wrote figure:", fig_d, "\n")

fig_e <- open_pdf_safe("12I_FINAL_FigE_12J_methods_reproducibility_handoff.pdf", 11.8, 6.6)
new_canvas()
draw_title("12J Methods / reproducibility handoff", "12J should document how the locked framework can be reproduced.")

y_positions <- seq(0.78, 0.28, length.out = nrow(handoff_12j))
for (idx_row in seq_len(nrow(handoff_12j))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$blue
  if (idx_row %% 3 == 1) color_now <- nature_palette$blue
  if (idx_row %% 3 == 2) color_now <- nature_palette$teal
  if (idx_row %% 3 == 0) color_now <- nature_palette$purple
  rect(0.07, yy - 0.023, 0.31, yy + 0.023, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.19, yy, handoff_12j$methods_reproducibility_item[idx_row], cex = 0.31, font = 2, col = nature_palette$white)
  text(0.34, yy + 0.010, handoff_12j$linked_12I_sections[idx_row], cex = 0.30, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.34, yy - 0.014, substr(handoff_12j$required_12J_content[idx_row], 1, 100), cex = 0.27, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12I FINAL] Wrote figure:", fig_e, "\n")

n_sections <- nrow(discussion_blocks)
n_limitations <- nrow(limitations_table)
n_journal_rows <- nrow(journal_positioning)
n_handoff_rows <- nrow(handoff_12j)
n_claim_pass <- sum(discussion_claim_audit$discussion_claim_boundary_status == "claim_boundary_pass")
n_claim_repair <- sum(discussion_claim_audit$discussion_claim_boundary_status != "claim_boundary_pass")
n_text_files <- sum(file.exists(c(
  file.path(out_text_dir, "12I_FINAL_discussion_text_full.txt"),
  file.path(out_text_dir, "12I_FINAL_discussion_text_full.md"),
  file.path(out_text_dir, "12I_FINAL_discussion_text_compact.txt"),
  file.path(out_text_dir, "12I_FINAL_limitations_paragraph.txt")
)))

decision_value <- "INPUT_READY_FOR_12J_METHODS_REPRODUCIBILITY_REFRESH"
if (n_claim_repair > 0) decision_value <- "REPAIR_REQUIRED_BEFORE_12J"
if (n_sections < 6 || n_limitations < 7 || n_handoff_rows < 6) decision_value <- "REVIEW_REQUIRED_BEFORE_12J"

summary_df <- data.frame(
  item = c(
    "discussion_sections_generated",
    "discussion_text_blocks_generated",
    "limitations_rows",
    "journal_positioning_rows",
    "handoff_to_12J_rows",
    "claim_boundary_pass_sections",
    "claim_boundary_repair_needed",
    "text_files_written",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(n_sections),
    as.character(nrow(discussion_blocks)),
    as.character(n_limitations),
    as.character(n_journal_rows),
    as.character(n_handoff_rows),
    as.character(n_claim_pass),
    as.character(n_claim_repair),
    as.character(n_text_files),
    "5",
    decision_value
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "12I_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "12I_FINAL_execution_summary.tsv"))

report_lines <- c(
  "12I FINAL report",
  "================",
  "Module: Discussion / limitations refresh",
  "Mode: complete standalone 12I rebuild; no previous 12I output dependency; no internet; no 00-10P rerun.",
  "Allowed upstream inputs: locked 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F, 12G and 12H outputs.",
  "",
  paste0("Discussion sections generated: ", n_sections),
  paste0("Discussion text blocks generated: ", nrow(discussion_blocks)),
  paste0("Limitations rows: ", n_limitations),
  paste0("Journal-positioning rows: ", n_journal_rows),
  paste0("Handoff to 12J rows: ", n_handoff_rows),
  paste0("Claim-boundary pass sections: ", n_claim_pass),
  paste0("Claim-boundary repair needed: ", n_claim_repair),
  paste0("Text files written: ", n_text_files),
  "",
  "Main outputs:",
  paste0("- ", file.path(out_text_dir, "12I_FINAL_discussion_text_full.txt")),
  paste0("- ", file.path(out_text_dir, "12I_FINAL_discussion_text_full.md")),
  paste0("- ", file.path(out_text_dir, "12I_FINAL_discussion_text_compact.txt")),
  paste0("- ", file.path(out_text_dir, "12I_FINAL_limitations_paragraph.txt")),
  paste0("- ", file.path(out_table_dir, "12I_FINAL_limitations_table.csv")),
  paste0("- ", file.path(out_table_dir, "12I_FINAL_handoff_to_12J_methods_reproducibility.csv")),
  "",
  "Claim boundary:",
  "- Discussion text frames the study as computational and hypothesis-generating.",
  "- Candidate signatures remain candidate transcriptomic marker signatures, not validated clinical biomarkers.",
  "- ML remains marker-rule-derived transcriptomic prioritisation audit, not clinical prediction.",
  "- Projection/state/risk/genetic layers remain proxy/contextual support, not functional or causal proof.",
  "",
  paste0("Decision: ", decision_value)
)
report_file <- file.path(out_text_dir, "12I_FINAL_discussion_limitations_refresh_report.txt")
writeLines(report_lines, report_file)
cat("[12I FINAL] Wrote:", report_file, "\n")

cat("\n[12I FINAL] Completed Discussion / limitations refresh.\n")
cat("[12I FINAL] Discussion sections generated:", n_sections, "\n")
cat("[12I FINAL] Discussion text blocks generated:", nrow(discussion_blocks), "\n")
cat("[12I FINAL] Limitations rows:", n_limitations, "\n")
cat("[12I FINAL] Journal-positioning rows:", n_journal_rows, "\n")
cat("[12I FINAL] Handoff to 12J rows:", n_handoff_rows, "\n")
cat("[12I FINAL] Claim-boundary pass sections:", n_claim_pass, "\n")
cat("[12I FINAL] Claim-boundary repair needed:", n_claim_repair, "\n")
cat("[12I FINAL] Text files written:", n_text_files, "\n")
cat("[12I FINAL] Figures written: 5\n")
cat("[12I FINAL] Decision:", decision_value, "\n")
cat("[12I FINAL] Output tables:", out_table_dir, "\n")
cat("[12I FINAL] Output figs  :", out_fig_dir, "\n")
cat("[12I FINAL] Output text  :", out_text_dir, "\n")
cat("[12I FINAL] Next         : review 12I Discussion text and PDFs; if accepted, proceed to 12J Methods / reproducibility refresh.\n")
