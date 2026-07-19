################################################################################
# 10N_advanced_manuscript_storyline_V2_COMPLETE_STANDALONE.R
# Project: PD_Graft_Project
# Purpose:
#   10N converts the accepted 10M advanced figure plan into a manuscript-level
#   storyline, Results skeleton, figure-by-figure narrative logic, claim-boundary
#   map, and abstract-level key message draft.
#
# Important:
#   - This script does NOT generate figures.
#   - This script does NOT rerun analysis.
#   - This script does NOT overwrite 10D/10E/10F or 10G-10M outputs.
#   - 10O source lock should be run only after the 10N storyline is accepted.
################################################################################

cat("\n[10N] Starting advanced manuscript storyline V2...\n")

project_root <- "D:/PD_Graft_Project"
input_table_dir <- file.path(project_root, "03_tables", "10M_advanced_figure_plan_V2")
input_text_dir  <- file.path(project_root, "09_manuscript", "10M_advanced_figure_plan_V2")
output_table_dir <- file.path(project_root, "03_tables", "10N_advanced_manuscript_storyline_V2")
output_text_dir  <- file.path(project_root, "09_manuscript", "10N_advanced_manuscript_storyline_V2")

dir.create(output_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_text_dir, recursive = TRUE, showWarnings = FALSE)

cat("[10N] Project root :", project_root, "\n")
cat("[10N] Input tables :", input_table_dir, "\n")
cat("[10N] Input text   :", input_text_dir, "\n")
cat("[10N] Output tables:", output_table_dir, "\n")
cat("[10N] Output text  :", output_text_dir, "\n")

# ----------------------------- helper functions -----------------------------

safe_read_csv <- function(path, required = FALSE) {
  if (!file.exists(path)) {
    msg <- paste0("[10N] Missing input file: ", path)
    if (required) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
    return(data.frame())
  }
  x <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      if (required) stop(paste0("[10N] Failed to read: ", path, "\n", conditionMessage(e)), call. = FALSE)
      warning(paste0("[10N] Failed to read: ", path, "\n", conditionMessage(e)), call. = FALSE)
      data.frame()
    }
  )
  return(x)
}

safe_read_lines <- function(path) {
  if (!file.exists(path)) return(character(0))
  tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
}

safe_write_csv <- function(x, path) {
  write.csv(x, path, row.names = FALSE, na = "")
  cat("[10N] Wrote:", path, "\n")
}

safe_write_lines <- function(x, path) {
  writeLines(x, con = path, useBytes = TRUE)
  cat("[10N] Wrote:", path, "\n")
}

first_existing_col <- function(df, candidates) {
  hits <- intersect(candidates, colnames(df))
  if (length(hits) == 0) return(NA_character_)
  hits[1]
}

get_col_or_blank <- function(df, candidates) {
  col <- first_existing_col(df, candidates)
  if (is.na(col)) return(rep("", nrow(df)))
  as.character(df[[col]])
}

collapse_nonempty <- function(x, sep = "; ") {
  x <- unique(trimws(as.character(x)))
  x <- x[nchar(x) > 0 & !is.na(x)]
  if (length(x) == 0) return("")
  paste(x, collapse = sep)
}

# ----------------------------- input files ----------------------------------

main_fig_path <- file.path(input_table_dir, "10M_V2_main_figure_plan.csv")
supp_fig_path <- file.path(input_table_dir, "10M_V2_supplementary_figure_plan.csv")
module_path <- file.path(input_table_dir, "10M_V2_module_integration_decision_table.csv")
story_path <- file.path(input_table_dir, "10M_V2_results_storyline_sequence_for_10N.csv")
ref_scaffold_path <- file.path(input_table_dir, "10M_V2_old_to_new_figure_reference_scaffold.csv")
evidence_scan_path <- file.path(input_table_dir, "10M_V2_module_completion_and_evidence_scan.csv")
source_inv_path <- file.path(input_table_dir, "10M_V2_candidate_source_figure_inventory.csv")
claim_note_path <- file.path(input_text_dir, "10M_V2_claim_boundary_and_wording_bank.txt")
teacher_summary_path <- file.path(input_text_dir, "10M_V2_teacher_safe_project_summary.txt")

main_fig <- safe_read_csv(main_fig_path, required = TRUE)
supp_fig <- safe_read_csv(supp_fig_path, required = FALSE)
module_decision <- safe_read_csv(module_path, required = TRUE)
story_seq <- safe_read_csv(story_path, required = TRUE)
ref_scaffold <- safe_read_csv(ref_scaffold_path, required = FALSE)
evidence_scan <- safe_read_csv(evidence_scan_path, required = FALSE)
source_inventory <- safe_read_csv(source_inv_path, required = FALSE)
claim_note <- safe_read_lines(claim_note_path)
teacher_summary <- safe_read_lines(teacher_summary_path)

# ----------------------------- module lock summary --------------------------

module_lock_summary <- data.frame(
  module = get_col_or_blank(module_decision, c("module", "Module")),
  final_lock = get_col_or_blank(module_decision, c("final_lock", "Final lock", "lock")),
  manuscript_role = get_col_or_blank(module_decision, c("figure_plan_decision", "manuscript_role", "role")),
  conclusion = get_col_or_blank(module_decision, c("conclusion", "Conclusion")),
  must_not_claim = get_col_or_blank(module_decision, c("must_not_claim", "blocked_claim", "must_not")),
  stringsAsFactors = FALSE
)
safe_write_csv(module_lock_summary, file.path(output_table_dir, "10N_V2_module_lock_summary.csv"))

# ----------------------------- manuscript storyline table -------------------

storyline_table <- data.frame(
  storyline_order = seq_len(nrow(story_seq)),
  results_section_title = get_col_or_blank(story_seq, c("result_section_title", "section_title", "title")),
  linked_main_figures = get_col_or_blank(story_seq, c("linked_main_figures", "main_figures", "figures")),
  core_message = get_col_or_blank(story_seq, c("key_message", "core_message", "message")),
  required_caution_language = get_col_or_blank(story_seq, c("caution_language", "caution", "claim_boundary")),
  manuscript_function = c(
    "Establish corrected project scope and dataset logic",
    "Define prioritized transcriptional cell-state programs",
    "Show model audit, generalization, and robustness within locked scope",
    "Support cross-dataset transcriptomic/contextual validation",
    "Add multi-timepoint pseudotime as maturation-associated ordering evidence",
    "Position user inference as reusable signature-priority demo, not primary evidence",
    rep("Integrate evidence into conservative manuscript narrative", max(0, nrow(story_seq) - 6))
  )[seq_len(nrow(story_seq))],
  stringsAsFactors = FALSE
)
safe_write_csv(storyline_table, file.path(output_table_dir, "10N_V2_manuscript_storyline_table.csv"))

# ----------------------------- results skeleton -----------------------------

allowed_phrases <- c(
  "dopaminergic neuron/graft-related transcriptomic cell-state prioritization",
  "marker-rule-derived prioritization model",
  "locked dopaminergic/graft-related dataset scope",
  "graph-based pseudotime recapitulated chronological differentiation progression",
  "transcriptomic priority proxy increased in later pseudotime states",
  "marker-targeted disease-context validation",
  "signature-priority inference demo"
)

blocked_phrases <- c(
  "Parkinson's disease clinical-use model",
  "prediction of graft efficacy",
  "prediction of clinical safety",
  "tumorigenicity prediction",
  "anatomical projection or host integration",
  "functional innervation",
  "true lineage tracing or fate mapping",
  "full independent clinical test set",
  "serialized 09C frozen-model prediction for 10L"
)

results_skeleton <- data.frame(
  paragraph_id = sprintf("R%02d", seq_len(nrow(storyline_table))),
  section_title = storyline_table$results_section_title,
  linked_main_figures = storyline_table$linked_main_figures,
  opening_sentence_template = paste0(
    "We next evaluated ", tolower(storyline_table$results_section_title), 
    " within a dopaminergic neuron/graft-related transcriptomic prioritization framework."
  ),
  evidence_sentence_template = storyline_table$core_message,
  conservative_interpretation_template = paste0(
    "Together, these results support transcriptomic cell-state prioritization, while remaining bounded by ",
    storyline_table$required_caution_language
  ),
  allowed_wording = collapse_nonempty(allowed_phrases),
  blocked_wording = collapse_nonempty(blocked_phrases),
  stringsAsFactors = FALSE
)
safe_write_csv(results_skeleton, file.path(output_table_dir, "10N_V2_results_section_skeleton.csv"))

# Text version of Results skeleton
results_lines <- c(
  "10N ADVANCED RESULTS SECTION SKELETON V2",
  "========================================",
  "",
  "Project framing used throughout Results:",
  "  Dopaminergic neuron/graft-related transcriptomic cell-state prioritization framework.",
  "",
  "Global claim boundary:",
  "  This is not a Parkinson's disease clinical-use model, not a clinical graft-efficacy/safety model,",
  "  and not lineage tracing. Pseudotime is graph-based transcriptomic ordering.",
  ""
)
for (i in seq_len(nrow(results_skeleton))) {
  results_lines <- c(
    results_lines,
    paste0("[", results_skeleton$paragraph_id[i], "] ", results_skeleton$section_title[i]),
    paste0("Linked figures: ", results_skeleton$linked_main_figures[i]),
    paste0("Opening: ", results_skeleton$opening_sentence_template[i]),
    paste0("Evidence: ", results_skeleton$evidence_sentence_template[i]),
    paste0("Conservative interpretation: ", results_skeleton$conservative_interpretation_template[i]),
    ""
  )
}
safe_write_lines(results_lines, file.path(output_text_dir, "10N_V2_results_text_skeleton.txt"))

# ----------------------------- figure narrative logic -----------------------

main_id <- get_col_or_blank(main_fig, c("main_figure_id", "figure_id", "Figure", "figure", "main_figure"))
main_title <- get_col_or_blank(main_fig, c("title", "figure_title", "Title"))
main_panels <- get_col_or_blank(main_fig, c("panels", "panel", "panel_description", "Panel description"))
main_source <- get_col_or_blank(main_fig, c("source_module", "source", "module", "Source module"))
main_role <- get_col_or_blank(main_fig, c("intended_role", "role", "manuscript_role"))
main_claim <- get_col_or_blank(main_fig, c("claim_boundary", "claim", "boundary"))
main_priority <- get_col_or_blank(main_fig, c("include_priority", "priority", "status"))

figure_narrative <- data.frame(
  figure_id = main_id,
  figure_title = main_title,
  source_module = main_source,
  planned_panels = main_panels,
  narrative_role = ifelse(nchar(main_role) > 0, main_role, "Main-text evidence panel"),
  result_logic = paste0("This figure should support: ", ifelse(nchar(main_title) > 0, main_title, main_id)),
  claim_boundary = ifelse(nchar(main_claim) > 0, main_claim, "Use conservative transcriptomic prioritization wording only."),
  include_priority = main_priority,
  action_for_10O = "Source-lock exact panel-level PDFs and verify figure-source provenance before assembly.",
  stringsAsFactors = FALSE
)
safe_write_csv(figure_narrative, file.path(output_table_dir, "10N_V2_figure_by_figure_narrative_logic.csv"))

fig_lines <- c(
  "10N FIGURE-BY-FIGURE NARRATIVE LOGIC V2",
  "========================================",
  ""
)
for (i in seq_len(nrow(figure_narrative))) {
  fig_lines <- c(
    fig_lines,
    paste0(figure_narrative$figure_id[i], ": ", figure_narrative$figure_title[i]),
    paste0("  Source module: ", figure_narrative$source_module[i]),
    paste0("  Panels: ", figure_narrative$planned_panels[i]),
    paste0("  Narrative role: ", figure_narrative$narrative_role[i]),
    paste0("  Claim boundary: ", figure_narrative$claim_boundary[i]),
    ""
  )
}
safe_write_lines(fig_lines, file.path(output_text_dir, "10N_V2_figure_by_figure_narrative_logic.txt"))

# ----------------------------- claim-boundary map ---------------------------

claim_map <- data.frame(
  paragraph_id = results_skeleton$paragraph_id,
  section_title = results_skeleton$section_title,
  linked_figures = results_skeleton$linked_main_figures,
  allowed_claim_type = c(
    "Scope/framing only",
    "Transcriptomic cell-state prioritization",
    "Marker-rule-derived model support within locked scope",
    "External/contextual transcriptomic validation",
    "Graph-based pseudotime chronological ordering",
    "Signature-priority user-inference demo",
    rep("Conservative integration", max(0, nrow(results_skeleton) - 6))
  )[seq_len(nrow(results_skeleton))],
  required_safe_wording = c(
    "dopaminergic neuron/graft-related transcriptomic prioritization framework",
    "transcriptional cell-state programs and priority proxies",
    "marker-rule-derived prioritization model; not clinical prediction",
    "external/context validation; not clinical gold-standard testing",
    "graph-based pseudotime; not lineage tracing",
    "signature-priority inference demo; not direct serialized 09C model prediction",
    rep("conservative transcriptomic interpretation", max(0, nrow(results_skeleton) - 6))
  )[seq_len(nrow(results_skeleton))],
  blocked_claims = collapse_nonempty(blocked_phrases),
  manual_check_before_10O = "Check this paragraph after 10O source lock to ensure figure numbers and panel references match.",
  stringsAsFactors = FALSE
)
safe_write_csv(claim_map, file.path(output_table_dir, "10N_V2_result_paragraph_claim_boundary_map.csv"))

# ----------------------------- abstract key message draft -------------------

abstract_lines <- c(
  "10N ABSTRACT-LEVEL KEY MESSAGE DRAFT V2",
  "========================================",
  "",
  "Background:",
  "  Dopaminergic neuron replacement strategies require careful prioritization of transcriptional cell states, but public datasets are heterogeneous and often lack direct functional graft-quality labels.",
  "",
  "Objective:",
  "  We developed a dopaminergic neuron/graft-related transcriptomic cell-state prioritization framework integrating single-cell scoring, marker-rule-derived modeling, external/context validation, and multi-timepoint pseudotime analysis.",
  "",
  "Methods:",
  "  Public dopaminergic/graft/differentiation-related transcriptomic datasets were curated under a locked dataset-role framework. We combined marker/signature scoring, marker-rule-derived machine learning, robustness checks, external/context validation, and graph-based pseudotime analysis in GSE204796.",
  "",
  "Key results:",
  "  The framework identified prioritized and risk-associated transcriptional programs, supported model behavior within the locked dopaminergic/graft-related scope, and showed that multi-timepoint graph-based pseudotime recapitulated chronological progression from D8 to D35. Later pseudotime states showed supportive maturation and priority-proxy trends.",
  "",
  "Conclusion:",
  "  This study provides a reusable transcriptomic prioritization framework for dopaminergic neuron/graft-related cell states. The results should be interpreted as computational transcriptomic prioritization rather than clinical prediction, graft-efficacy prediction, safety prediction, or lineage tracing.",
  ""
)
safe_write_lines(abstract_lines, file.path(output_text_dir, "10N_V2_abstract_key_message_draft.txt"))

# ----------------------------- 10O instructions -----------------------------

source_count <- nrow(source_inventory)
main_count <- nrow(main_fig)
supp_count <- nrow(supp_fig)

source_lock_lines <- c(
  "10N TO 10O SOURCE-LOCK INSTRUCTIONS",
  "====================================",
  "",
  paste0("10M candidate source figure inventory rows detected: ", source_count),
  paste0("10M main figure plan rows: ", main_count),
  paste0("10M supplementary figure plan rows: ", supp_count),
  "",
  "10O should:",
  "  1. Source-lock exact panel-level PDFs for every accepted main/supplementary panel.",
  "  2. Preserve 10K final as V4 analysis + V7 figure export heatmap left-label final fix.",
  "  3. Preserve 10J final as V15 separate publication-safe pilot only if used as supplementary/diagnostic.",
  "  4. Preserve 10L as signature-priority inference demo, not serialized 09C model prediction.",
  "  5. Verify that all figure labels match the 10N storyline and claim-boundary map.",
  "  6. Do not assemble 10P until 10O resolves all missing/ambiguous source panels.",
  ""
)
if (nrow(ref_scaffold) > 0) {
  source_lock_lines <- c(source_lock_lines, "Old-to-new reference scaffold from 10M:", "")
  for (i in seq_len(nrow(ref_scaffold))) {
    source_lock_lines <- c(
      source_lock_lines,
      paste0("- ", paste(ref_scaffold[i, ], collapse = " | "))
    )
  }
}
safe_write_lines(source_lock_lines, file.path(output_text_dir, "10N_V2_10O_source_lock_instructions.txt"))

# ----------------------------- teacher-safe storyline -----------------------

teacher_lines <- c(
  "10N TEACHER-SAFE STORYLINE SUMMARY V2",
  "======================================",
  "",
  "One-sentence project identity:",
  "  This project is a dopaminergic neuron/graft-related transcriptomic cell-state prioritization framework, not a Parkinson's disease clinical-use model.",
  "",
  "What is strong:",
  "  - The dataset scope has been explicitly locked and audited.",
  "  - The project integrates scoring, marker-rule-derived model support, external/context validation, and multi-timepoint pseudotime.",
  "  - 10K strengthens the storyline because GSE204796 graph-based pseudotime recapitulates D8-D35 chronological progression.",
  "",
  "What must remain conservative:",
  "  - No claim of clinical prediction, graft efficacy, clinical safety, tumorigenicity, host integration, or true lineage tracing.",
  "  - 10L is a signature-priority inference demo because no directly callable serialized 09C model prediction was detected.",
  "",
  "Next step:",
  "  Run 10O only after accepting this storyline, then source-lock all figure panels for V2 assembly.",
  ""
)
safe_write_lines(teacher_lines, file.path(output_text_dir, "10N_V2_teacher_safe_storyline_summary.txt"))

# ----------------------------- execution summary ----------------------------

execution_summary <- data.frame(
  item = c(
    "module", "status", "main_figure_rows", "supplementary_figure_rows",
    "storyline_rows", "module_decision_rows", "source_inventory_rows",
    "next_module", "source_lock_allowed_now"
  ),
  value = c(
    "10N_advanced_manuscript_storyline_V2",
    "completed",
    as.character(nrow(main_fig)),
    as.character(nrow(supp_fig)),
    as.character(nrow(story_seq)),
    as.character(nrow(module_decision)),
    as.character(nrow(source_inventory)),
    "10O_advanced_source_figure_lock_V2",
    "yes_after_user_accepts_10N_storyline"
  ),
  stringsAsFactors = FALSE
)
safe_write_csv(execution_summary, file.path(output_table_dir, "10N_V2_execution_summary.csv"))

report_lines <- c(
  "10N ADVANCED MANUSCRIPT STORYLINE V2 - EXECUTION REPORT",
  "========================================================",
  "",
  paste0("Run time: ", as.character(Sys.time())),
  paste0("Project root: ", project_root),
  paste0("Input table dir: ", input_table_dir),
  paste0("Input text dir: ", input_text_dir),
  paste0("Output table dir: ", output_table_dir),
  paste0("Output text dir: ", output_text_dir),
  "",
  paste0("Main figure rows: ", nrow(main_fig)),
  paste0("Supplementary figure rows: ", nrow(supp_fig)),
  paste0("Storyline rows: ", nrow(story_seq)),
  paste0("Module decision rows: ", nrow(module_decision)),
  paste0("Source inventory rows: ", nrow(source_inventory)),
  "",
  "Generated outputs:",
  "  10N_V2_manuscript_storyline_table.csv",
  "  10N_V2_results_section_skeleton.csv",
  "  10N_V2_figure_by_figure_narrative_logic.csv",
  "  10N_V2_result_paragraph_claim_boundary_map.csv",
  "  10N_V2_abstract_key_message_draft.txt",
  "  10N_V2_results_text_skeleton.txt",
  "  10N_V2_figure_by_figure_narrative_logic.txt",
  "  10N_V2_10O_source_lock_instructions.txt",
  "  10N_V2_teacher_safe_storyline_summary.txt",
  "",
  "Next:",
  "  Review and accept 10N storyline before running 10O source lock.",
  ""
)
safe_write_lines(report_lines, file.path(output_text_dir, "10N_V2_execution_report.txt"))

cat("\n[10N] Completed advanced manuscript storyline V2.\n")
cat("[10N] Main figure rows:", nrow(main_fig), "\n")
cat("[10N] Supplementary figure rows:", nrow(supp_fig), "\n")
cat("[10N] Storyline rows:", nrow(story_seq), "\n")
cat("[10N] Output tables:", output_table_dir, "\n")
cat("[10N] Output text  :", output_text_dir, "\n")
cat("[10N] Next         : review 10N outputs, then run 10O_advanced_source_figure_lock_V2\n")
