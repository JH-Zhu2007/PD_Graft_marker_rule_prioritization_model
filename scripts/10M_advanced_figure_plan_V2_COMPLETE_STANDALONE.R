
options(stringsAsFactors = FALSE)

PROJECT_ROOT <- "D:/PD_Graft_Project"
MODULE_TAG   <- "10M_advanced_figure_plan_V2"

TABLE_DIR <- file.path(PROJECT_ROOT, "03_tables", MODULE_TAG)
TEXT_DIR  <- file.path(PROJECT_ROOT, "09_manuscript", MODULE_TAG)

if (!dir.exists(TABLE_DIR)) dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(TEXT_DIR))  dir.create(TEXT_DIR,  recursive = TRUE, showWarnings = FALSE)

cat("\n[10M] Starting advanced figure plan V2...\n")
cat("[10M] Project root :", PROJECT_ROOT, "\n")
cat("[10M] Output tables:", TABLE_DIR, "\n")
cat("[10M] Output text  :", TEXT_DIR, "\n")

safe_write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
  cat("[10M] Wrote:", path, "\n")
}

safe_write_lines <- function(x, path) {
  writeLines(enc2utf8(x), con = path, useBytes = TRUE)
  cat("[10M] Wrote:", path, "\n")
}

find_files <- function(root, pattern, recursive = TRUE) {
  if (!dir.exists(root)) return(character(0))
  out <- list.files(root, pattern = pattern, recursive = recursive, full.names = TRUE, ignore.case = TRUE)
  out <- out[file.exists(out)]
  sort(normalizePath(out, winslash = "/", mustWork = FALSE))
}

first_or_na <- function(x) {
  if (length(x) == 0) return(NA_character_)
  x[1]
}

collapse_paths <- function(x, max_n = 3) {
  if (length(x) == 0) return(NA_character_)
  x <- sort(unique(normalizePath(x, winslash = "/", mustWork = FALSE)))
  if (length(x) > max_n) {
    paste(c(x[seq_len(max_n)], paste0("... +", length(x) - max_n, " more")), collapse = " | ")
  } else {
    paste(x, collapse = " | ")
  }
}

FIG_ROOT <- file.path(PROJECT_ROOT, "04_figures")
TAB_ROOT <- file.path(PROJECT_ROOT, "03_tables")
TXT_ROOT <- file.path(PROJECT_ROOT, "09_manuscript")

module_scan <- data.frame(
  module = c(
    "10D_old_final_multipanel_assembly",
    "10E_old_consistency_audit",
    "10F_old_legends_reference_map",
    "10G_dataset_domain_project_reframing",
    "10H_dataset_role_model_scope_freeze",
    "10I_pseudotime_input_readiness",
    "10J_D8_pseudotime_pilot_final",
    "10K_multi_timepoint_pseudotime_final",
    "10L_user_scRNA_inference_demo"
  ),
  expected_final_status = c(
    "Old baseline figure package; keep as backup, not deleted.",
    "Old baseline audit; keep as backup.",
    "Old baseline legends/reference map; keep as backup.",
    "Completed: reframed from PD disease project to dopaminergic neuron/graft cell-state prioritization.",
    "Completed: locked core and non-core datasets/model scope.",
    "Completed: selected pseudotime input candidates.",
    "Completed: D8-only pilot; diagnostic only after 10K.",
    "Completed: multi-timepoint trajectory; candidate for main figure.",
    "Completed: signature-priority demo; not serialized 09C ML prediction."
  ),
  evidence_glob_hint = c(
    "10D_final_multipanel_figure_assembly_V17",
    "10E_final_consistency_audit_V2_FAST",
    "10F_figure_legends_and_reference_map_V1",
    "10G0_dataset_domain_and_project_reframing_audit_V8_STANDALONE_STRICT",
    "10H_dataset_role_and_model_scope_freeze_V1",
    "10I_pseudotime_input_readiness_audit_V2_COMPLETE_STANDALONE",
    "10J_pseudotime_pilot_V15_FINAL_SEPARATE_PUBLICATION_SAFE",
    "10K_final_pseudotime_trajectory_analysis_V7_F_HEATMAP_LEFT_LABEL_FINAL_FIX",
    "10L_user_scRNA_frozen_predictor_inference_V2_SAFE_ZSCORE_COMPLETE_STANDALONE"
  ),
  stringsAsFactors = FALSE
)

module_scan$tables_found <- vapply(module_scan$evidence_glob_hint, function(h) {
  collapse_paths(find_files(TAB_ROOT, paste0(".*", gsub("([\\.\\+\\?\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1", h), ".*"), recursive = TRUE), max_n = 2)
}, character(1))

module_scan$figures_found <- vapply(module_scan$evidence_glob_hint, function(h) {
  collapse_paths(find_files(FIG_ROOT, paste0(".*", gsub("([\\.\\+\\?\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1", h), ".*"), recursive = TRUE), max_n = 2)
}, character(1))

module_scan$text_found <- vapply(module_scan$evidence_glob_hint, function(h) {
  collapse_paths(find_files(TXT_ROOT, paste0(".*", gsub("([\\.\\+\\?\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1", h), ".*"), recursive = TRUE), max_n = 2)
}, character(1))

safe_write_csv(module_scan, file.path(TABLE_DIR, "10M_V2_module_completion_and_evidence_scan.csv"))

all_pdfs <- find_files(FIG_ROOT, "\\.pdf$", recursive = TRUE)

source_by_keywords <- function(required = character(0), avoid = character(0), prefer = character(0)) {
  x <- all_pdfs
  if (length(required) > 0) {
    for (kw in required) x <- x[grepl(kw, basename(x), ignore.case = TRUE) | grepl(kw, x, ignore.case = TRUE)]
  }
  if (length(avoid) > 0) {
    for (kw in avoid) x <- x[!grepl(kw, basename(x), ignore.case = TRUE) & !grepl(kw, x, ignore.case = TRUE)]
  }
  if (length(x) == 0) return(NA_character_)
  if (length(prefer) > 0) {
    score <- rep(0, length(x))
    for (kw in prefer) score <- score + as.integer(grepl(kw, x, ignore.case = TRUE))
    x <- x[order(-score, x)]
  }
  normalizePath(x[1], winslash = "/", mustWork = FALSE)
}

src <- list(
  old_F1_umap             = source_by_keywords(c("F1", "UMAP"), prefer = c("10D", "V17")),
  old_DA_score            = source_by_keywords(c("DA", "score"), prefer = c("10D", "08A", "05A")),
  old_safety_score        = source_by_keywords(c("safety", "risk"), prefer = c("10D", "05B")),
  old_priority_index      = source_by_keywords(c("priority", "index"), prefer = c("09A", "10D")),
  old_volcano             = source_by_keywords(c("volcano"), prefer = c("10D", "Figure_3", "F2B")),
  old_GO                  = source_by_keywords(c("GO"), prefer = c("10D", "F2C")),
  old_KEGG                = source_by_keywords(c("KEGG"), prefer = c("10D", "F2D")),
  old_Hallmark            = source_by_keywords(c("Hallmark"), avoid = c("dot", "spot", "bubble"), prefer = c("bar", "10D", "10C")),
  old_internal_CV         = source_by_keywords(c("internal", "CV"), prefer = c("09C", "10D")),
  old_LODO                = source_by_keywords(c("LODO"), prefer = c("09C", "10D")),
  old_feature_importance  = source_by_keywords(c("feature", "importance"), prefer = c("09C", "10D")),
  old_negative_control    = source_by_keywords(c("negative", "control"), prefer = c("09H", "10D")),
  old_GSE183248_priority  = source_by_keywords(c("GSE183248", "priority"), prefer = c("09F", "10D")),
  old_GSE183248_heatmap   = source_by_keywords(c("GSE183248", "heatmap"), prefer = c("09F", "10D")),
  old_GSE243639_import    = source_by_keywords(c("GSE243639", "import"), prefer = c("09I", "10D")),
  old_GSE243639_cluster   = source_by_keywords(c("GSE243639", "cluster"), prefer = c("09I", "10D")),
  old_GSE243639_priority  = source_by_keywords(c("GSE243639", "priority"), prefer = c("09I", "10D")),
  tenK_A_timepoint        = source_by_keywords(c("10K", "V7", "A_embedding_by_timepoint"), prefer = c("V7")),
  tenK_B_pseudotime       = source_by_keywords(c("10K", "V7", "B_embedding_pseudotime"), prefer = c("V7")),
  tenK_C_timepoint_order  = source_by_keywords(c("10K", "V7", "C_pseudotime_by_timepoint"), prefer = c("V7")),
  tenK_D_program          = source_by_keywords(c("10K", "V7", "D_program_trends"), prefer = c("V7")),
  tenK_E_priority         = source_by_keywords(c("10K", "V7", "E_priority_proxy"), prefer = c("V7")),
  tenK_F_heatmap          = source_by_keywords(c("10K", "V7", "F_marker_trend_heatmap"), prefer = c("V7")),
  tenL_A_embedding        = source_by_keywords(c("10L", "V2", "A_embedding"), prefer = c("V2")),
  tenL_B_cluster          = source_by_keywords(c("10L", "V2", "B_cluster"), prefer = c("V2")),
  tenL_C_heatmap          = source_by_keywords(c("10L", "V2", "C_cluster"), prefer = c("V2"))
)

source_inventory <- data.frame(
  source_key = names(src),
  source_path = unlist(src, use.names = FALSE),
  source_available = !is.na(unlist(src, use.names = FALSE)),
  stringsAsFactors = FALSE
)
safe_write_csv(source_inventory, file.path(TABLE_DIR, "10M_V2_candidate_source_figure_inventory.csv"))

main_figs <- data.frame(
  figure_id = character(0),
  figure_title = character(0),
  panel_id = character(0),
  panel_title = character(0),
  source_module = character(0),
  source_file_candidate = character(0),
  panel_role = character(0),
  main_claim_allowed = character(0),
  claim_boundary = character(0),
  priority = character(0),
  status = character(0),
  stringsAsFactors = FALSE
)

add_main <- function(fig, fig_title, panel, panel_title, module, source_key, role, claim, boundary, priority = "required") {
  path <- if (!is.null(src[[source_key]])) src[[source_key]] else NA_character_
  st <- ifelse(is.na(path), "SOURCE_NOT_AUTO_FOUND_REVIEW_IN_10O", "SOURCE_CANDIDATE_FOUND")
  main_figs <<- rbind(main_figs, data.frame(
    figure_id = fig,
    figure_title = fig_title,
    panel_id = panel,
    panel_title = panel_title,
    source_module = module,
    source_file_candidate = path,
    panel_role = role,
    main_claim_allowed = claim,
    claim_boundary = boundary,
    priority = priority,
    status = st,
    stringsAsFactors = FALSE
  ))
}

add_main("Figure 1", "Dopaminergic graft-related cell-state atlas and transcriptional scoring framework", "1A", "Cell-state layout / atlas", "08A/10D old baseline", "old_F1_umap", "atlas", "Defines the single-cell state space used for downstream prioritization.", "Do not call this a PD disease atlas or clinical cell-therapy atlas.")
add_main("Figure 1", "Dopaminergic graft-related cell-state atlas and transcriptional scoring framework", "1B", "Dopaminergic / projection-associated molecular competence score", "05A/08A", "old_DA_score", "scoring", "Shows molecular competence/prioritization features across cell states.", "Do not claim anatomical projection or functional innervation.")
add_main("Figure 1", "Dopaminergic graft-related cell-state atlas and transcriptional scoring framework", "1C", "Safety-risk-associated transcriptional score", "05B/08A", "old_safety_score", "risk scoring", "Shows risk-associated transcriptional programs across cell states.", "Do not claim clinical safety or tumorigenicity prediction.")

add_main("Figure 2", "Candidate-state prioritization and transcriptional evidence", "2A", "Dataset/cell-state priority index", "09A", "old_priority_index", "prioritization", "Ranks candidate states/datasets by locked transcriptomic prioritization rules.", "Do not call this a functional graft-success score.")
add_main("Figure 2", "Candidate-state prioritization and transcriptional evidence", "2B", "Differential expression volcano", "06/10D", "old_volcano", "DEG evidence", "Shows transcriptional differences supporting candidate-state prioritization.", "Do not overinterpret individual genes without experimental validation.")

add_main("Figure 3", "Functional enrichment of candidate-state transcriptional programs", "3A", "GO enrichment", "06/10D", "old_GO", "functional enrichment", "Summarizes biological processes enriched in prioritized signatures.", "Enrichment is pathway-level association, not mechanistic proof.")
add_main("Figure 3", "Functional enrichment of candidate-state transcriptional programs", "3B", "KEGG enrichment", "06/10D", "old_KEGG", "functional enrichment", "Provides pathway-level context for prioritized signatures.", "Do not claim pathway activation without direct functional assays.")
add_main("Figure 3", "Functional enrichment of candidate-state transcriptional programs", "3C", "Hallmark GSEA barplot", "06/10C/10D", "old_Hallmark", "functional enrichment", "Shows Hallmark program enrichment with barplot representation.", "Use barplot, not dot/spot plot, per locked preference.")

add_main("Figure 4", "Marker-rule-derived machine-learning audit and cross-dataset generalization", "4A", "Internal cross-validation performance", "09C", "old_internal_CV", "ML audit", "Reports internal model performance under locked marker-rule-derived setup.", "marker-rule-derived prioritization model is not clinical prediction.")
add_main("Figure 4", "Marker-rule-derived machine-learning audit and cross-dataset generalization", "4B", "Leave-one-dataset-out generalization", "09C", "old_LODO", "ML audit", "Evaluates cross-dataset robustness within dopaminergic/graft-related scope.", "Do not describe as gold-standard external clinical test set.")
add_main("Figure 4", "Marker-rule-derived machine-learning audit and cross-dataset generalization", "4C", "Feature importance / model interpretation", "09C", "old_feature_importance", "model interpretation", "Shows which features support marker-rule-derived prioritization.", "Feature importance is interpretability, not causal evidence.")

add_main("Figure 5", "Robustness and negative-control analyses", "5A", "Negative-control performance", "09H", "old_negative_control", "robustness", "Shows prioritization is not explained only by generic artifacts.", "Negative controls reduce but do not eliminate all confounding.")
add_main("Figure 5", "Robustness and negative-control analyses", "5B", "Threshold / sensitivity stability", "09G/10D", "old_negative_control", "robustness", "Summarizes sensitivity of key calls across thresholds.", "Report as robustness evidence, not absolute biological truth.", priority = "optional_if_source_available")

add_main("Figure 6", "Multi-timepoint pseudotime recapitulates dopaminergic differentiation chronology", "6A", "GSE204796 time-course cell-state layout", "10K V4/V7", "tenK_A_timepoint", "trajectory", "Shows multi-timepoint cell-state organization from D8 to D35.", "Computational ordering only; not lineage tracing.")
add_main("Figure 6", "Multi-timepoint pseudotime recapitulates dopaminergic differentiation chronology", "6B", "Graph-based pseudotime embedding", "10K V4/V7", "tenK_B_pseudotime", "trajectory", "Shows graph-based pseudotime gradient across the time-course state space.", "Do not call this real fate mapping.")
add_main("Figure 6", "Multi-timepoint pseudotime recapitulates dopaminergic differentiation chronology", "6C", "Pseudotime ordering by chronological timepoint", "10K V4/V7", "tenK_C_timepoint_order", "trajectory validation", "Pseudotime strongly recapitulates D8 to D35 chronological progression.", "Use the measured correlation as computational validation, not biological ground truth.")

add_main("Figure 7", "Trajectory-linked maturation, risk, and priority programs", "7A", "Program trends along pseudotime", "10K V4/V7", "tenK_D_program", "trajectory-linked programs", "Neuronal maturation and progenitor/cell-cycle programs shift along pseudotime.", "DA maturation is not perfectly monotonic; write conservatively.")
add_main("Figure 7", "Trajectory-linked maturation, risk, and priority programs", "7B", "Priority proxy along pseudotime", "10K V4/V7", "tenK_E_priority", "trajectory-linked priority", "Priority proxy increases in later pseudotime states with moderate positive association.", "Proxy is not frozen 09C model prediction or graft success.")
add_main("Figure 7", "Trajectory-linked maturation, risk, and priority programs", "7C", "Marker trends across pseudotime bins", "10K V4/V7", "tenK_F_heatmap", "marker trajectory", "Shows marker-level trends across pseudotime bins.", "Use as supportive marker evidence; not standalone proof.", priority = "candidate_supp_if_too_dense")

add_main("Figure 8", "External validation in independent dopaminergic scRNA-seq context", "8A", "GSE183248 external priority index", "09F", "old_GSE183248_priority", "external validation", "Tests prioritization in an independent dopaminergic/graft-related dataset.", "External validation is transcriptomic, not clinical outcome validation.")
add_main("Figure 8", "External validation in independent dopaminergic scRNA-seq context", "8B", "Frozen-signature heatmap", "09F", "old_GSE183248_heatmap", "external validation", "Shows signature preservation across external cell states.", "Do not claim universal generalization beyond tested domain.")

add_main("Figure 9", "Disease-context marker-targeted validation in GSE243639", "9A", "GSE243639 marker-targeted import summary", "09I", "old_GSE243639_import", "context validation", "Places prioritization markers in a disease-context single-cell reference.", "Marker-targeted context validation is not a full independent test set.")
add_main("Figure 9", "Disease-context marker-targeted validation in GSE243639", "9B", "Disease-context cluster sizes", "09I/10C V16", "old_GSE243639_cluster", "context validation", "Summarizes disease-context cluster landscape for interpretation.", "Do not infer clinical abundance effects.")
add_main("Figure 9", "Disease-context marker-targeted validation in GSE243639", "9C", "Context priority index / signature scoring", "09I", "old_GSE243639_priority", "context validation", "Evaluates marker-targeted prioritization in disease-context data.", "Use as contextual support only.")

safe_write_csv(main_figs, file.path(TABLE_DIR, "10M_V2_main_figure_plan.csv"))

supp_figs <- data.frame(
  supplementary_figure_id = character(0),
  title = character(0),
  panels = character(0),
  source_module = character(0),
  intended_role = character(0),
  claim_boundary = character(0),
  include_priority = character(0),
  stringsAsFactors = FALSE
)

add_supp <- function(id, title, panels, module, role, boundary, priority = "recommended") {
  supp_figs <<- rbind(supp_figs, data.frame(
    supplementary_figure_id = id,
    title = title,
    panels = panels,
    source_module = module,
    intended_role = role,
    claim_boundary = boundary,
    include_priority = priority,
    stringsAsFactors = FALSE
  ))
}

add_supp("Supplementary Figure 1", "Dataset-domain and model-scope audit", "10G/10H dataset role tables and model-scope freeze summaries", "10G/10H", "transparency / teacher defense", "Shows dataset roles; does not add biological evidence.")
add_supp("Supplementary Figure 2", "QC, object inclusion and preprocessing diagnostics", "QC/object readiness summary; object manifest", "02/10I", "reproducibility", "QC supports analysis quality but is not biological conclusion.")
add_supp("Supplementary Figure 3", "Additional cell-state scoring diagnostics", "DA/projection/safety score distributions and signature coverage", "05/08", "diagnostic support", "Signature scores are transcriptomic proxies.")
add_supp("Supplementary Figure 4", "Additional ML diagnostics", "LODO by dataset, probability distributions, calibration-like summaries", "09C", "ML transparency", "Marker-rule-derived only; avoid clinical prediction language.")
add_supp("Supplementary Figure 5", "Negative-control and robustness details", "09G threshold stability and 09H empirical significance details", "09G/09H", "robustness", "Robustness is supportive, not definitive proof.")
add_supp("Supplementary Figure 6", "10J D8-only pseudotime pilot", "10J V15 cluster layout, pseudotime, dotrange, program trends, priority proxy, heatmap", "10J", "pilot diagnostic", "D8-only pilot is diagnostic; main trajectory evidence comes from 10K.")
add_supp("Supplementary Figure 7", "10K marker trend heatmap and additional trajectory diagnostics", "10K V7 marker heatmap or additional timepoint/cluster summaries", "10K", "trajectory support", "Computational pseudotime only; not lineage tracing.")
add_supp("Supplementary Figure 8", "External validation details in GSE183248", "Additional external heatmaps/probability distributions/cluster summaries", "09F", "external validation support", "Independent transcriptomic validation only.")
add_supp("Supplementary Figure 9", "Disease-context marker-targeted validation details", "GSE243639 marker overlap, heatmaps, cluster diagnostic details", "09I", "context validation", "Marker-targeted context; not full clinical validation.")
add_supp("Supplementary Figure 10", "10L user scRNA signature-priority inference demo", "10L V2 embedding, cluster priority dotrange, program heatmap", "10L", "GitHub/tool extension", "Signature-priority fallback only; not serialized 09C ML prediction.")

safe_write_csv(supp_figs, file.path(TABLE_DIR, "10M_V2_supplementary_figure_plan.csv"))

module_decisions <- data.frame(
  module = c("10G", "10H", "10I", "10J", "10K", "10L"),
  final_lock = c(
    "10G final = V8_STANDALONE_STRICT",
    "10H final = V1_dataset_role_and_model_scope_freeze",
    "10I final = V2_COMPLETE_STANDALONE",
    "10J final = V15_FINAL_SEPARATE_PUBLICATION_SAFE",
    "10K final = V4 analysis + V7 figure export heatmap left-label final fix",
    "10L final = V2_SAFE_ZSCORE_COMPLETE_STANDALONE"
  ),
  conclusion = c(
    "Project reframed from PD disease project to dopaminergic neuron/graft-related cell-state prioritization framework.",
    "Core model-development scope locked to dopaminergic/graft/differentiation-related datasets; non-core datasets separated.",
    "Pseudotime candidate object readiness completed.",
    "D8-only pseudotime pilot completed; use as diagnostic/supplementary only.",
    "Multi-timepoint GSE204796 trajectory recapitulated chronological progression; candidate for main trajectory figures.",
    "User scRNA inference demo completed using signature-priority proxy fallback; no serialized 09C model prediction detected."
  ),
  figure_plan_decision = c(
    "Supplementary transparency / Methods / teacher defense.",
    "Supplementary transparency / Methods / teacher defense.",
    "Do not include as main result; mention as input readiness.",
    "Supplementary or diagnostic only.",
    "Include in main Figures 6-7 if 10N storyline confirms.",
    "Supplementary/GitHub extension only; not main evidence."
  ),
  must_not_claim = c(
    "Do not call the project a PD clinical-use model.",
    "Do not claim unrelated tissue datasets entered the core model if locked as non-core.",
    "Do not use readiness audit as biological result.",
    "Do not claim D8-only pseudotime proves maturation trajectory.",
    "Do not claim true lineage tracing or fate mapping.",
    "Do not claim frozen 09C ML prediction if serialized model prediction is FALSE."
  ),
  stringsAsFactors = FALSE
)
safe_write_csv(module_decisions, file.path(TABLE_DIR, "10M_V2_module_integration_decision_table.csv"))

storyline <- data.frame(
  result_order = 1:9,
  result_section_title = c(
    "Define the dopaminergic graft-related cell-state prioritization framework",
    "Identify candidate transcriptional states and functional programs",
    "Audit marker-rule-derived prioritization model prioritization and cross-dataset generalization",
    "Demonstrate robustness against negative controls and threshold choices",
    "Show multi-timepoint pseudotime recapitulates chronological differentiation",
    "Link pseudotime to maturation, risk and priority programs",
    "Validate prioritization in an independent dopaminergic single-cell dataset",
    "Evaluate marker-targeted disease-context support",
    "Provide a user-facing signature-priority inference extension"
  ),
  linked_main_figures = c(
    "Figure 1",
    "Figures 2-3",
    "Figure 4",
    "Figure 5",
    "Figure 6",
    "Figure 7",
    "Figure 8",
    "Figure 9",
    "Supplementary Figure 10 / GitHub module"
  ),
  key_message = c(
    "The study is a transcriptomic cell-state prioritization framework, not a PD disease model.",
    "Candidate states are supported by scoring, differential expression and pathway enrichment.",
    "marker-rule-derived prioritization model supports prioritization within the locked dopaminergic/graft-related domain.",
    "Robustness and negative-control analyses reduce artifact concerns.",
    "GSE204796 multi-timepoint pseudotime follows D8-D35 chronology.",
    "Maturation/priority proxy increases in later pseudotime states, while progenitor/cell-cycle programs decline.",
    "External dopaminergic context supports the transcriptomic prioritization concept.",
    "Disease-context data provide marker-targeted contextual support but not a full test set.",
    "10L demonstrates reusable signature-priority scoring for user scRNA objects, not serialized ML prediction."
  ),
  caution_language = c(
    "Avoid PD clinical prediction wording.",
    "Avoid causal pathway claims.",
    "Avoid gold-standard clinical test-set language.",
    "Avoid claiming all confounding is removed.",
    "Avoid lineage tracing/fate mapping wording.",
    "Avoid overclaiming DA maturation monotonicity.",
    "Avoid universal generalization claims.",
    "Avoid clinical disease mechanism conclusions.",
    "Explicitly label signature-priority fallback."
  ),
  stringsAsFactors = FALSE
)
safe_write_csv(storyline, file.path(TABLE_DIR, "10M_V2_results_storyline_sequence_for_10N.csv"))

old_to_new <- data.frame(
  old_reference_type = c(
    "Old Figure 1",
    "Old Figure 2",
    "Old Figure 3",
    "Old Figure 4",
    "Old Figure 5",
    "Old Figure 6",
    "Old Figure 7",
    "Old Figure 8",
    "Old Figure 9",
    "Old Figure 10",
    "New 10K trajectory",
    "New 10L inference demo"
  ),
  proposed_new_location = c(
    "New Figure 1",
    "New Figures 2-3 depending panel type",
    "New Figure 4",
    "New Figure 5",
    "New Figure 8 or 9 depending source",
    "New Figure 6",
    "New Figure 7",
    "New Figure 8",
    "New Figure 9",
    "New Figure 9 / supplementary depending panel",
    "New Figures 6-7",
    "Supplementary Figure 10 / GitHub extension"
  ),
  action_for_10N_10O = c(
    "Map exact old panels to new panels during 10O source lock.",
    "Split DEG and enrichment content into separate clearer figures.",
    "Use only if still matches ML audit storyline.",
    "Use as robustness figure after ML audit.",
    "Avoid overloading disease-context panels.",
    "Replace/upgrade with 10K V7 multi-timepoint trajectory.",
    "Use 10K V7 trajectory-linked programs and priority proxy.",
    "Keep external validation as one figure.",
    "Keep GSE243639 context validation as one figure.",
    "Move dense marker/diagnostic panels to supplementary if needed.",
    "Add as core new advanced module.",
    "Add as tool/demo extension only."
  ),
  stringsAsFactors = FALSE
)
safe_write_csv(old_to_new, file.path(TABLE_DIR, "10M_V2_old_to_new_figure_reference_scaffold.csv"))

claim_boundary <- c(
  "10M ADVANCED FIGURE PLAN V2 - CLAIM BOUNDARY NOTE",
  "=================================================",
  "",
  "Final project framing:",
  "  Dopaminergic neuron / graft-related cell-state transcriptomic prioritization framework.",
  "",
  "Allowed claims:",
  "  1. The framework prioritizes dopaminergic/graft-related transcriptional cell states.",
  "  2. marker-rule-derived prioritization model supports prioritization within a locked dopaminergic/graft-related dataset scope.",
  "  3. GSE204796 multi-timepoint pseudotime recapitulates D8-D35 chronological progression.",
  "  4. Later pseudotime states show supportive maturation/priority-proxy trends.",
  "  5. External and disease-context datasets provide transcriptomic/contextual validation.",
  "  6. 10L provides a reusable signature-priority inference demo for user scRNA objects.",
  "",
  "Blocked claims:",
  "  1. Do NOT call this a Parkinson's disease clinical-use model.",
  "  2. Do NOT claim prediction of graft efficacy, clinical safety, or tumorigenicity.",
  "  3. Do NOT claim anatomical projection, host integration, or functional innervation.",
  "  4. Do NOT claim pseudotime is true lineage tracing or fate mapping.",
  "  5. Do NOT call marker-targeted disease-context validation a full independent clinical test set.",
  "  6. Do NOT call 10L serialized 09C frozen-model prediction, because 10L V2 used signature-priority fallback.",
  "",
  "Preferred wording:",
  "  graph-based pseudotime recapitulated chronological differentiation progression",
  "  transcriptomic priority proxy increased in later pseudotime states",
  "  marker-rule-derived prioritization model",
  "  marker-targeted disease-context validation",
  "  signature-priority inference demo",
  "",
  "Next module:",
  "  10N_advanced_manuscript_storyline_V2 should use this plan to rewrite the Results structure and manuscript skeleton."
)
safe_write_lines(claim_boundary, file.path(TEXT_DIR, "10M_V2_claim_boundary_and_wording_bank.txt"))

teacher_answer <- c(
  "10M TEACHER-SAFE PROJECT ANSWER",
  "================================",
  "",
  "This project should not be presented as a Parkinson's disease clinical model.",
  "The corrected scope is a dopaminergic neuron/graft-related transcriptomic cell-state prioritization framework.",
  "",
  "The core model-development datasets were locked in 10H within a dopaminergic/graft/differentiation-related scope.",
  "Non-core datasets are used only for external validation, disease-context support, background, or manual review.",
  "",
  "The new 10K module strengthens the project because multi-timepoint pseudotime in GSE204796 recapitulated chronological progression from D8 to D35.",
  "This supports a maturation-associated computational ordering, but it remains graph-based pseudotime rather than lineage tracing.",
  "",
  "The 10L module is useful for GitHub/tool demonstration, but it should be described as signature-priority inference fallback, not direct frozen 09C model prediction.",
  "",
  "Overall, the project is suitable for a rigorous research presentation and GitHub release, with realistic manuscript potential if the final claims remain conservative."
)
safe_write_lines(teacher_answer, file.path(TEXT_DIR, "10M_V2_teacher_safe_project_summary.txt"))

next_steps <- c(
  "NEXT STEPS AFTER 10M",
  "====================",
  "",
  "10M completed: advanced figure plan V2.",
  "",
  "Recommended next module:",
  "  10N_advanced_manuscript_storyline_V2",
  "",
  "10N should generate:",
  "  1. New manuscript storyline table",
  "  2. Updated Results section skeleton",
  "  3. Figure-by-figure narrative logic",
  "  4. Allowed/blocked claim mapping for each result paragraph",
  "  5. Updated abstract-level key message draft",
  "",
  "Do not run 10O source lock until 10N storyline is accepted."
)
safe_write_lines(next_steps, file.path(TEXT_DIR, "10M_V2_next_steps_to_10N.txt"))

summary_df <- data.frame(
  item = c(
    "module",
    "main_figure_rows",
    "supplementary_figure_rows",
    "source_inventory_rows",
    "candidate_sources_found",
    "candidate_sources_missing",
    "recommended_next_module"
  ),
  value = c(
    MODULE_TAG,
    nrow(main_figs),
    nrow(supp_figs),
    nrow(source_inventory),
    sum(source_inventory$source_available),
    sum(!source_inventory$source_available),
    "10N_advanced_manuscript_storyline_V2"
  ),
  stringsAsFactors = FALSE
)
safe_write_csv(summary_df, file.path(TABLE_DIR, "10M_V2_execution_summary.csv"))

cat("\n[10M] Completed advanced figure plan V2.\n")
cat("[10M] Main figure plan rows:", nrow(main_figs), "\n")
cat("[10M] Supplementary figure plan rows:", nrow(supp_figs), "\n")
cat("[10M] Candidate source figures found:", sum(source_inventory$source_available), "/", nrow(source_inventory), "\n")
cat("[10M] Output tables:", TABLE_DIR, "\n")
cat("[10M] Output text  :", TEXT_DIR, "\n")
cat("[10M] Next         : 10N_advanced_manuscript_storyline_V2\n\n")
