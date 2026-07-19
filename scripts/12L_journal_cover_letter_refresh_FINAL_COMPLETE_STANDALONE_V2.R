
cat("\n[12L FINAL V2] Starting Journal / cover-letter refresh with abstract boundary repair...\n")
cat("[12L FINAL] Mode: complete standalone 12L rebuild; no previous 12L dependency; no internet; no 00-10P rerun.\n")
cat("[12L FINAL] Inputs allowed: locked upstream 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F, 12G, 12H, 12I, 12J and 12K outputs.\n")
cat("[12L FINAL] Formal input: 12K V2 GitHub/repository package and 12L handoff.\n")
cat("[12L FINAL] Claim boundary: journal positioning and cover-letter drafting only; no clinical prediction or validated biomarker claim.\n")

graphics.off()

project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")
figure_root <- file.path(project_root, "04_figures")
text_root <- file.path(project_root, "09_manuscript")
script_root <- file.path(project_root, "01_scripts")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2"
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
  cat("[12L FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[12L FINAL] Wrote:", file_value, "\n")
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
    cat("[12L FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
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

if (!dir.exists(table_root)) stop("[12L FINAL] Missing table root: ", table_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
table_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(table_info$size) & table_info$size > 0 & table_info$size < 320 * 1024 * 1024]

all_table_files <- all_table_files[!grepl("12L_journal_cover_letter_refresh", all_table_files, ignore.case = TRUE)]

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

repo_dir_12k <- file.path(project_root, "12K_GitHub_repository_package_FINAL_COMPLETE_STANDALONE_V2")

file_12k_handoff_12l <- first_existing_file(c(
  file.path(table_root, "12K_github_repository_package_refresh_FINAL_COMPLETE_STANDALONE_V2", "12K_FINAL_handoff_to_12L_journal_cover_letter.csv"),
  find_files_all_terms(all_table_files, c("12k", "handoff_to_12l_journal_cover_letter"), max_n = 10)
))
file_12k_repo_manifest <- first_existing_file(c(
  file.path(table_root, "12K_github_repository_package_refresh_FINAL_COMPLETE_STANDALONE_V2", "12K_FINAL_repository_file_manifest.csv"),
  find_files_all_terms(all_table_files, c("12k", "repository_file_manifest"), max_n = 10)
))
file_12k_source_manifest <- first_existing_file(c(
  file.path(table_root, "12K_github_repository_package_refresh_FINAL_COMPLETE_STANDALONE_V2", "12K_FINAL_data_source_manifest.csv"),
  file.path(repo_dir_12k, "metadata", "data_source_manifest.csv"),
  find_files_all_terms(all_table_files, c("12k", "data_source_manifest"), max_n = 10)
))
file_12k_script_index <- first_existing_file(c(
  file.path(table_root, "12K_github_repository_package_refresh_FINAL_COMPLETE_STANDALONE_V2", "12K_FINAL_workflow_script_index.csv"),
  file.path(repo_dir_12k, "metadata", "workflow_script_index.csv"),
  find_files_all_terms(all_table_files, c("12k", "workflow_script_index"), max_n = 10)
))
file_12k_claim_audit <- first_existing_file(c(
  file.path(table_root, "12K_github_repository_package_refresh_FINAL_COMPLETE_STANDALONE_V2", "12K_FINAL_repository_claim_boundary_audit.csv"),
  find_files_all_terms(all_table_files, c("12k", "repository_claim_boundary_audit"), max_n = 10)
))
file_12k_copy_manifest <- first_existing_file(c(
  file.path(table_root, "12K_github_repository_package_refresh_FINAL_COMPLETE_STANDALONE_V2", "12K_FINAL_repository_copy_manifest.csv"),
  find_files_all_terms(all_table_files, c("12k", "repository_copy_manifest"), max_n = 10)
))
file_12j_module_provenance <- first_existing_file(c(
  file.path(table_root, "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE", "12J_FINAL_locked_module_provenance_table.csv"),
  find_files_all_terms(all_table_files, c("12j", "locked_module_provenance_table"), max_n = 10)
))
file_12h_results_text <- first_existing_file(c(
  file.path(text_root, "12H_results_text_refresh_FINAL_COMPLETE_STANDALONE_V4", "12H_FINAL_results_text_full.txt")
))
file_12i_discussion_text <- first_existing_file(c(
  file.path(text_root, "12I_discussion_limitations_refresh_FINAL_COMPLETE_STANDALONE_V3", "12I_FINAL_discussion_text_full.txt")
))
file_12j_methods_text <- first_existing_file(c(
  file.path(text_root, "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE", "12J_FINAL_methods_text_full.txt")
))
file_12j_code_avail <- first_existing_file(c(
  file.path(text_root, "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE", "12J_FINAL_code_availability_statement.txt")
))

handoff_12k_df <- read_table_safe(file_12k_handoff_12l)
repo_manifest_12k_df <- read_table_safe(file_12k_repo_manifest)
source_manifest_12k_df <- read_table_safe(file_12k_source_manifest)
script_index_12k_df <- read_table_safe(file_12k_script_index)
claim_audit_12k_df <- read_table_safe(file_12k_claim_audit)
copy_manifest_12k_df <- read_table_safe(file_12k_copy_manifest)
module_provenance_12j_df <- read_table_safe(file_12j_module_provenance)

if (nrow(handoff_12k_df) < 1) stop("[12L FINAL] Missing 12K handoff to 12L table.", call. = FALSE)
if (nrow(source_manifest_12k_df) < 1) stop("[12L FINAL] Missing 12K data/source manifest.", call. = FALSE)
if (nrow(script_index_12k_df) < 1) stop("[12L FINAL] Missing or empty 12K workflow script index.", call. = FALSE)

input_audit_df <- data.frame(
  input_name = c(
    "12K_handoff_to_12L_journal_cover_letter",
    "12K_repository_file_manifest",
    "12K_data_source_manifest",
    "12K_workflow_script_index",
    "12K_repository_claim_boundary_audit",
    "12K_repository_copy_manifest",
    "12J_locked_module_provenance_table",
    "12H_results_text_full",
    "12I_discussion_text_full",
    "12J_methods_text_full",
    "12J_code_availability_statement",
    "12K_repository_package_dir"
  ),
  detected = c(
    file_12k_handoff_12l != "",
    file_12k_repo_manifest != "",
    file_12k_source_manifest != "",
    file_12k_script_index != "",
    file_12k_claim_audit != "",
    file_12k_copy_manifest != "",
    file_12j_module_provenance != "",
    file_12h_results_text != "",
    file_12i_discussion_text != "",
    file_12j_methods_text != "",
    file_12j_code_avail != "",
    dir.exists(repo_dir_12k)
  ),
  file_path = c(
    file_12k_handoff_12l,
    file_12k_repo_manifest,
    file_12k_source_manifest,
    file_12k_script_index,
    file_12k_claim_audit,
    file_12k_copy_manifest,
    file_12j_module_provenance,
    file_12h_results_text,
    file_12i_discussion_text,
    file_12j_methods_text,
    file_12j_code_avail,
    repo_dir_12k
  ),
  rows_loaded = c(
    nrow(handoff_12k_df),
    nrow(repo_manifest_12k_df),
    nrow(source_manifest_12k_df),
    nrow(script_index_12k_df),
    nrow(claim_audit_12k_df),
    nrow(copy_manifest_12k_df),
    nrow(module_provenance_12j_df),
    ifelse(file_12h_results_text != "", length(readLines(file_12h_results_text, warn = FALSE)), 0),
    ifelse(file_12i_discussion_text != "", length(readLines(file_12i_discussion_text, warn = FALSE)), 0),
    ifelse(file_12j_methods_text != "", length(readLines(file_12j_methods_text, warn = FALSE)), 0),
    ifelse(file_12j_code_avail != "", length(readLines(file_12j_code_avail, warn = FALSE)), 0),
    ifelse(dir.exists(repo_dir_12k), length(list.files(repo_dir_12k, recursive = TRUE, full.names = TRUE)), 0)
  ),
  allowed_as_locked_upstream_input = TRUE,
  stringsAsFactors = FALSE
)
write_csv_safe(input_audit_df, file.path(out_table_dir, "12L_FINAL_locked_12K_input_audit.csv"))

journal_positioning <- data.frame(
  tier = c("Best fit", "Strong fit", "Backup fit", "Avoid"),
  journal_type = c(
    "Computational biology / transcriptomics / systems-biology journal",
    "Disease-model or stem-cell focused journal that accepts computational frameworks",
    "Broad biomedical data-resource or methods-oriented journal",
    "Clinical neurology, therapeutic-efficacy or biomarker-validation journal"
  ),
  why_fit = c(
    "The manuscript is a source-traceable transcriptomic prioritisation framework with multi-layer evidence integration.",
    "The biological context is DA neuron/graft-related differentiation and candidate cell-state prioritisation.",
    "The main deliverables are reproducible scripts, source manifests, prioritisation framework, candidate signatures and claim-boundary controls.",
    "The project does not contain prospective clinical outcome labels, wet-lab validation, graft-function assays or validated biomarker evidence."
  ),
  positioning_language = c(
    "computational transcriptomic prioritisation framework",
    "DA neuron/graft-related candidate-state resource",
    "reproducible multi-layer transcriptomic workflow",
    "not recommended unless additional experimental or clinical validation is added"
  ),
  manual_pre_submission_check = c(
    "Check current aims/scope, article type, data/code policy, figure limits and open-access/APC rules.",
    "Check whether purely computational manuscripts without new wet-lab validation are in scope.",
    "Check whether resource/methods articles and derived public-data analyses are accepted.",
    "Avoid this category for the current manuscript version."
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(journal_positioning, file.path(out_table_dir, "12L_FINAL_journal_positioning_table.csv"))

title_options <- data.frame(
  title_rank = 1:6,
  title_option = c(
    "A source-traceable transcriptomic framework for prioritising dopaminergic neuron and graft-related cell states",
    "Integrated transcriptomic prioritisation of dopaminergic neuron and graft-related candidate cell states",
    "A multi-layer computational framework for dopaminergic neuron graft-related transcriptomic state prioritisation",
    "Source-locked evidence integration for dopaminergic neuron and graft-related transcriptomic cell-state prioritisation",
    "Transcriptomic prioritisation of candidate dopaminergic neuron graft-related cell states using source-traceable evidence integration",
    "A conservative computational resource for dopaminergic neuron and graft-related cell-state prioritisation"
  ),
  recommended_use = c(
    "primary title candidate",
    "shorter title candidate",
    "methods/resource journal candidate",
    "provenance-focused candidate",
    "descriptive full-scope candidate",
    "backup conservative title"
  ),
  claim_safety = c("high", "high", "high", "high", "high", "high"),
  stringsAsFactors = FALSE
)
write_csv_safe(title_options, file.path(out_table_dir, "12L_FINAL_title_options.csv"))

abstract_positioning <- data.frame(
  abstract_section = c("Background", "Methods", "Results", "Conclusions", "Limitations"),
  recommended_message = c(
    "DA neuron/graft-related cell-state prioritisation requires source-traceable integration of public transcriptomic evidence.",
    "We developed a modular computational workflow integrating dataset-role locking, marker-rule-derived prioritisation, pseudotime/module scoring, proxy evidence layers, evidence-tier summaries and reproducibility audits.",
    "The framework generated candidate transcriptomic cell-state and marker-signature outputs supported by temporal, module-level, proxy, genetic-context and marker-rule-derived prioritization model audit layers.",
    "The study provides a reproducible hypothesis-generating transcriptomic prioritisation resource for future experimental validation.",
    "The current study does not establish clinical prediction, therapeutic efficacy, graft safety, anatomical projection or barcode-lineage claim, and requires experimental validation before translational interpretation."
  ),
  figure_or_table_anchor = c("Main Fig. 1", "Main Figs. 1-5", "Main Figs. 2-5; Supplementary Figs. S1-S10", "12K package; 12J Methods", "12I limitations; 12J claim-boundary statement"),
  stringsAsFactors = FALSE
)
write_csv_safe(abstract_positioning, file.path(out_table_dir, "12L_FINAL_abstract_positioning_table.csv"))

cover_letter_blocks <- data.frame(
  block_order = 1:7,
  block_id = c(
    "CL1_opening",
    "CL2_problem_gap",
    "CL3_what_we_did",
    "CL4_key_outputs",
    "CL5_fit_to_journal",
    "CL6_scope_boundary",
    "CL7_closing"
  ),
  block_purpose = c(
    "editor opening",
    "why the study is needed",
    "methodological contribution",
    "main outputs",
    "journal fit",
    "claim boundary",
    "closing statement"
  ),
  text = c(
    paste(
      "Dear Editor,",
      "We are pleased to submit our manuscript, tentatively titled",
      "\"A source-traceable transcriptomic framework for prioritising dopaminergic neuron and graft-related cell states\",",
      "for consideration as a computational transcriptomics study."
    ),
    paste(
      "A major challenge in dopaminergic neuron and graft-related transcriptomic studies is that candidate cell states, maturation programs, projection-associated signatures and risk-context signals are often interpreted across heterogeneous public datasets.",
      "This creates a need for a transparent framework that separates source roles, evidence layers and interpretation boundaries."
    ),
    paste(
      "In this manuscript, we developed a modular, source-traceable computational workflow that integrates dataset-role locking, marker-rule-derived transcriptomic prioritisation, graph-based pseudotime support, module-score correlations, external/proxy evidence layers, integrated evidence tiers and reproducibility audits."
    ),
    paste(
      "The study provides candidate transcriptomic cell-state priorities, candidate marker-signature outputs, source-locked figure/table provenance, a conservative claim-boundary framework and a GitHub-ready reproducibility package.",
      "Together, these outputs support the manuscript as a hypothesis-generating transcriptomic prioritisation resource."
    ),
    paste(
      "We believe this work is suited to journals interested in computational biology, transcriptomics, systems biology, stem-cell differentiation resources or reproducible public-data integration frameworks.",
      "The manuscript emphasizes transparent evidence integration and reproducibility rather than clinical or therapeutic claims."
    ),
    paste(
      "The manuscript is intentionally conservative in scope.",
      "It does not present clinical decision use, validated biomarker status, graft efficacy or safety proof, anatomical-projection claim or barcode-lineage claim.",
      "Instead, it defines a reproducible framework for prioritising candidate transcriptomic states for future experimental validation."
    ),
    paste(
      "We hope the manuscript will be of interest to readers working on dopaminergic neuron differentiation, graft-related transcriptomics, computational cell-state prioritisation and reproducible multi-dataset integration.",
      "Thank you for considering our submission."
    )
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(cover_letter_blocks, file.path(out_table_dir, "12L_FINAL_cover_letter_text_blocks.csv"))
write_tsv_safe(cover_letter_blocks, file.path(out_table_dir, "12L_FINAL_cover_letter_text_blocks.tsv"))

submission_strategy <- data.frame(
  strategy_item = c(
    "Primary positioning",
    "Article type",
    "Required pre-submission check",
    "What to emphasize",
    "What not to emphasize",
    "Reviewer-risk mitigation",
    "Data/code availability",
    "Suggested first submission tier",
    "Fallback strategy"
  ),
  recommendation = c(
    "Computational transcriptomic prioritisation framework for DA neuron/graft-related candidate states.",
    "Computational biology / transcriptomics resource / methods-style research article.",
    "Manually verify journal aims/scope, article type, figure limits, word limits, data/code policy, APC and current editorial standards.",
    "Source traceability, evidence-tier integration, reproducible workflow, conservative boundaries and GitHub-ready package.",
    "Clinical prediction, therapeutic efficacy, validated biomarker, graft safety, anatomical projection or barcode-lineage proof.",
    "Pre-emptively state validation requirements and proxy-evidence limitations in Abstract, Discussion and cover letter.",
    "Public raw data from original repositories; scripts and derived manifests in GitHub/package.",
    "Computational genomics/transcriptomics or systems-biology journal that accepts public-data framework studies.",
    "If rejected for lack of wet-lab validation, redirect to methods/resource/data-integration journals or add experimental validation."
  ),
  ready_for_submission_package = TRUE,
  stringsAsFactors = FALSE
)
write_csv_safe(submission_strategy, file.path(out_table_dir, "12L_FINAL_submission_strategy_table.csv"))

editor_claim_checklist <- data.frame(
  checklist_id = paste0("EC", sprintf("%02d", 1:10)),
  editor_or_reviewer_question = c(
    "Is this a clinical prediction manuscript?",
    "Are markers claimed as validated biomarkers?",
    "Does the study prove graft function or therapeutic efficacy?",
    "Does projection-associated evidence prove anatomical projection?",
    "Does state-level evidence prove barcode lineage tracing?",
    "Does risk-context evidence prove clinical safety?",
    "Does PD genetic-context overlap prove genetic causality?",
    "Is the ML model externally validated with clinical labels?",
    "Are raw public data redistributed in the repository?",
    "Is the study still publishable without wet-lab validation?"
  ),
  safe_answer = c(
    "No. It is a computational transcriptomic prioritisation framework.",
    "No. They are candidate transcriptomic marker signatures for follow-up.",
    "No. Functional validation is required.",
    "No. It is molecular competence proxy support.",
    "No. It is state-level transcriptomic proxy support.",
    "No. It is survival/stress/risk-context transcriptomic support.",
    "No. It is limited disease-context overlap support.",
    "No. ML is a marker-rule-derived prioritization model audit.",
    "No. Raw data should be obtained from original public repositories.",
    "Yes, if framed as a conservative computational framework and not as clinical/functional proof."
  ),
  where_to_address = c(
    "Abstract; Discussion; cover letter",
    "Results; Discussion; claim-boundary statement",
    "Discussion limitations",
    "Results R4; Discussion D4",
    "Results R4; Discussion D4",
    "Limitations table; Discussion D4",
    "Discussion limitations",
    "Methods M7; Discussion D3",
    "README; code availability",
    "Cover letter; journal selection"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(editor_claim_checklist, file.path(out_table_dir, "12L_FINAL_editor_claim_boundary_checklist.csv"))

cover_letter_lines <- c(
  "Cover letter draft",
  "==================",
  "",
  cover_letter_blocks$text[1],
  "",
  cover_letter_blocks$text[2],
  "",
  cover_letter_blocks$text[3],
  "",
  cover_letter_blocks$text[4],
  "",
  cover_letter_blocks$text[5],
  "",
  cover_letter_blocks$text[6],
  "",
  cover_letter_blocks$text[7],
  "",
  "Sincerely,",
  "[Corresponding author name]"
)
writeLines(cover_letter_lines, file.path(out_text_dir, "12L_FINAL_cover_letter_draft.txt"))
writeLines(cover_letter_lines, file.path(out_text_dir, "12L_FINAL_cover_letter_draft.md"))
cat("[12L FINAL] Wrote:", file.path(out_text_dir, "12L_FINAL_cover_letter_draft.txt"), "\n")
cat("[12L FINAL] Wrote:", file.path(out_text_dir, "12L_FINAL_cover_letter_draft.md"), "\n")

title_lines <- c(
  "Title options",
  "=============",
  "",
  paste0(title_options$title_rank, ". ", title_options$title_option, " [", title_options$recommended_use, "]")
)
writeLines(title_lines, file.path(out_text_dir, "12L_FINAL_title_options.txt"))
cat("[12L FINAL] Wrote:", file.path(out_text_dir, "12L_FINAL_title_options.txt"), "\n")

abstract_lines <- c(
  "Abstract positioning draft",
  "==========================",
  "",
  paste(abstract_positioning$abstract_section, abstract_positioning$recommended_message, sep = ": ")
)
writeLines(abstract_lines, file.path(out_text_dir, "12L_FINAL_abstract_positioning_draft.txt"))
cat("[12L FINAL] Wrote:", file.path(out_text_dir, "12L_FINAL_abstract_positioning_draft.txt"), "\n")

strategy_lines <- c(
  "Submission strategy note",
  "========================",
  "",
  paste(submission_strategy$strategy_item, submission_strategy$recommendation, sep = ": "),
  "",
  "Manual verification required before real submission:",
  "Check each journal website for current scope, article types, word limits, figure limits, data/code policy, APC/open-access rules and any current editorial requirements."
)
writeLines(strategy_lines, file.path(out_text_dir, "12L_FINAL_submission_strategy_note.txt"))
cat("[12L FINAL] Wrote:", file.path(out_text_dir, "12L_FINAL_submission_strategy_note.txt"), "\n")

prohibited_positive_phrases <- c(
  "we provide evidence supporting",
  "we evaluated",
  "we examine clinical-context",
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
  "source-traceable",
  "hypothesis-generating",
  "marker-rule-derived",
  "proxy",
  "validation"
)

audit_text_blocks <- data.frame(
  text_block_id = c(
    paste0("CL", 1:nrow(cover_letter_blocks)),
    paste0("ABS_", abstract_positioning$abstract_section),
    paste0("STRATEGY_", seq_len(nrow(submission_strategy)))
  ),
  text_block_type = c(
    rep("cover_letter", nrow(cover_letter_blocks)),
    rep("abstract_positioning", nrow(abstract_positioning)),
    rep("submission_strategy", nrow(submission_strategy))
  ),
  text_value = c(
    cover_letter_blocks$text,
    abstract_positioning$recommended_message,
    submission_strategy$recommendation
  ),
  stringsAsFactors = FALSE
)

audit_list <- list()
for (idx_row in seq_len(nrow(audit_text_blocks))) {
  text_lower <- tolower(clean_space(audit_text_blocks$text_value[idx_row]))
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
  if (audit_text_blocks$text_block_type[idx_row] %in% c("cover_letter", "abstract_positioning") && length(boundary_hits) < 1) {
    status_now <- "needs_boundary_language_review"
  }
  audit_list[[length(audit_list) + 1]] <- data.frame(
    text_block_id = audit_text_blocks$text_block_id[idx_row],
    text_block_type = audit_text_blocks$text_block_type[idx_row],
    prohibited_positive_phrases_detected = paste(positive_hits, collapse = ";"),
    protective_boundary_concepts_detected = paste(boundary_hits, collapse = ";"),
    journal_cover_letter_claim_boundary_status = status_now,
    stringsAsFactors = FALSE
  )
}
claim_audit_12l <- safe_bind_rows(audit_list)
write_csv_safe(claim_audit_12l, file.path(out_table_dir, "12L_FINAL_journal_cover_letter_claim_boundary_audit.csv"))

handoff_12m <- data.frame(
  submission_package_item = c(
    "Title options",
    "Abstract positioning",
    "Cover letter draft",
    "Journal positioning table",
    "Submission strategy table",
    "Editor claim-boundary checklist",
    "Repository package",
    "Data/code availability material",
    "Results/Discussion/Methods text",
    "Manual journal website verification"
  ),
  source_file = c(
    file.path(out_table_dir, "12L_FINAL_title_options.csv"),
    file.path(out_table_dir, "12L_FINAL_abstract_positioning_table.csv"),
    file.path(out_text_dir, "12L_FINAL_cover_letter_draft.txt"),
    file.path(out_table_dir, "12L_FINAL_journal_positioning_table.csv"),
    file.path(out_table_dir, "12L_FINAL_submission_strategy_table.csv"),
    file.path(out_table_dir, "12L_FINAL_editor_claim_boundary_checklist.csv"),
    repo_dir_12k,
    file_12j_code_avail,
    paste(c(file_12h_results_text, file_12i_discussion_text, file_12j_methods_text), collapse = ";"),
    "manual external check required before real submission"
  ),
  use_in_12M = c(
    "final title selection",
    "abstract drafting",
    "cover letter assembly",
    "journal targeting",
    "submission order and fallback plan",
    "editor/reviewer risk control",
    "supplement/repository link preparation",
    "data availability and code availability statement",
    "main manuscript assembly",
    "final pre-submission checklist"
  ),
  ready = c(
    file.exists(file.path(out_table_dir, "12L_FINAL_title_options.csv")),
    file.exists(file.path(out_table_dir, "12L_FINAL_abstract_positioning_table.csv")),
    file.exists(file.path(out_text_dir, "12L_FINAL_cover_letter_draft.txt")),
    file.exists(file.path(out_table_dir, "12L_FINAL_journal_positioning_table.csv")),
    file.exists(file.path(out_table_dir, "12L_FINAL_submission_strategy_table.csv")),
    file.exists(file.path(out_table_dir, "12L_FINAL_editor_claim_boundary_checklist.csv")),
    dir.exists(repo_dir_12k),
    file_12j_code_avail != "",
    file_12h_results_text != "" && file_12i_discussion_text != "" && file_12j_methods_text != "",
    TRUE
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(handoff_12m, file.path(out_table_dir, "12L_FINAL_handoff_to_12M_submission_package.csv"))

fig_a <- open_pdf_safe("12L_FINAL_FigA_journal_cover_letter_package_overview.pdf", 11.8, 6.6)
new_canvas()
draw_title("Journal / cover-letter package overview", "12L converts the repository package into journal-positioning and submission-planning materials.")

overview_df <- data.frame(
  label = c(
    "Journal-positioning rows",
    "Title options",
    "Abstract sections",
    "Cover-letter blocks",
    "Editor checklist rows",
    "12M handoff rows",
    "Claim-boundary pass blocks"
  ),
  value = c(
    nrow(journal_positioning),
    nrow(title_options),
    nrow(abstract_positioning),
    nrow(cover_letter_blocks),
    nrow(editor_claim_checklist),
    nrow(handoff_12m),
    sum(claim_audit_12l$journal_cover_letter_claim_boundary_status == "claim_boundary_pass")
  ),
  family = c("journal", "title", "abstract", "letter", "check", "handoff", "pass"),
  stringsAsFactors = FALSE
)
max_value <- max(safe_num(overview_df$value), na.rm = TRUE)
if (!is.finite(max_value) || max_value <= 0) max_value <- 1
bar_x0 <- 0.43
bar_x1 <- 0.80
y_positions <- seq(0.82, 0.28, length.out = nrow(overview_df))
for (idx_row in seq_len(nrow(overview_df))) {
  yy <- y_positions[idx_row]
  count_now <- safe_num(overview_df$value[idx_row])
  width_now <- count_now / max_value
  if (count_now == 0) width_now <- 0.018
  color_now <- nature_palette$navy
  if (overview_df$family[idx_row] == "journal") color_now <- nature_palette$blue
  if (overview_df$family[idx_row] == "title") color_now <- nature_palette$teal
  if (overview_df$family[idx_row] == "abstract") color_now <- nature_palette$purple
  if (overview_df$family[idx_row] == "letter") color_now <- nature_palette$orange
  if (overview_df$family[idx_row] == "check") color_now <- nature_palette$gold
  if (overview_df$family[idx_row] == "handoff") color_now <- nature_palette$navy
  if (overview_df$family[idx_row] == "pass") color_now <- nature_palette$teal
  text(bar_x0 - 0.018, yy, overview_df$label[idx_row], cex = 0.52, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.021, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.021,
       col = color_now, border = nature_palette$border, lwd = 0.42)
  text(bar_x0 + width_now * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_now), cex = 0.48, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.14, "Next: 12M should assemble final submission-package materials.", cex = 0.42, col = nature_palette$muted)
dev.off()
cat("[12L FINAL] Wrote figure:", fig_a, "\n")

fig_b <- open_pdf_safe("12L_FINAL_FigB_journal_positioning_tier_map.pdf", 12.0, 6.8)
new_canvas()
draw_title("Journal positioning tier map", "Offline journal-positioning logic; real submission requires manual journal website verification.")

y_positions <- seq(0.76, 0.34, length.out = nrow(journal_positioning))
for (idx_row in seq_len(nrow(journal_positioning))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$blue
  if (journal_positioning$tier[idx_row] == "Best fit") color_now <- nature_palette$teal
  if (journal_positioning$tier[idx_row] == "Strong fit") color_now <- nature_palette$blue
  if (journal_positioning$tier[idx_row] == "Backup fit") color_now <- nature_palette$purple
  if (journal_positioning$tier[idx_row] == "Avoid") color_now <- nature_palette$red
  rect(0.07, yy - 0.030, 0.20, yy + 0.030, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.135, yy, journal_positioning$tier[idx_row], cex = 0.33, font = 2, col = nature_palette$white)
  text(0.23, yy + 0.014, journal_positioning$journal_type[idx_row], cex = 0.33, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.23, yy - 0.015, substr(journal_positioning$positioning_language[idx_row], 1, 94), cex = 0.28, adj = c(0, 0.5), col = nature_palette$muted)
}
text(0.50, 0.16, "This figure intentionally avoids current impact factor/APC claims because the module is offline.", cex = 0.38, col = nature_palette$muted)
dev.off()
cat("[12L FINAL] Wrote figure:", fig_b, "\n")

fig_c <- open_pdf_safe("12L_FINAL_FigC_title_abstract_positioning_map.pdf", 12.0, 7.0)
new_canvas()
draw_title("Title and abstract positioning map", "Title candidates and abstract messages keep the project within conservative transcriptomic framework scope.")

text(0.08, 0.82, "Top title candidates", cex = 0.46, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
for (idx_row in seq_len(min(3, nrow(title_options)))) {
  yy <- 0.75 - (idx_row - 1) * 0.085
  rect(0.08, yy - 0.026, 0.15, yy + 0.026, col = nature_palette$teal, border = nature_palette$border, lwd = 0.35)
  text(0.115, yy, paste0("T", idx_row), cex = 0.32, font = 2, col = nature_palette$white)
  text(0.17, yy, substr(title_options$title_option[idx_row], 1, 98), cex = 0.30, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.08, 0.44, "Abstract positioning", cex = 0.46, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
y_abs <- seq(0.37, 0.13, length.out = nrow(abstract_positioning))
for (idx_row in seq_len(nrow(abstract_positioning))) {
  yy <- y_abs[idx_row]
  rect(0.08, yy - 0.019, 0.22, yy + 0.019, col = nature_palette$blue, border = nature_palette$border, lwd = 0.35)
  text(0.15, yy, abstract_positioning$abstract_section[idx_row], cex = 0.27, font = 2, col = nature_palette$white)
  text(0.25, yy, substr(abstract_positioning$recommended_message[idx_row], 1, 88), cex = 0.25, adj = c(0, 0.5), col = nature_palette$ink)
}
dev.off()
cat("[12L FINAL] Wrote figure:", fig_c, "\n")

fig_d <- open_pdf_safe("12L_FINAL_FigD_cover_letter_claim_boundary_audit.pdf", 12.0, 6.8)
new_canvas()
draw_title("Cover-letter and journal-positioning claim-boundary audit", "Text blocks are checked before final submission-package assembly.")

audit_summary <- data.frame(
  block_type = unique(claim_audit_12l$text_block_type),
  total = 0,
  pass = 0,
  review = 0,
  stringsAsFactors = FALSE
)
for (idx_row in seq_len(nrow(audit_summary))) {
  type_now <- audit_summary$block_type[idx_row]
  keep_now <- claim_audit_12l$text_block_type == type_now
  audit_summary$total[idx_row] <- sum(keep_now)
  audit_summary$pass[idx_row] <- sum(claim_audit_12l$journal_cover_letter_claim_boundary_status[keep_now] == "claim_boundary_pass")
  audit_summary$review[idx_row] <- sum(claim_audit_12l$journal_cover_letter_claim_boundary_status[keep_now] != "claim_boundary_pass")
}
max_total <- max(audit_summary$total, na.rm = TRUE)
if (!is.finite(max_total) || max_total <= 0) max_total <- 1
y_positions <- seq(0.72, 0.40, length.out = nrow(audit_summary))
for (idx_row in seq_len(nrow(audit_summary))) {
  yy <- y_positions[idx_row]
  text(0.26, yy, audit_summary$block_type[idx_row], cex = 0.46, adj = c(1, 0.5), col = nature_palette$ink)
  rect(0.30, yy - 0.026, 0.30 + 0.40 * audit_summary$pass[idx_row] / max_total, yy + 0.026,
       col = nature_palette$teal, border = nature_palette$border, lwd = 0.35)
  if (audit_summary$review[idx_row] > 0) {
    rect(0.30 + 0.40 * audit_summary$pass[idx_row] / max_total, yy - 0.026,
         0.30 + 0.40 * audit_summary$total[idx_row] / max_total, yy + 0.026,
         col = nature_palette$red, border = nature_palette$border, lwd = 0.35)
  }
  text(0.73, yy, paste0("pass ", audit_summary$pass[idx_row], "/", audit_summary$total[idx_row]),
       cex = 0.42, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.18, paste0("Blocks needing repair: ", sum(claim_audit_12l$journal_cover_letter_claim_boundary_status != "claim_boundary_pass")),
     cex = 0.42, col = ifelse(sum(claim_audit_12l$journal_cover_letter_claim_boundary_status != "claim_boundary_pass") == 0, nature_palette$teal, nature_palette$red))
dev.off()
cat("[12L FINAL] Wrote figure:", fig_d, "\n")

fig_e <- open_pdf_safe("12L_FINAL_FigE_12M_submission_package_handoff.pdf", 12.0, 7.2)
new_canvas()
draw_title("12M submission-package handoff", "12M should assemble final submission materials from 12L outputs and locked repository package.")

y_positions <- seq(0.84, 0.16, length.out = nrow(handoff_12m))
for (idx_row in seq_len(nrow(handoff_12m))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$blue
  if (idx_row %% 3 == 1) color_now <- nature_palette$blue
  if (idx_row %% 3 == 2) color_now <- nature_palette$teal
  if (idx_row %% 3 == 0) color_now <- nature_palette$purple
  rect(0.06, yy - 0.020, 0.31, yy + 0.020, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.185, yy, handoff_12m$submission_package_item[idx_row], cex = 0.25, font = 2, col = nature_palette$white)
  text(0.34, yy + 0.008, substr(handoff_12m$use_in_12M[idx_row], 1, 88), cex = 0.25, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.34, yy - 0.012, ifelse(handoff_12m$ready[idx_row], "ready", "review"), cex = 0.23, adj = c(0, 0.5), col = ifelse(handoff_12m$ready[idx_row], nature_palette$teal, nature_palette$red))
}
dev.off()
cat("[12L FINAL] Wrote figure:", fig_e, "\n")

n_journal_rows <- nrow(journal_positioning)
n_title_options <- nrow(title_options)
n_abstract_sections <- nrow(abstract_positioning)
n_cover_blocks <- nrow(cover_letter_blocks)
n_editor_checks <- nrow(editor_claim_checklist)
n_claim_pass <- sum(claim_audit_12l$journal_cover_letter_claim_boundary_status == "claim_boundary_pass")
n_claim_total <- nrow(claim_audit_12l)
n_claim_repair <- sum(claim_audit_12l$journal_cover_letter_claim_boundary_status != "claim_boundary_pass")
n_handoff_ready <- sum(handoff_12m$ready)
n_handoff_total <- nrow(handoff_12m)
n_text_files <- sum(file.exists(c(
  file.path(out_text_dir, "12L_FINAL_cover_letter_draft.txt"),
  file.path(out_text_dir, "12L_FINAL_cover_letter_draft.md"),
  file.path(out_text_dir, "12L_FINAL_title_options.txt"),
  file.path(out_text_dir, "12L_FINAL_abstract_positioning_draft.txt"),
  file.path(out_text_dir, "12L_FINAL_submission_strategy_note.txt")
)))

decision_value <- "INPUT_READY_FOR_12M_SUBMISSION_PACKAGE_REFRESH"
if (n_claim_repair > 0) decision_value <- "REPAIR_REQUIRED_BEFORE_12M"
if (n_handoff_ready < n_handoff_total) decision_value <- "REVIEW_REQUIRED_BEFORE_12M"
if (n_journal_rows < 4 || n_title_options < 4 || n_cover_blocks < 6) decision_value <- "REVIEW_REQUIRED_BEFORE_12M"

summary_df <- data.frame(
  item = c(
    "journal_positioning_rows",
    "title_options",
    "abstract_sections",
    "cover_letter_blocks",
    "editor_claim_boundary_checklist_rows",
    "claim_boundary_pass_blocks",
    "claim_boundary_total_blocks",
    "claim_boundary_repair_needed",
    "12M_handoff_ready_rows",
    "12M_handoff_total_rows",
    "text_files_written",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(n_journal_rows),
    as.character(n_title_options),
    as.character(n_abstract_sections),
    as.character(n_cover_blocks),
    as.character(n_editor_checks),
    as.character(n_claim_pass),
    as.character(n_claim_total),
    as.character(n_claim_repair),
    as.character(n_handoff_ready),
    as.character(n_handoff_total),
    as.character(n_text_files),
    "5",
    decision_value
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "12L_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "12L_FINAL_execution_summary.tsv"))

report_lines <- c(
  "12L FINAL report",
  "================",
  "Module: Journal / cover-letter refresh",
  "Mode: complete standalone 12L V2 rebuild; no previous 12L output dependency; no internet; no 00-10P rerun. V2 repair: Abstract limitations block now contains explicit validation boundary language.",
  "Allowed upstream inputs: locked 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F, 12G, 12H, 12I, 12J and 12K outputs.",
  "",
  paste0("Journal-positioning rows: ", n_journal_rows),
  paste0("Title options: ", n_title_options),
  paste0("Abstract sections: ", n_abstract_sections),
  paste0("Cover-letter blocks: ", n_cover_blocks),
  paste0("Editor claim-boundary checklist rows: ", n_editor_checks),
  paste0("Claim-boundary pass blocks: ", n_claim_pass, "/", n_claim_total),
  paste0("Claim-boundary repair needed: ", n_claim_repair),
  paste0("12M handoff ready rows: ", n_handoff_ready, "/", n_handoff_total),
  paste0("Text files written: ", n_text_files),
  "",
  "Main outputs:",
  paste0("- ", file.path(out_text_dir, "12L_FINAL_cover_letter_draft.txt")),
  paste0("- ", file.path(out_text_dir, "12L_FINAL_cover_letter_draft.md")),
  paste0("- ", file.path(out_text_dir, "12L_FINAL_title_options.txt")),
  paste0("- ", file.path(out_text_dir, "12L_FINAL_abstract_positioning_draft.txt")),
  paste0("- ", file.path(out_text_dir, "12L_FINAL_submission_strategy_note.txt")),
  paste0("- ", file.path(out_table_dir, "12L_FINAL_journal_positioning_table.csv")),
  paste0("- ", file.path(out_table_dir, "12L_FINAL_handoff_to_12M_submission_package.csv")),
  "",
  "Claim boundary:",
  "- Journal targeting avoids live claims about current impact factors, APCs or policies.",
  "- Real journal scope and formatting must be manually checked before submission.",
  "- The manuscript remains a computational transcriptomic prioritisation framework.",
  "- Candidate signatures remain candidate transcriptomic marker signatures.",
  "- ML remains marker-rule-derived prioritization model audit.",
  "",
  paste0("Decision: ", decision_value)
)
report_file <- file.path(out_text_dir, "12L_FINAL_journal_cover_letter_refresh_report.txt")
writeLines(report_lines, report_file)
cat("[12L FINAL] Wrote:", report_file, "\n")

cat("\n[12L FINAL] Completed Journal / cover-letter refresh.\n")
cat("[12L FINAL] Journal-positioning rows:", n_journal_rows, "\n")
cat("[12L FINAL] Title options:", n_title_options, "\n")
cat("[12L FINAL] Abstract sections:", n_abstract_sections, "\n")
cat("[12L FINAL] Cover-letter blocks:", n_cover_blocks, "\n")
cat("[12L FINAL] Editor claim-boundary checklist rows:", n_editor_checks, "\n")
cat("[12L FINAL] Claim-boundary pass blocks:", n_claim_pass, "/", n_claim_total, "\n")
cat("[12L FINAL] Claim-boundary repair needed:", n_claim_repair, "\n")
cat("[12L FINAL] 12M handoff ready rows:", n_handoff_ready, "/", n_handoff_total, "\n")
cat("[12L FINAL] Text files written:", n_text_files, "\n")
cat("[12L FINAL] Figures written: 5\n")
cat("[12L FINAL] Decision:", decision_value, "\n")
cat("[12L FINAL] Output tables:", out_table_dir, "\n")
cat("[12L FINAL] Output figs  :", out_fig_dir, "\n")
cat("[12L FINAL] Output text  :", out_text_dir, "\n")
cat("[12L FINAL] Next         : review 12L journal/cover-letter outputs and PDFs; if accepted, proceed to 12M submission package refresh.\n")
