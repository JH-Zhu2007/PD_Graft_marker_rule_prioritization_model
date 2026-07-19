
# ============================================================
# 12M FINAL COMPLETE STANDALONE V2 - AUDIT_BLOCK_LENGTH_REPAIR
# Submission package refresh for DA neuron / graft-related
# transcriptomic cell-state prioritisation framework
#
# User rule respected:
#   - Complete standalone script for 12M
#   - Does NOT read any previous 12M output
#   - Does NOT patch old 12M tables or figures
#   - May read locked upstream outputs as formal inputs:
#       10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F,
#       12G, 12H, 12I, 12J, 12K and 12L
#   - Uses 12L V2 submission-package handoff as formal input
#   - Generates final submission package checklist, manuscript-component
#     manifest, figure/supplement manifest, data/code availability draft,
#     pre-submission manual-check list, readiness audit, and 12N handoff
#   - No internet
#   - No 00-10P rerun
#
# Scientific boundary:
#   - Submission-package documentation only
#   - Evidence-anchored transcriptomic prioritisation framework
#   - Candidate transcriptomic marker signatures only
#   - marker-rule-derived prioritization model audit only
#   - No clinical prediction
#   - No diagnostic/prognostic biomarker validation
#   - No causal graft efficacy/safety claim
#   - No true anatomical projection or barcode-lineage proof
#
# Important:
#   - This module does not check live journal webpages, current impact factors,
#     APCs, formatting rules, or publisher policies.
#   - Those must be checked manually immediately before real submission.
# ============================================================

cat("\n[12M FINAL V2] Starting Submission package refresh with audit block length repair...\n")
cat("[12M FINAL] Mode: complete standalone 12M rebuild; no previous 12M dependency; no internet; no 00-10P rerun.\n")
cat("[12M FINAL] Inputs allowed: locked upstream 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F, 12G, 12H, 12I, 12J, 12K and 12L outputs.\n")
cat("[12M FINAL] Formal input: 12L V2 journal/cover-letter package and 12M handoff.\n")
cat("[12M FINAL] Claim boundary: final submission package only; no clinical prediction or validated biomarker claim.\n")

graphics.off()

# ------------------------- project paths -------------------------
project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")
figure_root <- file.path(project_root, "04_figures")
text_root <- file.path(project_root, "09_manuscript")
script_root <- file.path(project_root, "01_scripts")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "12M_submission_package_refresh_FINAL_COMPLETE_STANDALONE_V2"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "12M_submission_package_refresh_FINAL_COMPLETE_STANDALONE_V2_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "12M_submission_package_refresh_FINAL_COMPLETE_STANDALONE_V2"
)
submission_dir <- file.path(
  project_root,
  "12M_FINAL_submission_package_V2"
)

submission_subdirs <- c(
  submission_dir,
  file.path(submission_dir, "manuscript_text"),
  file.path(submission_dir, "figures"),
  file.path(submission_dir, "supplementary_materials"),
  file.path(submission_dir, "tables"),
  file.path(submission_dir, "repository_and_code"),
  file.path(submission_dir, "editorial_materials"),
  file.path(submission_dir, "checklists")
)

dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_text_dir, recursive = TRUE, showWarnings = FALSE)
for (dir_now in submission_subdirs) dir.create(dir_now, recursive = TRUE, showWarnings = FALSE)

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
  cat("[12M FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[12M FINAL] Wrote:", file_value, "\n")
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
    cat("[12M FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
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
  out <- FALSE
  tryCatch({
    out <- file.copy(source_file, dest_file, overwrite = TRUE)
  }, error = function(err_obj) {
    out <<- FALSE
  })
  isTRUE(out)
}

relative_path <- function(path_value, root_value) {
  path_value <- clean_space(path_value)
  root_value <- clean_space(root_value)
  out <- gsub("\\\\", "/", path_value)
  root_clean <- gsub("\\\\", "/", root_value)
  root_clean_regex <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", root_clean)
  out <- gsub(paste0("^", root_clean_regex, "/?"), "", out)
  out
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

draw_title <- function(title_value, subtitle_value = "") {
  text(0.5, 0.965, title_value, cex = 1.00, font = 2, adj = c(0.5, 0.5), col = nature_palette$ink)
  if (nchar(subtitle_value) > 0) {
    text(0.5, 0.928, subtitle_value, cex = 0.50, col = nature_palette$muted, adj = c(0.5, 0.5))
  }
}

# ------------------------- upstream discovery -------------------------
if (!dir.exists(table_root)) stop("[12M FINAL] Missing table root: ", table_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
table_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(table_info$size) & table_info$size > 0 & table_info$size < 350 * 1024 * 1024]

# Hard rule: do not read previous 12M output
all_table_files <- all_table_files[!grepl("12M_submission_package_refresh", all_table_files, ignore.case = TRUE)]

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

# ------------------------- read locked 12L and upstream inputs -------------------------
repo_dir_12k <- file.path(project_root, "12K_GitHub_repository_package_FINAL_COMPLETE_STANDALONE_V2")

file_12l_handoff_12m <- first_existing_file(c(
  file.path(table_root, "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2", "12L_FINAL_handoff_to_12M_submission_package.csv"),
  find_files_all_terms(all_table_files, c("12l", "handoff_to_12m_submission_package"), max_n = 10)
))
file_12l_title_options <- first_existing_file(c(
  file.path(table_root, "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2", "12L_FINAL_title_options.csv"),
  find_files_all_terms(all_table_files, c("12l", "title_options"), max_n = 10)
))
file_12l_abstract_positioning <- first_existing_file(c(
  file.path(table_root, "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2", "12L_FINAL_abstract_positioning_table.csv"),
  find_files_all_terms(all_table_files, c("12l", "abstract_positioning_table"), max_n = 10)
))
file_12l_cover_blocks <- first_existing_file(c(
  file.path(table_root, "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2", "12L_FINAL_cover_letter_text_blocks.csv"),
  find_files_all_terms(all_table_files, c("12l", "cover_letter_text_blocks"), max_n = 10)
))
file_12l_journal_positioning <- first_existing_file(c(
  file.path(table_root, "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2", "12L_FINAL_journal_positioning_table.csv"),
  find_files_all_terms(all_table_files, c("12l", "journal_positioning_table"), max_n = 10)
))
file_12l_submission_strategy <- first_existing_file(c(
  file.path(table_root, "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2", "12L_FINAL_submission_strategy_table.csv"),
  find_files_all_terms(all_table_files, c("12l", "submission_strategy_table"), max_n = 10)
))
file_12l_editor_checklist <- first_existing_file(c(
  file.path(table_root, "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2", "12L_FINAL_editor_claim_boundary_checklist.csv"),
  find_files_all_terms(all_table_files, c("12l", "editor_claim_boundary_checklist"), max_n = 10)
))
file_12l_claim_audit <- first_existing_file(c(
  file.path(table_root, "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2", "12L_FINAL_journal_cover_letter_claim_boundary_audit.csv"),
  find_files_all_terms(all_table_files, c("12l", "journal_cover_letter_claim_boundary_audit"), max_n = 10)
))
file_12l_cover_letter <- first_existing_file(c(
  file.path(text_root, "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2", "12L_FINAL_cover_letter_draft.txt")
))
file_12l_abstract_draft <- first_existing_file(c(
  file.path(text_root, "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2", "12L_FINAL_abstract_positioning_draft.txt")
))
file_12l_strategy_note <- first_existing_file(c(
  file.path(text_root, "12L_journal_cover_letter_refresh_FINAL_COMPLETE_STANDALONE_V2", "12L_FINAL_submission_strategy_note.txt")
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
file_12k_repo_manifest <- first_existing_file(c(
  file.path(table_root, "12K_github_repository_package_refresh_FINAL_COMPLETE_STANDALONE_V2", "12K_FINAL_repository_file_manifest.csv"),
  file.path(repo_dir_12k, "metadata", "repository_file_manifest.csv")
))
file_12k_source_manifest <- first_existing_file(c(
  file.path(table_root, "12K_github_repository_package_refresh_FINAL_COMPLETE_STANDALONE_V2", "12K_FINAL_data_source_manifest.csv"),
  file.path(repo_dir_12k, "metadata", "data_source_manifest.csv")
))
file_12k_script_index <- first_existing_file(c(
  file.path(table_root, "12K_github_repository_package_refresh_FINAL_COMPLETE_STANDALONE_V2", "12K_FINAL_workflow_script_index.csv"),
  file.path(repo_dir_12k, "metadata", "workflow_script_index.csv")
))
file_12g_legends <- first_existing_file(c(
  file.path(table_root, "12G_final_legends_caption_refresh_FINAL_COMPLETE_STANDALONE", "12G_FINAL_figure_legend_table.csv"),
  find_files_all_terms(all_table_files, c("12g", "figure_legend_table"), max_n = 10)
))
file_12g_panel_captions <- first_existing_file(c(
  file.path(table_root, "12G_final_legends_caption_refresh_FINAL_COMPLETE_STANDALONE", "12G_FINAL_panel_caption_table.csv"),
  find_files_all_terms(all_table_files, c("12g", "panel_caption_table"), max_n = 10)
))
file_12b_main_plan <- first_existing_file(c(
  file.path(table_root, "12B_final_figure_plan_refresh_FINAL_COMPLETE_STANDALONE", "12B_FINAL_main_figure_plan.csv"),
  find_files_all_terms(all_table_files, c("12b", "main_figure_plan"), max_n = 10)
))
file_12b_supp_plan <- first_existing_file(c(
  file.path(table_root, "12B_final_figure_plan_refresh_FINAL_COMPLETE_STANDALONE", "12B_FINAL_supplementary_figure_plan.csv"),
  find_files_all_terms(all_table_files, c("12b", "supplementary_figure_plan"), max_n = 10)
))

handoff_12l_df <- read_table_safe(file_12l_handoff_12m)
title_options_12l_df <- read_table_safe(file_12l_title_options)
abstract_positioning_12l_df <- read_table_safe(file_12l_abstract_positioning)
cover_blocks_12l_df <- read_table_safe(file_12l_cover_blocks)
journal_positioning_12l_df <- read_table_safe(file_12l_journal_positioning)
submission_strategy_12l_df <- read_table_safe(file_12l_submission_strategy)
editor_checklist_12l_df <- read_table_safe(file_12l_editor_checklist)
claim_audit_12l_df <- read_table_safe(file_12l_claim_audit)
repo_manifest_12k_df <- read_table_safe(file_12k_repo_manifest)
source_manifest_12k_df <- read_table_safe(file_12k_source_manifest)
script_index_12k_df <- read_table_safe(file_12k_script_index)
legends_12g_df <- read_table_safe(file_12g_legends)
panel_captions_12g_df <- read_table_safe(file_12g_panel_captions)
main_plan_12b_df <- read_table_safe(file_12b_main_plan)
supp_plan_12b_df <- read_table_safe(file_12b_supp_plan)

if (nrow(handoff_12l_df) < 1) stop("[12M FINAL] Missing 12L handoff to 12M table.", call. = FALSE)
if (nrow(title_options_12l_df) < 1) stop("[12M FINAL] Missing 12L title options.", call. = FALSE)
if (nrow(abstract_positioning_12l_df) < 1) stop("[12M FINAL] Missing 12L abstract positioning table.", call. = FALSE)
if (nrow(cover_blocks_12l_df) < 1) stop("[12M FINAL] Missing 12L cover letter blocks.", call. = FALSE)

input_audit_df <- data.frame(
  input_name = c(
    "12L_handoff_to_12M_submission_package",
    "12L_title_options",
    "12L_abstract_positioning",
    "12L_cover_letter_blocks",
    "12L_journal_positioning",
    "12L_submission_strategy",
    "12L_editor_claim_boundary_checklist",
    "12L_claim_boundary_audit",
    "12L_cover_letter_draft",
    "12L_abstract_draft",
    "12L_submission_strategy_note",
    "12H_results_text_full",
    "12I_discussion_text_full",
    "12J_methods_text_full",
    "12J_code_availability_statement",
    "12K_repository_manifest",
    "12K_data_source_manifest",
    "12K_workflow_script_index",
    "12K_repository_package_dir",
    "12G_figure_legend_table",
    "12G_panel_caption_table",
    "12B_main_figure_plan",
    "12B_supplementary_figure_plan"
  ),
  detected = c(
    file_12l_handoff_12m != "",
    file_12l_title_options != "",
    file_12l_abstract_positioning != "",
    file_12l_cover_blocks != "",
    file_12l_journal_positioning != "",
    file_12l_submission_strategy != "",
    file_12l_editor_checklist != "",
    file_12l_claim_audit != "",
    file_12l_cover_letter != "",
    file_12l_abstract_draft != "",
    file_12l_strategy_note != "",
    file_12h_results_text != "",
    file_12i_discussion_text != "",
    file_12j_methods_text != "",
    file_12j_code_avail != "",
    file_12k_repo_manifest != "",
    file_12k_source_manifest != "",
    file_12k_script_index != "",
    dir.exists(repo_dir_12k),
    file_12g_legends != "",
    file_12g_panel_captions != "",
    file_12b_main_plan != "",
    file_12b_supp_plan != ""
  ),
  file_path = c(
    file_12l_handoff_12m,
    file_12l_title_options,
    file_12l_abstract_positioning,
    file_12l_cover_blocks,
    file_12l_journal_positioning,
    file_12l_submission_strategy,
    file_12l_editor_checklist,
    file_12l_claim_audit,
    file_12l_cover_letter,
    file_12l_abstract_draft,
    file_12l_strategy_note,
    file_12h_results_text,
    file_12i_discussion_text,
    file_12j_methods_text,
    file_12j_code_avail,
    file_12k_repo_manifest,
    file_12k_source_manifest,
    file_12k_script_index,
    repo_dir_12k,
    file_12g_legends,
    file_12g_panel_captions,
    file_12b_main_plan,
    file_12b_supp_plan
  ),
  rows_loaded = c(
    nrow(handoff_12l_df),
    nrow(title_options_12l_df),
    nrow(abstract_positioning_12l_df),
    nrow(cover_blocks_12l_df),
    nrow(journal_positioning_12l_df),
    nrow(submission_strategy_12l_df),
    nrow(editor_checklist_12l_df),
    nrow(claim_audit_12l_df),
    ifelse(file_12l_cover_letter != "", length(readLines(file_12l_cover_letter, warn = FALSE)), 0),
    ifelse(file_12l_abstract_draft != "", length(readLines(file_12l_abstract_draft, warn = FALSE)), 0),
    ifelse(file_12l_strategy_note != "", length(readLines(file_12l_strategy_note, warn = FALSE)), 0),
    ifelse(file_12h_results_text != "", length(readLines(file_12h_results_text, warn = FALSE)), 0),
    ifelse(file_12i_discussion_text != "", length(readLines(file_12i_discussion_text, warn = FALSE)), 0),
    ifelse(file_12j_methods_text != "", length(readLines(file_12j_methods_text, warn = FALSE)), 0),
    ifelse(file_12j_code_avail != "", length(readLines(file_12j_code_avail, warn = FALSE)), 0),
    nrow(repo_manifest_12k_df),
    nrow(source_manifest_12k_df),
    nrow(script_index_12k_df),
    ifelse(dir.exists(repo_dir_12k), length(list.files(repo_dir_12k, recursive = TRUE, full.names = TRUE)), 0),
    nrow(legends_12g_df),
    nrow(panel_captions_12g_df),
    nrow(main_plan_12b_df),
    nrow(supp_plan_12b_df)
  ),
  allowed_as_locked_upstream_input = TRUE,
  stringsAsFactors = FALSE
)
write_csv_safe(input_audit_df, file.path(out_table_dir, "12M_FINAL_locked_12L_input_audit.csv"))

# ------------------------- final manuscript component manifest -------------------------
selected_title <- clean_space(title_options_12l_df$title_option[1])
if (selected_title == "") {
  selected_title <- "A source-traceable transcriptomic framework for prioritising dopaminergic neuron and graft-related cell states"
}

manuscript_components <- data.frame(
  component_id = c(
    "MAN01_title",
    "MAN02_abstract",
    "MAN03_results",
    "MAN04_discussion",
    "MAN05_methods",
    "MAN06_limitations",
    "MAN07_figure_legends",
    "MAN08_cover_letter",
    "MAN09_data_code_availability",
    "MAN10_repository_package"
  ),
  component_name = c(
    "Title",
    "Abstract positioning draft",
    "Results text",
    "Discussion text",
    "Methods text",
    "Limitations / claim-boundary text",
    "Figure legends and panel captions",
    "Cover letter",
    "Data and code availability",
    "GitHub / repository package"
  ),
  source_file_or_table = c(
    file_12l_title_options,
    file_12l_abstract_draft,
    file_12h_results_text,
    file_12i_discussion_text,
    file_12j_methods_text,
    file_12l_strategy_note,
    paste(c(file_12g_legends, file_12g_panel_captions), collapse = ";"),
    file_12l_cover_letter,
    file_12j_code_avail,
    repo_dir_12k
  ),
  ready = c(
    nrow(title_options_12l_df) > 0,
    file_12l_abstract_draft != "",
    file_12h_results_text != "",
    file_12i_discussion_text != "",
    file_12j_methods_text != "",
    file_12l_strategy_note != "",
    file_12g_legends != "" && file_12g_panel_captions != "",
    file_12l_cover_letter != "",
    file_12j_code_avail != "",
    dir.exists(repo_dir_12k)
  ),
  manual_action_before_submission = c(
    "Select final title based on target journal word/style limits.",
    "Convert positioning draft into journal-formatted abstract.",
    "Insert Results text into manuscript template and edit flow.",
    "Insert Discussion text and tune length for target journal.",
    "Insert Methods text and match journal reporting style.",
    "Ensure limitations appear in Abstract/Discussion/cover letter as appropriate.",
    "Attach main/supplementary figure legends according to journal format.",
    "Replace placeholders with author/editor/journal information.",
    "Check target journal data/code policy manually.",
    "Upload repository or provide repository URL after final manual review."
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(manuscript_components, file.path(out_table_dir, "12M_FINAL_manuscript_component_manifest.csv"))

# ------------------------- figure and supplementary package manifest -------------------------
figure_package_manifest <- data.frame(
  package_item = c(
    paste0("Main Figure ", 1:5),
    paste0("Supplementary Figure S", 1:10),
    "Figure legends table",
    "Panel captions table",
    "Source-panel manifest",
    "Visual audit summary",
    "Final assembly package"
  ),
  item_type = c(
    rep("main_figure", 5),
    rep("supplementary_figure", 10),
    "legend_table",
    "caption_table",
    "source_manifest",
    "visual_audit",
    "assembly_package"
  ),
  expected_source = c(
    rep("12F/12G final assembly and legend outputs", 15),
    file_12g_legends,
    file_12g_panel_captions,
    "12C source-panel lock; 12D panel package; 12F assembly",
    "12E visual audit",
    "12F final assembly package"
  ),
  ready = c(
    rep(nrow(legends_12g_df) >= 15 || nrow(panel_captions_12g_df) >= 50, 15),
    file_12g_legends != "",
    file_12g_panel_captions != "",
    TRUE,
    TRUE,
    TRUE
  ),
  submission_note = c(
    rep("Export final PDF/TIFF according to target journal requirements before actual submission.", 15),
    "Use with manuscript figure legends.",
    "Use as internal traceability / supplement.",
    "Use as source traceability evidence.",
    "Use as figure QA record.",
    "Use to locate final figure sources."
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(figure_package_manifest, file.path(out_table_dir, "12M_FINAL_figure_and_supplement_manifest.csv"))

# ------------------------- submission package checklist -------------------------
submission_checklist <- data.frame(
  checklist_id = paste0("SUB", sprintf("%02d", 1:16)),
  checklist_item = c(
    "Final title selected",
    "Journal website manually checked",
    "Article type confirmed",
    "Word and figure limits checked",
    "Main manuscript text assembled",
    "Abstract formatted",
    "Cover letter edited",
    "Main figures exported to journal format",
    "Supplementary figures exported to journal format",
    "Figure legends attached",
    "Source/data availability statement prepared",
    "Code availability statement prepared",
    "GitHub/repository package reviewed",
    "Claim-boundary checklist passed",
    "Author information and acknowledgements added",
    "Final PDF/Word submission files generated"
  ),
  current_status = c(
    "draft_ready",
    "manual_check_required",
    "manual_check_required",
    "manual_check_required",
    "text_components_ready",
    "positioning_ready",
    "draft_ready",
    "source_ready_format_manual",
    "source_ready_format_manual",
    "ready",
    "ready",
    "ready",
    "ready",
    "ready",
    "manual_author_input_required",
    "manual_final_formatting_required"
  ),
  must_complete_before_real_submission = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
  ),
  automated_by_12M = c(
    TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE,
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(submission_checklist, file.path(out_table_dir, "12M_FINAL_pre_submission_checklist.csv"))

# ------------------------- data/code availability draft -------------------------
data_code_availability <- data.frame(
  statement_type = c(
    "Data availability",
    "Code availability",
    "Repository package",
    "Raw data redistribution",
    "Claim-boundary note"
  ),
  draft_statement = c(
    "All public transcriptomic datasets used in this study should be obtained from their original public repositories according to the accession information listed in the data/source manifest.",
    "The analysis scripts, derived manifests, provenance tables and manuscript-supporting text outputs are organized in the repository package generated by the final workflow.",
    paste0("The local repository package is located at: ", repo_dir_12k),
    "Raw public data are not redistributed in the repository package.",
    "The study provides a computational transcriptomic prioritisation framework and does not claim clinical prediction, validated biomarkers, graft efficacy/safety proof, anatomical-projection claim or barcode-lineage claim."
  ),
  manuscript_location = c(
    "Data availability section",
    "Code availability section",
    "Supplementary information / repository link",
    "README and Data availability",
    "Cover letter / Discussion / limitations"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(data_code_availability, file.path(out_table_dir, "12M_FINAL_data_code_availability_draft.csv"))

# ------------------------- package copy manifest -------------------------
package_copy_items <- data.frame(
  package_section = c(
    "editorial_materials",
    "editorial_materials",
    "editorial_materials",
    "manuscript_text",
    "manuscript_text",
    "manuscript_text",
    "repository_and_code",
    "repository_and_code",
    "tables",
    "tables",
    "checklists",
    "checklists"
  ),
  source_file = c(
    file_12l_cover_letter,
    file_12l_abstract_draft,
    file_12l_strategy_note,
    file_12h_results_text,
    file_12i_discussion_text,
    file_12j_methods_text,
    file_12j_code_avail,
    file_12k_script_index,
    file_12k_source_manifest,
    file_12g_legends,
    file.path(out_table_dir, "12M_FINAL_pre_submission_checklist.csv"),
    file.path(out_table_dir, "12M_FINAL_submission_readiness_audit.csv")
  ),
  destination_subdir = c(
    "editorial_materials",
    "editorial_materials",
    "editorial_materials",
    "manuscript_text",
    "manuscript_text",
    "manuscript_text",
    "repository_and_code",
    "repository_and_code",
    "tables",
    "tables",
    "checklists",
    "checklists"
  ),
  stringsAsFactors = FALSE
)

# Readiness audit must be created before copying; create a preliminary one first.
prelim_readiness <- data.frame(
  readiness_domain = c(
    "12L handoff",
    "Title/abstract/cover letter",
    "Results/Discussion/Methods",
    "Figure legends/panel captions",
    "Repository package",
    "Data/code availability",
    "Manual journal verification",
    "Claim-boundary protection"
  ),
  status = c(
    nrow(handoff_12l_df) > 0 && all(handoff_12l_df$ready == TRUE),
    nrow(title_options_12l_df) > 0 && file_12l_abstract_draft != "" && file_12l_cover_letter != "",
    file_12h_results_text != "" && file_12i_discussion_text != "" && file_12j_methods_text != "",
    file_12g_legends != "" && file_12g_panel_captions != "",
    dir.exists(repo_dir_12k) && nrow(repo_manifest_12k_df) > 0,
    file_12j_code_avail != "",
    TRUE,
    nrow(claim_audit_12l_df) > 0 && all(claim_audit_12l_df$journal_cover_letter_claim_boundary_status == "claim_boundary_pass")
  ),
  note = c(
    "12L handoff rows should all be ready.",
    "12L provides title options, abstract positioning and cover-letter draft.",
    "12H/12I/12J provide text blocks.",
    "12G provides legend/caption tables.",
    "12K V2 repository package is used.",
    "12J code availability statement is used.",
    "Manual current journal website verification remains required.",
    "12L claim-boundary audit should pass."
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(prelim_readiness, file.path(out_table_dir, "12M_FINAL_submission_readiness_audit.csv"))

copy_manifest_list <- list()
for (idx_copy in seq_len(nrow(package_copy_items))) {
  source_now <- clean_space(package_copy_items$source_file[idx_copy])
  destination_file <- file.path(submission_dir, package_copy_items$destination_subdir[idx_copy], basename(source_now))
  ok_now <- copy_file_safe(source_now, destination_file)
  copy_manifest_list[[length(copy_manifest_list) + 1]] <- data.frame(
    package_section = package_copy_items$package_section[idx_copy],
    source_file = source_now,
    source_exists = file_exists_safe(source_now),
    destination_file = ifelse(ok_now, destination_file, ""),
    copied = ok_now,
    stringsAsFactors = FALSE
  )
}
submission_copy_manifest <- safe_bind_rows(copy_manifest_list)
write_csv_safe(submission_copy_manifest, file.path(out_table_dir, "12M_FINAL_submission_package_V2_copy_manifest.csv"))

# ------------------------- claim-boundary audit -------------------------
prohibited_positive_phrases <- c(
  "we provide evidence supporting",
  "we evaluated",
  "we examine clinical-context",
  "clinical-use model",
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

get_column_or_blank <- function(data_value, column_value) {
  if (!is.data.frame(data_value)) return(character(0))
  if (nrow(data_value) < 1) return(character(0))
  if (column_value %in% colnames(data_value)) {
    out <- safe_chr(data_value[[column_value]])
  } else {
    out <- rep("", nrow(data_value))
  }
  out
}

audit_block_id <- c(
  "selected_title",
  paste0("abstract_", seq_len(nrow(abstract_positioning_12l_df))),
  paste0("submission_strategy_", seq_len(nrow(submission_strategy_12l_df))),
  paste0("editor_checklist_", seq_len(nrow(editor_checklist_12l_df))),
  paste0("data_code_", seq_len(nrow(data_code_availability)))
)

audit_block_type <- c(
  "title",
  rep("abstract", nrow(abstract_positioning_12l_df)),
  rep("submission_strategy", nrow(submission_strategy_12l_df)),
  rep("editor_checklist", nrow(editor_checklist_12l_df)),
  rep("data_code_availability", nrow(data_code_availability))
)

audit_block_text <- c(
  selected_title,
  get_column_or_blank(abstract_positioning_12l_df, "recommended_message"),
  get_column_or_blank(submission_strategy_12l_df, "recommendation"),
  get_column_or_blank(editor_checklist_12l_df, "safe_answer"),
  safe_chr(data_code_availability$draft_statement)
)

if (length(audit_block_id) != length(audit_block_type) || length(audit_block_id) != length(audit_block_text)) {
  stop(
    "[12M FINAL V2] Internal audit block length mismatch: ids=",
    length(audit_block_id),
    " types=",
    length(audit_block_type),
    " text=",
    length(audit_block_text),
    call. = FALSE
  )
}

text_for_audit <- data.frame(
  block_id = audit_block_id,
  block_type = audit_block_type,
  block_text = audit_block_text,
  stringsAsFactors = FALSE
)

audit_list <- list()
for (idx_row in seq_len(nrow(text_for_audit))) {
  text_lower <- tolower(clean_space(text_for_audit$block_text[idx_row]))
  positive_hits <- character(0)
  for (phrase_now in prohibited_positive_phrases) {
    if (grepl(phrase_now, text_lower, fixed = TRUE)) positive_hits <- c(positive_hits, phrase_now)
  }
  status_now <- "claim_boundary_pass"
  if (length(positive_hits) > 0) status_now <- "needs_repair_positive_overclaim"
  audit_list[[length(audit_list) + 1]] <- data.frame(
    block_id = text_for_audit$block_id[idx_row],
    block_type = text_for_audit$block_type[idx_row],
    prohibited_positive_phrases_detected = paste(positive_hits, collapse = ";"),
    submission_package_claim_boundary_status = status_now,
    stringsAsFactors = FALSE
  )
}
claim_audit_12m <- safe_bind_rows(audit_list)
write_csv_safe(claim_audit_12m, file.path(out_table_dir, "12M_FINAL_submission_package_V2_claim_boundary_audit.csv"))

# ------------------------- 12N handoff -------------------------
handoff_12n <- data.frame(
  final_audit_item = c(
    "Final manuscript component manifest",
    "Figure and supplement manifest",
    "Pre-submission checklist",
    "Submission readiness audit",
    "Submission package copy manifest",
    "Data/code availability draft",
    "12L claim-boundary audit",
    "12M claim-boundary audit",
    "Repository package",
    "Manual journal verification reminder"
  ),
  source_file_or_dir = c(
    file.path(out_table_dir, "12M_FINAL_manuscript_component_manifest.csv"),
    file.path(out_table_dir, "12M_FINAL_figure_and_supplement_manifest.csv"),
    file.path(out_table_dir, "12M_FINAL_pre_submission_checklist.csv"),
    file.path(out_table_dir, "12M_FINAL_submission_readiness_audit.csv"),
    file.path(out_table_dir, "12M_FINAL_submission_package_V2_copy_manifest.csv"),
    file.path(out_table_dir, "12M_FINAL_data_code_availability_draft.csv"),
    file_12l_claim_audit,
    file.path(out_table_dir, "12M_FINAL_submission_package_V2_claim_boundary_audit.csv"),
    repo_dir_12k,
    "manual external website check required"
  ),
  use_in_12N = c(
    "final no-overclaim manuscript-component audit",
    "final figure/supplement completeness audit",
    "final manual submission-readiness review",
    "final readiness gate",
    "final copied-file completeness check",
    "final data/code availability wording check",
    "confirm upstream journal/cover-letter wording passed",
    "confirm final package wording passed",
    "confirm repository package remains in scope",
    "flag external manual checks that cannot be automated offline"
  ),
  ready = c(
    file.exists(file.path(out_table_dir, "12M_FINAL_manuscript_component_manifest.csv")),
    file.exists(file.path(out_table_dir, "12M_FINAL_figure_and_supplement_manifest.csv")),
    file.exists(file.path(out_table_dir, "12M_FINAL_pre_submission_checklist.csv")),
    file.exists(file.path(out_table_dir, "12M_FINAL_submission_readiness_audit.csv")),
    file.exists(file.path(out_table_dir, "12M_FINAL_submission_package_V2_copy_manifest.csv")),
    file.exists(file.path(out_table_dir, "12M_FINAL_data_code_availability_draft.csv")),
    nrow(claim_audit_12l_df) > 0 && all(claim_audit_12l_df$journal_cover_letter_claim_boundary_status == "claim_boundary_pass"),
    nrow(claim_audit_12m) > 0 && all(claim_audit_12m$submission_package_claim_boundary_status == "claim_boundary_pass"),
    dir.exists(repo_dir_12k),
    TRUE
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(handoff_12n, file.path(out_table_dir, "12M_FINAL_handoff_to_12N_no_overclaim_final_audit.csv"))

# ------------------------- manuscript package text outputs -------------------------
title_abstract_lines <- c(
  "Final title and abstract positioning",
  "====================================",
  "",
  paste0("Recommended title: ", selected_title),
  "",
  "Abstract positioning:",
  paste(abstract_positioning_12l_df$abstract_section, abstract_positioning_12l_df$recommended_message, sep = ": ")
)
writeLines(title_abstract_lines, file.path(out_text_dir, "12M_FINAL_title_abstract_package.txt"))
cat("[12M FINAL] Wrote:", file.path(out_text_dir, "12M_FINAL_title_abstract_package.txt"), "\n")

availability_lines <- c(
  "Data and code availability draft",
  "===============================",
  "",
  paste(data_code_availability$statement_type, data_code_availability$draft_statement, sep = ": ")
)
writeLines(availability_lines, file.path(out_text_dir, "12M_FINAL_data_code_availability_draft.txt"))
cat("[12M FINAL] Wrote:", file.path(out_text_dir, "12M_FINAL_data_code_availability_draft.txt"), "\n")

checklist_lines <- c(
  "Pre-submission checklist",
  "=======================",
  "",
  paste(submission_checklist$checklist_id, submission_checklist$checklist_item, submission_checklist$current_status, sep = " | ")
)
writeLines(checklist_lines, file.path(out_text_dir, "12M_FINAL_pre_submission_checklist.txt"))
cat("[12M FINAL] Wrote:", file.path(out_text_dir, "12M_FINAL_pre_submission_checklist.txt"), "\n")

# Copy newly generated text/checklists into submission package
copy_file_safe(file.path(out_text_dir, "12M_FINAL_title_abstract_package.txt"), file.path(submission_dir, "editorial_materials", "12M_FINAL_title_abstract_package.txt"))
copy_file_safe(file.path(out_text_dir, "12M_FINAL_data_code_availability_draft.txt"), file.path(submission_dir, "repository_and_code", "12M_FINAL_data_code_availability_draft.txt"))
copy_file_safe(file.path(out_text_dir, "12M_FINAL_pre_submission_checklist.txt"), file.path(submission_dir, "checklists", "12M_FINAL_pre_submission_checklist.txt"))
copy_file_safe(file.path(out_table_dir, "12M_FINAL_handoff_to_12N_no_overclaim_final_audit.csv"), file.path(submission_dir, "checklists", "12M_FINAL_handoff_to_12N_no_overclaim_final_audit.csv"))

submission_files <- list.files(submission_dir, recursive = TRUE, full.names = TRUE)
submission_files <- submission_files[file.exists(submission_files)]
submission_file_info <- file.info(submission_files)
submission_file_manifest <- data.frame(
  submission_file = submission_files,
  submission_relative_path = relative_path(submission_files, submission_dir),
  file_type = tolower(tools::file_ext(submission_files)),
  size_bytes = safe_num(submission_file_info$size),
  modified_time = as.character(submission_file_info$mtime),
  stringsAsFactors = FALSE
)
write_csv_safe(submission_file_manifest, file.path(out_table_dir, "12M_FINAL_submission_file_manifest.csv"))

# ------------------------- figures -------------------------
# FigA overview
fig_a <- open_pdf_safe("12M_FINAL_FigA_submission_package_overview.pdf", 11.8, 6.6)
new_canvas()
draw_title("Submission package overview", "12M assembles final manuscript, figure, repository and editorial package components.")

overview_df <- data.frame(
  label = c(
    "Manuscript components",
    "Figure/supplement items",
    "Checklist rows",
    "Submission files",
    "Copied package files",
    "12N handoff rows",
    "Claim-boundary pass blocks"
  ),
  value = c(
    nrow(manuscript_components),
    nrow(figure_package_manifest),
    nrow(submission_checklist),
    nrow(submission_file_manifest),
    sum(submission_copy_manifest$copied),
    nrow(handoff_12n),
    sum(claim_audit_12m$submission_package_claim_boundary_status == "claim_boundary_pass")
  ),
  family = c("manuscript", "figure", "check", "files", "copy", "handoff", "pass"),
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
  if (overview_df$family[idx_row] == "manuscript") color_now <- nature_palette$blue
  if (overview_df$family[idx_row] == "figure") color_now <- nature_palette$purple
  if (overview_df$family[idx_row] == "check") color_now <- nature_palette$gold
  if (overview_df$family[idx_row] == "files") color_now <- nature_palette$orange
  if (overview_df$family[idx_row] == "copy") color_now <- nature_palette$teal
  if (overview_df$family[idx_row] == "handoff") color_now <- nature_palette$navy
  if (overview_df$family[idx_row] == "pass") color_now <- nature_palette$teal
  text(bar_x0 - 0.018, yy, overview_df$label[idx_row], cex = 0.52, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.021, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.021,
       col = color_now, border = nature_palette$border, lwd = 0.42)
  text(bar_x0 + width_now * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_now), cex = 0.48, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.14, "Next: 12N should run the final no-overclaim and submission-readiness audit.", cex = 0.42, col = nature_palette$muted)
dev.off()
cat("[12M FINAL] Wrote figure:", fig_a, "\n")

# FigB manuscript component readiness
fig_b <- open_pdf_safe("12M_FINAL_FigB_manuscript_component_readiness.pdf", 12.0, 7.0)
new_canvas()
draw_title("Manuscript component readiness", "Each submission component is linked to a locked source and a manual finalization action.")

y_positions <- seq(0.84, 0.16, length.out = nrow(manuscript_components))
for (idx_row in seq_len(nrow(manuscript_components))) {
  yy <- y_positions[idx_row]
  color_now <- ifelse(manuscript_components$ready[idx_row], nature_palette$teal, nature_palette$red)
  rect(0.06, yy - 0.020, 0.18, yy + 0.020, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.12, yy, manuscript_components$component_id[idx_row], cex = 0.24, font = 2, col = nature_palette$white)
  text(0.21, yy + 0.008, manuscript_components$component_name[idx_row], cex = 0.27, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.21, yy - 0.012, substr(manuscript_components$manual_action_before_submission[idx_row], 1, 96), cex = 0.22, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12M FINAL] Wrote figure:", fig_b, "\n")

# FigC figure/supplement package map
fig_c <- open_pdf_safe("12M_FINAL_FigC_figure_supplement_package_map.pdf", 11.8, 6.8)
new_canvas()
draw_title("Figure and supplementary package map", "Main figures, supplementary figures and traceability tables are prepared for final formatting.")

fig_summary <- data.frame(
  item_type = c("main_figure", "supplementary_figure", "legend_table", "caption_table", "source_manifest_or_audit"),
  count = c(
    sum(figure_package_manifest$item_type == "main_figure"),
    sum(figure_package_manifest$item_type == "supplementary_figure"),
    sum(figure_package_manifest$item_type == "legend_table"),
    sum(figure_package_manifest$item_type == "caption_table"),
    sum(figure_package_manifest$item_type %in% c("source_manifest", "visual_audit", "assembly_package"))
  ),
  stringsAsFactors = FALSE
)
max_count <- max(fig_summary$count, na.rm = TRUE)
if (!is.finite(max_count) || max_count <= 0) max_count <- 1
y_positions <- seq(0.76, 0.34, length.out = nrow(fig_summary))
for (idx_row in seq_len(nrow(fig_summary))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$blue
  if (idx_row == 2) color_now <- nature_palette$purple
  if (idx_row == 3) color_now <- nature_palette$teal
  if (idx_row == 4) color_now <- nature_palette$orange
  if (idx_row == 5) color_now <- nature_palette$gold
  text(0.30, yy, fig_summary$item_type[idx_row], cex = 0.46, adj = c(1, 0.5), col = nature_palette$ink)
  rect(0.34, yy - 0.026, 0.34 + 0.38 * fig_summary$count[idx_row] / max_count, yy + 0.026,
       col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.74, yy, as.character(fig_summary$count[idx_row]), cex = 0.42, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.16, "Journal-specific PDF/TIFF/Word formatting remains a manual final step.", cex = 0.40, col = nature_palette$muted)
dev.off()
cat("[12M FINAL] Wrote figure:", fig_c, "\n")

# FigD pre-submission manual checklist
fig_d <- open_pdf_safe("12M_FINAL_FigD_pre_submission_manual_checklist.pdf", 12.0, 7.2)
new_canvas()
draw_title("Pre-submission checklist", "Automated package readiness is separated from manual journal-specific checks.")

manual_status_df <- data.frame(
  current_status = unique(submission_checklist$current_status),
  count = 0,
  automated = 0,
  stringsAsFactors = FALSE
)
for (idx_row in seq_len(nrow(manual_status_df))) {
  status_now <- manual_status_df$current_status[idx_row]
  keep_now <- submission_checklist$current_status == status_now
  manual_status_df$count[idx_row] <- sum(keep_now)
  manual_status_df$automated[idx_row] <- sum(submission_checklist$automated_by_12M[keep_now] == TRUE)
}
max_count <- max(manual_status_df$count, na.rm = TRUE)
if (!is.finite(max_count) || max_count <= 0) max_count <- 1
y_positions <- seq(0.78, 0.26, length.out = nrow(manual_status_df))
for (idx_row in seq_len(nrow(manual_status_df))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$blue
  if (grepl("manual", manual_status_df$current_status[idx_row], ignore.case = TRUE)) color_now <- nature_palette$orange
  if (grepl("ready", manual_status_df$current_status[idx_row], ignore.case = TRUE)) color_now <- nature_palette$teal
  text(0.36, yy, manual_status_df$current_status[idx_row], cex = 0.40, adj = c(1, 0.5), col = nature_palette$ink)
  rect(0.40, yy - 0.022, 0.40 + 0.35 * manual_status_df$count[idx_row] / max_count, yy + 0.022,
       col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.78, yy, paste0(manual_status_df$count[idx_row], " items"), cex = 0.34, adj = c(0, 0.5), col = nature_palette$ink)
}
dev.off()
cat("[12M FINAL] Wrote figure:", fig_d, "\n")

# FigE 12N handoff
fig_e <- open_pdf_safe("12M_FINAL_FigE_12N_no_overclaim_handoff.pdf", 12.0, 7.2)
new_canvas()
draw_title("12N final no-overclaim audit handoff", "12N should perform the final submission-readiness and overclaim audit.")

y_positions <- seq(0.84, 0.16, length.out = nrow(handoff_12n))
for (idx_row in seq_len(nrow(handoff_12n))) {
  yy <- y_positions[idx_row]
  color_now <- ifelse(handoff_12n$ready[idx_row], nature_palette$teal, nature_palette$red)
  rect(0.06, yy - 0.020, 0.32, yy + 0.020, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.19, yy, handoff_12n$final_audit_item[idx_row], cex = 0.24, font = 2, col = nature_palette$white)
  text(0.35, yy + 0.008, substr(handoff_12n$use_in_12N[idx_row], 1, 88), cex = 0.25, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.35, yy - 0.012, ifelse(handoff_12n$ready[idx_row], "ready", "review"), cex = 0.23, adj = c(0, 0.5), col = color_now)
}
dev.off()
cat("[12M FINAL] Wrote figure:", fig_e, "\n")

# ------------------------- final summary -------------------------
n_components_ready <- sum(manuscript_components$ready)
n_components_total <- nrow(manuscript_components)
n_fig_ready <- sum(figure_package_manifest$ready)
n_fig_total <- nrow(figure_package_manifest)
n_submission_check_rows <- nrow(submission_checklist)
n_readiness_pass <- sum(prelim_readiness$status)
n_readiness_total <- nrow(prelim_readiness)
n_copied <- sum(submission_copy_manifest$copied)
n_copy_total <- nrow(submission_copy_manifest)
n_claim_pass <- sum(claim_audit_12m$submission_package_claim_boundary_status == "claim_boundary_pass")
n_claim_total <- nrow(claim_audit_12m)
n_claim_repair <- sum(claim_audit_12m$submission_package_claim_boundary_status != "claim_boundary_pass")
n_handoff_ready <- sum(handoff_12n$ready)
n_handoff_total <- nrow(handoff_12n)
n_text_files <- sum(file.exists(c(
  file.path(out_text_dir, "12M_FINAL_title_abstract_package.txt"),
  file.path(out_text_dir, "12M_FINAL_data_code_availability_draft.txt"),
  file.path(out_text_dir, "12M_FINAL_pre_submission_checklist.txt")
)))

decision_value <- "INPUT_READY_FOR_12N_NO_OVERCLAIM_FINAL_AUDIT"
if (n_claim_repair > 0) decision_value <- "REPAIR_REQUIRED_BEFORE_12N"
if (n_handoff_ready < n_handoff_total) decision_value <- "REVIEW_REQUIRED_BEFORE_12N"
if (n_components_ready < n_components_total) decision_value <- "REVIEW_REQUIRED_BEFORE_12N"
if (n_fig_ready < n_fig_total) decision_value <- "REVIEW_REQUIRED_BEFORE_12N"

summary_df <- data.frame(
  item = c(
    "submission_package_dir",
    "manuscript_components_ready",
    "manuscript_components_total",
    "figure_supplement_items_ready",
    "figure_supplement_items_total",
    "pre_submission_checklist_rows",
    "readiness_audit_pass",
    "readiness_audit_total",
    "submission_files",
    "copied_package_files",
    "copied_package_total",
    "claim_boundary_pass_blocks",
    "claim_boundary_total_blocks",
    "claim_boundary_repair_needed",
    "12N_handoff_ready_rows",
    "12N_handoff_total_rows",
    "text_files_written",
    "figures_written",
    "decision"
  ),
  value = c(
    submission_dir,
    as.character(n_components_ready),
    as.character(n_components_total),
    as.character(n_fig_ready),
    as.character(n_fig_total),
    as.character(n_submission_check_rows),
    as.character(n_readiness_pass),
    as.character(n_readiness_total),
    as.character(nrow(submission_file_manifest)),
    as.character(n_copied),
    as.character(n_copy_total),
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
write_csv_safe(summary_df, file.path(out_table_dir, "12M_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "12M_FINAL_execution_summary.tsv"))

report_lines <- c(
  "12M FINAL report",
  "================",
  "Module: Submission package refresh",
  "Mode: complete standalone 12M V2 rebuild; no previous 12M output dependency; no internet; no 00-10P rerun. V2 repair: fixed claim-audit block vector length mismatch.",
  "Allowed upstream inputs: locked 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F, 12G, 12H, 12I, 12J, 12K and 12L outputs.",
  "",
  paste0("Submission package directory: ", submission_dir),
  paste0("Selected title: ", selected_title),
  paste0("Manuscript components ready: ", n_components_ready, "/", n_components_total),
  paste0("Figure/supplement items ready: ", n_fig_ready, "/", n_fig_total),
  paste0("Pre-submission checklist rows: ", n_submission_check_rows),
  paste0("Readiness audit pass: ", n_readiness_pass, "/", n_readiness_total),
  paste0("Submission files: ", nrow(submission_file_manifest)),
  paste0("Copied package files: ", n_copied, "/", n_copy_total),
  paste0("Claim-boundary pass blocks: ", n_claim_pass, "/", n_claim_total),
  paste0("Claim-boundary repair needed: ", n_claim_repair),
  paste0("12N handoff ready rows: ", n_handoff_ready, "/", n_handoff_total),
  "",
  "Main outputs:",
  paste0("- ", file.path(out_table_dir, "12M_FINAL_manuscript_component_manifest.csv")),
  paste0("- ", file.path(out_table_dir, "12M_FINAL_figure_and_supplement_manifest.csv")),
  paste0("- ", file.path(out_table_dir, "12M_FINAL_pre_submission_checklist.csv")),
  paste0("- ", file.path(out_table_dir, "12M_FINAL_submission_readiness_audit.csv")),
  paste0("- ", file.path(out_table_dir, "12M_FINAL_data_code_availability_draft.csv")),
  paste0("- ", file.path(out_table_dir, "12M_FINAL_handoff_to_12N_no_overclaim_final_audit.csv")),
  paste0("- ", submission_dir),
  "",
  "Manual checks still required before real submission:",
  "- target journal website, article type, APC/open-access rules and formatting policies",
  "- author list, affiliations, acknowledgements, ethics/funding/conflict-of-interest details",
  "- final PDF/TIFF/Word formatting according to journal requirements",
  "- repository URL and public release decision",
  "",
  "Claim boundary:",
  "- Submission package frames the study as a computational transcriptomic prioritisation framework.",
  "- Candidate signatures remain candidate transcriptomic marker signatures.",
  "- ML remains marker-rule-derived prioritization model audit.",
  "- Proxy evidence layers remain contextual support, not functional or causal proof.",
  "",
  paste0("Decision: ", decision_value)
)
report_file <- file.path(out_text_dir, "12M_FINAL_submission_package_V2_refresh_report.txt")
writeLines(report_lines, report_file)
cat("[12M FINAL] Wrote:", report_file, "\n")

cat("\n[12M FINAL] Completed Submission package refresh.\n")
cat("[12M FINAL] Submission package directory:", submission_dir, "\n")
cat("[12M FINAL] Selected title:", selected_title, "\n")
cat("[12M FINAL] Manuscript components ready:", n_components_ready, "/", n_components_total, "\n")
cat("[12M FINAL] Figure/supplement items ready:", n_fig_ready, "/", n_fig_total, "\n")
cat("[12M FINAL] Pre-submission checklist rows:", n_submission_check_rows, "\n")
cat("[12M FINAL] Readiness audit pass:", n_readiness_pass, "/", n_readiness_total, "\n")
cat("[12M FINAL] Submission files:", nrow(submission_file_manifest), "\n")
cat("[12M FINAL] Copied package files:", n_copied, "/", n_copy_total, "\n")
cat("[12M FINAL] Claim-boundary pass blocks:", n_claim_pass, "/", n_claim_total, "\n")
cat("[12M FINAL] Claim-boundary repair needed:", n_claim_repair, "\n")
cat("[12M FINAL] 12N handoff ready rows:", n_handoff_ready, "/", n_handoff_total, "\n")
cat("[12M FINAL] Text files written:", n_text_files, "\n")
cat("[12M FINAL] Figures written: 5\n")
cat("[12M FINAL] Decision:", decision_value, "\n")
cat("[12M FINAL] Output tables:", out_table_dir, "\n")
cat("[12M FINAL] Output figs  :", out_fig_dir, "\n")
cat("[12M FINAL] Output text  :", out_text_dir, "\n")
cat("[12M FINAL] Submission package:", submission_dir, "\n")
cat("[12M FINAL] Next         : review 12M submission package and PDFs; if accepted, proceed to 12N final no-overclaim audit.\n")
