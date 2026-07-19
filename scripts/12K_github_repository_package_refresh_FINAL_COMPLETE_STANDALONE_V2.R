
cat("\n[12K FINAL V2] Starting GitHub / repository package refresh with workflow-script indexing repair...\n")
cat("[12K FINAL] Mode: complete standalone 12K rebuild; no previous 12K dependency; no internet; no 00-10P rerun.\n")
cat("[12K FINAL] Inputs allowed: locked upstream 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F, 12G, 12H, 12I and 12J outputs.\n")
cat("[12K FINAL] Formal input: 12J Methods/reproducibility and GitHub handoff outputs.\n")
cat("[12K FINAL] Claim boundary: repository packaging only; no clinical prediction or validated biomarker claim.\n")

graphics.off()

project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")
figure_root <- file.path(project_root, "04_figures")
script_root <- file.path(project_root, "01_scripts")
text_root <- file.path(project_root, "09_manuscript")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "12K_github_repository_package_refresh_FINAL_COMPLETE_STANDALONE_V2"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "12K_github_repository_package_refresh_FINAL_COMPLETE_STANDALONE_V2_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "12K_github_repository_package_refresh_FINAL_COMPLETE_STANDALONE_V2"
)
repo_dir <- file.path(
  project_root,
  "12K_GitHub_repository_package_FINAL_COMPLETE_STANDALONE_V2"
)

repo_subdirs <- c(
  repo_dir,
  file.path(repo_dir, "docs"),
  file.path(repo_dir, "docs", "manuscript_text"),
  file.path(repo_dir, "docs", "provenance"),
  file.path(repo_dir, "docs", "claim_boundary"),
  file.path(repo_dir, "tables"),
  file.path(repo_dir, "figures"),
  file.path(repo_dir, "scripts"),
  file.path(repo_dir, "metadata")
)

dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_text_dir, recursive = TRUE, showWarnings = FALSE)
for (dir_now in repo_subdirs) dir.create(dir_now, recursive = TRUE, showWarnings = FALSE)

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
  cat("[12K FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[12K FINAL] Wrote:", file_value, "\n")
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
    cat("[12K FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
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
  out <- path_value
  out <- gsub("\\\\", "/", out)
  root_clean <- gsub("\\\\", "/", root_value)
  root_clean_regex <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", root_clean)
  out <- gsub(paste0("^", root_clean_regex, "/?"), "", out)
  out
}

get_current_source_file <- function() {
  frame_files <- character(0)
  frames <- sys.frames()
  for (idx_frame in seq_along(frames)) {
    ofile_now <- tryCatch(frames[[idx_frame]]$ofile, error = function(err_obj) NULL)
    if (!is.null(ofile_now)) frame_files <- c(frame_files, safe_chr(ofile_now))
  }
  frame_files <- unique(frame_files[file.exists(frame_files)])
  if (length(frame_files) < 1) return("")
  normalizePath(frame_files[length(frame_files)], winslash = "/", mustWork = FALSE)
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

if (!dir.exists(table_root)) stop("[12K FINAL] Missing table root: ", table_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
table_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(table_info$size) & table_info$size > 0 & table_info$size < 300 * 1024 * 1024]

all_table_files <- all_table_files[!grepl("12K_github_repository_package_refresh", all_table_files, ignore.case = TRUE)]

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

file_12j_github_handoff <- first_existing_file(c(
  file.path(table_root, "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE", "12J_FINAL_handoff_to_12K_github_repository_package.csv"),
  find_files_all_terms(all_table_files, c("12j", "handoff_to_12k_github_repository_package"), max_n = 10)
))
file_12j_module_provenance <- first_existing_file(c(
  file.path(table_root, "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE", "12J_FINAL_locked_module_provenance_table.csv"),
  find_files_all_terms(all_table_files, c("12j", "locked_module_provenance_table"), max_n = 10)
))
file_12j_repro_checklist <- first_existing_file(c(
  file.path(table_root, "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE", "12J_FINAL_reproducibility_checklist.csv"),
  find_files_all_terms(all_table_files, c("12j", "reproducibility_checklist"), max_n = 10)
))
file_12j_claim_boundary <- first_existing_file(c(
  file.path(table_root, "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE", "12J_FINAL_claim_boundary_statement.csv"),
  find_files_all_terms(all_table_files, c("12j", "claim_boundary_statement"), max_n = 10)
))
file_12j_methods_blocks <- first_existing_file(c(
  file.path(table_root, "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE", "12J_FINAL_methods_text_blocks.csv"),
  find_files_all_terms(all_table_files, c("12j", "methods_text_blocks"), max_n = 10)
))
file_12j_methods_claim_audit <- first_existing_file(c(
  file.path(table_root, "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE", "12J_FINAL_methods_claim_boundary_audit.csv"),
  find_files_all_terms(all_table_files, c("12j", "methods_claim_boundary_audit"), max_n = 10)
))
file_12j_code_avail <- first_existing_file(c(
  file.path(text_root, "12J_methods_reproducibility_refresh_FINAL_COMPLETE_STANDALONE", "12J_FINAL_code_availability_statement.txt")
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
file_12f_panel_assembly <- first_existing_file(c(
  file.path(table_root, "12F_optional_final_assembly_MAIN_FIG1_REDESIGN_FINAL_COMPLETE_STANDALONE_V2", "12F_FINAL_panel_assembly_manifest.csv"),
  find_files_all_terms(all_table_files, c("12f", "panel_assembly_manifest"), max_n = 10)
))
file_12g_panel_captions <- first_existing_file(c(
  file.path(table_root, "12G_final_legends_caption_refresh_FINAL_COMPLETE_STANDALONE", "12G_FINAL_panel_caption_table.csv"),
  find_files_all_terms(all_table_files, c("12g", "panel_caption_table"), max_n = 10)
))

github_handoff_12j_df <- read_table_safe(file_12j_github_handoff)
module_provenance_12j_df <- read_table_safe(file_12j_module_provenance)
repro_checklist_12j_df <- read_table_safe(file_12j_repro_checklist)
claim_boundary_12j_df <- read_table_safe(file_12j_claim_boundary)
methods_blocks_12j_df <- read_table_safe(file_12j_methods_blocks)
methods_claim_audit_12j_df <- read_table_safe(file_12j_methods_claim_audit)
panel_assembly_12f_df <- read_table_safe(file_12f_panel_assembly)
panel_captions_12g_df <- read_table_safe(file_12g_panel_captions)

if (nrow(github_handoff_12j_df) < 1) stop("[12K FINAL] Missing 12J handoff to 12K table.", call. = FALSE)
if (nrow(module_provenance_12j_df) < 1) stop("[12K FINAL] Missing 12J locked module provenance table.", call. = FALSE)
if (nrow(repro_checklist_12j_df) < 1) stop("[12K FINAL] Missing 12J reproducibility checklist.", call. = FALSE)

input_audit_df <- data.frame(
  input_name = c(
    "12J_handoff_to_12K_github_repository_package",
    "12J_locked_module_provenance_table",
    "12J_reproducibility_checklist",
    "12J_claim_boundary_statement",
    "12J_methods_text_blocks",
    "12J_methods_claim_boundary_audit",
    "12J_code_availability_statement",
    "12H_results_text_full",
    "12I_discussion_text_full",
    "12J_methods_text_full",
    "12F_panel_assembly_manifest",
    "12G_panel_caption_table"
  ),
  detected = c(
    file_12j_github_handoff != "",
    file_12j_module_provenance != "",
    file_12j_repro_checklist != "",
    file_12j_claim_boundary != "",
    file_12j_methods_blocks != "",
    file_12j_methods_claim_audit != "",
    file_12j_code_avail != "",
    file_12h_results_text != "",
    file_12i_discussion_text != "",
    file_12j_methods_text != "",
    file_12f_panel_assembly != "",
    file_12g_panel_captions != ""
  ),
  file_path = c(
    file_12j_github_handoff,
    file_12j_module_provenance,
    file_12j_repro_checklist,
    file_12j_claim_boundary,
    file_12j_methods_blocks,
    file_12j_methods_claim_audit,
    file_12j_code_avail,
    file_12h_results_text,
    file_12i_discussion_text,
    file_12j_methods_text,
    file_12f_panel_assembly,
    file_12g_panel_captions
  ),
  rows_loaded = c(
    nrow(github_handoff_12j_df),
    nrow(module_provenance_12j_df),
    nrow(repro_checklist_12j_df),
    nrow(claim_boundary_12j_df),
    nrow(methods_blocks_12j_df),
    nrow(methods_claim_audit_12j_df),
    ifelse(file_12j_code_avail != "", length(readLines(file_12j_code_avail, warn = FALSE)), 0),
    ifelse(file_12h_results_text != "", length(readLines(file_12h_results_text, warn = FALSE)), 0),
    ifelse(file_12i_discussion_text != "", length(readLines(file_12i_discussion_text, warn = FALSE)), 0),
    ifelse(file_12j_methods_text != "", length(readLines(file_12j_methods_text, warn = FALSE)), 0),
    nrow(panel_assembly_12f_df),
    nrow(panel_captions_12g_df)
  ),
  allowed_as_locked_upstream_input = TRUE,
  stringsAsFactors = FALSE
)
write_csv_safe(input_audit_df, file.path(out_table_dir, "12K_FINAL_locked_12J_input_audit.csv"))

copy_items <- data.frame(
  package_category = c(
    "provenance", "provenance", "provenance", "claim_boundary", "manuscript_text",
    "manuscript_text", "manuscript_text", "manuscript_text", "tables", "tables"
  ),
  source_file = c(
    file_12j_module_provenance,
    file_12j_repro_checklist,
    file_12j_github_handoff,
    file_12j_claim_boundary,
    file_12h_results_text,
    file_12i_discussion_text,
    file_12j_methods_text,
    file_12j_code_avail,
    file_12f_panel_assembly,
    file_12g_panel_captions
  ),
  destination_subdir = c(
    "docs/provenance",
    "docs/provenance",
    "docs/provenance",
    "docs/claim_boundary",
    "docs/manuscript_text",
    "docs/manuscript_text",
    "docs/manuscript_text",
    "docs/manuscript_text",
    "tables",
    "tables"
  ),
  stringsAsFactors = FALSE
)

copy_manifest_list <- list()
for (idx_copy in seq_len(nrow(copy_items))) {
  source_now <- clean_space(copy_items$source_file[idx_copy])
  destination_dir <- file.path(repo_dir, copy_items$destination_subdir[idx_copy])
  destination_file <- file.path(destination_dir, basename(source_now))
  ok_now <- copy_file_safe(source_now, destination_file)
  copy_manifest_list[[length(copy_manifest_list) + 1]] <- data.frame(
    package_category = copy_items$package_category[idx_copy],
    source_file = source_now,
    source_exists = file_exists_safe(source_now),
    destination_file = ifelse(ok_now, destination_file, ""),
    copied = ok_now,
    stringsAsFactors = FALSE
  )
}
copy_manifest <- safe_bind_rows(copy_manifest_list)
write_csv_safe(copy_manifest, file.path(out_table_dir, "12K_FINAL_repository_copy_manifest.csv"))

write_csv_safe(module_provenance_12j_df, file.path(repo_dir, "docs", "provenance", "locked_module_provenance_table.csv"))
write_csv_safe(repro_checklist_12j_df, file.path(repo_dir, "docs", "provenance", "reproducibility_checklist.csv"))
write_csv_safe(claim_boundary_12j_df, file.path(repo_dir, "docs", "claim_boundary", "claim_boundary_statement.csv"))

script_files <- character(0)

if (dir.exists(script_root)) {
  script_files <- c(script_files, list.files(script_root, pattern = "\\.(R|r)$", recursive = TRUE, full.names = TRUE))
}

if (dir.exists(project_root)) {
  project_scripts_all <- list.files(project_root, pattern = "\\.(R|r)$", recursive = TRUE, full.names = TRUE)
  project_scripts_all <- project_scripts_all[!grepl("/(02_objects|03_tables|04_figures|05_figures|06_data|07_raw|08_cache|09_manuscript|12K_GitHub_repository_package)", gsub("\\\\", "/", project_scripts_all), ignore.case = TRUE)]
  script_files <- c(script_files, project_scripts_all)
}

current_script <- get_current_source_file()
if (current_script != "" && file.exists(current_script)) script_files <- c(script_files, current_script)

script_files <- unique(script_files[file.exists(script_files)])
script_files <- script_files[!grepl("~\\$", basename(script_files))]
script_files <- script_files[file.info(script_files)$size > 0 & file.info(script_files)$size < 30 * 1024 * 1024]

script_info <- data.frame(stringsAsFactors = FALSE)
if (length(script_files) > 0) {
  info <- file.info(script_files)
  script_info <- data.frame(
    script_file = normalizePath(script_files, winslash = "/", mustWork = FALSE),
    script_name = basename(script_files),
    relative_path = ifelse(grepl(gsub("\\\\", "/", project_root), normalizePath(script_files, winslash = "/", mustWork = FALSE), fixed = TRUE),
                           relative_path(normalizePath(script_files, winslash = "/", mustWork = FALSE), project_root),
                           paste0("external_source_path/", basename(script_files))),
    size_bytes = safe_num(info$size),
    modified_time = as.character(info$mtime),
    module_guess = gsub("_.*$", "", basename(script_files)),
    capture_source = ifelse(normalizePath(script_files, winslash = "/", mustWork = FALSE) == current_script, "current_sourced_script", "project_script_search"),
    copied_to_repo = FALSE,
    repo_path = "",
    stringsAsFactors = FALSE
  )

  used_names <- character(0)
  for (idx_script in seq_len(nrow(script_info))) {
    base_now <- script_info$script_name[idx_script]
    if (base_now %in% used_names) {
      base_now <- paste0(tools::file_path_sans_ext(base_now), "_dup", idx_script, ".", tools::file_ext(base_now))
    }
    used_names <- c(used_names, base_now)
    dest_script <- file.path(repo_dir, "scripts", base_now)
    ok_script <- copy_file_safe(script_info$script_file[idx_script], dest_script)
    script_info$copied_to_repo[idx_script] <- ok_script
    script_info$repo_path[idx_script] <- ifelse(ok_script, dest_script, "")
  }
}

write_csv_safe(script_info, file.path(out_table_dir, "12K_FINAL_workflow_script_index.csv"))
write_csv_safe(script_info, file.path(repo_dir, "metadata", "workflow_script_index.csv"))

script_capture_note <- data.frame(
  item = c(
    "script_search_root_primary",
    "script_search_root_broader",
    "current_sourced_script_detected",
    "workflow_scripts_indexed",
    "workflow_scripts_copied_to_repo",
    "interpretation"
  ),
  value = c(
    script_root,
    project_root,
    current_script,
    as.character(nrow(script_info)),
    as.character(ifelse(nrow(script_info) > 0, sum(script_info$copied_to_repo), 0)),
    ifelse(nrow(script_info) > 1,
           "workflow script archive contains multiple discovered scripts",
           "workflow script archive may contain only the currently sourced script; add full historical scripts manually before public GitHub release if needed")
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(script_capture_note, file.path(out_table_dir, "12K_FINAL_script_capture_note.csv"))
write_csv_safe(script_capture_note, file.path(repo_dir, "metadata", "script_capture_note.csv"))

source_manifest <- data.frame(
  accession = c(
    "GSE128040", "GSE132758", "GSE148434", "GSE157783", "GSE178265", "GSE183248",
    "GSE184950", "GSE200610", "GSE204795", "GSE204796", "GSE233885", "GSE243639"
  ),
  locked_role = c(
    "manual_review_or_background_not_core_training",
    "core_model_development_reference",
    "manual_review_or_background_not_core_training",
    "manual_review_or_background_not_core_training",
    "core_model_development_reference",
    "independent_external_validation_not_training",
    "manual_review_or_background_not_core_training",
    "core_model_development_reference",
    "bulk_support_not_scRNA_training",
    "core_model_development_reference",
    "core_model_development_reference",
    "marker_targeted_context_validation_not_training"
  ),
  used_for_training_or_model_development = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE),
  used_as_external_or_context_support = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
  repository_note = "Public raw/processed data should be obtained from original GEO/source repositories; this package stores scripts and derived manifests only.",
  stringsAsFactors = FALSE
)
write_csv_safe(source_manifest, file.path(out_table_dir, "12K_FINAL_data_source_manifest.csv"))
write_csv_safe(source_manifest, file.path(repo_dir, "metadata", "data_source_manifest.csv"))

readme_lines <- c(
  "# DA neuron / graft-related transcriptomic cell-state prioritisation framework",
  "",
  "This repository package contains scripts, source manifests, provenance tables and manuscript-supporting text for a source-traceable computational transcriptomic prioritisation framework.",
  "",
  "## Scope",
  "",
  "The project prioritises candidate dopaminergic neuron and graft-related transcriptomic cell states using multi-layer computational evidence.",
  "",
  "Allowed interpretation:",
  "",
  "- source-traceable computational transcriptomic prioritisation framework",
  "- candidate transcriptomic cell states",
  "- candidate transcriptomic marker signatures",
  "- marker-rule-derived prioritization model audit",
  "- graph-based transcriptomic pseudotime/module support",
  "- proxy/contextual evidence support",
  "",
  "Not claimed:",
  "",
  "- clinical-use model",
  "- validated diagnostic, prognostic or therapeutic biomarker",
  "- graft efficacy or clinical safety prediction",
  "- anatomical-projection claim",
  "- barcode-lineage claim",
  "- genetic causality or disease mechanism proof",
  "",
  "## Repository structure",
  "",
  "- `scripts/`: discovered R scripts copied from the local workflow, including the currently sourced 12K V2 script when available.",
  "- `tables/`: selected panel/caption/source tables.",
  "- `docs/provenance/`: locked module provenance and reproducibility checklist.",
  "- `docs/claim_boundary/`: allowed/prohibited claim wording statement.",
  "- `docs/manuscript_text/`: Results, Discussion, Methods and code availability text outputs.",
  "- `metadata/`: script index and data/source manifest.",
  "",
  "## Reproducibility principle",
  "",
  "Final manuscript-preparation modules were designed to read locked upstream outputs only and to avoid same-module old-output reuse. Raw public data are not redistributed here and should be retrieved from original public repositories.",
  "",
  "## Script archive note",
  "",
  "The workflow script index records scripts discovered locally at the time this package was generated. If earlier scripts were run from temporary download folders and were not saved under the project directory, they should be manually added before public GitHub release.",
  "",
  "## Module status",
  "",
  "The package is generated from locked 12J Methods/reproducibility outputs and is ready for 12L journal/cover-letter planning."
)
writeLines(readme_lines, file.path(repo_dir, "README.md"))
writeLines(readme_lines, file.path(out_text_dir, "12K_FINAL_README.md"))
cat("[12K FINAL] Wrote:", file.path(repo_dir, "README.md"), "\n")

readme_zh_lines <- c(
  "# DA neuron / graft-related 转录组细胞状态优先级框架",
  "",
  "这个文件夹不是重新跑统计分析，而是把已经锁定的分析结果整理成 GitHub / 附件 / 投稿补充材料可用的结构。",
  "",
  "本项目定位为 source-traceable computational transcriptomic prioritisation framework。",
  "",
  "可以说：候选转录组细胞状态、候选转录组 marker signature、marker-rule-derived 内部优先级审计、pseudotime/module 支持、proxy/context 支持。",
  "",
  "不能说：临床预测模型、已验证 biomarker、移植物疗效/安全性预测、真实解剖投射证明、barcode 谱系追踪证明、遗传因果证明。",
  "",
  "原始公共数据不在此文件夹重新分发，需要从 GEO/source repository 获取。",
  "",
  "注意：本次 12K V2 会索引并复制本地能找到的 R 脚本，也会捕获当前 source 运行的 12K V2 脚本。如果早期脚本只在浏览器临时下载目录里运行、没有保存到项目目录，公开 GitHub 前需要手动补齐。"
)
writeLines(readme_zh_lines, file.path(repo_dir, "README_zh.md"))
writeLines(readme_zh_lines, file.path(out_text_dir, "12K_FINAL_README_zh.md"))
cat("[12K FINAL] Wrote:", file.path(repo_dir, "README_zh.md"), "\n")

repo_files <- list.files(repo_dir, recursive = TRUE, full.names = TRUE)
repo_files <- repo_files[file.exists(repo_files)]
repo_file_info <- file.info(repo_files)
repo_manifest <- data.frame(
  repo_file = repo_files,
  repo_relative_path = relative_path(repo_files, repo_dir),
  file_type = tolower(tools::file_ext(repo_files)),
  size_bytes = safe_num(repo_file_info$size),
  modified_time = as.character(repo_file_info$mtime),
  stringsAsFactors = FALSE
)
write_csv_safe(repo_manifest, file.path(out_table_dir, "12K_FINAL_repository_file_manifest.csv"))
write_csv_safe(repo_manifest, file.path(repo_dir, "metadata", "repository_file_manifest.csv"))

handoff_12l <- data.frame(
  journal_package_item = c(
    "Repository README",
    "Data/source manifest",
    "Workflow script index",
    "Script capture note",
    "Locked module provenance",
    "Reproducibility checklist",
    "Claim-boundary statement",
    "Results/Discussion/Methods text",
    "Code availability statement",
    "Repository file manifest"
  ),
  package_file = c(
    file.path(repo_dir, "README.md"),
    file.path(repo_dir, "metadata", "data_source_manifest.csv"),
    file.path(repo_dir, "metadata", "workflow_script_index.csv"),
    file.path(repo_dir, "metadata", "script_capture_note.csv"),
    file.path(repo_dir, "docs", "provenance", "locked_module_provenance_table.csv"),
    file.path(repo_dir, "docs", "provenance", "reproducibility_checklist.csv"),
    file.path(repo_dir, "docs", "claim_boundary", "claim_boundary_statement.csv"),
    file.path(repo_dir, "docs", "manuscript_text"),
    file.path(repo_dir, "docs", "manuscript_text", "12J_FINAL_code_availability_statement.txt"),
    file.path(repo_dir, "metadata", "repository_file_manifest.csv")
  ),
  use_in_12L = c(
    "project summary and repository description",
    "Data availability and source transparency",
    "Code availability and workflow transparency",
    "Code archive completeness note",
    "Methods/reproducibility supplement",
    "Reproducibility statement",
    "Cover letter and limitation statement",
    "Manuscript package planning",
    "Code availability section",
    "Submission package checklist"
  ),
  ready = c(
    file.exists(file.path(repo_dir, "README.md")),
    file.exists(file.path(repo_dir, "metadata", "data_source_manifest.csv")),
    file.exists(file.path(repo_dir, "metadata", "workflow_script_index.csv")) && nrow(script_info) > 0,
    file.exists(file.path(repo_dir, "metadata", "script_capture_note.csv")),
    file.exists(file.path(repo_dir, "docs", "provenance", "locked_module_provenance_table.csv")),
    file.exists(file.path(repo_dir, "docs", "provenance", "reproducibility_checklist.csv")),
    file.exists(file.path(repo_dir, "docs", "claim_boundary", "claim_boundary_statement.csv")),
    dir.exists(file.path(repo_dir, "docs", "manuscript_text")),
    file.exists(file.path(repo_dir, "docs", "manuscript_text", "12J_FINAL_code_availability_statement.txt")),
    file.exists(file.path(repo_dir, "metadata", "repository_file_manifest.csv"))
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(handoff_12l, file.path(out_table_dir, "12K_FINAL_handoff_to_12L_journal_cover_letter.csv"))

readme_file <- file.path(repo_dir, "README.md")
readme_lines_for_audit <- if (file.exists(readme_file)) readLines(readme_file, warn = FALSE) else character(0)

claim_audit_package <- data.frame(
  audit_item = c(
    "README exists",
    "README contains computational framing",
    "Claim-boundary statement exists",
    "No raw data redistribution statement included",
    "Code availability statement exists",
    "12J claim-boundary sections pass",
    "Repository manifest exists",
    "Workflow script index non-empty",
    "12L handoff exists"
  ),
  status = c(
    file.exists(file.path(repo_dir, "README.md")),
    any(grepl("computational transcriptomic prioritisation", readme_lines_for_audit, ignore.case = TRUE)),
    file.exists(file.path(repo_dir, "docs", "claim_boundary", "claim_boundary_statement.csv")),
    any(grepl("Raw public data are not redistributed", readme_lines_for_audit, ignore.case = TRUE)),
    file.exists(file.path(repo_dir, "docs", "manuscript_text", "12J_FINAL_code_availability_statement.txt")),
    ifelse(nrow(methods_claim_audit_12j_df) > 0, all(methods_claim_audit_12j_df$methods_claim_boundary_status == "claim_boundary_pass"), FALSE),
    file.exists(file.path(repo_dir, "metadata", "repository_file_manifest.csv")),
    nrow(script_info) > 0,
    file.exists(file.path(out_table_dir, "12K_FINAL_handoff_to_12L_journal_cover_letter.csv"))
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(claim_audit_package, file.path(out_table_dir, "12K_FINAL_repository_claim_boundary_audit.csv"))

fig_a <- open_pdf_safe("12K_FINAL_FigA_repository_package_overview.pdf", 11.8, 6.6)
new_canvas()
draw_title("GitHub / repository package overview", "12K packages scripts, manifests, provenance tables and manuscript text outputs.")

overview_df <- data.frame(
  label = c(
    "Repository files",
    "Copied source files",
    "Script index rows",
    "Scripts copied",
    "Source manifest rows",
    "12L handoff rows",
    "Claim-boundary audit pass"
  ),
  value = c(
    nrow(repo_manifest),
    sum(copy_manifest$copied),
    nrow(script_info),
    ifelse(nrow(script_info) > 0, sum(script_info$copied_to_repo), 0),
    nrow(source_manifest),
    nrow(handoff_12l),
    sum(claim_audit_package$status)
  ),
  family = c("repo", "copy", "script", "scriptcopy", "source", "handoff", "pass"),
  stringsAsFactors = FALSE
)
max_value <- max(safe_num(overview_df$value), na.rm = TRUE)
if (!is.finite(max_value) || max_value <= 0) max_value <- 1
bar_x0 <- 0.43
bar_x1 <- 0.80
y_positions <- seq(0.80, 0.28, length.out = nrow(overview_df))
for (idx_row in seq_len(nrow(overview_df))) {
  yy <- y_positions[idx_row]
  count_now <- safe_num(overview_df$value[idx_row])
  width_now <- count_now / max_value
  if (count_now == 0) width_now <- 0.018
  color_now <- nature_palette$navy
  if (overview_df$family[idx_row] == "repo") color_now <- nature_palette$blue
  if (overview_df$family[idx_row] == "copy") color_now <- nature_palette$teal
  if (overview_df$family[idx_row] == "script") color_now <- nature_palette$purple
  if (overview_df$family[idx_row] == "scriptcopy") color_now <- nature_palette$purple
  if (overview_df$family[idx_row] == "source") color_now <- nature_palette$orange
  if (overview_df$family[idx_row] == "handoff") color_now <- nature_palette$gold
  if (overview_df$family[idx_row] == "pass") color_now <- nature_palette$teal
  text(bar_x0 - 0.018, yy, overview_df$label[idx_row], cex = 0.52, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.021, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.021,
       col = color_now, border = nature_palette$border, lwd = 0.42)
  text(bar_x0 + width_now * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_now), cex = 0.48, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.14, "Next: 12L should use this package for journal targeting and cover-letter drafting.", cex = 0.42, col = nature_palette$muted)
dev.off()
cat("[12K FINAL] Wrote figure:", fig_a, "\n")

fig_b <- open_pdf_safe("12K_FINAL_FigB_repository_structure_map.pdf", 12.0, 6.8)
new_canvas()
draw_title("Repository structure map", "Generated repository package folders and intended contents.")

structure_df <- data.frame(
  folder = c("README", "scripts", "tables", "metadata", "docs/provenance", "docs/claim_boundary", "docs/manuscript_text"),
  content = c(
    "project framing and scope boundary",
    "discovered workflow scripts including current sourced 12K V2 script",
    "selected panel/caption/source tables",
    "data-source, script index and repository manifests",
    "module provenance and reproducibility checklist",
    "allowed/prohibited claim wording",
    "Results, Discussion, Methods and code availability text"
  ),
  stringsAsFactors = FALSE
)
y_positions <- seq(0.78, 0.28, length.out = nrow(structure_df))
for (idx_row in seq_len(nrow(structure_df))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$blue
  if (idx_row %% 4 == 1) color_now <- nature_palette$blue
  if (idx_row %% 4 == 2) color_now <- nature_palette$teal
  if (idx_row %% 4 == 3) color_now <- nature_palette$purple
  if (idx_row %% 4 == 0) color_now <- nature_palette$orange
  rect(0.08, yy - 0.026, 0.31, yy + 0.026, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.195, yy, structure_df$folder[idx_row], cex = 0.34, font = 2, col = nature_palette$white)
  text(0.35, yy, structure_df$content[idx_row], cex = 0.35, adj = c(0, 0.5), col = nature_palette$ink)
}
dev.off()
cat("[12K FINAL] Wrote figure:", fig_b, "\n")

fig_c <- open_pdf_safe("12K_FINAL_FigC_package_provenance_readiness.pdf", 11.8, 6.8)
new_canvas()
draw_title("Package provenance readiness", "Key package components are checked before journal/cover-letter planning.")

readiness_df <- data.frame(
  item = c(
    "README",
    "Data/source manifest",
    "Workflow script index non-empty",
    "Script capture note",
    "Locked module provenance",
    "Reproducibility checklist",
    "Claim-boundary statement",
    "Repository file manifest",
    "12L handoff"
  ),
  ready = c(
    file.exists(file.path(repo_dir, "README.md")),
    file.exists(file.path(repo_dir, "metadata", "data_source_manifest.csv")),
    file.exists(file.path(repo_dir, "metadata", "workflow_script_index.csv")) && nrow(script_info) > 0,
    file.exists(file.path(repo_dir, "metadata", "script_capture_note.csv")),
    file.exists(file.path(repo_dir, "docs", "provenance", "locked_module_provenance_table.csv")),
    file.exists(file.path(repo_dir, "docs", "provenance", "reproducibility_checklist.csv")),
    file.exists(file.path(repo_dir, "docs", "claim_boundary", "claim_boundary_statement.csv")),
    file.exists(file.path(repo_dir, "metadata", "repository_file_manifest.csv")),
    file.exists(file.path(out_table_dir, "12K_FINAL_handoff_to_12L_journal_cover_letter.csv"))
  ),
  stringsAsFactors = FALSE
)
y_positions <- seq(0.80, 0.22, length.out = nrow(readiness_df))
for (idx_row in seq_len(nrow(readiness_df))) {
  yy <- y_positions[idx_row]
  color_now <- ifelse(readiness_df$ready[idx_row], nature_palette$teal, nature_palette$red)
  symbols(0.22, yy, circles = 0.016, inches = FALSE, add = TRUE,
          bg = color_now, fg = nature_palette$border, lwd = 0.35)
  text(0.26, yy, readiness_df$item[idx_row], cex = 0.44, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.72, yy, ifelse(readiness_df$ready[idx_row], "ready", "review"), cex = 0.40, adj = c(0, 0.5), col = color_now)
}
dev.off()
cat("[12K FINAL] Wrote figure:", fig_c, "\n")

fig_d <- open_pdf_safe("12K_FINAL_FigD_repository_claim_boundary_audit.pdf", 11.8, 6.6)
new_canvas()
draw_title("Repository claim-boundary audit", "The package is checked for conservative framing before external sharing.")

y_positions <- seq(0.80, 0.26, length.out = nrow(claim_audit_package))
for (idx_row in seq_len(nrow(claim_audit_package))) {
  yy <- y_positions[idx_row]
  color_now <- ifelse(claim_audit_package$status[idx_row], nature_palette$teal, nature_palette$red)
  rect(0.10, yy - 0.018, 0.17, yy + 0.018, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.20, yy, claim_audit_package$audit_item[idx_row], cex = 0.37, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.72, yy, ifelse(claim_audit_package$status[idx_row], "pass", "review"), cex = 0.34, adj = c(0, 0.5), col = color_now)
}
dev.off()
cat("[12K FINAL] Wrote figure:", fig_d, "\n")

fig_e <- open_pdf_safe("12K_FINAL_FigE_12L_journal_cover_letter_handoff.pdf", 12.0, 7.2)
new_canvas()
draw_title("12L journal / cover-letter handoff", "12L should use repository package outputs for journal targeting and cover-letter drafting.")

y_positions <- seq(0.84, 0.16, length.out = nrow(handoff_12l))
for (idx_row in seq_len(nrow(handoff_12l))) {
  yy <- y_positions[idx_row]
  color_now <- nature_palette$blue
  if (idx_row %% 3 == 1) color_now <- nature_palette$blue
  if (idx_row %% 3 == 2) color_now <- nature_palette$teal
  if (idx_row %% 3 == 0) color_now <- nature_palette$purple
  rect(0.06, yy - 0.020, 0.30, yy + 0.020, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.18, yy, handoff_12l$journal_package_item[idx_row], cex = 0.26, font = 2, col = nature_palette$white)
  text(0.33, yy + 0.008, substr(handoff_12l$use_in_12L[idx_row], 1, 88), cex = 0.25, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.33, yy - 0.012, ifelse(handoff_12l$ready[idx_row], "ready", "review"), cex = 0.23, adj = c(0, 0.5), col = ifelse(handoff_12l$ready[idx_row], nature_palette$teal, nature_palette$red))
}
dev.off()
cat("[12K FINAL] Wrote figure:", fig_e, "\n")

n_repo_files <- nrow(repo_manifest)
n_copied <- sum(copy_manifest$copied)
n_copy_missing <- sum(!copy_manifest$copied)
n_scripts_indexed <- nrow(script_info)
n_scripts_copied <- ifelse(nrow(script_info) > 0, sum(script_info$copied_to_repo), 0)
n_claim_pass <- sum(claim_audit_package$status)
n_claim_total <- nrow(claim_audit_package)
n_12l_ready <- sum(handoff_12l$ready)
n_12l_total <- nrow(handoff_12l)

decision_value <- "INPUT_READY_FOR_12L_JOURNAL_COVER_LETTER_REFRESH"
if (n_scripts_indexed < 1 || n_scripts_copied < 1) decision_value <- "REPAIR_REQUIRED_BEFORE_12L"
if (n_claim_pass < n_claim_total) decision_value <- "REPAIR_REQUIRED_BEFORE_12L"
if (n_12l_ready < n_12l_total) decision_value <- "REVIEW_REQUIRED_BEFORE_12L"

summary_df <- data.frame(
  item = c(
    "repository_package_dir",
    "repository_files",
    "copied_locked_source_files",
    "copy_missing_or_failed",
    "workflow_scripts_indexed",
    "workflow_scripts_copied_to_repo",
    "current_sourced_script",
    "data_source_manifest_rows",
    "claim_boundary_audit_pass",
    "claim_boundary_audit_total",
    "12L_handoff_ready_rows",
    "12L_handoff_total_rows",
    "figures_written",
    "decision"
  ),
  value = c(
    repo_dir,
    as.character(n_repo_files),
    as.character(n_copied),
    as.character(n_copy_missing),
    as.character(n_scripts_indexed),
    as.character(n_scripts_copied),
    current_script,
    as.character(nrow(source_manifest)),
    as.character(n_claim_pass),
    as.character(n_claim_total),
    as.character(n_12l_ready),
    as.character(n_12l_total),
    "5",
    decision_value
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "12K_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "12K_FINAL_execution_summary.tsv"))

report_lines <- c(
  "12K FINAL V2 report",
  "===================",
  "Module: GitHub / repository package refresh",
  "Mode: complete standalone 12K rebuild; no previous 12K output dependency; no internet; no 00-10P rerun.",
  "Allowed upstream inputs: locked 10A-10P, 11A-11J, 12A, 12B, 12C, 12D, 12E, 12F, 12G, 12H, 12I and 12J outputs.",
  "V2 repair: workflow-script index must be non-empty.",
  "",
  paste0("Repository package directory: ", repo_dir),
  paste0("Repository files: ", n_repo_files),
  paste0("Copied locked source files: ", n_copied),
  paste0("Copy missing or failed: ", n_copy_missing),
  paste0("Workflow scripts indexed: ", n_scripts_indexed),
  paste0("Workflow scripts copied to repo: ", n_scripts_copied),
  paste0("Current sourced script captured: ", current_script),
  paste0("Data/source manifest rows: ", nrow(source_manifest)),
  paste0("Claim-boundary audit pass: ", n_claim_pass, "/", n_claim_total),
  paste0("12L handoff ready rows: ", n_12l_ready, "/", n_12l_total),
  "",
  "Main outputs:",
  paste0("- ", file.path(repo_dir, "README.md")),
  paste0("- ", file.path(repo_dir, "README_zh.md")),
  paste0("- ", file.path(repo_dir, "metadata", "data_source_manifest.csv")),
  paste0("- ", file.path(repo_dir, "metadata", "workflow_script_index.csv")),
  paste0("- ", file.path(repo_dir, "metadata", "script_capture_note.csv")),
  paste0("- ", file.path(repo_dir, "metadata", "repository_file_manifest.csv")),
  paste0("- ", file.path(repo_dir, "docs", "provenance", "locked_module_provenance_table.csv")),
  paste0("- ", file.path(repo_dir, "docs", "provenance", "reproducibility_checklist.csv")),
  paste0("- ", file.path(repo_dir, "docs", "claim_boundary", "claim_boundary_statement.csv")),
  paste0("- ", file.path(out_table_dir, "12K_FINAL_handoff_to_12L_journal_cover_letter.csv")),
  "",
  "Claim boundary:",
  "- Repository package frames the study as a computational transcriptomic prioritisation framework.",
  "- Raw public data are not redistributed; users should obtain raw data from original public repositories.",
  "- Candidate signatures remain candidate transcriptomic marker signatures, not validated clinical biomarkers.",
  "- ML remains marker-rule-derived transcriptomic prioritisation audit, not clinical prediction.",
  "",
  paste0("Decision: ", decision_value)
)
report_file <- file.path(out_text_dir, "12K_FINAL_github_repository_package_report.txt")
writeLines(report_lines, report_file)
cat("[12K FINAL] Wrote:", report_file, "\n")

cat("\n[12K FINAL] Completed GitHub / repository package refresh.\n")
cat("[12K FINAL] Repository package directory:", repo_dir, "\n")
cat("[12K FINAL] Repository files:", n_repo_files, "\n")
cat("[12K FINAL] Copied locked source files:", n_copied, "\n")
cat("[12K FINAL] Copy missing or failed:", n_copy_missing, "\n")
cat("[12K FINAL] Workflow scripts indexed:", n_scripts_indexed, "\n")
cat("[12K FINAL] Workflow scripts copied to repo:", n_scripts_copied, "\n")
cat("[12K FINAL] Current sourced script:", current_script, "\n")
cat("[12K FINAL] Data/source manifest rows:", nrow(source_manifest), "\n")
cat("[12K FINAL] Claim-boundary audit pass:", n_claim_pass, "/", n_claim_total, "\n")
cat("[12K FINAL] 12L handoff ready rows:", n_12l_ready, "/", n_12l_total, "\n")
cat("[12K FINAL] Figures written: 5\n")
cat("[12K FINAL] Decision:", decision_value, "\n")
cat("[12K FINAL] Output tables:", out_table_dir, "\n")
cat("[12K FINAL] Output figs  :", out_fig_dir, "\n")
cat("[12K FINAL] Output text  :", out_text_dir, "\n")
cat("[12K FINAL] Repo package :", repo_dir, "\n")
cat("[12K FINAL] Next         : review 12K repository package and PDFs; if accepted, proceed to 12L journal / cover-letter refresh.\n")
