
# ============================================================
# 12C FINAL COMPLETE STANDALONE - NATURE COLOR
# Final source-panel lock refresh for DA neuron / graft-related
# transcriptomic cell-state prioritisation framework
#
# User rule respected:
#   - Complete standalone script for 12C
#   - Does NOT read any previous 12C output
#   - Does NOT use table-only patch logic
#   - May read locked upstream outputs as formal inputs:
#       10A-10P, 11A-11J, 12A, 12B
#   - Rebuilds all 12C source-lock tables, report text and PDFs
#   - No internet
#   - No 00-10P rerun
#
# Scientific boundary:
#   - Source-panel lock / figure-source traceability only
#   - Evidence-anchored transcriptomic prioritisation framework
#   - Candidate transcriptomic marker signatures only
#   - marker-rule-derived prioritization model audit only
#   - No clinical prediction
#   - No diagnostic/prognostic biomarker validation
#   - No causal graft efficacy/safety claim
#   - No true anatomical projection or barcode-lineage proof
# ============================================================

cat("\n[12C FINAL] Starting final source-panel lock refresh...\n")
cat("[12C FINAL] Mode: complete standalone 12C rebuild; no previous 12C dependency; no internet; no 00-10P rerun.\n")
cat("[12C FINAL] Inputs allowed: locked upstream 10A-10P, 11A-11J, 12A and 12B outputs.\n")
cat("[12C FINAL] Claim boundary: source-panel traceability only; no clinical prediction or validated biomarker claim.\n")
cat("[12C FINAL] Figure style: Nature-style clean publication layout.\n")

graphics.off()

# ------------------------- project paths -------------------------
project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")
figure_root <- file.path(project_root, "04_figures")
text_root <- file.path(project_root, "09_manuscript")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "12C_final_source_panel_lock_refresh_FINAL_COMPLETE_STANDALONE"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "12C_final_source_panel_lock_refresh_FINAL_COMPLETE_STANDALONE_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "12C_final_source_panel_lock_refresh_FINAL_COMPLETE_STANDALONE"
)

dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_text_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------- safe helper functions -------------------------
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
  cat("[12C FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[12C FINAL] Wrote:", file_value, "\n")
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

open_pdf_safe <- function(filename, width_value = 10, height_value = 6) {
  file_primary <- file.path(out_fig_dir, filename)
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
    file_alt <- file.path(out_fig_dir, alt_name)
    grDevices::pdf(
      file_alt,
      width = width_value,
      height = height_value,
      onefile = FALSE,
      useDingbats = FALSE,
      paper = "special"
    )
    cat("[12C FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
    return(file_alt)
  }
  file_primary
}

new_canvas <- function() {
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", xpd = FALSE)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
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

nature_continuous_color <- function(value_obj, max_obj, low_color = nature_palette$pale_blue, high_color = nature_palette$navy) {
  value_num <- safe_num(value_obj)
  max_num <- max(safe_num(max_obj), na.rm = TRUE)
  if (!is.finite(max_num) || max_num <= 0) max_num <- 1
  fraction_value <- value_num / max_num
  fraction_value[!is.finite(fraction_value)] <- 0
  fraction_value[fraction_value < 0] <- 0
  fraction_value[fraction_value > 1] <- 1
  blend_color(low_color, high_color, fraction_value)
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
  text(0.5, 0.965, title_value, cex = 0.98, font = 2, adj = c(0.5, 0.5), col = nature_palette$ink)
  if (nchar(subtitle_value) > 0) {
    text(0.5, 0.928, subtitle_value, cex = 0.50, col = nature_palette$muted, adj = c(0.5, 0.5))
  }
}

# ------------------------- file discovery -------------------------
if (!dir.exists(table_root)) stop("[12C FINAL] Missing table root: ", table_root, call. = FALSE)
if (!dir.exists(figure_root)) stop("[12C FINAL] Missing figure root: ", figure_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
table_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(table_info$size) & table_info$size > 0 & table_info$size < 150 * 1024 * 1024]

# Hard rule: do not read previous 12C output
all_table_files <- all_table_files[!grepl("12C_final_source_panel_lock_refresh", all_table_files, ignore.case = TRUE)]

all_figure_files <- list.files(figure_root, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
if (length(all_figure_files) < 1) all_figure_files <- character(0)
figure_info <- file.info(all_figure_files)
all_figure_files <- all_figure_files[is.finite(figure_info$size) & figure_info$size > 0]

# Hard rule: do not read previous 12C figures
all_figure_files <- all_figure_files[!grepl("12C_final_source_panel_lock_refresh", all_figure_files, ignore.case = TRUE)]

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

find_files_any_terms <- function(file_values, term_values, max_n = 20) {
  if (length(file_values) < 1) return(character(0))
  term_values <- tolower(safe_chr(term_values))
  path_lower <- tolower(file_values)
  keep_vec <- rep(FALSE, length(file_values))
  for (term_value in term_values) keep_vec <- keep_vec | grepl(term_value, path_lower, fixed = TRUE)
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

split_modules <- function(module_text_value) {
  module_values <- unlist(strsplit(safe_chr(module_text_value), ";", fixed = TRUE), use.names = FALSE)
  module_values <- clean_space(module_values)
  module_values <- module_values[module_values != ""]
  unique(module_values)
}

# ------------------------- read locked 12B planning inputs -------------------------
file_12b_main <- first_existing_file(c(
  file.path(table_root, "12B_final_figure_plan_refresh_FINAL_COMPLETE_STANDALONE", "12B_FINAL_main_figure_plan.csv"),
  find_files_all_terms(all_table_files, c("12b", "main_figure_plan"), max_n = 10)
))
file_12b_supp <- first_existing_file(c(
  file.path(table_root, "12B_final_figure_plan_refresh_FINAL_COMPLETE_STANDALONE", "12B_FINAL_supplementary_figure_plan.csv"),
  find_files_all_terms(all_table_files, c("12b", "supplementary_figure_plan"), max_n = 10)
))
file_12b_dependency <- first_existing_file(c(
  file.path(table_root, "12B_final_figure_plan_refresh_FINAL_COMPLETE_STANDALONE", "12B_FINAL_figure_to_locked_module_dependency_matrix.csv"),
  find_files_all_terms(all_table_files, c("12b", "figure_to_locked_module_dependency_matrix"), max_n = 10)
))
file_12b_claim <- first_existing_file(c(
  file.path(table_root, "12B_final_figure_plan_refresh_FINAL_COMPLETE_STANDALONE", "12B_FINAL_claim_placement_and_caption_language.csv"),
  find_files_all_terms(all_table_files, c("12b", "claim_placement_and_caption_language"), max_n = 10)
))

main_plan <- read_table_safe(file_12b_main)
supp_plan <- read_table_safe(file_12b_supp)
dependency_12b <- read_table_safe(file_12b_dependency)
claim_12b <- read_table_safe(file_12b_claim)

if (nrow(main_plan) < 1) stop("[12C FINAL] Missing 12B main figure plan.", call. = FALSE)
if (nrow(supp_plan) < 1) stop("[12C FINAL] Missing 12B supplementary figure plan.", call. = FALSE)

input_audit_df <- data.frame(
  input_name = c("12B_main_figure_plan", "12B_supplementary_figure_plan", "12B_dependency_matrix", "12B_claim_language"),
  detected = c(file_12b_main != "", file_12b_supp != "", file_12b_dependency != "", file_12b_claim != ""),
  file_path = c(file_12b_main, file_12b_supp, file_12b_dependency, file_12b_claim),
  rows_loaded = c(nrow(main_plan), nrow(supp_plan), nrow(dependency_12b), nrow(claim_12b)),
  allowed_as_locked_upstream_input = c(TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
write_csv_safe(input_audit_df, file.path(out_table_dir, "12C_FINAL_locked_12B_input_audit.csv"))

# ------------------------- source selection helpers -------------------------
priority_keywords_by_module <- data.frame(
  module_id = c(
    "09C", "10C", "10D", "10G", "10H", "10K", "10L", "10P",
    "11A", "11B", "11C", "11D", "11E", "11F", "11G", "11H", "11I", "11J", "12A", "12B"
  ),
  priority_terms = c(
    "09C;ML;model;priority;feature;prediction",
    "10C;source;lock;panel;manifest",
    "10D;multipanel;assembly;figure",
    "10G;dataset;domain;reframe;audit",
    "10H;core;dataset;definition",
    "10K;pseudotime;multi;timepoint;maturation",
    "10L;signature;priority;demo;user",
    "10P;source;panel;package",
    "11A;evidence;audit;dataset",
    "11B;download;import;manual;input",
    "11C;preclinical;outcome;marker",
    "11D;survival;stress;CRISPR;risk",
    "11E;state;barcode;lineage;proxy",
    "11F;projection;tracing;competence",
    "11G;GWAS;genetic;PD",
    "11H;integrated;umbrella;marker;candidate",
    "11I;module;correlation;identity;risk",
    "11J;ML;ROC;PR;feature;audit",
    "12A;storyline;claim;roadmap",
    "12B;figure;plan;architecture"
  ),
  stringsAsFactors = FALSE
)

get_priority_terms <- function(module_id_value) {
  idx_hit <- which(priority_keywords_by_module$module_id == module_id_value)
  if (length(idx_hit) < 1) return(module_id_value)
  priority_keywords_by_module$priority_terms[idx_hit[1]]
}

score_candidate_file <- function(file_value, module_id_value, role_text_value) {
  file_lower <- tolower(file_value)
  base_lower <- tolower(basename(file_value))
  score_value <- 0
  if (grepl(tolower(module_id_value), file_lower, fixed = TRUE)) score_value <- score_value + 10
  term_values <- unlist(strsplit(get_priority_terms(module_id_value), ";", fixed = TRUE), use.names = FALSE)
  for (term_now in term_values) {
    if (term_now == "") next
    if (grepl(tolower(term_now), base_lower, fixed = TRUE)) score_value <- score_value + 3
    if (grepl(tolower(term_now), file_lower, fixed = TRUE)) score_value <- score_value + 1
  }
  role_terms <- unlist(strsplit(clean_space(role_text_value), "[ ;,/()_-]+"), use.names = FALSE)
  role_terms <- role_terms[nchar(role_terms) >= 4]
  role_terms <- role_terms[seq_len(min(length(role_terms), 8))]
  for (term_now in role_terms) {
    if (grepl(tolower(term_now), base_lower, fixed = TRUE)) score_value <- score_value + 1
  }
  if (grepl("final|locked|complete|publication", base_lower)) score_value <- score_value + 2
  if (grepl("alt_|old|backup|tmp|temp|failed", base_lower)) score_value <- score_value - 5
  score_value
}

select_source_files_for_module <- function(module_id_value, role_text_value, max_pdf = 4, max_table = 4) {
  pdf_hits <- find_files_all_terms(all_figure_files, c(tolower(module_id_value)), max_n = 200)
  table_hits <- find_files_all_terms(all_table_files, c(tolower(module_id_value)), max_n = 200)

  if (length(pdf_hits) > 0) {
    pdf_score <- vapply(pdf_hits, function(file_now) score_candidate_file(file_now, module_id_value, role_text_value), numeric(1))
    pdf_info <- file.info(pdf_hits)
    pdf_hits <- pdf_hits[order(pdf_score, pdf_info$mtime, decreasing = TRUE)]
  }
  if (length(table_hits) > 0) {
    table_score <- vapply(table_hits, function(file_now) score_candidate_file(file_now, module_id_value, role_text_value), numeric(1))
    table_info <- file.info(table_hits)
    table_hits <- table_hits[order(table_score, table_info$mtime, decreasing = TRUE)]
  }

  list(
    pdf = unique(pdf_hits)[seq_len(min(max_pdf, length(unique(pdf_hits))))],
    table = unique(table_hits)[seq_len(min(max_table, length(unique(table_hits))))]
  )
}

# ------------------------- panel-level source lock -------------------------
panel_letters <- LETTERS[1:8]

make_panel_rows_for_figure <- function(plan_row, figure_type_value) {
  figure_id_now <- clean_space(plan_row$figure_id)
  title_now <- if ("final_title" %in% colnames(plan_row)) clean_space(plan_row$final_title) else clean_space(plan_row$figure_role)
  role_now <- if ("primary_story_role" %in% colnames(plan_row)) clean_space(plan_row$primary_story_role) else clean_space(plan_row$primary_story_role)
  module_text_now <- clean_space(plan_row$locked_input_modules)
  claim_boundary_now <- if ("claim_boundary" %in% colnames(plan_row)) clean_space(plan_row$claim_boundary) else ""
  allowed_claim_now <- if ("main_claim_allowed" %in% colnames(plan_row)) clean_space(plan_row$main_claim_allowed) else ""
  if (allowed_claim_now == "" && "figure_claim_boundary" %in% colnames(plan_row)) allowed_claim_now <- clean_space(plan_row$figure_claim_boundary)

  module_values <- split_modules(module_text_now)
  if (length(module_values) < 1) module_values <- "unassigned"

  if (figure_type_value == "main") {
    panel_text_raw <- if ("required_panels" %in% colnames(plan_row)) clean_space(plan_row$required_panels) else ""
    panel_values <- unlist(strsplit(panel_text_raw, ";", fixed = TRUE), use.names = FALSE)
    panel_values <- clean_space(panel_values)
    panel_values <- panel_values[panel_values != ""]
    if (length(panel_values) < 1) {
      panel_values <- c("A overview", "B source evidence", "C quantitative support", "D claim boundary")
    }
  } else {
    panel_values <- c("A source detail", "B quantitative detail", "C claim-boundary detail")
  }

  out_rows <- list()
  for (idx_panel in seq_along(panel_values)) {
    panel_label_now <- panel_letters[idx_panel]
    panel_desc_now <- panel_values[idx_panel]
    module_now <- module_values[((idx_panel - 1) %% length(module_values)) + 1]
    sources_now <- select_source_files_for_module(module_now, paste(title_now, role_now, panel_desc_now), max_pdf = 3, max_table = 3)
    primary_pdf_now <- ifelse(length(sources_now$pdf) > 0, sources_now$pdf[1], "")
    primary_table_now <- ifelse(length(sources_now$table) > 0, sources_now$table[1], "")
    status_now <- "locked"
    if (primary_pdf_now == "" && primary_table_now == "") status_now <- "needs_source_review"
    if (primary_pdf_now == "" && primary_table_now != "") status_now <- "table_only_source_locked"
    if (primary_pdf_now != "" && primary_table_now == "") status_now <- "figure_only_source_locked"

    out_rows[[length(out_rows) + 1]] <- data.frame(
      figure_type = figure_type_value,
      figure_id = figure_id_now,
      figure_title = title_now,
      panel_id = paste0(figure_id_now, panel_label_now),
      panel_label = panel_label_now,
      planned_panel_content = panel_desc_now,
      primary_locked_module = module_now,
      all_locked_input_modules = module_text_now,
      primary_source_pdf = primary_pdf_now,
      backup_source_pdfs = paste(sources_now$pdf, collapse = ";"),
      primary_source_table = primary_table_now,
      backup_source_tables = paste(sources_now$table, collapse = ";"),
      source_lock_status = status_now,
      allowed_claim = allowed_claim_now,
      claim_boundary = claim_boundary_now,
      prohibited_claim = "clinical prediction; validated clinical biomarker; causal graft efficacy/safety; true anatomical projection; barcode-lineage claim",
      downstream_use = "12D final panel package assembly",
      stringsAsFactors = FALSE
    )
  }
  safe_bind_rows(out_rows)
}

panel_lock_list <- list()
for (idx_row in seq_len(nrow(main_plan))) {
  panel_lock_list[[length(panel_lock_list) + 1]] <- make_panel_rows_for_figure(main_plan[idx_row, , drop = FALSE], "main")
}
for (idx_row in seq_len(nrow(supp_plan))) {
  panel_lock_list[[length(panel_lock_list) + 1]] <- make_panel_rows_for_figure(supp_plan[idx_row, , drop = FALSE], "supplement")
}
panel_lock_df <- safe_bind_rows(panel_lock_list)

write_csv_safe(panel_lock_df, file.path(out_table_dir, "12C_FINAL_panel_level_source_lock.csv"))
write_tsv_safe(panel_lock_df, file.path(out_table_dir, "12C_FINAL_panel_level_source_lock.tsv"))

main_panel_lock_df <- panel_lock_df[panel_lock_df$figure_type == "main", , drop = FALSE]
supp_panel_lock_df <- panel_lock_df[panel_lock_df$figure_type == "supplement", , drop = FALSE]
write_csv_safe(main_panel_lock_df, file.path(out_table_dir, "12C_FINAL_main_figure_source_panel_lock.csv"))
write_csv_safe(supp_panel_lock_df, file.path(out_table_dir, "12C_FINAL_supplementary_figure_source_panel_lock.csv"))

# ------------------------- source file manifest -------------------------
source_file_values <- unique(c(
  panel_lock_df$primary_source_pdf,
  unlist(strsplit(paste(panel_lock_df$backup_source_pdfs, collapse = ";"), ";", fixed = TRUE), use.names = FALSE),
  panel_lock_df$primary_source_table,
  unlist(strsplit(paste(panel_lock_df$backup_source_tables, collapse = ";"), ";", fixed = TRUE), use.names = FALSE)
))
source_file_values <- clean_space(source_file_values)
source_file_values <- source_file_values[source_file_values != ""]
source_file_values <- unique(source_file_values)

manifest_list <- list()
if (length(source_file_values) > 0) {
  for (idx_source in seq_along(source_file_values)) {
    source_now <- source_file_values[idx_source]
    file_info_now <- file.info(source_now)
    module_match <- regmatches(source_now, gregexpr("(09C|10[A-Z]|11[A-Z]|12[A-Z])", source_now, ignore.case = TRUE))[[1]]
    module_match <- unique(toupper(module_match))
    used_panels <- panel_lock_df$panel_id[
      panel_lock_df$primary_source_pdf == source_now |
        grepl(source_now, panel_lock_df$backup_source_pdfs, fixed = TRUE) |
        panel_lock_df$primary_source_table == source_now |
        grepl(source_now, panel_lock_df$backup_source_tables, fixed = TRUE)
    ]
    manifest_list[[length(manifest_list) + 1]] <- data.frame(
      source_file = source_now,
      file_exists = file.exists(source_now),
      file_type = tolower(tools::file_ext(source_now)),
      file_size_bytes = ifelse(file.exists(source_now), as.numeric(file_info_now$size), NA_real_),
      modified_time = ifelse(file.exists(source_now), as.character(file_info_now$mtime), ""),
      matched_modules = paste(module_match, collapse = ";"),
      used_by_panels = paste(unique(used_panels), collapse = ";"),
      n_used_by_panels = length(unique(used_panels)),
      source_role = ifelse(grepl("\\.pdf$", source_now, ignore.case = TRUE), "source_panel_pdf", "source_table"),
      stringsAsFactors = FALSE
    )
  }
}
source_manifest_df <- safe_bind_rows(manifest_list)
write_csv_safe(source_manifest_df, file.path(out_table_dir, "12C_FINAL_source_file_manifest.csv"))
write_tsv_safe(source_manifest_df, file.path(out_table_dir, "12C_FINAL_source_file_manifest.tsv"))

# ------------------------- figure-level lock summary -------------------------
figure_ids_all <- unique(panel_lock_df$figure_id)
figure_lock_list <- list()
for (idx_fig in seq_along(figure_ids_all)) {
  fig_now <- figure_ids_all[idx_fig]
  sub_fig <- panel_lock_df[panel_lock_df$figure_id == fig_now, , drop = FALSE]
  n_panels_now <- nrow(sub_fig)
  n_locked_now <- sum(sub_fig$source_lock_status %in% c("locked", "table_only_source_locked", "figure_only_source_locked"))
  n_full_locked_now <- sum(sub_fig$source_lock_status == "locked")
  n_review_now <- sum(sub_fig$source_lock_status == "needs_source_review")
  figure_status_now <- "ready_for_12D"
  if (n_review_now > 0) figure_status_now <- "needs_source_review_before_12D"
  if (n_full_locked_now < n_panels_now && n_review_now == 0) figure_status_now <- "partial_source_lock_ready_for_review"
  figure_lock_list[[length(figure_lock_list) + 1]] <- data.frame(
    figure_id = fig_now,
    figure_type = sub_fig$figure_type[1],
    n_planned_panels = n_panels_now,
    n_panels_with_any_source = n_locked_now,
    n_panels_with_pdf_and_table = n_full_locked_now,
    n_panels_needing_source_review = n_review_now,
    source_lock_status_for_12D = figure_status_now,
    locked_modules = paste(unique(unlist(strsplit(paste(sub_fig$all_locked_input_modules, collapse = ";"), ";", fixed = TRUE), use.names = FALSE)), collapse = ";"),
    stringsAsFactors = FALSE
  )
}
figure_lock_df <- safe_bind_rows(figure_lock_list)
write_csv_safe(figure_lock_df, file.path(out_table_dir, "12C_FINAL_figure_level_source_lock_summary.csv"))

# ------------------------- claim boundary source lock -------------------------
claim_source_lock_df <- data.frame(
  source_lock_rule = c(
    "framework panels",
    "prioritisation panels",
    "pseudotime/module correlation panels",
    "preclinical/projection/state proxy panels",
    "risk/genetic-context panels",
    "integrated evidence-tier panels",
    "ML audit panels",
    "candidate marker signature panels"
  ),
  allowed_language = c(
    "source-traceable transcriptomic prioritisation framework",
    "marker-rule-derived transcriptomic prioritisation",
    "pseudotime support and module co-variation",
    "proxy support and molecular competence support",
    "risk-context proxy and limited genetic-context background",
    "umbrella evidence-tier support",
    "internal marker-rule-derived ROC/PR audit",
    "candidate transcriptomic marker signatures"
  ),
  prohibited_language = c(
    "clinical PD/graft prediction model",
    "clinical graft outcome predictor",
    "lineage tracing or causal maturation proof",
    "anatomical projection tracing or clinical efficacy validation",
    "clinical safety prediction or genetic causality proof",
    "clinical prediction tier",
    "clinical performance or patient prediction",
    "validated diagnostic/prognostic biomarker"
  ),
  locked_reference_modules = c(
    "10G;10H;11A;11B;12A;12B",
    "09C;10L;11H",
    "10K;11I",
    "11C;11E;11F",
    "11D;11G",
    "11H",
    "11J",
    "11H;11J"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(claim_source_lock_df, file.path(out_table_dir, "12C_FINAL_claim_boundary_source_lock.csv"))

# ------------------------- 12D handoff -------------------------
handoff_12d_df <- data.frame(
  handoff_item = c(
    "panel-level source lock",
    "main figure source lock",
    "supplementary figure source lock",
    "figure-level source summary",
    "source file manifest",
    "claim-boundary source lock"
  ),
  file_path = c(
    file.path(out_table_dir, "12C_FINAL_panel_level_source_lock.csv"),
    file.path(out_table_dir, "12C_FINAL_main_figure_source_panel_lock.csv"),
    file.path(out_table_dir, "12C_FINAL_supplementary_figure_source_panel_lock.csv"),
    file.path(out_table_dir, "12C_FINAL_figure_level_source_lock_summary.csv"),
    file.path(out_table_dir, "12C_FINAL_source_file_manifest.csv"),
    file.path(out_table_dir, "12C_FINAL_claim_boundary_source_lock.csv")
  ),
  role_in_12D = c(
    "controls final panel package assembly",
    "defines source panels for 5 main figures",
    "defines source panels for 10 supplementary figures",
    "flags figure-level readiness",
    "provides exact file provenance",
    "prevents overclaiming during final panel package assembly"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(handoff_12d_df, file.path(out_table_dir, "12C_FINAL_handoff_to_12D_panel_package.csv"))

# ------------------------- figures -------------------------
# FigA source lock overview
fig_a <- open_pdf_safe("12C_FINAL_FigA_source_lock_overview.pdf", 11.8, 6.6)
new_canvas()
draw_title("Final source-panel lock overview", "Panel-level sources are locked from upstream modules; no previous 12C output was used.")

overview_df <- data.frame(
  label = c("Total planned figures", "Main figures", "Supplementary figures", "Planned panels", "Panels with source", "Figures ready for 12D"),
  value = c(
    length(figure_ids_all),
    sum(figure_lock_df$figure_type == "main"),
    sum(figure_lock_df$figure_type == "supplement"),
    nrow(panel_lock_df),
    sum(panel_lock_df$source_lock_status %in% c("locked", "table_only_source_locked", "figure_only_source_locked")),
    sum(figure_lock_df$source_lock_status_for_12D %in% c("ready_for_12D", "partial_source_lock_ready_for_review"))
  ),
  family = c("all", "main", "supp", "panel", "source", "ready"),
  stringsAsFactors = FALSE
)
max_overview <- max(safe_num(overview_df$value), na.rm = TRUE)
if (!is.finite(max_overview) || max_overview <= 0) max_overview <- 1
bar_x0 <- 0.35
bar_x1 <- 0.78
y_values <- seq(0.78, 0.33, length.out = nrow(overview_df))
for (idx_row in seq_len(nrow(overview_df))) {
  yy <- y_values[idx_row]
  count_now <- safe_num(overview_df$value[idx_row])
  width_now <- count_now / max_overview
  color_now <- nature_palette$navy
  if (overview_df$family[idx_row] == "main") color_now <- nature_palette$blue
  if (overview_df$family[idx_row] == "supp") color_now <- nature_palette$purple
  if (overview_df$family[idx_row] == "source") color_now <- nature_palette$teal
  if (overview_df$family[idx_row] == "ready") color_now <- nature_palette$teal
  text(bar_x0 - 0.018, yy, overview_df$label[idx_row], cex = 0.58, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.026, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.026,
       col = color_now, border = nature_palette$border, lwd = 0.45)
  text(bar_x0 + width_now * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_now), cex = 0.54, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.16, "12C locks source provenance. 12D will assemble final panels from these locked sources.", cex = 0.50, col = nature_palette$muted)
dev.off()
cat("[12C FINAL] Wrote figure:", fig_a, "\n")

# FigB main figure source matrix
fig_b <- open_pdf_safe("12C_FINAL_FigB_main_figure_source_lock_matrix.pdf", 12.6, 6.8)
new_canvas()
draw_title("Main figure source-lock matrix", "Rows are main figures; columns are locked upstream modules used as source inputs.")

main_figures <- unique(main_panel_lock_df$figure_id)
main_modules <- unique(unlist(strsplit(paste(main_panel_lock_df$all_locked_input_modules, collapse = ";"), ";", fixed = TRUE), use.names = FALSE))
main_modules <- clean_space(main_modules)
main_modules <- main_modules[main_modules != ""]
main_modules <- unique(main_modules)
if (length(main_modules) < 1) main_modules <- "none"

mat_main <- matrix(0, nrow = length(main_figures), ncol = length(main_modules))
rownames(mat_main) <- main_figures
colnames(mat_main) <- main_modules
for (idx_fig in seq_along(main_figures)) {
  sub_fig <- main_panel_lock_df[main_panel_lock_df$figure_id == main_figures[idx_fig], , drop = FALSE]
  module_values <- unique(unlist(strsplit(paste(sub_fig$all_locked_input_modules, collapse = ";"), ";", fixed = TRUE), use.names = FALSE))
  module_values <- clean_space(module_values)
  module_values <- module_values[module_values != ""]
  for (idx_mod in seq_along(main_modules)) {
    mat_main[idx_fig, idx_mod] <- as.integer(main_modules[idx_mod] %in% module_values)
  }
}
hm_x0 <- 0.23
hm_x1 <- 0.88
hm_y0 <- 0.22
hm_y1 <- 0.78
cell_w <- (hm_x1 - hm_x0) / ncol(mat_main)
cell_h <- (hm_y1 - hm_y0) / nrow(mat_main)
for (idx_row in seq_len(nrow(mat_main))) {
  for (idx_col in seq_len(ncol(mat_main))) {
    color_now <- ifelse(mat_main[idx_row, idx_col] > 0, figure_color(rownames(mat_main)[idx_row]), nature_palette$pale_blue)
    rect(
      hm_x0 + (idx_col - 1) * cell_w,
      hm_y1 - idx_row * cell_h,
      hm_x0 + idx_col * cell_w,
      hm_y1 - (idx_row - 1) * cell_h,
      col = color_now,
      border = nature_palette$white,
      lwd = 0.35
    )
  }
}
rect(hm_x0, hm_y0, hm_x1, hm_y1, border = nature_palette$border, lwd = 0.65)
for (idx_row in seq_len(nrow(mat_main))) {
  yy <- hm_y1 - (idx_row - 0.5) * cell_h
  text(hm_x0 - 0.012, yy, rownames(mat_main)[idx_row], cex = 0.46, adj = c(1, 0.5), col = nature_palette$ink)
}
for (idx_col in seq_len(ncol(mat_main))) {
  xx <- hm_x0 + (idx_col - 0.5) * cell_w
  text(xx, 0.145, colnames(mat_main)[idx_col], cex = 0.38, srt = 90, adj = c(0.5, 0.5), col = nature_palette$ink)
}
dev.off()
cat("[12C FINAL] Wrote figure:", fig_b, "\n")

# FigC supplementary source matrix
fig_c <- open_pdf_safe("12C_FINAL_FigC_supplementary_source_lock_matrix.pdf", 12.6, 7.4)
new_canvas()
draw_title("Supplementary figure source-lock matrix", "Rows are supplementary figures; source-detail modules are preserved for 12D assembly.")

supp_figures <- unique(supp_panel_lock_df$figure_id)
supp_modules <- unique(unlist(strsplit(paste(supp_panel_lock_df$all_locked_input_modules, collapse = ";"), ";", fixed = TRUE), use.names = FALSE))
supp_modules <- clean_space(supp_modules)
supp_modules <- supp_modules[supp_modules != ""]
supp_modules <- unique(supp_modules)
if (length(supp_modules) < 1) supp_modules <- "none"

mat_supp <- matrix(0, nrow = length(supp_figures), ncol = length(supp_modules))
rownames(mat_supp) <- supp_figures
colnames(mat_supp) <- supp_modules
for (idx_fig in seq_along(supp_figures)) {
  sub_fig <- supp_panel_lock_df[supp_panel_lock_df$figure_id == supp_figures[idx_fig], , drop = FALSE]
  module_values <- unique(unlist(strsplit(paste(sub_fig$all_locked_input_modules, collapse = ";"), ";", fixed = TRUE), use.names = FALSE))
  module_values <- clean_space(module_values)
  module_values <- module_values[module_values != ""]
  for (idx_mod in seq_along(supp_modules)) {
    mat_supp[idx_fig, idx_mod] <- as.integer(supp_modules[idx_mod] %in% module_values)
  }
}
hm_x0 <- 0.25
hm_x1 <- 0.90
hm_y0 <- 0.20
hm_y1 <- 0.82
cell_w <- (hm_x1 - hm_x0) / ncol(mat_supp)
cell_h <- (hm_y1 - hm_y0) / nrow(mat_supp)
for (idx_row in seq_len(nrow(mat_supp))) {
  for (idx_col in seq_len(ncol(mat_supp))) {
    color_now <- ifelse(mat_supp[idx_row, idx_col] > 0, figure_color(rownames(mat_supp)[idx_row]), nature_palette$pale_blue)
    rect(
      hm_x0 + (idx_col - 1) * cell_w,
      hm_y1 - idx_row * cell_h,
      hm_x0 + idx_col * cell_w,
      hm_y1 - (idx_row - 1) * cell_h,
      col = color_now,
      border = nature_palette$white,
      lwd = 0.35
    )
  }
}
rect(hm_x0, hm_y0, hm_x1, hm_y1, border = nature_palette$border, lwd = 0.65)
for (idx_row in seq_len(nrow(mat_supp))) {
  yy <- hm_y1 - (idx_row - 0.5) * cell_h
  text(hm_x0 - 0.012, yy, rownames(mat_supp)[idx_row], cex = 0.38, adj = c(1, 0.5), col = nature_palette$ink)
}
for (idx_col in seq_len(ncol(mat_supp))) {
  xx <- hm_x0 + (idx_col - 0.5) * cell_w
  text(xx, 0.130, colnames(mat_supp)[idx_col], cex = 0.34, srt = 90, adj = c(0.5, 0.5), col = nature_palette$ink)
}
dev.off()
cat("[12C FINAL] Wrote figure:", fig_c, "\n")

# FigD 12D handoff readiness
fig_d <- open_pdf_safe("12C_FINAL_FigD_12D_handoff_readiness_summary.pdf", 11.8, 6.5)
new_canvas()
draw_title("12D handoff readiness summary", "Source-locked panels are ready for final panel-package assembly.")

status_table <- as.data.frame(table(figure_lock_df$source_lock_status_for_12D), stringsAsFactors = FALSE)
colnames(status_table) <- c("status", "figure_count")
status_table <- status_table[order(status_table$figure_count, decreasing = TRUE), , drop = FALSE]
bar_x0 <- 0.38
bar_x1 <- 0.78
y_values <- seq(0.76, 0.56, length.out = max(1, nrow(status_table)))
max_count <- max(safe_num(status_table$figure_count), na.rm = TRUE)
if (!is.finite(max_count) || max_count <= 0) max_count <- 1
for (idx_row in seq_len(nrow(status_table))) {
  yy <- y_values[idx_row]
  count_now <- safe_num(status_table$figure_count[idx_row])
  width_now <- count_now / max_count
  color_now <- ifelse(grepl("review", status_table$status[idx_row], ignore.case = TRUE), nature_palette$orange, nature_palette$teal)
  text(bar_x0 - 0.018, yy, status_table$status[idx_row], cex = 0.50, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.026, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.026,
       col = color_now, border = nature_palette$border, lwd = 0.42)
  text(bar_x0 + width_now * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_now), cex = 0.50, adj = c(0, 0.5), col = nature_palette$ink)
}

handoff_items <- c("panel-level lock", "source manifest", "figure-level summary", "claim-boundary lock", "12D handoff table")
handoff_y <- seq(0.40, 0.22, length.out = length(handoff_items))
for (idx_item in seq_along(handoff_items)) {
  symbols(0.33, handoff_y[idx_item], circles = 0.014, inches = FALSE, add = TRUE,
          bg = nature_palette$navy, fg = nature_palette$border, lwd = 0.35)
  text(0.36, handoff_y[idx_item], handoff_items[idx_item], cex = 0.46, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.10, "Next module: 12D final panel-package generation from locked source-panel manifest.", cex = 0.48, col = nature_palette$muted)
dev.off()
cat("[12C FINAL] Wrote figure:", fig_d, "\n")

# ------------------------- execution summary and report -------------------------
n_figures_ready <- sum(figure_lock_df$source_lock_status_for_12D %in% c("ready_for_12D", "partial_source_lock_ready_for_review"))
n_figures_review <- sum(grepl("needs", figure_lock_df$source_lock_status_for_12D, ignore.case = TRUE))
n_panels_source <- sum(panel_lock_df$source_lock_status %in% c("locked", "table_only_source_locked", "figure_only_source_locked"))
n_panels_review <- sum(panel_lock_df$source_lock_status == "needs_source_review")

summary_df <- data.frame(
  item = c(
    "total_figures_source_locked",
    "main_figures_source_locked",
    "supplementary_figures_source_locked",
    "panel_rows_locked",
    "panels_with_any_source",
    "panels_needing_source_review",
    "unique_source_files_manifested",
    "figures_ready_for_12D",
    "figures_needing_source_review",
    "claim_boundary_rules",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(length(figure_ids_all)),
    as.character(sum(figure_lock_df$figure_type == "main")),
    as.character(sum(figure_lock_df$figure_type == "supplement")),
    as.character(nrow(panel_lock_df)),
    as.character(n_panels_source),
    as.character(n_panels_review),
    as.character(nrow(source_manifest_df)),
    as.character(n_figures_ready),
    as.character(n_figures_review),
    as.character(nrow(claim_source_lock_df)),
    "4",
    "INPUT_READY_FOR_12D_FINAL_PANEL_PACKAGE_GENERATION"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "12C_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "12C_FINAL_execution_summary.tsv"))

report_lines <- c(
  "12C FINAL report",
  "================",
  "Module: final source-panel lock refresh",
  "Mode: complete standalone 12C rebuild; no previous 12C output dependency; no internet; no 00-10P rerun.",
  "Allowed upstream inputs: locked 10A-10P, 11A-11J, 12A and 12B outputs.",
  "",
  paste0("Total figures source-locked: ", length(figure_ids_all)),
  paste0("Main figures source-locked: ", sum(figure_lock_df$figure_type == "main")),
  paste0("Supplementary figures source-locked: ", sum(figure_lock_df$figure_type == "supplement")),
  paste0("Panel rows locked: ", nrow(panel_lock_df)),
  paste0("Panels with any source: ", n_panels_source),
  paste0("Panels needing source review: ", n_panels_review),
  paste0("Unique source files manifested: ", nrow(source_manifest_df)),
  paste0("Figures ready for 12D: ", n_figures_ready),
  paste0("Figures needing source review: ", n_figures_review),
  "",
  "Main 12D inputs:",
  paste0("- ", file.path(out_table_dir, "12C_FINAL_panel_level_source_lock.csv")),
  paste0("- ", file.path(out_table_dir, "12C_FINAL_main_figure_source_panel_lock.csv")),
  paste0("- ", file.path(out_table_dir, "12C_FINAL_supplementary_figure_source_panel_lock.csv")),
  paste0("- ", file.path(out_table_dir, "12C_FINAL_source_file_manifest.csv")),
  paste0("- ", file.path(out_table_dir, "12C_FINAL_claim_boundary_source_lock.csv")),
  "",
  "Claim boundary:",
  "- Source-panel lock supports traceability and final panel assembly only.",
  "- Do not convert proxy/source locked panels into clinical prediction, clinical biomarker, causal efficacy/safety, anatomical projection or barcode-lineage claims.",
  "",
  "Decision: INPUT_READY_FOR_12D_FINAL_PANEL_PACKAGE_GENERATION"
)
report_file <- file.path(out_text_dir, "12C_FINAL_source_panel_lock_report.txt")
writeLines(report_lines, report_file)
cat("[12C FINAL] Wrote:", report_file, "\n")

cat("\n[12C FINAL] Completed final source-panel lock refresh.\n")
cat("[12C FINAL] Total figures source-locked:", length(figure_ids_all), "\n")
cat("[12C FINAL] Main figures source-locked:", sum(figure_lock_df$figure_type == "main"), "\n")
cat("[12C FINAL] Supplementary figures source-locked:", sum(figure_lock_df$figure_type == "supplement"), "\n")
cat("[12C FINAL] Panel rows locked:", nrow(panel_lock_df), "\n")
cat("[12C FINAL] Panels with any source:", n_panels_source, "\n")
cat("[12C FINAL] Panels needing source review:", n_panels_review, "\n")
cat("[12C FINAL] Unique source files manifested:", nrow(source_manifest_df), "\n")
cat("[12C FINAL] Figures ready for 12D:", n_figures_ready, "\n")
cat("[12C FINAL] Figures needing source review:", n_figures_review, "\n")
cat("[12C FINAL] Claim boundary rules:", nrow(claim_source_lock_df), "\n")
cat("[12C FINAL] Figures written: 4\n")
cat("[12C FINAL] Decision: INPUT_READY_FOR_12D_FINAL_PANEL_PACKAGE_GENERATION\n")
cat("[12C FINAL] Output tables:", out_table_dir, "\n")
cat("[12C FINAL] Output figs  :", out_fig_dir, "\n")
cat("[12C FINAL] Output text  :", out_text_dir, "\n")
cat("[12C FINAL] Next         : review 12C FINAL PDFs; if accepted, proceed to 12D final panel-package generation.\n")
