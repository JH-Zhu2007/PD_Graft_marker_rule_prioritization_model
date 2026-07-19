
# ============================================================
# 12B FINAL COMPLETE STANDALONE - NATURE COLOR
# Final figure plan refresh for DA neuron / graft-related
# transcriptomic cell-state prioritisation framework
#
# User rule respected:
#   - Complete standalone script for 12B
#   - Does NOT read any previous 12B output
#   - Does NOT use table-only patch logic
#   - May read locked upstream outputs as formal inputs:
#       10A-10P, 11A-11J, 12A
#   - Rebuilds all 12B tables, report text and PDFs
#   - No internet
#   - No 00-10P rerun
#
# Scientific boundary:
#   - Final figure planning and manuscript figure architecture only
#   - Evidence-anchored transcriptomic prioritisation framework
#   - Candidate transcriptomic marker signatures only
#   - marker-rule-derived prioritization model audit only
#   - No clinical prediction
#   - No diagnostic/prognostic biomarker validation
#   - No causal graft efficacy/safety claim
#   - No true anatomical projection or barcode-lineage proof
# ============================================================

cat("\n[12B FINAL] Starting final figure plan refresh...\n")
cat("[12B FINAL] Mode: complete standalone 12B rebuild; no previous 12B dependency; no internet; no 00-10P rerun.\n")
cat("[12B FINAL] Inputs allowed: locked upstream 10A-10P, 11A-11J and 12A outputs.\n")
cat("[12B FINAL] Claim boundary: final figure architecture only; no clinical prediction or validated biomarker claim.\n")
cat("[12B FINAL] Figure style: Nature-style clean publication layout.\n")

graphics.off()

# ------------------------- project paths -------------------------
project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")
figure_root <- file.path(project_root, "04_figures")
text_root <- file.path(project_root, "09_manuscript")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "12B_final_figure_plan_refresh_FINAL_COMPLETE_STANDALONE"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "12B_final_figure_plan_refresh_FINAL_COMPLETE_STANDALONE_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "12B_final_figure_plan_refresh_FINAL_COMPLETE_STANDALONE"
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
  cat("[12B FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[12B FINAL] Wrote:", file_value, "\n")
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
    cat("[12B FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
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
if (!dir.exists(table_root)) stop("[12B FINAL] Missing table root: ", table_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
table_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(table_info$size) & table_info$size > 0 & table_info$size < 120 * 1024 * 1024]

# Hard rule: do not read previous 12B output
all_table_files <- all_table_files[!grepl("12B_final_figure_plan_refresh", all_table_files, ignore.case = TRUE)]

all_figure_files <- character(0)
if (dir.exists(figure_root)) {
  all_figure_files <- list.files(figure_root, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
}
all_figure_files <- all_figure_files[!grepl("12B_final_figure_plan_refresh", all_figure_files, ignore.case = TRUE)]

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

# ------------------------- read locked 12A planning inputs -------------------------
file_12a_figure_plan <- first_existing_file(c(
  file.path(table_root, "12A_final_storyline_refresh_FINAL_COMPLETE_STANDALONE", "12A_FINAL_12B_ready_figure_refresh_plan.csv"),
  find_files_all_terms(all_table_files, c("12a", "12b_ready_figure_refresh_plan"), max_n = 10)
))

file_12a_storyline <- first_existing_file(c(
  file.path(table_root, "12A_final_storyline_refresh_FINAL_COMPLETE_STANDALONE", "12A_FINAL_storyline_refresh_table.csv"),
  find_files_all_terms(all_table_files, c("12a", "storyline_refresh_table"), max_n = 10)
))

file_12a_claim <- first_existing_file(c(
  file.path(table_root, "12A_final_storyline_refresh_FINAL_COMPLETE_STANDALONE", "12A_FINAL_claim_boundary_table.csv"),
  find_files_all_terms(all_table_files, c("12a", "claim_boundary_table"), max_n = 10)
))

figure_plan_12a <- read_table_safe(file_12a_figure_plan)
storyline_12a <- read_table_safe(file_12a_storyline)
claim_12a <- read_table_safe(file_12a_claim)

input_audit_df <- data.frame(
  input_name = c("12A_figure_refresh_plan", "12A_storyline_table", "12A_claim_boundary_table"),
  detected = c(file_12a_figure_plan != "", file_12a_storyline != "", file_12a_claim != ""),
  file_path = c(file_12a_figure_plan, file_12a_storyline, file_12a_claim),
  rows_loaded = c(nrow(figure_plan_12a), nrow(storyline_12a), nrow(claim_12a)),
  allowed_as_locked_upstream_input = c(TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
write_csv_safe(input_audit_df, file.path(out_table_dir, "12B_FINAL_locked_12A_input_audit.csv"))

# ------------------------- final main figure plan -------------------------
main_figure_plan <- data.frame(
  figure_id = c("Main Fig 1", "Main Fig 2", "Main Fig 3", "Main Fig 4", "Main Fig 5"),
  final_title = c(
    "Source-traceable framework for DA/graft transcriptomic cell-state prioritisation",
    "Core marker-rule-derived prioritisation and DA/graft identity evidence",
    "Temporal maturation and module co-variation architecture",
    "External/proxy evidence expansion and risk-context boundaries",
    "Integrated umbrella evidence tier and ML audit"
  ),
  primary_story_role = c(
    "establish framework and source traceability",
    "show core prioritisation signal and DA/graft identity modules",
    "show pseudotime and coordinated module structure",
    "show preclinical/projection/state proxy support plus risk/genetic context",
    "show final integrated evidence tier and marker-rule-derived prioritization model audit"
  ),
  required_panels = c(
    "A workflow schematic; B dataset/source map; C domain-boundary audit; D source-panel traceability",
    "A prioritisation overview; B DA identity module heatmap/score; C high-priority state landscape; D candidate signature summary",
    "A multi-timepoint pseudotime; B maturation trajectory; C module-correlation heatmap; D identity-risk axis landscape",
    "A preclinical marker support; B projection proxy support; C state-level proxy support; D risk/safety-context and limited genetic-context summary",
    "A 11H umbrella evidence-tier summary; B candidate marker signature matrix; C ROC/PR audit; D final claim-boundary compact summary"
  ),
  locked_input_modules = c(
    "10C;10D;10G;10H;10P;11A;11B",
    "09C;10L;11H",
    "10K;11I",
    "11C;11D;11E;11F;11G",
    "11H;11J;12A"
  ),
  main_claim_allowed = c(
    "source-traceable computational framework",
    "marker-rule-derived transcriptomic prioritisation and candidate DA/graft identity states",
    "pseudotime and module co-variation support",
    "multi-layer proxy and risk/genetic-context support",
    "umbrella evidence-tier and marker-rule-derived prioritization model audit support"
  ),
  claim_boundary = c(
    "not a clinical PD/graft prediction dataset",
    "not a clinical graft outcome predictor",
    "not lineage tracing or causal maturation proof",
    "not anatomical projection, clinical efficacy or safety validation",
    "not diagnostic/prognostic biomarker validation"
  ),
  priority_for_main_text = c("essential", "essential", "essential", "essential", "essential"),
  stringsAsFactors = FALSE
)

write_csv_safe(main_figure_plan, file.path(out_table_dir, "12B_FINAL_main_figure_plan.csv"))
write_tsv_safe(main_figure_plan, file.path(out_table_dir, "12B_FINAL_main_figure_plan.tsv"))

# ------------------------- final supplementary figure plan -------------------------
supp_figure_plan <- data.frame(
  figure_id = paste0("Supplement Fig S", 1:10),
  final_title = c(
    "Dataset-domain audit and source manifest",
    "Detailed source-panel dependency map",
    "QC and object-level processing audit",
    "Preclinical marker support detail",
    "Projection-associated molecular competence detail",
    "State-level proxy and lineage-audit boundary detail",
    "Risk/safety-context perturbation proxy detail",
    "Limited PD genetic-context support detail",
    "Candidate transcriptomic marker signature detail",
    "ML audit, ROC/PR and feature-transparency detail"
  ),
  primary_story_role = c(
    "support source/domain validity",
    "support figure traceability",
    "support reproducibility",
    "detail 11C",
    "detail 11F",
    "detail 11E",
    "detail 11D",
    "detail 11G",
    "detail 11H marker signature",
    "detail 11J"
  ),
  locked_input_modules = c(
    "10G;10H;11A;11B",
    "10C;10P;12A",
    "02A;02B;03A;03B;10E",
    "11C",
    "11F",
    "11E",
    "11D",
    "11G",
    "11H",
    "11J"
  ),
  claim_boundary = c(
    "domain/source boundaries only",
    "traceability only",
    "technical reproducibility only",
    "preclinical marker proxy only",
    "projection molecular competence, not anatomical tracing",
    "state-level proxy, not barcode lineage tracing",
    "risk-context proxy, not clinical safety prediction",
    "limited genetic-context background only",
    "candidate transcriptomic marker signatures only",
    "marker-rule-derived prioritization model audit only"
  ),
  priority = c("high", "high", "medium", "high", "high", "medium", "high", "medium", "high", "high"),
  stringsAsFactors = FALSE
)

write_csv_safe(supp_figure_plan, file.path(out_table_dir, "12B_FINAL_supplementary_figure_plan.csv"))
write_tsv_safe(supp_figure_plan, file.path(out_table_dir, "12B_FINAL_supplementary_figure_plan.tsv"))

# ------------------------- figure-to-module dependency matrix -------------------------
all_figures <- c(main_figure_plan$figure_id, supp_figure_plan$figure_id)
dependency_modules <- c("09C", "10C", "10D", "10G", "10H", "10K", "10L", "10P", "11A", "11B", "11C", "11D", "11E", "11F", "11G", "11H", "11I", "11J", "12A")
dependency_matrix <- matrix(0, nrow = length(all_figures), ncol = length(dependency_modules))
rownames(dependency_matrix) <- all_figures
colnames(dependency_matrix) <- dependency_modules

figure_module_text <- c(main_figure_plan$locked_input_modules, supp_figure_plan$locked_input_modules)
for (idx_fig in seq_along(all_figures)) {
  text_now <- toupper(figure_module_text[idx_fig])
  for (idx_mod in seq_along(dependency_modules)) {
    if (grepl(dependency_modules[idx_mod], text_now, fixed = TRUE)) dependency_matrix[idx_fig, idx_mod] <- 1
  }
}

dependency_df <- data.frame(figure_id = rownames(dependency_matrix), dependency_matrix, check.names = FALSE, stringsAsFactors = FALSE)
write_csv_safe(dependency_df, file.path(out_table_dir, "12B_FINAL_figure_to_locked_module_dependency_matrix.csv"))

# ------------------------- source figure availability audit -------------------------
source_audit_list <- list()
for (idx_fig in seq_along(all_figures)) {
  fig_id_now <- all_figures[idx_fig]
  module_text_now <- figure_module_text[idx_fig]
  modules_now <- unlist(strsplit(module_text_now, ";", fixed = TRUE), use.names = FALSE)
  modules_now <- clean_space(modules_now)
  modules_now <- modules_now[modules_now != ""]
  detected_pdf_count <- 0
  detected_table_count <- 0
  representative_pdf <- ""
  representative_table <- ""
  for (module_now in modules_now) {
    table_hits <- find_files_all_terms(all_table_files, c(tolower(module_now)), max_n = 20)
    pdf_hits <- find_files_all_terms(all_figure_files, c(tolower(module_now)), max_n = 20)
    detected_pdf_count <- detected_pdf_count + length(pdf_hits)
    detected_table_count <- detected_table_count + length(table_hits)
    if (representative_pdf == "" && length(pdf_hits) > 0) representative_pdf <- pdf_hits[1]
    if (representative_table == "" && length(table_hits) > 0) representative_table <- table_hits[1]
  }
  readiness <- "ready"
  if (detected_table_count < 1) readiness <- "needs_table_review"
  if (detected_pdf_count < 1) readiness <- "needs_figure_review"
  if (detected_table_count < 1 && detected_pdf_count < 1) readiness <- "needs_source_review"
  source_audit_list[[length(source_audit_list) + 1]] <- data.frame(
    figure_id = fig_id_now,
    locked_input_modules = module_text_now,
    detected_upstream_table_files = detected_table_count,
    detected_upstream_pdf_figures = detected_pdf_count,
    representative_table = representative_table,
    representative_pdf = representative_pdf,
    readiness_for_12C_source_lock = readiness,
    stringsAsFactors = FALSE
  )
}
source_audit_df <- safe_bind_rows(source_audit_list)
write_csv_safe(source_audit_df, file.path(out_table_dir, "12B_FINAL_source_availability_and_12C_readiness_audit.csv"))

# ------------------------- claim placement and caption notes -------------------------
claim_placement_df <- data.frame(
  claim_type = c(
    "framework framing",
    "marker-rule-derived prioritisation",
    "pseudotime/module coordination",
    "preclinical marker support",
    "projection-associated competence",
    "state-level proxy support",
    "risk/safety-context proxy",
    "limited PD genetic context",
    "umbrella evidence tier",
    "candidate transcriptomic marker signatures",
    "marker-rule-derived prioritization model audit"
  ),
  primary_figure = c(
    "Main Fig 1",
    "Main Fig 2",
    "Main Fig 3",
    "Main Fig 4",
    "Main Fig 4",
    "Main Fig 4",
    "Main Fig 4 / Supplement Fig S7",
    "Main Fig 4 / Supplement Fig S8",
    "Main Fig 5",
    "Main Fig 5 / Supplement Fig S9",
    "Main Fig 5 / Supplement Fig S10"
  ),
  recommended_caption_language = c(
    "source-traceable transcriptomic prioritisation framework",
    "marker-rule-derived transcriptomic prioritization model",
    "graph-based pseudotime and module-score co-variation support",
    "preclinical marker/outcome-associated transcriptomic support",
    "projection-associated molecular competence proxy support",
    "state-level proxy support after barcode/lineage audit",
    "risk/safety-context module support and perturbation proxy",
    "limited PD genetic-context support",
    "umbrella evidence-tier support",
    "candidate transcriptomic marker signatures",
    "internal marker-rule-derived ROC/PR audit"
  ),
  avoid_language = c(
    "clinical PD prediction model",
    "clinical graft outcome predictor",
    "lineage tracing or causal maturation proof",
    "clinical efficacy validation",
    "anatomical projection tracing proof",
    "barcode-lineage claim",
    "clinical safety prediction",
    "genetic validation of PD causality",
    "clinical prediction tier",
    "validated diagnostic/prognostic biomarker",
    "clinical model performance"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(claim_placement_df, file.path(out_table_dir, "12B_FINAL_claim_placement_and_caption_language.csv"))

# ------------------------- final 12C handoff table -------------------------
handoff_12c_df <- data.frame(
  handoff_item = c(
    "main figure plan",
    "supplementary figure plan",
    "figure-module dependency matrix",
    "source availability audit",
    "claim placement table",
    "caption language table"
  ),
  file_path = c(
    file.path(out_table_dir, "12B_FINAL_main_figure_plan.csv"),
    file.path(out_table_dir, "12B_FINAL_supplementary_figure_plan.csv"),
    file.path(out_table_dir, "12B_FINAL_figure_to_locked_module_dependency_matrix.csv"),
    file.path(out_table_dir, "12B_FINAL_source_availability_and_12C_readiness_audit.csv"),
    file.path(out_table_dir, "12B_FINAL_claim_placement_and_caption_language.csv"),
    file.path(out_table_dir, "12B_FINAL_claim_placement_and_caption_language.csv")
  ),
  role_in_12C = c(
    "defines final main figure architecture",
    "defines final supplement structure",
    "controls source-panel lock mapping",
    "identifies missing upstream source files before 12C",
    "guards against overclaiming in figure plan",
    "feeds figure legends and final captions"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(handoff_12c_df, file.path(out_table_dir, "12B_FINAL_handoff_to_12C_source_panel_lock.csv"))

# ------------------------- figure outputs -------------------------
# FigA final figure architecture
fig_a <- open_pdf_safe("12B_FINAL_FigA_final_figure_architecture.pdf", 12.4, 6.8)
new_canvas()
draw_title("Final figure architecture", "Five main figures and ten supplementary figures for the final manuscript package.")

# main figure row
main_x <- seq(0.10, 0.90, length.out = nrow(main_figure_plan))
main_y <- 0.68
for (idx_row in seq_len(nrow(main_figure_plan))) {
  color_now <- figure_color(main_figure_plan$figure_id[idx_row])
  rect(main_x[idx_row] - 0.075, main_y - 0.065, main_x[idx_row] + 0.075, main_y + 0.065,
       col = blend_color(nature_palette$white, color_now, 0.22), border = color_now, lwd = 0.9)
  text(main_x[idx_row], main_y + 0.025, main_figure_plan$figure_id[idx_row], cex = 0.48, font = 2, col = nature_palette$ink)
  short_role <- c("Framework", "Core model", "Maturation", "Proxy evidence", "Integration")[idx_row]
  text(main_x[idx_row], main_y - 0.020, short_role, cex = 0.38, col = nature_palette$muted)
  if (idx_row < nrow(main_figure_plan)) {
    arrows(main_x[idx_row] + 0.083, main_y, main_x[idx_row + 1] - 0.083, main_y,
           length = 0.045, angle = 20, col = nature_palette$muted, lwd = 0.65)
  }
}
text(0.10, 0.80, "Main figures", cex = 0.56, font = 2, adj = c(0, 0.5), col = nature_palette$ink)

# supplementary row
supp_x <- seq(0.08, 0.92, length.out = nrow(supp_figure_plan))
supp_y <- 0.34
for (idx_row in seq_len(nrow(supp_figure_plan))) {
  color_now <- figure_color(supp_figure_plan$figure_id[idx_row])
  symbols(supp_x[idx_row], supp_y, circles = 0.026, inches = FALSE, add = TRUE,
          bg = color_now, fg = nature_palette$border, lwd = 0.45)
  text(supp_x[idx_row], supp_y - 0.055, supp_figure_plan$figure_id[idx_row], cex = 0.36, adj = c(0.5, 0.5), col = nature_palette$ink)
}
text(0.10, 0.45, "Supplementary figures", cex = 0.56, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
text(0.50, 0.15, "12B defines figure architecture only; 12C will lock exact source panels.", cex = 0.50, col = nature_palette$muted)
dev.off()
cat("[12B FINAL] Wrote figure:", fig_a, "\n")

# FigB main figure panel map
fig_b <- open_pdf_safe("12B_FINAL_FigB_main_figure_panel_map.pdf", 13.0, 7.2)
new_canvas()
draw_title("Main figure panel map", "Panel-level plan for final main figures; source locking follows in 12C.")

plot_main <- main_figure_plan
y_values <- seq(0.78, 0.24, length.out = nrow(plot_main))
for (idx_row in seq_len(nrow(plot_main))) {
  yy <- y_values[idx_row]
  color_now <- figure_color(plot_main$figure_id[idx_row])
  rect(0.06, yy - 0.045, 0.94, yy + 0.045,
       col = blend_color(nature_palette$white, color_now, 0.15), border = color_now, lwd = 0.75)
  text(0.08, yy + 0.012, plot_main$figure_id[idx_row], cex = 0.45, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.20, yy + 0.012, plot_main$final_title[idx_row], cex = 0.39, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.20, yy - 0.018, plot_main$locked_input_modules[idx_row], cex = 0.34, adj = c(0, 0.5), col = nature_palette$muted)
  text(0.70, yy - 0.018, plot_main$main_claim_allowed[idx_row], cex = 0.34, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12B FINAL] Wrote figure:", fig_b, "\n")

# FigC supplementary figure structure
fig_c <- open_pdf_safe("12B_FINAL_FigC_supplementary_figure_structure.pdf", 12.4, 7.2)
new_canvas()
draw_title("Supplementary figure structure", "Supplementary figures preserve source detail, proxy boundaries and ML audit transparency.")

plot_supp <- supp_figure_plan
y_values <- seq(0.82, 0.18, length.out = nrow(plot_supp))
for (idx_row in seq_len(nrow(plot_supp))) {
  yy <- y_values[idx_row]
  color_now <- figure_color(plot_supp$figure_id[idx_row])
  rect(0.08, yy - 0.023, 0.30, yy + 0.023, col = color_now, border = nature_palette$border, lwd = 0.35)
  text(0.19, yy, plot_supp$figure_id[idx_row], cex = 0.40, font = 2, col = nature_palette$white)
  text(0.32, yy, plot_supp$final_title[idx_row], cex = 0.40, adj = c(0, 0.5), col = nature_palette$ink)
  text(0.79, yy, plot_supp$claim_boundary[idx_row], cex = 0.32, adj = c(0, 0.5), col = nature_palette$muted)
}
dev.off()
cat("[12B FINAL] Wrote figure:", fig_c, "\n")

# FigD source readiness and claim boundary summary
fig_d <- open_pdf_safe("12B_FINAL_FigD_source_readiness_and_claim_boundary_summary.pdf", 11.8, 6.7)
new_canvas()
draw_title("12C source-lock readiness summary", "Final figure plan is ready for source-panel locking if all planned figures have upstream tables/figures.")

readiness_table <- as.data.frame(table(source_audit_df$readiness_for_12C_source_lock), stringsAsFactors = FALSE)
colnames(readiness_table) <- c("readiness", "figure_count")
if (nrow(readiness_table) < 1) {
  readiness_table <- data.frame(readiness = "no_readiness_rows", figure_count = 0, stringsAsFactors = FALSE)
}
readiness_table <- readiness_table[order(readiness_table$figure_count, decreasing = TRUE), , drop = FALSE]

bar_x0 <- 0.36
bar_x1 <- 0.78
y_values <- seq(0.76, 0.52, length.out = nrow(readiness_table))
max_count <- max(safe_num(readiness_table$figure_count), na.rm = TRUE)
if (!is.finite(max_count) || max_count <= 0) max_count <- 1
for (idx_row in seq_len(nrow(readiness_table))) {
  yy <- y_values[idx_row]
  count_now <- safe_num(readiness_table$figure_count[idx_row])
  width_now <- count_now / max_count
  color_now <- nature_palette$teal
  if (grepl("needs", readiness_table$readiness[idx_row], ignore.case = TRUE)) color_now <- nature_palette$orange
  text(bar_x0 - 0.018, yy, readiness_table$readiness[idx_row], cex = 0.52, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.025, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.025,
       col = color_now, border = nature_palette$border, lwd = 0.42)
  text(bar_x0 + width_now * (bar_x1 - bar_x0) + 0.012, yy, as.character(count_now), cex = 0.50, adj = c(0, 0.5), col = nature_palette$ink)
}

claim_counts <- data.frame(
  claim_group = c("allowed transcriptomic/prioritisation claims", "prohibited clinical/causal claims"),
  count = c(11, 11),
  color = c(nature_palette$teal, nature_palette$orange),
  stringsAsFactors = FALSE
)
y_claim <- c(0.34, 0.25)
for (idx_row in seq_len(nrow(claim_counts))) {
  rect(0.26, y_claim[idx_row] - 0.022, 0.60, y_claim[idx_row] + 0.022,
       col = claim_counts$color[idx_row], border = nature_palette$border, lwd = 0.35)
  text(0.62, y_claim[idx_row], claim_counts$claim_group[idx_row], cex = 0.46, adj = c(0, 0.5), col = nature_palette$ink)
}
text(0.50, 0.12, "12B output is a plan. Final data panels will be source-locked and assembled in 12C-12D.", cex = 0.48, col = nature_palette$muted)
dev.off()
cat("[12B FINAL] Wrote figure:", fig_d, "\n")

# ------------------------- execution summary and report -------------------------
summary_df <- data.frame(
  item = c(
    "main_figures_planned",
    "supplementary_figures_planned",
    "total_figures_planned",
    "dependency_modules_tracked",
    "figures_ready_for_12C_source_lock",
    "figures_needing_source_review",
    "claim_placement_rows",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(nrow(main_figure_plan)),
    as.character(nrow(supp_figure_plan)),
    as.character(length(all_figures)),
    as.character(length(dependency_modules)),
    as.character(sum(source_audit_df$readiness_for_12C_source_lock == "ready")),
    as.character(sum(source_audit_df$readiness_for_12C_source_lock != "ready")),
    as.character(nrow(claim_placement_df)),
    "4",
    "INPUT_READY_FOR_12C_FINAL_SOURCE_PANEL_LOCK_REFRESH"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "12B_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "12B_FINAL_execution_summary.tsv"))

report_lines <- c(
  "12B FINAL report",
  "================",
  "Module: final figure plan refresh",
  "Mode: complete standalone 12B rebuild; no previous 12B output dependency; no internet; no 00-10P rerun.",
  "Allowed upstream inputs: locked 10A-10P, 11A-11J and 12A outputs.",
  "",
  paste0("Main figures planned: ", nrow(main_figure_plan)),
  paste0("Supplementary figures planned: ", nrow(supp_figure_plan)),
  paste0("Total figures planned: ", length(all_figures)),
  paste0("Dependency modules tracked: ", length(dependency_modules)),
  paste0("Figures ready for 12C source lock: ", sum(source_audit_df$readiness_for_12C_source_lock == "ready")),
  paste0("Figures needing source review: ", sum(source_audit_df$readiness_for_12C_source_lock != "ready")),
  "",
  "Main figure architecture:",
  "- Fig 1: framework and source traceability.",
  "- Fig 2: core prioritisation and DA/graft identity.",
  "- Fig 3: pseudotime and module co-variation.",
  "- Fig 4: preclinical/projection/state proxy evidence and risk/genetic-context boundaries.",
  "- Fig 5: integrated umbrella evidence tier and marker-rule-derived prioritization model audit.",
  "",
  "Core claim boundary:",
  "- All figures should use transcriptomic prioritisation language.",
  "- Candidate marker signatures should not be described as clinical biomarkers.",
  "- Projection and state-level evidence should be described as proxy/molecular competence support only.",
  "- ML audit should not be described as clinical prediction.",
  "",
  "Decision: INPUT_READY_FOR_12C_FINAL_SOURCE_PANEL_LOCK_REFRESH"
)
report_file <- file.path(out_text_dir, "12B_FINAL_figure_plan_refresh_report.txt")
writeLines(report_lines, report_file)
cat("[12B FINAL] Wrote:", report_file, "\n")

cat("\n[12B FINAL] Completed final figure plan refresh.\n")
cat("[12B FINAL] Main figures planned:", nrow(main_figure_plan), "\n")
cat("[12B FINAL] Supplementary figures planned:", nrow(supp_figure_plan), "\n")
cat("[12B FINAL] Total figures planned:", length(all_figures), "\n")
cat("[12B FINAL] Dependency modules tracked:", length(dependency_modules), "\n")
cat("[12B FINAL] Figures ready for 12C source lock:", sum(source_audit_df$readiness_for_12C_source_lock == "ready"), "\n")
cat("[12B FINAL] Figures needing source review:", sum(source_audit_df$readiness_for_12C_source_lock != "ready"), "\n")
cat("[12B FINAL] Claim placement rows:", nrow(claim_placement_df), "\n")
cat("[12B FINAL] Figures written: 4\n")
cat("[12B FINAL] Decision: INPUT_READY_FOR_12C_FINAL_SOURCE_PANEL_LOCK_REFRESH\n")
cat("[12B FINAL] Output tables:", out_table_dir, "\n")
cat("[12B FINAL] Output figs  :", out_fig_dir, "\n")
cat("[12B FINAL] Output text  :", out_text_dir, "\n")
cat("[12B FINAL] Next         : review 12B FINAL PDFs; if accepted, proceed to 12C final source-panel lock refresh.\n")
