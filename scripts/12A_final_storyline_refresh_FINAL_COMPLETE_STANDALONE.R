
# ============================================================
# 12A FINAL COMPLETE STANDALONE - NATURE COLOR
# Final storyline refresh for DA neuron / graft-related
# transcriptomic cell-state prioritisation framework
#
# User rule respected:
#   - Complete standalone script for 12A
#   - Does NOT read any previous 12A output
#   - Does NOT use table-only patch logic
#   - May read locked upstream outputs as formal inputs:
#       10A-10P and 11A-11J locked module outputs
#   - Rebuilds all 12A tables, report text and PDFs
#   - No internet
#   - No 00-10P rerun
#
# Scientific boundary:
#   - Final storyline refresh / manuscript integration only
#   - Evidence-anchored transcriptomic prioritisation framework
#   - Candidate transcriptomic marker signatures only
#   - marker-rule-derived prioritization model audit only
#   - No clinical prediction
#   - No diagnostic/prognostic biomarker validation
#   - No causal graft efficacy/safety claim
#   - No true anatomical projection or barcode-lineage proof
# ============================================================

cat("\n[12A FINAL] Starting final storyline refresh...\n")
cat("[12A FINAL] Mode: complete standalone 12A rebuild; no previous 12A dependency; no internet; no 00-10P rerun.\n")
cat("[12A FINAL] Inputs allowed: locked upstream 10A-10P and 11A-11J outputs.\n")
cat("[12A FINAL] Claim boundary: final manuscript storyline only; no clinical prediction or validated biomarker claim.\n")
cat("[12A FINAL] Figure style: Nature-style clean publication layout.\n")

graphics.off()

# ------------------------- project paths -------------------------
project_root <- "D:/PD_Graft_Project"
table_root <- file.path(project_root, "03_tables")
figure_root <- file.path(project_root, "04_figures")
text_root <- file.path(project_root, "09_manuscript")

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "12A_final_storyline_refresh_FINAL_COMPLETE_STANDALONE"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "12A_final_storyline_refresh_FINAL_COMPLETE_STANDALONE_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "12A_final_storyline_refresh_FINAL_COMPLETE_STANDALONE"
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
  cat("[12A FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[12A FINAL] Wrote:", file_value, "\n")
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
    cat("[12A FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
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

module_color <- function(module_id_value) {
  module_text <- toupper(safe_chr(module_id_value))
  out_colors <- rep(nature_palette$navy, length(module_text))
  out_colors[grepl("^10|BASELINE|FIGURE|SOURCE", module_text)] <- nature_palette$blue
  out_colors[grepl("^11A|^11B|DATASET|EVIDENCE", module_text)] <- nature_palette$teal
  out_colors[grepl("^11C|PRECLINICAL", module_text)] <- nature_palette$teal
  out_colors[grepl("^11D|RISK|SAFETY|STRESS|CRISPR", module_text)] <- nature_palette$orange
  out_colors[grepl("^11E|STATE|LINEAGE", module_text)] <- nature_palette$purple
  out_colors[grepl("^11F|PROJECTION", module_text)] <- nature_palette$blue
  out_colors[grepl("^11G|GENETIC|GWAS", module_text)] <- nature_palette$gold
  out_colors[grepl("^11H|INTEGRATED|UMBRELLA", module_text)] <- nature_palette$navy
  out_colors[grepl("^11I|CORRELATION", module_text)] <- nature_palette$red
  out_colors[grepl("^11J|ML", module_text)] <- nature_palette$teal
  out_colors
}

draw_title <- function(title_value, subtitle_value = "") {
  text(0.5, 0.965, title_value, cex = 0.98, font = 2, adj = c(0.5, 0.5), col = nature_palette$ink)
  if (nchar(subtitle_value) > 0) {
    text(0.5, 0.928, subtitle_value, cex = 0.50, col = nature_palette$muted, adj = c(0.5, 0.5))
  }
}

# ------------------------- file discovery -------------------------
if (!dir.exists(table_root)) stop("[12A FINAL] Missing table root: ", table_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
table_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(table_info$size) & table_info$size > 0 & table_info$size < 120 * 1024 * 1024]

# Hard rule: do not read previous 12A output
all_table_files <- all_table_files[!grepl("12A_final_storyline_refresh", all_table_files, ignore.case = TRUE)]

all_figure_files <- character(0)
if (dir.exists(figure_root)) {
  all_figure_files <- list.files(figure_root, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
}
all_figure_files <- all_figure_files[!grepl("12A_final_storyline_refresh", all_figure_files, ignore.case = TRUE)]

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

# ------------------------- locked module catalog -------------------------
module_catalog <- data.frame(
  module_id = c(
    "10A", "10B", "10C", "10D", "10E", "10F", "10G", "10H", "10I", "10J", "10K", "10L", "10M", "10N", "10O", "10P",
    "11A", "11B", "11C", "11D", "11E", "11F", "11G", "11H", "11I", "11J"
  ),
  module_name = c(
    "figure/storyline planning",
    "locked manuscript draft",
    "figure source lock",
    "multipanel assembly",
    "consistency audit",
    "legends/reference map",
    "dataset-domain reframing audit",
    "core dataset definition",
    "demo object selection",
    "D8 pseudotime pilot",
    "multi-timepoint pseudotime",
    "user scRNA signature-priority inference demo",
    "post-10L planning",
    "figure/claim update planning",
    "source panel planning",
    "source panel package",
    "new evidence audit",
    "new evidence input import/manual check",
    "preclinical graft-outcome marker support",
    "survival/stress perturbation CRISPR proxy support",
    "GSE200610 state-level proxy support",
    "projection molecular competence proxy support",
    "limited PD genetic-context support",
    "umbrella evidence tier + candidate marker signatures",
    "module-score correlation",
    "ML audit / ROC-PR / feature transparency"
  ),
  locked_status = c(
    "LOCKED", "LOCKED", "LOCKED", "LOCKED", "LOCKED", "LOCKED", "LOCKED", "LOCKED", "LOCKED", "SUPPLEMENT_DIAGNOSTIC", "LOCKED", "LOCKED", "LOCKED", "LOCKED", "LOCKED", "LOCKED",
    "LOCKED", "LOCKED", "LOCKED", "LOCKED", "LOCKED", "LOCKED", "LOCKED_LIMITED", "LOCKED", "LOCKED", "LOCKED_CONSERVATIVE"
  ),
  evidence_role = c(
    "storyline scaffold",
    "manuscript text scaffold",
    "source panel traceability",
    "assembled figure package",
    "consistency control",
    "legend/reference map",
    "dataset/domain boundary",
    "core input definition",
    "demo selection",
    "diagnostic pseudotime pilot",
    "temporal differentiation support",
    "user-data application demo",
    "planning support",
    "claim update planning",
    "source package planning",
    "source package",
    "new evidence expansion",
    "input completeness audit",
    "preclinical marker/outcome-associated support",
    "risk/safety-context perturbation proxy",
    "state-level proxy after lineage audit",
    "projection-associated molecular competence support",
    "conservative PD genetic context",
    "main integrated evidence tier",
    "module co-variation support",
    "marker-rule-derived prioritization model audit support"
  ),
  interpretation_strength = c(
    "supporting", "supporting", "supporting", "supporting", "supporting", "supporting", "supporting", "supporting", "supporting", "diagnostic", "moderate", "supporting", "supporting", "supporting", "supporting", "supporting",
    "supporting", "supporting", "moderate", "moderate", "limited_proxy", "moderate", "limited", "strong_integrative", "moderate", "moderate_performance_limited_feature"
  ),
  stringsAsFactors = FALSE
)

# Add detected table/figure counts by module
module_table_counts <- integer(nrow(module_catalog))
module_figure_counts <- integer(nrow(module_catalog))
representative_tables <- character(nrow(module_catalog))
representative_figures <- character(nrow(module_catalog))

for (idx_module in seq_len(nrow(module_catalog))) {
  module_id_now <- module_catalog$module_id[idx_module]
  hits_table <- find_files_all_terms(all_table_files, c(tolower(module_id_now)), max_n = 1000)
  hits_fig <- find_files_all_terms(all_figure_files, c(tolower(module_id_now)), max_n = 1000)
  module_table_counts[idx_module] <- length(hits_table)
  module_figure_counts[idx_module] <- length(hits_fig)
  representative_tables[idx_module] <- ifelse(length(hits_table) > 0, hits_table[1], "")
  representative_figures[idx_module] <- ifelse(length(hits_fig) > 0, hits_fig[1], "")
}

module_catalog$detected_table_files <- module_table_counts
module_catalog$detected_pdf_figures <- module_figure_counts
module_catalog$representative_table <- representative_tables
module_catalog$representative_figure <- representative_figures

write_csv_safe(module_catalog, file.path(out_table_dir, "12A_FINAL_locked_module_status_and_input_audit.csv"))
write_tsv_safe(module_catalog, file.path(out_table_dir, "12A_FINAL_locked_module_status_and_input_audit.tsv"))

# ------------------------- extract key upstream metrics -------------------------
extract_execution_summary_value <- function(file_values, item_terms) {
  file_values <- safe_chr(file_values)
  item_terms <- tolower(safe_chr(item_terms))
  for (file_value in file_values) {
    if (!file.exists(file_value)) next
    data_value <- read_table_safe(file_value)
    if (!is.data.frame(data_value) || nrow(data_value) < 1) next
    col_names <- colnames(data_value)
    item_col <- ""
    value_col <- ""
    for (candidate_col in col_names) {
      if (tolower(candidate_col) %in% c("item", "metric", "name", "key")) item_col <- candidate_col
      if (tolower(candidate_col) %in% c("value", "result", "count")) value_col <- candidate_col
    }
    if (item_col == "" || value_col == "") next
    item_text <- tolower(safe_chr(data_value[[item_col]]))
    for (term_value in item_terms) {
      hit_idx <- which(grepl(term_value, item_text, fixed = TRUE))
      if (length(hit_idx) > 0) return(safe_chr(data_value[[value_col]][hit_idx[1]]))
    }
  }
  ""
}

execution_files_11h <- find_files_all_terms(all_table_files, c("11h", "execution_summary"), max_n = 20)
execution_files_11i <- find_files_all_terms(all_table_files, c("11i", "execution_summary"), max_n = 20)
execution_files_11j <- find_files_all_terms(all_table_files, c("11j", "execution_summary"), max_n = 20)
execution_files_11g <- find_files_all_terms(all_table_files, c("11g", "execution_summary"), max_n = 20)

key_metric_df <- data.frame(
  metric = c(
    "11H imported evidence rows",
    "11H umbrella evidence axes",
    "11H candidate marker signature genes",
    "11I state-level module-score rows",
    "11I variable modules",
    "11I strong module-correlation pairs",
    "11I identity-risk axis rho",
    "11J valid ROC/PR tasks",
    "11J median AUROC",
    "11J median AUPRC",
    "11J feature-marker overlap",
    "11G candidate genes with PD genetic context"
  ),
  value = c(
    extract_execution_summary_value(execution_files_11h, c("all_imported_evidence_rows")),
    extract_execution_summary_value(execution_files_11h, c("umbrella_evidence_axes")),
    extract_execution_summary_value(execution_files_11h, c("candidate_marker_signature_genes")),
    extract_execution_summary_value(execution_files_11i, c("state_level_module_score_rows")),
    extract_execution_summary_value(execution_files_11i, c("variable_modules_for_correlation")),
    extract_execution_summary_value(execution_files_11i, c("strong_or_very_strong_pairs", "strong")),
    extract_execution_summary_value(execution_files_11i, c("identity_risk_axis_spearman_rho")),
    extract_execution_summary_value(execution_files_11j, c("valid_roc_pr_tasks_detected")),
    extract_execution_summary_value(execution_files_11j, c("median_auroc")),
    extract_execution_summary_value(execution_files_11j, c("median_auprc")),
    extract_execution_summary_value(execution_files_11j, c("features_overlapping_11h_candidate_marker_signatures")),
    extract_execution_summary_value(execution_files_11g, c("candidate_genes_with_pd_genetic_context"))
  ),
  interpretation = c(
    "breadth of integrated 11H evidence",
    "main 11H umbrella tier structure",
    "candidate transcriptomic marker signature count",
    "state-level module correlation input size",
    "module correlation breadth",
    "coordinated module structure",
    "identity-risk co-variation; not clinical safety prediction",
    "ML audit task count",
    "marker-rule-derived internal discrimination only",
    "marker-rule-derived internal PR performance only",
    "feature-marker overlap limitation",
    "limited PD genetic-context support"
  ),
  stringsAsFactors = FALSE
)
key_metric_df$value[key_metric_df$value == ""] <- "not_detected"
write_csv_safe(key_metric_df, file.path(out_table_dir, "12A_FINAL_key_upstream_metrics_for_storyline.csv"))

# ------------------------- final storyline table -------------------------
storyline_df <- data.frame(
  storyline_step = 1:8,
  manuscript_section_role = c(
    "Problem framing",
    "Dataset and source traceability",
    "Core transcriptomic prioritisation model",
    "Temporal differentiation and module coordination",
    "Preclinical/projection/state-level evidence expansion",
    "Risk/safety-context and genetic-context boundary layers",
    "Integrated umbrella evidence tier and candidate marker signatures",
    "ML audit and final claim boundary"
  ),
  final_message = c(
    "The study is framed as a dopaminergic neuron / graft-related transcriptomic cell-state prioritisation framework, not a PD clinical prediction study.",
    "Locked dataset/domain/source audits define what each GEO-derived and manually imported source can and cannot support.",
    "marker-rule-derived prioritization model and prioritisation modules define candidate high-priority transcriptional states under a bounded transcriptomic framework.",
    "Pseudotime and module-score correlation analyses support coordinated maturation, DA identity and projection-associated molecular competence.",
    "Preclinical marker/outcome-associated support, state-level proxy evidence and projection-associated proxy evidence broaden the model beyond one dataset.",
    "Survival/stress perturbation proxy adds risk-context evidence; PD genetic-context support is retained as limited, low-weight background support.",
    "11H integrates ML, pseudotime, preclinical, state-proxy and projection-proxy evidence into a Tier1 high-priority DA/graft transcriptomic identity umbrella.",
    "11J supports internal marker-rule-derived prioritization model performance by ROC/PR audit while feature-level biological interpretation remains conservative."
  ),
  main_supporting_modules = c(
    "10G;10H;11A",
    "10C;10D;10P;11A;11B",
    "09C;10L",
    "10K;11I",
    "11C;11E;11F",
    "11D;11G",
    "11H",
    "11J"
  ),
  allowed_claim = c(
    "transcriptomic cell-state prioritisation framework",
    "source-traceable computational framework",
    "marker-rule-derived prioritisation model",
    "coordinated transcriptomic module structure",
    "multi-layer proxy support",
    "risk-context and limited genetic-context support",
    "umbrella evidence-tier support",
    "marker-rule-derived prioritization model audit support"
  ),
  prohibited_claim = c(
    "PD clinical prediction or treatment-response prediction",
    "unbounded cross-domain generalisation",
    "clinical graft outcome prediction",
    "true lineage tracing or causal maturation proof",
    "true anatomical projection or clinical efficacy validation",
    "clinical safety prediction or strong GWAS validation",
    "validated biomarker discovery",
    "clinical diagnostic/prognostic predictor"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(storyline_df, file.path(out_table_dir, "12A_FINAL_storyline_refresh_table.csv"))
write_tsv_safe(storyline_df, file.path(out_table_dir, "12A_FINAL_storyline_refresh_table.tsv"))

# ------------------------- final claim boundary -------------------------
claim_boundary_df <- data.frame(
  category = c(
    "allowed", "allowed", "allowed", "allowed", "allowed", "allowed",
    "prohibited", "prohibited", "prohibited", "prohibited", "prohibited", "prohibited", "prohibited"
  ),
  statement = c(
    "DA neuron / graft-related transcriptomic cell-state prioritisation framework",
    "Evidence-anchored prioritisation across ML, pseudotime, preclinical marker support, state-level proxy support and projection-associated molecular competence support",
    "Candidate transcriptomic marker signatures",
    "Risk/safety-context module support and survival/stress perturbation proxy support",
    "Limited PD genetic-context support as a conservative background layer",
    "marker-rule-derived prioritization model audit with internal ROC/PR performance",
    "Clinical prediction model",
    "PD diagnosis, prognosis or treatment-response biomarker validation",
    "Clinical graft efficacy prediction",
    "Clinical graft safety prediction",
    "True anatomical projection or host integration proof",
    "Barcode-confirmed lineage tracing when strict barcode/clone metadata are absent",
    "Causal therapeutic mechanism proof"
  ),
  manuscript_location = c(
    "Title/Abstract/Results",
    "Results/Discussion",
    "Results/Supplement",
    "Results/Discussion",
    "Results/Limitations",
    "Results/Supplement",
    "Do not use",
    "Do not use",
    "Do not use",
    "Do not use",
    "Do not use",
    "Do not use",
    "Do not use"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(claim_boundary_df, file.path(out_table_dir, "12A_FINAL_claim_boundary_table.csv"))

# ------------------------- 12B-ready figure plan -------------------------
figure_refresh_df <- data.frame(
  figure_id = c(
    "Main Fig 1", "Main Fig 2", "Main Fig 3", "Main Fig 4", "Main Fig 5",
    "Supplement Fig S1", "Supplement Fig S2", "Supplement Fig S3", "Supplement Fig S4", "Supplement Fig S5"
  ),
  figure_role = c(
    "framework overview and source traceability",
    "core prioritisation and DA/graft identity modules",
    "temporal maturation and module correlation",
    "multi-layer external/proxy evidence expansion",
    "integrated umbrella evidence tier and claim boundary",
    "dataset/source audit",
    "preclinical/projection/state proxy details",
    "risk/safety and genetic-context details",
    "candidate marker signature details",
    "ML ROC/PR audit and feature-transparency audit"
  ),
  key_locked_inputs = c(
    "10C;10D;10P;11A;11B",
    "09C;10L",
    "10K;11I",
    "11C;11E;11F",
    "11H;11J",
    "10G;10H;11A;11B",
    "11C;11E;11F",
    "11D;11G",
    "11H",
    "11J"
  ),
  figure_claim_boundary = c(
    "source-traceable computational framework",
    "marker-rule-derived transcriptomic prioritisation",
    "module co-variation and pseudotime support",
    "proxy support, not anatomical/clinical validation",
    "integrated evidence tier, not clinical prediction",
    "dataset and domain boundaries",
    "proxy evidence detail",
    "risk-context and limited genetic-context detail",
    "candidate transcriptomic marker signatures only",
    "marker-rule-derived internal ML audit only"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(figure_refresh_df, file.path(out_table_dir, "12A_FINAL_12B_ready_figure_refresh_plan.csv"))

# ------------------------- manuscript paragraph drafts -------------------------
abstract_sentence_df <- data.frame(
  section = c("Abstract background", "Abstract method", "Abstract result", "Abstract limitation"),
  sentence = c(
    "We developed a source-traceable transcriptomic framework for prioritising dopaminergic neuron and graft-related cell states.",
    "The framework integrates marker-rule-derived machine learning, pseudotime, module-score correlation, preclinical marker support, projection-associated molecular competence, state-level proxy evidence, risk-context modelling and limited PD genetic-context support.",
    "Integrated evidence-tier analysis identified a high-priority DA/graft transcriptomic identity umbrella supported by multiple upstream evidence layers, while ML audit showed moderate-to-good internal marker-rule-derived discrimination.",
    "The framework does not constitute a clinical-use model, validated biomarker assay, anatomical-projection claim or barcode-lineage claim experiment."
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(abstract_sentence_df, file.path(out_table_dir, "12A_FINAL_abstract_sentence_refresh.csv"))

results_paragraph_lines <- c(
  "12A final storyline refresh - Results paragraph draft",
  "==================================================",
  "",
  "The final analysis supports a transcriptomic cell-state prioritisation framework rather than a clinical-use model. Across locked upstream modules, source-traceable dataset audits, marker-rule-derived machine learning, pseudotime analysis, preclinical marker support, state-level proxy evidence, projection-associated molecular competence analysis, risk-context perturbation proxy evidence, and limited PD genetic-context support were integrated into an umbrella evidence-tier structure. The main 11H integration identified a high-priority DA/graft transcriptomic identity umbrella supported by ML, pseudotime, preclinical, state-level proxy and projection-proxy evidence layers. Module-score correlation further showed coordinated transcriptomic coupling among DA identity, projection-associated competence, axon guidance, synaptic maturation and neuronal maturation modules. The 11J ML audit supported internal marker-rule-derived prioritisation performance, while feature-level biological interpretation remained conservative because numeric feature-importance and 11H marker-overlap support were limited.",
  "",
  "Claim boundary:",
  "These analyses support evidence-anchored transcriptomic prioritisation and candidate transcriptomic marker signatures only. They do not establish clinical prediction, validated biomarker performance, causal graft efficacy or safety, true anatomical projection, host integration, or barcode-lineage claim."
)
writeLines(results_paragraph_lines, file.path(out_text_dir, "12A_FINAL_results_paragraph_draft.txt"))
cat("[12A FINAL] Wrote:", file.path(out_text_dir, "12A_FINAL_results_paragraph_draft.txt"), "\n")

# ------------------------- figures -------------------------
# FigA: locked module roadmap
fig_a <- open_pdf_safe("12A_FINAL_FigA_locked_module_roadmap.pdf", 13.5, 7.4)
new_canvas()
draw_title("Final locked workflow roadmap", "Locked upstream modules supporting the final transcriptomic prioritisation storyline.")

plot_catalog <- module_catalog
plot_catalog$plot_group <- ifelse(grepl("^10", plot_catalog$module_id), "Baseline and source package", "Evidence expansion and final audit")
plot_catalog$idx <- seq_len(nrow(plot_catalog))

x_positions <- rep(NA_real_, nrow(plot_catalog))
y_positions <- rep(NA_real_, nrow(plot_catalog))
# 10A-10P top row, 11A-11J bottom row
idx_10 <- which(grepl("^10", plot_catalog$module_id))
idx_11 <- which(grepl("^11", plot_catalog$module_id))
x_positions[idx_10] <- seq(0.08, 0.92, length.out = length(idx_10))
y_positions[idx_10] <- 0.66
x_positions[idx_11] <- seq(0.08, 0.92, length.out = length(idx_11))
y_positions[idx_11] <- 0.36

# connecting lines
for (idx_link in seq_len(length(idx_10) - 1)) {
  segments(x_positions[idx_10[idx_link]], y_positions[idx_10[idx_link]], x_positions[idx_10[idx_link + 1]], y_positions[idx_10[idx_link + 1]], col = nature_palette$grid, lwd = 2)
}
for (idx_link in seq_len(length(idx_11) - 1)) {
  segments(x_positions[idx_11[idx_link]], y_positions[idx_11[idx_link]], x_positions[idx_11[idx_link + 1]], y_positions[idx_11[idx_link + 1]], col = nature_palette$grid, lwd = 2)
}
segments(0.92, 0.66, 0.08, 0.36, col = nature_palette$grid, lwd = 2, lty = 2)

for (idx_point in seq_len(nrow(plot_catalog))) {
  color_now <- module_color(plot_catalog$module_id[idx_point])
  radius_now <- 0.018
  if (plot_catalog$module_id[idx_point] %in% c("11H", "11I", "11J")) radius_now <- 0.024
  symbols(x_positions[idx_point], y_positions[idx_point], circles = radius_now, inches = FALSE,
          bg = color_now, fg = nature_palette$border, lwd = 0.5, add = TRUE)
  text(x_positions[idx_point], y_positions[idx_point] - 0.055, plot_catalog$module_id[idx_point], cex = 0.44, adj = c(0.5, 0.5), col = nature_palette$ink)
}
text(0.08, 0.76, "10A-10P baseline/source package", cex = 0.56, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
text(0.08, 0.46, "11A-11J evidence expansion and final audits", cex = 0.56, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
text(0.50, 0.14, "12A refresh converts locked outputs into final manuscript storyline and 12B figure plan.", cex = 0.56, col = nature_palette$muted)
dev.off()
cat("[12A FINAL] Wrote figure:", fig_a, "\n")

# FigB: evidence role strength summary
fig_b <- open_pdf_safe("12A_FINAL_FigB_evidence_layer_strength_summary.pdf", 11.2, 6.4)
new_canvas()
draw_title("Final evidence-layer strength summary", "Interpretation strength is conservative and claim-boundary aware.")

strength_df <- data.frame(
  layer = c("11H integrated umbrella tier", "11J marker-rule-derived prioritization model audit", "11I module correlation", "11C preclinical marker support", "11F projection proxy", "11D risk-context proxy", "11E state-level proxy", "11G PD genetic context"),
  strength_score = c(5, 4, 4, 3.5, 3.5, 3, 2, 1),
  display_label = c("strong integrative", "moderate ML audit", "moderate module support", "moderate proxy", "moderate proxy", "risk-context proxy", "limited proxy", "limited background"),
  color_id = c("11H", "11J", "11I", "11C", "11F", "11D", "11E", "11G"),
  stringsAsFactors = FALSE
)
bar_x0 <- 0.36
bar_x1 <- 0.82
y_values <- seq(0.78, 0.22, length.out = nrow(strength_df))
max_strength <- max(strength_df$strength_score, na.rm = TRUE)
for (idx_row in seq_len(nrow(strength_df))) {
  yy <- y_values[idx_row]
  score_now <- strength_df$strength_score[idx_row]
  width_now <- score_now / max_strength
  color_now <- module_color(strength_df$color_id[idx_row])
  text(bar_x0 - 0.018, yy, strength_df$layer[idx_row], cex = 0.48, adj = c(1, 0.5), col = nature_palette$ink)
  rect(bar_x0, yy - 0.023, bar_x0 + width_now * (bar_x1 - bar_x0), yy + 0.023,
       col = color_now, border = nature_palette$border, lwd = 0.4)
  text(bar_x1 + 0.014, yy, strength_df$display_label[idx_row], cex = 0.42, adj = c(0, 0.5), col = nature_palette$muted)
}
text(0.50, 0.10, "Higher score indicates stronger support for transcriptomic prioritisation, not clinical validation.", cex = 0.50, col = nature_palette$muted)
dev.off()
cat("[12A FINAL] Wrote figure:", fig_b, "\n")

# FigC: final storyline flow
fig_c <- open_pdf_safe("12A_FINAL_FigC_final_storyline_flow.pdf", 13.0, 7.0)
new_canvas()
draw_title("Final manuscript storyline flow", "From source-traceable inputs to bounded transcriptomic prioritisation claims.")

flow_steps <- storyline_df
flow_steps <- flow_steps[seq_len(nrow(flow_steps)), , drop = FALSE]
box_x0 <- 0.08
box_x1 <- 0.92
top_y <- 0.78
box_h <- 0.056
gap_h <- 0.022
for (idx_step in seq_len(nrow(flow_steps))) {
  yy_top <- top_y - (idx_step - 1) * (box_h + gap_h)
  yy_bottom <- yy_top - box_h
  color_now <- module_color(flow_steps$main_supporting_modules[idx_step])
  rect(box_x0, yy_bottom, box_x1, yy_top, col = blend_color(nature_palette$white, color_now, 0.18), border = color_now, lwd = 0.8)
  text(box_x0 + 0.018, (yy_top + yy_bottom) / 2, paste0(idx_step, "."), cex = 0.50, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
  text(box_x0 + 0.060, (yy_top + yy_bottom) / 2, flow_steps$manuscript_section_role[idx_step], cex = 0.48, font = 2, adj = c(0, 0.5), col = nature_palette$ink)
  text(box_x0 + 0.345, (yy_top + yy_bottom) / 2, flow_steps$allowed_claim[idx_step], cex = 0.42, adj = c(0, 0.5), col = nature_palette$muted)
  if (idx_step < nrow(flow_steps)) {
    arrows(0.50, yy_bottom - 0.003, 0.50, yy_bottom - gap_h + 0.004, length = 0.04, angle = 20, col = nature_palette$muted, lwd = 0.7)
  }
}
dev.off()
cat("[12A FINAL] Wrote figure:", fig_c, "\n")

# FigD: final claim boundary compact plot
fig_d <- open_pdf_safe("12A_FINAL_FigD_final_claim_boundary_summary.pdf", 11.6, 6.6)
new_canvas()
draw_title("Final claim-boundary summary", "Allowed claims are transcriptomic and prioritisation-focused; prohibited claims are clinical or causal.")

boundary_plot <- data.frame(
  category = c("Transcriptomic prioritisation", "Candidate marker signatures", "Risk-context module support", "Limited PD genetic context", "marker-rule-derived prioritization model audit", "Clinical prediction", "Validated biomarker", "Causal graft efficacy/safety", "True projection/lineage proof"),
  status = c("Allowed", "Allowed", "Allowed", "Allowed", "Allowed", "Prohibited", "Prohibited", "Prohibited", "Prohibited"),
  score = c(1, 1, 1, 1, 1, -1, -1, -1, -1),
  stringsAsFactors = FALSE
)
bar_x_mid <- 0.52
bar_scale <- 0.32
y_values <- seq(0.78, 0.22, length.out = nrow(boundary_plot))
for (idx_row in seq_len(nrow(boundary_plot))) {
  yy <- y_values[idx_row]
  score_now <- boundary_plot$score[idx_row]
  if (score_now > 0) {
    rect(bar_x_mid, yy - 0.022, bar_x_mid + bar_scale, yy + 0.022, col = nature_palette$teal, border = nature_palette$border, lwd = 0.35)
    text(bar_x_mid + bar_scale + 0.014, yy, "allowed", cex = 0.42, adj = c(0, 0.5), col = nature_palette$teal)
  } else {
    rect(bar_x_mid - bar_scale, yy - 0.022, bar_x_mid, yy + 0.022, col = nature_palette$orange, border = nature_palette$border, lwd = 0.35)
    text(bar_x_mid - bar_scale - 0.014, yy, "prohibited", cex = 0.42, adj = c(1, 0.5), col = nature_palette$orange)
  }
  text(bar_x_mid, yy, boundary_plot$category[idx_row], cex = 0.44, adj = c(0.5, 0.5), col = nature_palette$ink)
}
segments(bar_x_mid, 0.16, bar_x_mid, 0.84, col = nature_palette$border, lwd = 0.6)
text(0.50, 0.10, "Claim boundaries should be stated in legends, methods and discussion, not overdrawn onto final data panels.", cex = 0.48, col = nature_palette$muted)
dev.off()
cat("[12A FINAL] Wrote figure:", fig_d, "\n")

# ------------------------- execution summary and report -------------------------
summary_df <- data.frame(
  item = c(
    "locked_modules_in_catalog",
    "modules_with_detected_tables",
    "modules_with_detected_pdf_figures",
    "storyline_steps",
    "figure_refresh_plan_rows",
    "claim_boundary_rows",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(nrow(module_catalog)),
    as.character(sum(module_catalog$detected_table_files > 0)),
    as.character(sum(module_catalog$detected_pdf_figures > 0)),
    as.character(nrow(storyline_df)),
    as.character(nrow(figure_refresh_df)),
    as.character(nrow(claim_boundary_df)),
    "4",
    "INPUT_READY_FOR_12B_FINAL_FIGURE_PLAN_REFRESH"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "12A_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "12A_FINAL_execution_summary.tsv"))

report_lines <- c(
  "12A FINAL report",
  "================",
  "Module: final storyline refresh",
  "Mode: complete standalone 12A rebuild; no previous 12A output dependency; no internet; no 00-10P rerun.",
  "Allowed upstream inputs: locked 10A-10P and 11A-11J outputs.",
  "",
  paste0("Locked modules in catalog: ", nrow(module_catalog)),
  paste0("Modules with detected tables: ", sum(module_catalog$detected_table_files > 0)),
  paste0("Modules with detected PDF figures: ", sum(module_catalog$detected_pdf_figures > 0)),
  paste0("Storyline steps: ", nrow(storyline_df)),
  paste0("12B-ready figure plan rows: ", nrow(figure_refresh_df)),
  paste0("Claim-boundary rows: ", nrow(claim_boundary_df)),
  "",
  "Final project framing:",
  "DA neuron / graft-related transcriptomic cell-state prioritisation framework.",
  "",
  "Core allowed claims:",
  "- Evidence-anchored transcriptomic prioritisation.",
  "- Candidate transcriptomic marker signatures.",
  "- Module co-variation and pseudotime support.",
  "- Preclinical/projection/state-level proxy support.",
  "- Risk-context and limited genetic-context support.",
  "- marker-rule-derived prioritization model audit support.",
  "",
  "Core prohibited claims:",
  "- No clinical prediction.",
  "- No diagnostic/prognostic/therapeutic-response biomarker validation.",
  "- No causal graft efficacy or safety claim.",
  "- No true anatomical projection, host integration or barcode-confirmed lineage proof.",
  "",
  "Decision: INPUT_READY_FOR_12B_FINAL_FIGURE_PLAN_REFRESH"
)
report_file <- file.path(out_text_dir, "12A_FINAL_storyline_refresh_report.txt")
writeLines(report_lines, report_file)
cat("[12A FINAL] Wrote:", report_file, "\n")

cat("\n[12A FINAL] Completed final storyline refresh.\n")
cat("[12A FINAL] Locked modules in catalog:", nrow(module_catalog), "\n")
cat("[12A FINAL] Modules with detected tables:", sum(module_catalog$detected_table_files > 0), "\n")
cat("[12A FINAL] Modules with detected PDF figures:", sum(module_catalog$detected_pdf_figures > 0), "\n")
cat("[12A FINAL] Storyline steps:", nrow(storyline_df), "\n")
cat("[12A FINAL] 12B-ready figure plan rows:", nrow(figure_refresh_df), "\n")
cat("[12A FINAL] Claim-boundary rows:", nrow(claim_boundary_df), "\n")
cat("[12A FINAL] Figures written: 4\n")
cat("[12A FINAL] Decision: INPUT_READY_FOR_12B_FINAL_FIGURE_PLAN_REFRESH\n")
cat("[12A FINAL] Output tables:", out_table_dir, "\n")
cat("[12A FINAL] Output figs  :", out_fig_dir, "\n")
cat("[12A FINAL] Output text  :", out_text_dir, "\n")
cat("[12A FINAL] Next         : review 12A FINAL PDFs; if accepted, proceed to 12B final figure plan refresh.\n")
