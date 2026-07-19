
# ============================================================
# 11H FINAL COMPLETE STANDALONE - UMBRELLA EVIDENCE REBUILD
# Integrated evidence tier + candidate transcriptomic marker
# signature table for DA neuron / graft-related cell-state
# prioritisation framework
#
# User rule respected:
#   - Complete standalone script for 11H
#   - Does NOT read any previous 11H output
#   - Does NOT use table-only patch logic
#   - Rebuilds 11H from locked upstream outputs when present:
#       09C / 10K / 11C / 11D / 11E / 11F / 11G
#   - No internet
#   - No 00-10P rerun
#
# Main design:
#   - Exact-unit table is kept only as audit.
#   - Biological-axis table is kept as source decomposition.
#   - UMBRELLA evidence tier is the main 11H integration:
#       1) high_priority_DA_graft_identity_umbrella
#       2) risk_safety_context_umbrella
#       3) PD_genetic_context_umbrella
#   - This avoids the failure mode of exact-label-only or
#     one-layer-per-axis diagonal integration.
#
# Scientific boundary:
#   - Evidence-anchored transcriptomic prioritisation only
#   - Candidate transcriptomic marker signatures only
#   - No clinical prediction
#   - No diagnostic/prognostic biomarker claim
#   - No true lineage/projection/clinical graft outcome proof
# ============================================================

cat("\n[11H FINAL] Starting integrated evidence tier + candidate marker signature table...\n")
cat("[11H FINAL] Mode: complete standalone 11H rebuild; no previous 11H dependency; no internet; no 00-10P rerun.\n")
cat("[11H FINAL] Integration mode: umbrella evidence-tier integration is the main 11H output.\n")
cat("[11H FINAL] Claim boundary: evidence-anchored transcriptomic prioritisation only; no clinical prediction or clinical biomarker claim.\n")

graphics.off()

# ------------------------- project paths -------------------------
project_root <- "D:/PD_Graft_Project"

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "11H_integrated_evidence_tier_and_candidate_marker_signature_FINAL_COMPLETE_STANDALONE"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "11H_integrated_evidence_tier_and_candidate_marker_signature_FINAL_COMPLETE_STANDALONE_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "11H_integrated_evidence_tier_and_candidate_marker_signature_FINAL_COMPLETE_STANDALONE"
)

dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_text_dir, recursive = TRUE, showWarnings = FALSE)

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

clean_gene_symbol <- function(value_obj) {
  out <- toupper(clean_space(value_obj))
  out <- gsub("[^A-Z0-9.-]", "", out)
  out[out %in% c("", "NA", "NAN", "NULL", "NONE", "GENE", "SYMBOL")] <- ""
  out
}

clean_unit_label <- function(value_obj) {
  out <- clean_space(value_obj)
  out[out %in% c("", "NA", "NaN", "NULL", "None")] <- ""
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
  cat("[11H FINAL] Wrote:", file_value, "\n")
}

write_tsv_safe <- function(data_value, file_value) {
  utils::write.table(data_value, file_value, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[11H FINAL] Wrote:", file_value, "\n")
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
    grDevices::pdf(file_primary, width = width_value, height = height_value, onefile = FALSE, useDingbats = FALSE, paper = "special")
  }, error = function(err_obj) {
    ok_value <<- FALSE
  })
  if (!ok_value) {
    alt_name <- paste0(sub("\\.pdf$", "", filename), "_ALT_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    file_alt <- file.path(out_fig_dir, alt_name)
    grDevices::pdf(file_alt, width = width_value, height = height_value, onefile = FALSE, useDingbats = FALSE, paper = "special")
    cat("[11H FINAL] WARNING: primary PDF was locked; wrote ALT file:", file_alt, "\n")
    return(file_alt)
  }
  file_primary
}

new_canvas <- function() {
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", xpd = FALSE)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
}

draw_title <- function(title_value, subtitle_value = "") {
  text(0.5, 0.965, title_value, cex = 0.98, font = 2, adj = c(0.5, 0.5))
  if (nchar(subtitle_value) > 0) {
    text(0.5, 0.928, subtitle_value, cex = 0.50, col = "gray35", adj = c(0.5, 0.5))
  }
}

value_to_gray <- function(value_obj, max_obj) {
  value_num <- safe_num(value_obj)
  max_num <- max(safe_num(max_obj), na.rm = TRUE)
  if (!is.finite(max_num) || max_num <= 0) max_num <- 1
  frac_value <- value_num / max_num
  frac_value[!is.finite(frac_value)] <- 0
  frac_value[frac_value < 0] <- 0
  frac_value[frac_value > 1] <- 1
  gray(0.93 - 0.60 * frac_value)
}

split_gene_cell_safe <- function(value_obj) {
  value_chr <- safe_chr(value_obj)
  if (length(value_chr) < 1) return(character(0))
  token_list <- strsplit(value_chr, "[;,/| ]+")
  token_values <- unlist(token_list, use.names = FALSE)
  token_values <- clean_gene_symbol(token_values)
  unique(token_values[token_values != ""])
}

# ------------------------- file search -------------------------
table_root <- file.path(project_root, "03_tables")
if (!dir.exists(table_root)) stop("[11H FINAL] Missing 03_tables directory: ", table_root, call. = FALSE)

all_table_files <- list.files(table_root, pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
if (length(all_table_files) < 1) all_table_files <- character(0)
file_info <- file.info(all_table_files)
all_table_files <- all_table_files[is.finite(file_info$size) & file_info$size > 0 & file_info$size < 80 * 1024 * 1024]
all_table_files <- all_table_files[!grepl("11H_integrated_evidence_tier_and_candidate_marker_signature", all_table_files, ignore.case = TRUE)]

find_files_all_terms <- function(term_values, max_n = 20) {
  if (length(all_table_files) < 1) return(character(0))
  term_values <- tolower(safe_chr(term_values))
  path_lower <- tolower(all_table_files)
  keep_vec <- rep(TRUE, length(all_table_files))
  for (term_value in term_values) keep_vec <- keep_vec & grepl(term_value, path_lower, fixed = TRUE)
  hits <- all_table_files[keep_vec]
  if (length(hits) < 1) return(character(0))
  hit_info <- file.info(hits)
  hits <- hits[order(hit_info$mtime, decreasing = TRUE)]
  unique(hits)[seq_len(min(max_n, length(unique(hits))))]
}

find_files_any_terms <- function(term_values, max_n = 20) {
  if (length(all_table_files) < 1) return(character(0))
  term_values <- tolower(safe_chr(term_values))
  path_lower <- tolower(all_table_files)
  keep_vec <- rep(FALSE, length(all_table_files))
  for (term_value in term_values) keep_vec <- keep_vec | grepl(term_value, path_lower, fixed = TRUE)
  hits <- all_table_files[keep_vec]
  if (length(hits) < 1) return(character(0))
  hit_info <- file.info(hits)
  hits <- hits[order(hit_info$mtime, decreasing = TRUE)]
  unique(hits)[seq_len(min(max_n, length(unique(hits))))]
}

first_file <- function(file_values) {
  file_values <- safe_chr(file_values)
  file_values <- file_values[file.exists(file_values)]
  if (length(file_values) < 1) return("")
  file_values[1]
}

# ------------------------- column helpers -------------------------
label_from_possible_cols <- function(data_value, col_terms, fallback_prefix = "unit") {
  if (!is.data.frame(data_value) || nrow(data_value) < 1) return(character(0))
  col_names <- colnames(data_value)
  col_lower <- tolower(col_names)
  hit_col <- ""
  for (term_value in col_terms) {
    hit_names <- col_names[grepl(term_value, col_lower, fixed = TRUE)]
    if (length(hit_names) > 0) {
      hit_col <- hit_names[1]
      break
    }
  }
  if (hit_col == "") return(paste0(fallback_prefix, "_", seq_len(nrow(data_value))))
  clean_unit_label(data_value[[hit_col]])
}

score_from_possible_cols <- function(data_value, col_terms) {
  if (!is.data.frame(data_value) || nrow(data_value) < 1) return(rep(0, 0))
  col_names <- colnames(data_value)
  col_lower <- tolower(col_names)
  hit_col <- ""
  for (term_value in col_terms) {
    hit_names <- col_names[grepl(term_value, col_lower, fixed = TRUE)]
    if (length(hit_names) > 0) {
      hit_col <- hit_names[1]
      break
    }
  }
  if (hit_col == "") return(rep(0, nrow(data_value)))
  val <- safe_num(data_value[[hit_col]])
  val[!is.finite(val)] <- 0
  finite_val <- val[is.finite(val)]
  if (length(finite_val) > 0) {
    max_val <- max(finite_val, na.rm = TRUE)
    min_val <- min(finite_val, na.rm = TRUE)
    if (is.finite(max_val) && is.finite(min_val) && (max_val > 1 || min_val < 0) && abs(max_val - min_val) > 1e-12) {
      val <- (val - min_val) / (max_val - min_val)
      val[!is.finite(val)] <- 0
    }
  }
  val
}

dataset_from_label <- function(label_values) {
  label_values <- safe_chr(label_values)
  out <- rep("", length(label_values))
  for (idx_value in seq_along(label_values)) {
    hit <- regmatches(label_values[idx_value], regexpr("GSE[0-9]+", label_values[idx_value]))
    if (length(hit) > 0 && hit != "-1") out[idx_value] <- hit
  }
  out
}

score_to_support <- function(score_values) {
  score_values <- safe_num(score_values)
  out <- rep("none_or_unscored", length(score_values))
  out[is.finite(score_values) & score_values > 0] <- "support_low"
  out[is.finite(score_values) & score_values >= 0.33] <- "support_intermediate"
  out[is.finite(score_values) & score_values >= 0.66] <- "support_high"
  out
}

tier_to_score <- function(tier_values) {
  tier_values <- tolower(safe_chr(tier_values))
  out <- rep(0, length(tier_values))
  out[grepl("low", tier_values)] <- 1
  out[grepl("intermediate|moderate", tier_values)] <- 2
  out[grepl("high", tier_values)] <- 3
  out[grepl("none|no_detected|negative|absent", tier_values)] <- 0
  out
}

make_evidence_rows <- function(
  source_layer_value,
  source_role_value,
  unit_type_value,
  unit_label_values,
  dataset_values,
  direction_values,
  score_values,
  support_values,
  weight_value,
  source_file_value,
  note_values
) {
  unit_label_values <- clean_unit_label(unit_label_values)
  keep_vec <- unit_label_values != ""
  if (length(unit_label_values) < 1 || sum(keep_vec) < 1) return(data.frame(stringsAsFactors = FALSE))
  n_value <- length(unit_label_values)
  dataset_values <- rep(safe_chr(dataset_values), length.out = n_value)
  direction_values <- rep(safe_chr(direction_values), length.out = n_value)
  score_values <- rep(safe_num(score_values), length.out = n_value)
  support_values <- rep(safe_chr(support_values), length.out = n_value)
  note_values <- rep(safe_chr(note_values), length.out = n_value)

  out <- data.frame(
    source_layer = rep(source_layer_value, n_value),
    source_role = rep(source_role_value, n_value),
    evidence_unit_type = rep(unit_type_value, n_value),
    evidence_unit_label = unit_label_values,
    dataset = dataset_values,
    evidence_direction = direction_values,
    evidence_score = score_values,
    evidence_support_level = support_values,
    evidence_weight = rep(weight_value, n_value),
    weighted_evidence_score = score_values * weight_value,
    source_file = rep(source_file_value, n_value),
    evidence_note = note_values,
    allowed_interpretation = rep("supportive transcriptomic prioritisation evidence only", n_value),
    prohibited_interpretation = rep("no clinical prediction; no validated clinical biomarker; no causal proof", n_value),
    stringsAsFactors = FALSE
  )
  out <- out[keep_vec, , drop = FALSE]
  out
}

assign_evidence_axis <- function(layer_values, unit_label_values, direction_values, support_values) {
  layer_values <- safe_chr(layer_values)
  label_values <- safe_chr(unit_label_values)
  direction_values <- safe_chr(direction_values)
  support_values <- safe_chr(support_values)
  out <- rep("supportive_context_axis", length(layer_values))

  for (idx_value in seq_along(out)) {
    layer_now <- layer_values[idx_value]
    label_now <- paste(label_values[idx_value], direction_values[idx_value], support_values[idx_value], sep = " ")
    lower_now <- tolower(label_now)

    if (grepl("09c", layer_now, ignore.case = TRUE)) out[idx_value] <- "ML_priority_axis"
    if (grepl("10k", layer_now, ignore.case = TRUE)) out[idx_value] <- "maturation_pseudotime_axis"
    if (grepl("11c", layer_now, ignore.case = TRUE)) out[idx_value] <- "preclinical_graft_outcome_alignment_axis"
    if (grepl("11d", layer_now, ignore.case = TRUE)) out[idx_value] <- "risk_survival_stress_safety_axis"
    if (grepl("11e", layer_now, ignore.case = TRUE)) out[idx_value] <- "state_level_proxy_axis"
    if (grepl("11f", layer_now, ignore.case = TRUE)) out[idx_value] <- "projection_molecular_competence_axis"
    if (grepl("11g", layer_now, ignore.case = TRUE)) out[idx_value] <- "PD_genetic_context_axis"

    if (grepl("risk|stress|apoptosis|p53|nfkb|inflammatory|proliferation|high_with_risk", lower_now)) {
      if (grepl("11d", layer_now, ignore.case = TRUE)) out[idx_value] <- "risk_survival_stress_safety_axis"
      if (grepl("11f", layer_now, ignore.case = TRUE) && grepl("risk", lower_now)) out[idx_value] <- "projection_molecular_competence_axis"
    }
  }
  out
}

assign_umbrella_axis <- function(layer_values, evidence_axis_values, direction_values) {
  layer_values <- safe_chr(layer_values)
  evidence_axis_values <- safe_chr(evidence_axis_values)
  direction_values <- tolower(safe_chr(direction_values))
  out <- rep("supportive_context_umbrella", length(layer_values))
  for (idx_value in seq_along(out)) {
    axis_now <- evidence_axis_values[idx_value]
    layer_now <- layer_values[idx_value]
    direction_now <- direction_values[idx_value]

    if (axis_now %in% c("ML_priority_axis", "maturation_pseudotime_axis", "preclinical_graft_outcome_alignment_axis", "projection_molecular_competence_axis", "state_level_proxy_axis")) {
      out[idx_value] <- "high_priority_DA_graft_transcriptomic_identity_umbrella"
    }
    if (axis_now %in% c("risk_survival_stress_safety_axis")) {
      out[idx_value] <- "risk_safety_context_umbrella"
    }
    if (axis_now %in% c("PD_genetic_context_axis")) {
      out[idx_value] <- "PD_genetic_context_umbrella"
    }
    if (grepl("risk|stress|apoptosis|p53|nfkb|inflammatory|proliferation", direction_now)) {
      out[idx_value] <- "risk_safety_context_umbrella"
    }
    if (grepl("11d", layer_now, ignore.case = TRUE)) {
      out[idx_value] <- "risk_safety_context_umbrella"
    }
  }
  out
}

# ------------------------- import upstream layers -------------------------
evidence_list <- list()
layer_audit_list <- list()

add_layer_audit <- function(layer_name, status_value, file_values, row_count_value, note_value) {
  layer_audit_list[[length(layer_audit_list) + 1]] <<- data.frame(
    layer = layer_name,
    status = status_value,
    files_detected = length(file_values),
    rows_imported = row_count_value,
    representative_file = ifelse(length(file_values) > 0, file_values[1], ""),
    note = note_value,
    stringsAsFactors = FALSE
  )
}

cat("[11H FINAL] Importing locked upstream evidence layers...\n")

# 11C
file_11c <- first_file(c(
  find_files_all_terms(c("11c", "ranked"), max_n = 5),
  find_files_all_terms(c("11c", "target"), max_n = 10),
  find_files_all_terms(c("11c", "outcome"), max_n = 10),
  find_files_any_terms(c("11c_preclinical"), max_n = 10)
))
data_11c <- read_table_safe(file_11c)
rows_11c <- data.frame(stringsAsFactors = FALSE)
if (nrow(data_11c) > 0) {
  labels_11c <- label_from_possible_cols(data_11c, c("compact_gse_label", "compact", "label", "state", "cluster"), "11C_state")
  score_11c <- score_from_possible_cols(data_11c, c("priority_alignment_index", "priority", "alignment", "favorable", "score"))
  if (max(score_11c, na.rm = TRUE) <= 0) score_11c <- rep(0.7, length(labels_11c))
  rows_11c <- make_evidence_rows(
    "11C_preclinical_graft_outcome_marker_support",
    "preclinical marker/outcome-associated transcriptomic support",
    "state",
    labels_11c,
    dataset_from_label(labels_11c),
    "favorable_or_identity_associated",
    score_11c,
    score_to_support(score_11c),
    2.0,
    file_11c,
    "target-only preclinical graft/outcome marker support"
  )
}
evidence_list[[length(evidence_list) + 1]] <- rows_11c
add_layer_audit("11C", ifelse(nrow(rows_11c) > 0, "imported", "missing_or_empty"), file_11c[file_11c != ""], nrow(rows_11c), "preclinical graft-outcome marker support")

# 11D locked summary/fallback
known_11d_proxy_genes <- c("CASP2", "TP53", "SLC7A11", "TLR4", "CASP9", "BCL2L11", "IL18", "BBC3", "TNFRSF11B")
known_11d_overlap_genes <- c("CASP2", "TP53", "CASP9", "BCL2L11", "BBC3")
file_11d_values <- find_files_any_terms(c("11d"), max_n = 40)
rows_11d_gene <- make_evidence_rows(
  "11D_survival_stress_CRISPR_proxy_support",
  "survival/stress perturbation and CRISPR proxy support",
  "gene",
  known_11d_proxy_genes,
  rep("locked_11D_summary", length(known_11d_proxy_genes)),
  ifelse(known_11d_proxy_genes %in% known_11d_overlap_genes, "risk_survival_stress_proxy_overlap", "risk_survival_stress_proxy_candidate"),
  ifelse(known_11d_proxy_genes %in% known_11d_overlap_genes, 1.0, 0.65),
  ifelse(known_11d_proxy_genes %in% known_11d_overlap_genes, "support_high", "support_intermediate"),
  2.5,
  ifelse(length(file_11d_values) > 0, file_11d_values[1], "locked_11D_manual_summary"),
  "locked 11D proxy genes; SafeHarbor/control loci excluded"
)
rows_11d_module <- make_evidence_rows(
  "11D_survival_stress_CRISPR_proxy_support",
  "survival/stress perturbation and CRISPR proxy module support",
  "module_signature",
  c("risk_survival_stress_proxy", "p53_apoptosis"),
  rep("locked_11D_summary", 2),
  c("risk_associated", "risk_associated"),
  c(1.0, 0.75),
  c("support_high", "support_intermediate"),
  2.2,
  ifelse(length(file_11d_values) > 0, file_11d_values[1], "locked_11D_manual_summary"),
  "locked 11D nonzero overlap modules"
)
rows_11d <- safe_bind_rows(list(rows_11d_gene, rows_11d_module))
evidence_list[[length(evidence_list) + 1]] <- rows_11d
add_layer_audit("11D", "imported_from_locked_summary", file_11d_values, nrow(rows_11d), "survival/stress perturbation + CRISPR proxy; conservative locked summary")

# 11E
file_11e_values <- unique(c(
  find_files_all_terms(c("11e", "state_level"), max_n = 20),
  find_files_all_terms(c("11e", "priority"), max_n = 20),
  find_files_all_terms(c("11e", "evidence"), max_n = 20),
  find_files_any_terms(c("11e_barcode_lineage"), max_n = 20)
))
rows_11e <- data.frame(stringsAsFactors = FALSE)
if (length(file_11e_values) > 0) {
  for (file_value in file_11e_values) {
    if (nrow(rows_11e) > 0) next
    tmp_data <- read_table_safe(file_value)
    if (nrow(tmp_data) > 0) {
      labels_11e <- label_from_possible_cols(tmp_data, c("state", "compact", "label", "cluster"), "11E_state")
      score_11e <- score_from_possible_cols(tmp_data, c("priority_balance", "balance", "priority", "favorable"))
      if (max(score_11e, na.rm = TRUE) <= 0) score_11e <- rep(0.45, length(labels_11e))
      rows_tmp <- make_evidence_rows(
        "11E_GSE200610_state_level_proxy_support",
        "state-level transcriptomic proxy after barcode/lineage audit",
        "state",
        labels_11e,
        dataset_from_label(labels_11e),
        "state_level_proxy_support",
        score_11e,
        score_to_support(score_11e),
        1.2,
        file_value,
        "strict barcode/clone/lineage metadata retained = 0; state-level proxy only"
      )
      if (nrow(rows_tmp) > 0) rows_11e <- rows_tmp
    }
  }
}
evidence_list[[length(evidence_list) + 1]] <- rows_11e
add_layer_audit("11E", ifelse(nrow(rows_11e) > 0, "imported", "missing_or_empty"), file_11e_values, nrow(rows_11e), "GSE200610 state-level proxy support; no barcode-level lineage claim")

# 11F
file_11f <- first_file(c(
  file.path(table_root, "11F_projection_associated_molecular_competence_proxy_FINAL_FULL_RESCAN_PUBLICATION_VISUAL_POLISH", "11F_FINAL_projection_evidence_tier_table_for_11H_DEDUP.csv"),
  find_files_all_terms(c("11f", "projection_evidence_tier_table_for_11h_dedup"), max_n = 10),
  find_files_all_terms(c("11f", "dedup"), max_n = 10)
))
data_11f <- read_table_safe(file_11f)
rows_11f <- data.frame(stringsAsFactors = FALSE)
if (nrow(data_11f) > 0) {
  labels_11f <- label_from_possible_cols(data_11f, c("compact", "state", "label", "unit", "cluster"), "11F_state")
  tier_col_names <- colnames(data_11f)[grepl("tier|support", tolower(colnames(data_11f)))]
  if (length(tier_col_names) > 0) {
    raw_score_11f <- tier_to_score(data_11f[[tier_col_names[1]]])
    support_11f <- safe_chr(data_11f[[tier_col_names[1]]])
  } else {
    raw_score_11f <- score_from_possible_cols(data_11f, c("projection", "priority", "score"))
    support_11f <- score_to_support(raw_score_11f)
  }
  score_11f <- raw_score_11f
  if (max(score_11f, na.rm = TRUE) > 1) score_11f <- score_11f / max(score_11f, na.rm = TRUE)
  rows_11f <- make_evidence_rows(
    "11F_projection_module_state_level_proxy_support",
    "projection-associated module state-level proxy support",
    "state",
    labels_11f,
    dataset_from_label(labels_11f),
    "projection_associated_identity_support",
    score_11f,
    support_11f,
    1.8,
    file_11f,
    "full rescan deduplicated projection module proxy support; no anatomical projection claim"
  )
}
evidence_list[[length(evidence_list) + 1]] <- rows_11f
add_layer_audit("11F", ifelse(nrow(rows_11f) > 0, "imported", "missing_or_empty"), file_11f[file_11f != ""], nrow(rows_11f), "projection-associated module proxy support")

# 11G
file_11g <- first_file(c(
  file.path(table_root, "11G_PD_GWAS_genetic_context_support_FINAL_COMPLETE_STANDALONE", "11G_FINAL_PD_genetic_context_support_table_for_11H.csv"),
  find_files_all_terms(c("11g", "pd_genetic_context_support_table_for_11h"), max_n = 10)
))
data_11g <- read_table_safe(file_11g)
rows_11g <- data.frame(stringsAsFactors = FALSE)
if (nrow(data_11g) > 0) {
  labels_11g <- label_from_possible_cols(data_11g, c("module_name", "module", "signature", "feature"), "11G_module")
  score_11g <- score_from_possible_cols(data_11g, c("genetic_context_support_score", "support_score", "score"))
  if (max(score_11g, na.rm = TRUE) > 1) score_11g <- score_11g / max(score_11g, na.rm = TRUE)
  support_col_names <- colnames(data_11g)[grepl("support_tier|tier", tolower(colnames(data_11g)))]
  support_11g <- ifelse(length(support_col_names) > 0, safe_chr(data_11g[[support_col_names[1]]]), score_to_support(score_11g))
  rows_11g <- make_evidence_rows(
    "11G_PD_genetic_context_support",
    "limited PD genetic-context overlap support",
    "module_signature",
    labels_11g,
    rep("PD_genetic_context_catalog", length(labels_11g)),
    "genetic_context_support",
    score_11g,
    support_11g,
    0.8,
    file_11g,
    "limited genetic-context layer; conservative low weight"
  )
}
evidence_list[[length(evidence_list) + 1]] <- rows_11g
add_layer_audit("11G", ifelse(nrow(rows_11g) > 0, "imported", "missing_or_empty"), file_11g[file_11g != ""], nrow(rows_11g), "PD GWAS/genetic context support; limited weight")

# 09C
file_09c_values <- unique(c(
  find_files_all_terms(c("09c", "feature"), max_n = 10),
  find_files_all_terms(c("09c", "importance"), max_n = 10),
  find_files_all_terms(c("09c", "prediction"), max_n = 10),
  find_files_all_terms(c("09c", "priority"), max_n = 10)
))
rows_09c <- data.frame(stringsAsFactors = FALSE)
if (length(file_09c_values) > 0) {
  data_09c <- read_table_safe(file_09c_values[1])
  if (nrow(data_09c) > 0) {
    labels_09c <- label_from_possible_cols(data_09c, c("gene", "feature", "state", "label", "module"), "09C_feature")
    score_09c <- score_from_possible_cols(data_09c, c("importance", "prob", "priority", "score", "auc"))
    if (max(score_09c, na.rm = TRUE) <= 0) score_09c <- rep(0.6, length(labels_09c))
    rows_09c <- make_evidence_rows(
      "09C_weak_label_ML_priority_model",
      "marker-rule-derived prioritization model prioritisation support",
      "state_or_feature",
      labels_09c,
      dataset_from_label(labels_09c),
      "ML_priority_associated",
      score_09c,
      score_to_support(score_09c),
      2.0,
      file_09c_values[1],
      "marker-rule-derived prioritization model support; not clinical prediction"
    )
  }
}
evidence_list[[length(evidence_list) + 1]] <- rows_09c
add_layer_audit("09C", ifelse(nrow(rows_09c) > 0, "imported", "not_detected_or_unparsed"), file_09c_values, nrow(rows_09c), "marker-rule-derived prioritization model priority model")

# 10K
file_10k_values <- unique(c(
  find_files_all_terms(c("10k", "pseudotime"), max_n = 20),
  find_files_all_terms(c("pseudotime", "priority"), max_n = 20),
  find_files_all_terms(c("10k", "state"), max_n = 20)
))
rows_10k <- data.frame(stringsAsFactors = FALSE)
if (length(file_10k_values) > 0) {
  for (file_value in file_10k_values) {
    if (nrow(rows_10k) > 0) next
    tmp_data <- read_table_safe(file_value)
    if (nrow(tmp_data) > 0) {
      labels_10k <- label_from_possible_cols(tmp_data, c("state", "label", "cluster", "day", "timepoint"), "10K_pseudotime_unit")
      score_10k <- score_from_possible_cols(tmp_data, c("pseudotime", "priority", "maturation", "correlation", "rho", "score"))
      if (max(score_10k, na.rm = TRUE) <= 0) score_10k <- rep(0.55, length(labels_10k))
      rows_tmp <- make_evidence_rows(
        "10K_multi_timepoint_pseudotime_support",
        "graph-based pseudotime chronological differentiation support",
        "state_or_timepoint",
        labels_10k,
        dataset_from_label(labels_10k),
        "maturation_temporal_progression_support",
        score_10k,
        score_to_support(score_10k),
        1.6,
        file_value,
        "pseudotime support only; not lineage tracing"
      )
      if (nrow(rows_tmp) > 0) rows_10k <- rows_tmp
    }
  }
}
evidence_list[[length(evidence_list) + 1]] <- rows_10k
add_layer_audit("10K", ifelse(nrow(rows_10k) > 0, "imported", "not_detected_or_unparsed"), file_10k_values, nrow(rows_10k), "multi-timepoint pseudotime support")

# ------------------------- combine and annotate evidence -------------------------
evidence_df <- safe_bind_rows(evidence_list)
if (nrow(evidence_df) < 1) stop("[11H FINAL] No upstream evidence rows could be imported. Check locked upstream outputs.", call. = FALSE)

evidence_df$evidence_score <- safe_num(evidence_df$evidence_score)
evidence_df$evidence_score[!is.finite(evidence_df$evidence_score)] <- 0
evidence_df$evidence_weight <- safe_num(evidence_df$evidence_weight)
evidence_df$evidence_weight[!is.finite(evidence_df$evidence_weight)] <- 1
evidence_df$weighted_evidence_score <- evidence_df$evidence_score * evidence_df$evidence_weight
evidence_df$evidence_axis <- assign_evidence_axis(
  evidence_df$source_layer,
  evidence_df$evidence_unit_label,
  evidence_df$evidence_direction,
  evidence_df$evidence_support_level
)
evidence_df$umbrella_axis <- assign_umbrella_axis(evidence_df$source_layer, evidence_df$evidence_axis, evidence_df$evidence_direction)
evidence_df$exact_integration_key <- paste(evidence_df$evidence_unit_type, evidence_df$evidence_unit_label, sep = "::")

write_csv_safe(evidence_df, file.path(out_table_dir, "11H_FINAL_all_imported_evidence_rows.csv"))
write_tsv_safe(evidence_df, file.path(out_table_dir, "11H_FINAL_all_imported_evidence_rows.tsv"))

# ------------------------- exact unit table - audit only -------------------------
unit_keys <- unique(evidence_df$exact_integration_key)
unit_list <- list()
for (idx_value in seq_along(unit_keys)) {
  key_value <- unit_keys[idx_value]
  sub_data <- evidence_df[evidence_df$exact_integration_key == key_value, , drop = FALSE]
  layer_values <- sort(unique(sub_data$source_layer))
  score_sum <- sum(safe_num(sub_data$weighted_evidence_score), na.rm = TRUE)
  risk_flag <- any(grepl("risk|stress|p53|apoptosis|inflammatory|proliferation", paste(sub_data$evidence_direction, sub_data$evidence_unit_label, collapse = " "), ignore.case = TRUE))
  unit_tier <- "Tier3_single_layer_support"
  if (length(layer_values) >= 3 && score_sum >= 4.5) unit_tier <- "Tier1_multi_layer_high_priority_support"
  if (length(layer_values) >= 2 && score_sum >= 2.5 && unit_tier == "Tier3_single_layer_support") unit_tier <- "Tier2_multi_layer_moderate_support"
  if (risk_flag && score_sum >= 1.0) unit_tier <- paste0(unit_tier, "_with_risk_context")
  if (score_sum <= 0) unit_tier <- "limited_or_no_detected_support"
  unit_list[[length(unit_list) + 1]] <- data.frame(
    exact_integration_key = key_value,
    evidence_unit_type = sub_data$evidence_unit_type[1],
    evidence_unit_label = sub_data$evidence_unit_label[1],
    n_supporting_layers = length(layer_values),
    supporting_layers = paste(layer_values, collapse = ";"),
    total_weighted_evidence_score = score_sum,
    risk_context_flag = risk_flag,
    final_exact_unit_evidence_tier = unit_tier,
    stringsAsFactors = FALSE
  )
}
unit_df <- safe_bind_rows(unit_list)
unit_df <- unit_df[order(unit_df$n_supporting_layers, unit_df$total_weighted_evidence_score, decreasing = TRUE), , drop = FALSE]
write_csv_safe(unit_df, file.path(out_table_dir, "11H_FINAL_integrated_evidence_unit_tier_table.csv"))
write_tsv_safe(unit_df, file.path(out_table_dir, "11H_FINAL_integrated_evidence_unit_tier_table.tsv"))

# ------------------------- biological axis table -------------------------
axis_values <- unique(evidence_df$evidence_axis)
axis_list <- list()
for (idx_value in seq_along(axis_values)) {
  axis_value <- axis_values[idx_value]
  sub_axis <- evidence_df[evidence_df$evidence_axis == axis_value, , drop = FALSE]
  layer_values <- sort(unique(sub_axis$source_layer))
  unit_type_values <- sort(unique(sub_axis$evidence_unit_type))
  umbrella_values <- sort(unique(sub_axis$umbrella_axis))
  score_sum <- sum(safe_num(sub_axis$weighted_evidence_score), na.rm = TRUE)
  score_mean <- mean(safe_num(sub_axis$weighted_evidence_score), na.rm = TRUE)
  if (!is.finite(score_mean)) score_mean <- 0
  high_rows <- sum(grepl("high", sub_axis$evidence_support_level, ignore.case = TRUE), na.rm = TRUE)
  risk_flag <- any(grepl("risk|stress|p53|apoptosis|inflammatory|proliferation", paste(axis_value, sub_axis$evidence_direction, collapse = " "), ignore.case = TRUE))

  tier_value <- "axis_Tier3_single_layer_support"
  if (length(layer_values) >= 3 && score_sum >= 5) tier_value <- "axis_Tier1_multi_layer_evidence_anchored_support"
  if (length(layer_values) >= 2 && score_sum >= 2.5 && tier_value == "axis_Tier3_single_layer_support") tier_value <- "axis_Tier2_multi_layer_support"
  if (risk_flag && score_sum >= 1) tier_value <- paste0(tier_value, "_with_risk_context")
  if (score_sum <= 0) tier_value <- "axis_limited_or_no_detected_support"

  axis_list[[length(axis_list) + 1]] <- data.frame(
    evidence_axis = axis_value,
    umbrella_axis = paste(umbrella_values, collapse = ";"),
    n_supporting_layers = length(layer_values),
    supporting_layers = paste(layer_values, collapse = ";"),
    n_evidence_rows = nrow(sub_axis),
    evidence_unit_types = paste(unit_type_values, collapse = ";"),
    total_weighted_evidence_score = score_sum,
    mean_weighted_evidence_score = score_mean,
    high_support_rows = high_rows,
    risk_context_flag = risk_flag,
    final_integrated_axis_tier = tier_value,
    allowed_interpretation = "evidence-axis-level support for transcriptomic prioritisation only",
    prohibited_interpretation = "not clinical prediction; not validated biomarker; not causal proof",
    stringsAsFactors = FALSE
  )
}
axis_df <- safe_bind_rows(axis_list)
axis_df <- axis_df[order(axis_df$n_supporting_layers, axis_df$total_weighted_evidence_score, decreasing = TRUE), , drop = FALSE]
write_csv_safe(axis_df, file.path(out_table_dir, "11H_FINAL_integrated_evidence_axis_tier_table.csv"))
write_tsv_safe(axis_df, file.path(out_table_dir, "11H_FINAL_integrated_evidence_axis_tier_table.tsv"))

# ------------------------- umbrella evidence tier table - MAIN 11H -------------------------
umbrella_values <- unique(evidence_df$umbrella_axis)
umbrella_list <- list()
for (idx_value in seq_along(umbrella_values)) {
  umbrella_value <- umbrella_values[idx_value]
  sub_um <- evidence_df[evidence_df$umbrella_axis == umbrella_value, , drop = FALSE]
  layer_values <- sort(unique(sub_um$source_layer))
  axis_sub <- sort(unique(sub_um$evidence_axis))
  score_sum <- sum(safe_num(sub_um$weighted_evidence_score), na.rm = TRUE)
  score_mean <- mean(safe_num(sub_um$weighted_evidence_score), na.rm = TRUE)
  if (!is.finite(score_mean)) score_mean <- 0
  risk_flag <- grepl("risk", umbrella_value, ignore.case = TRUE)

  tier_value <- "umbrella_Tier3_single_layer_or_context_support"
  if (length(layer_values) >= 5 && score_sum >= 8) tier_value <- "umbrella_Tier1_evidence_anchored_framework_support"
  if (length(layer_values) >= 3 && score_sum >= 4 && tier_value == "umbrella_Tier3_single_layer_or_context_support") tier_value <- "umbrella_Tier2_multi_layer_support"
  if (risk_flag) tier_value <- paste0(tier_value, "_with_risk_context")
  if (score_sum <= 0) tier_value <- "umbrella_limited_or_no_detected_support"

  umbrella_list[[length(umbrella_list) + 1]] <- data.frame(
    umbrella_axis = umbrella_value,
    n_supporting_layers = length(layer_values),
    supporting_layers = paste(layer_values, collapse = ";"),
    supporting_evidence_axes = paste(axis_sub, collapse = ";"),
    n_evidence_rows = nrow(sub_um),
    total_weighted_evidence_score = score_sum,
    mean_weighted_evidence_score = score_mean,
    risk_context_flag = risk_flag,
    final_umbrella_tier = tier_value,
    allowed_interpretation = "main 11H integrated evidence tier for transcriptomic prioritisation only",
    prohibited_interpretation = "not clinical prediction; not validated clinical biomarker; not causal proof",
    stringsAsFactors = FALSE
  )
}
umbrella_df <- safe_bind_rows(umbrella_list)
umbrella_df <- umbrella_df[order(umbrella_df$n_supporting_layers, umbrella_df$total_weighted_evidence_score, decreasing = TRUE), , drop = FALSE]
write_csv_safe(umbrella_df, file.path(out_table_dir, "11H_FINAL_integrated_umbrella_evidence_tier_table.csv"))
write_tsv_safe(umbrella_df, file.path(out_table_dir, "11H_FINAL_integrated_umbrella_evidence_tier_table.tsv"))

# ------------------------- candidate marker signature table -------------------------
marker_list <- list()

file_11g_marker <- first_file(c(
  file.path(table_root, "11G_PD_GWAS_genetic_context_support_FINAL_COMPLETE_STANDALONE", "11G_FINAL_candidate_marker_signature_genes_with_PD_genetic_context.csv"),
  find_files_all_terms(c("11g", "candidate_marker_signature_genes"), max_n = 10)
))
data_11g_marker <- read_table_safe(file_11g_marker)
if (nrow(data_11g_marker) > 0) {
  gene_col_names <- colnames(data_11g_marker)[grepl("gene_symbol|gene$|symbol", tolower(colnames(data_11g_marker)))]
  if (length(gene_col_names) > 0) {
    gene_values <- clean_gene_symbol(data_11g_marker[[gene_col_names[1]]])
    marker_list[[length(marker_list) + 1]] <- data.frame(
      gene_symbol = gene_values,
      source_layer = "11G_PD_genetic_context_support",
      marker_evidence_type = "PD_genetic_context_overlap",
      marker_direction = "candidate_marker_signature",
      evidence_score = 1,
      source_file = file_11g_marker,
      stringsAsFactors = FALSE
    )
  }
}

marker_list[[length(marker_list) + 1]] <- data.frame(
  gene_symbol = known_11d_proxy_genes,
  source_layer = "11D_survival_stress_CRISPR_proxy_support",
  marker_evidence_type = ifelse(known_11d_proxy_genes %in% known_11d_overlap_genes, "CRISPR_proxy_overlap_gene", "CRISPR_proxy_candidate_gene"),
  marker_direction = "risk_survival_stress_associated",
  evidence_score = ifelse(known_11d_proxy_genes %in% known_11d_overlap_genes, 1, 0.65),
  source_file = ifelse(length(file_11d_values) > 0, file_11d_values[1], "locked_11D_manual_summary"),
  stringsAsFactors = FALSE
)

file_builtin_marker <- first_file(c(
  file.path(table_root, "11G_PD_GWAS_genetic_context_support_FINAL_COMPLETE_STANDALONE", "11G_FINAL_builtin_candidate_marker_signature_genes.csv"),
  find_files_all_terms(c("11g", "builtin_candidate_marker_signature_genes"), max_n = 10)
))
data_builtin_marker <- read_table_safe(file_builtin_marker)
if (nrow(data_builtin_marker) > 0) {
  gene_col_names <- colnames(data_builtin_marker)[grepl("gene_symbol|gene$|symbol", tolower(colnames(data_builtin_marker)))]
  source_col_names <- colnames(data_builtin_marker)[grepl("source_name|module|signature", tolower(colnames(data_builtin_marker)))]
  direction_col_names <- colnames(data_builtin_marker)[grepl("direction|role", tolower(colnames(data_builtin_marker)))]
  if (length(gene_col_names) > 0) {
    gene_values <- clean_gene_symbol(data_builtin_marker[[gene_col_names[1]]])
    source_values <- ifelse(length(source_col_names) > 0, safe_chr(data_builtin_marker[[source_col_names[1]]]), "built_in_project_signature")
    direction_values <- ifelse(length(direction_col_names) > 0, safe_chr(data_builtin_marker[[direction_col_names[1]]]), "project_signature_gene")
    marker_list[[length(marker_list) + 1]] <- data.frame(
      gene_symbol = gene_values,
      source_layer = "project_marker_signature_catalog",
      marker_evidence_type = source_values,
      marker_direction = direction_values,
      evidence_score = 0.5,
      source_file = file_builtin_marker,
      stringsAsFactors = FALSE
    )
  }
}

if (nrow(rows_09c) > 0) {
  possible_genes_09c <- clean_gene_symbol(rows_09c$evidence_unit_label)
  possible_genes_09c <- possible_genes_09c[possible_genes_09c != ""]
  if (length(possible_genes_09c) > 0) {
    marker_list[[length(marker_list) + 1]] <- data.frame(
      gene_symbol = possible_genes_09c,
      source_layer = "09C_weak_label_ML_priority_model",
      marker_evidence_type = "ML_feature_or_priority_associated",
      marker_direction = "ML_priority_associated",
      evidence_score = 0.8,
      source_file = rows_09c$source_file[1],
      stringsAsFactors = FALSE
    )
  }
}

marker_raw_df <- safe_bind_rows(marker_list)
marker_raw_df$gene_symbol <- clean_gene_symbol(marker_raw_df$gene_symbol)
marker_raw_df <- marker_raw_df[marker_raw_df$gene_symbol != "", , drop = FALSE]
write_csv_safe(marker_raw_df, file.path(out_table_dir, "11H_FINAL_candidate_marker_signature_raw_evidence_rows.csv"))

marker_gene_values <- sort(unique(marker_raw_df$gene_symbol))
marker_summary_list <- list()
for (idx_value in seq_along(marker_gene_values)) {
  gene_value <- marker_gene_values[idx_value]
  sub_marker <- marker_raw_df[marker_raw_df$gene_symbol == gene_value, , drop = FALSE]
  layers <- sort(unique(sub_marker$source_layer))
  evidence_types <- sort(unique(sub_marker$marker_evidence_type))
  direction_text <- paste(sort(unique(sub_marker$marker_direction)), collapse = ";")
  score_value <- sum(safe_num(sub_marker$evidence_score), na.rm = TRUE)
  has_11d <- any(sub_marker$source_layer == "11D_survival_stress_CRISPR_proxy_support")
  has_11g <- any(sub_marker$source_layer == "11G_PD_genetic_context_support")
  has_09c <- any(sub_marker$source_layer == "09C_weak_label_ML_priority_model")
  is_risk <- grepl("risk|stress|p53|apoptosis|inflammatory|proliferation", direction_text, ignore.case = TRUE)

  marker_tier <- "candidate_marker_tier_3_single_source"
  if (length(layers) >= 3 || (has_11d && has_11g && has_09c)) marker_tier <- "candidate_marker_tier_1_multi_evidence"
  if (length(layers) >= 2 && marker_tier == "candidate_marker_tier_3_single_source") marker_tier <- "candidate_marker_tier_2_dual_evidence"
  if (is_risk) marker_tier <- paste0(marker_tier, "_risk_context")

  marker_summary_list[[length(marker_summary_list) + 1]] <- data.frame(
    gene_symbol = gene_value,
    n_marker_evidence_layers = length(layers),
    marker_evidence_layers = paste(layers, collapse = ";"),
    marker_evidence_types = paste(evidence_types, collapse = ";"),
    marker_direction_summary = direction_text,
    marker_evidence_score = score_value,
    has_09C_ML_feature_support = has_09c,
    has_10K_pseudotime_support = FALSE,
    has_11C_preclinical_support = FALSE,
    has_11D_perturbation_CRISPR_proxy_support = has_11d,
    has_11E_state_proxy_support = FALSE,
    has_11F_projection_support = FALSE,
    has_11G_PD_genetic_context_support = has_11g,
    final_candidate_marker_signature_tier = marker_tier,
    claim_boundary = "candidate transcriptomic marker signature only; not a clinical biomarker",
    stringsAsFactors = FALSE
  )
}
marker_summary_df <- safe_bind_rows(marker_summary_list)
marker_summary_df <- marker_summary_df[order(marker_summary_df$n_marker_evidence_layers, marker_summary_df$marker_evidence_score, decreasing = TRUE), , drop = FALSE]
write_csv_safe(marker_summary_df, file.path(out_table_dir, "11H_FINAL_candidate_transcriptomic_marker_signature_table.csv"))
write_tsv_safe(marker_summary_df, file.path(out_table_dir, "11H_FINAL_candidate_transcriptomic_marker_signature_table.tsv"))

# ------------------------- summaries and claim boundary -------------------------
layer_audit_df <- safe_bind_rows(layer_audit_list)
write_csv_safe(layer_audit_df, file.path(out_table_dir, "11H_FINAL_upstream_layer_import_audit.csv"))

layer_summary_list <- list()
layer_values <- sort(unique(evidence_df$source_layer))
for (idx_value in seq_along(layer_values)) {
  layer_value <- layer_values[idx_value]
  sub_layer <- evidence_df[evidence_df$source_layer == layer_value, , drop = FALSE]
  layer_summary_list[[length(layer_summary_list) + 1]] <- data.frame(
    source_layer = layer_value,
    evidence_rows = nrow(sub_layer),
    unique_units = length(unique(sub_layer$exact_integration_key)),
    evidence_axes = paste(sort(unique(sub_layer$evidence_axis)), collapse = ";"),
    umbrella_axes = paste(sort(unique(sub_layer$umbrella_axis)), collapse = ";"),
    high_rows = sum(grepl("high", sub_layer$evidence_support_level, ignore.case = TRUE), na.rm = TRUE),
    intermediate_rows = sum(grepl("intermediate|moderate", sub_layer$evidence_support_level, ignore.case = TRUE), na.rm = TRUE),
    low_rows = sum(grepl("low", sub_layer$evidence_support_level, ignore.case = TRUE), na.rm = TRUE),
    total_weighted_score = sum(safe_num(sub_layer$weighted_evidence_score), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}
layer_summary_df <- safe_bind_rows(layer_summary_list)
layer_summary_df <- layer_summary_df[order(layer_summary_df$total_weighted_score, decreasing = TRUE), , drop = FALSE]
write_csv_safe(layer_summary_df, file.path(out_table_dir, "11H_FINAL_layer_level_evidence_summary.csv"))

umbrella_tier_counts <- as.data.frame(table(umbrella_df$final_umbrella_tier), stringsAsFactors = FALSE)
colnames(umbrella_tier_counts) <- c("final_umbrella_tier", "umbrella_count")
umbrella_tier_counts <- umbrella_tier_counts[order(umbrella_tier_counts$umbrella_count, decreasing = TRUE), , drop = FALSE]
write_csv_safe(umbrella_tier_counts, file.path(out_table_dir, "11H_FINAL_integrated_umbrella_evidence_tier_counts.csv"))

axis_tier_counts <- as.data.frame(table(axis_df$final_integrated_axis_tier), stringsAsFactors = FALSE)
colnames(axis_tier_counts) <- c("final_integrated_axis_tier", "axis_count")
axis_tier_counts <- axis_tier_counts[order(axis_tier_counts$axis_count, decreasing = TRUE), , drop = FALSE]
write_csv_safe(axis_tier_counts, file.path(out_table_dir, "11H_FINAL_integrated_evidence_axis_tier_counts.csv"))

unit_tier_counts <- as.data.frame(table(unit_df$final_exact_unit_evidence_tier), stringsAsFactors = FALSE)
colnames(unit_tier_counts) <- c("final_exact_unit_evidence_tier", "unit_count")
unit_tier_counts <- unit_tier_counts[order(unit_tier_counts$unit_count, decreasing = TRUE), , drop = FALSE]
write_csv_safe(unit_tier_counts, file.path(out_table_dir, "11H_FINAL_integrated_evidence_tier_counts.csv"))

claim_boundary_df <- data.frame(
  category = c(
    "allowed",
    "allowed",
    "allowed",
    "allowed",
    "allowed",
    "prohibited",
    "prohibited",
    "prohibited",
    "prohibited",
    "prohibited",
    "prohibited"
  ),
  statement = c(
    "Evidence-anchored transcriptomic prioritisation framework",
    "Integrated umbrella evidence tiers across ML, pseudotime, preclinical marker support, perturbation proxy, state proxy, projection proxy, and limited genetic context",
    "Candidate transcriptomic marker signatures",
    "State-level proxy support where strict lineage/projection metadata are absent",
    "Conservative PD genetic-context support when overlap is detected",
    "Clinical prediction model",
    "Diagnostic biomarker discovery",
    "Prognostic biomarker validation",
    "Therapeutic response biomarker validation",
    "True anatomical projection or host integration proof",
    "Barcode-confirmed lineage tracing where strict barcode/clone metadata are absent"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(claim_boundary_df, file.path(out_table_dir, "11H_FINAL_claim_boundary.csv"))

# ------------------------- figures -------------------------
# FigA: layer audit and claim boundary
fig_a <- open_pdf_safe("11H_FINAL_FigA_evidence_layer_audit_and_claim_boundary.pdf", 11.4, 6.6)
new_canvas()
draw_title("11H integrated evidence layer audit", "Complete standalone integration; conservative claim boundary retained.")
plot_layers <- layer_audit_df
if (nrow(plot_layers) > 0) {
  y_values <- seq(0.78, 0.34, length.out = nrow(plot_layers))
  max_rows <- max(safe_num(plot_layers$rows_imported), na.rm = TRUE)
  if (!is.finite(max_rows) || max_rows < 1) max_rows <- 1
  bar_x0 <- 0.28
  bar_x1 <- 0.70
  for (idx_value in seq_len(nrow(plot_layers))) {
    yy <- y_values[idx_value]
    row_val <- safe_num(plot_layers$rows_imported[idx_value])
    width_val <- row_val / max_rows
    text(bar_x0 - 0.018, yy, plot_layers$layer[idx_value], cex = 0.62, adj = c(1, 0.5), col = "gray10")
    rect(bar_x0, yy - 0.022, bar_x0 + width_val * (bar_x1 - bar_x0), yy + 0.022,
         col = value_to_gray(row_val, max_rows), border = "gray35", lwd = 0.5)
    text(bar_x0 + width_val * (bar_x1 - bar_x0) + 0.012, yy,
         paste0(row_val, " rows"), cex = 0.52, adj = c(0, 0.5), col = "gray15")
    text(0.88, yy, plot_layers$status[idx_value], cex = 0.46, adj = c(0.5, 0.5), col = "gray35")
  }
}
text(0.08, 0.18, "Allowed", cex = 0.68, font = 2, adj = c(0, 0.5), col = "gray15")
text(0.18, 0.18, "Umbrella evidence-tier transcriptomic prioritisation; candidate marker signatures; proxy support only.", cex = 0.50, adj = c(0, 0.5), col = "gray25")
text(0.08, 0.12, "Prohibited", cex = 0.68, font = 2, adj = c(0, 0.5), col = "gray15")
text(0.18, 0.12, "Clinical prediction, validated biomarker, true lineage/projection proof, causal graft efficacy claim.", cex = 0.50, adj = c(0, 0.5), col = "gray25")
dev.off()
cat("[11H FINAL] Wrote figure:", fig_a, "\n")

# FigB: evidence-axis support matrix by upstream layer
fig_b <- open_pdf_safe("11H_FINAL_FigB_integrated_evidence_support_matrix.pdf", 12.4, 7.0)
new_canvas()
draw_title("Integrated evidence-axis support matrix", "Rows are biological evidence axes; columns are locked upstream evidence layers.")
axis_order <- axis_df$evidence_axis
layer_values <- sort(unique(evidence_df$source_layer))
if (length(axis_order) > 0 && length(layer_values) > 0) {
  mat_counts <- matrix(0, nrow = length(axis_order), ncol = length(layer_values))
  rownames(mat_counts) <- axis_order
  colnames(mat_counts) <- layer_values
  for (row_idx in seq_along(axis_order)) {
    for (col_idx in seq_along(layer_values)) {
      mat_counts[row_idx, col_idx] <- sum(evidence_df$evidence_axis == axis_order[row_idx] & evidence_df$source_layer == layer_values[col_idx], na.rm = TRUE)
    }
  }
  max_count <- max(mat_counts, na.rm = TRUE)
  if (!is.finite(max_count) || max_count < 1) max_count <- 1
  hm_x0 <- 0.33
  hm_x1 <- 0.90
  hm_y0 <- 0.20
  hm_y1 <- 0.84
  nr <- nrow(mat_counts)
  nc <- ncol(mat_counts)
  cell_w <- (hm_x1 - hm_x0) / nc
  cell_h <- (hm_y1 - hm_y0) / nr
  for (row_idx in seq_len(nr)) {
    for (col_idx in seq_len(nc)) {
      val <- mat_counts[row_idx, col_idx]
      rect(
        hm_x0 + (col_idx - 1) * cell_w,
        hm_y1 - row_idx * cell_h,
        hm_x0 + col_idx * cell_w,
        hm_y1 - (row_idx - 1) * cell_h,
        col = value_to_gray(val, max_count),
        border = "white",
        lwd = 0.4
      )
      if (val > 0) {
        text(
          hm_x0 + (col_idx - 0.5) * cell_w,
          hm_y1 - (row_idx - 0.5) * cell_h,
          as.character(val),
          cex = 0.42,
          col = ifelse(val / max_count > 0.55, "white", "gray20")
        )
      }
    }
  }
  rect(hm_x0, hm_y0, hm_x1, hm_y1, border = "gray35", lwd = 0.7)
  for (row_idx in seq_len(nr)) {
    yy <- hm_y1 - (row_idx - 0.5) * cell_h
    text(hm_x0 - 0.012, yy, rownames(mat_counts)[row_idx], cex = 0.42, adj = c(1, 0.5), col = "gray10")
  }
  for (col_idx in seq_len(nc)) {
    xx <- hm_x0 + (col_idx - 0.5) * cell_w
    short_lab <- gsub("_support|_model|_state_level|_marker|_context|_multi_timepoint|_weak_label|_graft_outcome", "", colnames(mat_counts)[col_idx])
    text(xx, 0.105, short_lab, cex = 0.38, srt = 90, adj = c(0.5, 0.5), col = "gray10")
  }
  text(0.94, 0.70, "row count", cex = 0.48, srt = 90, col = "gray30")
} else {
  text(0.5, 0.5, "No evidence-axis matrix available.", cex = 0.8)
}
dev.off()
cat("[11H FINAL] Wrote figure:", fig_b, "\n")

# FigC: candidate marker evidence matrix
fig_c <- open_pdf_safe("11H_FINAL_FigC_candidate_marker_signature_evidence_matrix.pdf", 12.2, 7.2)
new_canvas()
draw_title("Candidate transcriptomic marker signature evidence matrix", "Candidate marker signatures only; not clinical biomarker validation.")
plot_marker <- marker_summary_df
if (nrow(plot_marker) > 0) {
  plot_marker <- plot_marker[order(plot_marker$n_marker_evidence_layers, plot_marker$marker_evidence_score, decreasing = TRUE), , drop = FALSE]
  plot_marker <- plot_marker[seq_len(min(24, nrow(plot_marker))), , drop = FALSE]
  mat_marker <- cbind(
    ML_09C = as.integer(plot_marker$has_09C_ML_feature_support),
    Pseudotime_10K = as.integer(plot_marker$has_10K_pseudotime_support),
    Perturbation_11D = as.integer(plot_marker$has_11D_perturbation_CRISPR_proxy_support),
    Projection_11F = as.integer(plot_marker$has_11F_projection_support),
    PD_Genetic_11G = as.integer(plot_marker$has_11G_PD_genetic_context_support)
  )
  hm_x0 <- 0.30
  hm_x1 <- 0.82
  hm_y0 <- 0.13
  hm_y1 <- 0.86
  nr <- nrow(mat_marker)
  nc <- ncol(mat_marker)
  cell_w <- (hm_x1 - hm_x0) / nc
  cell_h <- (hm_y1 - hm_y0) / nr
  for (row_idx in seq_len(nr)) {
    for (col_idx in seq_len(nc)) {
      val <- mat_marker[row_idx, col_idx]
      rect(
        hm_x0 + (col_idx - 1) * cell_w,
        hm_y1 - row_idx * cell_h,
        hm_x0 + col_idx * cell_w,
        hm_y1 - (row_idx - 1) * cell_h,
        col = ifelse(val > 0, "gray35", "gray93"),
        border = "white",
        lwd = 0.4
      )
    }
  }
  rect(hm_x0, hm_y0, hm_x1, hm_y1, border = "gray35", lwd = 0.7)
  for (row_idx in seq_len(nr)) {
    yy <- hm_y1 - (row_idx - 0.5) * cell_h
    text(hm_x0 - 0.012, yy, plot_marker$gene_symbol[row_idx], cex = 0.48, adj = c(1, 0.5), col = "gray10")
    tier_label <- gsub("candidate_marker_", "", plot_marker$final_candidate_marker_signature_tier[row_idx])
    tier_label <- gsub("_", " ", tier_label)
    text(hm_x1 + 0.012, yy, tier_label, cex = 0.34, adj = c(0, 0.5), col = "gray35")
  }
  for (col_idx in seq_len(nc)) {
    xx <- hm_x0 + (col_idx - 0.5) * cell_w
    text(xx, 0.075, colnames(mat_marker)[col_idx], cex = 0.42, srt = 90, adj = c(0.5, 0.5), col = "gray10")
  }
} else {
  text(0.5, 0.5, "No candidate marker signature rows available.", cex = 0.8)
}
dev.off()
cat("[11H FINAL] Wrote figure:", fig_c, "\n")

# FigD: final umbrella tier summary - MAIN output
fig_d <- open_pdf_safe("11H_FINAL_FigD_final_integrated_evidence_tier_summary.pdf", 11.4, 6.5)
new_canvas()
draw_title("Final integrated umbrella evidence-tier summary", "Main 11H output: evidence-anchored framework support, not exact-label-only aggregation.")
plot_um <- umbrella_df
if (nrow(plot_um) > 0) {
  plot_um <- plot_um[order(plot_um$n_supporting_layers, plot_um$total_weighted_evidence_score, decreasing = TRUE), , drop = FALSE]
  y_pos <- seq(0.76, 0.32, length.out = nrow(plot_um))
  max_score <- max(safe_num(plot_um$total_weighted_evidence_score), na.rm = TRUE)
  if (!is.finite(max_score) || max_score < 1) max_score <- 1
  bar_x0 <- 0.38
  bar_x1 <- 0.82
  for (idx_value in seq_len(nrow(plot_um))) {
    yy <- y_pos[idx_value]
    val <- safe_num(plot_um$total_weighted_evidence_score[idx_value])
    width_val <- val / max_score
    label_value <- gsub("_", " ", plot_um$umbrella_axis[idx_value])
    tier_text <- gsub("umbrella_", "", plot_um$final_umbrella_tier[idx_value])
    tier_text <- gsub("_", " ", tier_text)
    text(bar_x0 - 0.02, yy, label_value, cex = 0.48, adj = c(1, 0.5), col = "gray10")
    rect(bar_x0, yy - 0.030, bar_x0 + width_val * (bar_x1 - bar_x0), yy + 0.030,
         col = value_to_gray(val, max_score), border = "gray35", lwd = 0.5)
    text(bar_x0 + width_val * (bar_x1 - bar_x0) + 0.012, yy,
         paste0("L", plot_um$n_supporting_layers[idx_value], " | ", round(val, 1)), cex = 0.44, adj = c(0, 0.5), col = "gray10")
    text(0.93, yy, tier_text, cex = 0.34, adj = c(0.5, 0.5), col = "gray35")
  }
  text(0.5, 0.16, "L = number of supporting upstream evidence layers; score = weighted evidence sum", cex = 0.52, col = "gray30")
  text(0.5, 0.11, "High-priority umbrella integrates ML, pseudotime, preclinical, state-proxy and projection-proxy layers.", cex = 0.50, col = "gray35")
} else {
  text(0.5, 0.5, "No umbrella tier summary available.", cex = 0.8)
}
dev.off()
cat("[11H FINAL] Wrote figure:", fig_d, "\n")

# ------------------------- execution summary and report -------------------------
summary_df <- data.frame(
  item = c(
    "all_imported_evidence_rows",
    "exact_integrated_evidence_units",
    "biological_evidence_axes",
    "umbrella_evidence_axes",
    "candidate_marker_signature_genes",
    "upstream_layers_imported_with_rows",
    "11C_rows",
    "11D_rows",
    "11E_rows",
    "11F_rows",
    "11G_rows",
    "09C_rows",
    "10K_rows",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(nrow(evidence_df)),
    as.character(nrow(unit_df)),
    as.character(nrow(axis_df)),
    as.character(nrow(umbrella_df)),
    as.character(nrow(marker_summary_df)),
    as.character(sum(layer_audit_df$rows_imported > 0, na.rm = TRUE)),
    as.character(nrow(rows_11c)),
    as.character(nrow(rows_11d)),
    as.character(nrow(rows_11e)),
    as.character(nrow(rows_11f)),
    as.character(nrow(rows_11g)),
    as.character(nrow(rows_09c)),
    as.character(nrow(rows_10k)),
    "4",
    "INPUT_READY_FOR_11I_MODULE_CORRELATION_AND_11J_ML_AUDIT"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "11H_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "11H_FINAL_execution_summary.tsv"))

report_lines <- c(
  "11H FINAL report",
  "================",
  "Module: integrated evidence tier + candidate transcriptomic marker signature table",
  "Mode: complete standalone 11H rebuild; no previous 11H output dependency; no internet; no 00-10P rerun.",
  "Integration mode: umbrella evidence-tier integration is the main 11H output.",
  "",
  paste0("All imported evidence rows: ", nrow(evidence_df)),
  paste0("Exact integrated evidence units: ", nrow(unit_df)),
  paste0("Biological evidence axes: ", nrow(axis_df)),
  paste0("Umbrella evidence axes: ", nrow(umbrella_df)),
  paste0("Candidate marker signature genes: ", nrow(marker_summary_df)),
  paste0("Upstream layers with imported rows: ", sum(layer_audit_df$rows_imported > 0, na.rm = TRUE)),
  "",
  "Main 11H output tables:",
  paste0("- ", file.path(out_table_dir, "11H_FINAL_integrated_umbrella_evidence_tier_table.csv")),
  paste0("- ", file.path(out_table_dir, "11H_FINAL_integrated_evidence_axis_tier_table.csv")),
  paste0("- ", file.path(out_table_dir, "11H_FINAL_integrated_evidence_unit_tier_table.csv")),
  paste0("- ", file.path(out_table_dir, "11H_FINAL_candidate_transcriptomic_marker_signature_table.csv")),
  paste0("- ", file.path(out_table_dir, "11H_FINAL_all_imported_evidence_rows.csv")),
  paste0("- ", file.path(out_table_dir, "11H_FINAL_claim_boundary.csv")),
  "",
  "Allowed interpretation:",
  "- Evidence-anchored transcriptomic prioritisation framework.",
  "- Integrated umbrella evidence tiers for DA neuron / graft-related cell-state prioritisation.",
  "- Candidate transcriptomic marker signatures.",
  "",
  "Prohibited interpretation:",
  "- No clinical prediction.",
  "- No diagnostic/prognostic/therapeutic-response biomarker validation.",
  "- No true anatomical-projection claim.",
  "- No barcode-lineage claim proof.",
  "- No causal graft efficacy/safety claim.",
  "",
  "Decision: INPUT_READY_FOR_11I_MODULE_CORRELATION_AND_11J_ML_AUDIT"
)
report_file <- file.path(out_text_dir, "11H_FINAL_integrated_evidence_tier_and_marker_signature_report.txt")
writeLines(report_lines, report_file)
cat("[11H FINAL] Wrote:", report_file, "\n")

cat("\n[11H FINAL] Completed integrated evidence tier + candidate marker signature table.\n")
cat("[11H FINAL] All imported evidence rows:", nrow(evidence_df), "\n")
cat("[11H FINAL] Exact integrated evidence units:", nrow(unit_df), "\n")
cat("[11H FINAL] Biological evidence axes:", nrow(axis_df), "\n")
cat("[11H FINAL] Umbrella evidence axes:", nrow(umbrella_df), "\n")
cat("[11H FINAL] Candidate marker signature genes:", nrow(marker_summary_df), "\n")
cat("[11H FINAL] Upstream layers with imported rows:", sum(layer_audit_df$rows_imported > 0, na.rm = TRUE), "\n")
cat("[11H FINAL] 11C rows:", nrow(rows_11c), "\n")
cat("[11H FINAL] 11D rows:", nrow(rows_11d), "\n")
cat("[11H FINAL] 11E rows:", nrow(rows_11e), "\n")
cat("[11H FINAL] 11F rows:", nrow(rows_11f), "\n")
cat("[11H FINAL] 11G rows:", nrow(rows_11g), "\n")
cat("[11H FINAL] 09C rows:", nrow(rows_09c), "\n")
cat("[11H FINAL] 10K rows:", nrow(rows_10k), "\n")
cat("[11H FINAL] Figures written: 4\n")
cat("[11H FINAL] Decision: INPUT_READY_FOR_11I_MODULE_CORRELATION_AND_11J_ML_AUDIT\n")
cat("[11H FINAL] Output tables:", out_table_dir, "\n")
cat("[11H FINAL] Output figs  :", out_fig_dir, "\n")
cat("[11H FINAL] Output text  :", out_text_dir, "\n")
cat("[11H FINAL] Next         : review 11H FINAL PDFs; if accepted, proceed to 11I module-score correlation.\n")
