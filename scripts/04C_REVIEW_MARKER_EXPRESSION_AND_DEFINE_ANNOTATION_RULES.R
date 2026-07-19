
PROJECT_DIR <- "D:/PD_Graft_Project"

MIN_COVERAGE <- 0.4
MIN_MEAN_SCORE <- 0.05
MIN_PCT_CELLS_SCORE_GT0 <- 0.05

MAX_REVIEW_GROUPS_PER_CATEGORY <- 200

cat("\n============================================================\n")
cat("04C：review marker expression and define annotation rules\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 04C。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_group_category <- file.path(tables_dir, "04B_marker_expression", "04B_group_marker_category_scores.csv")
input_group_gene <- file.path(tables_dir, "04B_marker_expression", "04B_group_marker_gene_expression.csv")
input_prelim <- file.path(tables_dir, "04B_marker_expression", "04B_preliminary_annotation_suggestions.csv")
input_object_category <- file.path(tables_dir, "04B_marker_expression", "04B_object_marker_category_scores.csv")
input_failed <- file.path(tables_dir, "04B_marker_expression", "04B_failed_objects.csv")
input_matrix_audit <- file.path(tables_dir, "04B_marker_expression", "04B_matrix_source_audit.csv")
input_marker_panel <- file.path(tables_dir, "04A_annotation_prep", "04A_marker_panel_master.csv")
input_dataset_role <- file.path(tables_dir, "03C_strategy", "03C_dataset_role_and_usage.csv")

out_tables_dir <- file.path(tables_dir, "04C_annotation_review")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

annotation_rules_csv <- file.path(out_tables_dir, "04C_annotation_rule_table.csv")
suggestion_summary_csv <- file.path(out_tables_dir, "04C_preliminary_suggestion_summary_by_dataset.csv")
dataset_category_signal_csv <- file.path(out_tables_dir, "04C_dataset_marker_category_signal_summary.csv")
candidate_groups_csv <- file.path(out_tables_dir, "04C_candidate_groups_for_manual_review.csv")
da_a9_a10_csv <- file.path(out_tables_dir, "04C_DA_A9_A10_candidate_groups.csv")
safety_risk_csv <- file.path(out_tables_dir, "04C_safety_risk_candidate_groups.csv")
marker_gene_snapshot_csv <- file.path(out_tables_dir, "04C_key_marker_gene_snapshot.csv")
qc_audit_csv <- file.path(out_tables_dir, "04C_04B_QC_audit_summary.csv")
report_txt <- file.path(reports_dir, "04C_marker_review_and_annotation_rules_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path)
  }
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) {
    return(data.frame())
  }
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

category_signal_flag <- function(mean_score, coverage_fraction, pct_cells_score_gt0) {
  !is.na(mean_score) &
    !is.na(coverage_fraction) &
    !is.na(pct_cells_score_gt0) &
    coverage_fraction >= MIN_COVERAGE &
    mean_score >= MIN_MEAN_SCORE &
    pct_cells_score_gt0 >= MIN_PCT_CELLS_SCORE_GT0
}

wide_category_scores <- function(dt) {

  base_cols <- c("dataset", "object_id", "group_source", "group_id", "n_cells")

  mean_wide <- dcast(
    dt,
    dataset + object_id + group_source + group_id + n_cells ~ category,
    value.var = "mean_score",
    fun.aggregate = max,
    fill = NA_real_
  )

  mean_wide
}

get_cat <- function(row, cat) {
  if (!cat %in% names(row)) return(NA_real_)
  as.numeric(row[[cat]])
}

stamp("读取 04B 输出。")

group_category <- read_csv_required(input_group_category)
group_gene <- read_csv_required(input_group_gene)
prelim <- read_csv_required(input_prelim)
object_category <- read_csv_required(input_object_category)
failed <- read_csv_optional(input_failed)
matrix_audit <- read_csv_optional(input_matrix_audit)
marker_panel <- read_csv_required(input_marker_panel)
dataset_role <- read_csv_optional(input_dataset_role)

gc_dt <- as.data.table(group_category)
gg_dt <- as.data.table(group_gene)
prelim_dt <- as.data.table(prelim)
oc_dt <- as.data.table(object_category)
failed_dt <- as.data.table(failed)
matrix_dt <- as.data.table(matrix_audit)

needed_gc <- c("dataset", "object_id", "group_id", "category", "mean_score", "coverage_fraction", "pct_cells_score_gt0")
if (!all(needed_gc %in% colnames(gc_dt))) {
  stop("04B_group_marker_category_scores 缺少必要列：", paste(setdiff(needed_gc, colnames(gc_dt)), collapse = ", "))
}

needed_prelim <- c("dataset", "object_id", "group_id", "preliminary_suggestion")
if (!all(needed_prelim %in% colnames(prelim_dt))) {
  stop("04B_preliminary_annotation_suggestions 缺少必要列。")
}

stamp("生成 annotation rule table。")

annotation_rules <- data.frame(
  proposed_label = c(
    "DA_like_neuronal_candidate",
    "A9_like_DA_supported_candidate",
    "A10_like_DA_supported_candidate",
    "midbrain_progenitor_like_candidate",
    "cycling_progenitor_safety_risk_candidate",
    "immature_pluripotency_risk_signal_candidate",
    "astrocyte_glial_candidate",
    "oligodendrocyte_OPC_candidate",
    "immune_microglia_macrophage_candidate",
    "vascular_mesenchymal_candidate",
    "GABAergic_neuronal_candidate",
    "glutamatergic_neuronal_candidate",
    "stress_response_high_candidate",
    "unassigned_low_marker_signal"
  ),
  required_positive_categories = c(
    "DA_core_identity;neuronal_maturation_synapse",
    "DA_core_identity;A9_like_DA_identity",
    "DA_core_identity;A10_like_DA_identity",
    "midbrain_floor_plate_progenitor;progenitor_neuroepithelial",
    "cell_cycle_proliferation;progenitor_neuroepithelial",
    "pluripotency_immature_risk",
    "astrocyte_glial",
    "oligodendrocyte_OPC",
    "microglia_macrophage_immune",
    "vascular_pericyte_meningeal OR extracellular_matrix_fibroblast",
    "GABAergic_neuron",
    "glutamatergic_neuron",
    "stress_apoptosis_response",
    "none"
  ),
  supporting_marker_examples = c(
    "TH/DDC/SLC6A3/SLC18A2/NR4A2 + RBFOX3/MAP2/SNAP25/SYT1",
    "ALDH1A1/KCNJ6/SOX6/DCLK3/GCH1 with DA core",
    "CALB1/OTX2/CCK/SLC17A6 with DA core",
    "FOXA2/LMX1A/LMX1B/OTX2/CORIN + SOX2/NES/HES1",
    "MKI67/TOP2A/PCNA/MCM2/CENPF + SOX2/NES/PAX6",
    "POU5F1/NANOG/LIN28A/DPPA4/TERT/PROM1",
    "GFAP/AQP4/ALDH1L1/SLC1A3/S100B",
    "OLIG1/OLIG2/PDGFRA/SOX10/MBP/PLP1",
    "PTPRC/AIF1/C1QA/TYROBP/LST1/CD74",
    "PECAM1/VWF/CLDN5/PDGFRB/RGS5/COL1A1/DCN",
    "GAD1/GAD2/SLC32A1/DLX1/DLX2",
    "SLC17A6/SLC17A7/SLC17A8/TBR1/NEUROD6",
    "FOS/JUN/HSPA1A/DDIT3/ATF3/BAX",
    "No robust category signal"
  ),
  caution_for_manuscript = c(
    "DA-like transcriptomic identity only; not proof of graft function.",
    "A9-like molecular support only; not direct substantia nigra functional identity.",
    "A10-like molecular support only; interpret relative to DA core.",
    "Developmental/progenitor state; not automatically unsafe.",
    "Safety-risk-associated transcriptomic state; not direct tumorigenicity proof.",
    "Immature-risk signal; requires checking expression level and dataset context.",
    "Off-target glial-like identity; requires multi-marker support.",
    "Off-target oligodendrocyte/OPC-like identity; requires multi-marker support.",
    "Immune/macrophage-like signal may reflect host cells or contamination depending dataset.",
    "Vascular/mesenchymal signal may reflect host/stromal/meningeal components.",
    "Subtype signal only; requires pan-neuronal support.",
    "Subtype signal only; requires pan-neuronal support.",
    "Stress state, not a cell type.",
    "Do not annotate strongly."
  ),
  use_as_final_label_without_04D_validation = FALSE,
  stringsAsFactors = FALSE
)

atomic_write_csv(annotation_rules, annotation_rules_csv)

stamp("汇总 preliminary suggestions。")

suggestion_summary <- prelim_dt[
  ,
  .(
    n_groups = .N,
    total_cells = sum(n_cells_group, na.rm = TRUE),
    median_group_size = median(n_cells_group, na.rm = TRUE)
  ),
  by = .(dataset, preliminary_suggestion)
][order(dataset, -n_groups)]

atomic_write_csv(as.data.frame(suggestion_summary), suggestion_summary_csv)

stamp("汇总 dataset marker-category signal。")

gc_dt[, signal_positive := category_signal_flag(mean_score, coverage_fraction, pct_cells_score_gt0)]

dataset_category <- gc_dt[
  ,
  .(
    n_groups = .N,
    n_positive_groups = sum(signal_positive, na.rm = TRUE),
    positive_group_fraction = mean(signal_positive, na.rm = TRUE),
    median_mean_score = median(mean_score, na.rm = TRUE),
    max_mean_score = max(mean_score, na.rm = TRUE),
    median_pct_cells_score_gt0 = median(pct_cells_score_gt0, na.rm = TRUE),
    median_coverage = median(coverage_fraction, na.rm = TRUE)
  ),
  by = .(dataset, category)
][order(dataset, category)]

atomic_write_csv(as.data.frame(dataset_category), dataset_category_signal_csv)

stamp("生成候选 group review table。")

score_wide <- wide_category_scores(gc_dt)

prelim_small <- prelim_dt[, .(
  dataset,
  object_id,
  group_source,
  group_id,
  n_cells_group,
  preliminary_suggestion,
  supporting_categories,
  top_category,
  top_category_mean_score,
  caution
)]

candidate <- merge(
  prelim_small,
  score_wide,
  by = c("dataset", "object_id", "group_source", "group_id"),
  all.x = TRUE
)

if (!"DA_core_identity" %in% names(candidate)) candidate[, DA_core_identity := NA_real_]
if (!"neuronal_maturation_synapse" %in% names(candidate)) candidate[, neuronal_maturation_synapse := NA_real_]
if (!"A9_like_DA_identity" %in% names(candidate)) candidate[, A9_like_DA_identity := NA_real_]
if (!"A10_like_DA_identity" %in% names(candidate)) candidate[, A10_like_DA_identity := NA_real_]
if (!"cell_cycle_proliferation" %in% names(candidate)) candidate[, cell_cycle_proliferation := NA_real_]
if (!"pluripotency_immature_risk" %in% names(candidate)) candidate[, pluripotency_immature_risk := NA_real_]
if (!"progenitor_neuroepithelial" %in% names(candidate)) candidate[, progenitor_neuroepithelial := NA_real_]
if (!"midbrain_floor_plate_progenitor" %in% names(candidate)) candidate[, midbrain_floor_plate_progenitor := NA_real_]
if (!"stress_apoptosis_response" %in% names(candidate)) candidate[, stress_apoptosis_response := NA_real_]

candidate[
  ,
  DA_maturation_combined := rowMeans(
    cbind(DA_core_identity, neuronal_maturation_synapse),
    na.rm = TRUE
  )
]

candidate[
  ,
  A9_minus_A10_score := A9_like_DA_identity - A10_like_DA_identity
]

candidate[
  ,
  safety_risk_combined := rowMeans(
    cbind(cell_cycle_proliferation, pluripotency_immature_risk, progenitor_neuroepithelial),
    na.rm = TRUE
  )
]

candidate[
  ,
  progenitor_combined := rowMeans(
    cbind(midbrain_floor_plate_progenitor, progenitor_neuroepithelial),
    na.rm = TRUE
  )
]

candidate[, review_priority := "standard_review"]

candidate[
  preliminary_suggestion %in% c(
    "DA_like_neuronal_candidate",
    "cycling_progenitor_safety_risk_candidate",
    "immature_pluripotency_risk_signal_candidate",
    "midbrain_progenitor_like_candidate"
  ),
  review_priority := "high_priority"
]

candidate[
  preliminary_suggestion %in% c(
    "unassigned_low_marker_signal",
    "mixed_or_unassigned_marker_signal"
  ),
  review_priority := "manual_check_if_large_group"
]

candidate[
  n_cells_group >= 500 & review_priority == "manual_check_if_large_group",
  review_priority := "high_priority_large_unassigned"
]

candidate <- candidate[order(dataset, -DA_maturation_combined, -safety_risk_combined, -n_cells_group)]

atomic_write_csv(as.data.frame(candidate), candidate_groups_csv)

da_candidates <- candidate[
  !is.na(DA_core_identity) &
    (
      preliminary_suggestion == "DA_like_neuronal_candidate" |
        DA_core_identity >= MIN_MEAN_SCORE |
        DA_maturation_combined >= MIN_MEAN_SCORE
    )
]

da_candidates[
  ,
  DA_subtype_bias := fifelse(
    is.na(A9_minus_A10_score),
    "unknown",
    fifelse(A9_minus_A10_score > 0.02, "A9_like_bias",
            fifelse(A9_minus_A10_score < -0.02, "A10_like_bias", "A9_A10_mixed_or_unclear"))
  )
]

da_candidates <- da_candidates[order(dataset, -DA_maturation_combined, -A9_like_DA_identity, -A10_like_DA_identity)]

if (nrow(da_candidates) > MAX_REVIEW_GROUPS_PER_CATEGORY) {
  da_candidates <- da_candidates[seq_len(MAX_REVIEW_GROUPS_PER_CATEGORY)]
}

atomic_write_csv(as.data.frame(da_candidates), da_a9_a10_csv)

safety_candidates <- candidate[
  !is.na(safety_risk_combined) &
    (
      preliminary_suggestion %in% c(
        "cycling_progenitor_safety_risk_candidate",
        "immature_pluripotency_risk_signal_candidate",
        "midbrain_progenitor_like_candidate"
      ) |
        safety_risk_combined >= MIN_MEAN_SCORE |
        cell_cycle_proliferation >= MIN_MEAN_SCORE |
        pluripotency_immature_risk >= MIN_MEAN_SCORE
    )
]

safety_candidates[
  ,
  safety_risk_reason := paste0(
    "cell_cycle=", round(cell_cycle_proliferation, 3),
    "; pluripotency=", round(pluripotency_immature_risk, 3),
    "; progenitor=", round(progenitor_neuroepithelial, 3),
    "; stress=", round(stress_apoptosis_response, 3)
  )
]

safety_candidates <- safety_candidates[order(dataset, -safety_risk_combined, -cell_cycle_proliferation, -pluripotency_immature_risk)]

if (nrow(safety_candidates) > MAX_REVIEW_GROUPS_PER_CATEGORY) {
  safety_candidates <- safety_candidates[seq_len(MAX_REVIEW_GROUPS_PER_CATEGORY)]
}

atomic_write_csv(as.data.frame(safety_candidates), safety_risk_csv)

stamp("生成 key marker gene snapshot。")

key_genes <- c(
  "TH", "DDC", "SLC6A3", "SLC18A2", "NR4A2", "FOXA2", "LMX1A", "LMX1B", "PITX3",
  "ALDH1A1", "KCNJ6", "SOX6", "CALB1", "OTX2",
  "MKI67", "TOP2A", "PCNA", "SOX2", "NES", "POU5F1", "NANOG",
  "GFAP", "AQP4", "OLIG2", "PDGFRA", "PTPRC", "COL1A1"
)

if (nrow(gg_dt) > 0 && all(c("gene_symbol_matched", "dataset", "object_id", "group_id") %in% colnames(gg_dt))) {
  gg_dt[, gene_upper := toupper(gene_symbol_matched)]
  key_upper <- toupper(key_genes)

  marker_snapshot <- gg_dt[
    gene_upper %in% key_upper,
    .(
      dataset,
      object_id,
      group_source,
      group_id,
      category,
      gene_symbol_matched,
      n_cells,
      mean_expression,
      median_expression,
      pct_expressing
    )
  ][order(dataset, object_id, group_id, gene_symbol_matched)]
} else {
  marker_snapshot <- data.table()
}

atomic_write_csv(as.data.frame(marker_snapshot), marker_gene_snapshot_csv)

n_success_objects <- length(unique(paste(gc_dt$dataset, gc_dt$object_id, sep = "||")))
n_failed_objects <- if (nrow(failed_dt) > 0 && all(c("dataset", "object_id") %in% colnames(failed_dt))) {
  length(unique(paste(failed_dt$dataset, failed_dt$object_id, sep = "||")))
} else {
  0L
}

matrix_method_summary <- if (nrow(matrix_dt) > 0 && "extraction_method" %in% colnames(matrix_dt)) {
  matrix_dt[, .N, by = .(extraction_method, layer_type, layer)][order(-N)]
} else {
  data.table()
}

qc_audit <- data.frame(
  metric = c(
    "objects_with_marker_scores",
    "objects_failed",
    "group_category_rows",
    "group_gene_rows",
    "preliminary_suggestion_rows",
    "candidate_review_groups",
    "DA_candidate_groups",
    "safety_risk_candidate_groups"
  ),
  value = c(
    n_success_objects,
    n_failed_objects,
    nrow(gc_dt),
    nrow(gg_dt),
    nrow(prelim_dt),
    nrow(candidate),
    nrow(da_candidates),
    nrow(safety_candidates)
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(qc_audit, qc_audit_csv)

suggestion_lines <- if (nrow(suggestion_summary) > 0) {
  sug_top <- suggestion_summary[order(-n_groups)][seq_len(min(20, nrow(suggestion_summary)))]
  paste0(sug_top$dataset, " / ", sug_top$preliminary_suggestion, ": ", sug_top$n_groups)
} else {
  character()
}

matrix_method_lines <- if (nrow(matrix_method_summary) > 0) {
  apply(as.data.frame(matrix_method_summary), 1, function(x) {
    paste0(x[["extraction_method"]], " / ", x[["layer_type"]], " / ", x[["layer"]], ": ", x[["N"]])
  })
} else {
  character()
}

failed_lines <- if (nrow(failed_dt) > 0 && all(c("dataset", "object_id", "message") %in% colnames(failed_dt))) {
  apply(as.data.frame(failed_dt), 1, function(x) {
    paste0(x[["dataset"]], " :: ", x[["object_id"]], " — ", x[["message"]])
  })
} else {
  character()
}

report_lines <- c(
  "04C marker review and annotation rules report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "04B input summary:",
  paste0("Objects with marker scores: ", n_success_objects),
  paste0("Objects failed: ", n_failed_objects),
  paste0("Group-category score rows: ", nrow(gc_dt)),
  paste0("Group-marker gene expression rows: ", nrow(gg_dt)),
  paste0("Preliminary suggestion rows: ", nrow(prelim_dt)),
  "",
  "Matrix extraction method summary:",
  matrix_method_lines,
  "",
  "Failed object notes:",
  failed_lines,
  "",
  "Top preliminary suggestion summary:",
  suggestion_lines,
  "",
  "Candidate table summary:",
  paste0("Manual review groups: ", nrow(candidate)),
  paste0("DA/A9/A10 candidate groups: ", nrow(da_candidates)),
  paste0("Safety-risk candidate groups: ", nrow(safety_candidates)),
  "",
  "Output files:",
  paste0("Annotation rule table: ", annotation_rules_csv),
  paste0("Suggestion summary: ", suggestion_summary_csv),
  paste0("Dataset category signal summary: ", dataset_category_signal_csv),
  paste0("Candidate groups for manual review: ", candidate_groups_csv),
  paste0("DA/A9/A10 candidate groups: ", da_a9_a10_csv),
  paste0("Safety-risk candidate groups: ", safety_risk_csv),
  paste0("Key marker gene snapshot: ", marker_gene_snapshot_csv),
  paste0("QC audit summary: ", qc_audit_csv),
  "",
  "Next step:",
  "04D_APPLY_REVIEWED_ANNOTATION_LABELS.R",
  "",
  "Journal-rigor note:",
  "04C defines review rules and candidate groups only. Final labels must be applied after manual review and should avoid overclaiming real projection or therapeutic function."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("04C marker review and annotation rules 运行结束\n")
cat("============================================================\n\n")

cat("Objects with marker scores：", n_success_objects, "\n")
cat("Objects failed：", n_failed_objects, "\n")
cat("Group-category score rows：", nrow(gc_dt), "\n")
cat("Group-gene expression rows：", nrow(gg_dt), "\n")
cat("Preliminary suggestion rows：", nrow(prelim_dt), "\n")
cat("DA/A9/A10 candidate groups：", nrow(da_candidates), "\n")
cat("Safety-risk candidate groups：", nrow(safety_candidates), "\n\n")

cat("输出文件：\n")
cat(annotation_rules_csv, "\n")
cat(suggestion_summary_csv, "\n")
cat(dataset_category_signal_csv, "\n")
cat(candidate_groups_csv, "\n")
cat(da_a9_a10_csv, "\n")
cat(safety_risk_csv, "\n")
cat(marker_gene_snapshot_csv, "\n")
cat(qc_audit_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 04C marker review and annotation rules 完成。\n")
cat("下一步：先人工查看 04C_DA_A9_A10_candidate_groups.csv 和 04C_safety_risk_candidate_groups.csv，再进入 04D。\n")
