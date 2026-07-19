# ============================================================
# 03C_MAIN_ANALYSIS_STRATEGY_AND_MANIFEST.R
# ============================================================
# 目的：
#   接在 03A / 03B 后运行。
#
#   03B 是辅助 batch inspection，不是最终主分析。
#   如果 03B 因 Seurat layer / Windows 内存问题部分失败，不影响主线。
#
#   03C 做的是“期刊级主分析策略定稿”：
#     1. 读取 02B full filtered object manifest
#     2. 读取 03A reduced object manifest
#     3. 读取 03B batch check 状态
#     4. 给每个 dataset 定义 role / usage
#     5. 明确哪些对象用于最终主分析，哪些只用于 QC/batch check
#     6. 输出 main analysis manifest
#     7. 输出后续 04A cell annotation 的输入清单
#     8. 写 03C 报告
#
# 重要严谨性：
#   最终 cell annotation / signature scoring / DEG / enrichment / ML
#   不使用 03B downsampled objects。
#
#   最终主分析以 02B filtered full objects 和 03A reduced PCA objects 为基础。
#
# 成功标志：
#   ✅ 03C main analysis strategy and manifest 完成。
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"


# ============================================================
# 1. 加载包
# ============================================================

cat("\n============================================================\n")
cat("03C：main analysis strategy and manifest\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 03C。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})


# ============================================================
# 2. 路径
# ============================================================

metadata_dir <- file.path(PROJECT_DIR, "01_metadata")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_02B_manifest <- file.path(tables_dir, "02B_qc", "02B_filtered_object_manifest.csv")
input_03A_manifest <- file.path(tables_dir, "03A_reduction", "03A_reduced_object_manifest.csv")
input_03A_status <- file.path(metadata_dir, "03A_reduction_status.csv")
input_03B_status <- file.path(metadata_dir, "03B_dataset_merge_status.csv")
input_03B_manifest <- file.path(tables_dir, "03B_dataset_merge", "03B_merged_dataset_manifest.csv")

out_tables_dir <- file.path(tables_dir, "03C_strategy")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

dataset_role_csv <- file.path(out_tables_dir, "03C_dataset_role_and_usage.csv")
main_manifest_csv <- file.path(out_tables_dir, "03C_main_analysis_object_manifest.csv")
final_vs_qc_csv <- file.path(out_tables_dir, "03C_final_vs_qc_object_usage.csv")
next_steps_csv <- file.path(out_tables_dir, "03C_recommended_next_steps.csv")
report_txt <- file.path(reports_dir, "03C_main_analysis_strategy_and_manifest_report.txt")


# ============================================================
# 3. 工具函数
# ============================================================

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) {
    return(data.frame())
  }
  data.table::fread(path, data.table = FALSE)
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path)
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

dataset_role <- function(ds) {
  if (ds == "GSE178265_DA_01B") {
    return("core_DA_target_cells")
  }
  if (ds %in% c("GSE132758", "GSE204796", "GSE233885")) {
    return("graft_single_cell_atlas_and_validation")
  }
  if (ds == "GSE157783") {
    return("external_midbrain_single_cell_reference")
  }
  if (ds == "GSE200610") {
    return("auxiliary_single_cell_reference")
  }
  if (ds == "GSE204795") {
    return("bulk_RNA_support_validation")
  }
  "other"
}

dataset_final_usage <- function(ds) {
  if (ds == "GSE178265_DA_01B") {
    return("Final DA-like identity, A9/A10-like signature, projection-associated competence, and safety-risk scoring.")
  }
  if (ds %in% c("GSE132758", "GSE204796", "GSE233885")) {
    return("Final graft-state atlas/supporting validation; used for conserved graft cell-state signatures and safety-risk comparison.")
  }
  if (ds == "GSE157783") {
    return("Reference/supporting single-cell dataset; used for external pattern comparison, not primary graft frequency claims.")
  }
  if (ds == "GSE200610") {
    return("Auxiliary/reference dataset; used cautiously for robustness checks, not primary claims unless metadata compatibility is confirmed.")
  }
  if (ds == "GSE204795") {
    return("Bulk-level orthogonal support for pathways/signatures, not single-cell clustering.")
  }
  "To be decided."
}

dataset_integration_strategy <- function(ds) {
  if (ds == "GSE178265_DA_01B") {
    return("Analyze as core DA-target object; integrate/scoring with relevant graft datasets only after marker QC.")
  }
  if (ds %in% c("GSE132758", "GSE204796", "GSE233885")) {
    return("Dataset-specific analysis first; later compare signatures across datasets. Avoid relying on one global all-cell integration before annotation.")
  }
  if (ds %in% c("GSE157783", "GSE200610")) {
    return("Use as reference/auxiliary; do not force into final graft atlas until cell-type comparability is verified.")
  }
  if (ds == "GSE204795") {
    return("Analyze separately as bulk/pseudobulk support.")
  }
  "Dataset-specific evaluation first."
}


# ============================================================
# 4. 读取输入
# ============================================================

stamp("读取 02B / 03A / 03B 输出。")

m02B <- read_csv_required(input_02B_manifest)
m03A <- read_csv_required(input_03A_manifest)
s03A <- read_csv_optional(input_03A_status)
s03B <- read_csv_optional(input_03B_status)
m03B <- read_csv_optional(input_03B_manifest)

if (!all(c("dataset", "object_id", "filtered_rds") %in% colnames(m02B))) {
  stop("02B manifest 缺少 dataset/object_id/filtered_rds。")
}

if (!all(c("dataset", "object_id", "reduced_rds") %in% colnames(m03A))) {
  stop("03A manifest 缺少 dataset/object_id/reduced_rds。")
}


# ============================================================
# 5. 建立 dataset role 表
# ============================================================

datasets_from_sc <- unique(c(m02B$dataset, m03A$dataset))

datasets_all <- unique(c(datasets_from_sc, "GSE204795"))

dataset_role_df <- data.frame(
  dataset = datasets_all,
  role = vapply(datasets_all, dataset_role, character(1)),
  final_usage = vapply(datasets_all, dataset_final_usage, character(1)),
  integration_strategy = vapply(datasets_all, dataset_integration_strategy, character(1)),
  use_02B_full_filtered_for_final = datasets_all %in% datasets_from_sc,
  use_03A_pca_for_qc_or_initial_reduction = datasets_all %in% datasets_from_sc,
  use_03B_downsampled_for_final_claims = FALSE,
  use_03B_only_for_batch_inspection = datasets_all %in% datasets_from_sc,
  stringsAsFactors = FALSE
)

# 补充 03B 状态
if (nrow(s03B) > 0 && "dataset" %in% colnames(s03B) && "status" %in% colnames(s03B)) {
  s03B_small <- s03B[, intersect(c("dataset", "status", "message"), colnames(s03B)), drop = FALSE]
  colnames(s03B_small)[colnames(s03B_small) == "status"] <- "status_03B"
  colnames(s03B_small)[colnames(s03B_small) == "message"] <- "message_03B"
  dataset_role_df <- merge(dataset_role_df, s03B_small, by = "dataset", all.x = TRUE)
} else {
  dataset_role_df$status_03B <- NA_character_
  dataset_role_df$message_03B <- NA_character_
}

atomic_write_csv(dataset_role_df, dataset_role_csv)


# ============================================================
# 6. 建立 main analysis manifest
# ============================================================

m02B_small <- m02B[, intersect(c(
  "dataset", "object_id", "filtered_rds",
  "n_cells_before", "n_cells_after", "retention_rate"
), colnames(m02B)), drop = FALSE]

m03A_small <- m03A[, intersect(c(
  "dataset", "object_id", "reduced_rds",
  "n_cells", "n_features", "n_variable_features", "npcs", "dims_used", "n_clusters", "status"
), colnames(m03A)), drop = FALSE]

main_manifest <- merge(
  m02B_small,
  m03A_small,
  by = c("dataset", "object_id"),
  all.x = TRUE,
  suffixes = c("_02B", "_03A")
)

main_manifest$dataset_role <- vapply(main_manifest$dataset, dataset_role, character(1))
main_manifest$final_usage <- vapply(main_manifest$dataset, dataset_final_usage, character(1))
main_manifest$final_expression_object <- main_manifest$filtered_rds
main_manifest$initial_pca_object <- main_manifest$reduced_rds
main_manifest$use_for_final_annotation <- TRUE
main_manifest$use_for_final_scoring <- TRUE
main_manifest$use_for_final_DEG <- TRUE
main_manifest$use_for_final_ML_feature_building <- TRUE
main_manifest$use_03B_downsampled_object <- FALSE

# 辅助 reference 不作为 primary claim
main_manifest$primary_claim_dataset <- !(main_manifest$dataset %in% c("GSE157783", "GSE200610"))

atomic_write_csv(main_manifest, main_manifest_csv)


# ============================================================
# 7. 明确 final vs QC usage
# ============================================================

usage_df <- data.frame(
  object_stage = c(
    "01A standardized objects",
    "01B GSE178265 DA submatrix object",
    "02A QC-metric objects",
    "02B full filtered objects",
    "03A per-object normalized/PCA objects",
    "03B dataset-level merged/downsampled PCA objects",
    "GSE204795 bulk DESeqDataSet"
  ),
  use_for = c(
    "Raw standardized import backup and reproducibility.",
    "Core DA target single-cell input for downstream analysis.",
    "QC metric audit and traceability.",
    "Primary final single-cell expression source after QC filtering.",
    "Initial per-object PCA/reduction; input reference for main object design and inspection.",
    "Batch inspection only. Not for final biological claims.",
    "Bulk-level pathway/signature support only."
  ),
  can_use_for_final_biological_claim = c(
    FALSE,
    TRUE,
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    TRUE
  ),
  can_use_for_DEG_or_ML = c(
    FALSE,
    TRUE,
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    TRUE
  ),
  notes = c(
    "Do not analyze unfiltered cells directly.",
    "Use after QC filtering where available.",
    "Metrics only; not filtered expression source.",
    "Main full data source for annotation/scoring/DEG/ML.",
    "Useful but avoid over-interpreting per-object UMAP/cluster if present.",
    "Downsample/PCA only; 03B failures are documented and do not block main analysis.",
    "Separate bulk analysis; not merged with scRNA Seurat objects."
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(usage_df, final_vs_qc_csv)


# ============================================================
# 8. 下一步建议
# ============================================================

next_steps_df <- data.frame(
  step = c(
    "04A",
    "04B",
    "05A",
    "05B",
    "06A",
    "07A",
    "08A"
  ),
  name = c(
    "Marker gene panel and dataset-specific annotation preparation",
    "Cell-type annotation and marker validation",
    "DA-like/A9/A10-like identity scoring",
    "Projection-associated molecular competence scoring",
    "Safety-risk state scoring",
    "Pseudobulk/DEG/enrichment",
    "ML model with cross-validation and external validation"
  ),
  rigor_requirement = c(
    "Use curated marker lists and record marker source/version.",
    "Do not overclaim; annotation must be marker-supported and checked across datasets.",
    "Use predefined gene signatures; report gene coverage per dataset.",
    "Use molecular competence language, not real projection claims.",
    "Risk score is transcriptomic risk-associated state, not direct tumor proof.",
    "Use sample-aware/pseudobulk where possible; avoid treating cells as independent biological replicates for strong claims.",
    "Use train/test or cross-validation; report AUC, feature importance, and external validation."
  ),
  input_source = c(
    "03C_main_analysis_object_manifest.csv",
    "02B full filtered + 03A PCA objects",
    "02B/03A objects",
    "02B/03A objects",
    "02B/03A objects",
    "annotated objects + metadata",
    "scored/annotated objects + validation datasets"
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(next_steps_df, next_steps_csv)


# ============================================================
# 9. 报告
# ============================================================

n_sc_objects <- nrow(main_manifest)
n_datasets <- length(unique(main_manifest$dataset))
n_primary_objects <- sum(main_manifest$primary_claim_dataset, na.rm = TRUE)
n_reference_objects <- sum(!main_manifest$primary_claim_dataset, na.rm = TRUE)

failed_03B <- if ("status_03B" %in% colnames(dataset_role_df)) {
  dataset_role_df$dataset[grepl("^FAILED", dataset_role_df$status_03B)]
} else {
  character()
}

report_lines <- c(
  "03C main analysis strategy and manifest report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("Single-cell objects in main manifest: ", n_sc_objects),
  paste0("Single-cell datasets: ", n_datasets),
  paste0("Primary-claim objects: ", n_primary_objects),
  paste0("Reference/auxiliary objects: ", n_reference_objects),
  paste0("03B failed datasets documented: ", paste(failed_03B, collapse = ", ")),
  "",
  "Key decision:",
  "03B downsampled/batch-check objects are not used for final biological claims.",
  "Final biological analyses will use 02B full filtered objects and 03A per-object normalized/PCA outputs.",
  "",
  "Output files:",
  paste0("Dataset role table: ", dataset_role_csv),
  paste0("Main analysis manifest: ", main_manifest_csv),
  paste0("Final vs QC object usage: ", final_vs_qc_csv),
  paste0("Recommended next steps: ", next_steps_csv),
  "",
  "Next script:",
  "04A_MARKER_PANEL_AND_ANNOTATION_PREP.R",
  "",
  "Journal-rigor note:",
  "Downsampling and PCA-only modes were used only to stabilize QC/batch-inspection steps on Windows/RStudio. They are explicitly excluded from final DEG, enrichment, scoring, cell-frequency, and ML conclusions."
)

writeLines(report_lines, report_txt)


# ============================================================
# 10. 结束
# ============================================================

cat("\n============================================================\n")
cat("03C main analysis strategy and manifest 运行结束\n")
cat("============================================================\n\n")

cat("single-cell objects in main manifest：", n_sc_objects, "\n")
cat("single-cell datasets：", n_datasets, "\n")
cat("primary-claim objects：", n_primary_objects, "\n")
cat("reference/auxiliary objects：", n_reference_objects, "\n")

if (length(failed_03B) > 0) {
  cat("03B failed datasets documented：", paste(failed_03B, collapse = ", "), "\n")
}

cat("\n输出文件：\n")
cat(dataset_role_csv, "\n")
cat(main_manifest_csv, "\n")
cat(final_vs_qc_csv, "\n")
cat(next_steps_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 03C main analysis strategy and manifest 完成。\n")
cat("下一步进入 04A：marker panel 和 annotation 准备。\n")
