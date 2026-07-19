
PROJECT_DIR <- "D:/PD_Graft_Project"

MIN_GROUPS_PER_CLASS_WARNING <- 10

cat("\n============================================================\n")
cat("07A V3：contrast-based ML dataset preparation\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 07A V3。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_04B_group <- file.path(tables_dir, "04B_marker_expression", "04B_group_marker_category_scores.csv")
input_05B_contrast <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_DA_projection_vs_safety_contrast_groups.csv")
input_05A_group <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_group_level_scores.csv")
input_05B_group_safety <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_group_safety_risk_scores.csv")

out_tables_dir <- file.path(tables_dir, "07A_ML_dataset_preparation")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

ml_master_csv <- file.path(out_tables_dir, "07A_group_level_ML_master_table.csv")
ideal_train_csv <- file.path(out_tables_dir, "07A_ideal_graft_like_model_training_table.csv")
safety_train_csv <- file.path(out_tables_dir, "07A_safety_risk_model_training_table.csv")
feature_dictionary_csv <- file.path(out_tables_dir, "07A_feature_dictionary.csv")
label_definition_csv <- file.path(out_tables_dir, "07A_label_definition_table.csv")
split_plan_csv <- file.path(out_tables_dir, "07A_dataset_split_recommendation.csv")
class_balance_csv <- file.path(out_tables_dir, "07A_class_balance_summary.csv")
qc_audit_csv <- file.path(out_tables_dir, "07A_ML_dataset_QC_audit.csv")
merge_audit_csv <- file.path(out_tables_dir, "07A_V3_merge_audit.csv")
report_txt <- file.path(reports_dir, "07A_ML_dataset_preparation_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

num <- function(x) suppressWarnings(as.numeric(x))

make_key <- function(dataset, object_id, group_id) {
  paste(dataset, object_id, as.character(group_id), sep = "||")
}

wide_category_scores <- function(group_dt) {
  dcast(
    group_dt,
    dataset + object_id + group_id ~ category,
    value.var = "mean_score",
    fun.aggregate = max,
    fill = NA_real_
  )
}

safe_add_col <- function(dt, col, value = NA_character_) {
  if (!col %in% colnames(dt)) dt[[col]] <- value
  dt
}

stamp("读取 05B contrast table 作为 ML master base。")

contrast <- as.data.table(read_csv_required(input_05B_contrast))
g04B <- as.data.table(read_csv_optional(input_04B_group))
g05A <- as.data.table(read_csv_optional(input_05A_group))
g05B <- as.data.table(read_csv_optional(input_05B_group_safety))

needed_contrast <- c("dataset", "object_id", "group_id", "safety_contrast_class_05B")
if (!all(needed_contrast %in% colnames(contrast))) {
  stop("05B contrast table 缺少必要列：", paste(setdiff(needed_contrast, colnames(contrast)), collapse = ", "))
}

contrast[, group_id := as.character(group_id)]
contrast[, group_key := make_key(dataset, object_id, group_id)]

stamp("构建 contrast-based ML master。")

master <- copy(contrast)

for (cc in c("dataset", "object_id", "group_id", "group_key")) {
  if (!cc %in% colnames(master)) stop("master 缺少必要列：", cc)
}

if (!"n_cells" %in% colnames(master)) {

  if ("n_cells_05B" %in% colnames(master)) {
    master[, n_cells := n_cells_05B]
  } else if ("n_cells_05A" %in% colnames(master)) {
    master[, n_cells := n_cells_05A]
  } else {
    master[, n_cells := NA_integer_]
  }
}

needed_scores <- c(
  "DA_like_composite_score",
  "projection_competence_composite_score",
  "DA_projection_competence_composite_score",
  "A9_minus_A10_score_05A",
  "safety_risk_composite_05B",
  "safety_cell_cycle_score_05B",
  "safety_progenitor_score_05B",
  "safety_pluripotency_score_05B",
  "safety_stress_score_05B",
  "safety_ecm_score_05B",
  "safety_vascular_score_05B"
)

for (sc in needed_scores) {
  if (!sc %in% colnames(master)) master[[sc]] <- NA_real_
  master[[sc]] <- num(master[[sc]])
}

stamp("尝试 merge 04B marker-category features。")

marker_merge_status <- "not_attempted"
n_marker_features <- 0L
n_marker_matched <- 0L

if (nrow(g04B) > 0 && all(c("dataset", "object_id", "group_id", "category", "mean_score") %in% colnames(g04B))) {
  g04B[, group_id := as.character(group_id)]

  marker_wide <- wide_category_scores(g04B)

  marker_id_cols <- c("dataset", "object_id", "group_id")
  marker_feature_cols <- setdiff(colnames(marker_wide), marker_id_cols)

  for (col in marker_feature_cols) {
    setnames(marker_wide, col, paste0("marker_", col))
  }

  marker_wide[, group_key := make_key(dataset, object_id, group_id)]

  before_rows <- nrow(master)

  master <- merge(
    master,
    marker_wide[, c("group_key", paste0("marker_", marker_feature_cols)), with = FALSE],
    by = "group_key",
    all.x = TRUE
  )

  n_marker_features <- length(marker_feature_cols)
  marker_cols_final <- paste0("marker_", marker_feature_cols)
  n_marker_matched <- sum(rowSums(!is.na(master[, marker_cols_final, with = FALSE])) > 0)

  marker_merge_status <- "attempted"
} else {
  marker_merge_status <- "04B_marker_table_missing_or_incomplete"
}

stamp("定义 ideal / safety marker-rule-derived labels。")

master[
  ,
  ideal_graft_like_weak_label := fifelse(
    safety_contrast_class_05B == "ideal_DA_projection_high_safety_low",
    1L,
    fifelse(
      safety_contrast_class_05B %in% c(
        "high_safety_risk_low_DA",
        "mixed_DA_or_projection_with_safety_risk"
      ),
      0L,
      NA_integer_
    )
  )
]

master[
  ,
  safety_risk_weak_label := fifelse(
    safety_contrast_class_05B %in% c(
      "high_safety_risk_low_DA",
      "mixed_DA_or_projection_with_safety_risk"
    ),
    1L,
    fifelse(
      safety_contrast_class_05B %in% c(
        "ideal_DA_projection_high_safety_low",
        "projection_competence_without_DA_low_safety"
      ),
      0L,
      NA_integer_
    )
  )
]

master[, ML_label_source := "rule_derived_weak_label_from_05B_contrast"]
master[, ML_claim_boundary := "Marker-rule-derived labels are rule-derived from transcriptomic scores; they are not experimental ground truth."]

master[, has_required_DA_projection_scores := !is.na(DA_like_composite_score) & !is.na(projection_competence_composite_score)]
master[, has_required_safety_scores := !is.na(safety_risk_composite_05B)]

stamp("定义 ML feature columns。")

leakage_cols <- c(
  "DA_like_composite_score",
  "projection_competence_composite_score",
  "DA_projection_competence_composite_score",
  "safety_risk_composite_05B",
  "safety_cell_cycle_score_05B",
  "safety_progenitor_score_05B",
  "safety_pluripotency_score_05B",
  "safety_stress_score_05B",
  "safety_ecm_score_05B",
  "safety_vascular_score_05B"
)

numeric_cols <- names(master)[vapply(master, is.numeric, logical(1))]

exclude_numeric <- c(
  "ideal_graft_like_weak_label",
  "safety_risk_weak_label",
  "n_cells",
  "n_cells_05A",
  "n_cells_05B",
  "total_groups_dataset",
  "group_fraction"
)

full_feature_cols <- setdiff(numeric_cols, exclude_numeric)

primary_feature_cols <- setdiff(full_feature_cols, leakage_cols)

if (length(primary_feature_cols) == 0L) {
  primary_feature_cols <- full_feature_cols
}

stamp("输出 ML master 和 training tables。")

atomic_write_csv(as.data.frame(master), ml_master_csv)

id_cols <- intersect(
  c(
    "dataset", "object_id", "group_id", "group_key",
    "n_cells",
    "annotation_04D_v1",
    "safety_contrast_class_05B",
    "A9_A10_bias_label_05B",
    "story_priority_05B"
  ),
  colnames(master)
)

common_meta_cols <- intersect(
  c(
    "ML_label_source",
    "ML_claim_boundary",
    "has_required_DA_projection_scores",
    "has_required_safety_scores"
  ),
  colnames(master)
)

ideal_train <- master[
  !is.na(ideal_graft_like_weak_label) &
    has_required_DA_projection_scores == TRUE &
    has_required_safety_scores == TRUE
]

ideal_cols <- unique(c(
  id_cols,
  "ideal_graft_like_weak_label",
  primary_feature_cols,
  common_meta_cols
))

ideal_cols <- ideal_cols[ideal_cols %in% colnames(ideal_train)]
ideal_train <- ideal_train[, ideal_cols, with = FALSE]

safety_train <- master[
  !is.na(safety_risk_weak_label) &
    has_required_DA_projection_scores == TRUE &
    has_required_safety_scores == TRUE
]

safety_cols <- unique(c(
  id_cols,
  "safety_risk_weak_label",
  primary_feature_cols,
  common_meta_cols
))

safety_cols <- safety_cols[safety_cols %in% colnames(safety_train)]
safety_train <- safety_train[, safety_cols, with = FALSE]

atomic_write_csv(as.data.frame(ideal_train), ideal_train_csv)
atomic_write_csv(as.data.frame(safety_train), safety_train_csv)

stamp("生成 feature dictionary / label definitions。")

feature_dictionary <- rbindlist(
  list(
    data.table(
      feature = primary_feature_cols,
      feature_set = "primary_feature_set",
      recommended_primary_ML = TRUE,
      leakage_warning = fifelse(
        primary_feature_cols %in% leakage_cols,
        "Potential label leakage; use only for exploratory marker-rule-derived model.",
        "Lower leakage risk; still marker-rule-derived-derived context."
      )
    ),
    data.table(
      feature = setdiff(full_feature_cols, primary_feature_cols),
      feature_set = "descriptive_or_leakage_sensitive_feature_set",
      recommended_primary_ML = FALSE,
      leakage_warning = "Potential label leakage or descriptive-only feature."
    )
  ),
  fill = TRUE
)

feature_dictionary[
  ,
  interpretation := fifelse(
    grepl("^marker_", feature),
    "04B marker-category score.",
    fifelse(
      feature %in% leakage_cols,
      "Composite score used directly or indirectly in marker-rule-derived definition.",
      "Numeric score feature from 05A/05B contrast table."
    )
  )
]

atomic_write_csv(as.data.frame(feature_dictionary), feature_dictionary_csv)

label_definition <- data.frame(
  label_name = c(
    "ideal_graft_like_weak_label",
    "safety_risk_weak_label"
  ),
  positive_class = c(
    "safety_contrast_class_05B == ideal_DA_projection_high_safety_low",
    "safety_contrast_class_05B in high_safety_risk_low_DA or mixed_DA_or_projection_with_safety_risk"
  ),
  negative_class = c(
    "safety_contrast_class_05B in high_safety_risk_low_DA or mixed_DA_or_projection_with_safety_risk",
    "safety_contrast_class_05B in ideal_DA_projection_high_safety_low or projection_competence_without_DA_low_safety"
  ),
  excluded_class = c(
    "lower_priority_or_mixed and ambiguous/unscored groups",
    "lower_priority_or_mixed and ambiguous/unscored groups"
  ),
  evidence_boundary = c(
    "Marker-rule-derived label; not experimentally validated ideal graft outcome.",
    "Marker-rule-derived label; not proof of tumorigenicity or clinical safety risk."
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(label_definition, label_definition_csv)

stamp("生成 split recommendation。")

datasets <- sort(unique(master$dataset))

split_plan <- data.frame(
  split_strategy = character(),
  train_datasets = character(),
  test_datasets = character(),
  use_case = character(),
  caution = character(),
  stringsAsFactors = FALSE
)

for (ds in datasets) {
  train_ds <- setdiff(datasets, ds)

  split_plan <- rbind(
    split_plan,
    data.frame(
      split_strategy = "leave_one_dataset_out",
      train_datasets = paste(train_ds, collapse = ";"),
      test_datasets = ds,
      use_case = "Cross-dataset robustness check.",
      caution = "Dataset number is small; marker-rule-derived labels are rule-derived.",
      stringsAsFactors = FALSE
    )
  )
}

split_plan <- rbind(
  split_plan,
  data.frame(
    split_strategy = "exploratory_random_split_stratified_by_label",
    train_datasets = paste(datasets, collapse = ";"),
    test_datasets = paste(datasets, collapse = ";"),
    use_case = "Internal exploratory benchmark only.",
    caution = "May overestimate performance due to dataset leakage and marker-rule-derived circularity.",
    stringsAsFactors = FALSE
  )
)

atomic_write_csv(split_plan, split_plan_csv)

stamp("计算 class balance 和 audit。")

class_balance_ideal <- ideal_train[
  ,
  .(
    n_groups = .N,
    total_cells = sum(n_cells, na.rm = TRUE)
  ),
  by = .(dataset, ideal_graft_like_weak_label)
]
class_balance_ideal[, model_task := "ideal_graft_like_model"]
setnames(class_balance_ideal, "ideal_graft_like_weak_label", "class_label")

class_balance_safety <- safety_train[
  ,
  .(
    n_groups = .N,
    total_cells = sum(n_cells, na.rm = TRUE)
  ),
  by = .(dataset, safety_risk_weak_label)
]
class_balance_safety[, model_task := "safety_risk_model"]
setnames(class_balance_safety, "safety_risk_weak_label", "class_label")

class_balance <- rbindlist(
  list(
    class_balance_ideal[, .(model_task, dataset, class_label, n_groups, total_cells)],
    class_balance_safety[, .(model_task, dataset, class_label, n_groups, total_cells)]
  ),
  fill = TRUE
)

atomic_write_csv(as.data.frame(class_balance), class_balance_csv)

ideal_pos <- sum(ideal_train$ideal_graft_like_weak_label == 1, na.rm = TRUE)
ideal_neg <- sum(ideal_train$ideal_graft_like_weak_label == 0, na.rm = TRUE)
safety_pos <- sum(safety_train$safety_risk_weak_label == 1, na.rm = TRUE)
safety_neg <- sum(safety_train$safety_risk_weak_label == 0, na.rm = TRUE)

qc_audit <- data.frame(
  metric = c(
    "ML_master_groups",
    "ideal_training_groups",
    "ideal_positive_groups",
    "ideal_negative_groups",
    "safety_training_groups",
    "safety_positive_groups",
    "safety_negative_groups",
    "primary_numeric_features",
    "full_numeric_features",
    "datasets_represented",
    "marker_merge_status",
    "marker_features_available",
    "groups_with_marker_features",
    "warning_ideal_positive_lt_min",
    "warning_ideal_negative_lt_min",
    "warning_safety_positive_lt_min",
    "warning_safety_negative_lt_min"
  ),
  value = c(
    nrow(master),
    nrow(ideal_train),
    ideal_pos,
    ideal_neg,
    nrow(safety_train),
    safety_pos,
    safety_neg,
    length(primary_feature_cols),
    length(full_feature_cols),
    length(unique(master$dataset)),
    marker_merge_status,
    n_marker_features,
    n_marker_matched,
    ideal_pos < MIN_GROUPS_PER_CLASS_WARNING,
    ideal_neg < MIN_GROUPS_PER_CLASS_WARNING,
    safety_pos < MIN_GROUPS_PER_CLASS_WARNING,
    safety_neg < MIN_GROUPS_PER_CLASS_WARNING
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(qc_audit, qc_audit_csv)

merge_audit <- data.frame(
  item = c(
    "base_table",
    "base_rows",
    "marker_merge_status",
    "marker_features_available",
    "groups_with_marker_features",
    "training_table_reason"
  ),
  value = c(
    "05B_DA_projection_vs_safety_contrast_groups.csv",
    nrow(contrast),
    marker_merge_status,
    n_marker_features,
    n_marker_matched,
    "V3 uses 05B contrast as base to preserve marker-rule-derived labels and avoid group_id merge loss."
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(merge_audit, merge_audit_csv)

balance_lines <- if (nrow(class_balance) > 0) {
  apply(as.data.frame(class_balance), 1, function(x) {
    paste0(
      x[["model_task"]],
      " / ",
      x[["dataset"]],
      " / class ",
      x[["class_label"]],
      ": groups=",
      x[["n_groups"]],
      "; cells=",
      x[["total_cells"]]
    )
  })
} else {
  "none"
}

report_lines <- c(
  "07A V3 contrast-based ML dataset preparation report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("ML master groups: ", nrow(master)),
  paste0("Ideal model training groups: ", nrow(ideal_train)),
  paste0("Ideal positive groups: ", ideal_pos),
  paste0("Ideal negative groups: ", ideal_neg),
  paste0("Safety model training groups: ", nrow(safety_train)),
  paste0("Safety positive groups: ", safety_pos),
  paste0("Safety negative groups: ", safety_neg),
  paste0("Primary numeric features: ", length(primary_feature_cols)),
  paste0("Full numeric features: ", length(full_feature_cols)),
  paste0("Datasets represented: ", paste(sort(unique(master$dataset)), collapse = "; ")),
  paste0("Marker merge status: ", marker_merge_status),
  paste0("Groups with marker features: ", n_marker_matched),
  "",
  "Class balance detail:",
  balance_lines,
  "",
  "Output files:",
  paste0("ML master table: ", ml_master_csv),
  paste0("Ideal training table: ", ideal_train_csv),
  paste0("Safety training table: ", safety_train_csv),
  paste0("Feature dictionary: ", feature_dictionary_csv),
  paste0("Label definition: ", label_definition_csv),
  paste0("Split recommendation: ", split_plan_csv),
  paste0("Class balance: ", class_balance_csv),
  paste0("QC audit: ", qc_audit_csv),
  paste0("Merge audit: ", merge_audit_csv),
  "",
  "Next step:",
  "07B_TRAIN_WEAK_LABEL_ML_MODELS.R",
  "",
  "Journal-rigor note:",
  "V3 fixes label loss by using 05B contrast as the master table. Labels remain rule-derived marker-rule-derived labels. Any downstream model must be reported as exploratory marker-rule-derived classification, not experimental prediction."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("07A V3 contrast-based ML dataset preparation 运行结束\n")
cat("============================================================\n\n")

cat("ML master groups：", nrow(master), "\n")
cat("Ideal model training groups：", nrow(ideal_train), "\n")
cat("Ideal positive groups：", ideal_pos, "\n")
cat("Ideal negative groups：", ideal_neg, "\n")
cat("Safety model training groups：", nrow(safety_train), "\n")
cat("Safety positive groups：", safety_pos, "\n")
cat("Safety negative groups：", safety_neg, "\n")
cat("Primary numeric features：", length(primary_feature_cols), "\n")
cat("Full numeric features：", length(full_feature_cols), "\n")
cat("Datasets represented：", length(unique(master$dataset)), "\n")
cat("Marker merge status：", marker_merge_status, "\n")
cat("Groups with marker features：", n_marker_matched, "\n\n")

cat("输出文件：\n")
cat(ml_master_csv, "\n")
cat(ideal_train_csv, "\n")
cat(safety_train_csv, "\n")
cat(feature_dictionary_csv, "\n")
cat(label_definition_csv, "\n")
cat(split_plan_csv, "\n")
cat(class_balance_csv, "\n")
cat(qc_audit_csv, "\n")
cat(merge_audit_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 07A V3 contrast-based ML dataset preparation 完成。\n")
cat("下一步进入 07B：训练 exploratory marker-rule-derived prioritization model models。\n")
