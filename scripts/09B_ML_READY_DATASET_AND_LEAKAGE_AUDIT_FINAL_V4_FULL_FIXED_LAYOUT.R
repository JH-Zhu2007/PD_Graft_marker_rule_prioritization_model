
PROJECT_DIR <- "D:/PD_Graft_Project"

SEED <- 20260714

MIN_POS_PER_TASK <- 5
MIN_NEG_PER_TASK <- 5
MIN_DATASETS_FOR_LODO <- 3

PDF_WIDTH <- 10.5
PDF_HEIGHT <- 7.2

TOP_FEATURES_FOR_LEAKAGE_PLOT <- 30

cat("\n============================================================\n")
cat("09B：ML-ready dataset construction + leakage audit\n")
cat("============================================================\n\n")

options(stringsAsFactors = FALSE)
options(timeout = 60000)

required_pkgs <- c("data.table")

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop(
    "缺少 R 包，请先手动安装：",
    paste(missing_pkgs, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(data.table)
})

set.seed(SEED)

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_07A_master <- file.path(
  tables_dir,
  "07A_ML_dataset_preparation",
  "07A_group_level_ML_master_table.csv"
)

input_07A_ideal <- file.path(
  tables_dir,
  "07A_ML_dataset_preparation",
  "07A_ideal_graft_like_model_training_table.csv"
)

input_07A_safety <- file.path(
  tables_dir,
  "07A_ML_dataset_preparation",
  "07A_safety_risk_model_training_table.csv"
)

input_07A_feature_dict <- file.path(
  tables_dir,
  "07A_ML_dataset_preparation",
  "07A_feature_dictionary.csv"
)

input_05B_contrast <- file.path(
  tables_dir,
  "05B_safety_risk_scoring",
  "05B_DA_projection_vs_safety_contrast_groups.csv"
)

input_09A_object_priority <- file.path(
  tables_dir,
  "09A_scRNA_cell_state_proportion_final_V6",
  "09A_object_priority_summary.csv"
)

input_09A_dataset_priority <- file.path(
  tables_dir,
  "09A_scRNA_cell_state_proportion_final_V6",
  "09A_dataset_priority_summary.csv"
)

input_09A_fraction_sum_audit <- file.path(
  tables_dir,
  "09A_scRNA_cell_state_proportion_final_V6",
  "09A_fraction_sum_audit.csv"
)

out_tables_dir <- file.path(
  tables_dir,
  "09B_ML_ready_dataset_and_leakage_audit_V4_FULL_FIXED_LAYOUT"
)

out_figures_dir <- file.path(
  figures_dir,
  "09B_ML_ready_dataset_and_leakage_audit_V4_FULL_FIXED_LAYOUT_pdf"
)

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

ml_master_csv <- file.path(out_tables_dir, "09B_ML_master_table.csv")
feature_dictionary_csv <- file.path(out_tables_dir, "09B_feature_dictionary_with_leakage_risk.csv")
feature_set_summary_csv <- file.path(out_tables_dir, "09B_feature_set_summary.csv")
label_definition_csv <- file.path(out_tables_dir, "09B_weak_label_definition_and_boundaries.csv")
leakage_audit_csv <- file.path(out_tables_dir, "09B_feature_leakage_circularity_audit.csv")
class_balance_csv <- file.path(out_tables_dir, "09B_class_balance_by_dataset.csv")
lodo_plan_csv <- file.path(out_tables_dir, "09B_leave_one_dataset_out_feasibility_plan.csv")
readiness_summary_csv <- file.path(out_tables_dir, "09B_ML_readiness_summary.csv")
input_audit_csv <- file.path(out_tables_dir, "09B_input_audit.csv")
object_comp_duplicate_audit_csv <- file.path(out_tables_dir, "09B_09A_object_comp_duplicate_key_audit.csv")
output_check_csv <- file.path(out_tables_dir, "09B_output_verification.csv")
session_info_txt <- file.path(out_tables_dir, "09B_sessionInfo.txt")
method_note_txt <- file.path(out_tables_dir, "09B_method_and_claim_boundary_note.txt")
report_txt <- file.path(reports_dir, "09B_ML_ready_dataset_and_leakage_audit_V4_FULL_FIXED_LAYOUT_report.txt")

ideal_reduced_csv <- file.path(out_tables_dir, "09B_ideal_like_training_reduced_non_direct_features.csv")
safety_reduced_csv <- file.path(out_tables_dir, "09B_safety_risk_training_reduced_non_direct_features.csv")
ideal_full_csv <- file.path(out_tables_dir, "09B_ideal_like_training_full_exploratory_features.csv")
safety_full_csv <- file.path(out_tables_dir, "09B_safety_risk_training_full_exploratory_features.csv")

python_manifest_txt <- file.path(out_tables_dir, "09B_python_ML_input_manifest.txt")

fig_ideal_balance_pdf <- file.path(out_figures_dir, "09B_ideal_like_class_balance_by_dataset.pdf")
fig_safety_balance_pdf <- file.path(out_figures_dir, "09B_safety_risk_class_balance_by_dataset.pdf")
fig_leakage_top_pdf <- file.path(out_figures_dir, "09B_top_feature_label_association_audit_FIXED_LAYOUT.pdf")
fig_feature_category_pdf <- file.path(out_figures_dir, "09B_feature_leakage_category_summary_FIXED_LAYOUT.pdf")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

num <- function(x) suppressWarnings(as.numeric(x))

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }

  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }

  data.table::fwrite(df, path)

  if (!file.exists(path)) {
    stop("CSV 未生成：", path)
  }

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes <= 0) {
    stop("CSV 已创建但为空或无效：", path)
  }

  invisible(path)
}

read_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path)
  }
  data.table::fread(path, data.table = TRUE, showProgress = FALSE)
}

read_optional <- function(path) {
  if (!file.exists(path)) {
    return(data.table())
  }
  data.table::fread(path, data.table = TRUE, showProgress = FALSE)
}

safe_pdf <- function(path, width = PDF_WIDTH, height = PDF_HEIGHT) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(path)) {
    removed <- file.remove(path)
    if (!isTRUE(removed)) {
      stop(
        "旧 PDF 正在被占用，无法覆盖：", path,
        "\n请关闭 Edge/Adobe/RStudio Viewer/文件资源管理器预览窗口后重跑。"
      )
    }
  }

  while (grDevices::dev.cur() > 1) {
    try(grDevices::dev.off(), silent = TRUE)
    if (grDevices::dev.cur() <= 1) break
  }

  grDevices::pdf(path, width = width, height = height, useDingbats = FALSE, onefile = TRUE)
}

finish_pdf <- function(path) {
  try(grDevices::dev.off(), silent = TRUE)

  if (!file.exists(path)) {
    stop("PDF 未生成：", path)
  }

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes < 1000) {
    stop("PDF 已创建但文件过小或无效：", path, "；size = ", size_bytes)
  }

  message("已保存 PDF：", normalizePath(path, winslash = "/", mustWork = TRUE),
          " | size = ", round(size_bytes / 1024, 1), " KB")
}

make_key <- function(dataset, object_id, group_id) {
  paste(dataset, object_id, as.character(group_id), sep = "||")
}

auc_base <- function(labels, scores) {
  labels <- as.integer(labels)
  scores <- as.numeric(scores)

  ok <- !is.na(labels) & !is.na(scores) & is.finite(scores)
  labels <- labels[ok]
  scores <- scores[ok]

  if (length(unique(labels)) < 2) return(NA_real_)

  pos <- scores[labels == 1]
  neg <- scores[labels == 0]

  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)

  r <- rank(c(pos, neg), ties.method = "average")
  n_pos <- length(pos)
  n_neg <- length(neg)

  auc <- (sum(r[seq_len(n_pos)]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  as.numeric(auc)
}

feature_label_correlation <- function(labels, x) {
  labels <- as.numeric(labels)
  x <- as.numeric(x)

  ok <- !is.na(labels) & !is.na(x) & is.finite(x)
  labels <- labels[ok]
  x <- x[ok]

  if (length(labels) < 3) return(NA_real_)
  if (length(unique(labels)) < 2) return(NA_real_)
  if (length(unique(x)) < 2) return(NA_real_)

  suppressWarnings(as.numeric(cor(labels, x, method = "pearson")))
}

classify_feature_leakage <- function(feature_name) {
  f <- tolower(feature_name)

  if (grepl("weak_label|label|class|story|priority|contrast", f)) {
    return("label_or_outcome_EXCLUDE")
  }

  if (grepl("^n_cells$|n_cells|cell_count|total_cells|group_rows|object_group_rows|dataset_group_rows", f)) {
    return("technical_or_weight_EXCLUDE")
  }

  if (grepl("fraction|favorable_minus|composition|object_comp|dataset_comp", f)) {
    return("label_derived_composition_EXCLUDE")
  }

  direct_exact <- c(
    "da_like_composite_score",
    "projection_competence_composite_score",
    "da_projection_competence_composite_score",
    "safety_risk_composite_05b",
    "safety_cell_cycle_score_05b",
    "safety_progenitor_score_05b",
    "safety_pluripotency_score_05b",
    "safety_stress_score_05b",
    "safety_ecm_score_05b",
    "safety_vascular_score_05b"
  )

  if (f %in% direct_exact) {
    return("direct_label_defining_CRITICAL_CIRCULARITY")
  }

  if (grepl("da|dopamin|projection|neurite|axon|synap|safety|risk|progenitor|pluripot|cell_cycle|cycling|stress|ecm|vascular|stromal", f)) {
    return("label_adjacent_signature_HIGH_CIRCULARITY")
  }

  if (grepl("a9|a10|calb|sox6|agtr", f)) {
    return("identity_related_signature_MODERATE_CIRCULARITY")
  }

  "candidate_non_direct_feature"
}

is_numeric_nonempty <- function(x) {
  is.numeric(x) && any(!is.na(x) & is.finite(x))
}

safe_col_subset <- function(dt, cols) {
  cols <- unique(cols)
  cols <- cols[cols %in% names(dt)]
  dt[, cols, with = FALSE]
}

wrap_text_09B <- function(x, width = 28) {
  vapply(as.character(x), function(s) {
    s <- gsub("_", " ", s)
    paste(strwrap(s, width = width), collapse = "\n")
  }, character(1))
}

short_leakage_category_09B <- function(x) {
  x <- as.character(x)

  out <- x
  out[x == "label_adjacent_signature_HIGH_CIRCULARITY"] <- "Label-adjacent signature\nHIGH circularity"
  out[x == "direct_label_defining_CRITICAL_CIRCULARITY"] <- "Direct label-defining\nCRITICAL circularity"
  out[x == "candidate_non_direct_feature"] <- "Candidate non-direct\nfeature"
  out[x == "technical_or_weight_EXCLUDE"] <- "Technical / weight\nEXCLUDE"
  out[x == "label_derived_composition_EXCLUDE"] <- "Label-derived composition\nEXCLUDE"
  out[x == "identity_related_signature_MODERATE_CIRCULARITY"] <- "Identity-related signature\nMODERATE circularity"
  out[x == "label_or_outcome_EXCLUDE"] <- "Label / outcome\nEXCLUDE"

  ifelse(out == x, wrap_text_09B(out, width = 28), out)
}

short_task_09B <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "ideal_like_classifier", "Ideal-like", x)
  x <- ifelse(x == "safety_risk_classifier", "Safety-risk", x)
  x
}

short_feature_09B <- function(x) {
  x <- as.character(x)
  x <- gsub("_composite_score", " composite", x)
  x <- gsub("_score_05B", " score", x)
  x <- gsub("_score_05A", " score", x)
  x <- gsub("^marker_", "marker: ", x)
  x <- gsub("_", " ", x)
  x
}

make_top_label_09B <- function(task, feature) {
  paste0(short_task_09B(task), " | ", short_feature_09B(feature))
}

stamp("读取 07A / 05B / 09A 输入。")

master <- read_required(input_07A_master)
contrast05B <- read_optional(input_05B_contrast)
ideal07A <- read_optional(input_07A_ideal)
safety07A <- read_optional(input_07A_safety)
feature07A <- read_optional(input_07A_feature_dict)
object09A <- read_optional(input_09A_object_priority)
dataset09A <- read_optional(input_09A_dataset_priority)
audit09A <- read_optional(input_09A_fraction_sum_audit)

input_audit <- data.table(
  input_name = c(
    "07A_master",
    "07A_ideal_training",
    "07A_safety_training",
    "07A_feature_dictionary",
    "05B_contrast",
    "09A_object_priority",
    "09A_dataset_priority",
    "09A_fraction_sum_audit"
  ),
  path = c(
    input_07A_master,
    input_07A_ideal,
    input_07A_safety,
    input_07A_feature_dict,
    input_05B_contrast,
    input_09A_object_priority,
    input_09A_dataset_priority,
    input_09A_fraction_sum_audit
  ),
  exists = file.exists(c(
    input_07A_master,
    input_07A_ideal,
    input_07A_safety,
    input_07A_feature_dict,
    input_05B_contrast,
    input_09A_object_priority,
    input_09A_dataset_priority,
    input_09A_fraction_sum_audit
  )),
  rows = c(
    nrow(master),
    nrow(ideal07A),
    nrow(safety07A),
    nrow(feature07A),
    nrow(contrast05B),
    nrow(object09A),
    nrow(dataset09A),
    nrow(audit09A)
  )
)

atomic_write_csv(as.data.frame(input_audit), input_audit_csv)

required_master_cols <- c("dataset", "object_id", "group_id")
missing_master <- setdiff(required_master_cols, names(master))
if (length(missing_master) > 0) {
  stop("07A master 缺少必要列：", paste(missing_master, collapse = ", "))
}

master[, dataset := as.character(dataset)]
master[, object_id := as.character(object_id)]
master[, group_id := as.character(group_id)]

if (!"group_key" %in% names(master)) {
  master[, group_key := make_key(dataset, object_id, group_id)]
}

stamp("07A master rows：", nrow(master))
stamp("07A master columns：", ncol(master))

stamp("标准化 marker-rule-derived labels。")

if (!"safety_contrast_class_05B" %in% names(master)) {
  if (nrow(contrast05B) > 0 && all(c("dataset", "object_id", "group_id", "safety_contrast_class_05B") %in% names(contrast05B))) {
    contrast05B[, group_id := as.character(group_id)]
    contrast05B[, group_key := make_key(dataset, object_id, group_id)]
    master <- merge(
      master,
      unique(contrast05B[, .(group_key, safety_contrast_class_05B)]),
      by = "group_key",
      all.x = TRUE
    )
  } else {
    stop("master 和 05B contrast 中都找不到 safety_contrast_class_05B。")
  }
}

if (!"ideal_graft_like_weak_label" %in% names(master)) {
  master[, ideal_graft_like_weak_label := fifelse(
    safety_contrast_class_05B == "ideal_DA_projection_high_safety_low",
    1L,
    fifelse(
      safety_contrast_class_05B %in% c(
        "high_safety_risk_low_DA",
        "mixed_DA_or_projection_with_safety_risk",
        "lower_priority_or_mixed"
      ),
      0L,
      NA_integer_
    )
  )]
}

if (!"safety_risk_weak_label" %in% names(master)) {
  master[, safety_risk_weak_label := fifelse(
    safety_contrast_class_05B %in% c(
      "high_safety_risk_low_DA",
      "mixed_DA_or_projection_with_safety_risk"
    ),
    1L,
    fifelse(
      safety_contrast_class_05B %in% c(
        "ideal_DA_projection_high_safety_low",
        "projection_competence_without_DA_low_safety",
        "lower_priority_or_mixed"
      ),
      0L,
      NA_integer_
    )
  )]
}

master[, ideal_graft_like_weak_label := as.integer(ideal_graft_like_weak_label)]
master[, safety_risk_weak_label := as.integer(safety_risk_weak_label)]

if (!"n_cells" %in% names(master)) {
  if ("n_cells_05B" %in% names(master)) {
    master[, n_cells := num(n_cells_05B)]
  } else if ("n_cells_05A" %in% names(master)) {
    master[, n_cells := num(n_cells_05A)]
  } else {
    master[, n_cells := NA_real_]
  }
}

master[, n_cells := num(n_cells)]
master[, sample_weight_equal := 1]
master[, sample_weight_sqrt_cells := sqrt(pmax(n_cells, 1))]

stamp("合并 09A object-level composition 信息，标记为 label-derived composition。")

object_comp_duplicate_audit <- data.table(
  dataset = character(),
  object_id = character(),
  n_rows_in_09A_object_priority = integer()
)

if (nrow(object09A) > 0 && all(c("dataset", "object_id") %in% names(object09A))) {
  object09A[, dataset := as.character(dataset)]
  object09A[, object_id := as.character(object_id)]

  object_comp_duplicate_audit <- object09A[
    ,
    .(n_rows_in_09A_object_priority = .N),
    by = .(dataset, object_id)
  ][n_rows_in_09A_object_priority > 1]

  atomic_write_csv(as.data.frame(object_comp_duplicate_audit), object_comp_duplicate_audit_csv)

  comp_cols <- intersect(
    c(
      "ideal_fraction",
      "lower_priority_fraction",
      "safety_risk_fraction",
      "other_fraction",
      "favorable_minus_safety_index",
      "favorable_minus_lower_priority_index",
      "object_total_cells",
      "object_group_rows"
    ),
    names(object09A)
  )

  if (length(comp_cols) > 0) {
    object_comp_raw <- object09A[, c("dataset", "object_id", comp_cols), with = FALSE]

    numeric_comp_cols <- comp_cols[
      vapply(object_comp_raw[, comp_cols, with = FALSE], function(z) {
        is.numeric(z) || is.integer(z)
      }, logical(1))
    ]

    non_numeric_comp_cols <- setdiff(comp_cols, numeric_comp_cols)

    if (length(numeric_comp_cols) > 0) {
      object_comp_num <- object_comp_raw[
        ,
        lapply(.SD, function(z) mean(num(z), na.rm = TRUE)),
        by = .(dataset, object_id),
        .SDcols = numeric_comp_cols
      ]

      for (cc in numeric_comp_cols) {
        object_comp_num[is.nan(get(cc)), (cc) := NA_real_]
      }
    } else {
      object_comp_num <- unique(object_comp_raw[, .(dataset, object_id)])
    }

    if (length(non_numeric_comp_cols) > 0) {
      object_comp_chr <- object_comp_raw[
        ,
        lapply(.SD, function(z) {
          z <- as.character(z)
          z <- z[!is.na(z) & z != ""]
          if (length(z) == 0) NA_character_ else z[1]
        }),
        by = .(dataset, object_id),
        .SDcols = non_numeric_comp_cols
      ]

      object_comp <- merge(
        object_comp_num,
        object_comp_chr,
        by = c("dataset", "object_id"),
        all = TRUE
      )
    } else {
      object_comp <- object_comp_num
    }

    key_check <- object_comp[, .N, by = .(dataset, object_id)][N > 1]
    if (nrow(key_check) > 0) {
      print(key_check)
      stop("09B V2 object_comp 唯一化后仍存在重复 dataset/object_id key。")
    }

    setnames(
      object_comp,
      old = comp_cols,
      new = paste0("object_comp_", comp_cols),
      skip_absent = TRUE
    )

    before_n <- nrow(master)

    master <- merge(
      master,
      object_comp,
      by = c("dataset", "object_id"),
      all.x = TRUE,
      allow.cartesian = FALSE
    )

    after_n <- nrow(master)

    if (after_n != before_n) {
      stop(
        "合并 09A object composition 后 master 行数改变：before=",
        before_n,
        " after=",
        after_n,
        "。这不允许。"
      )
    }
  } else {
    atomic_write_csv(as.data.frame(object_comp_duplicate_audit), object_comp_duplicate_audit_csv)
  }
} else {
  atomic_write_csv(as.data.frame(object_comp_duplicate_audit), object_comp_duplicate_audit_csv)
}

stamp("构建 feature dictionary + leakage risk audit。")

id_cols <- c(
  "dataset", "object_id", "group_id", "group_key"
)

known_label_cols <- c(
  "ideal_graft_like_weak_label",
  "safety_risk_weak_label",
  "safety_contrast_class_05B",
  "A9_A10_bias_label_05B",
  "story_priority_05B",
  "ML_label_source",
  "ML_claim_boundary"
)

numeric_cols <- names(master)[vapply(master, is_numeric_nonempty, logical(1))]

candidate_numeric_features <- setdiff(
  numeric_cols,
  c(id_cols, known_label_cols)
)

feature_dict <- data.table(
  feature = candidate_numeric_features
)

feature_dict[, leakage_category := vapply(feature, classify_feature_leakage, character(1))]

feature_dict[, include_full_exploratory := !leakage_category %in% c(
  "label_or_outcome_EXCLUDE",
  "technical_or_weight_EXCLUDE",
  "label_derived_composition_EXCLUDE"
)]

feature_dict[, include_reduced_non_direct := leakage_category %in% c(
  "candidate_non_direct_feature",
  "identity_related_signature_MODERATE_CIRCULARITY"
)]

feature_dict[, include_strict_non_direct := leakage_category == "candidate_non_direct_feature"]

feature_dict[, recommended_use := fifelse(
  leakage_category == "direct_label_defining_CRITICAL_CIRCULARITY",
  "exclude_from_primary_ML_use_only_rule_recapitulation",
  fifelse(
    leakage_category == "label_adjacent_signature_HIGH_CIRCULARITY",
    "high_circularity_use_only_sensitivity",
    fifelse(
      leakage_category == "identity_related_signature_MODERATE_CIRCULARITY",
      "moderate_circularity_candidate_for_reduced_model_with_caution",
      fifelse(
        leakage_category == "candidate_non_direct_feature",
        "candidate_for_primary_reduced_model",
        "exclude_from_training"
      )
    )
  )
)]

feature_dict[, non_missing_n := vapply(feature, function(fc) sum(!is.na(master[[fc]]) & is.finite(master[[fc]])), integer(1))]
feature_dict[, non_missing_fraction := non_missing_n / nrow(master)]
feature_dict[, unique_values_n := vapply(feature, function(fc) length(unique(master[[fc]][!is.na(master[[fc]])])), integer(1))]

feature_dict[, usable_numeric := non_missing_n > 0 & unique_values_n >= 2]

feature_dict[, include_full_exploratory := include_full_exploratory & usable_numeric]
feature_dict[, include_reduced_non_direct := include_reduced_non_direct & usable_numeric]
feature_dict[, include_strict_non_direct := include_strict_non_direct & usable_numeric]

full_features <- feature_dict[include_full_exploratory == TRUE & !is.na(include_full_exploratory), feature]
reduced_features <- feature_dict[include_reduced_non_direct == TRUE & !is.na(include_reduced_non_direct), feature]
strict_features <- feature_dict[include_strict_non_direct == TRUE & !is.na(include_strict_non_direct), feature]

if (length(full_features) == 0) {
  stop("没有可用于 full exploratory ML 的 numeric features。")
}

if (length(reduced_features) == 0) {
  warning("reduced non-direct feature set 为空；09C primary ML 可能需要重新定义非直接特征。")
}

feature_set_summary <- data.table(
  feature_set = c(
    "full_exploratory",
    "reduced_non_direct",
    "strict_non_direct",
    "direct_label_defining",
    "label_adjacent_high_circularity",
    "excluded_label_or_technical_or_composition"
  ),
  n_features = c(
    length(full_features),
    length(reduced_features),
    length(strict_features),
    nrow(feature_dict[leakage_category == "direct_label_defining_CRITICAL_CIRCULARITY"]),
    nrow(feature_dict[leakage_category == "label_adjacent_signature_HIGH_CIRCULARITY"]),
    nrow(feature_dict[include_full_exploratory != TRUE])
  ),
  recommended_downstream_use = c(
    "Exploratory rule-recapitulation only; high risk of circularity if direct label-defining features dominate.",
    "Recommended primary 09C candidate if enough features remain; still marker-rule-derived exploratory.",
    "Strictest non-direct sensitivity set; may be too small.",
    "Do not use for primary ML; keep for audit and rule-recapitulation sensitivity.",
    "Avoid for primary claims; use only sensitivity with explicit circularity warning.",
    "Exclude from training."
  )
)

atomic_write_csv(as.data.frame(feature_dict), feature_dictionary_csv)
atomic_write_csv(as.data.frame(feature_set_summary), feature_set_summary_csv)

stamp("生成 ML-ready training tables。")

label_definitions <- data.table(
  task = c("ideal_like_classifier", "safety_risk_classifier"),
  label_column = c("ideal_graft_like_weak_label", "safety_risk_weak_label"),
  positive_definition = c(
    "safety_contrast_class_05B == ideal_DA_projection_high_safety_low",
    "safety_contrast_class_05B in high_safety_risk_low_DA or mixed_DA_or_projection_with_safety_risk"
  ),
  negative_definition = c(
    "selected non-ideal classes defined by 07A/05B marker-rule-derived rules",
    "selected non-high-safety-risk classes defined by 07A/05B marker-rule-derived rules"
  ),
  label_source = c("05B rule-derived marker-rule-derived label", "05B rule-derived marker-rule-derived label"),
  claim_boundary = c(
    "not experimental ground truth; not validated graft outcome",
    "not clinical safety label; not tumorigenicity proof"
  )
)

atomic_write_csv(as.data.frame(label_definitions), label_definition_csv)

base_cols <- unique(c(
  "dataset",
  "object_id",
  "group_id",
  "group_key",
  "safety_contrast_class_05B",
  "n_cells",
  "sample_weight_equal",
  "sample_weight_sqrt_cells"
))

make_training_table <- function(dt, label_col, feature_cols, task_name) {
  cols <- unique(c(base_cols, label_col, feature_cols))
  cols <- cols[cols %in% names(dt)]

  out <- dt[!is.na(get(label_col)), cols, with = FALSE]
  setnames(out, old = label_col, new = "weak_label", skip_absent = TRUE)

  out[, task := task_name]
  setcolorder(out, c("task", "weak_label", intersect(base_cols, names(out)), setdiff(names(out), c("task", "weak_label", base_cols))))

  out
}

ideal_reduced <- make_training_table(
  master,
  "ideal_graft_like_weak_label",
  reduced_features,
  "ideal_like_classifier_reduced_non_direct"
)

safety_reduced <- make_training_table(
  master,
  "safety_risk_weak_label",
  reduced_features,
  "safety_risk_classifier_reduced_non_direct"
)

ideal_full <- make_training_table(
  master,
  "ideal_graft_like_weak_label",
  full_features,
  "ideal_like_classifier_full_exploratory"
)

safety_full <- make_training_table(
  master,
  "safety_risk_weak_label",
  full_features,
  "safety_risk_classifier_full_exploratory"
)

atomic_write_csv(as.data.frame(master), ml_master_csv)
atomic_write_csv(as.data.frame(ideal_reduced), ideal_reduced_csv)
atomic_write_csv(as.data.frame(safety_reduced), safety_reduced_csv)
atomic_write_csv(as.data.frame(ideal_full), ideal_full_csv)
atomic_write_csv(as.data.frame(safety_full), safety_full_csv)

stamp("计算 feature-label association audit。")

task_specs <- list(
  ideal_like_classifier = "ideal_graft_like_weak_label",
  safety_risk_classifier = "safety_risk_weak_label"
)

leakage_list <- list()

for (task_name in names(task_specs)) {
  label_col <- task_specs[[task_name]]
  labels <- master[[label_col]]

  for (fc in candidate_numeric_features) {
    if (!fc %in% names(master)) next

    x <- master[[fc]]

    cor_val <- feature_label_correlation(labels, x)
    auc_val <- auc_base(labels, x)
    auc_sep <- ifelse(is.na(auc_val), NA_real_, max(auc_val, 1 - auc_val))

    leakage_list[[length(leakage_list) + 1L]] <- data.table(
      task = task_name,
      label_col = label_col,
      feature = fc,
      leakage_category = feature_dict[feature == fc, leakage_category][1],
      include_full_exploratory = feature_dict[feature == fc, include_full_exploratory][1],
      include_reduced_non_direct = feature_dict[feature == fc, include_reduced_non_direct][1],
      n_non_missing_pair = sum(!is.na(labels) & !is.na(x) & is.finite(num(x))),
      pearson_cor_with_label = cor_val,
      abs_pearson_cor_with_label = abs(cor_val),
      univariate_auc = auc_val,
      univariate_auc_separation = auc_sep
    )
  }
}

leakage_audit <- rbindlist(leakage_list, fill = TRUE)

leakage_audit[, statistical_separation_flag := fifelse(
  !is.na(univariate_auc_separation) & univariate_auc_separation >= 0.95,
  "very_high_univariate_separation_AUC_ge_0.95",
  fifelse(
    !is.na(abs_pearson_cor_with_label) & abs_pearson_cor_with_label >= 0.90,
    "very_high_correlation_abs_cor_ge_0.90",
    "not_extreme_by_univariate_audit"
  )
)]

leakage_audit[, primary_ML_recommendation := fifelse(
  leakage_category == "direct_label_defining_CRITICAL_CIRCULARITY",
  "exclude_from_primary_09C_model",
  fifelse(
    leakage_category == "label_adjacent_signature_HIGH_CIRCULARITY",
    "avoid_primary_use_or_report_only_as_sensitivity",
    fifelse(
      include_reduced_non_direct == TRUE,
      "eligible_for_reduced_non_direct_09C_model",
      "exclude_or_context_only"
    )
  )
)]

setorder(leakage_audit, task, -univariate_auc_separation, -abs_pearson_cor_with_label)

atomic_write_csv(as.data.frame(leakage_audit), leakage_audit_csv)

stamp("生成 class balance 和 leave-one-dataset-out feasibility plan。")

class_balance_list <- list()

for (task_name in names(task_specs)) {
  label_col <- task_specs[[task_name]]

  cb <- master[!is.na(get(label_col)), .(
    n_groups = .N,
    positives = sum(get(label_col) == 1, na.rm = TRUE),
    negatives = sum(get(label_col) == 0, na.rm = TRUE),
    positive_fraction = mean(get(label_col) == 1, na.rm = TRUE),
    total_cells = sum(n_cells, na.rm = TRUE)
  ), by = dataset]

  cb[, task := task_name]
  cb[, label_col := label_col]
  class_balance_list[[length(class_balance_list) + 1L]] <- cb
}

class_balance <- rbindlist(class_balance_list, fill = TRUE)
setcolorder(class_balance, c("task", "label_col", "dataset"))

atomic_write_csv(as.data.frame(class_balance), class_balance_csv)

lodo_list <- list()

for (task_name in names(task_specs)) {
  label_col <- task_specs[[task_name]]
  dsets <- sort(unique(master[!is.na(get(label_col)), dataset]))

  for (ds in dsets) {
    test_dt <- master[dataset == ds & !is.na(get(label_col))]
    train_dt <- master[dataset != ds & !is.na(get(label_col))]

    lodo_list[[length(lodo_list) + 1L]] <- data.table(
      task = task_name,
      label_col = label_col,
      leave_out_dataset = ds,
      train_n = nrow(train_dt),
      train_pos = sum(train_dt[[label_col]] == 1, na.rm = TRUE),
      train_neg = sum(train_dt[[label_col]] == 0, na.rm = TRUE),
      test_n = nrow(test_dt),
      test_pos = sum(test_dt[[label_col]] == 1, na.rm = TRUE),
      test_neg = sum(test_dt[[label_col]] == 0, na.rm = TRUE),
      train_has_both_classes = length(unique(train_dt[[label_col]])) == 2,
      test_has_both_classes = length(unique(test_dt[[label_col]])) == 2,
      auc_evaluable = length(unique(test_dt[[label_col]])) == 2,
      lodo_recommendation = ifelse(
        length(unique(train_dt[[label_col]])) == 2 && length(unique(test_dt[[label_col]])) == 2,
        "usable_for_LODO_AUC",
        "not_AUC_evaluable_for_this_left_out_dataset"
      )
    )
  }
}

lodo_plan <- rbindlist(lodo_list, fill = TRUE)
atomic_write_csv(as.data.frame(lodo_plan), lodo_plan_csv)

ideal_n_pos <- sum(master$ideal_graft_like_weak_label == 1, na.rm = TRUE)
ideal_n_neg <- sum(master$ideal_graft_like_weak_label == 0, na.rm = TRUE)
safety_n_pos <- sum(master$safety_risk_weak_label == 1, na.rm = TRUE)
safety_n_neg <- sum(master$safety_risk_weak_label == 0, na.rm = TRUE)

readiness_summary <- data.table(
  item = c(
    "master_rows",
    "master_columns",
    "datasets",
    "objects",
    "full_exploratory_features",
    "reduced_non_direct_features",
    "strict_non_direct_features",
    "ideal_positive_groups",
    "ideal_negative_groups",
    "safety_positive_groups",
    "safety_negative_groups",
    "ideal_LODO_auc_evaluable_splits",
    "safety_LODO_auc_evaluable_splits",
    "critical_circularity_features",
    "high_circularity_features"
  ),
  value = c(
    nrow(master),
    ncol(master),
    uniqueN(master$dataset),
    uniqueN(paste(master$dataset, master$object_id, sep = "||")),
    length(full_features),
    length(reduced_features),
    length(strict_features),
    ideal_n_pos,
    ideal_n_neg,
    safety_n_pos,
    safety_n_neg,
    nrow(lodo_plan[task == "ideal_like_classifier" & auc_evaluable == TRUE]),
    nrow(lodo_plan[task == "safety_risk_classifier" & auc_evaluable == TRUE]),
    nrow(feature_dict[leakage_category == "direct_label_defining_CRITICAL_CIRCULARITY"]),
    nrow(feature_dict[leakage_category == "label_adjacent_signature_HIGH_CIRCULARITY"])
  )
)

readiness_summary[, interpretation := c(
  "Number of group-level rows available for ML-ready tables.",
  "Total columns after adding audit/context variables.",
  "Number of datasets represented.",
  "Number of objects represented.",
  "All non-excluded numeric features; exploratory and circularity-prone.",
  "Features not directly labelled as direct label-defining; recommended primary 09C candidate set if non-empty.",
  "Strictest non-direct features only.",
  "Ideal-like marker-rule-derived positives.",
  "Ideal-like marker-rule-derived negatives.",
  "Safety-risk marker-rule-derived positives.",
  "Safety-risk marker-rule-derived negatives.",
  "LODO splits with both classes in the left-out test dataset for ideal task.",
  "LODO splits with both classes in the left-out test dataset for safety task.",
  "Features that directly define or strongly encode the marker-rule-derived rule.",
  "Signature features adjacent to label construction; use cautiously."
)]

readiness_summary[, status := "recorded"]

if (ideal_n_pos < MIN_POS_PER_TASK || ideal_n_neg < MIN_NEG_PER_TASK) {
  readiness_summary[item %in% c("ideal_positive_groups", "ideal_negative_groups"), status := "warning_low_class_count"]
}

if (safety_n_pos < MIN_POS_PER_TASK || safety_n_neg < MIN_NEG_PER_TASK) {
  readiness_summary[item %in% c("safety_positive_groups", "safety_negative_groups"), status := "warning_low_class_count"]
}

if (length(reduced_features) == 0) {
  readiness_summary[item == "reduced_non_direct_features", status := "warning_empty_reduced_feature_set"]
}

atomic_write_csv(as.data.frame(readiness_summary), readiness_summary_csv)

stamp("生成 09B PDF-only audit figures。")

plot_class_balance <- function(cb, task_name, pdf_path, title) {
  dt <- copy(cb[task == task_name])
  if (nrow(dt) == 0) return(invisible(NULL))

  dt <- dt[order(-positive_fraction)]
  mat <- rbind(dt$positives, dt$negatives)
  colnames(mat) <- dt$dataset
  rownames(mat) <- c("Positive", "Negative")

  safe_pdf(pdf_path, width = PDF_WIDTH, height = PDF_HEIGHT)

  par(mar = c(8, 5, 4, 9), xpd = FALSE)
  barplot(
    mat,
    beside = FALSE,
    col = c("#D73027", "#4575B4"),
    border = "grey25",
    las = 2,
    ylab = "Number of groups",
    main = title,
    cex.names = 0.75
  )
  legend(
    "topright",
    inset = c(-0.23, 0),
    legend = rownames(mat),
    fill = c("#D73027", "#4575B4"),
    bty = "n",
    xpd = TRUE
  )

  finish_pdf(pdf_path)
}

plot_class_balance(
  class_balance,
  "ideal_like_classifier",
  fig_ideal_balance_pdf,
  "09B ideal-like marker-rule-derived class balance"
)

plot_class_balance(
  class_balance,
  "safety_risk_classifier",
  fig_safety_balance_pdf,
  "09B safety-risk marker-rule-derived class balance"
)

leak_plot_dt <- leakage_audit[
  include_full_exploratory == TRUE &
    !is.na(univariate_auc_separation) &
    is.finite(univariate_auc_separation)
]

if (nrow(leak_plot_dt) > 0) {
  leak_plot_dt <- leak_plot_dt[
    order(-univariate_auc_separation)
  ][seq_len(min(TOP_FEATURES_FOR_LEAKAGE_PLOT, .N))]

  leak_plot_dt[, plot_label_raw := make_top_label_09B(task, feature)]
  leak_plot_dt[, plot_label := wrap_text_09B(plot_label_raw, width = 40)]

  leak_plot_dt <- leak_plot_dt[order(univariate_auc_separation)]

  top_pdf_height <- max(9.0, 0.34 * nrow(leak_plot_dt) + 2.6)

  safe_pdf(fig_leakage_top_pdf, width = 16.5, height = top_pdf_height)

  par(mar = c(5.2, 20.5, 4.2, 2.0), xpd = FALSE)

  bp_leak <- barplot(
    leak_plot_dt$univariate_auc_separation,
    names.arg = leak_plot_dt$plot_label,
    horiz = TRUE,
    las = 1,
    xlab = "Univariate label-separation AUC",
    main = "09B top feature-label association audit",
    col = "grey58",
    border = "grey25",
    cex.names = 0.62,
    cex.axis = 0.90,
    cex.lab = 1.05,
    xlim = c(0, 1.04)
  )

  abline(v = 0.95, lty = 2, col = "red", lwd = 1.1)

  text(
    x = pmin(leak_plot_dt$univariate_auc_separation + 0.015, 1.02),
    y = bp_leak,
    labels = sprintf("%.2f", leak_plot_dt$univariate_auc_separation),
    cex = 0.58,
    adj = 0
  )

  legend(
    "bottomright",
    legend = "Red dashed line = AUC 0.95",
    lty = 2,
    col = "red",
    bty = "n",
    cex = 0.82
  )

  finish_pdf(fig_leakage_top_pdf)
}

cat_summary <- feature_dict[, .N, by = leakage_category][order(N)]

if (nrow(cat_summary) > 0) {
  cat_summary[, category_label := short_leakage_category_09B(leakage_category)]

  safe_pdf(fig_feature_category_pdf, width = 12.5, height = 7.2)

  par(mar = c(5.2, 16.5, 4.2, 2.0), xpd = FALSE)

  bp_cat <- barplot(
    cat_summary$N,
    names.arg = cat_summary$category_label,
    horiz = TRUE,
    las = 1,
    xlab = "Number of numeric features",
    main = "09B feature leakage category summary",
    col = "grey62",
    border = "grey25",
    cex.names = 0.80,
    cex.axis = 0.90,
    cex.lab = 1.05,
    xlim = c(0, max(cat_summary$N, na.rm = TRUE) * 1.22)
  )

  text(
    x = cat_summary$N + max(cat_summary$N, na.rm = TRUE) * 0.035,
    y = bp_cat,
    labels = cat_summary$N,
    cex = 0.82,
    adj = 0
  )

  finish_pdf(fig_feature_category_pdf)
}

python_manifest_lines <- c(
  "09B Python ML input manifest",
  "",
  "Recommended primary 09C input files:",
  paste0("ideal_like_reduced_non_direct: ", ideal_reduced_csv),
  paste0("safety_risk_reduced_non_direct: ", safety_reduced_csv),
  "",
  "Exploratory sensitivity input files:",
  paste0("ideal_like_full_exploratory: ", ideal_full_csv),
  paste0("safety_risk_full_exploratory: ", safety_full_csv),
  "",
  "Audit files:",
  paste0("feature_dictionary: ", feature_dictionary_csv),
  paste0("leakage_audit: ", leakage_audit_csv),
  paste0("class_balance: ", class_balance_csv),
  paste0("LODO_plan: ", lodo_plan_csv),
  "",
  "Important rule:",
  "Use reduced_non_direct tables for primary exploratory ML.",
  "Use full_exploratory tables only as rule-recapitulation / sensitivity analysis.",
  "All labels are marker-rule-derived labels derived from 05B rules, not experimental ground truth."
)

writeLines(python_manifest_lines, python_manifest_txt)

method_lines <- c(
  "09B ML-ready dataset construction and leakage/circularity audit",
  "",
  "Method-ready wording:",
  paste0(
    "Group-level machine-learning-ready datasets were constructed from the 07A contrast-based master table, ",
    "which was derived from the 05B DA/projection-vs-safety marker-rule-derived classification. ",
    "The labels used for the ideal-like and safety-risk tasks were treated as rule-derived marker-rule-derived labels rather than experimental ground truth. ",
    "To reduce circularity, features were annotated according to their relationship to label construction. ",
    "Direct label-defining DA/projection and safety composite scores were flagged as critical circularity features and excluded from the reduced non-direct feature tables. ",
    "The full exploratory tables were retained only for rule-recapitulation sensitivity analysis. ",
    "Leave-one-dataset-out feasibility and class balance were audited before downstream model training."
  ),
  "",
  "Claim boundary:",
  "09B does not train or validate an ML model.",
  "09B prepares marker-rule-derived prioritization model input tables and audits circularity/leakage risk.",
  "Any downstream AUC should be interpreted as exploratory marker-rule-derived performance, not clinical prediction, treatment outcome prediction, or validated safety prediction.",
  "",
  "Recommended 09C primary input:",
  ideal_reduced_csv,
  safety_reduced_csv
)

writeLines(method_lines, method_note_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

report_lines <- c(
  "09B ML-ready dataset and leakage audit FINAL V4 FULL FIXED-LAYOUT report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input audit:",
  capture.output(print(input_audit)),
  "",
  "Readiness summary:",
  capture.output(print(readiness_summary)),
  "",
  "Feature set summary:",
  capture.output(print(feature_set_summary)),
  "",
  "Class balance:",
  capture.output(print(class_balance)),
  "",
  "LODO feasibility:",
  capture.output(print(lodo_plan)),
  "",
  "Output directory:",
  out_tables_dir,
  out_figures_dir
)

writeLines(report_lines, report_txt)

required_output_files <- c(
  ml_master_csv,
  feature_dictionary_csv,
  feature_set_summary_csv,
  label_definition_csv,
  leakage_audit_csv,
  class_balance_csv,
  lodo_plan_csv,
  readiness_summary_csv,
  input_audit_csv,
  object_comp_duplicate_audit_csv,
  ideal_reduced_csv,
  safety_reduced_csv,
  ideal_full_csv,
  safety_full_csv,
  python_manifest_txt,
  method_note_txt,
  session_info_txt,
  report_txt,
  fig_ideal_balance_pdf,
  fig_safety_balance_pdf,
  fig_leakage_top_pdf,
  fig_feature_category_pdf
)

output_check <- data.table(
  file = required_output_files,
  exists = file.exists(required_output_files),
  size_bytes = ifelse(
    file.exists(required_output_files),
    file.info(required_output_files)$size,
    NA_real_
  )
)

atomic_write_csv(as.data.frame(output_check), output_check_csv)

bad_outputs <- output_check[!exists | is.na(size_bytes) | size_bytes <= 0]

if (nrow(bad_outputs) > 0) {
  print(bad_outputs)
  stop("09B 输出验证失败。")
}

cat("\n============================================================\n")
cat("09B ML-ready dataset and leakage audit FINAL V4 FULL FIXED-LAYOUT 运行结束\n")
cat("============================================================\n\n")

cat("Master rows：", nrow(master), "\n")
cat("Datasets：", uniqueN(master$dataset), "\n")
cat("Objects：", uniqueN(paste(master$dataset, master$object_id, sep = '||')), "\n")
cat("Full exploratory features：", length(full_features), "\n")
cat("Reduced non-direct features：", length(reduced_features), "\n")
cat("Strict non-direct features：", length(strict_features), "\n\n")

cat("Ideal-like labels：positive=", ideal_n_pos, " negative=", ideal_n_neg, "\n", sep = "")
cat("Safety-risk labels：positive=", safety_n_pos, " negative=", safety_n_neg, "\n\n", sep = "")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("推荐给 09C 的 primary ML input：\n")
cat(ideal_reduced_csv, "\n")
cat(safety_reduced_csv, "\n\n")

cat("exploratory sensitivity input：\n")
cat(ideal_full_csv, "\n")
cat(safety_full_csv, "\n\n")

cat("关键审计文件：\n")
cat(feature_dictionary_csv, "\n")
cat(leakage_audit_csv, "\n")
cat(class_balance_csv, "\n")
cat(lodo_plan_csv, "\n")
cat(readiness_summary_csv, "\n")
cat(object_comp_duplicate_audit_csv, "\n")
cat(method_note_txt, "\n\n")

cat("✅ 09B ML-ready dataset and leakage audit FINAL V4 FULL FIXED-LAYOUT 完成。\n")
