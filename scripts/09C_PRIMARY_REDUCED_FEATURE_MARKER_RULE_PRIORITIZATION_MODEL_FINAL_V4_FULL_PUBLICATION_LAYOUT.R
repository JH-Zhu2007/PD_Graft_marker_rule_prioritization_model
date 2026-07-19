# 09C~09C_PRIMARY_REDUCED_FEATURE_MARKER_RULE_PRIORITIZATION_MODEL_FINAL_V4_FULL_PUBLICATION_LAYOUT.R ----
#
# ============================================================
# 09C_PRIMARY_REDUCED_FEATURE_MARKER_RULE_PRIORITIZATION_MODEL_FINAL_V4_FULL_PUBLICATION_LAYOUT.R
# ============================================================
# 09C 完整版：primary reduced-feature marker-rule-derived prioritization model + publication-layout figures
#
# 这是完整 09C，不是只修图：
#   1. 从 09B reduced non-direct feature tables 读取输入。
#   2. 训练 logistic regression baseline。
#   3. 如果 randomForest 已安装，训练 random forest。
#   4. 做 internal stratified K-fold CV。
#   5. 做 leave-one-dataset-out validation。
#   6. 输出 prediction / performance / feature importance / selected features。
#   7. 保留 row_id / fold / label-like feature preflight，防止技术列进入模型。
#   8. 生成投稿布局修正版 PDF：
#        - AUC 图增加数值标签，边距加大，避免文字超出。
#        - predicted probability 图图例上移，避免 Negative/Positive 和柱子重叠。
#        - feature importance 按 task/model 分开，并在组内归一化，避免
#          logistic coefficient 和 random forest importance 放在同一原始尺度比较。
#
# 严谨性原则：
#   - 只使用 09B reduced non-direct features 作为 primary ML 输入。
#   - 05B labels 是 rule-derived marker-rule-derived labels，不是实验 ground truth。
#   - 不能写成 clinical-use model。
#   - 不能写成 validated therapeutic outcome / safety prediction。
#
# 成功标志：
#   ✅ 09C primary reduced-feature marker-rule-derived prioritization model FINAL V4 FULL PUBLICATION LAYOUT 完成。
# ============================================================
#

# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"

SEED <- 20260714

K_FOLDS <- 5

# Logistic regression 为了避免小样本过拟合，每个 fold 只使用训练集内排序最高的前 MAX_GLM_FEATURES 个特征。
# 特征选择只在训练集内完成，不用 test fold 信息。
MAX_GLM_FEATURES <- 10

# Random forest 参数；只有 randomForest 包存在时才训练。
RF_NTREES <- 500
RF_MTRY_MODE <- "sqrt"   # "sqrt" or "all"

# PDF only
PDF_WIDTH <- 10.5
PDF_HEIGHT <- 7.0

# plot only
TOP_IMPORTANCE_FEATURES <- 20


# ============================================================
# 1. 加载包
# ============================================================

cat("\n============================================================\n")
cat("09C：primary reduced-feature marker-rule-derived prioritization model\n")
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

HAS_RANDOMFOREST <- requireNamespace("randomForest", quietly = TRUE)

if (HAS_RANDOMFOREST) {
  suppressPackageStartupMessages(library(randomForest))
} else {
  message("未检测到 randomForest 包：09C 会训练 logistic regression；random forest 会记录为 skipped。")
}

set.seed(SEED)


# ============================================================
# 2. 路径
# ============================================================

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_ideal_reduced <- file.path(
  tables_dir,
  "09B_ML_ready_dataset_and_leakage_audit_V3",
  "09B_ideal_like_training_reduced_non_direct_features.csv"
)

input_safety_reduced <- file.path(
  tables_dir,
  "09B_ML_ready_dataset_and_leakage_audit_V3",
  "09B_safety_risk_training_reduced_non_direct_features.csv"
)

input_09B_feature_dict <- file.path(
  tables_dir,
  "09B_ML_ready_dataset_and_leakage_audit_V3",
  "09B_feature_dictionary_with_leakage_risk.csv"
)

input_09B_leakage_audit <- file.path(
  tables_dir,
  "09B_ML_ready_dataset_and_leakage_audit_V3",
  "09B_feature_leakage_circularity_audit.csv"
)

input_09B_lodo_plan <- file.path(
  tables_dir,
  "09B_ML_ready_dataset_and_leakage_audit_V3",
  "09B_leave_one_dataset_out_feasibility_plan.csv"
)

out_tables_dir <- file.path(
  tables_dir,
  "09C_primary_reduced_feature_weak_label_ML_V4_FULL_PUBLICATION_LAYOUT"
)

out_figures_dir <- file.path(
  figures_dir,
  "09C_primary_reduced_feature_weak_label_ML_V4_FULL_PUBLICATION_LAYOUT_pdf"
)

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

input_audit_csv <- file.path(out_tables_dir, "09C_input_audit.csv")
model_audit_csv <- file.path(out_tables_dir, "09C_model_training_audit.csv")
internal_pred_csv <- file.path(out_tables_dir, "09C_internal_CV_predictions.csv")
lodo_pred_csv <- file.path(out_tables_dir, "09C_leave_one_dataset_out_predictions.csv")
performance_csv <- file.path(out_tables_dir, "09C_model_performance_summary.csv")
feature_importance_csv <- file.path(out_tables_dir, "09C_feature_importance_summary.csv")
feature_importance_normalized_csv <- file.path(out_tables_dir, "09C_feature_importance_normalized_by_task_model.csv")
selected_features_csv <- file.path(out_tables_dir, "09C_selected_features_by_split.csv")
task_dataset_summary_csv <- file.path(out_tables_dir, "09C_task_dataset_summary.csv")
feature_preflight_audit_csv <- file.path(out_tables_dir, "09C_feature_preflight_audit.csv")
method_note_txt <- file.path(out_tables_dir, "09C_method_and_claim_boundary_note.txt")
output_check_csv <- file.path(out_tables_dir, "09C_output_verification.csv")
session_info_txt <- file.path(out_tables_dir, "09C_sessionInfo.txt")
report_txt <- file.path(reports_dir, "09C_primary_reduced_feature_weak_label_ML_V4_FULL_PUBLICATION_LAYOUT_report.txt")

fig_internal_auc_pdf <- file.path(out_figures_dir, "09C_internal_CV_AUC_summary_PUBLICATION_LAYOUT.pdf")
fig_lodo_auc_pdf <- file.path(out_figures_dir, "09C_LODO_AUC_summary_PUBLICATION_LAYOUT.pdf")
fig_prob_dist_pdf <- file.path(out_figures_dir, "09C_predicted_probability_distribution_PUBLICATION_LAYOUT.pdf")
fig_importance_pdf <- file.path(out_figures_dir, "09C_top_feature_importance_NORMALIZED_BY_TASK_MODEL.pdf")


# ============================================================
# 3. 工具函数
# ============================================================

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

pr_auc_base <- function(labels, scores) {
  labels <- as.integer(labels)
  scores <- as.numeric(scores)

  ok <- !is.na(labels) & !is.na(scores) & is.finite(scores)
  labels <- labels[ok]
  scores <- scores[ok]

  if (length(labels) == 0 || sum(labels == 1) == 0) return(NA_real_)

  ord <- order(scores, decreasing = TRUE)
  labels <- labels[ord]

  tp <- cumsum(labels == 1)
  fp <- cumsum(labels == 0)

  precision <- tp / pmax(tp + fp, 1)
  recall <- tp / sum(labels == 1)

  recall2 <- c(0, recall)
  precision2 <- c(precision[1], precision)

  auc <- sum((recall2[-1] - recall2[-length(recall2)]) * precision2[-1])
  as.numeric(auc)
}

binary_metrics <- function(labels, probs, threshold = 0.5) {
  labels <- as.integer(labels)
  probs <- as.numeric(probs)

  ok <- !is.na(labels) & !is.na(probs) & is.finite(probs)
  labels <- labels[ok]
  probs <- probs[ok]

  if (length(labels) == 0) {
    return(data.table(
      n = 0,
      positives = 0,
      negatives = 0,
      accuracy = NA_real_,
      balanced_accuracy = NA_real_,
      sensitivity = NA_real_,
      specificity = NA_real_,
      precision = NA_real_,
      f1 = NA_real_,
      auc = NA_real_,
      pr_auc = NA_real_
    ))
  }

  pred <- ifelse(probs >= threshold, 1L, 0L)

  tp <- sum(pred == 1 & labels == 1)
  tn <- sum(pred == 0 & labels == 0)
  fp <- sum(pred == 1 & labels == 0)
  fn <- sum(pred == 0 & labels == 1)

  acc <- (tp + tn) / length(labels)
  sens <- ifelse((tp + fn) == 0, NA_real_, tp / (tp + fn))
  spec <- ifelse((tn + fp) == 0, NA_real_, tn / (tn + fp))
  prec <- ifelse((tp + fp) == 0, NA_real_, tp / (tp + fp))
  f1 <- ifelse(is.na(prec) | is.na(sens) | (prec + sens) == 0, NA_real_, 2 * prec * sens / (prec + sens))
  bal <- ifelse(is.na(sens) | is.na(spec), NA_real_, (sens + spec) / 2)

  data.table(
    n = length(labels),
    positives = sum(labels == 1),
    negatives = sum(labels == 0),
    accuracy = acc,
    balanced_accuracy = bal,
    sensitivity = sens,
    specificity = spec,
    precision = prec,
    f1 = f1,
    auc = auc_base(labels, probs),
    pr_auc = pr_auc_base(labels, probs)
  )
}

make_stratified_folds <- function(labels, k = 5, seed = 1) {
  set.seed(seed)
  labels <- as.integer(labels)
  folds <- rep(NA_integer_, length(labels))

  for (cl in sort(unique(labels))) {
    idx <- which(labels == cl)
    idx <- sample(idx)
    fold_ids <- rep(seq_len(k), length.out = length(idx))
    folds[idx] <- fold_ids
  }

  folds
}

get_feature_cols <- function(dt) {
  exclude <- c(
    "task",
    "weak_label",
    "dataset",
    "object_id",
    "group_id",
    "group_key",
    "safety_contrast_class_05B",
    "n_cells",
    "sample_weight_equal",
    "sample_weight_sqrt_cells",
    # V2: technical columns generated by the script or inherited from splitting
    "row_id",
    "fold",
    "predicted_probability"
  )

  numeric_cols <- names(dt)[vapply(dt, function(z) is.numeric(z) || is.integer(z), logical(1))]
  feature_cols <- setdiff(numeric_cols, exclude)

  # V2 strict preflight: no technical / label / ID-like leakage variables in training features.
  dangerous_pattern <- "(^row_id$|^fold$|weak_label|label|class|dataset|object_id|group_id|group_key|sample_weight|predicted_probability)"
  dangerous <- feature_cols[grepl(dangerous_pattern, feature_cols, ignore.case = TRUE)]

  if (length(dangerous) > 0) {
    stop(
      "09C V2 feature preflight failed: dangerous technical/label columns detected in feature_cols: ",
      paste(dangerous, collapse = ", ")
    )
  }

  feature_cols
}

rank_features_train_only <- function(train_dt, feature_cols, max_features = 10) {
  labels <- as.integer(train_dt$weak_label)

  scores <- sapply(feature_cols, function(fc) {
    x <- num(train_dt[[fc]])
    ok <- !is.na(x) & !is.na(labels)
    x <- x[ok]
    y <- labels[ok]

    if (length(x) < 3 || length(unique(y)) < 2 || length(unique(x)) < 2) return(0)

    x[is.na(x)] <- median(x, na.rm = TRUE)

    m1 <- mean(x[y == 1], na.rm = TRUE)
    m0 <- mean(x[y == 0], na.rm = TRUE)
    sd_all <- sd(x, na.rm = TRUE)

    if (!is.finite(sd_all) || sd_all == 0) return(0)

    abs(m1 - m0) / sd_all
  })

  scores[is.na(scores)] <- 0
  ranked <- names(sort(scores, decreasing = TRUE))
  ranked <- ranked[scores[ranked] > 0]

  if (length(ranked) == 0) ranked <- feature_cols

  head(ranked, min(max_features, length(ranked)))
}

prep_fit <- function(train_dt, feature_cols) {
  med <- sapply(feature_cols, function(fc) {
    x <- num(train_dt[[fc]])
    if (all(is.na(x))) return(0)
    median(x, na.rm = TRUE)
  })

  sdv <- sapply(feature_cols, function(fc) {
    x <- num(train_dt[[fc]])
    x[is.na(x)] <- med[[fc]]
    s <- sd(x, na.rm = TRUE)
    if (!is.finite(s) || s == 0) s <- 1
    s
  })

  list(median = med, sd = sdv)
}

prep_apply <- function(dt, feature_cols, prep, scale = TRUE) {
  mat <- matrix(NA_real_, nrow = nrow(dt), ncol = length(feature_cols))
  colnames(mat) <- make.names(feature_cols, unique = TRUE)

  for (j in seq_along(feature_cols)) {
    fc <- feature_cols[[j]]
    x <- num(dt[[fc]])
    x[is.na(x)] <- prep$median[[fc]]

    if (scale) {
      x <- (x - prep$median[[fc]]) / prep$sd[[fc]]
    }

    mat[, j] <- x
  }

  as.data.frame(mat, check.names = FALSE)
}

fit_predict_glm <- function(train_dt, test_dt, feature_cols) {
  if (nrow(train_dt) < 5 || length(unique(train_dt$weak_label)) < 2) {
    return(list(
      success = FALSE,
      message = "training data too small or one class only",
      probs = rep(NA_real_, nrow(test_dt)),
      importance = data.table()
    ))
  }

  prep <- prep_fit(train_dt, feature_cols)
  x_train <- prep_apply(train_dt, feature_cols, prep, scale = TRUE)
  x_test <- prep_apply(test_dt, feature_cols, prep, scale = TRUE)

  train_df <- data.frame(
    weak_label = as.integer(train_dt$weak_label),
    x_train,
    check.names = FALSE
  )

  formula_txt <- paste("weak_label ~", paste(colnames(x_train), collapse = " + "))
  fit <- tryCatch({
    suppressWarnings(stats::glm(
      as.formula(formula_txt),
      data = train_df,
      family = stats::binomial()
    ))
  }, error = function(e) {
    e
  })

  if (inherits(fit, "error")) {
    return(list(
      success = FALSE,
      message = conditionMessage(fit),
      probs = rep(NA_real_, nrow(test_dt)),
      importance = data.table()
    ))
  }

  probs <- tryCatch({
    as.numeric(stats::predict(fit, newdata = x_test, type = "response"))
  }, error = function(e) {
    rep(NA_real_, nrow(test_dt))
  })

  coefs <- stats::coef(fit)
  coefs <- coefs[names(coefs) != "(Intercept)"]
  imp <- data.table(
    feature_sanitized = names(coefs),
    importance = abs(as.numeric(coefs)),
    signed_coefficient = as.numeric(coefs)
  )

  feature_map <- data.table(
    feature_original = feature_cols,
    feature_sanitized = colnames(x_train)
  )

  imp <- merge(imp, feature_map, by = "feature_sanitized", all.x = TRUE)

  list(
    success = TRUE,
    message = "ok",
    probs = probs,
    importance = imp
  )
}

fit_predict_rf <- function(train_dt, test_dt, feature_cols) {
  if (!HAS_RANDOMFOREST) {
    return(list(
      success = FALSE,
      message = "randomForest package not installed",
      probs = rep(NA_real_, nrow(test_dt)),
      importance = data.table()
    ))
  }

  if (nrow(train_dt) < 10 || length(unique(train_dt$weak_label)) < 2) {
    return(list(
      success = FALSE,
      message = "training data too small or one class only",
      probs = rep(NA_real_, nrow(test_dt)),
      importance = data.table()
    ))
  }

  prep <- prep_fit(train_dt, feature_cols)
  x_train <- prep_apply(train_dt, feature_cols, prep, scale = FALSE)
  x_test <- prep_apply(test_dt, feature_cols, prep, scale = FALSE)

  y_train <- factor(as.integer(train_dt$weak_label), levels = c(0, 1))

  mtry_value <- if (RF_MTRY_MODE == "sqrt") {
    max(1, floor(sqrt(ncol(x_train))))
  } else {
    ncol(x_train)
  }

  fit <- tryCatch({
    randomForest::randomForest(
      x = x_train,
      y = y_train,
      ntree = RF_NTREES,
      mtry = mtry_value,
      importance = TRUE
    )
  }, error = function(e) {
    e
  })

  if (inherits(fit, "error")) {
    return(list(
      success = FALSE,
      message = conditionMessage(fit),
      probs = rep(NA_real_, nrow(test_dt)),
      importance = data.table()
    ))
  }

  probs <- tryCatch({
    pr <- stats::predict(fit, newdata = x_test, type = "prob")
    if ("1" %in% colnames(pr)) as.numeric(pr[, "1"]) else as.numeric(pr[, ncol(pr)])
  }, error = function(e) {
    rep(NA_real_, nrow(test_dt))
  })

  imp_raw <- tryCatch({
    randomForest::importance(fit)
  }, error = function(e) {
    NULL
  })

  if (!is.null(imp_raw)) {
    imp_dt <- as.data.table(as.data.frame(imp_raw), keep.rownames = "feature_sanitized")

    score_col <- intersect(c("MeanDecreaseGini", "MeanDecreaseAccuracy"), names(imp_dt))
    if (length(score_col) == 0) {
      numeric_imp_cols <- names(imp_dt)[vapply(imp_dt, is.numeric, logical(1))]
      score_col <- numeric_imp_cols[1]
    }

    if (length(score_col) > 0 && !is.na(score_col[1])) {
      imp <- data.table(
        feature_sanitized = imp_dt$feature_sanitized,
        importance = num(imp_dt[[score_col[1]]]),
        importance_metric = score_col[1]
      )
    } else {
      imp <- data.table()
    }

    feature_map <- data.table(
      feature_original = feature_cols,
      feature_sanitized = colnames(x_train)
    )

    if (nrow(imp) > 0) {
      imp <- merge(imp, feature_map, by = "feature_sanitized", all.x = TRUE)
    }
  } else {
    imp <- data.table()
  }

  list(
    success = TRUE,
    message = "ok",
    probs = probs,
    importance = imp
  )
}

train_predict_one_split <- function(train_dt, test_dt, task_name, evaluation, split_id, feature_cols) {
  out_pred <- list()
  out_audit <- list()
  out_imp <- list()
  out_selected <- list()

  # Logistic feature selection is train-only
  glm_features <- rank_features_train_only(
    train_dt,
    feature_cols,
    max_features = min(MAX_GLM_FEATURES, length(feature_cols))
  )

  out_selected[[length(out_selected) + 1L]] <- data.table(
    task_name = task_name,
    evaluation = evaluation,
    split_id = as.character(split_id),
    model = "logistic_glm",
    selected_feature = glm_features,
    selected_feature_rank = seq_along(glm_features)
  )

  glm_res <- fit_predict_glm(train_dt, test_dt, glm_features)

  out_audit[[length(out_audit) + 1L]] <- data.table(
    task_name = task_name,
    evaluation = evaluation,
    split_id = as.character(split_id),
    model = "logistic_glm",
    n_train = nrow(train_dt),
    n_test = nrow(test_dt),
    train_pos = sum(train_dt$weak_label == 1, na.rm = TRUE),
    train_neg = sum(train_dt$weak_label == 0, na.rm = TRUE),
    test_pos = sum(test_dt$weak_label == 1, na.rm = TRUE),
    test_neg = sum(test_dt$weak_label == 0, na.rm = TRUE),
    n_features_used = length(glm_features),
    success = glm_res$success,
    message = glm_res$message
  )

  out_pred[[length(out_pred) + 1L]] <- data.table(
    task_name = task_name,
    evaluation = evaluation,
    split_id = as.character(split_id),
    model = "logistic_glm",
    row_id = test_dt$row_id,
    dataset = test_dt$dataset,
    object_id = test_dt$object_id,
    group_id = test_dt$group_id,
    weak_label = as.integer(test_dt$weak_label),
    predicted_probability = glm_res$probs
  )

  if (nrow(glm_res$importance) > 0) {
    imp <- copy(glm_res$importance)
    imp[, task_name := task_name]
    imp[, evaluation := evaluation]
    imp[, split_id := as.character(split_id)]
    imp[, model := "logistic_glm"]
    imp[, importance_metric := "abs_logistic_coefficient"]
    out_imp[[length(out_imp) + 1L]] <- imp
  }

  # Random forest uses all reduced features
  rf_features <- feature_cols

  out_selected[[length(out_selected) + 1L]] <- data.table(
    task_name = task_name,
    evaluation = evaluation,
    split_id = as.character(split_id),
    model = "random_forest",
    selected_feature = rf_features,
    selected_feature_rank = seq_along(rf_features)
  )

  rf_res <- fit_predict_rf(train_dt, test_dt, rf_features)

  out_audit[[length(out_audit) + 1L]] <- data.table(
    task_name = task_name,
    evaluation = evaluation,
    split_id = as.character(split_id),
    model = "random_forest",
    n_train = nrow(train_dt),
    n_test = nrow(test_dt),
    train_pos = sum(train_dt$weak_label == 1, na.rm = TRUE),
    train_neg = sum(train_dt$weak_label == 0, na.rm = TRUE),
    test_pos = sum(test_dt$weak_label == 1, na.rm = TRUE),
    test_neg = sum(test_dt$weak_label == 0, na.rm = TRUE),
    n_features_used = length(rf_features),
    success = rf_res$success,
    message = rf_res$message
  )

  out_pred[[length(out_pred) + 1L]] <- data.table(
    task_name = task_name,
    evaluation = evaluation,
    split_id = as.character(split_id),
    model = "random_forest",
    row_id = test_dt$row_id,
    dataset = test_dt$dataset,
    object_id = test_dt$object_id,
    group_id = test_dt$group_id,
    weak_label = as.integer(test_dt$weak_label),
    predicted_probability = rf_res$probs
  )

  if (nrow(rf_res$importance) > 0) {
    imp <- copy(rf_res$importance)
    imp[, task_name := task_name]
    imp[, evaluation := evaluation]
    imp[, split_id := as.character(split_id)]
    imp[, model := "random_forest"]
    out_imp[[length(out_imp) + 1L]] <- imp
  }

  list(
    predictions = rbindlist(out_pred, fill = TRUE),
    audit = rbindlist(out_audit, fill = TRUE),
    importance = rbindlist(out_imp, fill = TRUE),
    selected = rbindlist(out_selected, fill = TRUE)
  )
}

plot_empty_pdf <- function(path, title, message) {
  safe_pdf(path)
  plot.new()
  title(main = title)
  text(0.5, 0.5, message, cex = 1.0)
  finish_pdf(path)
}


short_task_09C <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "ideal_like_classifier", "Ideal-like", x)
  x <- ifelse(x == "safety_risk_classifier", "Safety-risk", x)
  x
}

short_model_09C <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "logistic_glm", "Logistic", x)
  x <- ifelse(x == "random_forest", "Random forest", x)
  x
}

clean_feature_label_09C <- function(x) {
  x <- as.character(x)
  x <- gsub("^marker_", "", x)
  x <- gsub("_score_05A$", " score", x)
  x <- gsub("_score_05B$", " score", x)
  x <- gsub("_composite_score$", " composite", x)
  x <- gsub("pct_cells_", "pct cells ", x)
  x <- gsub("_gt0$", " > 0", x)
  x <- gsub("_", " ", x)
  x
}

wrap_text_09C <- function(x, width = 38) {
  vapply(as.character(x), function(s) {
    paste(strwrap(s, width = width), collapse = "\n")
  }, character(1))
}



# ============================================================
# 4. 读取输入
# ============================================================

stamp("读取 09B reduced non-direct ML inputs。")

ideal_dt <- read_required(input_ideal_reduced)
safety_dt <- read_required(input_safety_reduced)
feature_dict <- read_optional(input_09B_feature_dict)
leakage09B <- read_optional(input_09B_leakage_audit)
lodo09B <- read_optional(input_09B_lodo_plan)

input_audit <- data.table(
  input_name = c(
    "ideal_reduced",
    "safety_reduced",
    "09B_feature_dictionary",
    "09B_leakage_audit",
    "09B_lodo_plan"
  ),
  path = c(
    input_ideal_reduced,
    input_safety_reduced,
    input_09B_feature_dict,
    input_09B_leakage_audit,
    input_09B_lodo_plan
  ),
  exists = file.exists(c(
    input_ideal_reduced,
    input_safety_reduced,
    input_09B_feature_dict,
    input_09B_leakage_audit,
    input_09B_lodo_plan
  )),
  rows = c(
    nrow(ideal_dt),
    nrow(safety_dt),
    nrow(feature_dict),
    nrow(leakage09B),
    nrow(lodo09B)
  )
)

atomic_write_csv(as.data.frame(input_audit), input_audit_csv)

task_list <- list(
  ideal_like_classifier = ideal_dt,
  safety_risk_classifier = safety_dt
)

for (task_name in names(task_list)) {
  dt <- task_list[[task_name]]
  if (!all(c("weak_label", "dataset", "object_id", "group_id") %in% names(dt))) {
    stop(task_name, " input 缺少 weak_label/dataset/object_id/group_id。")
  }
  dt[, weak_label := as.integer(weak_label)]
  dt[, dataset := as.character(dataset)]
  dt[, object_id := as.character(object_id)]
  dt[, group_id := as.character(group_id)]
  dt[, row_id := seq_len(.N)]
  task_list[[task_name]] <- dt
}

task_dataset_summary <- rbindlist(lapply(names(task_list), function(task_name) {
  dt <- task_list[[task_name]]
  dt[, .(
    n_groups = .N,
    positives = sum(weak_label == 1, na.rm = TRUE),
    negatives = sum(weak_label == 0, na.rm = TRUE),
    positive_fraction = mean(weak_label == 1, na.rm = TRUE)
  ), by = dataset][, task_name := task_name]
}), fill = TRUE)

setcolorder(task_dataset_summary, c("task_name", "dataset"))
atomic_write_csv(as.data.frame(task_dataset_summary), task_dataset_summary_csv)

# V2: feature preflight audit before model training.
feature_preflight_audit <- rbindlist(lapply(names(task_list), function(task_name) {
  dt <- task_list[[task_name]]
  feature_cols <- get_feature_cols(dt)
  data.table(
    task_name = task_name,
    feature = feature_cols,
    is_row_id = feature_cols == "row_id",
    is_technical_or_label_like = grepl(
      "(^row_id$|^fold$|weak_label|label|class|dataset|object_id|group_id|group_key|sample_weight|predicted_probability)",
      feature_cols,
      ignore.case = TRUE
    )
  )
}), fill = TRUE)

if (any(feature_preflight_audit$is_row_id == TRUE) || any(feature_preflight_audit$is_technical_or_label_like == TRUE)) {
  print(feature_preflight_audit[is_row_id == TRUE | is_technical_or_label_like == TRUE])
  stop("09C V2 feature preflight failed: technical/label-like features detected.")
}

atomic_write_csv(as.data.frame(feature_preflight_audit), feature_preflight_audit_csv)

stamp("Ideal input rows：", nrow(ideal_dt))
stamp("Safety input rows：", nrow(safety_dt))
stamp("randomForest available：", HAS_RANDOMFOREST)


# ============================================================
# 5. Internal stratified CV
# ============================================================

stamp("运行 internal stratified K-fold CV。")

internal_preds <- list()
internal_audits <- list()
internal_imps <- list()
internal_selected <- list()

for (task_name in names(task_list)) {
  dt <- copy(task_list[[task_name]])
  feature_cols <- get_feature_cols(dt)

  if (length(feature_cols) == 0) {
    stop(task_name, " 没有 reduced non-direct numeric features。")
  }

  if (length(unique(dt$weak_label)) < 2) {
    stop(task_name, " weak_label 只有一个类别，不能训练模型。")
  }

  k_use <- min(K_FOLDS, min(table(dt$weak_label)))
  if (k_use < 2) {
    stop(task_name, " 每个类别样本太少，不能做 K-fold CV。")
  }

  dt[, fold := make_stratified_folds(weak_label, k = k_use, seed = SEED)]

  for (fold_id in seq_len(k_use)) {
    train_dt <- dt[fold != fold_id]
    test_dt <- dt[fold == fold_id]

    res <- train_predict_one_split(
      train_dt = train_dt,
      test_dt = test_dt,
      task_name = task_name,
      evaluation = "internal_stratified_CV",
      split_id = paste0("fold_", fold_id),
      feature_cols = feature_cols
    )

    internal_preds[[length(internal_preds) + 1L]] <- res$predictions
    internal_audits[[length(internal_audits) + 1L]] <- res$audit
    internal_imps[[length(internal_imps) + 1L]] <- res$importance
    internal_selected[[length(internal_selected) + 1L]] <- res$selected
  }
}

internal_pred_dt <- rbindlist(internal_preds, fill = TRUE)
internal_audit_dt <- rbindlist(internal_audits, fill = TRUE)
internal_importance_dt <- rbindlist(internal_imps, fill = TRUE)
internal_selected_dt <- rbindlist(internal_selected, fill = TRUE)

atomic_write_csv(as.data.frame(internal_pred_dt), internal_pred_csv)


# ============================================================
# 6. Leave-one-dataset-out validation
# ============================================================

stamp("运行 leave-one-dataset-out validation。")

lodo_preds <- list()
lodo_audits <- list()
lodo_imps <- list()
lodo_selected <- list()

for (task_name in names(task_list)) {
  dt <- copy(task_list[[task_name]])
  feature_cols <- get_feature_cols(dt)
  datasets <- sort(unique(dt$dataset))

  for (ds in datasets) {
    train_dt <- dt[dataset != ds]
    test_dt <- dt[dataset == ds]

    if (nrow(test_dt) == 0 || nrow(train_dt) == 0) next

    if (length(unique(train_dt$weak_label)) < 2) {
      # 不能训练，但仍记录 audit
      lodo_audits[[length(lodo_audits) + 1L]] <- data.table(
        task_name = task_name,
        evaluation = "leave_one_dataset_out",
        split_id = ds,
        model = c("logistic_glm", "random_forest"),
        n_train = nrow(train_dt),
        n_test = nrow(test_dt),
        train_pos = sum(train_dt$weak_label == 1, na.rm = TRUE),
        train_neg = sum(train_dt$weak_label == 0, na.rm = TRUE),
        test_pos = sum(test_dt$weak_label == 1, na.rm = TRUE),
        test_neg = sum(test_dt$weak_label == 0, na.rm = TRUE),
        n_features_used = length(feature_cols),
        success = FALSE,
        message = "training set has one class only"
      )
      next
    }

    res <- train_predict_one_split(
      train_dt = train_dt,
      test_dt = test_dt,
      task_name = task_name,
      evaluation = "leave_one_dataset_out",
      split_id = ds,
      feature_cols = feature_cols
    )

    lodo_preds[[length(lodo_preds) + 1L]] <- res$predictions
    lodo_audits[[length(lodo_audits) + 1L]] <- res$audit
    lodo_imps[[length(lodo_imps) + 1L]] <- res$importance
    lodo_selected[[length(lodo_selected) + 1L]] <- res$selected
  }
}

lodo_pred_dt <- rbindlist(lodo_preds, fill = TRUE)
lodo_audit_dt <- rbindlist(lodo_audits, fill = TRUE)
lodo_importance_dt <- rbindlist(lodo_imps, fill = TRUE)
lodo_selected_dt <- rbindlist(lodo_selected, fill = TRUE)

atomic_write_csv(as.data.frame(lodo_pred_dt), lodo_pred_csv)


# ============================================================
# 7. Performance summary
# ============================================================

stamp("汇总模型性能。")

all_pred <- rbindlist(list(internal_pred_dt, lodo_pred_dt), fill = TRUE)

performance <- all_pred[
  ,
  binary_metrics(weak_label, predicted_probability),
  by = .(task_name, evaluation, split_id, model)
]

# aggregate overall internal CV by pooling all held-out predictions
pooled_internal <- internal_pred_dt[
  ,
  binary_metrics(weak_label, predicted_probability),
  by = .(task_name, evaluation, model)
][, split_id := "pooled_all_folds"]

# aggregate LODO pooled predictions across all left-out datasets
pooled_lodo <- lodo_pred_dt[
  ,
  binary_metrics(weak_label, predicted_probability),
  by = .(task_name, evaluation, model)
][, split_id := "pooled_all_lodo_predictions"]

performance <- rbindlist(list(
  performance,
  pooled_internal,
  pooled_lodo
), fill = TRUE)

setcolorder(performance, c(
  "task_name", "evaluation", "split_id", "model",
  "n", "positives", "negatives",
  "accuracy", "balanced_accuracy", "sensitivity", "specificity",
  "precision", "f1", "auc", "pr_auc"
))

atomic_write_csv(as.data.frame(performance), performance_csv)


# ============================================================
# 8. Feature importance summary
# ============================================================

stamp("汇总 feature importance。")

importance_all <- rbindlist(list(
  internal_importance_dt,
  lodo_importance_dt
), fill = TRUE)

if (nrow(importance_all) > 0) {
  if (!"feature_original" %in% names(importance_all)) {
    importance_all[, feature_original := feature_sanitized]
  }

  importance_summary <- importance_all[
    !is.na(feature_original) & !is.na(importance) & is.finite(importance),
    .(
      mean_importance = mean(abs(num(importance)), na.rm = TRUE),
      median_importance = median(abs(num(importance)), na.rm = TRUE),
      max_importance = max(abs(num(importance)), na.rm = TRUE),
      n_splits_with_importance = .N
    ),
    by = .(task_name, model, feature_original, importance_metric)
  ]

  setorder(importance_summary, task_name, model, -mean_importance)
} else {
  importance_summary <- data.table(
    task_name = character(),
    model = character(),
    feature_original = character(),
    importance_metric = character(),
    mean_importance = numeric(),
    median_importance = numeric(),
    max_importance = numeric(),
    n_splits_with_importance = integer()
  )
}

selected_all <- rbindlist(list(
  internal_selected_dt,
  lodo_selected_dt
), fill = TRUE)

atomic_write_csv(as.data.frame(importance_summary), feature_importance_csv)
atomic_write_csv(as.data.frame(selected_all), selected_features_csv)


# ============================================================
# 9. Model audit
# ============================================================

model_audit <- rbindlist(list(
  internal_audit_dt,
  lodo_audit_dt
), fill = TRUE)

model_audit[, randomForest_available := HAS_RANDOMFOREST]

atomic_write_csv(as.data.frame(model_audit), model_audit_csv)


# ============================================================
# 10. PDF-only figures - V4 publication layout
# ============================================================

stamp("生成 09C PDF-only publication-layout figures。")

plot_auc_summary <- function(perf, evaluation_pattern, pdf_path, title_main) {
  dt <- copy(perf[grepl(evaluation_pattern, evaluation) & grepl("^pooled", split_id)])
  dt <- dt[!is.na(auc) & is.finite(num(auc))]

  if (nrow(dt) == 0) {
    plot_empty_pdf(pdf_path, title_main, "No AUC-evaluable pooled results.")
    return(invisible(NULL))
  }

  dt[, auc := num(auc)]
  dt[, task_label := short_task_09C(task_name)]
  dt[, model_label := short_model_09C(model)]
  dt[, label := paste(task_label, model_label, sep = " | ")]
  dt <- dt[order(auc)]

  safe_pdf(pdf_path, width = 10.8, height = 6.6)

  par(mar = c(5.2, 12.5, 4.2, 2.0), xpd = FALSE)

  bp <- barplot(
    dt$auc,
    names.arg = dt$label,
    horiz = TRUE,
    las = 1,
    xlim = c(0, 1.05),
    xlab = "Pooled AUC",
    main = title_main,
    col = "grey62",
    border = "grey25",
    cex.names = 0.82,
    cex.axis = 0.90,
    cex.lab = 1.05
  )

  abline(v = 0.5, lty = 2, col = "grey45")
  abline(v = 0.8, lty = 3, col = "grey45")

  text(
    x = pmin(dt$auc + 0.025, 1.03),
    y = bp,
    labels = sprintf("%.2f", dt$auc),
    cex = 0.82,
    adj = 0
  )

  legend(
    "bottomright",
    legend = c("AUC 0.5", "AUC 0.8"),
    lty = c(2, 3),
    col = c("grey45", "grey45"),
    bty = "n",
    cex = 0.82
  )

  finish_pdf(pdf_path)
}

plot_auc_summary(
  performance,
  "internal_stratified_CV",
  fig_internal_auc_pdf,
  "09C internal cross-validation pooled AUC"
)

plot_auc_summary(
  performance,
  "leave_one_dataset_out",
  fig_lodo_auc_pdf,
  "09C leave-one-dataset-out pooled AUC"
)

# predicted probability distribution
plot_probability_panel_09C <- function(d, task_i, model_i) {
  neg <- d[weak_label == 0 & !is.na(predicted_probability) & is.finite(predicted_probability)]
  pos <- d[weak_label == 1 & !is.na(predicted_probability) & is.finite(predicted_probability)]

  breaks_use <- seq(0, 1, by = 0.05)

  if (nrow(neg) == 0 && nrow(pos) == 0) {
    plot.new()
    title(main = paste(short_task_09C(task_i), short_model_09C(model_i), sep = "\n"))
    text(0.5, 0.5, "No finite predictions", cex = 0.9)
    return(invisible(NULL))
  }

  h_neg <- hist(neg$predicted_probability, breaks = breaks_use, plot = FALSE)
  h_pos <- hist(pos$predicted_probability, breaks = breaks_use, plot = FALSE)

  ymax <- max(c(h_neg$counts, h_pos$counts, 1), na.rm = TRUE)
  ylim_use <- c(0, ymax * 1.42)

  hist(
    neg$predicted_probability,
    breaks = breaks_use,
    col = rgb(0.2, 0.4, 0.7, 0.42),
    border = NA,
    xlim = c(0, 1),
    ylim = ylim_use,
    main = paste(short_task_09C(task_i), short_model_09C(model_i), sep = "\n"),
    xlab = "Predicted probability",
    ylab = "Count"
  )

  hist(
    pos$predicted_probability,
    breaks = breaks_use,
    col = rgb(0.8, 0.1, 0.1, 0.42),
    border = NA,
    add = TRUE
  )

  # V4: legend moved upward into reserved top space.
  # This avoids Negative/Positive labels sticking to histogram bars,
  # especially in the bottom two panels.
  legend(
    x = 0.50,
    y = ymax * 1.34,
    legend = c("Negative", "Positive"),
    fill = c(rgb(0.2, 0.4, 0.7, 0.42), rgb(0.8, 0.1, 0.1, 0.42)),
    horiz = TRUE,
    bty = "n",
    cex = 0.76,
    xjust = 0.5,
    yjust = 1
  )

  box()
}

if (nrow(all_pred) > 0) {
  plot_dt <- all_pred[!is.na(predicted_probability) & is.finite(predicted_probability)]

  if (nrow(plot_dt) > 0) {
    safe_pdf(fig_prob_dist_pdf, width = 12.0, height = 8.4)

    par(mfrow = c(2, 2), mar = c(4.8, 4.7, 5.4, 1.4), oma = c(0, 0, 0, 0))

    split_keys <- unique(paste(plot_dt$task_name, plot_dt$model, sep = "||"))
    split_keys <- split_keys[
      order(
        match(sub("\\|\\|.*$", "", split_keys), c("ideal_like_classifier", "safety_risk_classifier")),
        match(sub("^.*\\|\\|", "", split_keys), c("logistic_glm", "random_forest"))
      )
    ]
    split_keys <- head(split_keys, 4)

    for (sk in split_keys) {
      task_i <- strsplit(sk, "\\|\\|")[[1]][1]
      model_i <- strsplit(sk, "\\|\\|")[[1]][2]

      d <- plot_dt[
        task_name == task_i &
          model == model_i &
          evaluation == "internal_stratified_CV"
      ]

      if (nrow(d) == 0) {
        plot.new()
        title(main = paste(short_task_09C(task_i), short_model_09C(model_i), sep = "\n"))
        text(0.5, 0.5, "No internal CV predictions", cex = 0.9)
      } else {
        plot_probability_panel_09C(d, task_i, model_i)
      }
    }

    finish_pdf(fig_prob_dist_pdf)
  } else {
    plot_empty_pdf(fig_prob_dist_pdf, "09C predicted probability distribution", "No finite predictions.")
  }
} else {
  plot_empty_pdf(fig_prob_dist_pdf, "09C predicted probability distribution", "No predictions.")
}

# feature importance
# V4: raw logistic coefficients and random forest importance are not on a shared scale.
# Therefore the publication figure normalizes importance within each task/model.
if (nrow(importance_summary) > 0) {
  imp_plot <- copy(importance_summary)

  if (!"feature_original" %in% names(imp_plot)) {
    imp_plot[, feature_original := feature_sanitized]
  }

  imp_plot <- imp_plot[
    !is.na(feature_original) &
      !grepl("^row_id$", feature_original, ignore.case = TRUE) &
      !is.na(mean_importance) &
      is.finite(num(mean_importance)) &
      num(mean_importance) >= 0
  ]

  if (nrow(imp_plot) > 0) {
    imp_plot[, mean_importance := num(mean_importance)]
    imp_plot[, task_label := short_task_09C(task_name)]
    imp_plot[, model_label := short_model_09C(model)]
    imp_plot[, feature_label := clean_feature_label_09C(feature_original)]

    imp_plot[, max_importance_group := max(mean_importance, na.rm = TRUE), by = .(task_name, model)]
    imp_plot[, normalized_importance := ifelse(max_importance_group > 0, mean_importance / max_importance_group, NA_real_)]

    imp_plot <- imp_plot[
      !is.na(normalized_importance) &
        is.finite(normalized_importance)
    ]

    imp_plot <- imp_plot[
      ,
      .SD[order(-normalized_importance)][seq_len(min(TOP_IMPORTANCE_FEATURES, .N))],
      by = .(task_name, model)
    ]

    imp_plot[, feature_label_wrapped := wrap_text_09C(feature_label, width = 38)]

    atomic_write_csv(as.data.frame(imp_plot), feature_importance_normalized_csv)

    safe_pdf(fig_importance_pdf, width = 12.0, height = 8.6)

    groups <- unique(imp_plot[, .(task_name, model, task_label, model_label)])
    setorder(groups, task_name, model)

    par(mfrow = c(2, 2), mar = c(5.0, 12.8, 3.3, 1.4))

    for (ii in seq_len(nrow(groups))) {
      g <- groups[ii]
      d <- imp_plot[task_name == g$task_name & model == g$model]
      d <- d[order(normalized_importance)]

      barplot(
        d$normalized_importance,
        names.arg = d$feature_label_wrapped,
        horiz = TRUE,
        las = 1,
        xlim = c(0, 1.05),
        xlab = "Normalized importance within task/model",
        main = paste(g$task_label, g$model_label, sep = " | "),
        col = "grey62",
        border = "grey25",
        cex.names = 0.52,
        cex.axis = 0.72,
        cex.lab = 0.75,
        cex.main = 0.86
      )
    }

    finish_pdf(fig_importance_pdf)
  } else {
    atomic_write_csv(data.frame(message = "No plottable feature importance after filtering row_id."), feature_importance_normalized_csv)
    plot_empty_pdf(fig_importance_pdf, "09C top feature importance", "No feature importance available after filtering.")
  }
} else {
  atomic_write_csv(data.frame(message = "No feature importance available."), feature_importance_normalized_csv)
  plot_empty_pdf(fig_importance_pdf, "09C top feature importance", "No feature importance available.")
}


# ============================================================
# 11. Method note / report
# ============================================================

method_lines <- c(
  "09C primary reduced-feature marker-rule-derived prioritization model method and claim-boundary note",
  "",
  "Method-ready wording:",
  paste0(
    "Primary marker-rule-derived machine-learning models were trained using the reduced non-direct feature tables generated in 09B. ",
    "Two binary tasks were evaluated: ideal-like cell-state classification and safety-risk-associated cell-state classification. ",
    "Logistic regression was used as a transparent baseline model, with train-fold-only feature ranking applied within each cross-validation split. ",
    "Random forest models were trained when the randomForest R package was available. ",
    "Model performance was evaluated by internal stratified K-fold cross-validation and leave-one-dataset-out validation. Publication-layout figures used labeled AUC summaries, non-overlapping probability-distribution legends, and task/model-normalized feature-importance displays. ",
    "All labels were rule-derived marker-rule-derived labels from 05B and were not experimental ground truth."
  ),
  "",
  "Claim boundary:",
  "09C is an exploratory marker-rule-derived prioritization model analysis.",
  "Do not write this as a clinical-use model.",
  "Do not write this as validated therapeutic outcome prediction.",
  "Do not write this as validated clinical safety or tumorigenicity prediction.",
  "The main value is to test whether reduced non-direct transcriptomic features can recapitulate marker-rule-derived cell-state prioritization under internal and dataset-held-out validation."
)

writeLines(method_lines, method_note_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

report_lines <- c(
  "09C primary reduced-feature marker-rule-derived prioritization model FINAL V4 FULL PUBLICATION LAYOUT report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  paste0("randomForest available: ", HAS_RANDOMFOREST),
  paste0("K_FOLDS: ", K_FOLDS),
  paste0("MAX_GLM_FEATURES: ", MAX_GLM_FEATURES),
  "",
  "Input audit:",
  capture.output(print(input_audit)),
  "",
  "Task dataset summary:",
  capture.output(print(task_dataset_summary)),
  "",
  "Model audit:",
  capture.output(print(model_audit)),
  "",
  "Performance summary:",
  capture.output(print(performance)),
  "",
  "Output directories:",
  out_tables_dir,
  out_figures_dir
)

writeLines(report_lines, report_txt)


# ============================================================
# 12. 输出验证
# ============================================================

required_output_files <- c(
  input_audit_csv,
  model_audit_csv,
  internal_pred_csv,
  lodo_pred_csv,
  performance_csv,
  feature_importance_csv,
  feature_importance_normalized_csv,
  selected_features_csv,
  task_dataset_summary_csv,
  feature_preflight_audit_csv,
  method_note_txt,
  session_info_txt,
  report_txt,
  fig_internal_auc_pdf,
  fig_lodo_auc_pdf,
  fig_prob_dist_pdf,
  fig_importance_pdf
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
  stop("09C 输出验证失败。")
}


# ============================================================
# 13. 完成
# ============================================================

cat("\n============================================================\n")
cat("09C primary reduced-feature marker-rule-derived prioritization model FINAL V4 FULL PUBLICATION LAYOUT 运行结束\n")
cat("============================================================\n\n")

cat("randomForest available：", HAS_RANDOMFOREST, "\n")
cat("Ideal reduced rows：", nrow(ideal_dt), "\n")
cat("Safety reduced rows：", nrow(safety_dt), "\n")
cat("Internal CV predictions：", nrow(internal_pred_dt), "\n")
cat("LODO predictions：", nrow(lodo_pred_dt), "\n")
cat("Performance rows：", nrow(performance), "\n")
cat("Importance rows：", nrow(importance_summary), "\n\n")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("主要结果表：\n")
cat(performance_csv, "\n")
cat(model_audit_csv, "\n")
cat(feature_importance_csv, "\n")
cat(feature_importance_normalized_csv, "\n")
cat(feature_preflight_audit_csv, "\n")
cat(internal_pred_csv, "\n")
cat(lodo_pred_csv, "\n")
cat(method_note_txt, "\n\n")

cat("主要 PDF 图：\n")
cat(fig_internal_auc_pdf, "\n")
cat(fig_lodo_auc_pdf, "\n")
cat(fig_prob_dist_pdf, "\n")
cat(fig_importance_pdf, "\n\n")

cat("✅ 09C primary reduced-feature marker-rule-derived prioritization model FINAL V4 FULL PUBLICATION LAYOUT 完成。\n")
