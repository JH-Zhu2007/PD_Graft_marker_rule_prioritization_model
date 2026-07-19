# ============================================================
# 07B_TRAIN_EXPLORATORY_MARKER_RULE_DERIVED_PRIORITIZATION_MODELS.R
# ============================================================
# 目的：
#   接在 07A V3 后运行。
#
#   07B 训练两个 exploratory marker-rule-derived prioritization model models：
#     Model 1: ideal graft-like marker-rule-derived classifier
#     Model 2: safety-risk marker-rule-derived classifier
#
#   输入：
#     07A_ideal_graft_like_model_training_table.csv
#     07A_safety_risk_model_training_table.csv
#     07A_feature_dictionary.csv
#     07A_dataset_split_recommendation.csv
#
#   输出：
#     1. internal stratified CV predictions
#     2. leave-one-dataset-out predictions
#     3. model performance summary
#     4. feature importance summary
#     5. exploratory ML report
#
# 重要严谨性：
#   这些 label 是 05B rule-derived marker-rule-derived labels，不是实验 ground truth。
#   所以 07B 的模型只能叫 exploratory marker-rule-derived classifier。
#   不能写成 clinical-use model。
#   不能写成 validated therapeutic outcome model。
#
# 成功标志：
#   ✅ 07B exploratory marker-rule-derived prioritization model models 完成。
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"

SEED <- 20260714
K_FOLDS <- 5
MAX_FEATURES_GLM <- 20
MIN_CLASS_PER_DATASET_FOR_LODO <- 2

# 是否训练可选 rpart 模型；rpart 通常是 R 推荐包，但如果没有会自动跳过
TRAIN_RPART_IF_AVAILABLE <- TRUE


# ============================================================
# 1. 加载包
# ============================================================

cat("\n============================================================\n")
cat("07B：exploratory marker-rule-derived prioritization model models\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 07B。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

HAS_RPART <- requireNamespace("rpart", quietly = TRUE)

if (!HAS_RPART) {
  message("未检测到 rpart；07B 将只训练 logistic marker-rule-derived model。")
}


# ============================================================
# 2. 路径
# ============================================================

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")
figures_dir <- file.path(PROJECT_DIR, "04_figures")

input_ideal_train <- file.path(tables_dir, "07A_ML_dataset_preparation", "07A_ideal_graft_like_model_training_table.csv")
input_safety_train <- file.path(tables_dir, "07A_ML_dataset_preparation", "07A_safety_risk_model_training_table.csv")
input_feature_dict <- file.path(tables_dir, "07A_ML_dataset_preparation", "07A_feature_dictionary.csv")
input_class_balance <- file.path(tables_dir, "07A_ML_dataset_preparation", "07A_class_balance_summary.csv")
input_qc07A <- file.path(tables_dir, "07A_ML_dataset_preparation", "07A_ML_dataset_QC_audit.csv")

out_tables_dir <- file.path(tables_dir, "07B_weak_label_ML_models")
out_figures_dir <- file.path(figures_dir, "07B_weak_label_ML_models")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

internal_pred_csv <- file.path(out_tables_dir, "07B_internal_CV_predictions.csv")
lodo_pred_csv <- file.path(out_tables_dir, "07B_leave_one_dataset_out_predictions.csv")
performance_csv <- file.path(out_tables_dir, "07B_model_performance_summary.csv")
feature_importance_csv <- file.path(out_tables_dir, "07B_feature_importance_summary.csv")
selected_features_csv <- file.path(out_tables_dir, "07B_selected_features_by_model.csv")
qc_audit_csv <- file.path(out_tables_dir, "07B_ML_training_QC_audit.csv")
report_txt <- file.path(reports_dir, "07B_exploratory_weak_label_ML_models_report.txt")


# ============================================================
# 3. 工具函数
# ============================================================

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

sigmoid <- function(x) {
  1 / (1 + exp(-pmax(pmin(x, 30), -30)))
}

auc_base <- function(labels, scores) {
  labels <- as.integer(labels)
  scores <- as.numeric(scores)

  ok <- !is.na(labels) & !is.na(scores)
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

binary_metrics <- function(labels, probs, threshold = 0.5) {
  labels <- as.integer(labels)
  probs <- as.numeric(probs)

  ok <- !is.na(labels) & !is.na(probs)
  labels <- labels[ok]
  probs <- probs[ok]

  if (length(labels) == 0) {
    return(data.frame(
      n = 0, positives = 0, negatives = 0,
      accuracy = NA_real_, sensitivity = NA_real_, specificity = NA_real_,
      precision = NA_real_, f1 = NA_real_, auc = NA_real_
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
  auc <- auc_base(labels, probs)

  data.frame(
    n = length(labels),
    positives = sum(labels == 1),
    negatives = sum(labels == 0),
    accuracy = acc,
    sensitivity = sens,
    specificity = spec,
    precision = prec,
    f1 = f1,
    auc = auc
  )
}

get_feature_cols <- function(dt, label_col) {
  exclude <- c(
    "dataset", "object_id", "group_id", "group_key",
    "annotation_04D_v1", "safety_contrast_class_05B",
    "A9_A10_bias_label_05B", "story_priority_05B",
    "ML_label_source", "ML_claim_boundary",
    "has_required_DA_projection_scores", "has_required_safety_scores",
    "n_cells", label_col,
    "ideal_graft_like_weak_label", "safety_risk_weak_label"
  )

  numeric_cols <- names(dt)[vapply(dt, is.numeric, logical(1))]
  setdiff(numeric_cols, exclude)
}

median_impute_fit <- function(train_dt, feature_cols) {
  med <- sapply(train_dt[, feature_cols, with = FALSE], function(x) {
    x <- as.numeric(x)
    if (all(is.na(x))) return(0)
    median(x, na.rm = TRUE)
  })

  sds <- sapply(train_dt[, feature_cols, with = FALSE], function(x) {
    x <- as.numeric(x)
    x[is.na(x)] <- median(x, na.rm = TRUE)
    sd(x, na.rm = TRUE)
  })

  sds[is.na(sds) | sds == 0] <- 1

  list(median = med, sd = sds)
}

apply_impute_scale <- function(dt, feature_cols, prep) {
  mat <- as.matrix(dt[, feature_cols, with = FALSE])

  for (j in seq_along(feature_cols)) {
    col <- feature_cols[[j]]
    mat[, j] <- as.numeric(mat[, j])
    mat[is.na(mat[, j]), j] <- prep$median[[col]]
    mat[, j] <- (mat[, j] - prep$median[[col]]) / prep$sd[[col]]
  }

  colnames(mat) <- make.names(feature_cols)
  as.data.frame(mat, check.names = FALSE)
}

rank_features <- function(dt, label_col, feature_cols, max_features = 20) {
  labels <- as.integer(dt[[label_col]])

  scores <- sapply(feature_cols, function(fc) {
    x <- as.numeric(dt[[fc]])
    if (all(is.na(x))) return(0)
    x[is.na(x)] <- median(x, na.rm = TRUE)

    if (length(unique(labels[!is.na(labels)])) < 2) return(0)

    m1 <- mean(x[labels == 1], na.rm = TRUE)
    m0 <- mean(x[labels == 0], na.rm = TRUE)
    sd_all <- sd(x, na.rm = TRUE)
    if (is.na(sd_all) || sd_all == 0) return(0)

    abs(m1 - m0) / sd_all
  })

  scores[is.na(scores)] <- 0

  ranked <- names(sort(scores, decreasing = TRUE))
  ranked <- ranked[scores[ranked] > 0]

  if (length(ranked) == 0) {
    ranked <- feature_cols
  }

  head(ranked, min(max_features, length(ranked)))
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

train_predict_glm <- function(train_dt, test_dt, label_col, feature_cols) {
  # Feature selection inside training only
  selected <- rank_features(train_dt, label_col, feature_cols, max_features = MAX_FEATURES_GLM)

  prep <- median_impute_fit(train_dt, selected)
  x_train <- apply_impute_scale(train_dt, selected, prep)
  x_test <- apply_impute_scale(test_dt, selected, prep)

  train_model_dt <- data.frame(
    label = as.integer(train_dt[[label_col]]),
    x_train,
    check.names = FALSE
  )

  # if too few cases or one class, fallback
  if (length(unique(train_model_dt$label)) < 2) {
    return(list(
      prob = rep(mean(train_model_dt$label, na.rm = TRUE), nrow(test_dt)),
      selected_features = selected,
      coef_table = data.frame(feature = selected, coefficient = NA_real_),
      status = "single_class_train_fallback"
    ))
  }

  fit <- tryCatch({
    suppressWarnings(glm(label ~ ., data = train_model_dt, family = binomial()))
  }, error = function(e) {
    NULL
  })

  if (is.null(fit)) {
    p <- mean(train_model_dt$label, na.rm = TRUE)
    return(list(
      prob = rep(p, nrow(test_dt)),
      selected_features = selected,
      coef_table = data.frame(feature = selected, coefficient = NA_real_),
      status = "glm_failed_mean_fallback"
    ))
  }

  prob <- tryCatch({
    as.numeric(predict(fit, newdata = x_test, type = "response"))
  }, error = function(e) {
    rep(mean(train_model_dt$label, na.rm = TRUE), nrow(test_dt))
  })

  coef_vec <- suppressWarnings(coef(fit))
  coef_dt <- data.frame(
    feature = names(coef_vec),
    coefficient = as.numeric(coef_vec),
    stringsAsFactors = FALSE
  )
  coef_dt <- coef_dt[coef_dt$feature != "(Intercept)", , drop = FALSE]

  # Map make.names back approximately
  name_map <- data.frame(
    feature_sanitized = make.names(selected),
    feature_original = selected,
    stringsAsFactors = FALSE
  )
  coef_dt <- merge(
    coef_dt,
    name_map,
    by.x = "feature",
    by.y = "feature_sanitized",
    all.x = TRUE
  )
  coef_dt$feature <- ifelse(is.na(coef_dt$feature_original), coef_dt$feature, coef_dt$feature_original)
  coef_dt$feature_original <- NULL

  list(
    prob = prob,
    selected_features = selected,
    coef_table = coef_dt,
    status = "ok"
  )
}

train_predict_rpart <- function(train_dt, test_dt, label_col, feature_cols) {
  if (!HAS_RPART || !TRAIN_RPART_IF_AVAILABLE) {
    return(NULL)
  }

  selected <- rank_features(train_dt, label_col, feature_cols, max_features = MAX_FEATURES_GLM)

  prep <- median_impute_fit(train_dt, selected)
  x_train <- apply_impute_scale(train_dt, selected, prep)
  x_test <- apply_impute_scale(test_dt, selected, prep)

  train_model_dt <- data.frame(
    label = factor(as.integer(train_dt[[label_col]]), levels = c(0, 1)),
    x_train,
    check.names = FALSE
  )

  if (length(unique(train_model_dt$label)) < 2) return(NULL)

  fit <- tryCatch({
    rpart::rpart(
      label ~ .,
      data = train_model_dt,
      method = "class",
      control = rpart::rpart.control(cp = 0.01, minsplit = 10)
    )
  }, error = function(e) {
    NULL
  })

  if (is.null(fit)) return(NULL)

  prob <- tryCatch({
    pp <- predict(fit, newdata = x_test, type = "prob")
    as.numeric(pp[, "1"])
  }, error = function(e) {
    rep(mean(as.integer(train_dt[[label_col]]), na.rm = TRUE), nrow(test_dt))
  })

  vi <- tryCatch({
    imp <- fit$variable.importance
    data.frame(
      feature = names(imp),
      importance = as.numeric(imp),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(feature = selected, importance = NA_real_)
  })

  list(
    prob = prob,
    selected_features = selected,
    importance_table = vi,
    status = "ok"
  )
}

run_internal_cv <- function(dt, label_col, model_task) {
  dt <- as.data.table(copy(dt))
  feature_cols <- get_feature_cols(dt, label_col)

  dt <- dt[!is.na(get(label_col))]
  dt[[label_col]] <- as.integer(dt[[label_col]])

  if (length(unique(dt[[label_col]])) < 2) {
    stop("训练数据只有一个类别，无法训练：", model_task)
  }

  folds <- make_stratified_folds(dt[[label_col]], k = min(K_FOLDS, min(table(dt[[label_col]]))), seed = SEED)
  dt[, fold := folds]

  preds <- list()
  imps <- list()
  selected_records <- list()

  for (fold_id in sort(unique(folds))) {
    stamp("  ", model_task, " internal CV fold ", fold_id)

    train_dt <- dt[fold != fold_id]
    test_dt <- dt[fold == fold_id]

    glm_res <- train_predict_glm(train_dt, test_dt, label_col, feature_cols)

    pred_dt <- test_dt[
      ,
      .(
        dataset,
        object_id,
        group_id,
        group_key,
        true_label = get(label_col),
        n_cells
      )
    ]

    pred_dt[, model_task := model_task]
    pred_dt[, evaluation_type := "internal_stratified_CV"]
    pred_dt[, fold := fold_id]
    pred_dt[, algorithm := "logistic_glm"]
    pred_dt[, predicted_probability := glm_res$prob]
    pred_dt[, model_status := glm_res$status]

    preds[[length(preds) + 1L]] <- pred_dt

    imp_dt <- as.data.table(glm_res$coef_table)
    if (nrow(imp_dt) > 0) {
      imp_dt[, model_task := model_task]
      imp_dt[, evaluation_type := "internal_stratified_CV"]
      imp_dt[, fold := fold_id]
      imp_dt[, algorithm := "logistic_glm"]
      imp_dt[, importance := abs(coefficient)]
      imps[[length(imps) + 1L]] <- imp_dt
    }

    selected_records[[length(selected_records) + 1L]] <- data.table(
      model_task = model_task,
      evaluation_type = "internal_stratified_CV",
      fold = fold_id,
      algorithm = "logistic_glm",
      feature = glm_res$selected_features
    )

    # Optional rpart
    rp <- train_predict_rpart(train_dt, test_dt, label_col, feature_cols)

    if (!is.null(rp)) {
      pred_rp <- copy(pred_dt)
      pred_rp[, algorithm := "rpart_tree"]
      pred_rp[, predicted_probability := rp$prob]
      pred_rp[, model_status := rp$status]

      preds[[length(preds) + 1L]] <- pred_rp

      imp_rp <- as.data.table(rp$importance_table)
      if (nrow(imp_rp) > 0) {
        imp_rp[, model_task := model_task]
        imp_rp[, evaluation_type := "internal_stratified_CV"]
        imp_rp[, fold := fold_id]
        imp_rp[, algorithm := "rpart_tree"]
        if (!"coefficient" %in% colnames(imp_rp)) imp_rp[, coefficient := NA_real_]
        imps[[length(imps) + 1L]] <- imp_rp
      }

      selected_records[[length(selected_records) + 1L]] <- data.table(
        model_task = model_task,
        evaluation_type = "internal_stratified_CV",
        fold = fold_id,
        algorithm = "rpart_tree",
        feature = rp$selected_features
      )
    }
  }

  list(
    predictions = rbindlist(preds, fill = TRUE),
    importance = rbindlist(imps, fill = TRUE),
    selected = rbindlist(selected_records, fill = TRUE)
  )
}

run_lodo <- function(dt, label_col, model_task) {
  dt <- as.data.table(copy(dt))
  feature_cols <- get_feature_cols(dt, label_col)

  dt <- dt[!is.na(get(label_col))]
  dt[[label_col]] <- as.integer(dt[[label_col]])

  preds <- list()
  imps <- list()
  selected_records <- list()

  for (ds in sort(unique(dt$dataset))) {
    test_dt <- dt[dataset == ds]
    train_dt <- dt[dataset != ds]

    # Require both train and test contain both classes; otherwise skip and record no predictions.
    if (length(unique(train_dt[[label_col]])) < 2 || length(unique(test_dt[[label_col]])) < 2) {
      stamp("  ", model_task, " LODO skip ", ds, "：train/test class 不足。")
      next
    }

    if (min(table(train_dt[[label_col]])) < MIN_CLASS_PER_DATASET_FOR_LODO) {
      stamp("  ", model_task, " LODO warning ", ds, "：训练集某类别数量偏少。")
    }

    stamp("  ", model_task, " LODO test dataset：", ds)

    glm_res <- train_predict_glm(train_dt, test_dt, label_col, feature_cols)

    pred_dt <- test_dt[
      ,
      .(
        dataset,
        object_id,
        group_id,
        group_key,
        true_label = get(label_col),
        n_cells
      )
    ]

    pred_dt[, model_task := model_task]
    pred_dt[, evaluation_type := "leave_one_dataset_out"]
    pred_dt[, test_dataset := ds]
    pred_dt[, algorithm := "logistic_glm"]
    pred_dt[, predicted_probability := glm_res$prob]
    pred_dt[, model_status := glm_res$status]

    preds[[length(preds) + 1L]] <- pred_dt

    imp_dt <- as.data.table(glm_res$coef_table)
    if (nrow(imp_dt) > 0) {
      imp_dt[, model_task := model_task]
      imp_dt[, evaluation_type := "leave_one_dataset_out"]
      imp_dt[, test_dataset := ds]
      imp_dt[, algorithm := "logistic_glm"]
      imp_dt[, importance := abs(coefficient)]
      imps[[length(imps) + 1L]] <- imp_dt
    }

    selected_records[[length(selected_records) + 1L]] <- data.table(
      model_task = model_task,
      evaluation_type = "leave_one_dataset_out",
      test_dataset = ds,
      algorithm = "logistic_glm",
      feature = glm_res$selected_features
    )

    rp <- train_predict_rpart(train_dt, test_dt, label_col, feature_cols)

    if (!is.null(rp)) {
      pred_rp <- copy(pred_dt)
      pred_rp[, algorithm := "rpart_tree"]
      pred_rp[, predicted_probability := rp$prob]
      pred_rp[, model_status := rp$status]

      preds[[length(preds) + 1L]] <- pred_rp

      imp_rp <- as.data.table(rp$importance_table)
      if (nrow(imp_rp) > 0) {
        imp_rp[, model_task := model_task]
        imp_rp[, evaluation_type := "leave_one_dataset_out"]
        imp_rp[, test_dataset := ds]
        imp_rp[, algorithm := "rpart_tree"]
        if (!"coefficient" %in% colnames(imp_rp)) imp_rp[, coefficient := NA_real_]
        imps[[length(imps) + 1L]] <- imp_rp
      }

      selected_records[[length(selected_records) + 1L]] <- data.table(
        model_task = model_task,
        evaluation_type = "leave_one_dataset_out",
        test_dataset = ds,
        algorithm = "rpart_tree",
        feature = rp$selected_features
      )
    }
  }

  list(
    predictions = if (length(preds) > 0) rbindlist(preds, fill = TRUE) else data.table(),
    importance = if (length(imps) > 0) rbindlist(imps, fill = TRUE) else data.table(),
    selected = if (length(selected_records) > 0) rbindlist(selected_records, fill = TRUE) else data.table()
  )
}

summarize_performance <- function(pred_dt) {
  if (nrow(pred_dt) == 0) return(data.table())

  perf <- pred_dt[
    ,
    {
      m <- binary_metrics(true_label, predicted_probability)
      as.data.table(m)
    },
    by = .(model_task, evaluation_type, algorithm)
  ]

  # add per-dataset LODO performance
  if ("test_dataset" %in% colnames(pred_dt)) {
    lodo_perf <- pred_dt[
      evaluation_type == "leave_one_dataset_out" & !is.na(test_dataset),
      {
        m <- binary_metrics(true_label, predicted_probability)
        as.data.table(m)
      },
      by = .(model_task, evaluation_type, algorithm, test_dataset)
    ]

    if (nrow(lodo_perf) > 0) {
      lodo_perf[, group_level := "per_test_dataset"]
      perf[, test_dataset := NA_character_]
      perf[, group_level := "overall"]
      perf <- rbindlist(list(perf, lodo_perf), fill = TRUE)
    }
  } else {
    perf[, test_dataset := NA_character_]
    perf[, group_level := "overall"]
  }

  perf
}


# ============================================================
# 4. 读取训练表
# ============================================================

set.seed(SEED)

stamp("读取 07A V3 ML training tables。")

ideal_train <- as.data.table(read_csv_required(input_ideal_train))
safety_train <- as.data.table(read_csv_required(input_safety_train))
feature_dict <- as.data.table(read_csv_optional(input_feature_dict))
class_balance <- as.data.table(read_csv_optional(input_class_balance))
qc07A <- as.data.table(read_csv_optional(input_qc07A))

if (!"ideal_graft_like_weak_label" %in% colnames(ideal_train)) {
  stop("ideal training table 缺少 ideal_graft_like_weak_label。")
}

if (!"safety_risk_weak_label" %in% colnames(safety_train)) {
  stop("safety training table 缺少 safety_risk_weak_label。")
}

stamp("Ideal training groups：", nrow(ideal_train))
stamp("Safety training groups：", nrow(safety_train))


# ============================================================
# 5. Internal CV
# ============================================================

stamp("训练 internal stratified CV models。")

ideal_cv <- run_internal_cv(
  ideal_train,
  label_col = "ideal_graft_like_weak_label",
  model_task = "ideal_graft_like_model"
)

safety_cv <- run_internal_cv(
  safety_train,
  label_col = "safety_risk_weak_label",
  model_task = "safety_risk_model"
)

internal_preds <- rbindlist(
  list(ideal_cv$predictions, safety_cv$predictions),
  fill = TRUE
)

atomic_write_csv(as.data.frame(internal_preds), internal_pred_csv)


# ============================================================
# 6. Leave-one-dataset-out
# ============================================================

stamp("训练 leave-one-dataset-out exploratory models。")

ideal_lodo <- run_lodo(
  ideal_train,
  label_col = "ideal_graft_like_weak_label",
  model_task = "ideal_graft_like_model"
)

safety_lodo <- run_lodo(
  safety_train,
  label_col = "safety_risk_weak_label",
  model_task = "safety_risk_model"
)

lodo_preds <- rbindlist(
  list(ideal_lodo$predictions, safety_lodo$predictions),
  fill = TRUE
)

atomic_write_csv(as.data.frame(lodo_preds), lodo_pred_csv)


# ============================================================
# 7. Performance summary
# ============================================================

stamp("汇总 model performance。")

perf_internal <- summarize_performance(internal_preds)
perf_lodo <- summarize_performance(lodo_preds)

performance <- rbindlist(
  list(perf_internal, perf_lodo),
  fill = TRUE
)

performance[
  ,
  claim_boundary := "Exploratory marker-rule-derived classification only; labels are rule-derived and performance may be circular if features overlap with label-defining scores."
]

atomic_write_csv(as.data.frame(performance), performance_csv)


# ============================================================
# 8. Feature importance summary
# ============================================================

stamp("汇总 feature importance。")

importance_all <- rbindlist(
  list(
    ideal_cv$importance,
    safety_cv$importance,
    ideal_lodo$importance,
    safety_lodo$importance
  ),
  fill = TRUE
)

if (nrow(importance_all) > 0) {
  if (!"importance" %in% colnames(importance_all)) {
    importance_all[, importance := abs(coefficient)]
  }

  importance_all[, importance := as.numeric(importance)]
  importance_all[is.na(importance), importance := 0]

  feature_importance <- importance_all[
    ,
    .(
      mean_importance = mean(importance, na.rm = TRUE),
      median_importance = median(importance, na.rm = TRUE),
      max_importance = max(importance, na.rm = TRUE),
      n_times_used = .N
    ),
    by = .(model_task, algorithm, feature)
  ][order(model_task, algorithm, -mean_importance)]
} else {
  feature_importance <- data.table()
}

atomic_write_csv(as.data.frame(feature_importance), feature_importance_csv)

selected_all <- rbindlist(
  list(
    ideal_cv$selected,
    safety_cv$selected,
    ideal_lodo$selected,
    safety_lodo$selected
  ),
  fill = TRUE
)

atomic_write_csv(as.data.frame(selected_all), selected_features_csv)


# ============================================================
# 9. QC audit
# ============================================================

stamp("生成 07B QC audit。")

ideal_pos <- sum(ideal_train$ideal_graft_like_weak_label == 1, na.rm = TRUE)
ideal_neg <- sum(ideal_train$ideal_graft_like_weak_label == 0, na.rm = TRUE)
safety_pos <- sum(safety_train$safety_risk_weak_label == 1, na.rm = TRUE)
safety_neg <- sum(safety_train$safety_risk_weak_label == 0, na.rm = TRUE)

n_internal_pred <- nrow(internal_preds)
n_lodo_pred <- nrow(lodo_preds)

best_auc_internal <- if (nrow(perf_internal) > 0) max(perf_internal$auc, na.rm = TRUE) else NA_real_
best_auc_lodo <- if (nrow(perf_lodo) > 0) max(perf_lodo$auc, na.rm = TRUE) else NA_real_

if (is.infinite(best_auc_internal)) best_auc_internal <- NA_real_
if (is.infinite(best_auc_lodo)) best_auc_lodo <- NA_real_

qc_audit <- data.frame(
  metric = c(
    "ideal_training_groups",
    "ideal_positive_groups",
    "ideal_negative_groups",
    "safety_training_groups",
    "safety_positive_groups",
    "safety_negative_groups",
    "internal_CV_prediction_rows",
    "LODO_prediction_rows",
    "algorithms_trained",
    "best_internal_CV_AUC",
    "best_LODO_AUC",
    "rpart_available",
    "claim_boundary"
  ),
  value = c(
    nrow(ideal_train),
    ideal_pos,
    ideal_neg,
    nrow(safety_train),
    safety_pos,
    safety_neg,
    n_internal_pred,
    n_lodo_pred,
    paste(sort(unique(internal_preds$algorithm)), collapse = ";"),
    round(best_auc_internal, 4),
    round(best_auc_lodo, 4),
    HAS_RPART,
    "exploratory marker-rule-derived classification only"
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(qc_audit, qc_audit_csv)


# ============================================================
# 10. 报告
# ============================================================

perf_lines <- if (nrow(performance) > 0) {
  apply(as.data.frame(performance), 1, function(x) {
    paste0(
      x[["model_task"]],
      " / ",
      x[["evaluation_type"]],
      " / ",
      x[["algorithm"]],
      ifelse(!is.na(x[["test_dataset"]]) && x[["test_dataset"]] != "", paste0(" / ", x[["test_dataset"]]), ""),
      ": n=",
      x[["n"]],
      "; AUC=",
      round(as.numeric(x[["auc"]]), 4),
      "; accuracy=",
      round(as.numeric(x[["accuracy"]]), 4)
    )
  })
} else {
  "none"
}

top_feature_lines <- if (nrow(feature_importance) > 0) {
  top_features <- feature_importance[
    ,
    head(.SD, 10),
    by = .(model_task, algorithm)
  ]

  apply(as.data.frame(top_features), 1, function(x) {
    paste0(
      x[["model_task"]],
      " / ",
      x[["algorithm"]],
      " / ",
      x[["feature"]],
      ": mean_importance=",
      round(as.numeric(x[["mean_importance"]]), 4)
    )
  })
} else {
  "none"
}

report_lines <- c(
  "07B exploratory marker-rule-derived prioritization model models report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Training summary:",
  paste0("Ideal training groups: ", nrow(ideal_train), " (positive=", ideal_pos, ", negative=", ideal_neg, ")"),
  paste0("Safety training groups: ", nrow(safety_train), " (positive=", safety_pos, ", negative=", safety_neg, ")"),
  paste0("Internal CV predictions: ", n_internal_pred),
  paste0("LODO predictions: ", n_lodo_pred),
  paste0("Algorithms trained: ", paste(sort(unique(internal_preds$algorithm)), collapse = "; ")),
  "",
  "Performance summary:",
  perf_lines,
  "",
  "Top feature summary:",
  top_feature_lines,
  "",
  "Output files:",
  paste0("Internal CV predictions: ", internal_pred_csv),
  paste0("LODO predictions: ", lodo_pred_csv),
  paste0("Performance summary: ", performance_csv),
  paste0("Feature importance: ", feature_importance_csv),
  paste0("Selected features: ", selected_features_csv),
  paste0("QC audit: ", qc_audit_csv),
  "",
  "Next step:",
  "08A_UMAP_FEATURE_VALIDATION_FOR_KEY_DATASETS.R",
  "",
  "Journal-rigor note:",
  "07B models are exploratory marker-rule-derived classifiers. Do not describe them as validated clinical-use models or experimentally validated graft outcome models."
)

writeLines(report_lines, report_txt)


# ============================================================
# 11. 结束
# ============================================================

cat("\n============================================================\n")
cat("07B exploratory marker-rule-derived prioritization model models 运行结束\n")
cat("============================================================\n\n")

cat("Ideal training groups：", nrow(ideal_train), "\n")
cat("Ideal positive / negative：", ideal_pos, " / ", ideal_neg, "\n")
cat("Safety training groups：", nrow(safety_train), "\n")
cat("Safety positive / negative：", safety_pos, " / ", safety_neg, "\n")
cat("Internal CV prediction rows：", n_internal_pred, "\n")
cat("LODO prediction rows：", n_lodo_pred, "\n")
cat("Best internal CV AUC：", round(best_auc_internal, 4), "\n")
cat("Best LODO AUC：", round(best_auc_lodo, 4), "\n")
cat("Algorithms：", paste(sort(unique(internal_preds$algorithm)), collapse = "; "), "\n\n")

cat("输出文件：\n")
cat(internal_pred_csv, "\n")
cat(lodo_pred_csv, "\n")
cat(performance_csv, "\n")
cat(feature_importance_csv, "\n")
cat(selected_features_csv, "\n")
cat(qc_audit_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 07B exploratory marker-rule-derived prioritization model models 完成。\n")
cat("下一步进入 08A：UMAP / FeaturePlot / DotPlot validation for key datasets。\n")
