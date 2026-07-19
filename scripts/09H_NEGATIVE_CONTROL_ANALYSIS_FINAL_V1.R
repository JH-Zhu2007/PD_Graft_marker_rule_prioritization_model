# 09H~09H_NEGATIVE_CONTROL_ANALYSIS_FINAL_V1.R ----
#
# ============================================================
# 09H_NEGATIVE_CONTROL_ANALYSIS_FINAL_V1.R
# ============================================================
# 09H：Negative-control analysis
#
# 输入：
#   09B reduced non-direct feature training tables：
#     09B_ideal_like_training_reduced_non_direct_features.csv
#     09B_safety_risk_training_reduced_non_direct_features.csv
#
# 目的：
#   回答审稿人可能会问的问题：
#     “你的 marker-rule-derived predictor 是不是真的学到了结构，
#      还是随机 label / 随机 feature 也能得到类似 AUC？”
#
# 负对照设计：
#   1. real_label_original_features：
#        原始 label + 原始 reduced non-direct features
#
#   2. permuted_label_original_features：
#        训练集 label 随机打乱，features 不变；
#        测试集仍用真实 label 评估。
#
#   3. real_label_permuted_features：
#        label 不变，但每个 feature 在全表内随机打乱；
#        破坏 feature-label 关系。
#
# 验证方式：
#   1. Internal stratified K-fold CV
#   2. Leave-one-dataset-out validation
#
# 模型：
#   1. logistic regression
#   2. random forest（如果已安装 randomForest）
#
# 注意：
#   09H 不改变 09B / 09C / 09E / 09F 的模型和结论。
#   09H 是 robustness / negative-control analysis。
#
# 输出：
#   D:/PD_Graft_Project/03_tables/09H_negative_control_analysis_V1/
#   D:/PD_Graft_Project/04_figures/09H_negative_control_analysis_V1_pdf/
#
# 成功标志：
#   ✅ 09H negative-control analysis FINAL V1 完成。
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"

SEED <- 20260715

N_FOLDS <- 5

# 负对照重复次数。30 次已经足够作为初版审稿级 robustness 证据；
# 如果电脑很快，后续可提高到 100。
N_NEGATIVE_CONTROL_REPEATS <- 30

# random forest 设置
RF_NTREE <- 300

# PDF
PDF_WIDTH <- 11.5
PDF_HEIGHT <- 7.5


# ============================================================
# 1. 加载包
# ============================================================

cat("\n============================================================\n")
cat("09H：Negative-control analysis\n")
cat("============================================================\n\n")

options(stringsAsFactors = FALSE)

required_pkgs <- c("data.table", "ggplot2")

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop("缺少 R 包，请先手动安装：", paste(missing_pkgs, collapse = ", "))
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

HAS_RANDOMFOREST <- requireNamespace("randomForest", quietly = TRUE)
if (HAS_RANDOMFOREST) {
  suppressPackageStartupMessages(library(randomForest))
}

set.seed(SEED)


# ============================================================
# 2. 路径
# ============================================================

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(tables_dir, "09H_negative_control_analysis_V1")
out_figures_dir <- file.path(figures_dir, "09H_negative_control_analysis_V1_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

# Output tables
input_file_audit_csv <- file.path(out_tables_dir, "09H_input_file_audit.csv")
feature_audit_csv <- file.path(out_tables_dir, "09H_feature_audit.csv")
split_audit_csv <- file.path(out_tables_dir, "09H_split_audit.csv")
performance_raw_csv <- file.path(out_tables_dir, "09H_negative_control_performance_raw.csv")
repeat_summary_csv <- file.path(out_tables_dir, "09H_negative_control_repeat_summary.csv")
performance_summary_csv <- file.path(out_tables_dir, "09H_negative_control_performance_summary.csv")
empirical_test_csv <- file.path(out_tables_dir, "09H_real_vs_negative_control_empirical_tests.csv")
key_findings_csv <- file.path(out_tables_dir, "09H_key_findings_summary.csv")
method_note_txt <- file.path(out_tables_dir, "09H_method_and_claim_boundary_note.txt")
session_info_txt <- file.path(out_tables_dir, "09H_sessionInfo.txt")
output_check_csv <- file.path(out_tables_dir, "09H_output_verification.csv")
report_txt <- file.path(reports_dir, "09H_negative_control_analysis_report.txt")

# Output figures
fig_internal_auc_pdf <- file.path(out_figures_dir, "09H_internal_CV_real_vs_negative_controls_AUC.pdf")
fig_lodo_auc_pdf <- file.path(out_figures_dir, "09H_LODO_real_vs_negative_controls_AUC.pdf")
fig_delta_auc_pdf <- file.path(out_figures_dir, "09H_real_minus_negative_control_delta_AUC.pdf")
fig_empirical_p_pdf <- file.path(out_figures_dir, "09H_empirical_pvalue_summary.pdf")
fig_design_summary_pdf <- file.path(out_figures_dir, "09H_negative_control_design_summary.pdf")


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

  if (file.exists(path)) unlink(path, force = TRUE)
  data.table::fwrite(df, path)

  if (!file.exists(path)) stop("CSV 未生成：", path)

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes <= 0) {
    stop("CSV 已创建但为空或无效：", path)
  }

  invisible(path)
}

save_pdf_plot <- function(plot_obj, path, width = PDF_WIDTH, height = PDF_HEIGHT) {
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

  ggplot2::ggsave(
    filename = path,
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    device = grDevices::cairo_pdf,
    limitsize = FALSE
  )

  if (!file.exists(path)) stop("PDF 未生成：", path)

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes < 1000) {
    stop("PDF 已创建但文件过小或无效：", path, "；size = ", size_bytes)
  }

  message("已保存 PDF：", normalizePath(path, winslash = "/", mustWork = TRUE),
          " | size = ", round(size_bytes / 1024, 1), " KB")
}

theme_pub <- function(base_size = 11) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = base_size + 2),
      plot.subtitle = element_text(hjust = 0.5, size = base_size - 1, color = "grey25"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      legend.title = element_text(face = "bold"),
      legend.position = "right",
      strip.background = element_rect(fill = "grey90", color = "grey40"),
      strip.text = element_text(face = "bold"),
      plot.margin = margin(8, 12, 8, 12)
    )
}

safe_auc <- function(y_true, score) {
  y <- as.integer(y_true)
  s <- num(score)

  ok <- is.finite(y) & is.finite(s) & y %in% c(0, 1)
  y <- y[ok]
  s <- s[ok]

  if (length(y) < 3) return(NA_real_)
  if (length(unique(y)) < 2) return(NA_real_)

  n_pos <- sum(y == 1)
  n_neg <- sum(y == 0)

  if (n_pos == 0 || n_neg == 0) return(NA_real_)

  r <- rank(s, ties.method = "average")
  auc <- (sum(r[y == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)

  as.numeric(auc)
}

find_training_file <- function(task_prefix) {
  pattern <- paste0("^09B_", task_prefix, "_training_reduced_non_direct_features\\.csv$")

  candidates <- list.files(
    tables_dir,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(candidates) == 0) {
    stop("找不到 09B reduced non-direct training table：", pattern)
  }

  audit <- data.table(path = candidates)
  audit[, file_name := basename(path)]
  audit[, dir_name := dirname(path)]
  audit[, score := 0]
  audit[grepl("V4_FULL_FIXED_LAYOUT", path, ignore.case = TRUE), score := score + 100]
  audit[grepl("V3", path, ignore.case = TRUE), score := score + 80]
  audit[grepl("09B_ML_ready", path, ignore.case = TRUE), score := score + 20]
  audit[, mtime := file.info(path)$mtime]
  audit[is.na(mtime), mtime := as.POSIXct("1970-01-01", tz = "UTC")]

  setorder(audit, -score, -mtime)

  audit[1]
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
    "row_id",
    "fold",
    "predicted_probability",
    "prediction",
    "label",
    "class"
  )

  numeric_cols <- names(dt)[vapply(dt, function(z) is.numeric(z) || is.integer(z), logical(1))]
  feature_cols <- setdiff(numeric_cols, exclude)

  # Remove all-NA / zero-variance features
  feature_cols <- feature_cols[vapply(feature_cols, function(fc) {
    x <- num(dt[[fc]])
    x <- x[is.finite(x)]
    if (length(x) < 3) return(FALSE)
    stats::sd(x, na.rm = TRUE) > 0
  }, logical(1))]

  feature_cols
}

prep_train_params <- function(dt, feature_cols) {
  med <- sapply(feature_cols, function(fc) {
    x <- num(dt[[fc]])
    if (all(is.na(x))) return(0)
    stats::median(x, na.rm = TRUE)
  })

  sdv <- sapply(feature_cols, function(fc) {
    x <- num(dt[[fc]])
    x[is.na(x)] <- med[[fc]]
    s <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(s) || s == 0) s <- 1
    s
  })

  list(median = med, sd = sdv)
}

prep_apply_matrix <- function(dt, feature_cols, prep, scale = TRUE) {
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

fit_predict_logistic <- function(train_dt, test_dt, feature_cols, y_train_model) {
  if (length(unique(y_train_model)) < 2) {
    return(rep(NA_real_, nrow(test_dt)))
  }

  prep <- prep_train_params(train_dt, feature_cols)
  x_train <- prep_apply_matrix(train_dt, feature_cols, prep, scale = TRUE)
  x_test <- prep_apply_matrix(test_dt, feature_cols, prep, scale = TRUE)

  train_df <- data.frame(
    weak_label = as.integer(y_train_model),
    x_train,
    check.names = FALSE
  )

  formula_txt <- paste("weak_label ~", paste(colnames(x_train), collapse = " + "))

  fit <- tryCatch({
    suppressWarnings(stats::glm(as.formula(formula_txt), data = train_df, family = stats::binomial()))
  }, error = function(e) e)

  if (inherits(fit, "error")) {
    return(rep(NA_real_, nrow(test_dt)))
  }

  pred <- tryCatch({
    as.numeric(stats::predict(fit, newdata = x_test, type = "response"))
  }, error = function(e) rep(NA_real_, nrow(test_dt)))

  pred[!is.finite(pred)] <- NA_real_

  pred
}

fit_predict_rf <- function(train_dt, test_dt, feature_cols, y_train_model) {
  if (!HAS_RANDOMFOREST) {
    return(rep(NA_real_, nrow(test_dt)))
  }

  if (length(unique(y_train_model)) < 2) {
    return(rep(NA_real_, nrow(test_dt)))
  }

  prep <- prep_train_params(train_dt, feature_cols)
  x_train <- prep_apply_matrix(train_dt, feature_cols, prep, scale = FALSE)
  x_test <- prep_apply_matrix(test_dt, feature_cols, prep, scale = FALSE)

  y_fac <- factor(as.integer(y_train_model), levels = c(0, 1))

  fit <- tryCatch({
    randomForest::randomForest(
      x = x_train,
      y = y_fac,
      ntree = RF_NTREE,
      mtry = max(1, floor(sqrt(length(feature_cols)))),
      importance = FALSE
    )
  }, error = function(e) e)

  if (inherits(fit, "error")) {
    return(rep(NA_real_, nrow(test_dt)))
  }

  pred <- tryCatch({
    pr <- stats::predict(fit, newdata = x_test, type = "prob")
    if ("1" %in% colnames(pr)) {
      as.numeric(pr[, "1"])
    } else {
      as.numeric(pr[, ncol(pr)])
    }
  }, error = function(e) rep(NA_real_, nrow(test_dt)))

  pred[!is.finite(pred)] <- NA_real_

  pred
}

make_stratified_folds <- function(y, k = N_FOLDS) {
  y <- as.integer(y)
  fold <- rep(NA_integer_, length(y))

  for (cls in sort(unique(y))) {
    idx <- which(y == cls)
    idx <- sample(idx, length(idx))
    fold[idx] <- rep(seq_len(k), length.out = length(idx))
  }

  fold
}

permute_features <- function(dt, feature_cols) {
  out <- copy(dt)
  for (fc in feature_cols) {
    out[[fc]] <- sample(out[[fc]], nrow(out), replace = FALSE)
  }
  out
}

run_internal_cv_once <- function(dt, feature_cols, task_name, control_type, repeat_id, k = N_FOLDS) {
  work_dt <- copy(dt)

  if (control_type == "real_label_permuted_features") {
    work_dt <- permute_features(work_dt, feature_cols)
  }

  y_true_all <- as.integer(work_dt$weak_label)
  fold <- make_stratified_folds(y_true_all, k = k)

  split_result <- list()

  for (ff in seq_len(k)) {
    train_idx <- which(fold != ff)
    test_idx <- which(fold == ff)

    train_dt <- work_dt[train_idx]
    test_dt <- work_dt[test_idx]

    y_train_true <- as.integer(train_dt$weak_label)
    y_test_true <- as.integer(test_dt$weak_label)

    if (length(unique(y_train_true)) < 2 || length(unique(y_test_true)) < 2) {
      next
    }

    if (control_type == "permuted_label_original_features") {
      y_train_model <- sample(y_train_true, length(y_train_true), replace = FALSE)
    } else {
      y_train_model <- y_train_true
    }

    pred_log <- fit_predict_logistic(train_dt, test_dt, feature_cols, y_train_model)
    pred_rf <- fit_predict_rf(train_dt, test_dt, feature_cols, y_train_model)

    split_result[[length(split_result) + 1L]] <- data.table(
      task_name = task_name,
      validation_type = "internal_CV",
      control_type = control_type,
      repeat_id = repeat_id,
      split_id = paste0("fold_", ff),
      heldout_dataset = NA_character_,
      model = "logistic",
      auc = safe_auc(y_test_true, pred_log),
      n_train = length(train_idx),
      n_test = length(test_idx),
      n_pos_test = sum(y_test_true == 1),
      n_neg_test = sum(y_test_true == 0)
    )

    if (HAS_RANDOMFOREST) {
      split_result[[length(split_result) + 1L]] <- data.table(
        task_name = task_name,
        validation_type = "internal_CV",
        control_type = control_type,
        repeat_id = repeat_id,
        split_id = paste0("fold_", ff),
        heldout_dataset = NA_character_,
        model = "random_forest",
        auc = safe_auc(y_test_true, pred_rf),
        n_train = length(train_idx),
        n_test = length(test_idx),
        n_pos_test = sum(y_test_true == 1),
        n_neg_test = sum(y_test_true == 0)
      )
    }
  }

  rbindlist(split_result, fill = TRUE)
}

run_lodo_once <- function(dt, feature_cols, task_name, control_type, repeat_id) {
  work_dt <- copy(dt)

  if (control_type == "real_label_permuted_features") {
    work_dt <- permute_features(work_dt, feature_cols)
  }

  datasets <- sort(unique(as.character(work_dt$dataset)))
  split_result <- list()

  for (ds in datasets) {
    train_dt <- work_dt[as.character(dataset) != ds]
    test_dt <- work_dt[as.character(dataset) == ds]

    y_train_true <- as.integer(train_dt$weak_label)
    y_test_true <- as.integer(test_dt$weak_label)

    if (nrow(train_dt) < 10 || nrow(test_dt) < 3) next
    if (length(unique(y_train_true)) < 2 || length(unique(y_test_true)) < 2) next

    if (control_type == "permuted_label_original_features") {
      y_train_model <- sample(y_train_true, length(y_train_true), replace = FALSE)
    } else {
      y_train_model <- y_train_true
    }

    pred_log <- fit_predict_logistic(train_dt, test_dt, feature_cols, y_train_model)
    pred_rf <- fit_predict_rf(train_dt, test_dt, feature_cols, y_train_model)

    split_result[[length(split_result) + 1L]] <- data.table(
      task_name = task_name,
      validation_type = "LODO",
      control_type = control_type,
      repeat_id = repeat_id,
      split_id = paste0("heldout_", ds),
      heldout_dataset = ds,
      model = "logistic",
      auc = safe_auc(y_test_true, pred_log),
      n_train = nrow(train_dt),
      n_test = nrow(test_dt),
      n_pos_test = sum(y_test_true == 1),
      n_neg_test = sum(y_test_true == 0)
    )

    if (HAS_RANDOMFOREST) {
      split_result[[length(split_result) + 1L]] <- data.table(
        task_name = task_name,
        validation_type = "LODO",
        control_type = control_type,
        repeat_id = repeat_id,
        split_id = paste0("heldout_", ds),
        heldout_dataset = ds,
        model = "random_forest",
        auc = safe_auc(y_test_true, pred_rf),
        n_train = nrow(train_dt),
        n_test = nrow(test_dt),
        n_pos_test = sum(y_test_true == 1),
        n_neg_test = sum(y_test_true == 0)
      )
    }
  }

  rbindlist(split_result, fill = TRUE)
}

run_task_negative_controls <- function(task_name, dt, feature_cols) {
  control_plan <- data.table(
    control_type = c(
      "real_label_original_features",
      "permuted_label_original_features",
      "real_label_permuted_features"
    ),
    n_repeats = c(1L, N_NEGATIVE_CONTROL_REPEATS, N_NEGATIVE_CONTROL_REPEATS)
  )

  all_results <- list()
  split_audits <- list()

  for (cc in seq_len(nrow(control_plan))) {
    control_type <- control_plan$control_type[cc]
    n_rep <- control_plan$n_repeats[cc]

    for (rr in seq_len(n_rep)) {
      stamp("Task=", task_name, " | control=", control_type, " | repeat=", rr, "/", n_rep)

      internal_dt <- run_internal_cv_once(
        dt = dt,
        feature_cols = feature_cols,
        task_name = task_name,
        control_type = control_type,
        repeat_id = rr,
        k = N_FOLDS
      )

      lodo_dt <- run_lodo_once(
        dt = dt,
        feature_cols = feature_cols,
        task_name = task_name,
        control_type = control_type,
        repeat_id = rr
      )

      all_results[[length(all_results) + 1L]] <- rbindlist(
        list(internal_dt, lodo_dt),
        fill = TRUE
      )
    }
  }

  rbindlist(all_results, fill = TRUE)
}


# ============================================================
# 4. 读取 09B reduced feature training tables
# ============================================================

stamp("定位并读取 09B reduced non-direct training tables。")

ideal_file <- find_training_file("ideal_like")
safety_file <- find_training_file("safety_risk")

input_file_audit <- rbindlist(list(
  cbind(task_name = "ideal_like_classifier", ideal_file),
  cbind(task_name = "safety_risk_classifier", safety_file)
), fill = TRUE)

atomic_write_csv(as.data.frame(input_file_audit), input_file_audit_csv)

ideal_dt <- data.table::fread(ideal_file$path[1], data.table = TRUE, showProgress = FALSE)
safety_dt <- data.table::fread(safety_file$path[1], data.table = TRUE, showProgress = FALSE)

# Basic checks
for (nm in c("weak_label", "dataset")) {
  if (!nm %in% names(ideal_dt)) stop("ideal training table 缺少列：", nm)
  if (!nm %in% names(safety_dt)) stop("safety training table 缺少列：", nm)
}

ideal_dt[, weak_label := as.integer(weak_label)]
safety_dt[, weak_label := as.integer(weak_label)]

ideal_features <- get_feature_cols(ideal_dt)
safety_features <- get_feature_cols(safety_dt)

if (length(ideal_features) < 2) stop("ideal feature cols 太少：", length(ideal_features))
if (length(safety_features) < 2) stop("safety feature cols 太少：", length(safety_features))

feature_audit <- rbindlist(list(
  data.table(
    task_name = "ideal_like_classifier",
    n_rows = nrow(ideal_dt),
    n_datasets = uniqueN(ideal_dt$dataset),
    n_positive = sum(ideal_dt$weak_label == 1, na.rm = TRUE),
    n_negative = sum(ideal_dt$weak_label == 0, na.rm = TRUE),
    n_features = length(ideal_features),
    features = paste(ideal_features, collapse = ";")
  ),
  data.table(
    task_name = "safety_risk_classifier",
    n_rows = nrow(safety_dt),
    n_datasets = uniqueN(safety_dt$dataset),
    n_positive = sum(safety_dt$weak_label == 1, na.rm = TRUE),
    n_negative = sum(safety_dt$weak_label == 0, na.rm = TRUE),
    n_features = length(safety_features),
    features = paste(safety_features, collapse = ";")
  )
), fill = TRUE)

atomic_write_csv(as.data.frame(feature_audit), feature_audit_csv)

stamp("randomForest available：", HAS_RANDOMFOREST)


# ============================================================
# 5. 运行 negative controls
# ============================================================

stamp("运行 09H negative-control analysis。")

performance_list <- list(
  run_task_negative_controls(
    task_name = "ideal_like_classifier",
    dt = ideal_dt,
    feature_cols = ideal_features
  ),
  run_task_negative_controls(
    task_name = "safety_risk_classifier",
    dt = safety_dt,
    feature_cols = safety_features
  )
)

performance_raw <- rbindlist(performance_list, fill = TRUE)
performance_raw <- performance_raw[is.finite(auc)]

if (nrow(performance_raw) == 0) {
  stop("所有 negative-control performance 都是 NA，无法继续。")
}

atomic_write_csv(as.data.frame(performance_raw), performance_raw_csv)

split_audit <- performance_raw[
  ,
  .(
    n_splits = .N,
    mean_n_train = mean(n_train, na.rm = TRUE),
    mean_n_test = mean(n_test, na.rm = TRUE),
    min_n_pos_test = min(n_pos_test, na.rm = TRUE),
    min_n_neg_test = min(n_neg_test, na.rm = TRUE)
  ),
  by = .(task_name, validation_type, control_type, model)
]

atomic_write_csv(as.data.frame(split_audit), split_audit_csv)


# ============================================================
# 6. 汇总性能和 empirical tests
# ============================================================

stamp("汇总 negative-control performance。")

repeat_summary <- performance_raw[
  ,
  .(
    mean_auc = mean(auc, na.rm = TRUE),
    median_auc = median(auc, na.rm = TRUE),
    sd_auc = sd(auc, na.rm = TRUE),
    n_splits = .N
  ),
  by = .(task_name, validation_type, control_type, model, repeat_id)
]

atomic_write_csv(as.data.frame(repeat_summary), repeat_summary_csv)

performance_summary <- repeat_summary[
  ,
  .(
    n_repeats = .N,
    mean_of_mean_auc = mean(mean_auc, na.rm = TRUE),
    median_of_mean_auc = median(mean_auc, na.rm = TRUE),
    sd_of_mean_auc = sd(mean_auc, na.rm = TRUE),
    min_mean_auc = min(mean_auc, na.rm = TRUE),
    max_mean_auc = max(mean_auc, na.rm = TRUE)
  ),
  by = .(task_name, validation_type, control_type, model)
][order(task_name, validation_type, model, control_type)]

atomic_write_csv(as.data.frame(performance_summary), performance_summary_csv)

# Empirical tests: compare real repeat mean_auc to each negative-control distribution
test_rows <- list()

keys <- unique(repeat_summary[, .(task_name, validation_type, model)])

for (ii in seq_len(nrow(keys))) {
  kk <- keys[ii]

  real_val <- repeat_summary[
    task_name == kk$task_name &
      validation_type == kk$validation_type &
      model == kk$model &
      control_type == "real_label_original_features",
    mean_auc
  ]

  if (length(real_val) == 0 || !is.finite(real_val[1])) next
  real_val <- real_val[1]

  for (ctrl in c("permuted_label_original_features", "real_label_permuted_features")) {
    null_vals <- repeat_summary[
      task_name == kk$task_name &
        validation_type == kk$validation_type &
        model == kk$model &
        control_type == ctrl,
      mean_auc
    ]

    null_vals <- null_vals[is.finite(null_vals)]

    if (length(null_vals) == 0) next

    empirical_p_high <- (1 + sum(null_vals >= real_val)) / (length(null_vals) + 1)
    delta_auc_vs_null_median <- real_val - median(null_vals, na.rm = TRUE)
    delta_auc_vs_null_mean <- real_val - mean(null_vals, na.rm = TRUE)

    test_rows[[length(test_rows) + 1L]] <- data.table(
      task_name = kk$task_name,
      validation_type = kk$validation_type,
      model = kk$model,
      negative_control_type = ctrl,
      real_mean_auc = real_val,
      null_mean_auc = mean(null_vals, na.rm = TRUE),
      null_median_auc = median(null_vals, na.rm = TRUE),
      null_sd_auc = sd(null_vals, na.rm = TRUE),
      null_repeats = length(null_vals),
      delta_auc_vs_null_mean = delta_auc_vs_null_mean,
      delta_auc_vs_null_median = delta_auc_vs_null_median,
      empirical_p_high = empirical_p_high
    )
  }
}

empirical_tests <- rbindlist(test_rows, fill = TRUE)
atomic_write_csv(as.data.frame(empirical_tests), empirical_test_csv)


# ============================================================
# 7. Figures
# ============================================================

stamp("绘制 09H PDF figures。")

plot_auc_box <- function(validation_name, title_text, out_path) {
  plot_dt <- repeat_summary[validation_type == validation_name]
  plot_dt[, control_type := factor(
    control_type,
    levels = c(
      "real_label_original_features",
      "permuted_label_original_features",
      "real_label_permuted_features"
    ),
    labels = c(
      "Real labels\nOriginal features",
      "Permuted labels\nOriginal features",
      "Real labels\nPermuted features"
    )
  )]

  p <- ggplot(plot_dt, aes(x = control_type, y = mean_auc)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", linewidth = 0.35, color = "grey45") +
    geom_boxplot(outlier.shape = NA, width = 0.55, fill = "grey85", color = "grey30") +
    geom_jitter(width = 0.08, height = 0, size = 1.4, alpha = 0.65) +
    facet_grid(task_name ~ model) +
    labs(
      title = title_text,
      subtitle = "Each point is one repeat-level mean AUC; dashed line indicates AUC = 0.5",
      x = NULL,
      y = "Mean AUC"
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_pub(base_size = 10.5) +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1, size = 8),
      strip.text = element_text(size = 9.5)
    )

  save_pdf_plot(p, out_path, width = 12.8, height = 7.8)
}

plot_auc_box("internal_CV", "Internal CV real models versus negative controls", fig_internal_auc_pdf)
plot_auc_box("LODO", "Leave-one-dataset-out real models versus negative controls", fig_lodo_auc_pdf)


# Delta AUC
delta_dt <- copy(empirical_tests)
delta_dt[, negative_control_label := fifelse(
  negative_control_type == "permuted_label_original_features",
  "Permuted labels",
  "Permuted features"
)]

p_delta <- ggplot(
  delta_dt,
  aes(x = negative_control_label, y = delta_auc_vs_null_median)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.35, color = "grey45") +
  geom_col(width = 0.68, fill = "grey55", color = "grey25", linewidth = 0.22) +
  facet_grid(validation_type + task_name ~ model) +
  labs(
    title = "Real model AUC improvement over negative-control null distributions",
    subtitle = "Delta AUC = real mean AUC − null median AUC",
    x = NULL,
    y = "Delta AUC vs null median"
  ) +
  theme_pub(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, size = 8),
    strip.text = element_text(size = 8.5)
  )

save_pdf_plot(p_delta, fig_delta_auc_pdf, width = 13.5, height = 8.5)


# Empirical p value
p_emp <- ggplot(
  delta_dt,
  aes(x = negative_control_label, y = empirical_p_high)
) +
  geom_col(width = 0.68, fill = "grey55", color = "grey25", linewidth = 0.22) +
  geom_hline(yintercept = 0.05, linetype = "dashed", linewidth = 0.35, color = "grey45") +
  facet_grid(validation_type + task_name ~ model) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.03))) +
  labs(
    title = "Empirical probability that negative controls match or exceed real-model AUC",
    subtitle = "Lower values indicate stronger separation from negative controls",
    x = NULL,
    y = "Empirical p-high"
  ) +
  theme_pub(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, size = 8),
    strip.text = element_text(size = 8.5)
  )

save_pdf_plot(p_emp, fig_empirical_p_pdf, width = 13.5, height = 8.5)


# Design summary
design_dt <- data.table(
  Metric = c(
    "Tasks",
    "Validation strategies",
    "Negative controls",
    "Control repeats",
    "Models",
    "Claim boundary"
  ),
  Value = c(
    "Ideal-like classifier; safety-risk classifier",
    "Internal stratified CV; leave-one-dataset-out",
    "Permuted training labels; permuted features",
    as.character(N_NEGATIVE_CONTROL_REPEATS),
    ifelse(HAS_RANDOMFOREST, "Logistic regression; random forest", "Logistic regression only"),
    "Marker-rule-derived transcriptomic robustness only"
  )
)

design_dt[, row_id := seq_len(.N)]
design_dt[, y := rev(row_id)]

p_design <- ggplot(design_dt, aes(y = y)) +
  annotate(
    "text",
    x = 0,
    y = max(design_dt$y) + 1.0,
    label = "09H negative-control analysis design",
    hjust = 0,
    fontface = "bold",
    size = 5.0
  ) +
  geom_text(aes(x = 0.02, label = Metric), hjust = 0, fontface = "bold", size = 3.75) +
  geom_text(aes(x = 0.42, label = Value), hjust = 0, size = 3.75) +
  annotate(
    "text",
    x = 0.02,
    y = 0.35,
    label = paste(
      "Interpretation:",
      "negative controls test whether model performance depends on true feature-label structure;",
      "they do not establish clinical predictive validity."
    ),
    hjust = 0,
    size = 3.25
  ) +
  xlim(0, 1.35) +
  ylim(0, max(design_dt$y) + 1.5) +
  theme_void()

save_pdf_plot(p_design, fig_design_summary_pdf, width = 11.5, height = 6.4)


# ============================================================
# 8. Key findings / method note / report
# ============================================================

stamp("写出 09H key findings / method note / report。")

# Summary values
real_perf <- performance_summary[control_type == "real_label_original_features"]
best_delta <- empirical_tests[
  ,
  .SD[which.max(delta_auc_vs_null_median)],
  by = .(task_name, validation_type, model)
]

strong_negative_control_pass <- empirical_tests[
  ,
  .(
    n_tests = .N,
    n_empirical_p_le_0_05 = sum(empirical_p_high <= 0.05, na.rm = TRUE),
    n_delta_auc_positive = sum(delta_auc_vs_null_median > 0, na.rm = TRUE),
    fraction_delta_auc_positive = mean(delta_auc_vs_null_median > 0, na.rm = TRUE)
  )
]

key_findings <- data.table(
  item = c(
    "ideal_training_file",
    "safety_training_file",
    "randomForest_available",
    "negative_control_repeats",
    "validation_types",
    "models",
    "total_performance_rows",
    "empirical_tests",
    "negative_control_positive_delta_fraction",
    "empirical_p_le_0.05_count",
    "claim_boundary"
  ),
  value = c(
    ideal_file$path[1],
    safety_file$path[1],
    as.character(HAS_RANDOMFOREST),
    as.character(N_NEGATIVE_CONTROL_REPEATS),
    "internal_CV; LODO",
    ifelse(HAS_RANDOMFOREST, "logistic; random_forest", "logistic"),
    as.character(nrow(performance_raw)),
    as.character(nrow(empirical_tests)),
    signif(strong_negative_control_pass$fraction_delta_auc_positive[1], 4),
    paste0(strong_negative_control_pass$n_empirical_p_le_0_05[1], "/", strong_negative_control_pass$n_tests[1]),
    "Negative-control separation supports transcriptomic model structure only; it is not clinical, functional, or therapeutic validation."
  )
)

atomic_write_csv(as.data.frame(key_findings), key_findings_csv)

method_lines <- c(
  "09H negative-control analysis method and claim-boundary note",
  "",
  "Purpose:",
  "09H tests whether the marker-rule-derived predictors outperform negative controls generated by label permutation or feature permutation.",
  "",
  "Inputs:",
  paste0("Ideal-like training table: ", ideal_file$path[1]),
  paste0("Safety-risk training table: ", safety_file$path[1]),
  "",
  "Negative controls:",
  "1. permuted_label_original_features: training labels are shuffled while features remain unchanged; test labels remain true.",
  "2. real_label_permuted_features: labels remain true, but feature columns are independently permuted to disrupt feature-label relationships.",
  "",
  "Validation:",
  "Internal stratified K-fold CV and leave-one-dataset-out validation were used.",
  "",
  "Models:",
  ifelse(HAS_RANDOMFOREST, "Logistic regression and random forest were evaluated.", "Only logistic regression was evaluated because randomForest was not installed."),
  "",
  "Claim boundary:",
  "This analysis evaluates whether model performance depends on non-random transcriptomic feature-label structure.",
  "It does not validate clinical outcome prediction, graft functional integration, anatomical projection, therapeutic efficacy, tumorigenicity, or clinical safety."
)

writeLines(method_lines, method_note_txt)

report_lines <- c(
  "09H negative-control analysis report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input file audit:",
  capture.output(print(input_file_audit)),
  "",
  "Feature audit:",
  capture.output(print(feature_audit)),
  "",
  "Performance summary:",
  capture.output(print(performance_summary)),
  "",
  "Empirical tests:",
  capture.output(print(empirical_tests)),
  "",
  "Key findings:",
  capture.output(print(key_findings)),
  "",
  "Output directories:",
  out_tables_dir,
  out_figures_dir
)

writeLines(report_lines, report_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)


# ============================================================
# 9. Output verification
# ============================================================

required_outputs <- c(
  input_file_audit_csv,
  feature_audit_csv,
  split_audit_csv,
  performance_raw_csv,
  repeat_summary_csv,
  performance_summary_csv,
  empirical_test_csv,
  key_findings_csv,
  method_note_txt,
  session_info_txt,
  report_txt,
  fig_internal_auc_pdf,
  fig_lodo_auc_pdf,
  fig_delta_auc_pdf,
  fig_empirical_p_pdf,
  fig_design_summary_pdf
)

output_check <- data.table(
  file = required_outputs,
  exists = file.exists(required_outputs),
  size_bytes = ifelse(file.exists(required_outputs), file.info(required_outputs)$size, NA_real_)
)

atomic_write_csv(as.data.frame(output_check), output_check_csv)

bad <- output_check[!exists | is.na(size_bytes) | size_bytes <= 0]
if (nrow(bad) > 0) {
  print(bad)
  stop("09H 输出验证失败。")
}


# ============================================================
# 10. 完成
# ============================================================

cat("\n============================================================\n")
cat("09H negative-control analysis FINAL V1 运行结束\n")
cat("============================================================\n\n")

cat("Ideal training file：", ideal_file$path[1], "\n")
cat("Safety training file：", safety_file$path[1], "\n")
cat("randomForest available：", HAS_RANDOMFOREST, "\n")
cat("Negative-control repeats：", N_NEGATIVE_CONTROL_REPEATS, "\n")
cat("Performance rows：", nrow(performance_raw), "\n")
cat("Empirical tests：", nrow(empirical_tests), "\n")
cat("Positive delta-AUC fraction：", signif(strong_negative_control_pass$fraction_delta_auc_positive[1], 4), "\n")
cat("Empirical p<=0.05 tests：", strong_negative_control_pass$n_empirical_p_le_0_05[1], "/", strong_negative_control_pass$n_tests[1], "\n\n")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("关键输出：\n")
cat(input_file_audit_csv, "\n")
cat(feature_audit_csv, "\n")
cat(performance_summary_csv, "\n")
cat(empirical_test_csv, "\n")
cat(key_findings_csv, "\n")
cat(method_note_txt, "\n\n")

cat("PDF 图：\n")
cat(fig_internal_auc_pdf, "\n")
cat(fig_lodo_auc_pdf, "\n")
cat(fig_delta_auc_pdf, "\n")
cat(fig_empirical_p_pdf, "\n")
cat(fig_design_summary_pdf, "\n\n")

cat("✅ 09H negative-control analysis FINAL V1 完成。\n")
