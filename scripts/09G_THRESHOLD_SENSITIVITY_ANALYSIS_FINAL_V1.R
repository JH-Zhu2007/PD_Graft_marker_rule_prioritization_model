
PROJECT_DIR <- "D:/PD_Graft_Project"

SEED <- 20260715

DIAGONAL_SETTINGS <- data.frame(
  setting_id = c("relaxed", "moderate", "baseline_like", "strict", "very_strict"),
  ideal_high_q = c(0.65, 0.70, 0.75, 0.80, 0.85),
  safety_low_q = c(0.35, 0.30, 0.25, 0.20, 0.15),
  safety_high_q = c(0.65, 0.70, 0.75, 0.80, 0.85),
  stringsAsFactors = FALSE
)

IDEAL_HIGH_Q_GRID <- c(0.65, 0.70, 0.75, 0.80, 0.85)
SAFETY_LOW_Q_GRID <- c(0.15, 0.20, 0.25, 0.30, 0.35)
SAFETY_HIGH_Q_GRID <- c(0.65, 0.70, 0.75, 0.80, 0.85)

BASELINE_SETTING_ID <- "baseline_like"

STABLE_CLASS_FRACTION_CUTOFF <- 0.80

PDF_WIDTH <- 11.5
PDF_HEIGHT <- 7.5

cat("\n============================================================\n")
cat("09G：Threshold sensitivity analysis\n")
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

set.seed(SEED)

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(tables_dir, "09G_threshold_sensitivity_analysis_V1")
out_figures_dir <- file.path(figures_dir, "09G_threshold_sensitivity_analysis_V1_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

input_candidate_audit_csv <- file.path(out_tables_dir, "09G_input_table_candidate_audit.csv")
selected_input_audit_csv <- file.path(out_tables_dir, "09G_selected_input_score_column_audit.csv")
threshold_grid_csv <- file.path(out_tables_dir, "09G_threshold_grid.csv")
threshold_values_csv <- file.path(out_tables_dir, "09G_threshold_values_by_setting.csv")
group_classification_csv <- file.path(out_tables_dir, "09G_group_classification_by_threshold.csv")
class_fraction_csv <- file.path(out_tables_dir, "09G_class_fraction_by_threshold.csv")
dataset_priority_csv <- file.path(out_tables_dir, "09G_dataset_priority_by_threshold.csv")
dataset_rank_stability_csv <- file.path(out_tables_dir, "09G_dataset_rank_stability.csv")
group_stability_csv <- file.path(out_tables_dir, "09G_group_classification_stability.csv")
baseline_label_audit_csv <- file.path(out_tables_dir, "09G_baseline_label_sensitivity_audit.csv")
key_findings_csv <- file.path(out_tables_dir, "09G_key_findings_summary.csv")
method_note_txt <- file.path(out_tables_dir, "09G_method_and_claim_boundary_note.txt")
session_info_txt <- file.path(out_tables_dir, "09G_sessionInfo.txt")
output_check_csv <- file.path(out_tables_dir, "09G_output_verification.csv")
report_txt <- file.path(reports_dir, "09G_threshold_sensitivity_analysis_report.txt")

fig_class_fraction_pdf <- file.path(out_figures_dir, "09G_class_fraction_sensitivity_heatmap.pdf")
fig_dataset_priority_pdf <- file.path(out_figures_dir, "09G_dataset_priority_index_by_setting.pdf")
fig_dataset_rank_pdf <- file.path(out_figures_dir, "09G_dataset_rank_stability_heatmap.pdf")
fig_group_stability_pdf <- file.path(out_figures_dir, "09G_group_classification_stability_barplot.pdf")
fig_baseline_agreement_pdf <- file.path(out_figures_dir, "09G_baseline_label_agreement_barplot.pdf")
fig_threshold_summary_pdf <- file.path(out_figures_dir, "09G_threshold_value_summary.pdf")

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

safe_quantile <- function(x, q) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = q, na.rm = TRUE, names = FALSE, type = 7))
}

has_cols <- function(dt, cols) all(cols %in% names(dt))

first_matching_col <- function(cols, patterns, must_be_numeric = TRUE, dt = NULL) {
  cols_chr <- as.character(cols)

  for (pat in patterns) {
    hit <- cols_chr[grepl(pat, cols_chr, ignore.case = TRUE, perl = TRUE)]
    if (length(hit) > 0) {
      if (!is.null(dt) && must_be_numeric) {
        hit <- hit[vapply(hit, function(cc) {
          suppressWarnings(any(is.finite(as.numeric(dt[[cc]]))))
        }, logical(1))]
      }
      if (length(hit) > 0) return(hit[1])
    }
  }

  NA_character_
}

normalize_class <- function(x) {
  x <- tolower(as.character(x))
  out <- rep("lower_priority_or_mixed", length(x))

  out[grepl("ideal|favorable|high_safety_low|candidate", x)] <- "ideal_like"
  out[grepl("safety.*risk.*high|risk_high|safety_risk|high_risk", x)] <- "safety_risk_like"
  out[grepl("mixed|uncertain|lower", x)] <- "lower_priority_or_mixed"

  out
}

short_dataset_label <- function(x) {
  x <- as.character(x)
  x <- gsub("_01B|_V[0-9]+|\\.rds$", "", x)
  x
}

score_table_candidate <- function(path) {
  out <- tryCatch({
    dt_head <- data.table::fread(path, nrows = 80, data.table = TRUE, showProgress = FALSE)
    cols <- names(dt_head)

    ideal_patterns <- c(
      "DA.*projection.*composite",
      "projection.*competence.*composite",
      "DA_projection",
      "ideal.*score",
      "favorable.*score"
    )

    safety_patterns <- c(
      "safety.*risk.*composite",
      "safety_risk",
      "risk.*composite"
    )

    label_patterns <- c(
      "safety_contrast_class",
      "class_05B",
      "weak_label",
      "candidate",
      "priority"
    )

    ideal_hit <- first_matching_col(cols, ideal_patterns, must_be_numeric = FALSE)
    safety_hit <- first_matching_col(cols, safety_patterns, must_be_numeric = FALSE)
    label_hit <- first_matching_col(cols, label_patterns, must_be_numeric = FALSE)

    id_score <- sum(c("dataset", "object_id", "group_id", "n_cells") %in% cols)
    file_score <- 0
    fn <- tolower(basename(path))

    if (grepl("05b|07a|09b|09a", fn)) file_score <- file_score + 5
    if (grepl("master|full|group|safety|contrast|feature|training", fn)) file_score <- file_score + 4
    if (grepl("reduced_non_direct", fn)) file_score <- file_score - 2

    score <- 0
    score <- score + ifelse(!is.na(ideal_hit), 20, 0)
    score <- score + ifelse(!is.na(safety_hit), 20, 0)
    score <- score + ifelse(!is.na(label_hit), 5, 0)
    score <- score + id_score
    score <- score + file_score
    score <- score + min(10, nrow(dt_head) / 8)

    data.table(
      path = path,
      file_name = basename(path),
      n_cols = length(cols),
      n_head_rows = nrow(dt_head),
      has_ideal_score_candidate = !is.na(ideal_hit),
      has_safety_score_candidate = !is.na(safety_hit),
      ideal_score_candidate = ideal_hit,
      safety_score_candidate = safety_hit,
      label_candidate = label_hit,
      id_score = id_score,
      candidate_score = score
    )
  }, error = function(e) {
    data.table(
      path = path,
      file_name = basename(path),
      n_cols = NA_integer_,
      n_head_rows = NA_integer_,
      has_ideal_score_candidate = FALSE,
      has_safety_score_candidate = FALSE,
      ideal_score_candidate = NA_character_,
      safety_score_candidate = NA_character_,
      label_candidate = NA_character_,
      id_score = NA_integer_,
      candidate_score = -Inf,
      error = conditionMessage(e)
    )
  })

  out
}

classify_by_threshold <- function(dt, setting_row, ideal_col, safety_col, baseline_flag = FALSE) {
  ideal_q <- setting_row$ideal_high_q
  safety_low_q <- setting_row$safety_low_q
  safety_high_q <- setting_row$safety_high_q

  ideal_threshold <- safe_quantile(dt[[ideal_col]], ideal_q)
  safety_low_threshold <- safe_quantile(dt[[safety_col]], safety_low_q)
  safety_high_threshold <- safe_quantile(dt[[safety_col]], safety_high_q)

  out <- copy(dt)
  out[, setting_id := setting_row$setting_id]
  out[, setting_type := setting_row$setting_type]
  out[, ideal_high_q := ideal_q]
  out[, safety_low_q := safety_low_q]
  out[, safety_high_q := safety_high_q]
  out[, ideal_high_threshold := ideal_threshold]
  out[, safety_low_threshold := safety_low_threshold]
  out[, safety_high_threshold := safety_high_threshold]

  out[, ideal_score_for_threshold := num(get(ideal_col))]
  out[, safety_score_for_threshold := num(get(safety_col))]

  out[, sensitivity_class := fifelse(
    safety_score_for_threshold >= safety_high_threshold,
    "safety_risk_like",
    fifelse(
      ideal_score_for_threshold >= ideal_high_threshold &
        safety_score_for_threshold <= safety_low_threshold,
      "ideal_like",
      "lower_priority_or_mixed"
    )
  )]

  out[]
}

stamp("扫描 03_tables，自动寻找 threshold sensitivity 输入表。")

all_csv <- list.files(
  tables_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(all_csv) == 0) {
  stop("03_tables 下没有找到 CSV 文件。")
}

exclude_patterns <- c(
  "sessionInfo",
  "output_verification",
  "figure_manifest",
  "method",
  "audit$",
  "gene_overlap",
  "probabilities",
  "prediction",
  "external",
  "GO_",
  "KEGG",
  "HALLMARK",
  "DEG_table"
)

all_csv_use <- all_csv[
  !grepl(paste(exclude_patterns, collapse = "|"), basename(all_csv), ignore.case = TRUE)
]

if (length(all_csv_use) == 0) all_csv_use <- all_csv

candidate_audit <- rbindlist(lapply(all_csv_use, score_table_candidate), fill = TRUE)
setorder(candidate_audit, -candidate_score)

atomic_write_csv(as.data.frame(candidate_audit), input_candidate_audit_csv)

selected_path <- candidate_audit[
  has_ideal_score_candidate == TRUE &
    has_safety_score_candidate == TRUE &
    is.finite(candidate_score)
][1, path]

if (length(selected_path) == 0 || is.na(selected_path)) {
  stop(
    "无法自动找到同时包含 ideal/projection score 和 safety-risk score 的输入表。\n",
    "请查看：", input_candidate_audit_csv, "\n",
    "需要的列类似：DA_projection_competence_composite_score + safety_risk_composite_05B。"
  )
}

stamp("选择输入表：", selected_path)

input_dt <- data.table::fread(selected_path, data.table = TRUE, showProgress = FALSE)

stamp("识别 ideal / safety / ID / baseline label 列。")

ideal_patterns <- c(
  "^DA_projection_competence_composite_score$",
  "DA.*projection.*competence.*composite",
  "DA_projection",
  "projection.*competence.*composite",
  "ideal.*score",
  "favorable.*score"
)

safety_patterns <- c(
  "^safety_risk_composite_05B$",
  "safety.*risk.*composite",
  "safety_risk",
  "risk.*composite"
)

baseline_patterns <- c(
  "safety_contrast_class_05B",
  "safety.*contrast.*class",
  "class_05B",
  "candidate_state",
  "priority_class",
  "final_class",
  "weak_label"
)

ideal_col <- first_matching_col(names(input_dt), ideal_patterns, must_be_numeric = TRUE, dt = input_dt)
safety_col <- first_matching_col(names(input_dt), safety_patterns, must_be_numeric = TRUE, dt = input_dt)
baseline_col <- first_matching_col(names(input_dt), baseline_patterns, must_be_numeric = FALSE, dt = input_dt)

if (is.na(ideal_col) || is.na(safety_col)) {
  stop(
    "输入表中无法识别 ideal/projection score 或 safety-risk score。\n",
    "selected_path = ", selected_path, "\n",
    "ideal_col = ", ideal_col, "\n",
    "safety_col = ", safety_col
  )
}

if (!"dataset" %in% names(input_dt)) input_dt[, dataset := "unknown_dataset"]
if (!"object_id" %in% names(input_dt)) input_dt[, object_id := paste0(dataset, "_object_unknown")]
if (!"group_id" %in% names(input_dt)) {
  if ("group" %in% names(input_dt)) {
    setnames(input_dt, "group", "group_id")
  } else if ("cluster" %in% names(input_dt)) {
    setnames(input_dt, "cluster", "group_id")
  } else {
    input_dt[, group_id := paste0("group_", seq_len(.N))]
  }
}
if (!"n_cells" %in% names(input_dt)) input_dt[, n_cells := 1L]

input_dt[, n_cells := num(n_cells)]
input_dt[!is.finite(n_cells) | n_cells <= 0, n_cells := 1]

input_dt[, ideal_score_for_threshold := num(get(ideal_col))]
input_dt[, safety_score_for_threshold := num(get(safety_col))]

input_dt <- input_dt[
  is.finite(ideal_score_for_threshold) &
    is.finite(safety_score_for_threshold)
]

if (nrow(input_dt) < 10) {
  stop("有效输入行太少，无法进行 threshold sensitivity：", nrow(input_dt))
}

input_dt[, group_key := paste(dataset, object_id, group_id, sep = "||")]

if (!is.na(baseline_col) && baseline_col %in% names(input_dt)) {
  input_dt[, baseline_class_raw := as.character(get(baseline_col))]
  input_dt[, baseline_class_normalized := normalize_class(baseline_class_raw)]
} else {
  input_dt[, baseline_class_raw := NA_character_]
  input_dt[, baseline_class_normalized := NA_character_]
}

score_column_audit <- data.table(
  selected_input_path = selected_path,
  n_rows = nrow(input_dt),
  n_datasets = uniqueN(input_dt$dataset),
  n_group_keys = uniqueN(input_dt$group_key),
  ideal_score_column = ideal_col,
  safety_score_column = safety_col,
  baseline_label_column = baseline_col,
  ideal_score_min = min(input_dt$ideal_score_for_threshold, na.rm = TRUE),
  ideal_score_median = median(input_dt$ideal_score_for_threshold, na.rm = TRUE),
  ideal_score_max = max(input_dt$ideal_score_for_threshold, na.rm = TRUE),
  safety_score_min = min(input_dt$safety_score_for_threshold, na.rm = TRUE),
  safety_score_median = median(input_dt$safety_score_for_threshold, na.rm = TRUE),
  safety_score_max = max(input_dt$safety_score_for_threshold, na.rm = TRUE)
)

atomic_write_csv(as.data.frame(score_column_audit), selected_input_audit_csv)

stamp("构建 threshold grid。")

diag_dt <- as.data.table(DIAGONAL_SETTINGS)
diag_dt[, setting_type := "diagonal_main"]

full_grid <- CJ(
  ideal_high_q = IDEAL_HIGH_Q_GRID,
  safety_low_q = SAFETY_LOW_Q_GRID,
  safety_high_q = SAFETY_HIGH_Q_GRID
)

full_grid[, setting_id := paste0(
  "I", round(ideal_high_q * 100),
  "_SL", round(safety_low_q * 100),
  "_SH", round(safety_high_q * 100)
)]
full_grid[, setting_type := "full_grid"]

threshold_grid <- rbindlist(
  list(
    diag_dt[, .(setting_id, setting_type, ideal_high_q, safety_low_q, safety_high_q)],
    full_grid[, .(setting_id, setting_type, ideal_high_q, safety_low_q, safety_high_q)]
  ),
  fill = TRUE
)

threshold_grid <- unique(threshold_grid, by = "setting_id")

atomic_write_csv(as.data.frame(threshold_grid), threshold_grid_csv)

stamp("运行 threshold sensitivity classification。")

classification_list <- vector("list", nrow(threshold_grid))
threshold_value_list <- vector("list", nrow(threshold_grid))

for (i in seq_len(nrow(threshold_grid))) {
  setting_row <- threshold_grid[i]

  cls <- classify_by_threshold(
    dt = input_dt,
    setting_row = setting_row,
    ideal_col = ideal_col,
    safety_col = safety_col
  )

  keep_cols <- c(
    "setting_id", "setting_type",
    "ideal_high_q", "safety_low_q", "safety_high_q",
    "ideal_high_threshold", "safety_low_threshold", "safety_high_threshold",
    "dataset", "object_id", "group_id", "group_key", "n_cells",
    "ideal_score_for_threshold", "safety_score_for_threshold",
    "sensitivity_class",
    "baseline_class_raw", "baseline_class_normalized"
  )

  keep_cols <- intersect(keep_cols, names(cls))
  classification_list[[i]] <- cls[, ..keep_cols]

  threshold_value_list[[i]] <- unique(cls[, .(
    setting_id, setting_type,
    ideal_high_q, safety_low_q, safety_high_q,
    ideal_high_threshold, safety_low_threshold, safety_high_threshold
  )])
}

classification_dt <- rbindlist(classification_list, fill = TRUE)
threshold_values <- rbindlist(threshold_value_list, fill = TRUE)

atomic_write_csv(as.data.frame(classification_dt), group_classification_csv)
atomic_write_csv(as.data.frame(threshold_values), threshold_values_csv)

stamp("计算 class fractions / dataset priority stability。")

class_fraction <- classification_dt[
  setting_type == "diagonal_main",
  .(
    n_groups = .N,
    n_cells = sum(n_cells, na.rm = TRUE)
  ),
  by = .(setting_id, ideal_high_q, safety_low_q, safety_high_q, sensitivity_class)
]

class_fraction[
  ,
  group_fraction := n_groups / sum(n_groups),
  by = setting_id
]

class_fraction[
  ,
  cell_fraction := n_cells / sum(n_cells),
  by = setting_id
]

atomic_write_csv(as.data.frame(class_fraction), class_fraction_csv)

dataset_class <- classification_dt[
  setting_type == "diagonal_main",
  .(
    n_groups = .N,
    n_cells = sum(n_cells, na.rm = TRUE)
  ),
  by = .(setting_id, dataset, sensitivity_class)
]

dataset_total <- dataset_class[
  ,
  .(
    dataset_total_groups = sum(n_groups),
    dataset_total_cells = sum(n_cells)
  ),
  by = .(setting_id, dataset)
]

dataset_class <- merge(dataset_class, dataset_total, by = c("setting_id", "dataset"), all.x = TRUE)
dataset_class[, group_fraction := n_groups / dataset_total_groups]
dataset_class[, cell_fraction := n_cells / dataset_total_cells]

dataset_wide <- dcast(
  dataset_class,
  setting_id + dataset + dataset_total_groups + dataset_total_cells ~ sensitivity_class,
  value.var = "group_fraction",
  fill = 0
)

for (needed in c("ideal_like", "safety_risk_like", "lower_priority_or_mixed")) {
  if (!needed %in% names(dataset_wide)) dataset_wide[, (needed) := 0]
}

dataset_wide[, priority_index_group_fraction := ideal_like - safety_risk_like]

dataset_cell_wide <- dcast(
  dataset_class,
  setting_id + dataset ~ sensitivity_class,
  value.var = "cell_fraction",
  fill = 0
)

for (needed in c("ideal_like", "safety_risk_like", "lower_priority_or_mixed")) {
  if (!needed %in% names(dataset_cell_wide)) dataset_cell_wide[, (needed) := 0]
}

dataset_cell_wide[, priority_index_cell_fraction := ideal_like - safety_risk_like]

dataset_priority <- merge(
  dataset_wide,
  dataset_cell_wide[, .(setting_id, dataset, priority_index_cell_fraction)],
  by = c("setting_id", "dataset"),
  all.x = TRUE
)

dataset_priority[
  ,
  priority_rank_group_fraction := frank(-priority_index_group_fraction, ties.method = "dense"),
  by = setting_id
]

dataset_priority[
  ,
  priority_rank_cell_fraction := frank(-priority_index_cell_fraction, ties.method = "dense"),
  by = setting_id
]

atomic_write_csv(as.data.frame(dataset_priority), dataset_priority_csv)

rank_stability <- dataset_priority[
  ,
  .(
    mean_priority_index_group_fraction = mean(priority_index_group_fraction, na.rm = TRUE),
    sd_priority_index_group_fraction = sd(priority_index_group_fraction, na.rm = TRUE),
    min_priority_index_group_fraction = min(priority_index_group_fraction, na.rm = TRUE),
    max_priority_index_group_fraction = max(priority_index_group_fraction, na.rm = TRUE),
    rank_min = min(priority_rank_group_fraction, na.rm = TRUE),
    rank_max = max(priority_rank_group_fraction, na.rm = TRUE),
    rank_range = max(priority_rank_group_fraction, na.rm = TRUE) - min(priority_rank_group_fraction, na.rm = TRUE),
    n_settings = uniqueN(setting_id)
  ),
  by = dataset
][order(rank_range, -mean_priority_index_group_fraction)]

atomic_write_csv(as.data.frame(rank_stability), dataset_rank_stability_csv)

stamp("计算 group-level classification stability。")

diag_cls <- classification_dt[setting_type == "diagonal_main"]

group_class_counts <- diag_cls[
  ,
  .N,
  by = .(group_key, dataset, object_id, group_id, sensitivity_class)
]

group_total <- group_class_counts[, .(n_settings = sum(N)), by = .(group_key, dataset, object_id, group_id)]

group_class_counts <- merge(group_class_counts, group_total, by = c("group_key", "dataset", "object_id", "group_id"))
group_class_counts[, class_fraction_across_settings := N / n_settings]

group_stability <- group_class_counts[
  ,
  .SD[which.max(class_fraction_across_settings)],
  by = .(group_key, dataset, object_id, group_id)
]

setnames(group_stability, "sensitivity_class", "dominant_sensitivity_class")
setnames(group_stability, "class_fraction_across_settings", "dominant_class_fraction")

group_stability[, classification_stability := fifelse(
  dominant_class_fraction >= STABLE_CLASS_FRACTION_CUTOFF,
  "stable",
  "unstable"
)]

baseline_map <- unique(input_dt[, .(group_key, baseline_class_raw, baseline_class_normalized)])
group_stability <- merge(group_stability, baseline_map, by = "group_key", all.x = TRUE)

atomic_write_csv(as.data.frame(group_stability), group_stability_csv)

stamp("计算 baseline label sensitivity audit。")

if (!all(is.na(diag_cls$baseline_class_normalized))) {
  baseline_audit <- diag_cls[
    !is.na(baseline_class_normalized),
    .(
      n_groups = .N,
      n_agree_with_baseline = sum(sensitivity_class == baseline_class_normalized, na.rm = TRUE),
      agreement_fraction = mean(sensitivity_class == baseline_class_normalized, na.rm = TRUE)
    ),
    by = .(setting_id, baseline_class_normalized, sensitivity_class)
  ]

  baseline_overall <- diag_cls[
    !is.na(baseline_class_normalized),
    .(
      n_groups = .N,
      n_agree_with_baseline = sum(sensitivity_class == baseline_class_normalized, na.rm = TRUE),
      agreement_fraction = mean(sensitivity_class == baseline_class_normalized, na.rm = TRUE)
    ),
    by = setting_id
  ]

  baseline_overall[, baseline_class_normalized := "ALL"]
  baseline_overall[, sensitivity_class := "ALL"]

  baseline_audit <- rbindlist(list(baseline_audit, baseline_overall), fill = TRUE)
} else {
  baseline_audit <- data.table(
    setting_id = unique(diag_cls$setting_id),
    baseline_class_normalized = "NO_BASELINE_LABEL_FOUND",
    sensitivity_class = "NO_BASELINE_LABEL_FOUND",
    n_groups = NA_integer_,
    n_agree_with_baseline = NA_integer_,
    agreement_fraction = NA_real_
  )
}

atomic_write_csv(as.data.frame(baseline_audit), baseline_label_audit_csv)

stamp("绘制 09G PDF figures。")

class_plot <- copy(class_fraction)
class_plot[, setting_id := factor(setting_id, levels = DIAGONAL_SETTINGS$setting_id)]
class_plot[, sensitivity_class := factor(
  sensitivity_class,
  levels = c("ideal_like", "lower_priority_or_mixed", "safety_risk_like")
)]

p_class <- ggplot(class_plot, aes(x = setting_id, y = sensitivity_class, fill = group_fraction)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(aes(label = sprintf("%.2f", group_fraction)), size = 3.2) +
  scale_fill_gradient(low = "white", high = "grey30", limits = c(0, 1), name = "Group\nfraction") +
  labs(
    title = "Cell-state class fractions across threshold settings",
    subtitle = "Diagonal threshold settings from relaxed to very strict",
    x = "Threshold setting",
    y = NULL
  ) +
  theme_pub(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_pdf_plot(p_class, fig_class_fraction_pdf, width = 9.8, height = 5.8)

priority_plot <- copy(dataset_priority)
priority_plot[, setting_id := factor(setting_id, levels = DIAGONAL_SETTINGS$setting_id)]
priority_plot[, dataset_label := short_dataset_label(dataset)]

p_priority <- ggplot(
  priority_plot,
  aes(x = setting_id, y = priority_index_group_fraction, group = dataset_label, color = dataset_label)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.35, color = "grey45") +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  geom_point(size = 2.0) +
  labs(
    title = "Dataset priority index stability across thresholds",
    subtitle = "Priority index = ideal-like fraction − safety-risk-like fraction",
    x = "Threshold setting",
    y = "Priority index",
    color = "Dataset"
  ) +
  theme_pub(base_size = 10.5) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_pdf_plot(p_priority, fig_dataset_priority_pdf, width = 11.5, height = 6.8)

rank_plot <- copy(dataset_priority)
rank_plot[, setting_id := factor(setting_id, levels = DIAGONAL_SETTINGS$setting_id)]
rank_plot[, dataset_label := short_dataset_label(dataset)]

dataset_order <- rank_plot[
  ,
  .(mean_rank = mean(priority_rank_group_fraction, na.rm = TRUE)),
  by = dataset_label
][order(mean_rank)]$dataset_label

rank_plot[, dataset_label := factor(dataset_label, levels = rev(dataset_order))]

p_rank <- ggplot(rank_plot, aes(x = setting_id, y = dataset_label, fill = priority_rank_group_fraction)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(aes(label = priority_rank_group_fraction), size = 3.0) +
  scale_fill_gradient(low = "grey20", high = "grey90", name = "Priority\nrank") +
  labs(
    title = "Dataset priority-rank stability across threshold settings",
    x = "Threshold setting",
    y = NULL
  ) +
  theme_pub(base_size = 10.5) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_pdf_plot(p_rank, fig_dataset_rank_pdf, width = 10.8, height = max(6.0, 0.35 * uniqueN(rank_plot$dataset_label) + 3.0))

stability_plot <- group_stability[
  ,
  .N,
  by = .(classification_stability, dominant_sensitivity_class)
]

p_group_stability <- ggplot(
  stability_plot,
  aes(x = dominant_sensitivity_class, y = N, fill = classification_stability)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, color = "grey25", linewidth = 0.2) +
  labs(
    title = "Group-level class stability across threshold settings",
    subtitle = paste0("Stable if dominant class fraction ≥ ", STABLE_CLASS_FRACTION_CUTOFF),
    x = "Dominant sensitivity class",
    y = "Number of groups",
    fill = "Stability"
  ) +
  theme_pub(base_size = 10.5) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

save_pdf_plot(p_group_stability, fig_group_stability_pdf, width = 9.5, height = 6.4)

if (any(baseline_audit$baseline_class_normalized == "ALL", na.rm = TRUE)) {
  baseline_plot <- baseline_audit[baseline_class_normalized == "ALL"]
  baseline_plot[, setting_id := factor(setting_id, levels = DIAGONAL_SETTINGS$setting_id)]

  p_baseline <- ggplot(baseline_plot, aes(x = setting_id, y = agreement_fraction)) +
    geom_col(width = 0.68, fill = "grey55", color = "grey25", linewidth = 0.25) +
    geom_text(aes(label = sprintf("%.2f", agreement_fraction)), vjust = -0.25, size = 3.2) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "Agreement between baseline labels and threshold-derived classes",
      x = "Threshold setting",
      y = "Agreement fraction"
    ) +
    theme_pub(base_size = 10.5) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
} else {
  p_baseline <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "No baseline class column detected.", size = 5) +
    theme_void() +
    labs(title = "Baseline label agreement")
}

save_pdf_plot(p_baseline, fig_baseline_agreement_pdf, width = 9.5, height = 5.8)

thr_plot <- threshold_values[setting_type == "diagonal_main"]
thr_plot[, setting_id := factor(setting_id, levels = DIAGONAL_SETTINGS$setting_id)]

thr_long <- melt(
  thr_plot,
  id.vars = "setting_id",
  measure.vars = c("ideal_high_threshold", "safety_low_threshold", "safety_high_threshold"),
  variable.name = "threshold_type",
  value.name = "threshold_value",
  variable.factor = FALSE,
  value.factor = FALSE
)

thr_long[, threshold_type := factor(
  threshold_type,
  levels = c("ideal_high_threshold", "safety_low_threshold", "safety_high_threshold"),
  labels = c("Ideal high", "Safety low", "Safety high")
)]

p_thr <- ggplot(thr_long, aes(x = setting_id, y = threshold_value, group = threshold_type, color = threshold_type)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.1) +
  labs(
    title = "Threshold values across sensitivity settings",
    x = "Threshold setting",
    y = "Score threshold",
    color = "Threshold"
  ) +
  theme_pub(base_size = 10.5) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_pdf_plot(p_thr, fig_threshold_summary_pdf, width = 9.8, height = 5.8)

stamp("写出 09G key findings / method note / report。")

overall_stable_fraction <- mean(group_stability$classification_stability == "stable", na.rm = TRUE)

n_stable <- sum(group_stability$classification_stability == "stable", na.rm = TRUE)
n_unstable <- sum(group_stability$classification_stability == "unstable", na.rm = TRUE)

rank_summary_line <- paste0(
  "Datasets with rank_range <= 1: ",
  sum(rank_stability$rank_range <= 1, na.rm = TRUE),
  " / ",
  nrow(rank_stability)
)

if (any(baseline_audit$baseline_class_normalized == "ALL", na.rm = TRUE)) {
  baseline_mean_agreement <- mean(
    baseline_audit[baseline_class_normalized == "ALL"]$agreement_fraction,
    na.rm = TRUE
  )
} else {
  baseline_mean_agreement <- NA_real_
}

key_findings <- data.table(
  item = c(
    "selected_input_path",
    "ideal_score_column",
    "safety_score_column",
    "baseline_label_column",
    "n_groups",
    "n_datasets",
    "diagonal_settings",
    "full_grid_settings",
    "stable_group_fraction",
    "stable_groups",
    "unstable_groups",
    "rank_stability_summary",
    "mean_baseline_agreement",
    "claim_boundary"
  ),
  value = c(
    selected_path,
    ideal_col,
    safety_col,
    ifelse(is.na(baseline_col), "none_detected", baseline_col),
    as.character(uniqueN(input_dt$group_key)),
    as.character(uniqueN(input_dt$dataset)),
    as.character(nrow(DIAGONAL_SETTINGS)),
    as.character(nrow(full_grid)),
    signif(overall_stable_fraction, 4),
    as.character(n_stable),
    as.character(n_unstable),
    rank_summary_line,
    ifelse(is.na(baseline_mean_agreement), "not_available", signif(baseline_mean_agreement, 4)),
    "Threshold sensitivity evaluates robustness of transcriptomic class assignment only; it does not validate clinical safety, therapeutic efficacy, anatomical projection, or graft outcome."
  )
)

atomic_write_csv(as.data.frame(key_findings), key_findings_csv)

method_lines <- c(
  "09G threshold sensitivity analysis method and claim-boundary note",
  "",
  "Purpose:",
  "09G evaluates whether ideal-like and safety-risk-associated cell-state assignments are robust to threshold choices.",
  "",
  "Input:",
  paste0("Selected input table: ", selected_path),
  paste0("Ideal/projection score column: ", ideal_col),
  paste0("Safety-risk score column: ", safety_col),
  paste0("Baseline label column: ", ifelse(is.na(baseline_col), "none detected", baseline_col)),
  "",
  "Method:",
  "Multiple quantile-based thresholds were applied to the continuous ideal/projection score and safety-risk score.",
  "A group was classified as ideal-like only when ideal/projection score was high and safety-risk score was low.",
  "A group was classified as safety-risk-like when the safety-risk score was high.",
  "All remaining groups were assigned to lower-priority-or-mixed.",
  "The diagonal settings were used for main plots, while the full grid was saved for audit.",
  "",
  "Claim boundary:",
  "This is a robustness analysis of threshold-derived transcriptomic prioritization labels.",
  "It does not modify the frozen labels used in previous modules.",
  "It does not prove anatomical projection, functional integration, clinical safety, tumorigenicity, or therapeutic efficacy."
)

writeLines(method_lines, method_note_txt)

report_lines <- c(
  "09G threshold sensitivity analysis report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Selected input:",
  selected_path,
  "",
  "Score column audit:",
  capture.output(print(score_column_audit)),
  "",
  "Key findings:",
  capture.output(print(key_findings)),
  "",
  "Rank stability:",
  capture.output(print(rank_stability)),
  "",
  "Group stability summary:",
  capture.output(print(group_stability[, .N, by = .(classification_stability, dominant_sensitivity_class)])),
  "",
  "Baseline label audit:",
  capture.output(print(baseline_audit)),
  "",
  "Output directories:",
  out_tables_dir,
  out_figures_dir
)

writeLines(report_lines, report_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

required_outputs <- c(
  input_candidate_audit_csv,
  selected_input_audit_csv,
  threshold_grid_csv,
  threshold_values_csv,
  group_classification_csv,
  class_fraction_csv,
  dataset_priority_csv,
  dataset_rank_stability_csv,
  group_stability_csv,
  baseline_label_audit_csv,
  key_findings_csv,
  method_note_txt,
  session_info_txt,
  report_txt,
  fig_class_fraction_pdf,
  fig_dataset_priority_pdf,
  fig_dataset_rank_pdf,
  fig_group_stability_pdf,
  fig_baseline_agreement_pdf,
  fig_threshold_summary_pdf
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
  stop("09G 输出验证失败。")
}

cat("\n============================================================\n")
cat("09G threshold sensitivity analysis FINAL V1 运行结束\n")
cat("============================================================\n\n")

cat("Selected input table：", selected_path, "\n")
cat("Ideal score column：", ideal_col, "\n")
cat("Safety score column：", safety_col, "\n")
cat("Baseline label column：", ifelse(is.na(baseline_col), "none_detected", baseline_col), "\n")
cat("Groups：", uniqueN(input_dt$group_key), "\n")
cat("Datasets：", uniqueN(input_dt$dataset), "\n")
cat("Diagonal threshold settings：", nrow(DIAGONAL_SETTINGS), "\n")
cat("Full grid settings：", nrow(full_grid), "\n")
cat("Stable group fraction：", signif(overall_stable_fraction, 4), "\n")
cat("Stable groups：", n_stable, "\n")
cat("Unstable groups：", n_unstable, "\n\n")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("关键输出：\n")
cat(selected_input_audit_csv, "\n")
cat(threshold_grid_csv, "\n")
cat(group_classification_csv, "\n")
cat(class_fraction_csv, "\n")
cat(dataset_priority_csv, "\n")
cat(dataset_rank_stability_csv, "\n")
cat(group_stability_csv, "\n")
cat(baseline_label_audit_csv, "\n")
cat(method_note_txt, "\n\n")

cat("PDF 图：\n")
cat(fig_class_fraction_pdf, "\n")
cat(fig_dataset_priority_pdf, "\n")
cat(fig_dataset_rank_pdf, "\n")
cat(fig_group_stability_pdf, "\n")
cat(fig_baseline_agreement_pdf, "\n")
cat(fig_threshold_summary_pdf, "\n\n")

cat("✅ 09G threshold sensitivity analysis FINAL V1 完成。\n")
