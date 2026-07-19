# ============================================================
# 06A_FIGURE_TABLE_PREP_DA_PROJECTION_SAFETY.R
# ============================================================
# 目的：
#   接在 05B 后运行。
#
#   06A 不做新的生物学计算，主要做“论文图表输入整理”：
#     1. 整理 DA/projection/safety 的 dataset-level summary
#     2. 整理 A9/A10 bias summary
#     3. 整理 ideal-like / risk-like / mixed candidate groups
#     4. 生成基础 publication-style figures，用于快速检查故事线
#     5. 输出 manuscript key numbers
#
# 重要：
#   06A 的图是“论文图表草稿输入”，不是最终排版图。
#   最终 Figure 还要在 06B/06C 美化。
#
# 成功标志：
#   ✅ 06A figure/table preparation 完成。
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"

MAKE_FIGURES <- TRUE
TOP_N_STORY_GROUPS <- 100


# ============================================================
# 1. 加载包
# ============================================================

cat("\n============================================================\n")
cat("06A：figure/table preparation for DA/projection/safety story\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 06A。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

if (MAKE_FIGURES && !has_ggplot2) {
  warning("未检测到 ggplot2；06A 会输出表格，但跳过图。")
  MAKE_FIGURES <- FALSE
}

if (MAKE_FIGURES) {
  suppressPackageStartupMessages(library(ggplot2))
}


# ============================================================
# 2. 路径
# ============================================================

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_05A_group <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_group_level_scores.csv")
input_05A_object <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_object_level_scores.csv")
input_05A_candidates <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_DA_A9_A10_projection_candidate_groups.csv")
input_05A_audit <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_V2_final_audit_summary.csv")

input_05B_group <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_group_safety_risk_scores.csv")
input_05B_object <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_object_safety_risk_scores.csv")
input_05B_dataset <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_dataset_safety_risk_summary.csv")
input_05B_contrast <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_DA_projection_vs_safety_contrast_groups.csv")
input_05B_story <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_candidate_groups_for_story.csv")
input_05B_qc <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_QC_audit_summary.csv")

out_tables_dir <- file.path(tables_dir, "06A_figure_table_prep")
out_figures_dir <- file.path(figures_dir, "06A_figure_table_prep")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

dataset_overview_csv <- file.path(out_tables_dir, "06A_dataset_overview_DA_projection_safety.csv")
a9_a10_summary_csv <- file.path(out_tables_dir, "06A_A9_A10_bias_summary_by_dataset.csv")
candidate_class_summary_csv <- file.path(out_tables_dir, "06A_candidate_class_summary_by_dataset.csv")
story_groups_csv <- file.path(out_tables_dir, "06A_top_story_candidate_groups.csv")
object_summary_csv <- file.path(out_tables_dir, "06A_object_level_DA_projection_safety_summary.csv")
manuscript_numbers_csv <- file.path(out_tables_dir, "06A_manuscript_key_numbers.csv")
qc_summary_csv <- file.path(out_tables_dir, "06A_QC_summary_for_methods.csv")
report_txt <- file.path(reports_dir, "06A_figure_table_preparation_report.txt")


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

save_plot_both <- function(p, filename_base, width = 7, height = 5) {
  if (!MAKE_FIGURES) return(invisible(NULL))

  pdf_path <- file.path(out_figures_dir, paste0(filename_base, ".pdf"))
  png_path <- file.path(out_figures_dir, paste0(filename_base, ".png"))

  tryCatch({
    ggplot2::ggsave(pdf_path, p, width = width, height = height, limitsize = FALSE)
    ggplot2::ggsave(png_path, p, width = width, height = height, dpi = 300, limitsize = FALSE)
  }, error = function(e) {
    warning("保存图失败：", filename_base, "；", conditionMessage(e))
  })

  invisible(NULL)
}

num <- function(x) suppressWarnings(as.numeric(x))


# ============================================================
# 4. 读取输入
# ============================================================

stamp("读取 05A / 05B 输出。")

g05A <- as.data.table(read_csv_required(input_05A_group))
o05A <- as.data.table(read_csv_required(input_05A_object))
c05A <- as.data.table(read_csv_optional(input_05A_candidates))
audit05A <- as.data.table(read_csv_optional(input_05A_audit))

g05B <- as.data.table(read_csv_required(input_05B_group))
o05B <- as.data.table(read_csv_required(input_05B_object))
d05B <- as.data.table(read_csv_required(input_05B_dataset))
contrast <- as.data.table(read_csv_required(input_05B_contrast))
story <- as.data.table(read_csv_required(input_05B_story))
qc05B <- as.data.table(read_csv_optional(input_05B_qc))


# ============================================================
# 5. Dataset overview table
# ============================================================

stamp("整理 dataset-level overview。")

dataset_overview <- copy(d05B)

# 保证关键列存在
needed_dataset_cols <- c(
  "dataset",
  "n_objects",
  "total_cells_represented",
  "mean_safety_risk_composite_05B",
  "mean_DA_projection_competence",
  "mean_DA_like",
  "mean_projection_competence",
  "dataset_story_class_05B"
)

for (col in needed_dataset_cols) {
  if (!col %in% colnames(dataset_overview)) dataset_overview[[col]] <- NA
}

dataset_overview <- dataset_overview[
  ,
  needed_dataset_cols,
  with = FALSE
]

dataset_overview[
  ,
  favorable_index_06A := num(mean_DA_projection_competence) - num(mean_safety_risk_composite_05B)
]

dataset_overview <- dataset_overview[
  order(-favorable_index_06A)
]

atomic_write_csv(as.data.frame(dataset_overview), dataset_overview_csv)


# ============================================================
# 6. A9/A10 bias summary
# ============================================================

stamp("整理 A9/A10 bias summary。")

if ("A9_A10_bias_label_05B" %in% colnames(contrast)) {
  a9_a10_summary <- contrast[
    ,
    .(
      n_groups = .N,
      total_cells = sum(n_cells_05B, na.rm = TRUE),
      median_A9_minus_A10 = median(A9_minus_A10_score_05A, na.rm = TRUE),
      median_DA_like = median(DA_like_composite_score, na.rm = TRUE),
      median_projection_competence = median(projection_competence_composite_score, na.rm = TRUE),
      median_safety_risk = median(safety_risk_composite_05B, na.rm = TRUE)
    ),
    by = .(dataset, A9_A10_bias_label_05B)
  ][order(dataset, A9_A10_bias_label_05B)]
} else {
  a9_a10_summary <- data.table()
}

atomic_write_csv(as.data.frame(a9_a10_summary), a9_a10_summary_csv)


# ============================================================
# 7. Candidate class summary
# ============================================================

stamp("整理 candidate class summary。")

if ("safety_contrast_class_05B" %in% colnames(contrast)) {
  candidate_class_summary <- contrast[
    ,
    .(
      n_groups = .N,
      total_cells = sum(n_cells_05B, na.rm = TRUE),
      median_DA_projection = median(DA_projection_competence_composite_score, na.rm = TRUE),
      median_DA_like = median(DA_like_composite_score, na.rm = TRUE),
      median_projection_competence = median(projection_competence_composite_score, na.rm = TRUE),
      median_safety_risk = median(safety_risk_composite_05B, na.rm = TRUE),
      median_A9_minus_A10 = median(A9_minus_A10_score_05A, na.rm = TRUE)
    ),
    by = .(dataset, safety_contrast_class_05B)
  ][order(dataset, safety_contrast_class_05B)]
} else {
  candidate_class_summary <- data.table()
}

atomic_write_csv(as.data.frame(candidate_class_summary), candidate_class_summary_csv)


# ============================================================
# 8. Top story candidate groups
# ============================================================

stamp("整理 top story candidate groups。")

story2 <- copy(story)

if (nrow(story2) > 0) {
  # 添加 story rank
  story2[
    ,
    story_rank_score_06A := num(DA_projection_competence_composite_score) -
      num(safety_risk_composite_05B)
  ]

  story2[
    safety_contrast_class_05B %in% c("high_safety_risk_low_DA", "mixed_DA_or_projection_with_safety_risk"),
    story_rank_score_06A := num(safety_risk_composite_05B)
  ]

  story2 <- story2[
    order(dataset, safety_contrast_class_05B, -story_rank_score_06A)
  ]

  story2 <- story2[
    ,
    head(.SD, TOP_N_STORY_GROUPS),
    by = .(dataset, safety_contrast_class_05B)
  ]
}

atomic_write_csv(as.data.frame(story2), story_groups_csv)


# ============================================================
# 9. Object-level summary
# ============================================================

stamp("整理 object-level summary。")

object_summary <- copy(o05B)

object_keep <- intersect(
  c(
    "dataset", "object_id",
    "total_cells_represented",
    "mean_safety_risk_composite_05B",
    "median_safety_risk_composite_05B",
    "max_safety_risk_composite_05B",
    "n_high_safety_groups",
    "n_low_safety_groups",
    "DA_like_composite_score",
    "projection_competence_composite_score",
    "DA_projection_competence_composite_score",
    "A9_minus_A10_score_05A",
    "object_safety_contrast_class_05B",
    "dominant_annotation"
  ),
  colnames(object_summary)
)

object_summary <- object_summary[, object_keep, with = FALSE]

atomic_write_csv(as.data.frame(object_summary), object_summary_csv)


# ============================================================
# 10. Manuscript key numbers
# ============================================================

stamp("生成 manuscript key numbers。")

n_scored_objects <- length(unique(paste(o05A$dataset, o05A$object_id, sep = "||")))
n_scored_cells <- if ("n_cells" %in% colnames(o05A)) sum(o05A$n_cells, na.rm = TRUE) else NA_real_
n_group_scores <- nrow(g05A)
n_safety_groups <- nrow(g05B)
n_contrast_groups <- nrow(contrast)
n_story_groups <- nrow(story2)

n_ideal_groups <- if ("safety_contrast_class_05B" %in% colnames(contrast)) {
  sum(contrast$safety_contrast_class_05B == "ideal_DA_projection_high_safety_low", na.rm = TRUE)
} else NA_integer_

n_high_risk_groups <- if ("safety_contrast_class_05B" %in% colnames(contrast)) {
  sum(contrast$safety_contrast_class_05B == "high_safety_risk_low_DA", na.rm = TRUE)
} else NA_integer_

n_mixed_groups <- if ("safety_contrast_class_05B" %in% colnames(contrast)) {
  sum(contrast$safety_contrast_class_05B == "mixed_DA_or_projection_with_safety_risk", na.rm = TRUE)
} else NA_integer_

manuscript_numbers <- data.frame(
  metric = c(
    "successfully_scored_objects_for_05A_05B",
    "successfully_scored_cells_for_05A",
    "group_level_DA_projection_score_rows",
    "group_level_safety_score_rows",
    "DA_projection_vs_safety_contrast_groups",
    "story_candidate_groups",
    "ideal_DA_projection_high_safety_low_groups",
    "high_safety_risk_low_DA_groups",
    "mixed_DA_or_projection_with_safety_risk_groups",
    "datasets_in_05B_summary"
  ),
  value = c(
    n_scored_objects,
    n_scored_cells,
    n_group_scores,
    n_safety_groups,
    n_contrast_groups,
    n_story_groups,
    n_ideal_groups,
    n_high_risk_groups,
    n_mixed_groups,
    nrow(dataset_overview)
  ),
  interpretation = c(
    "Objects used for downstream DA/projection/safety scoring.",
    "Cells represented in 05A cell-level score table.",
    "Group-level DA/A9/A10/projection score rows.",
    "Group-level safety-risk score rows.",
    "Merged group-level contrast rows.",
    "High-priority groups selected for story review.",
    "Groups with high DA/projection competence and low safety-risk score.",
    "Groups with high safety-risk score and low DA signal.",
    "Groups with DA/projection signal and concurrent safety-risk signal.",
    "Datasets represented in dataset-level 05B summary."
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(manuscript_numbers, manuscript_numbers_csv)


# ============================================================
# 11. QC summary
# ============================================================

stamp("整理 QC summary。")

qc_list <- list()

if (nrow(audit05A) > 0) {
  audit05A$source <- "05A_V2"
  qc_list[[length(qc_list) + 1L]] <- audit05A
}

if (nrow(qc05B) > 0) {
  qc05B$source <- "05B"
  qc_list[[length(qc_list) + 1L]] <- qc05B
}

qc_summary <- if (length(qc_list) > 0) {
  rbindlist(qc_list, fill = TRUE)
} else {
  data.table()
}

atomic_write_csv(as.data.frame(qc_summary), qc_summary_csv)


# ============================================================
# 12. 快速草图
# ============================================================

if (MAKE_FIGURES) {
  stamp("生成 06A 快速检查图。")

  # 1. dataset scatter: DA/projection vs safety
  p1 <- ggplot(dataset_overview, aes(
    x = mean_safety_risk_composite_05B,
    y = mean_DA_projection_competence,
    label = dataset
  )) +
    geom_point(size = 3) +
    geom_text(vjust = -0.7, size = 3) +
    labs(
      title = "Dataset-level DA/projection competence vs safety-risk score",
      x = "Mean safety-risk-associated score",
      y = "Mean DA/projection competence score"
    ) +
    theme_classic(base_size = 12)

  save_plot_both(p1, "06A_dataset_DA_projection_vs_safety_scatter", width = 7, height = 5)

  # 2. dataset safety barplot
  p2 <- ggplot(dataset_overview, aes(
    x = reorder(dataset, mean_safety_risk_composite_05B),
    y = mean_safety_risk_composite_05B
  )) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Dataset-level safety-risk-associated score",
      x = "Dataset",
      y = "Mean safety-risk score"
    ) +
    theme_classic(base_size = 12)

  save_plot_both(p2, "06A_dataset_safety_risk_barplot", width = 7, height = 5)

  # 3. dataset DA/projection barplot
  p3 <- ggplot(dataset_overview, aes(
    x = reorder(dataset, mean_DA_projection_competence),
    y = mean_DA_projection_competence
  )) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Dataset-level DA/projection competence score",
      x = "Dataset",
      y = "Mean DA/projection competence score"
    ) +
    theme_classic(base_size = 12)

  save_plot_both(p3, "06A_dataset_DA_projection_barplot", width = 7, height = 5)

  # 4. candidate class grouped barplot
  if (nrow(candidate_class_summary) > 0) {
    p4 <- ggplot(candidate_class_summary, aes(
      x = dataset,
      y = n_groups,
      fill = safety_contrast_class_05B
    )) +
      geom_col(position = "stack") +
      labs(
        title = "Candidate classes by dataset",
        x = "Dataset",
        y = "Number of groups",
        fill = "05B class"
      ) +
      theme_classic(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    save_plot_both(p4, "06A_candidate_class_by_dataset_barplot", width = 9, height = 5)
  }

  # 5. A9/A10 bias barplot
  if (nrow(a9_a10_summary) > 0) {
    p5 <- ggplot(a9_a10_summary, aes(
      x = dataset,
      y = n_groups,
      fill = A9_A10_bias_label_05B
    )) +
      geom_col(position = "stack") +
      labs(
        title = "A9/A10 molecular bias groups by dataset",
        x = "Dataset",
        y = "Number of groups",
        fill = "A9/A10 bias"
      ) +
      theme_classic(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    save_plot_both(p5, "06A_A9_A10_bias_by_dataset_barplot", width = 9, height = 5)
  }
}


# ============================================================
# 13. 报告
# ============================================================

dataset_lines <- if (nrow(dataset_overview) > 0) {
  apply(as.data.frame(dataset_overview), 1, function(x) {
    paste0(
      x[["dataset"]],
      ": DA_projection=",
      round(as.numeric(x[["mean_DA_projection_competence"]]), 4),
      "; safety=",
      round(as.numeric(x[["mean_safety_risk_composite_05B"]]), 4),
      "; favorable_index=",
      round(as.numeric(x[["favorable_index_06A"]]), 4),
      "; class=",
      x[["dataset_story_class_05B"]]
    )
  })
} else {
  "none"
}

number_lines <- apply(manuscript_numbers, 1, function(x) {
  paste0(x[["metric"]], ": ", x[["value"]])
})

report_lines <- c(
  "06A figure/table preparation report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Manuscript key numbers:",
  number_lines,
  "",
  "Dataset overview:",
  dataset_lines,
  "",
  "Output tables:",
  paste0("Dataset overview: ", dataset_overview_csv),
  paste0("A9/A10 summary: ", a9_a10_summary_csv),
  paste0("Candidate class summary: ", candidate_class_summary_csv),
  paste0("Top story groups: ", story_groups_csv),
  paste0("Object summary: ", object_summary_csv),
  paste0("Manuscript key numbers: ", manuscript_numbers_csv),
  paste0("QC summary: ", qc_summary_csv),
  "",
  "Output figures:",
  out_figures_dir,
  "",
  "Next step:",
  "06B_PUBLICATION_FIGURE_DRAFTS.R",
  "",
  "Journal-rigor note:",
  "06A figures are quick inspection drafts. Final manuscript claims should avoid saying real projection or proven safety; use projection-associated molecular competence and safety-risk-associated transcriptional state."
)

writeLines(report_lines, report_txt)


# ============================================================
# 14. 结束
# ============================================================

cat("\n============================================================\n")
cat("06A figure/table preparation 运行结束\n")
cat("============================================================\n\n")

cat("Dataset overview rows：", nrow(dataset_overview), "\n")
cat("A9/A10 summary rows：", nrow(a9_a10_summary), "\n")
cat("Candidate class summary rows：", nrow(candidate_class_summary), "\n")
cat("Top story groups rows：", nrow(story2), "\n")
cat("Object summary rows：", nrow(object_summary), "\n")
cat("Manuscript key numbers rows：", nrow(manuscript_numbers), "\n\n")

cat("输出表格：\n")
cat(dataset_overview_csv, "\n")
cat(a9_a10_summary_csv, "\n")
cat(candidate_class_summary_csv, "\n")
cat(story_groups_csv, "\n")
cat(object_summary_csv, "\n")
cat(manuscript_numbers_csv, "\n")
cat(qc_summary_csv, "\n\n")

if (MAKE_FIGURES) {
  cat("输出图片目录：\n")
  cat(out_figures_dir, "\n\n")
}

cat("✅ 06A figure/table preparation 完成。\n")
cat("下一步进入 06B：publication figure drafts。\n")
