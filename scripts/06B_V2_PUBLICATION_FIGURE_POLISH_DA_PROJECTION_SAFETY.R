
PROJECT_DIR <- "D:/PD_Graft_Project"

TOP_N_GROUPS_FOR_TILE <- 35

SAVE_PDF <- TRUE
SAVE_PNG <- TRUE

cat("\n============================================================\n")
cat("06B V2：publication figure polish\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table", "ggplot2")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 06B V2。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_dataset <- file.path(tables_dir, "06A_figure_table_prep", "06A_dataset_overview_DA_projection_safety.csv")
input_a9a10 <- file.path(tables_dir, "06A_figure_table_prep", "06A_A9_A10_bias_summary_by_dataset.csv")
input_class <- file.path(tables_dir, "06A_figure_table_prep", "06A_candidate_class_summary_by_dataset.csv")
input_story <- file.path(tables_dir, "06A_figure_table_prep", "06A_top_story_candidate_groups.csv")
input_numbers <- file.path(tables_dir, "06A_figure_table_prep", "06A_manuscript_key_numbers.csv")

out_tables_dir <- file.path(tables_dir, "06B_publication_figure_drafts_V2")
out_figures_dir <- file.path(figures_dir, "06B_publication_figure_drafts_V2")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

figure_index_csv <- file.path(out_tables_dir, "06B_V2_figure_index.csv")
dataset_plot_table_csv <- file.path(out_tables_dir, "06B_V2_dataset_plot_table.csv")
candidate_class_prop_csv <- file.path(out_tables_dir, "06B_V2_candidate_class_proportion_table.csv")
a9a10_prop_csv <- file.path(out_tables_dir, "06B_V2_A9_A10_proportion_table.csv")
top_tile_table_csv <- file.path(out_tables_dir, "06B_V2_top_story_group_tile_table.csv")
report_txt <- file.path(reports_dir, "06B_V2_publication_figure_polish_report.txt")

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

save_plot <- function(p, filename_base, width = 7, height = 5) {
  pdf_path <- file.path(out_figures_dir, paste0(filename_base, ".pdf"))
  png_path <- file.path(out_figures_dir, paste0(filename_base, ".png"))

  if (SAVE_PDF) {
    ggsave(pdf_path, p, width = width, height = height, limitsize = FALSE)
  }

  if (SAVE_PNG) {
    ggsave(png_path, p, width = width, height = height, dpi = 300, limitsize = FALSE)
  }

  data.frame(
    figure_id = filename_base,
    pdf_path = ifelse(SAVE_PDF, pdf_path, NA_character_),
    png_path = ifelse(SAVE_PNG, png_path, NA_character_),
    stringsAsFactors = FALSE
  )
}

num <- function(x) suppressWarnings(as.numeric(x))

short_dataset_label <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "GSE178265_DA_01B", "DA reference\nGSE178265", x)
  x <- ifelse(x == "GSE233885", "GSE233885", x)
  x <- ifelse(x == "GSE204796", "GSE204796", x)
  x <- ifelse(x == "GSE132758", "GSE132758", x)
  x <- ifelse(x == "GSE200610", "GSE200610", x)
  x
}

pretty_class <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "ideal_DA_projection_high_safety_low", "Ideal-like\nDA/proj high\nSafety low", x)
  x <- ifelse(x == "high_safety_risk_low_DA", "High safety-risk\nLow DA", x)
  x <- ifelse(x == "mixed_DA_or_projection_with_safety_risk", "Mixed\nDA/proj + risk", x)
  x <- ifelse(x == "projection_competence_without_DA_low_safety", "Projection-like\nDA low\nSafety low", x)
  x <- ifelse(x == "lower_priority_or_mixed", "Lower priority\nor mixed", x)
  x
}

pretty_bias <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "A9_like_bias", "A9-like", x)
  x <- ifelse(x == "A10_like_bias", "A10-like", x)
  x <- ifelse(x == "A9_A10_mixed_or_unclear", "Mixed/unclear", x)
  x <- ifelse(x == "unknown", "Unknown", x)
  x
}

pretty_score_type <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "DA_like_composite_score", "DA-like\nscore", x)
  x <- ifelse(x == "projection_competence_composite_score", "Projection\ncompetence", x)
  x <- ifelse(x == "safety_risk_composite_05B", "Safety-risk\nscore", x)
  x <- ifelse(x == "A9_minus_A10_score_05A", "A9−A10\nbias", x)
  x
}

stamp("读取 06A 表格。")

dataset_dt <- as.data.table(read_csv_required(input_dataset))
a9a10_dt <- as.data.table(read_csv_required(input_a9a10))
class_dt <- as.data.table(read_csv_required(input_class))
story_dt <- as.data.table(read_csv_required(input_story))
numbers_dt <- as.data.table(read_csv_optional(input_numbers))

stamp("整理 V2 plot tables。")

dataset_plot <- copy(dataset_dt)

required_dataset_cols <- c(
  "dataset",
  "mean_safety_risk_composite_05B",
  "mean_DA_projection_competence",
  "mean_DA_like",
  "mean_projection_competence",
  "favorable_index_06A",
  "dataset_story_class_05B"
)

for (col in required_dataset_cols) {
  if (!col %in% colnames(dataset_plot)) dataset_plot[[col]] <- NA
}

dataset_plot[, dataset_short := short_dataset_label(dataset)]

dataset_plot[
  ,
  dataset_order := factor(dataset_short, levels = dataset_short[order(favorable_index_06A)])
]

dataset_plot[
  ,
  story_quadrant_06B := fifelse(
    mean_DA_projection_competence >= median(mean_DA_projection_competence, na.rm = TRUE) &
      mean_safety_risk_composite_05B <= median(mean_safety_risk_composite_05B, na.rm = TRUE),
    "High DA/proj\nLow risk",
    fifelse(
      mean_DA_projection_competence < median(mean_DA_projection_competence, na.rm = TRUE) &
        mean_safety_risk_composite_05B > median(mean_safety_risk_composite_05B, na.rm = TRUE),
      "Low DA/proj\nHigh risk",
      "Intermediate\nor mixed"
    )
  )
]

atomic_write_csv(as.data.frame(dataset_plot), dataset_plot_table_csv)

class_prop <- copy(class_dt)

if (nrow(class_prop) > 0) {
  class_prop[, dataset_short := short_dataset_label(dataset)]
  class_prop[, class_short := pretty_class(safety_contrast_class_05B)]

  class_prop[
    ,
    total_groups_dataset := sum(n_groups, na.rm = TRUE),
    by = dataset
  ]

  class_prop[
    ,
    group_fraction := n_groups / total_groups_dataset
  ]

  class_prop[
    ,
    dataset_short := factor(dataset_short, levels = dataset_plot$dataset_short[order(dataset_plot$favorable_index_06A)])
  ]

  class_levels <- c(
    "Ideal-like\nDA/proj high\nSafety low",
    "Mixed\nDA/proj + risk",
    "High safety-risk\nLow DA",
    "Projection-like\nDA low\nSafety low",
    "Lower priority\nor mixed"
  )

  class_prop[
    ,
    class_short := factor(class_short, levels = class_levels)
  ]
}

atomic_write_csv(as.data.frame(class_prop), candidate_class_prop_csv)

a9a10_prop <- copy(a9a10_dt)

if (nrow(a9a10_prop) > 0) {
  a9a10_prop[, dataset_short := short_dataset_label(dataset)]
  a9a10_prop[, bias_short := pretty_bias(A9_A10_bias_label_05B)]

  a9a10_prop[
    ,
    total_groups_dataset := sum(n_groups, na.rm = TRUE),
    by = dataset
  ]

  a9a10_prop[
    ,
    group_fraction := n_groups / total_groups_dataset
  ]

  a9a10_prop[
    ,
    dataset_short := factor(dataset_short, levels = dataset_plot$dataset_short[order(dataset_plot$favorable_index_06A)])
  ]

  a9a10_prop[
    ,
    bias_short := factor(bias_short, levels = c("A9-like", "Mixed/unclear", "A10-like", "Unknown"))
  ]
}

atomic_write_csv(as.data.frame(a9a10_prop), a9a10_prop_csv)

tile_dt <- copy(story_dt)

if (nrow(tile_dt) > 0) {
  for (col in c(
    "DA_like_composite_score",
    "projection_competence_composite_score",
    "safety_risk_composite_05B",
    "A9_minus_A10_score_05A",
    "DA_projection_competence_composite_score"
  )) {
    if (!col %in% colnames(tile_dt)) tile_dt[[col]] <- NA_real_
  }

  tile_dt[, class_short := pretty_class(safety_contrast_class_05B)]

  tile_dt[
    ,
    story_rank_score_06B := fifelse(
      safety_contrast_class_05B == "ideal_DA_projection_high_safety_low",
      DA_projection_competence_composite_score - safety_risk_composite_05B,
      fifelse(
        safety_contrast_class_05B %in% c("high_safety_risk_low_DA", "mixed_DA_or_projection_with_safety_risk"),
        safety_risk_composite_05B,
        DA_projection_competence_composite_score
      )
    )
  ]

  tile_dt <- tile_dt[order(-story_rank_score_06B)]

  tile_dt <- head(tile_dt, TOP_N_GROUPS_FOR_TILE)

  tile_dt[
    ,
    group_label_06B := paste0(dataset, " | ", object_id, " | group ", group_id)
  ]

  tile_dt[
    ,
    group_label_short_06B := paste0(dataset, "_", seq_len(.N))
  ]
}

atomic_write_csv(as.data.frame(tile_dt), top_tile_table_csv)

stamp("生成 V2 publication draft figures。")

figure_records <- list()

x_med <- median(dataset_plot$mean_safety_risk_composite_05B, na.rm = TRUE)
y_med <- median(dataset_plot$mean_DA_projection_competence, na.rm = TRUE)

x_range <- range(dataset_plot$mean_safety_risk_composite_05B, na.rm = TRUE)
y_range <- range(dataset_plot$mean_DA_projection_competence, na.rm = TRUE)

x_pad <- diff(x_range) * 0.18
y_pad <- diff(y_range) * 0.18

p2a <- ggplot(
  dataset_plot,
  aes(
    x = mean_safety_risk_composite_05B,
    y = mean_DA_projection_competence
  )
) +
  geom_vline(xintercept = x_med, linetype = "dashed", linewidth = 0.35) +
  geom_hline(yintercept = y_med, linetype = "dashed", linewidth = 0.35) +
  geom_point(size = 3) +
  geom_text(aes(label = dataset_short), vjust = -0.8, size = 3.2, check_overlap = FALSE) +
  scale_x_continuous(
    limits = c(max(0, x_range[1] - x_pad), x_range[2] + x_pad),
    expand = expansion(mult = c(0.03, 0.08))
  ) +
  scale_y_continuous(
    limits = c(max(0, y_range[1] - y_pad), y_range[2] + y_pad),
    expand = expansion(mult = c(0.03, 0.10))
  ) +
  labs(
    title = "Dataset-level DA/projection competence versus safety-risk state",
    subtitle = "Projection score represents molecular competence, not anatomical projection",
    x = "Mean safety-risk-associated transcriptional score",
    y = "Mean DA/projection-associated molecular competence score"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.margin = margin(10, 18, 10, 14),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

figure_records[[length(figure_records) + 1L]] <- save_plot(
  p2a,
  "Figure2A_V2_dataset_DA_projection_vs_safety",
  width = 7.5,
  height = 5.7
)

p2b <- ggplot(
  dataset_plot,
  aes(
    x = reorder(dataset_short, favorable_index_06A),
    y = favorable_index_06A
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.35) +
  geom_col(width = 0.72) +
  coord_flip() +
  labs(
    title = "Dataset-level favorable index",
    subtitle = "Favorable index = DA/projection competence score − safety-risk score",
    x = "Dataset",
    y = "Favorable index"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

figure_records[[length(figure_records) + 1L]] <- save_plot(
  p2b,
  "Figure2B_V2_dataset_favorable_index",
  width = 7.2,
  height = 5.2
)

if (nrow(class_prop) > 0) {
  p2c <- ggplot(
    class_prop,
    aes(
      x = dataset_short,
      y = group_fraction,
      fill = class_short
    )
  ) +
    geom_col(width = 0.75) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Candidate class composition by dataset",
      x = "Dataset",
      y = "Fraction of groups",
      fill = "Candidate class"
    ) +
    theme_classic(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.title = element_text(face = "bold")
    )

  figure_records[[length(figure_records) + 1L]] <- save_plot(
    p2c,
    "Figure2C_V2_candidate_class_composition_by_dataset",
    width = 9.2,
    height = 5.5
  )
}

if (nrow(a9a10_prop) > 0) {
  p2d <- ggplot(
    a9a10_prop,
    aes(
      x = dataset_short,
      y = group_fraction,
      fill = bias_short
    )
  ) +
    geom_col(width = 0.75) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "A9/A10-like molecular bias composition",
      x = "Dataset",
      y = "Fraction of groups",
      fill = "A9/A10 bias"
    ) +
    theme_classic(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(face = "bold")
    )

  figure_records[[length(figure_records) + 1L]] <- save_plot(
    p2d,
    "Figure2D_V2_A9_A10_bias_composition_by_dataset",
    width = 8.8,
    height = 5.3
  )
}

if (nrow(tile_dt) > 0) {
  tile_long <- melt(
    tile_dt,
    id.vars = c(
      "dataset",
      "object_id",
      "group_id",
      "group_label_short_06B",
      "safety_contrast_class_05B",
      "class_short"
    ),
    measure.vars = c(
      "DA_like_composite_score",
      "projection_competence_composite_score",
      "safety_risk_composite_05B",
      "A9_minus_A10_score_05A"
    ),
    variable.name = "score_type",
    value.name = "score_value"
  )

  tile_long[
    ,
    score_type_short := pretty_score_type(score_type)
  ]

  tile_long[
    ,
    group_label_short_06B := factor(
      group_label_short_06B,
      levels = rev(unique(tile_dt$group_label_short_06B))
    )
  ]

  tile_long[
    ,
    score_type_short := factor(
      score_type_short,
      levels = c("DA-like\nscore", "Projection\ncompetence", "Safety-risk\nscore", "A9−A10\nbias")
    )
  ]

  p2e <- ggplot(
    tile_long,
    aes(
      x = score_type_short,
      y = group_label_short_06B,
      fill = score_value
    )
  ) +
    geom_tile() +
    facet_grid(class_short ~ ., scales = "free_y", space = "free_y") +
    labs(
      title = "Top story candidate groups",
      subtitle = "Rows are selected candidate groups; full IDs are in 06B_V2_top_story_group_tile_table.csv",
      x = "Score type",
      y = "Candidate group",
      fill = "Score"
    ) +
    theme_classic(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1),
      axis.text.y = element_text(size = 6),
      strip.text.y = element_text(size = 7, angle = 0),
      plot.title = element_text(face = "bold")
    )

  figure_records[[length(figure_records) + 1L]] <- save_plot(
    p2e,
    "Figure2E_V2_top_story_candidate_groups_tile",
    width = 8.2,
    height = 9
  )
}

figure_index <- rbindlist(figure_records, fill = TRUE)

figure_index$intended_panel <- c(
  "Figure 2A",
  "Figure 2B",
  "Figure 2C",
  "Figure 2D",
  "Figure 2E"
)[seq_len(nrow(figure_index))]

figure_index$description <- c(
  "Dataset-level DA/projection competence versus safety-risk score with unclipped labels.",
  "Dataset-level favorable index ranking.",
  "Candidate class composition by dataset as proportions with shortened labels.",
  "A9/A10-like molecular bias composition by dataset as proportions.",
  "Polished heatmap-like tile plot for top selected story candidate groups."
)[seq_len(nrow(figure_index))]

atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

number_lines <- if (nrow(numbers_dt) > 0 && all(c("metric", "value") %in% colnames(numbers_dt))) {
  paste0(numbers_dt$metric, ": ", numbers_dt$value)
} else {
  "No manuscript key numbers table found."
}

dataset_lines <- apply(as.data.frame(dataset_plot), 1, function(x) {
  paste0(
    x[["dataset"]],
    ": DA/projection=",
    round(as.numeric(x[["mean_DA_projection_competence"]]), 4),
    "; safety=",
    round(as.numeric(x[["mean_safety_risk_composite_05B"]]), 4),
    "; favorable_index=",
    round(as.numeric(x[["favorable_index_06A"]]), 4),
    "; quadrant=",
    x[["story_quadrant_06B"]]
  )
})

figure_lines <- if (nrow(figure_index) > 0) {
  paste0(figure_index$intended_panel, ": ", figure_index$figure_id)
} else {
  "none"
}

report_lines <- c(
  "06B V2 publication figure polish report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Manuscript key numbers:",
  number_lines,
  "",
  "Dataset figure interpretation:",
  dataset_lines,
  "",
  "Generated figure drafts:",
  figure_lines,
  "",
  "Output tables:",
  paste0("Figure index: ", figure_index_csv),
  paste0("Dataset plot table: ", dataset_plot_table_csv),
  paste0("Candidate class proportion table: ", candidate_class_prop_csv),
  paste0("A9/A10 proportion table: ", a9a10_prop_csv),
  paste0("Top tile table: ", top_tile_table_csv),
  "",
  "Output figure directory:",
  out_figures_dir,
  "",
  "Next step:",
  "06C_MANUSCRIPT_RESULTS_TEXT_DRAFT.R",
  "",
  "Journal-rigor note:",
  "V2 figures are improved drafts but remain transcriptional/molecular evidence. Do not claim real projection or proven safety."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("06B V2 publication figure polish 运行结束\n")
cat("============================================================\n\n")

cat("Figure drafts generated：", nrow(figure_index), "\n")
cat("Dataset rows：", nrow(dataset_plot), "\n")
cat("Candidate class proportion rows：", nrow(class_prop), "\n")
cat("A9/A10 proportion rows：", nrow(a9a10_prop), "\n")
cat("Top story tile rows：", nrow(tile_dt), "\n\n")

cat("输出表格：\n")
cat(figure_index_csv, "\n")
cat(dataset_plot_table_csv, "\n")
cat(candidate_class_prop_csv, "\n")
cat(a9a10_prop_csv, "\n")
cat(top_tile_table_csv, "\n\n")

cat("输出图片目录：\n")
cat(out_figures_dir, "\n\n")

cat("✅ 06B V2 publication figure polish 完成。\n")
cat("下一步进入 06C：manuscript results text draft。\n")
