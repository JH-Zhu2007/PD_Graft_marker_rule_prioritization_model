
PROJECT_DIR <- "D:/PD_Graft_Project"

EXTERNAL_GSE_ID <- "GSE183248"

INPUT_09E_VERSION <- "09E_frozen_external_validation_GSE183248_FINAL_V6_FIX_CLUSTER_CELLID"

PDF_WIDTH <- 11.5
PDF_HEIGHT <- 7.5

PROB_THRESHOLD <- 0.5

CLUSTER_LABEL_PREFIX <- "C"

cat("\n============================================================\n")
cat("09F：External validation figure polish FINAL V3 PUBLICATION LAYOUT\n")
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

HAS_GGREPEL <- requireNamespace("ggrepel", quietly = TRUE)

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_09E_dir <- file.path(tables_dir, INPUT_09E_VERSION)

out_tables_dir <- file.path(tables_dir, "09F_external_validation_figure_polish_V3_PUBLICATION_LAYOUT")
out_figures_dir <- file.path(figures_dir, "09F_external_validation_figure_polish_V3_PUBLICATION_LAYOUT_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

input_gene_overlap <- file.path(input_09E_dir, "09E_external_gene_overlap_audit.csv")
input_norm_audit <- file.path(input_09E_dir, "09E_external_normalization_decision_audit.csv")
input_whole_pred <- file.path(input_09E_dir, "09E_external_frozen_predictor_probabilities.csv")
input_grouping_audit <- file.path(input_09E_dir, "09E3_grouping_recovery_audit.csv")
input_cluster_assign <- file.path(input_09E_dir, "09E3_external_cell_grouping_assignments.csv")
input_cluster_scores <- file.path(input_09E_dir, "09E3_external_cluster_score_summary.csv")
input_cluster_pred <- file.path(input_09E_dir, "09E3_external_cluster_frozen_predictor_probabilities.csv")
input_ml_alignment <- file.path(input_09E_dir, "09E3_external_cluster_ML_feature_alignment_audit.csv")

cluster_priority_summary_csv <- file.path(out_tables_dir, "09F_V3_external_cluster_priority_summary.csv")
key_findings_csv <- file.path(out_tables_dir, "09F_V3_external_validation_key_findings.csv")
figure_manifest_csv <- file.path(out_tables_dir, "09F_V3_figure_manifest.csv")
method_note_txt <- file.path(out_tables_dir, "09F_V3_method_and_claim_boundary_note.txt")
report_txt <- file.path(reports_dir, "09F_external_validation_figure_polish_V3_report.txt")
session_info_txt <- file.path(out_tables_dir, "09F_V3_sessionInfo.txt")
output_check_csv <- file.path(out_tables_dir, "09F_V3_output_verification.csv")

fig_cluster_size_pdf <- file.path(out_figures_dir, "09F_V3_external_cluster_size_PUBLICATION.pdf")
fig_heatmap_pdf <- file.path(out_figures_dir, "09F_V3_external_cluster_signature_heatmap_PUBLICATION.pdf")
fig_probability_compact_pdf <- file.path(out_figures_dir, "09F_V3_external_predictor_probability_compact_PUBLICATION.pdf")
fig_priority_index_pdf <- file.path(out_figures_dir, "09F_V3_external_priority_index_barplot_PUBLICATION.pdf")
fig_scatter_rf_pdf <- file.path(out_figures_dir, "09F_V3_external_priority_scatter_random_forest_PUBLICATION.pdf")
fig_gene_overlap_pdf <- file.path(out_figures_dir, "09F_V3_external_gene_overlap_PUBLICATION.pdf")
fig_summary_pdf <- file.path(out_figures_dir, "09F_V3_external_validation_summary_panel_PUBLICATION.pdf")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

num <- function(x) suppressWarnings(as.numeric(x))

read_required_csv <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = TRUE, showProgress = FALSE)
}

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

clean_score_label <- function(x) {
  x <- as.character(x)
  x <- gsub("^score_", "", x)
  x <- gsub("^marker_", "", x)
  x <- gsub("_", " ", x)
  x
}

short_signature_label <- function(x) {
  x <- clean_score_label(x)
  x <- gsub("dopaminergic", "DA", x, ignore.case = TRUE)
  x <- gsub("extracellular matrix", "ECM", x, ignore.case = TRUE)
  x <- gsub("microglia macrophage immune", "immune", x, ignore.case = TRUE)
  x <- gsub("midbrain floor plate progenitor", "floor-plate prog.", x, ignore.case = TRUE)
  x <- gsub("neuronal maturation synapse", "neuronal maturation", x, ignore.case = TRUE)
  x <- gsub("pluripotency immature risk", "pluripotency risk", x, ignore.case = TRUE)
  x <- gsub("progenitor neuroepithelial", "neuroepithelial prog.", x, ignore.case = TRUE)
  x <- gsub("stress apoptosis response", "stress/apoptosis", x, ignore.case = TRUE)
  x <- gsub("vascular pericyte meningeal", "vascular/pericyte", x, ignore.case = TRUE)
  x
}

short_cluster_label <- function(x) {
  x <- as.character(x)
  idx <- gsub("^ExternalCluster_0*", "", x)
  paste0(CLUSTER_LABEL_PREFIX, idx)
}

extract_prob_long <- function(pred_dt) {
  if ("message" %in% names(pred_dt)) {
    return(data.table())
  }

  prob_cols <- names(pred_dt)[grepl("predicted_probability", names(pred_dt))]
  if (length(prob_cols) == 0) {
    return(data.table())
  }

  id_cols <- intersect(
    c("dataset", "external_group", "group_id", "object_id", "n_cells", "small_group_flag", "exploratory_external_class"),
    names(pred_dt)
  )

  long <- data.table::melt(
    pred_dt,
    id.vars = id_cols,
    measure.vars = prob_cols,
    variable.name = "model_probability",
    value.name = "probability",
    variable.factor = FALSE,
    value.factor = FALSE
  )

  long[, probability := num(probability)]
  long[, model := fifelse(grepl("^logistic", model_probability), "Logistic", "Random forest")]
  long[, task := fifelse(grepl("ideal_like", model_probability), "Ideal-like", "Safety-risk")]
  long[, cluster_short := short_cluster_label(external_group)]
  long[]
}

make_priority_summary <- function(pred_dt) {
  out <- copy(pred_dt)

  ideal_log_col <- "logistic_predicted_probability_ideal_like_classifier"
  safety_log_col <- "logistic_predicted_probability_safety_risk_classifier"
  ideal_rf_col <- "random_forest_predicted_probability_ideal_like_classifier"
  safety_rf_col <- "random_forest_predicted_probability_safety_risk_classifier"

  if (all(c(ideal_log_col, safety_log_col) %in% names(out))) {
    out[, ideal_like_probability_logistic := num(get(ideal_log_col))]
    out[, safety_risk_probability_logistic := num(get(safety_log_col))]
    out[, priority_index_logistic := ideal_like_probability_logistic - safety_risk_probability_logistic]
  } else {
    out[, ideal_like_probability_logistic := NA_real_]
    out[, safety_risk_probability_logistic := NA_real_]
    out[, priority_index_logistic := NA_real_]
  }

  if (all(c(ideal_rf_col, safety_rf_col) %in% names(out))) {
    out[, ideal_like_probability_random_forest := num(get(ideal_rf_col))]
    out[, safety_risk_probability_random_forest := num(get(safety_rf_col))]
    out[, priority_index_random_forest := ideal_like_probability_random_forest - safety_risk_probability_random_forest]
  } else {
    out[, ideal_like_probability_random_forest := NA_real_]
    out[, safety_risk_probability_random_forest := NA_real_]
    out[, priority_index_random_forest := NA_real_]
  }

  out[, consensus_priority_index := rowMeans(
    cbind(priority_index_logistic, priority_index_random_forest),
    na.rm = TRUE
  )]

  out[, consensus_external_class := fifelse(
    ideal_like_probability_logistic >= PROB_THRESHOLD & safety_risk_probability_logistic < PROB_THRESHOLD,
    "ideal_like_high_safety_low_like",
    fifelse(
      safety_risk_probability_logistic >= PROB_THRESHOLD,
      "safety_risk_high_like",
      "mixed_or_uncertain"
    )
  )]

  out[, cluster_short := short_cluster_label(external_group)]

  keep <- intersect(
    c(
      "dataset", "external_group", "cluster_short", "group_id", "object_id", "n_cells", "small_group_flag",
      "ideal_like_probability_logistic", "safety_risk_probability_logistic",
      "ideal_like_probability_random_forest", "safety_risk_probability_random_forest",
      "priority_index_logistic", "priority_index_random_forest", "consensus_priority_index",
      "consensus_external_class"
    ),
    names(out)
  )

  out[, ..keep][order(consensus_priority_index)]
}

stamp("读取 09E V6 outputs。")

gene_overlap <- read_required_csv(input_gene_overlap)
norm_audit <- read_required_csv(input_norm_audit)
whole_pred <- read_required_csv(input_whole_pred)
grouping_audit <- read_required_csv(input_grouping_audit)
cluster_assign <- read_required_csv(input_cluster_assign)
cluster_scores <- read_required_csv(input_cluster_scores)
cluster_pred <- read_required_csv(input_cluster_pred)
ml_alignment <- read_required_csv(input_ml_alignment)

if (!"external_group" %in% names(cluster_scores)) {
  stop("09E3_external_cluster_score_summary.csv 缺少 external_group。")
}

if (!"external_group" %in% names(cluster_pred) && !("message" %in% names(cluster_pred))) {
  stop("09E3_external_cluster_frozen_predictor_probabilities.csv 缺少 external_group。")
}

score_cols <- names(cluster_scores)[grepl("^score_", names(cluster_scores))]
if (length(score_cols) == 0) {
  stop("cluster score summary 里没有 score_ 列。")
}

stamp("External clusters：", uniqueN(cluster_scores$external_group))
stamp("External cells：", sum(cluster_scores$n_cells, na.rm = TRUE))
stamp("Score columns：", length(score_cols))

stamp("生成 09F V3 summary tables。")

priority_summary <- make_priority_summary(cluster_pred)

top_score <- data.table::melt(
  cluster_scores,
  id.vars = c("external_group", "n_cells"),
  measure.vars = score_cols,
  variable.name = "score_column",
  value.name = "mean_score",
  variable.factor = FALSE,
  value.factor = FALSE
)

top_score[, mean_score := num(mean_score)]
top_score <- top_score[order(external_group, -mean_score)]
top_score[, rank_in_cluster := seq_len(.N), by = external_group]

top3_score <- top_score[rank_in_cluster <= 3]
top3_collapsed <- top3_score[
  ,
  .(
    top3_signature_scores = paste0(
      clean_score_label(score_column),
      "=", signif(mean_score, 3),
      collapse = "; "
    )
  ),
  by = external_group
]

priority_summary <- merge(priority_summary, top3_collapsed, by = "external_group", all.x = TRUE)

atomic_write_csv(as.data.frame(priority_summary), cluster_priority_summary_csv)

n_safety <- sum(priority_summary$consensus_external_class == "safety_risk_high_like", na.rm = TRUE)
n_ideal <- sum(priority_summary$consensus_external_class == "ideal_like_high_safety_low_like", na.rm = TRUE)
n_uncertain <- sum(priority_summary$consensus_external_class == "mixed_or_uncertain", na.rm = TRUE)

key_findings <- data.table(
  item = c(
    "external_dataset",
    "external_cells",
    "external_clusters",
    "grouping_strategy",
    "ml_prediction_ready_tasks",
    "safety_risk_like_clusters",
    "ideal_like_clusters",
    "mixed_or_uncertain_clusters",
    "claim_boundary"
  ),
  value = c(
    EXTERNAL_GSE_ID,
    as.character(sum(cluster_scores$n_cells, na.rm = TRUE)),
    as.character(uniqueN(cluster_scores$external_group)),
    "unsupervised_external_cluster_recovery",
    as.character(sum(ml_alignment$prediction_ready == TRUE, na.rm = TRUE)),
    as.character(n_safety),
    as.character(n_ideal),
    as.character(n_uncertain),
    "External validation is transcriptomic and exploratory; it does not prove graft efficacy, anatomical projection, clinical safety, or therapeutic outcome."
  )
)

atomic_write_csv(as.data.frame(key_findings), key_findings_csv)

stamp("绘制 09F V3 external cluster size。")

size_dt <- copy(cluster_scores)
size_dt[, cluster_short := short_cluster_label(external_group)]
size_dt[, cluster_label := paste0(cluster_short, " (", external_group, ")")]
size_dt[, cluster_label := factor(cluster_label, levels = size_dt[order(n_cells)]$cluster_label)]

p_size <- ggplot(size_dt, aes(x = cluster_label, y = n_cells)) +
  geom_col(width = 0.72, fill = "grey55", color = "grey25", linewidth = 0.25) +
  geom_text(aes(label = n_cells), hjust = -0.10, size = 3.2) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "GSE183248 recovered external cluster sizes",
    x = NULL,
    y = "Number of cells"
  ) +
  theme_pub(base_size = 11) +
  theme(
    plot.margin = margin(8, 35, 8, 8),
    axis.text.y = element_text(size = 9)
  )

save_pdf_plot(p_size, fig_cluster_size_pdf, width = 10.8, height = 6.5)

stamp("绘制 09F V3 external cluster signature heatmap。")

heat_dt <- copy(cluster_scores)
heat_dt[, cluster_short := short_cluster_label(external_group)]

heat_mat <- as.matrix(heat_dt[, ..score_cols])
rownames(heat_mat) <- heat_dt$cluster_short

if (nrow(heat_mat) > 1) {
  heat_scaled <- scale(heat_mat)
} else {
  heat_scaled <- heat_mat
}
heat_scaled[!is.finite(heat_scaled)] <- 0
heat_scaled[heat_scaled > 2] <- 2
heat_scaled[heat_scaled < -2] <- -2

heat_long <- as.data.table(heat_scaled, keep.rownames = "cluster_short")
heat_long <- melt(
  heat_long,
  id.vars = "cluster_short",
  variable.name = "signature",
  value.name = "z_score",
  variable.factor = FALSE,
  value.factor = FALSE
)

if (nrow(priority_summary) > 0 && "consensus_priority_index" %in% names(priority_summary)) {
  cluster_order_short <- priority_summary[order(consensus_priority_index)]$cluster_short
} else {
  cluster_order_short <- heat_dt[order(n_cells)]$cluster_short
}

signature_order <- score_cols

heat_long[, cluster_short := factor(cluster_short, levels = rev(cluster_order_short))]
heat_long[, signature := factor(signature, levels = signature_order)]
heat_long[, signature_label := short_signature_label(signature)]
heat_long[, signature_label := factor(signature_label, levels = short_signature_label(signature_order))]

p_heat <- ggplot(heat_long, aes(x = signature_label, y = cluster_short, fill = z_score)) +
  geom_tile(color = "white", linewidth = 0.30) +
  scale_fill_gradient2(
    low = "navy",
    mid = "white",
    high = "firebrick",
    midpoint = 0,
    limits = c(-2, 2),
    name = "Scaled\nscore"
  ) +
  labs(
    title = "Frozen signature scores across recovered GSE183248 external clusters",
    subtitle = "Column-scaled scores for visualization only",
    x = NULL,
    y = NULL
  ) +
  theme_pub(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 42, hjust = 1, vjust = 1, size = 7.8),
    axis.text.y = element_text(size = 9.5, face = "bold"),
    legend.position = "right",
    plot.margin = margin(8, 14, 18, 8)
  )

save_pdf_plot(p_heat, fig_heatmap_pdf, width = 14.5, height = 6.7)

stamp("绘制 09F V3 compact external predictor probability。")

prob_long <- extract_prob_long(cluster_pred)

if (nrow(prob_long) == 0) {
  p_prob <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "No predictor probabilities available.", size = 5) +
    theme_void() +
    labs(title = "External predictor probabilities")
} else {
  prob_plot <- copy(prob_long)
  prob_plot[, cluster_short := factor(cluster_short, levels = short_cluster_label(priority_summary[order(consensus_priority_index)]$external_group))]

  p_prob <- ggplot(prob_plot, aes(x = cluster_short, y = probability, fill = task)) +
    geom_col(
      position = position_dodge(width = 0.72),
      width = 0.62,
      color = "grey25",
      linewidth = 0.18
    ) +
    geom_hline(yintercept = PROB_THRESHOLD, linetype = "dashed", linewidth = 0.35) +
    facet_wrap(~ model, ncol = 1) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.03))) +
    labs(
      title = "Frozen predictor probabilities across recovered external clusters",
      subtitle = "Compact layout; dashed line indicates probability threshold = 0.5",
      x = "Recovered external cluster",
      y = "Predicted probability",
      fill = "Prediction task"
    ) +
    theme_pub(base_size = 10.5) +
    theme(
      axis.text.x = element_text(size = 9, face = "bold"),
      strip.text = element_text(size = 10),
      legend.position = "right"
    )
}

save_pdf_plot(p_prob, fig_probability_compact_pdf, width = 10.8, height = 7.4)

stamp("绘制 09F V3 priority index barplot。")

priority_plot <- copy(priority_summary)
priority_plot[, cluster_short := factor(cluster_short, levels = priority_plot[order(consensus_priority_index)]$cluster_short)]

priority_long <- melt(
  priority_plot,
  id.vars = c("external_group", "cluster_short", "n_cells"),
  measure.vars = c("priority_index_logistic", "priority_index_random_forest", "consensus_priority_index"),
  variable.name = "priority_metric",
  value.name = "priority_index",
  variable.factor = FALSE,
  value.factor = FALSE
)

priority_long[, priority_metric := factor(
  priority_metric,
  levels = c("priority_index_logistic", "priority_index_random_forest", "consensus_priority_index"),
  labels = c("Logistic", "Random forest", "Consensus")
)]

p_priority <- ggplot(priority_long, aes(x = cluster_short, y = priority_index, fill = priority_metric)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.35, color = "grey40") +
  geom_col(
    position = position_dodge(width = 0.74),
    width = 0.65,
    color = "grey25",
    linewidth = 0.18
  ) +
  coord_flip() +
  labs(
    title = "External cluster priority index",
    subtitle = "Priority index = ideal-like probability − safety-risk probability",
    x = "Recovered external cluster",
    y = "Priority index",
    fill = "Model"
  ) +
  theme_pub(base_size = 10.5) +
  theme(
    axis.text.y = element_text(size = 9.5, face = "bold"),
    legend.position = "right"
  )

save_pdf_plot(p_priority, fig_priority_index_pdf, width = 9.8, height = 6.7)

stamp("绘制 09F V3 random forest priority scatter。")

if (nrow(priority_summary) > 0 &&
    all(c("ideal_like_probability_random_forest", "safety_risk_probability_random_forest") %in% names(priority_summary))) {

  scatter_dt <- copy(priority_summary)
  scatter_dt[, ideal_like_probability_random_forest := num(ideal_like_probability_random_forest)]
  scatter_dt[, safety_risk_probability_random_forest := num(safety_risk_probability_random_forest)]
  scatter_dt[, n_cells := num(n_cells)]

  p_scatter <- ggplot(
    scatter_dt,
    aes(
      x = ideal_like_probability_random_forest,
      y = safety_risk_probability_random_forest,
      size = n_cells,
      label = cluster_short
    )
  ) +
    geom_hline(yintercept = PROB_THRESHOLD, linetype = "dashed", color = "grey50", linewidth = 0.35) +
    geom_vline(xintercept = PROB_THRESHOLD, linetype = "dashed", color = "grey50", linewidth = 0.35) +
    geom_point(shape = 21, fill = "grey55", color = "black", alpha = 0.86) +
    scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0.04, 0.08))) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0.04, 0.08))) +
    scale_size_continuous(range = c(2.5, 8), name = "Cells") +
    labs(
      title = "External cluster prioritization using random-forest probabilities",
      subtitle = "Random forest view shown for layout clarity; logistic probabilities were saturated",
      x = "Ideal-like probability",
      y = "Safety-risk probability"
    ) +
    theme_pub(base_size = 11)

  if (HAS_GGREPEL) {
    p_scatter <- p_scatter +
      ggrepel::geom_text_repel(
        size = 3.4,
        max.overlaps = Inf,
        box.padding = 0.45,
        point.padding = 0.35,
        min.segment.length = 0,
        seed = 20260715
      )
  } else {
    p_scatter <- p_scatter +
      geom_text(vjust = -0.8, size = 3.2, check_overlap = TRUE)
  }

} else {
  p_scatter <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "Random-forest probability columns unavailable.", size = 5) +
    theme_void() +
    labs(title = "External cluster prioritization")
}

save_pdf_plot(p_scatter, fig_scatter_rf_pdf, width = 8.6, height = 7.6)

stamp("绘制 09F V3 external gene overlap。")

overlap_dt <- copy(gene_overlap)
overlap_dt[, category_label := paste0(short_signature_label(category), " (", n_overlap_genes, "/", n_marker_genes, ")")]
overlap_dt <- overlap_dt[order(overlap_fraction)]
overlap_dt[, category_label := factor(category_label, levels = category_label)]

p_overlap <- ggplot(overlap_dt, aes(x = category_label, y = overlap_fraction)) +
  geom_col(aes(fill = enough_overlap), width = 0.72, color = "grey25", linewidth = 0.2) +
  geom_text(aes(label = paste0(n_overlap_genes, "/", n_marker_genes)), hjust = -0.12, size = 2.9) +
  coord_flip(clip = "off") +
  scale_y_continuous(limits = c(0, 1.08), breaks = seq(0, 1, 0.25), expand = expansion(mult = c(0, 0.03))) +
  scale_fill_manual(
    values = c("TRUE" = "grey55", "FALSE" = "grey85"),
    name = "Meets minimum\noverlap"
  ) +
  labs(
    title = "Frozen marker-gene overlap in GSE183248",
    x = NULL,
    y = "Overlap fraction"
  ) +
  theme_pub(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 8.4),
    plot.margin = margin(8, 35, 8, 8)
  )

save_pdf_plot(p_overlap, fig_gene_overlap_pdf, width = 12.2, height = 7.8)

stamp("绘制 09F V3 external validation summary panel。")

summary_dt <- data.table(
  Metric = c(
    "External dataset",
    "Recovered cells",
    "Recovered clusters",
    "Grouping strategy",
    "ML tasks ready",
    "Safety-risk-like clusters",
    "Ideal-like clusters",
    "Mixed/uncertain clusters",
    "Interpretation boundary"
  ),
  Value = c(
    EXTERNAL_GSE_ID,
    as.character(sum(cluster_scores$n_cells, na.rm = TRUE)),
    as.character(uniqueN(cluster_scores$external_group)),
    "Unsupervised external cluster recovery",
    as.character(sum(ml_alignment$prediction_ready == TRUE, na.rm = TRUE)),
    as.character(n_safety),
    as.character(n_ideal),
    as.character(n_uncertain),
    "Transcriptomic prioritization only"
  )
)

summary_dt[, row_id := seq_len(.N)]
summary_dt[, y := rev(row_id)]

p_summary <- ggplot(summary_dt, aes(y = y)) +
  annotate(
    "text",
    x = 0,
    y = max(summary_dt$y) + 1.05,
    label = "GSE183248 frozen external validation summary",
    hjust = 0,
    fontface = "bold",
    size = 5.0
  ) +
  geom_text(aes(x = 0.02, label = Metric), hjust = 0, fontface = "bold", size = 3.75) +
  geom_text(aes(x = 0.50, label = Value), hjust = 0, size = 3.75) +
  annotate(
    "text",
    x = 0.02,
    y = 0.45,
    label = paste(
      "Claim boundary:",
      "external clusters are unsupervised transcriptomic groups;",
      "probabilities are marker-rule-derived prioritization scores, not clinical outcome or safety predictions."
    ),
    hjust = 0,
    size = 3.20
  ) +
  xlim(0, 1.45) +
  ylim(0, max(summary_dt$y) + 1.6) +
  theme_void()

save_pdf_plot(p_summary, fig_summary_pdf, width = 12.0, height = 6.8)

stamp("写出 09F V3 method note / report。")

method_lines <- c(
  "09F external validation figure polish V3 method and claim-boundary note",
  "",
  "Input:",
  paste0("09E final input version: ", INPUT_09E_VERSION),
  "",
  "Method:",
  "09F V3 did not rerun external data import, marker scoring, clustering, or model training.",
  "09F V3 only reformatted 09E V6 outputs into publication-layout PDF figures and summary tables.",
  "Probability visualization was changed from long-label barplots to compact grouped plots to avoid label crowding.",
  "The priority scatter uses random-forest probabilities for visual spread because logistic probabilities were saturated near ideal-like=0 and safety-risk=1.",
  "A priority-index barplot is provided as the recommended main visualization for external predictor output.",
  "",
  "Claim boundary:",
  "09F V3 figures support external transcriptomic application of the frozen framework.",
  "The recovered external clusters are unsupervised transcriptomic groups, not manually validated biological cell types.",
  "Predicted probabilities indicate marker-rule-derived transcriptomic prioritization only.",
  "These results do not prove anatomical projection, functional graft integration, clinical safety, tumorigenicity, or therapeutic efficacy.",
  "The external processed matrix contained a terminal incomplete row that was discarded during import in 09E; this should be documented as an input-data limitation."
)

writeLines(method_lines, method_note_txt)

fig_manifest <- data.table(
  figure_id = c(
    "09F_V3_Fig1",
    "09F_V3_Fig2",
    "09F_V3_Fig3",
    "09F_V3_Fig4",
    "09F_V3_Fig5",
    "09F_V3_Fig6",
    "09F_V3_Fig7"
  ),
  file = c(
    fig_cluster_size_pdf,
    fig_heatmap_pdf,
    fig_probability_compact_pdf,
    fig_priority_index_pdf,
    fig_scatter_rf_pdf,
    fig_gene_overlap_pdf,
    fig_summary_pdf
  ),
  description = c(
    "Recovered external cluster sizes in GSE183248.",
    "Publication-layout frozen signature heatmap across recovered external clusters.",
    "Compact ideal-like and safety-risk predictor probabilities across recovered external clusters.",
    "Priority-index barplot based on ideal-like minus safety-risk probabilities.",
    "Random-forest probability scatter for external cluster prioritization.",
    "Frozen marker-gene overlap audit in GSE183248.",
    "External validation summary panel."
  ),
  recommended_use = c(
    "Supplementary or part of external-validation panel.",
    "Main external-validation figure candidate.",
    "Supplementary; compact predictor view.",
    "Recommended main predictor-summary visualization.",
    "Supplementary; RF view avoids logistic saturation overlap.",
    "Supplementary marker-overlap audit.",
    "Supplementary summary or internal figure."
  )
)

atomic_write_csv(as.data.frame(fig_manifest), figure_manifest_csv)

report_lines <- c(
  "09F external validation figure polish V3 report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input 09E version:",
  INPUT_09E_VERSION,
  "",
  "Key external validation metrics:",
  capture.output(print(key_findings)),
  "",
  "Priority summary:",
  capture.output(print(priority_summary)),
  "",
  "ML feature alignment:",
  capture.output(print(ml_alignment)),
  "",
  "Output figures:",
  fig_cluster_size_pdf,
  fig_heatmap_pdf,
  fig_probability_compact_pdf,
  fig_priority_index_pdf,
  fig_scatter_rf_pdf,
  fig_gene_overlap_pdf,
  fig_summary_pdf
)

writeLines(report_lines, report_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

required_outputs <- c(
  cluster_priority_summary_csv,
  key_findings_csv,
  figure_manifest_csv,
  method_note_txt,
  report_txt,
  session_info_txt,
  fig_cluster_size_pdf,
  fig_heatmap_pdf,
  fig_probability_compact_pdf,
  fig_priority_index_pdf,
  fig_scatter_rf_pdf,
  fig_gene_overlap_pdf,
  fig_summary_pdf
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
  stop("09F V3 输出验证失败。")
}

cat("\n============================================================\n")
cat("09F external validation figure polish FINAL V3 PUBLICATION LAYOUT 运行结束\n")
cat("============================================================\n\n")

cat("Input 09E version：", INPUT_09E_VERSION, "\n")
cat("External clusters：", uniqueN(cluster_scores$external_group), "\n")
cat("External cells：", sum(cluster_scores$n_cells, na.rm = TRUE), "\n")
cat("ML prediction ready tasks：", sum(ml_alignment$prediction_ready == TRUE, na.rm = TRUE), "\n")
cat("Safety-risk-like clusters：", n_safety, "\n")
cat("Ideal-like clusters：", n_ideal, "\n")
cat("Mixed/uncertain clusters：", n_uncertain, "\n\n")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("关键输出：\n")
cat(cluster_priority_summary_csv, "\n")
cat(key_findings_csv, "\n")
cat(figure_manifest_csv, "\n")
cat(method_note_txt, "\n\n")

cat("PDF 图：\n")
cat(fig_cluster_size_pdf, "\n")
cat(fig_heatmap_pdf, "\n")
cat(fig_probability_compact_pdf, "\n")
cat(fig_priority_index_pdf, "\n")
cat(fig_scatter_rf_pdf, "\n")
cat(fig_gene_overlap_pdf, "\n")
cat(fig_summary_pdf, "\n\n")

cat("✅ 09F external validation figure polish FINAL V3 PUBLICATION LAYOUT 完成。\n")
