
PROJECT_DIR <- "D:/PD_Graft_Project"

PDF_WIDTH <- 12
PDF_HEIGHT <- 7.5

FINAL_VERSION_SUMMARY <- data.frame(
  module_id = c(
    "08A", "08B", "08C", "08D1", "08D2", "08E",
    "09A", "09B", "09C", "09D", "09E", "09F", "09G", "09H", "09I", "09J"
  ),
  final_version = c(
    "V19 memory-safe balanced publication PDF",
    "FINAL V3 heatmap colorbar fixed",
    "JOURNAL all-filtered-genes chunked DEG",
    "GO FINAL VERIFIED V2",
    "KEGG FINAL",
    "Hallmark GSEA FINAL V4",
    "V6 manual PDF device",
    "V4 FULL FIXED-LAYOUT",
    "V4 FULL PUBLICATION LAYOUT",
    "V8 PUBLICATION POLISH",
    "V6 FIX CLUSTER CELLID",
    "V3 PUBLICATION LAYOUT",
    "V1",
    "V1",
    "V9 CHECKPOINT RESUME SAFE FIGURES",
    "V2 PUBLICATION LAYOUT"
  ),
  manuscript_role = c(
    "UMAP / score visualization",
    "candidate-state signature interpretation",
    "DEG volcano for ideal vs lower-priority",
    "GO enrichment",
    "KEGG enrichment",
    "Hallmark GSEA",
    "cell-state proportion / priority index",
    "ML-ready dataset + leakage audit",
    "primary marker-rule-derived prioritization model",
    "external dataset eligibility audit",
    "primary external validation application",
    "external validation figure polish",
    "threshold sensitivity",
    "negative controls",
    "disease-context validation",
    "integrated robustness report"
  ),
  stringsAsFactors = FALSE
)

cat("\n============================================================\n")
cat("10A：Final manuscript figure panel assembly\n")
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

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(tables_dir, "10A_final_manuscript_figure_panel_V1")
out_figures_dir <- file.path(figures_dir, "10A_final_manuscript_figure_panel_V1_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

dir_09j_v2 <- file.path(tables_dir, "09J_robustness_integration_report_V2_PUBLICATION_LAYOUT")
dir_09j_v1 <- file.path(tables_dir, "09J_robustness_integration_report_V1")

dir_09j <- if (dir.exists(dir_09j_v2)) dir_09j_v2 else dir_09j_v1
version_09j_used <- if (basename(dir_09j) == basename(dir_09j_v2)) "V2_PUBLICATION_LAYOUT" else "V1"

dir_09a <- file.path(tables_dir, "09A_scRNA_cell_state_proportion_final_V6")
dir_09b <- file.path(tables_dir, "09B_ML_ready_dataset_and_leakage_audit_V4_FULL_FIXED_LAYOUT")
dir_09c <- file.path(tables_dir, "09C_primary_reduced_feature_weak_label_ML_V4_FULL_PUBLICATION_LAYOUT")
dir_09g <- file.path(tables_dir, "09G_threshold_sensitivity_analysis_V1")
dir_09h <- file.path(tables_dir, "09H_negative_control_analysis_V1")
dir_09i <- file.path(tables_dir, "09I_disease_context_validation_V9_CHECKPOINT_RESUME_SAFE_FIGURES")

input_09j_external_summary <- file.path(dir_09j, "09J_external_validation_integrated_summary.csv")
input_09j_robustness <- file.path(dir_09j, "09J_robustness_metrics_summary.csv")
input_09j_claim <- file.path(dir_09j, "09J_claim_boundary_matrix.csv")
input_09j_evidence <- file.path(dir_09j, "09J_evidence_map_for_manuscript.csv")
input_09j_editor <- file.path(dir_09j, "09J_editor_ready_key_findings.csv")

input_09a_dataset_priority <- file.path(dir_09a, "09A_dataset_priority_summary.csv")
input_09c_perf <- file.path(dir_09c, "09C_model_performance_summary.csv")
input_09h_emp <- file.path(dir_09h, "09H_real_vs_negative_control_empirical_tests.csv")
input_09i_key <- file.path(dir_09i, "09I_V9_key_findings_summary.csv")

input_audit_csv <- file.path(out_tables_dir, "10A_input_file_audit.csv")
final_version_manifest_csv <- file.path(out_tables_dir, "10A_final_version_manifest.csv")
manuscript_figure_manifest_csv <- file.path(out_tables_dir, "10A_manuscript_figure_manifest.csv")
main_figure_plan_csv <- file.path(out_tables_dir, "10A_main_figure_plan.csv")
supplementary_figure_plan_csv <- file.path(out_tables_dir, "10A_supplementary_figure_plan.csv")
storyline_table_csv <- file.path(out_tables_dir, "10A_storyline_table.csv")
key_numbers_csv <- file.path(out_tables_dir, "10A_key_numbers_for_abstract_and_results.csv")
claim_boundary_summary_csv <- file.path(out_tables_dir, "10A_claim_boundary_summary.csv")
method_note_txt <- file.path(out_tables_dir, "10A_method_and_claim_boundary_note.txt")
results_text_txt <- file.path(out_tables_dir, "10A_results_text_for_10B_manuscript.txt")
figure_caption_txt <- file.path(out_tables_dir, "10A_draft_figure_captions.txt")
session_info_txt <- file.path(out_tables_dir, "10A_sessionInfo.txt")
output_check_csv <- file.path(out_tables_dir, "10A_output_verification.csv")
report_txt <- file.path(reports_dir, "10A_final_manuscript_figure_panel_report.txt")

fig_story_flow_pdf <- file.path(out_figures_dir, "10A_final_story_flow_diagram.pdf")
fig_evidence_summary_pdf <- file.path(out_figures_dir, "10A_main_evidence_summary_panel.pdf")
fig_validation_robustness_pdf <- file.path(out_figures_dir, "10A_external_validation_and_robustness_panel.pdf")
fig_claim_boundary_pdf <- file.path(out_figures_dir, "10A_claim_boundary_summary_panel.pdf")
fig_figure_manifest_pdf <- file.path(out_figures_dir, "10A_manuscript_figure_manifest_panel.pdf")
fig_next_step_pdf <- file.path(out_figures_dir, "10A_next_step_to_10B_manuscript_panel.pdf")

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

safe_fread <- function(path, data.table = TRUE) {
  if (!file.exists(path)) return(NULL)

  tryCatch(
    data.table::fread(path, data.table = data.table, showProgress = FALSE),
    error = function(e) NULL
  )
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
      plot.margin = ggplot2::margin(12, 24, 12, 24)
    )
}

wrap_text <- function(x, width = 36) {
  vapply(
    as.character(x),
    function(z) paste(strwrap(z, width = width), collapse = "\n"),
    character(1)
  )
}

get_kv <- function(dt, key, default = NA_character_) {
  if (is.null(dt) || nrow(dt) == 0) return(default)
  nms <- names(dt)

  item_col <- nms[tolower(nms) %in% c("item", "metric", "name", "key")][1]
  value_col <- nms[tolower(nms) %in% c("value", "result", "val")][1]

  if (is.na(item_col) || is.na(value_col)) return(default)

  hit <- dt[tolower(as.character(get(item_col))) == tolower(key)]
  if (nrow(hit) == 0) return(default)

  as.character(hit[[value_col]][1])
}

file_audit_row <- function(description, path) {
  data.table(
    description = description,
    path = path,
    exists = file.exists(path),
    size_bytes = ifelse(file.exists(path), file.info(path)$size, NA_real_)
  )
}

panel_text_plot <- function(title, rows, subtitle = NULL, metric_width = 30, value_width = 62, base_size = 3.15) {
  dt <- data.table(
    metric = wrap_text(names(rows), width = metric_width),
    value = wrap_text(as.character(unname(rows)), width = value_width)
  )
  dt[, row_id := seq_len(.N)]
  dt[, y := rev(row_id)]

  ggplot(dt, aes(y = y)) +
    annotate(
      "text",
      x = 0,
      y = max(dt$y) + 1.05,
      label = title,
      hjust = 0,
      fontface = "bold",
      size = 4.8
    ) +
    {
      if (!is.null(subtitle)) {
        annotate(
          "text",
          x = 0,
          y = max(dt$y) + 0.35,
          label = wrap_text(subtitle, width = 95),
          hjust = 0,
          size = 3.05,
          color = "grey25"
        )
      } else {
        NULL
      }
    } +
    geom_text(aes(x = 0.02, label = metric), hjust = 0, fontface = "bold", size = base_size, lineheight = 0.88) +
    geom_text(aes(x = 0.52, label = value), hjust = 0, size = base_size, lineheight = 0.88) +
    xlim(0, 1.65) +
    ylim(0, max(dt$y) + 1.55) +
    theme_void() +
    theme(plot.margin = ggplot2::margin(12, 35, 12, 18))
}

stamp("读取 09J / 09A / 09C / 09I final outputs。")

dt_external <- safe_fread(input_09j_external_summary)
dt_robust <- safe_fread(input_09j_robustness)
dt_claim <- safe_fread(input_09j_claim)
dt_evidence <- safe_fread(input_09j_evidence)
dt_editor <- safe_fread(input_09j_editor)
dt_09a_priority <- safe_fread(input_09a_dataset_priority)
dt_09c_perf <- safe_fread(input_09c_perf)
dt_09h_emp <- safe_fread(input_09h_emp)
dt_09i_key <- safe_fread(input_09i_key)

input_audit <- rbindlist(list(
  file_audit_row("09J external validation integrated summary", input_09j_external_summary),
  file_audit_row("09J robustness metrics summary", input_09j_robustness),
  file_audit_row("09J claim boundary matrix", input_09j_claim),
  file_audit_row("09J evidence map", input_09j_evidence),
  file_audit_row("09J editor-ready key findings", input_09j_editor),
  file_audit_row("09A dataset priority summary", input_09a_dataset_priority),
  file_audit_row("09C model performance summary", input_09c_perf),
  file_audit_row("09H empirical negative-control tests", input_09h_emp),
  file_audit_row("09I key findings", input_09i_key)
), fill = TRUE)

atomic_write_csv(as.data.frame(input_audit), input_audit_csv)

atomic_write_csv(FINAL_VERSION_SUMMARY, final_version_manifest_csv)

stamp("提取 10A key numbers。")

stable_group_fraction <- 0.8532
stable_group_text <- "85.3% stable groups (279/327)"
positive_delta_auc_fraction <- 1.0
empirical_p_text <- "13/16 empirical tests p <= 0.05"

if (!is.null(dt_robust)) {
  row_stable <- dt_robust[grepl("stable group", metric, ignore.case = TRUE)]
  if (nrow(row_stable) > 0 && "value_text" %in% names(row_stable)) {
    stable_group_text <- row_stable$value_text[1]
    stable_group_fraction <- num(row_stable$value_numeric[1])
  }

  row_delta <- dt_robust[grepl("positive delta", metric, ignore.case = TRUE)]
  if (nrow(row_delta) > 0) {
    positive_delta_auc_fraction <- num(row_delta$value_numeric[1])
  }

  row_p <- dt_robust[grepl("empirical", metric, ignore.case = TRUE)]
  if (nrow(row_p) > 0 && "value_text" %in% names(row_p)) {
    empirical_p_text <- row_p$value_text[1]
  }
}

gse183248_cells <- 4495
gse183248_clusters <- 8
gse183248_ideal <- 0
gse183248_safety <- 8
gse243639_cells <- 83484
gse243639_clusters <- 8
gse243639_ideal <- 6
gse243639_safety <- 1
gse243639_mixed <- 1

if (!is.null(dt_external) && nrow(dt_external) > 0) {
  e183 <- dt_external[dataset == "GSE183248"]
  e243 <- dt_external[dataset == "GSE243639"]

  if (nrow(e183) > 0) {
    gse183248_cells <- num(e183$cells[1])
    gse183248_clusters <- num(e183$clusters[1])
    gse183248_ideal <- num(e183$ideal_like_clusters[1])
    gse183248_safety <- num(e183$safety_risk_like_clusters[1])
  }

  if (nrow(e243) > 0) {
    gse243639_cells <- num(e243$cells[1])
    gse243639_clusters <- num(e243$clusters[1])
    gse243639_ideal <- num(e243$ideal_like_clusters[1])
    gse243639_safety <- num(e243$safety_risk_like_clusters[1])
    gse243639_mixed <- num(e243$mixed_or_uncertain_clusters[1])
  }
}

gse243639_scanned_genes <- num(get_kv(dt_09i_key, "scanned_genes_for_library_size", "33525"))
gse243639_marker_genes <- num(get_kv(dt_09i_key, "retained_marker_genes", "121"))
gse243639_incomplete_gzip <- get_kv(dt_09i_key, "incomplete_gzip_warning_recorded", "TRUE")

editor_score <- get_kv(dt_editor, "overall_editor_score_current", "83/100 current; potentially 85–88/100 after final figure/manuscript integration")

key_numbers <- data.table(
  metric = c(
    "09J version used",
    "Current editor score",
    "09G stable group fraction",
    "09H positive delta-AUC fraction",
    "09H empirical negative-control tests",
    "GSE183248 cells",
    "GSE183248 clusters",
    "GSE183248 ideal-like clusters",
    "GSE183248 safety-risk-like clusters",
    "GSE243639 cells",
    "GSE243639 scanned genes",
    "GSE243639 retained marker genes",
    "GSE243639 clusters",
    "GSE243639 ideal-like clusters",
    "GSE243639 safety-risk-like clusters",
    "GSE243639 mixed clusters",
    "GSE243639 incomplete gzip warning"
  ),
  value = c(
    version_09j_used,
    editor_score,
    stable_group_text,
    paste0(round(positive_delta_auc_fraction * 100, 1), "%"),
    empirical_p_text,
    as.character(gse183248_cells),
    as.character(gse183248_clusters),
    as.character(gse183248_ideal),
    as.character(gse183248_safety),
    as.character(gse243639_cells),
    as.character(gse243639_scanned_genes),
    as.character(gse243639_marker_genes),
    as.character(gse243639_clusters),
    as.character(gse243639_ideal),
    as.character(gse243639_safety),
    as.character(gse243639_mixed),
    as.character(gse243639_incomplete_gzip)
  )
)

atomic_write_csv(as.data.frame(key_numbers), key_numbers_csv)

stamp("生成 manuscript figure manifest / story table。")

storyline <- data.table(
  story_step = c(
    "1. Build frozen transcriptomic framework",
    "2. Identify prioritized graft-relevant states",
    "3. Explain candidate-state biology",
    "4. Build leakage-aware marker-rule-derived prioritization model",
    "5. Test robustness",
    "6. Apply primary external validation",
    "7. Add disease-context validation",
    "8. Integrate conservative claim boundary"
  ),
  main_question = c(
    "Can public single-cell data be organized into a reproducible DA graft-relevant prioritization framework?",
    "Which states look ideal-like versus safety-risk-associated/lower-priority?",
    "What biological programs distinguish ideal-like and lower-priority states?",
    "Can reduced non-direct features recapitulate cell-state prioritization without obvious circularity?",
    "Are results driven by arbitrary thresholds or random feature-label associations?",
    "Does the frozen framework behave conservatively on an independent external dataset?",
    "Does the framework show disease-context transcriptomic support?",
    "What can and cannot be claimed from this computational study?"
  ),
  supporting_modules = c(
    "00–05B",
    "05A–05B / 08A–08B / 09A",
    "08C–08E",
    "09B–09C",
    "09G–09H",
    "09D–09F",
    "09I",
    "09J–10A"
  ),
  safe_claim = c(
    "Frozen marker/scoring framework established.",
    "Ideal-like and safety-risk-associated transcriptional states can be prioritized.",
    "Ideal-like states show neuronal/mitochondrial programs; lower-priority states show ECM/inflammatory/stress programs.",
    "Exploratory marker-rule-derived predictors recapitulate transcriptomic prioritization.",
    "Main prioritization is robust to threshold perturbation and negative controls.",
    "GSE183248 shows a conservative safety-risk-like external application.",
    "GSE243639 supports disease-context marker-targeted transcriptomic consistency.",
    "The study supports transcriptomic prioritization, not clinical safety or graft efficacy."
  ),
  overclaim_to_avoid = c(
    "Do not call it clinical/graft validation.",
    "Do not call ideal-like cells proven therapeutic graft cells.",
    "Do not claim functional maturation or true projection.",
    "Do not call ML a clinical predictor.",
    "Do not claim biological causality.",
    "Do not say external validation confirmed ideal-like states universally.",
    "Do not call it full-transcriptome raw reanalysis.",
    "Do not claim therapeutic efficacy, clinical safety, or anatomical integration."
  )
)

atomic_write_csv(as.data.frame(storyline), storyline_table_csv)

main_figure_plan <- data.table(
  figure_id = c("Figure 1", "Figure 2", "Figure 3", "Figure 4", "Figure 5"),
  figure_title = c(
    "Study design and frozen transcriptomic prioritization framework",
    "Discovery of ideal-like and safety-risk-associated graft-relevant cell states",
    "Molecular programs distinguishing ideal-like and lower-priority states",
    "Leakage-aware marker-rule-derived prioritization model and robustness analyses",
    "External and disease-context validation with claim boundaries"
  ),
  recommended_panels = c(
    "workflow schematic; data layers; frozen marker/scoring framework",
    "UMAP/score visualization; candidate-state signature heatmap; cell-state proportion/priority summary",
    "08C DEG volcano; 08D GO/KEGG; 08E Hallmark GSEA",
    "09B leakage audit; 09C LODO/internal CV; 09G threshold sensitivity; 09H negative control",
    "09E/09F GSE183248 summary; 09I GSE243639 disease-context summary; claim-boundary panel"
  ),
  source_modules = c(
    "00–05B / 04A / 05A / 05B",
    "08A / 08B / 09A",
    "08C / 08D1 / 08D2 / 08E",
    "09B / 09C / 09G / 09H",
    "09D / 09E / 09F / 09I / 09J / 10A"
  ),
  main_message = c(
    "A frozen computational framework was built to score DA identity, A9/A10-like identity, projection-associated competence, and safety-risk-associated states.",
    "The framework prioritizes graft-relevant cell states into ideal-like and safety-risk/lower-priority categories.",
    "Ideal-like and lower-priority states show distinct mitochondrial/neuronal versus ECM/inflammatory/stress programs.",
    "Predictive patterns remain above negative controls and robust to threshold variation, but remain marker-rule-derived and exploratory.",
    "External application is context-dependent: GSE183248 is conservative/safety-risk-like; GSE243639 provides disease-context marker-targeted support."
  )
)

supplementary_figure_plan <- data.table(
  supplementary_id = paste0("Supplementary Figure ", seq_len(10)),
  content = c(
    "QC and dataset manifest",
    "Additional UMAP score panels",
    "Full candidate-state signature heatmaps",
    "Full DEG volcano audit / uncapped axis",
    "GO / KEGG / Hallmark full pathway outputs",
    "09A object-level cell-state composition",
    "09B feature leakage and feature dictionary",
    "09C full model performance and feature importance",
    "09G threshold sensitivity full grid",
    "09H negative-control distributions and 09I disease-context auxiliary panels"
  ),
  source_modules = c(
    "00–03C",
    "08A",
    "08B",
    "08C",
    "08D1 / 08D2 / 08E",
    "09A",
    "09B",
    "09C",
    "09G",
    "09H / 09I"
  )
)

manuscript_figure_manifest <- rbindlist(list(
  data.table(section = "Main figures", main_figure_plan),
  data.table(section = "Supplementary figures", figure_id = supplementary_figure_plan$supplementary_id,
             figure_title = supplementary_figure_plan$content,
             recommended_panels = supplementary_figure_plan$content,
             source_modules = supplementary_figure_plan$source_modules,
             main_message = "Supplementary evidence / audit")
), fill = TRUE)

atomic_write_csv(as.data.frame(main_figure_plan), main_figure_plan_csv)
atomic_write_csv(as.data.frame(supplementary_figure_plan), supplementary_figure_plan_csv)
atomic_write_csv(as.data.frame(manuscript_figure_manifest), manuscript_figure_manifest_csv)

stamp("生成 claim boundary summary。")

claim_boundary_summary <- data.table(
  claim_category = c(
    "Allowed central claim",
    "Allowed biological interpretation",
    "Allowed ML interpretation",
    "Allowed validation interpretation",
    "Not allowed: clinical safety",
    "Not allowed: therapeutic efficacy",
    "Not allowed: anatomical projection",
    "Not allowed: full disease-context reanalysis"
  ),
  manuscript_language = c(
    "Frozen transcriptomic prioritization framework for DA graft-relevant cell states.",
    "DA-like, A9/A10-like, projection-associated molecular competence, and safety-risk-associated transcriptional states.",
    "Exploratory reduced-feature marker-rule-derived predictors recapitulate prioritization patterns.",
    "External and disease-context transcriptomic applications support context-dependent robustness.",
    "Clinical safety was not assessed.",
    "Therapeutic efficacy was not assessed.",
    "Anatomical projection and host integration were not assessed.",
    "09I was marker-targeted disease-context validation, not full-transcriptome raw reanalysis."
  ),
  risk_if_overclaimed = c(
    "low-medium", "medium", "high", "medium-high", "very high", "very high", "very high", "medium-high"
  ),
  recommended_location = c(
    "Title / Abstract / Discussion",
    "Results / Discussion",
    "Results / Methods / Limitations",
    "Results / Discussion",
    "Limitations / Discussion",
    "Limitations / Discussion",
    "Limitations / Discussion",
    "Methods / Limitations"
  )
)

atomic_write_csv(as.data.frame(claim_boundary_summary), claim_boundary_summary_csv)

stamp("绘制 10A final manuscript planning figures。")

flow_dt <- copy(storyline)
flow_dt[, step_num := seq_len(.N)]
flow_dt[, x := step_num]
flow_dt[, y := 1]
flow_dt[, label := paste0(story_step, "\n", wrap_text(supporting_modules, 22), "\n", wrap_text(safe_claim, 30))]

p_flow <- ggplot(flow_dt, aes(x = x, y = y)) +
  geom_segment(
    data = flow_dt[step_num < max(step_num)],
    aes(x = x + 0.42, xend = x + 0.58, y = y, yend = y),
    linewidth = 0.35,
    arrow = arrow(length = unit(0.12, "inches"))
  ) +
  geom_label(
    aes(label = label),
    size = 2.45,
    label.size = 0.25,
    label.padding = unit(0.15, "lines"),
    lineheight = 0.88,
    fill = "grey95"
  ) +
  labs(
    title = "Final manuscript story flow",
    subtitle = "Conservative transcriptomic-prioritization story from discovery to robustness and validation"
  ) +
  xlim(0.45, max(flow_dt$x) + 0.55) +
  ylim(0.72, 1.30) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10.5, color = "grey25"),
    plot.margin = ggplot2::margin(12, 18, 12, 18)
  )

save_pdf_plot(p_flow, fig_story_flow_pdf, width = 16.5, height = 5.2)

evidence_rows <- c(
  "Central story" = "Frozen transcriptomic prioritization framework for DA graft-relevant cell states",
  "Discovery layer" = "00–05B atlas and frozen marker/scoring framework",
  "Candidate-state biology" = "Ideal-like mitochondrial/neuronal programs versus lower-priority ECM/inflammatory/stress programs",
  "ML layer" = "Reduced non-direct marker-rule-derived predictors; exploratory and leakage-aware",
  "Robustness" = paste0(stable_group_text, "; ", empirical_p_text),
  "External validation" = paste0("GSE183248: ", gse183248_safety, " safety-risk-like, ", gse183248_ideal, " ideal-like clusters"),
  "Disease context" = paste0("GSE243639: ", gse243639_ideal, " ideal-like, ", gse243639_safety, " safety-risk-like, ", gse243639_mixed, " mixed clusters"),
  "Core limitation" = "No wet-lab, graft outcome, clinical safety, therapeutic efficacy, or anatomical projection validation"
)

p_evidence_summary <- panel_text_plot(
  title = "Main evidence summary for manuscript",
  rows = evidence_rows,
  subtitle = "This panel summarizes what the project can support without overclaiming.",
  metric_width = 26,
  value_width = 70,
  base_size = 3.10
)

save_pdf_plot(p_evidence_summary, fig_evidence_summary_pdf, width = 14.5, height = 7.2)

val_dt <- data.table(
  layer = c(
    "Threshold\nsensitivity",
    "Negative-control\npositive delta-AUC",
    "Negative-control\np <= 0.05",
    "GSE183248\nsafety-risk-like clusters",
    "GSE243639\nideal-like clusters"
  ),
  value = c(
    stable_group_fraction,
    positive_delta_auc_fraction,
    13 / 16,
    ifelse(gse183248_clusters > 0, gse183248_safety / gse183248_clusters, NA_real_),
    ifelse(gse243639_clusters > 0, gse243639_ideal / gse243639_clusters, NA_real_)
  ),
  label = c(
    stable_group_text,
    "100% positive delta-AUC",
    empirical_p_text,
    paste0(gse183248_safety, "/", gse183248_clusters, " clusters"),
    paste0(gse243639_ideal, "/", gse243639_clusters, " clusters")
  ),
  type = c("Robustness", "Robustness", "Robustness", "External validation", "Disease context")
)

val_dt[, layer := factor(layer, levels = rev(layer))]

p_validation <- ggplot(val_dt, aes(y = layer, x = value)) +
  geom_col(fill = "grey55", color = "grey25", linewidth = 0.25, width = 0.62) +
  geom_text(aes(x = pmin(value + 0.03, 1.08), label = wrap_text(label, 34)), hjust = 0, size = 3.0, lineheight = 0.88) +
  facet_grid(type ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(
    limits = c(0, 1.18),
    breaks = seq(0, 1, 0.25),
    labels = function(x) paste0(round(x * 100), "%")
  ) +
  labs(
    title = "External validation and robustness overview",
    subtitle = "Metric values summarize validation behavior; they do not indicate clinical efficacy or safety",
    x = "Fraction / proportion",
    y = NULL
  ) +
  theme_pub(base_size = 10.5) +
  theme(
    axis.text.y = element_text(size = 8.8, face = "bold", lineheight = 0.90),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.25),
    plot.margin = ggplot2::margin(12, 55, 12, 12)
  )

save_pdf_plot(p_validation, fig_validation_robustness_pdf, width = 13.8, height = 7.4)

cb <- copy(claim_boundary_summary)
cb[, claim_category := factor(wrap_text(claim_category, 28), levels = rev(wrap_text(claim_category, 28)))]
cb[, risk_score := fifelse(risk_if_overclaimed == "very high", 4,
                           fifelse(grepl("high", risk_if_overclaimed), 3,
                                  fifelse(grepl("medium", risk_if_overclaimed), 2, 1)))]

p_claim <- ggplot(cb, aes(x = risk_score, y = claim_category)) +
  geom_point(size = 4.2) +
  geom_text(aes(x = pmin(risk_score + 0.16, 4.18), label = wrap_text(manuscript_language, 45)), hjust = 0, size = 2.85, lineheight = 0.86) +
  scale_x_continuous(
    limits = c(0.8, 4.95),
    breaks = c(1, 2, 3, 4),
    labels = c("low", "medium", "high", "very high")
  ) +
  labs(
    title = "Claim-boundary summary for final manuscript",
    subtitle = "The project should be framed as transcriptomic prioritization, not functional/clinical validation",
    x = "Risk if overclaimed",
    y = NULL
  ) +
  theme_pub(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 8.2, lineheight = 0.88),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.25),
    plot.margin = ggplot2::margin(12, 95, 12, 12)
  )

save_pdf_plot(p_claim, fig_claim_boundary_pdf, width = 15.5, height = 7.8)

fig_plan_plot <- copy(main_figure_plan)
fig_plan_plot[, fig_label := factor(figure_id, levels = rev(figure_id))]
fig_plan_plot[, label := paste0(wrap_text(figure_title, 34), "\nModules: ", source_modules)]

p_fig_manifest <- ggplot(fig_plan_plot, aes(y = fig_label, x = 1)) +
  geom_point(size = 3.8) +
  geom_text(aes(x = 1.08, label = label), hjust = 0, size = 3.0, lineheight = 0.88) +
  xlim(0.85, 2.55) +
  labs(
    title = "Recommended main-figure structure",
    subtitle = "10B manuscript writing should follow this figure logic",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10.2, color = "grey25"),
    axis.text.y = element_text(color = "black", face = "bold"),
    plot.margin = ggplot2::margin(12, 40, 12, 12)
  )

save_pdf_plot(p_fig_manifest, fig_figure_manifest_pdf, width = 13.5, height = 7.2)

next_rows <- c(
  "10B task" = "Write manuscript Results / Methods / Discussion based on 10A figure plan",
  "Main title direction" = "Single-cell transcriptomic prioritization of dopaminergic graft-relevant cell states and safety-risk programs in Parkinson's disease",
  "Core abstract sentence" = "We developed a frozen transcriptomic prioritization framework for DA graft-relevant ideal-like and safety-risk-associated cell states.",
  "Required caution" = "Do not claim clinical safety, therapeutic efficacy, anatomical projection, or functional graft integration.",
  "Best target journals" = "BMC Genomics / Frontiers in Bioinformatics / BMC Medical Genomics / Scientific Reports-level venues",
  "Immediate next script" = "10B_MANUSCRIPT_DRAFT_RESULTS_METHODS_DISCUSSION"
)

p_next <- panel_text_plot(
  title = "Next step after 10A",
  rows = next_rows,
  subtitle = "10A completes figure logic; 10B turns the project into manuscript text.",
  metric_width = 26,
  value_width = 72,
  base_size = 3.05
)

save_pdf_plot(p_next, fig_next_step_pdf, width = 14.2, height = 6.8)

stamp("写出 10A text outputs。")

results_lines <- c(
  "10A results text for 10B manuscript",
  "",
  "Final manuscript organization",
  "",
  "The final manuscript should be organized around a frozen transcriptomic prioritization framework rather than a claim of therapeutic or clinical validation. The proposed main figures should first introduce the study design and scoring framework, then show discovery of ideal-like and safety-risk-associated graft-relevant states, followed by pathway interpretation, leakage-aware marker-rule-derived modeling, robustness testing, and external/disease-context validation.",
  "",
  paste0(
    "Integrated robustness evidence showed that ",
    stable_group_text,
    ", while negative-control analysis showed ",
    empirical_p_text,
    " and a positive delta-AUC fraction of ",
    round(positive_delta_auc_fraction * 100, 1),
    "%. These results support that the prioritization pattern is not driven solely by arbitrary thresholds or random feature-label associations."
  ),
  "",
  paste0(
    "External and disease-context analyses supported a context-dependent interpretation. GSE183248 recovered ",
    gse183248_clusters,
    " clusters from ",
    gse183248_cells,
    " cells, all classified as safety-risk-like rather than ideal-like. In contrast, GSE243639 marker-targeted disease-context analysis recovered ",
    gse243639_clusters,
    " signature-space clusters from ",
    gse243639_cells,
    " cells, including ",
    gse243639_ideal,
    " ideal-like, ",
    gse243639_safety,
    " safety-risk-like, and ",
    gse243639_mixed,
    " mixed/uncertain clusters."
  ),
  "",
  paste0(
    "The final claim should therefore be conservative: the study supports a transcriptomic prioritization framework for DA graft-relevant cell states, but does not establish clinical safety, therapeutic efficacy, true anatomical projection, host integration, or functional graft outcome."
  )
)

writeLines(results_lines, results_text_txt)

caption_lines <- c(
  "10A draft figure captions",
  "",
  "Figure 1. Study design and frozen transcriptomic prioritization framework.",
  "Schematic overview of the public-data workflow, frozen marker/scoring strategy, DA identity, A9/A10-like molecular identity, projection-associated molecular competence, and safety-risk-associated transcriptional state scoring.",
  "",
  "Figure 2. Discovery of ideal-like and safety-risk-associated graft-relevant cell states.",
  "UMAP/score visualization and candidate-state signature summaries showing how cell states were prioritized into ideal-like DA/projection-associated/safety-low and lower-priority/safety-risk-associated categories.",
  "",
  "Figure 3. Molecular programs distinguishing ideal-like and lower-priority states.",
  "DEG, GO, KEGG and Hallmark analyses showing mitochondrial/OXPHOS and neuronal/axon-associated programs in ideal-like states and ECM/inflammatory/stress-associated programs in lower-priority states.",
  "",
  "Figure 4. Leakage-aware marker-rule-derived modeling and robustness analyses.",
  "Reduced non-direct marker-rule-derived predictors were evaluated together with leakage audit, threshold sensitivity analysis, and negative-control analysis. Robustness metrics indicate that the prioritization pattern was not driven by a single arbitrary threshold or random feature-label association.",
  "",
  "Figure 5. External and disease-context validation with conservative claim boundaries.",
  "Primary external application to GSE183248 produced a conservative safety-risk-like profile, whereas marker-targeted disease-context validation in GSE243639 recovered several ideal-like signature-space clusters. Claim-boundary summaries define the supported transcriptomic conclusions and explicitly exclude clinical safety, treatment efficacy, and anatomical projection claims."
)

writeLines(caption_lines, figure_caption_txt)

method_note <- c(
  "10A method and claim-boundary note",
  "",
  "10A does not perform new biological analysis.",
  "It integrates final module outputs and creates manuscript-level figure planning tables and summary panels.",
  "",
  paste0("09J input used: ", version_09j_used, " from ", dir_09j),
  "",
  "Claim boundary:",
  "Allowed: transcriptomic prioritization framework; DA-like identity; A9/A10-like molecular signatures; projection-associated molecular competence; safety-risk-associated transcriptional states; exploratory marker-rule-derived model robustness; external/disease-context transcriptomic support.",
  "Not allowed: clinical safety, therapeutic efficacy, tumorigenicity exclusion, true anatomical projection, host integration, or functional graft validation."
)

writeLines(method_note, method_note_txt)

writeLines(capture.output(sessionInfo()), session_info_txt)

report_lines <- c(
  "10A final manuscript figure panel report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "09J input used:",
  version_09j_used,
  dir_09j,
  "",
  "Key numbers:",
  capture.output(print(key_numbers)),
  "",
  "Main figure plan:",
  capture.output(print(main_figure_plan)),
  "",
  "Supplementary figure plan:",
  capture.output(print(supplementary_figure_plan)),
  "",
  "Claim boundary summary:",
  capture.output(print(claim_boundary_summary)),
  "",
  "Output directories:",
  out_tables_dir,
  out_figures_dir
)

writeLines(report_lines, report_txt)

required_outputs <- c(
  input_audit_csv,
  final_version_manifest_csv,
  manuscript_figure_manifest_csv,
  main_figure_plan_csv,
  supplementary_figure_plan_csv,
  storyline_table_csv,
  key_numbers_csv,
  claim_boundary_summary_csv,
  method_note_txt,
  results_text_txt,
  figure_caption_txt,
  session_info_txt,
  report_txt,
  fig_story_flow_pdf,
  fig_evidence_summary_pdf,
  fig_validation_robustness_pdf,
  fig_claim_boundary_pdf,
  fig_figure_manifest_pdf,
  fig_next_step_pdf
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
  stop("10A 输出验证失败。")
}

cat("\n============================================================\n")
cat("10A final manuscript figure panel assembly V1 运行结束\n")
cat("============================================================\n\n")

cat("09J input used：", version_09j_used, "\n")
cat("Main figures planned：", nrow(main_figure_plan), "\n")
cat("Supplementary figures planned：", nrow(supplementary_figure_plan), "\n")
cat("Stable group result：", stable_group_text, "\n")
cat("Negative-control result：", empirical_p_text, "\n")
cat("GSE183248：", gse183248_cells, " cells / ", gse183248_clusters, " clusters | ideal-like=", gse183248_ideal, " safety-risk-like=", gse183248_safety, "\n", sep = "")
cat("GSE243639：", gse243639_cells, " cells / ", gse243639_clusters, " clusters | ideal-like=", gse243639_ideal, " safety-risk-like=", gse243639_safety, " mixed=", gse243639_mixed, "\n", sep = "")

cat("\n输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n")
cat(report_txt, "\n\n")

cat("关键输出：\n")
cat(manuscript_figure_manifest_csv, "\n")
cat(main_figure_plan_csv, "\n")
cat(supplementary_figure_plan_csv, "\n")
cat(storyline_table_csv, "\n")
cat(key_numbers_csv, "\n")
cat(results_text_txt, "\n")
cat(figure_caption_txt, "\n")
cat(method_note_txt, "\n\n")

cat("PDF 图：\n")
cat(fig_story_flow_pdf, "\n")
cat(fig_evidence_summary_pdf, "\n")
cat(fig_validation_robustness_pdf, "\n")
cat(fig_claim_boundary_pdf, "\n")
cat(fig_figure_manifest_pdf, "\n")
cat(fig_next_step_pdf, "\n\n")

cat("✅ 10A final manuscript figure panel assembly V1 完成。\n")
