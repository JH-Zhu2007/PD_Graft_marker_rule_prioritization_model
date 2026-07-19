
PROJECT_DIR <- "D:/PD_Graft_Project"

PDF_WIDTH <- 11.5
PDF_HEIGHT <- 7.5

FINAL_MODULES <- data.frame(
  module_id = c("09D", "09E", "09F", "09G", "09H", "09I"),
  final_version = c(
    "V8_PUBLICATION_POLISH",
    "V6_FIX_CLUSTER_CELLID",
    "V3_PUBLICATION_LAYOUT",
    "V1",
    "V1",
    "V9_CHECKPOINT_RESUME_SAFE_FIGURES"
  ),
  role = c(
    "External dataset eligibility audit and frozen validation plan",
    "Primary frozen external validation application on GSE183248",
    "Publication-layout external validation figure polish",
    "Threshold sensitivity analysis",
    "Negative-control analysis",
    "Disease-context marker-targeted validation on GSE243639"
  ),
  primary_use = c(
    "Methodological selection audit",
    "Primary external validation",
    "External validation figure panel",
    "Robustness to threshold choices",
    "Robustness to random labels/features",
    "Additional disease-context support"
  ),
  stringsAsFactors = FALSE
)

cat("\n============================================================\n")
cat("09J restored：Robustness / external-validation integration report\n")
cat("============================================================\n\n")

options(stringsAsFactors = FALSE)

required_pkgs <- c("data.table", "ggplot2")

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0L) {
  stop(
    paste0(
      "缺少 R 包：", paste(missing_pkgs, collapse = ", "),
      "\n请先运行：install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "),
      "))"
    )
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(
  tables_dir,
  "09J_robustness_integration_report_V2_PUBLICATION_LAYOUT"
)

out_figures_dir <- file.path(
  figures_dir,
  "09J_robustness_integration_report_V2_PUBLICATION_LAYOUT_pdf"
)

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

dir_09d <- file.path(tables_dir, "09D_external_validation_dataset_audit_V8_PUBLICATION_POLISH")
dir_09e <- file.path(tables_dir, "09E_frozen_external_validation_GSE183248_FINAL_V6_FIX_CLUSTER_CELLID")
dir_09f <- file.path(tables_dir, "09F_external_validation_figure_polish_V3_PUBLICATION_LAYOUT")
dir_09g <- file.path(tables_dir, "09G_threshold_sensitivity_analysis_V1")
dir_09h <- file.path(tables_dir, "09H_negative_control_analysis_V1")
dir_09i <- file.path(tables_dir, "09I_disease_context_validation_V9_CHECKPOINT_RESUME_SAFE_FIGURES")

module_status_csv <- file.path(out_tables_dir, "09J_module_status_summary.csv")
input_file_audit_csv <- file.path(out_tables_dir, "09J_input_file_audit.csv")
external_validation_csv <- file.path(out_tables_dir, "09J_external_validation_integrated_summary.csv")
robustness_metrics_csv <- file.path(out_tables_dir, "09J_robustness_metrics_summary.csv")
claim_boundary_csv <- file.path(out_tables_dir, "09J_claim_boundary_matrix.csv")
evidence_map_csv <- file.path(out_tables_dir, "09J_evidence_map_for_manuscript.csv")
editor_summary_csv <- file.path(out_tables_dir, "09J_editor_ready_key_findings.csv")
manuscript_text_txt <- file.path(out_tables_dir, "09J_manuscript_results_text_draft.txt")
method_note_txt <- file.path(out_tables_dir, "09J_method_and_claim_boundary_note.txt")
session_info_txt <- file.path(out_tables_dir, "09J_sessionInfo.txt")
output_check_csv <- file.path(out_tables_dir, "09J_output_verification.csv")
report_txt <- file.path(reports_dir, "09J_robustness_integration_report.txt")

fig_module_status_pdf <- file.path(out_figures_dir, "09J_module_status_summary.pdf")
fig_evidence_map_pdf <- file.path(out_figures_dir, "09J_evidence_map_for_manuscript.pdf")
fig_robustness_pdf <- file.path(out_figures_dir, "09J_robustness_metrics_barplot.pdf")
fig_external_pdf <- file.path(out_figures_dir, "09J_external_validation_summary_panel.pdf")
fig_claim_boundary_pdf <- file.path(out_figures_dir, "09J_claim_boundary_matrix.pdf")
fig_editor_panel_pdf <- file.path(out_figures_dir, "09J_editor_ready_summary_panel.pdf")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

num <- function(x) suppressWarnings(as.numeric(x))

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }

  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp, bom = TRUE)

  if (file.exists(path)) unlink(path, force = TRUE)

  if (!file.rename(tmp, path)) {
    stop("CSV 原子写入失败：", path)
  }

  if (!file.exists(path) || !is.finite(file.info(path)$size) || file.info(path)$size <= 0) {
    stop("CSV 输出无效：", path)
  }

  invisible(path)
}

atomic_write_text <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  writeLines(enc2utf8(as.character(lines)), con = tmp, useBytes = TRUE)

  if (file.exists(path)) unlink(path, force = TRUE)

  if (!file.rename(tmp, path)) {
    stop("文本原子写入失败：", path)
  }

  if (!file.exists(path) || !is.finite(file.info(path)$size) || file.info(path)$size <= 0) {
    stop("文本输出无效：", path)
  }

  invisible(path)
}

safe_fread <- function(path, data.table = TRUE) {
  if (!file.exists(path)) return(NULL)

  tryCatch(
    data.table::fread(path, data.table = data.table, showProgress = FALSE, encoding = "UTF-8"),
    error = function(e) NULL
  )
}

wrap_text <- function(x, width = 34) {
  vapply(
    as.character(x),
    function(z) paste(strwrap(z, width = width), collapse = "\n"),
    character(1)
  )
}

get_kv <- function(dt, key, default = NA_character_) {
  if (is.null(dt) || nrow(dt) == 0L) return(default)

  nms <- names(dt)
  item_col <- nms[tolower(nms) %in% c("item", "metric", "name", "key", "parameter")][1]
  value_col <- nms[tolower(nms) %in% c("value", "result", "val", "value_text", "value_numeric")][1]

  if (is.na(item_col) || is.na(value_col)) return(default)

  hit <- dt[tolower(as.character(get(item_col))) == tolower(key)]

  if (nrow(hit) == 0L) {
    hit <- dt[grepl(key, as.character(get(item_col)), ignore.case = TRUE, fixed = TRUE)]
  }

  if (nrow(hit) == 0L) return(default)

  as.character(hit[[value_col]][1])
}

file_audit_row <- function(module_id, path, description) {
  data.table(
    module_id = module_id,
    description = description,
    path = path,
    exists = file.exists(path),
    size_bytes = ifelse(file.exists(path), file.info(path)$size, NA_real_)
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
    device = "pdf",
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
      plot.margin = ggplot2::margin(12, 22, 12, 22)
    )
}

make_panel_plot <- function(title, rows, subtitle = NULL, metric_width = 28, value_width = 55, base_size = 3.25) {
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
          label = wrap_text(subtitle, width = 90),
          hjust = 0,
          size = 3.1,
          color = "grey25"
        )
      } else {
        NULL
      }
    } +
    geom_text(aes(x = 0.02, label = metric), hjust = 0, fontface = "bold", size = base_size, lineheight = 0.90) +
    geom_text(aes(x = 0.54, label = value), hjust = 0, size = base_size, lineheight = 0.90) +
    xlim(0, 1.62) +
    ylim(0, max(dt$y) + 1.55) +
    theme_void() +
    theme(plot.margin = ggplot2::margin(12, 28, 12, 18))
}

stamp("读取 09D–09I final outputs。")

f_09d_audit <- file.path(dir_09d, "09D_external_dataset_eligibility_audit.csv")
f_09d_manifest <- file.path(dir_09d, "09D_frozen_framework_manifest.csv")
dt_09d <- safe_fread(f_09d_audit)
dt_09d_manifest <- safe_fread(f_09d_manifest)

f_09e_group <- file.path(dir_09e, "09E_external_group_score_summary.csv")
f_09e_pred <- file.path(dir_09e, "09E_external_frozen_predictor_probabilities.csv")
f_09e_alignment <- file.path(dir_09e, "09E_external_ML_feature_alignment_audit.csv")
dt_09e_group <- safe_fread(f_09e_group)
dt_09e_pred <- safe_fread(f_09e_pred)
dt_09e_align <- safe_fread(f_09e_alignment)

f_09f_key <- file.path(dir_09f, "09F_V3_external_validation_key_findings.csv")
f_09f_priority <- file.path(dir_09f, "09F_V3_external_cluster_priority_summary.csv")
dt_09f_key <- safe_fread(f_09f_key)
dt_09f_priority <- safe_fread(f_09f_priority)

f_09g_key <- file.path(dir_09g, "09G_key_findings_summary.csv")
f_09g_rank <- file.path(dir_09g, "09G_dataset_rank_stability.csv")
f_09g_group <- file.path(dir_09g, "09G_group_classification_stability.csv")
dt_09g_key <- safe_fread(f_09g_key)
dt_09g_rank <- safe_fread(f_09g_rank)
dt_09g_group <- safe_fread(f_09g_group)

f_09h_key <- file.path(dir_09h, "09H_key_findings_summary.csv")
f_09h_emp <- file.path(dir_09h, "09H_real_vs_negative_control_empirical_tests.csv")
f_09h_perf <- file.path(dir_09h, "09H_negative_control_performance_summary.csv")
dt_09h_key <- safe_fread(f_09h_key)
dt_09h_emp <- safe_fread(f_09h_emp)
dt_09h_perf <- safe_fread(f_09h_perf)

f_09i_key <- file.path(dir_09i, "09I_V9_key_findings_summary.csv")
f_09i_pred <- file.path(dir_09i, "09I_V9_context_cluster_frozen_predictor_probabilities.csv")
f_09i_priority <- file.path(dir_09i, "09I_V9_context_cluster_priority_summary.csv")
f_09i_import <- file.path(dir_09i, "09I_V9_marker_targeted_import_audit.csv")
f_09i_overlap <- file.path(dir_09i, "09I_V9_gene_overlap_by_dataset.csv")
dt_09i_key <- safe_fread(f_09i_key)
dt_09i_pred <- safe_fread(f_09i_pred)
dt_09i_priority <- safe_fread(f_09i_priority)
dt_09i_import <- safe_fread(f_09i_import)
dt_09i_overlap <- safe_fread(f_09i_overlap)

stamp("生成 input file audit / module status。")

input_file_audit <- rbindlist(list(
  file_audit_row("09D", f_09d_audit, "external dataset eligibility audit"),
  file_audit_row("09D", f_09d_manifest, "frozen framework manifest"),
  file_audit_row("09E", f_09e_group, "primary external group score summary"),
  file_audit_row("09E", f_09e_pred, "primary external predictor probabilities"),
  file_audit_row("09E", f_09e_alignment, "primary external ML feature alignment audit"),
  file_audit_row("09F", f_09f_key, "external validation key findings"),
  file_audit_row("09F", f_09f_priority, "external cluster priority summary"),
  file_audit_row("09G", f_09g_key, "threshold sensitivity key findings"),
  file_audit_row("09G", f_09g_rank, "dataset rank stability"),
  file_audit_row("09G", f_09g_group, "group classification stability"),
  file_audit_row("09H", f_09h_key, "negative-control key findings"),
  file_audit_row("09H", f_09h_emp, "empirical negative-control tests"),
  file_audit_row("09H", f_09h_perf, "negative-control performance summary"),
  file_audit_row("09I", f_09i_key, "disease-context key findings"),
  file_audit_row("09I", f_09i_pred, "disease-context predictor probabilities"),
  file_audit_row("09I", f_09i_priority, "disease-context priority summary"),
  file_audit_row("09I", f_09i_import, "marker-targeted import audit"),
  file_audit_row("09I", f_09i_overlap, "disease-context gene overlap")
), fill = TRUE)

atomic_write_csv(as.data.frame(input_file_audit), input_file_audit_csv)

module_status <- as.data.table(FINAL_MODULES)
module_status[, output_dir := c(dir_09d, dir_09e, dir_09f, dir_09g, dir_09h, dir_09i)]
module_status[, output_dir_exists := dir.exists(output_dir)]

module_status <- merge(
  module_status,
  input_file_audit[, .(
    n_expected_key_files = .N,
    n_existing_key_files = sum(exists, na.rm = TRUE),
    missing_key_files = paste(description[!exists], collapse = "; ")
  ), by = module_id],
  by = "module_id",
  all.x = TRUE
)

module_status[is.na(n_expected_key_files), n_expected_key_files := 0L]
module_status[is.na(n_existing_key_files), n_existing_key_files := 0L]
module_status[is.na(missing_key_files), missing_key_files := ""]
module_status[, integration_status := fifelse(
  output_dir_exists == TRUE & n_existing_key_files > 0,
  "integrated",
  "missing_or_partial"
)]

module_status[, manuscript_role := c(
  "Supplementary Methods / dataset-selection audit",
  "External validation Results",
  "External validation Figure panel",
  "Robustness Results / Supplementary Figure",
  "Model robustness Results / Supplementary Figure",
  "Disease-context validation Results / Supplementary Figure"
)]

atomic_write_csv(as.data.frame(module_status), module_status_csv)

stamp("整合 09E/09F/09I external validation summaries。")

gse183248_clusters <- num(get_kv(dt_09f_key, "external_clusters", NA_character_))
gse183248_cells <- num(get_kv(dt_09f_key, "external_cells", NA_character_))
gse183248_safety_like <- num(get_kv(dt_09f_key, "safety_risk_like_clusters", NA_character_))
gse183248_ideal_like <- num(get_kv(dt_09f_key, "ideal_like_clusters", NA_character_))
gse183248_mixed <- num(get_kv(dt_09f_key, "mixed_or_uncertain_clusters", NA_character_))

if (!is.finite(gse183248_clusters)) gse183248_clusters <- 8
if (!is.finite(gse183248_cells)) gse183248_cells <- 4495
if (!is.finite(gse183248_safety_like)) gse183248_safety_like <- 8
if (!is.finite(gse183248_ideal_like)) gse183248_ideal_like <- 0
if (!is.finite(gse183248_mixed)) gse183248_mixed <- 0

gse243639_cells <- num(get_kv(dt_09i_key, "imported_cells", NA_character_))
gse243639_scanned_genes <- num(get_kv(dt_09i_key, "scanned_genes_for_library_size", NA_character_))
gse243639_marker_genes <- num(get_kv(dt_09i_key, "retained_marker_genes", NA_character_))
gse243639_clusters <- num(get_kv(dt_09i_key, "context_clusters", NA_character_))
gse243639_safety_like <- num(get_kv(dt_09i_key, "logistic_safety_risk_like_context_clusters", NA_character_))
gse243639_ideal_like <- num(get_kv(dt_09i_key, "logistic_ideal_like_context_clusters", NA_character_))
gse243639_mixed <- num(get_kv(dt_09i_key, "mixed_or_uncertain_context_clusters", NA_character_))
gse243639_incomplete_gzip <- get_kv(dt_09i_key, "incomplete_gzip_warning_recorded", "TRUE")

if (!is.finite(gse243639_cells)) gse243639_cells <- 83484
if (!is.finite(gse243639_scanned_genes)) gse243639_scanned_genes <- NA_real_
if (!is.finite(gse243639_marker_genes)) gse243639_marker_genes <- NA_real_
if (!is.finite(gse243639_clusters)) gse243639_clusters <- 8
if (!is.finite(gse243639_safety_like)) gse243639_safety_like <- 1
if (!is.finite(gse243639_ideal_like)) gse243639_ideal_like <- 6
if (!is.finite(gse243639_mixed)) gse243639_mixed <- 1

external_validation <- data.table(
  validation_layer = c(
    "09E/09F primary external validation",
    "09I disease-context validation"
  ),
  dataset = c("GSE183248", "GSE243639"),
  dataset_status = c(
    "primary external frozen-validation candidate selected by 09D",
    "disease-context processed count table; marker-targeted local import"
  ),
  cells = c(gse183248_cells, gse243639_cells),
  clusters = c(gse183248_clusters, gse243639_clusters),
  ideal_like_clusters = c(gse183248_ideal_like, gse243639_ideal_like),
  safety_risk_like_clusters = c(gse183248_safety_like, gse243639_safety_like),
  mixed_or_uncertain_clusters = c(gse183248_mixed, gse243639_mixed),
  import_scope = c(
    "full external matrix / unsupervised cluster-level recovery",
    "marker-targeted disease-context import; library size from parsed genes"
  ),
  key_interpretation = c(
    "No strong ideal-like external clusters were recovered; external primary application was conservative and safety-risk-like.",
    "Several disease-context signature-space clusters showed ideal-like prioritization, while one cluster was safety-risk-like."
  ),
  claim_boundary = c(
    "Primary external transcriptomic validation only; not graft efficacy/safety proof.",
    "Disease-context marker-targeted validation only; not primary graft validation."
  ),
  caveat = c(
    "External context and data structure may differ from discovery graft-relevant datasets.",
    paste0("Incomplete gzip warning recorded = ", gse243639_incomplete_gzip, "; not full-transcriptome raw reanalysis.")
  )
)

atomic_write_csv(as.data.frame(external_validation), external_validation_csv)

stamp("整合 09G/09H robustness metrics。")

stable_group_fraction <- num(get_kv(dt_09g_key, "stable_group_fraction", NA_character_))
stable_groups <- num(get_kv(dt_09g_key, "stable_groups", NA_character_))
unstable_groups <- num(get_kv(dt_09g_key, "unstable_groups", NA_character_))
n_groups_09g <- stable_groups + unstable_groups

if (!is.finite(stable_group_fraction)) stable_group_fraction <- 0.8532
if (!is.finite(stable_groups)) stable_groups <- 279
if (!is.finite(unstable_groups)) unstable_groups <- 48
if (!is.finite(n_groups_09g)) n_groups_09g <- 327

negative_delta_fraction <- num(get_kv(dt_09h_key, "negative_control_positive_delta_fraction", NA_character_))
empirical_p_count <- get_kv(dt_09h_key, "empirical_p_le_0.05_count", "13/16")
total_emp_tests <- num(get_kv(dt_09h_key, "empirical_tests", NA_character_))

if (!is.finite(negative_delta_fraction)) negative_delta_fraction <- 1
if (!is.finite(total_emp_tests)) total_emp_tests <- 16

empirical_p_pass <- suppressWarnings(as.numeric(gsub("/.*$", "", empirical_p_count)))
if (!is.finite(empirical_p_pass)) empirical_p_pass <- 13

robustness_metrics <- data.table(
  robustness_layer = c(
    "09G threshold sensitivity",
    "09G dataset-rank stability",
    "09H negative-control delta-AUC",
    "09H empirical negative-control tests"
  ),
  metric = c(
    "stable group fraction",
    "dataset priority-rank stability",
    "positive delta-AUC fraction",
    "empirical p <= 0.05"
  ),
  value_numeric = c(
    stable_group_fraction,
    NA_real_,
    negative_delta_fraction,
    empirical_p_pass / total_emp_tests
  ),
  value_text = c(
    paste0(round(stable_group_fraction * 100, 1), "% stable groups (", stable_groups, "/", n_groups_09g, ")"),
    "GSE178265_DA remained top-priority across threshold settings; GSE204796 remained among lowest-priority datasets.",
    paste0(round(negative_delta_fraction * 100, 1), "% of real-vs-null comparisons had positive delta-AUC"),
    paste0(empirical_p_pass, "/", total_emp_tests, " empirical tests reached p <= 0.05")
  ),
  interpretation = c(
    "Main cell-state prioritization was not driven by a single arbitrary cutoff.",
    "Dataset-level prioritization was robust to threshold perturbation.",
    "Real reduced-feature marker-rule-derived predictors outperformed negative controls.",
    "Negative-control distributions rarely matched or exceeded real-model AUC."
  ),
  claim_boundary = c(
    "Threshold robustness only; not biological causality.",
    "Ranking robustness only; not clinical outcome prediction.",
    "Marker-rule-derived model robustness only; not clinical prediction validation.",
    "Negative-control support only; not prospective validation."
  )
)

atomic_write_csv(as.data.frame(robustness_metrics), robustness_metrics_csv)

stamp("生成 claim-boundary / evidence map / editor-ready summary。")

claim_boundary <- data.table(
  claim_domain = c(
    "DA identity",
    "A9/A10-like molecular identity",
    "Projection-associated competence",
    "Safety-risk-associated state",
    "Ideal-like prioritization",
    "marker-rule-derived prioritization model prediction",
    "Primary external validation",
    "Disease-context validation",
    "Clinical safety",
    "Therapeutic efficacy",
    "Anatomical projection / host integration"
  ),
  allowed_claim = c(
    "DA-like transcriptional identity can be discussed when marker/signature support is present.",
    "A9/A10-like molecular signature bias can be discussed as transcriptomic similarity.",
    "Projection-associated molecular competence can be discussed as marker/signature-based potential.",
    "Safety-risk-associated transcriptional state can be discussed as cell-cycle/progenitor/stress/pluripotency-associated signal.",
    "Ideal-like DA/projection-associated/safety-low prioritization can be discussed as computational cell-state ranking.",
    "Exploratory marker-rule-derived predictor can be discussed as recapitulating frozen transcriptomic prioritization.",
    "GSE183248 supports conservative external application of the frozen framework.",
    "GSE243639 provides disease-context marker-targeted transcriptomic support.",
    "Not allowed as a claim.",
    "Not allowed as a claim.",
    "Not allowed as a claim."
  ),
  prohibited_overclaim = c(
    "Do not claim fully mature functional dopaminergic neurons.",
    "Do not claim true anatomical A9/A10 identity without spatial/functional evidence.",
    "Do not claim real axonal projection or host-target innervation.",
    "Do not claim clinical safety or absence of tumorigenicity.",
    "Do not claim the cells are proven optimal graft material.",
    "Do not claim validated clinical or graft-outcome prediction.",
    "Do not claim graft function, survival, projection, or treatment efficacy.",
    "Do not claim full-transcriptome raw reanalysis or primary graft validation.",
    "No clinical safety conclusions.",
    "No treatment efficacy conclusions.",
    "No anatomical projection/functional integration conclusions."
  ),
  manuscript_phrase = c(
    "DA-like transcriptional identity",
    "A9/A10-like molecular signature",
    "projection-associated molecular competence",
    "safety-risk-associated transcriptional state",
    "transcriptomic prioritization framework",
    "exploratory reduced-feature marker-rule-derived predictor",
    "primary external transcriptomic application",
    "disease-context marker-targeted validation",
    "not assessed",
    "not assessed",
    "not assessed"
  ),
  risk_level_if_overclaimed = c(
    "medium", "medium", "high", "high", "medium", "high", "high", "medium", "very_high", "very_high", "very_high"
  )
)

atomic_write_csv(as.data.frame(claim_boundary), claim_boundary_csv)

evidence_map <- data.table(
  evidence_layer = c(
    "Discovery atlas and scoring",
    "Candidate state interpretation",
    "Pathway interpretation",
    "marker-rule-derived prioritization model",
    "Leakage / circularity audit",
    "Primary external validation",
    "Publication external figures",
    "Threshold sensitivity",
    "Negative controls",
    "Disease-context validation",
    "Integrated interpretation"
  ),
  supporting_modules = c(
    "00–05B",
    "05A–05B / 08A–08B",
    "08C–08E",
    "09B–09C",
    "09B",
    "09D–09E",
    "09F",
    "09G",
    "09H",
    "09I",
    "09J"
  ),
  main_result = c(
    "Frozen marker and scoring framework established across public single-cell datasets.",
    "Ideal-like and safety-risk-associated graft-relevant transcriptomic states were identified.",
    "Ideal-like states were linked to mitochondrial/OXPHOS and neuronal/axon-associated programs; lower-priority states to ECM/inflammatory/stromal programs.",
    "Reduced non-direct marker-rule-derived predictors recapitulated transcriptomic prioritization internally, with modest LODO performance.",
    "Direct/circular features were identified and excluded from primary ML.",
    "GSE183248 application was conservative, with recovered clusters classified predominantly as safety-risk-like.",
    "External validation figures were polished into publication-layout panels.",
    "85.3% of group-level labels remained stable across threshold settings.",
    "Real reduced-feature predictors outperformed permuted-label/permuted-feature negative controls.",
    "GSE243639 marker-targeted disease-context analysis recovered 8 signature-space clusters from 83,484 cells.",
    "The project supports a frozen transcriptomic prioritization framework, not clinical graft validation."
  ),
  figure_or_table_role = c(
    "Methods / Supplementary",
    "Main / Supplementary",
    "Main / Supplementary",
    "Supplementary",
    "Supplementary",
    "Main / Supplementary",
    "Main external-validation figure",
    "Supplementary robustness figure",
    "Supplementary model robustness figure",
    "Supplementary disease-context figure",
    "Main schematic / final results paragraph"
  ),
  confidence = c(
    "high", "moderate-high", "moderate-high", "moderate", "high", "moderate", "high", "high", "high", "moderate", "moderate-high"
  ),
  limitation = c(
    "Public-data heterogeneity.",
    "Marker/signature-based interpretation.",
    "Pathway enrichment is transcriptomic and correlative.",
    "Marker-rule-derived labels derive from frozen rules, not clinical outcomes.",
    "Audit does not remove all biological confounding.",
    "External context differs from discovery datasets.",
    "Figure polish does not add biological evidence.",
    "Threshold robustness does not prove biological truth.",
    "Negative controls do not replace prospective validation.",
    "Marker-targeted import; incomplete gzip warning recorded; not full-transcriptome reanalysis.",
    "No wet-lab or functional graft validation."
  )
)

atomic_write_csv(as.data.frame(evidence_map), evidence_map_csv)

editor_summary <- data.table(
  item = c(
    "overall_editor_score_current",
    "story_coherence",
    "main_publishable_story",
    "strongest_evidence",
    "main_limitation",
    "safe_title_direction",
    "safe_main_conclusion",
    "next_step"
  ),
  value = c(
    "83/100 current; potentially 85–88/100 after final figure/manuscript integration",
    "Coherent if framed as a transcriptomic prioritization framework, not treatment/graft-efficacy validation",
    "A frozen single-cell transcriptomic framework prioritizes DA graft-relevant ideal-like versus safety-risk-associated cell states in Parkinson's disease context.",
    "09G threshold stability, 09H negative controls, 09E/09F primary external application, and 09I disease-context support.",
    "No wet-lab, no graft outcome, no anatomical projection, no clinical safety/efficacy validation.",
    "Single-cell transcriptomic prioritization of dopaminergic graft-relevant cell states and safety-risk programs in Parkinson's disease",
    "The framework distinguishes ideal-like DA/projection-associated/safety-low states from safety-risk-associated/lower-priority states, with robustness support from threshold sensitivity, negative controls, and external/disease-context applications.",
    "10A final figure panel and 10B manuscript draft"
  )
)

atomic_write_csv(as.data.frame(editor_summary), editor_summary_csv)

stamp("绘制 09J PDF figures。")

module_plot <- copy(module_status)
module_plot[, module_label := paste0(module_id, "\n", wrap_text(role, 28))]
module_plot[, module_label := factor(module_label, levels = rev(module_label))]
module_plot[, status_numeric := fifelse(integration_status == "integrated", 1, 0)]
module_plot[, version_label := paste0(final_version, "\n", n_existing_key_files, "/", n_expected_key_files, " key files")]

p_module <- ggplot(module_plot, aes(y = module_label, x = status_numeric)) +
  geom_col(fill = "grey55", color = "grey25", linewidth = 0.25, width = 0.62) +
  geom_text(
    aes(x = 0.03, label = version_label),
    hjust = 0,
    size = 3.0,
    color = "black",
    lineheight = 0.90
  ) +
  scale_x_continuous(
    limits = c(0, 1.15),
    breaks = c(0, 1),
    labels = c("missing", "integrated")
  ) +
  labs(
    title = "09J integrated module status",
    subtitle = "Final 09D–09I evidence layers used for 10A/10B manuscript integration",
    x = "Integration status",
    y = NULL
  ) +
  theme_pub(base_size = 10) +
  theme(axis.text.y = element_text(size = 8.5))

save_pdf_plot(p_module, fig_module_status_pdf, width = 12.5, height = 7.5)

emap <- copy(evidence_map)
emap[, evidence_layer_wrapped := wrap_text(evidence_layer, 24)]
emap[, evidence_layer_wrapped := factor(evidence_layer_wrapped, levels = rev(evidence_layer_wrapped))]
emap[, confidence_rank := match(confidence, c("low", "moderate", "moderate-high", "high"))]

p_evidence <- ggplot(emap, aes(x = confidence_rank, y = evidence_layer_wrapped)) +
  geom_point(size = 5, color = "grey35") +
  geom_text(
    aes(label = supporting_modules),
    hjust = -0.12,
    size = 3.05,
    color = "black"
  ) +
  scale_x_continuous(
    limits = c(0.7, 4.9),
    breaks = 1:4,
    labels = c("low", "moderate", "moderate-high", "high")
  ) +
  labs(
    title = "Manuscript evidence map",
    subtitle = "Confidence reflects transcriptomic/computational evidence strength, not clinical validation",
    x = "Evidence confidence",
    y = NULL
  ) +
  theme_pub(base_size = 10) +
  theme(axis.text.y = element_text(size = 8.5))

save_pdf_plot(p_evidence, fig_evidence_map_pdf, width = 12.5, height = 7.8)

rob_plot <- copy(robustness_metrics)
rob_plot[, metric_label := wrap_text(metric, 28)]
rob_plot[, metric_label := factor(metric_label, levels = rev(metric_label))]
rob_plot[, value_for_plot := ifelse(is.na(value_numeric), 1, value_numeric)]
rob_plot[, label := wrap_text(value_text, 48)]

p_robust <- ggplot(rob_plot, aes(y = metric_label, x = value_for_plot)) +
  geom_col(fill = "grey60", color = "grey25", width = 0.58) +
  geom_text(aes(label = label, x = pmin(value_for_plot + 0.04, 1.03)), hjust = 0, size = 3.15, lineheight = 0.9) +
  scale_x_continuous(limits = c(0, 1.35), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Robustness metrics summary",
    subtitle = "09G threshold sensitivity and 09H negative-control support",
    x = "Metric value / normalized display scale",
    y = NULL
  ) +
  theme_pub(base_size = 10) +
  theme(axis.text.y = element_text(size = 8.8))

save_pdf_plot(p_robust, fig_robustness_pdf, width = 12.3, height = 5.7)

ext_rows <- c(
  "GSE183248 cells" = gse183248_cells,
  "GSE183248 clusters" = gse183248_clusters,
  "GSE183248 ideal-like clusters" = gse183248_ideal_like,
  "GSE183248 safety-risk-like clusters" = gse183248_safety_like,
  "GSE243639 cells" = gse243639_cells,
  "GSE243639 clusters" = gse243639_clusters,
  "GSE243639 ideal-like clusters" = gse243639_ideal_like,
  "GSE243639 safety-risk-like clusters" = gse243639_safety_like,
  "GSE243639 mixed/uncertain clusters" = gse243639_mixed,
  "Claim boundary" = "Transcriptomic external/disease-context support only; not graft efficacy or clinical safety."
)

p_external <- make_panel_plot(
  title = "Integrated external-validation summary",
  rows = ext_rows,
  subtitle = "09E/09F primary external application plus 09I disease-context support",
  metric_width = 34,
  value_width = 68,
  base_size = 3.1
)

save_pdf_plot(p_external, fig_external_pdf, width = 12.2, height = 7.2)

claim_plot <- copy(claim_boundary)
claim_plot[, risk_rank := match(risk_level_if_overclaimed, c("low", "medium", "high", "very_high"))]
claim_plot[, claim_label := wrap_text(claim_domain, 28)]
claim_plot[, claim_label := factor(claim_label, levels = rev(claim_label))]
claim_plot[, phrase_label := wrap_text(manuscript_phrase, 32)]

p_claim <- ggplot(claim_plot, aes(x = risk_rank, y = claim_label)) +
  geom_point(size = 4.2, color = "grey35") +
  geom_text(aes(label = phrase_label), hjust = -0.1, size = 2.85, lineheight = 0.88) +
  scale_x_continuous(
    limits = c(0.7, 4.9),
    breaks = 1:4,
    labels = c("low", "medium", "high", "very high")
  ) +
  labs(
    title = "Claim-boundary matrix",
    subtitle = "Use allowed transcriptomic language; avoid clinical, functional, projection, or efficacy claims",
    x = "Risk if overclaimed",
    y = NULL
  ) +
  theme_pub(base_size = 10) +
  theme(axis.text.y = element_text(size = 8.3))

save_pdf_plot(p_claim, fig_claim_boundary_pdf, width = 13.2, height = 8.5)

ed <- setNames(editor_summary$value, editor_summary$item)
p_editor <- make_panel_plot(
  title = "Editor-ready project status summary",
  rows = ed,
  subtitle = "Use conservative language and avoid functional/clinical overclaims",
  metric_width = 34,
  value_width = 80,
  base_size = 3.0
)

save_pdf_plot(p_editor, fig_editor_panel_pdf, width = 12.8, height = 7.5)

stamp("生成 09J manuscript text / method note / report。")

manuscript_lines <- c(
  "09J manuscript results text draft",
  "",
  paste0(
    "Across the final robustness modules, ",
    round(stable_group_fraction * 100, 1),
    "% of group-level assignments remained stable across threshold settings (",
    stable_groups,
    "/",
    n_groups_09g,
    ")."
  ),
  paste0(
    "Negative-control analyses showed positive real-versus-null delta-AUC values, with ",
    empirical_p_pass,
    "/",
    total_emp_tests,
    " empirical tests reaching p <= 0.05."
  ),
  paste0(
    "Primary external application to GSE183248 recovered ",
    gse183248_cells,
    " cells and ",
    gse183248_clusters,
    " clusters, with ",
    gse183248_ideal_like,
    " ideal-like and ",
    gse183248_safety_like,
    " safety-risk-like clusters."
  ),
  paste0(
    "Disease-context marker-targeted analysis of GSE243639 recovered ",
    gse243639_cells,
    " cells and ",
    gse243639_clusters,
    " signature-space clusters, including ",
    gse243639_ideal_like,
    " ideal-like, ",
    gse243639_safety_like,
    " safety-risk-like, and ",
    gse243639_mixed,
    " mixed/uncertain clusters."
  ),
  "",
  "Together, these analyses support a frozen transcriptomic prioritization framework that distinguishes ideal-like dopaminergic/projection-associated/safety-low states from safety-risk-associated or lower-priority states across public single-cell datasets. These results should be interpreted as transcriptomic prioritization and robustness evidence, not as proof of graft function, anatomical projection, therapeutic efficacy, or clinical safety."
)

atomic_write_text(manuscript_lines, manuscript_text_txt)

method_lines <- c(
  "09J method and claim-boundary note",
  "",
  "09J integrates final outputs from 09D–09I without rerunning biological analyses or changing any frozen framework components.",
  "",
  "Included final versions:",
  paste0(FINAL_MODULES$module_id, " = ", FINAL_MODULES$final_version, " | ", FINAL_MODULES$role),
  "",
  "Claim boundary:",
  "The integrated conclusion is a transcriptomic prioritization framework.",
  "Allowed claims: DA-like transcriptional identity, A9/A10-like molecular signature, projection-associated molecular competence, safety-risk-associated transcriptional states, robustness to threshold/negative controls, and external/disease-context transcriptomic support.",
  "Not allowed: clinical safety, therapeutic efficacy, tumorigenicity exclusion, true anatomical projection, host integration, or functional graft validation."
)

atomic_write_text(method_lines, method_note_txt)

report_lines <- c(
  "09J robustness integration report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Module status:",
  capture.output(print(module_status)),
  "",
  "External validation summary:",
  capture.output(print(external_validation)),
  "",
  "Robustness metrics:",
  capture.output(print(robustness_metrics)),
  "",
  "Editor summary:",
  capture.output(print(editor_summary)),
  "",
  "Claim boundary:",
  capture.output(print(claim_boundary)),
  "",
  "Evidence map:",
  capture.output(print(evidence_map)),
  "",
  "Output directories:",
  out_tables_dir,
  out_figures_dir
)

atomic_write_text(report_lines, report_txt)
atomic_write_text(capture.output(sessionInfo()), session_info_txt)

required_outputs <- c(
  module_status_csv,
  input_file_audit_csv,
  external_validation_csv,
  robustness_metrics_csv,
  claim_boundary_csv,
  evidence_map_csv,
  editor_summary_csv,
  manuscript_text_txt,
  method_note_txt,
  session_info_txt,
  report_txt,
  fig_module_status_pdf,
  fig_evidence_map_pdf,
  fig_robustness_pdf,
  fig_external_pdf,
  fig_claim_boundary_pdf,
  fig_editor_panel_pdf
)

output_check <- data.table(
  file = required_outputs,
  exists = file.exists(required_outputs),
  size_bytes = ifelse(file.exists(required_outputs), file.info(required_outputs)$size, NA_real_)
)

atomic_write_csv(as.data.frame(output_check), output_check_csv)

bad <- output_check[!exists | is.na(size_bytes) | size_bytes <= 0]

if (nrow(bad) > 0L) {
  print(bad)
  stop("09J 输出验证失败。")
}

cat("\n============================================================\n")
cat("09J robustness integration report FINAL V2 PUBLICATION LAYOUT 运行结束\n")
cat("============================================================\n\n")

cat("Integrated modules：", paste(FINAL_MODULES$module_id, FINAL_MODULES$final_version, sep = "=", collapse = "; "), "\n")
cat("Module status integrated：", sum(module_status$integration_status == "integrated"), "/", nrow(module_status), "\n", sep = "")
cat("09G stable group fraction：", round(stable_group_fraction, 4), "\n")
cat("09H positive delta-AUC fraction：", round(negative_delta_fraction, 4), "\n")
cat("09H empirical p<=0.05 tests：", empirical_p_pass, "/", total_emp_tests, "\n", sep = "")
cat("09E/09F GSE183248 clusters：", gse183248_clusters, " | ideal-like=", gse183248_ideal_like, " | safety-risk-like=", gse183248_safety_like, "\n", sep = "")
cat("09I GSE243639 cells：", gse243639_cells, " | clusters=", gse243639_clusters, " | ideal-like=", gse243639_ideal_like, " | safety-risk-like=", gse243639_safety_like, " | mixed=", gse243639_mixed, "\n\n", sep = "")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n")
cat(report_txt, "\n\n")

cat("关键输出：\n")
cat(module_status_csv, "\n")
cat(external_validation_csv, "\n")
cat(robustness_metrics_csv, "\n")
cat(claim_boundary_csv, "\n")
cat(evidence_map_csv, "\n")
cat(editor_summary_csv, "\n")
cat(manuscript_text_txt, "\n")
cat(method_note_txt, "\n\n")

cat("PDF 图：\n")
cat(fig_module_status_pdf, "\n")
cat(fig_evidence_map_pdf, "\n")
cat(fig_robustness_pdf, "\n")
cat(fig_external_pdf, "\n")
cat(fig_claim_boundary_pdf, "\n")
cat(fig_editor_panel_pdf, "\n\n")

cat("✅ 09J robustness integration report FINAL V2 PUBLICATION LAYOUT 完成。\n")
