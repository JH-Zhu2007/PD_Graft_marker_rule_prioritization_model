# ============================================================
# 05B_SAFETY_RISK_SCORING_AND_CONTRAST.R
# ============================================================
# 目的：
#   接在 05A V2 后运行。
#
#   05B 是第二个核心创新模块：
#     Cell-state fate propensity + graft safety-risk modelling 的前置评分层
#
#   05B 做：
#     1. 基于 04B/04D 的 marker-category scores 构建 safety-risk composite
#     2. 汇总 group-level / object-level / dataset-level safety-risk
#     3. 和 05A 的 DA-like / A9-A10 / projection competence score 做对照
#     4. 找出：
#        - ideal-like groups：DA/projection 高，safety-risk 低
#        - risk-like groups：cycling/progenitor/pluripotency/stress 高
#        - mixed groups：DA/projection 与 safety-risk 同时高
#
# 重要严谨性：
#   safety-risk score 是 transcriptional safety-risk-associated state。
#   不能写成“肿瘤风险已被证明”。
#   不能写成“临床安全性预测已完成”。
#
# 输入：
#   04B_group_marker_category_scores.csv
#   04B_object_marker_category_scores.csv
#   04D_group_annotation_table.csv
#   05A_group_level_scores.csv
#   05A_object_level_scores.csv
#
# 输出：
#   05B_group_safety_risk_scores.csv
#   05B_object_safety_risk_scores.csv
#   05B_dataset_safety_risk_summary.csv
#   05B_DA_projection_vs_safety_contrast_groups.csv
#   05B_candidate_groups_for_story.csv
#   05B report
#
# 成功标志：
#   ✅ 05B safety-risk scoring and contrast 完成。
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"

# safety score 阈值，先用于候选筛选，不是最终生物学结论
SAFETY_LOW_MAX <- 0.20
SAFETY_HIGH_MIN <- 0.35

# ideal-like candidate 阈值
DA_HIGH_MIN <- 0.08
PROJECTION_HIGH_MIN <- 0.08

# mixed-risk candidate 阈值
DA_PRESENT_MIN <- 0.05
PROJECTION_PRESENT_MIN <- 0.05

# 输出每类 top group 数量
TOP_N_PER_DATASET_CLASS <- 50


# ============================================================
# 1. 加载包
# ============================================================

cat("\n============================================================\n")
cat("05B：safety-risk scoring and contrast\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 05B。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})


# ============================================================
# 2. 路径
# ============================================================

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_04B_group <- file.path(tables_dir, "04B_marker_expression", "04B_group_marker_category_scores.csv")
input_04B_object <- file.path(tables_dir, "04B_marker_expression", "04B_object_marker_category_scores.csv")
input_04D_group <- file.path(tables_dir, "04D_annotations", "04D_group_annotation_table.csv")
input_05A_group <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_group_level_scores.csv")
input_05A_object <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_object_level_scores.csv")
input_05A_audit <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_V2_final_audit_summary.csv")

out_tables_dir <- file.path(tables_dir, "05B_safety_risk_scoring")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

safety_signature_csv <- file.path(out_tables_dir, "05B_safety_signature_definition.csv")
group_safety_csv <- file.path(out_tables_dir, "05B_group_safety_risk_scores.csv")
object_safety_csv <- file.path(out_tables_dir, "05B_object_safety_risk_scores.csv")
dataset_safety_csv <- file.path(out_tables_dir, "05B_dataset_safety_risk_summary.csv")
contrast_groups_csv <- file.path(out_tables_dir, "05B_DA_projection_vs_safety_contrast_groups.csv")
candidate_story_csv <- file.path(out_tables_dir, "05B_candidate_groups_for_story.csv")
qc_audit_csv <- file.path(out_tables_dir, "05B_QC_audit_summary.csv")
report_txt <- file.path(reports_dir, "05B_safety_risk_scoring_and_contrast_report.txt")


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

ensure_col <- function(dt, col, value = NA_real_) {
  if (!col %in% colnames(dt)) dt[[col]] <- value
  dt
}

to_numeric_safe <- function(x) {
  suppressWarnings(as.numeric(x))
}

wide_category_scores <- function(group_dt) {
  dcast(
    group_dt,
    dataset + object_id + group_source + group_id + n_cells ~ category,
    value.var = "mean_score",
    fun.aggregate = max,
    fill = NA_real_
  )
}

wide_category_pct <- function(group_dt) {
  dcast(
    group_dt,
    dataset + object_id + group_source + group_id + n_cells ~ category,
    value.var = "pct_cells_score_gt0",
    fun.aggregate = max,
    fill = NA_real_
  )
}

classify_safety_contrast <- function(dt) {
  # required columns already created
  dt[
    ,
    safety_contrast_class_05B := fifelse(
      DA_like_composite_score >= DA_HIGH_MIN &
        projection_competence_composite_score >= PROJECTION_HIGH_MIN &
        safety_risk_composite_05B <= SAFETY_LOW_MAX,
      "ideal_DA_projection_high_safety_low",
      fifelse(
        safety_risk_composite_05B >= SAFETY_HIGH_MIN &
          DA_like_composite_score < DA_PRESENT_MIN,
        "high_safety_risk_low_DA",
        fifelse(
          safety_risk_composite_05B >= SAFETY_HIGH_MIN &
            DA_like_composite_score >= DA_PRESENT_MIN,
          "mixed_DA_or_projection_with_safety_risk",
          fifelse(
            projection_competence_composite_score >= PROJECTION_PRESENT_MIN &
              DA_like_composite_score < DA_PRESENT_MIN &
              safety_risk_composite_05B <= SAFETY_LOW_MAX,
            "projection_competence_without_DA_low_safety",
            "lower_priority_or_mixed"
          )
        )
      )
    )
  ]

  dt
}


# ============================================================
# 4. 读取输入
# ============================================================

stamp("读取 04B / 04D / 05A 输出。")

g04B <- as.data.table(read_csv_required(input_04B_group))
o04B <- as.data.table(read_csv_required(input_04B_object))
g04D <- as.data.table(read_csv_required(input_04D_group))
g05A <- as.data.table(read_csv_required(input_05A_group))
o05A <- as.data.table(read_csv_required(input_05A_object))
audit05A <- as.data.table(read_csv_optional(input_05A_audit))

needed_04B <- c("dataset", "object_id", "group_source", "group_id", "category", "mean_score", "pct_cells_score_gt0", "coverage_fraction")
if (!all(needed_04B %in% colnames(g04B))) {
  stop("04B group table 缺少必要列：", paste(setdiff(needed_04B, colnames(g04B)), collapse = ", "))
}

needed_05A <- c("dataset", "object_id", "annotation_04D_group_id", "annotation_04D_v1", "DA_like_composite_score", "projection_competence_composite_score", "A9_minus_A10_score_05A")
if (!all(needed_05A %in% colnames(g05A))) {
  stop("05A group table 缺少必要列：", paste(setdiff(needed_05A, colnames(g05A)), collapse = ", "))
}


# ============================================================
# 5. 定义 safety-risk score 组成
# ============================================================

stamp("定义 safety-risk score。")

safety_def <- data.frame(
  component = c(
    "cell_cycle_proliferation",
    "progenitor_neuroepithelial",
    "pluripotency_immature_risk",
    "stress_apoptosis_response",
    "extracellular_matrix_fibroblast",
    "vascular_pericyte_meningeal"
  ),
  weight = c(
    1.20,
    1.00,
    1.40,
    0.60,
    0.40,
    0.30
  ),
  interpretation = c(
    "Cycling/proliferating state; major safety-risk-associated signal.",
    "Neural progenitor/immature state; developmental fate propensity.",
    "Pluripotency/immature-risk marker signal; high-priority safety review.",
    "Stress/apoptosis response; not a cell type but can confound graft quality.",
    "ECM/fibroblast-like or mesenchymal state; off-target/stromal risk signal.",
    "Vascular/pericyte/meningeal-associated marker signal; off-target/stromal context."
  ),
  manuscript_caution = c(
    "Not direct tumorigenicity proof.",
    "Not automatically unsafe without proliferation/pluripotency.",
    "Requires strict manual marker validation.",
    "Stress signal should not be interpreted as lineage alone.",
    "Context-dependent; may reflect host/stromal cells.",
    "Context-dependent; may reflect host/stromal cells."
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(safety_def, safety_signature_csv)

safety_components <- safety_def$component
weights <- safety_def$weight
names(weights) <- safety_def$component


# ============================================================
# 6. group-level safety score from 04B
# ============================================================

stamp("计算 group-level safety-risk score。")

g_wide <- as.data.table(wide_category_scores(g04B))
g_pct <- as.data.table(wide_category_pct(g04B))

# 确保所有 component 存在
for (comp in safety_components) {
  g_wide <- ensure_col(g_wide, comp, NA_real_)
  g_pct <- ensure_col(g_pct, comp, NA_real_)
}

# weighted mean
score_mat <- as.matrix(g_wide[, safety_components, with = FALSE])
score_mat <- apply(score_mat, 2, to_numeric_safe)

weighted_score <- rep(NA_real_, nrow(g_wide))

for (i in seq_len(nrow(g_wide))) {
  vals <- as.numeric(score_mat[i, ])
  valid <- !is.na(vals)

  if (sum(valid) == 0L) {
    weighted_score[i] <- NA_real_
  } else {
    weighted_score[i] <- sum(vals[valid] * weights[valid]) / sum(weights[valid])
  }
}

g_wide[, safety_risk_composite_05B := weighted_score]

# component detail
g_wide[, safety_cell_cycle_score_05B := to_numeric_safe(cell_cycle_proliferation)]
g_wide[, safety_progenitor_score_05B := to_numeric_safe(progenitor_neuroepithelial)]
g_wide[, safety_pluripotency_score_05B := to_numeric_safe(pluripotency_immature_risk)]
g_wide[, safety_stress_score_05B := to_numeric_safe(stress_apoptosis_response)]
g_wide[, safety_ecm_score_05B := to_numeric_safe(extracellular_matrix_fibroblast)]
g_wide[, safety_vascular_score_05B := to_numeric_safe(vascular_pericyte_meningeal)]

# dominant component
component_values <- g_wide[, safety_components, with = FALSE]
dominant_component <- apply(component_values, 1, function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(NA_character_)
  safety_components[which.max(replace(x, is.na(x), -Inf))]
})

g_wide[, dominant_safety_component_05B := dominant_component]

g_wide[
  ,
  safety_risk_label_05B := fifelse(
    is.na(safety_risk_composite_05B),
    "safety_score_unavailable",
    fifelse(
      safety_risk_composite_05B >= SAFETY_HIGH_MIN,
      "high_safety_risk_associated_state",
      fifelse(
        safety_risk_composite_05B <= SAFETY_LOW_MAX,
        "low_safety_risk_signal_state",
        "intermediate_safety_risk_signal_state"
      )
    )
  )
]

# 保存纯 safety 表
group_safety_cols <- c(
  "dataset", "object_id", "group_source", "group_id", "n_cells",
  "safety_risk_composite_05B",
  "safety_risk_label_05B",
  "dominant_safety_component_05B",
  "safety_cell_cycle_score_05B",
  "safety_progenitor_score_05B",
  "safety_pluripotency_score_05B",
  "safety_stress_score_05B",
  "safety_ecm_score_05B",
  "safety_vascular_score_05B"
)

group_safety <- g_wide[, group_safety_cols, with = FALSE]

atomic_write_csv(as.data.frame(group_safety), group_safety_csv)


# ============================================================
# 7. Merge with 05A DA/projection scores
# ============================================================

stamp("合并 05A DA/projection scores，生成 contrast table。")

# 05A group table 用 annotation_04D_group_id；04B/04D 用 group_id
g05A2 <- copy(g05A)
g05A2[, group_id := as.character(annotation_04D_group_id)]

# 如果 group_id 是 NA，用 object_all
g05A2[is.na(group_id) | group_id == "", group_id := "object_all"]

# group_source 不一定一致，先按 dataset/object_id/group_id 合并
contrast <- merge(
  group_safety,
  g05A2,
  by = c("dataset", "object_id", "group_id"),
  all.x = TRUE,
  suffixes = c("_05B", "_05A")
)

# 如果某些 05A 不匹配，保留 safety 信息
needed_contrast_numeric <- c(
  "DA_like_composite_score",
  "projection_competence_composite_score",
  "DA_projection_competence_composite_score",
  "A9_minus_A10_score_05A"
)

for (col in needed_contrast_numeric) {
  if (!col %in% colnames(contrast)) contrast[[col]] <- NA_real_
}

contrast <- classify_safety_contrast(as.data.table(contrast))

# A9/A10 label
contrast[
  ,
  A9_A10_bias_label_05B := fifelse(
    is.na(A9_minus_A10_score_05A),
    "unknown",
    fifelse(
      A9_minus_A10_score_05A > 0.02,
      "A9_like_bias",
      fifelse(
        A9_minus_A10_score_05A < -0.02,
        "A10_like_bias",
        "A9_A10_mixed_or_unclear"
      )
    )
  )
]

# story priority
contrast[
  ,
  story_priority_05B := fifelse(
    safety_contrast_class_05B == "ideal_DA_projection_high_safety_low",
    "high_priority_positive_graft_like",
    fifelse(
      safety_contrast_class_05B == "high_safety_risk_low_DA",
      "high_priority_safety_risk",
      fifelse(
        safety_contrast_class_05B == "mixed_DA_or_projection_with_safety_risk",
        "high_priority_mixed_warning",
        "standard_or_low_priority"
      )
    )
  )
]

contrast <- contrast[
  order(
    dataset,
    -fifelse(is.na(DA_projection_competence_composite_score), -Inf, DA_projection_competence_composite_score),
    -fifelse(is.na(safety_risk_composite_05B), -Inf, safety_risk_composite_05B)
  )
]

atomic_write_csv(as.data.frame(contrast), contrast_groups_csv)


# ============================================================
# 8. Object-level safety score
# ============================================================

stamp("计算 object-level safety-risk score。")

object_safety <- group_safety[
  ,
  .(
    n_groups = .N,
    total_cells_represented = sum(n_cells, na.rm = TRUE),
    mean_safety_risk_composite_05B = weighted.mean(safety_risk_composite_05B, w = pmax(n_cells, 1), na.rm = TRUE),
    median_safety_risk_composite_05B = median(safety_risk_composite_05B, na.rm = TRUE),
    max_safety_risk_composite_05B = max(safety_risk_composite_05B, na.rm = TRUE),
    n_high_safety_groups = sum(safety_risk_label_05B == "high_safety_risk_associated_state", na.rm = TRUE),
    n_low_safety_groups = sum(safety_risk_label_05B == "low_safety_risk_signal_state", na.rm = TRUE),
    dominant_safety_component_object = names(sort(table(dominant_safety_component_05B), decreasing = TRUE))[1]
  ),
  by = .(dataset, object_id)
]

# merge 05A object
if (nrow(o05A) > 0 && all(c("dataset", "object_id") %in% colnames(o05A))) {
  keep_05A_cols <- intersect(
    c(
      "dataset", "object_id", "n_cells",
      "DA_like_composite_score",
      "projection_competence_composite_score",
      "DA_projection_competence_composite_score",
      "A9_minus_A10_score_05A",
      "dominant_annotation"
    ),
    colnames(o05A)
  )

  object_safety <- merge(
    object_safety,
    o05A[, keep_05A_cols, with = FALSE],
    by = c("dataset", "object_id"),
    all.x = TRUE
  )
}

object_safety[
  ,
  object_safety_contrast_class_05B := fifelse(
    mean_safety_risk_composite_05B <= SAFETY_LOW_MAX &
      DA_projection_competence_composite_score >= DA_HIGH_MIN,
    "object_level_DA_projection_high_safety_low",
    fifelse(
      mean_safety_risk_composite_05B >= SAFETY_HIGH_MIN,
      "object_level_high_safety_risk",
      "object_level_intermediate_or_mixed"
    )
  )
]

atomic_write_csv(as.data.frame(object_safety), object_safety_csv)


# ============================================================
# 9. Dataset-level summary
# ============================================================

stamp("计算 dataset-level safety-risk summary。")

dataset_safety <- object_safety[
  ,
  .(
    n_objects = .N,
    total_cells_represented = sum(total_cells_represented, na.rm = TRUE),
    mean_safety_risk_composite_05B = weighted.mean(mean_safety_risk_composite_05B, w = pmax(total_cells_represented, 1), na.rm = TRUE),
    median_object_safety_risk_05B = median(mean_safety_risk_composite_05B, na.rm = TRUE),
    max_object_safety_risk_05B = max(max_safety_risk_composite_05B, na.rm = TRUE),
    total_high_safety_groups = sum(n_high_safety_groups, na.rm = TRUE),
    total_low_safety_groups = sum(n_low_safety_groups, na.rm = TRUE),
    mean_DA_projection_competence = mean(DA_projection_competence_composite_score, na.rm = TRUE),
    mean_DA_like = mean(DA_like_composite_score, na.rm = TRUE),
    mean_projection_competence = mean(projection_competence_composite_score, na.rm = TRUE)
  ),
  by = dataset
][order(-mean_DA_projection_competence, mean_safety_risk_composite_05B)]

dataset_safety[
  ,
  dataset_story_class_05B := fifelse(
    mean_DA_projection_competence >= DA_HIGH_MIN &
      mean_safety_risk_composite_05B <= SAFETY_LOW_MAX,
    "dataset_with_favorable_DA_projection_vs_safety_profile",
    fifelse(
      mean_safety_risk_composite_05B >= SAFETY_HIGH_MIN,
      "dataset_with_high_safety_risk_signal",
      "dataset_with_intermediate_or_heterogeneous_profile"
    )
  )
]

atomic_write_csv(as.data.frame(dataset_safety), dataset_safety_csv)


# ============================================================
# 10. Candidate groups for story
# ============================================================

stamp("提取 story candidate groups。")

candidate_story <- contrast[
  story_priority_05B != "standard_or_low_priority"
]

# 每个 dataset/class 限制 top N，避免表太大
candidate_story <- candidate_story[
  ,
  head(.SD, TOP_N_PER_DATASET_CLASS),
  by = .(dataset, safety_contrast_class_05B)
]

candidate_story <- candidate_story[
  order(
    story_priority_05B,
    dataset,
    -DA_projection_competence_composite_score,
    -safety_risk_composite_05B
  )
]

atomic_write_csv(as.data.frame(candidate_story), candidate_story_csv)


# ============================================================
# 11. QC audit
# ============================================================

qc_audit <- data.frame(
  metric = c(
    "04B_group_rows",
    "05A_group_rows",
    "05B_group_safety_rows",
    "05B_contrast_group_rows",
    "05B_object_rows",
    "05B_dataset_rows",
    "05B_story_candidate_rows",
    "05A_blocking_failures_for_05B"
  ),
  value = c(
    nrow(g04B),
    nrow(g05A),
    nrow(group_safety),
    nrow(contrast),
    nrow(object_safety),
    nrow(dataset_safety),
    nrow(candidate_story),
    ifelse(nrow(audit05A) > 0 && "metric" %in% colnames(audit05A), {
      val <- audit05A$value[audit05A$metric == "blocking_failures_for_05B"]
      ifelse(length(val) == 0, NA, val[1])
    }, NA)
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(qc_audit, qc_audit_csv)


# ============================================================
# 12. 报告
# ============================================================

dataset_lines <- if (nrow(dataset_safety) > 0) {
  apply(as.data.frame(dataset_safety), 1, function(x) {
    paste0(
      x[["dataset"]],
      ": safety=",
      round(as.numeric(x[["mean_safety_risk_composite_05B"]]), 4),
      "; DA_projection=",
      round(as.numeric(x[["mean_DA_projection_competence"]]), 4),
      "; class=",
      x[["dataset_story_class_05B"]]
    )
  })
} else {
  "none"
}

candidate_summary <- if (nrow(candidate_story) > 0) {
  candidate_story[
    ,
    .(
      n_groups = .N,
      total_cells = sum(n_cells_05B, na.rm = TRUE)
    ),
    by = .(dataset, safety_contrast_class_05B)
  ][order(dataset, safety_contrast_class_05B)]
} else {
  data.table()
}

candidate_lines <- if (nrow(candidate_summary) > 0) {
  apply(as.data.frame(candidate_summary), 1, function(x) {
    paste0(
      x[["dataset"]],
      " / ",
      x[["safety_contrast_class_05B"]],
      ": groups=",
      x[["n_groups"]],
      "; cells=",
      x[["total_cells"]]
    )
  })
} else {
  "none"
}

report_lines <- c(
  "05B safety-risk scoring and contrast report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("Group safety rows: ", nrow(group_safety)),
  paste0("Contrast group rows: ", nrow(contrast)),
  paste0("Object safety rows: ", nrow(object_safety)),
  paste0("Dataset summary rows: ", nrow(dataset_safety)),
  paste0("Story candidate rows: ", nrow(candidate_story)),
  "",
  "Dataset-level summary:",
  dataset_lines,
  "",
  "Candidate story group summary:",
  candidate_lines,
  "",
  "Output files:",
  paste0("Safety signature definition: ", safety_signature_csv),
  paste0("Group safety-risk scores: ", group_safety_csv),
  paste0("Object safety-risk scores: ", object_safety_csv),
  paste0("Dataset safety-risk summary: ", dataset_safety_csv),
  paste0("DA/projection vs safety contrast groups: ", contrast_groups_csv),
  paste0("Candidate story groups: ", candidate_story_csv),
  paste0("QC audit summary: ", qc_audit_csv),
  "",
  "Next step:",
  "06A_FIGURE_TABLE_PREP_DA_PROJECTION_SAFETY.R",
  "",
  "Journal-rigor note:",
  "Safety-risk score is a transcriptional risk-associated state score based on proliferation, progenitor, pluripotency/immature, stress, ECM and vascular/mesenchymal signals. It is not direct proof of tumorigenicity or clinical safety."
)

writeLines(report_lines, report_txt)


# ============================================================
# 13. 结束
# ============================================================

cat("\n============================================================\n")
cat("05B safety-risk scoring and contrast 运行结束\n")
cat("============================================================\n\n")

cat("Group safety rows：", nrow(group_safety), "\n")
cat("Contrast group rows：", nrow(contrast), "\n")
cat("Object safety rows：", nrow(object_safety), "\n")
cat("Dataset summary rows：", nrow(dataset_safety), "\n")
cat("Story candidate rows：", nrow(candidate_story), "\n\n")

cat("输出文件：\n")
cat(safety_signature_csv, "\n")
cat(group_safety_csv, "\n")
cat(object_safety_csv, "\n")
cat(dataset_safety_csv, "\n")
cat(contrast_groups_csv, "\n")
cat(candidate_story_csv, "\n")
cat(qc_audit_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 05B safety-risk scoring and contrast 完成。\n")
cat("下一步进入 06A：整理 DA/projection/safety 的论文图表输入。\n")
