
PROJECT_DIR <- "D:/PD_Graft_Project"

PROJECT_TITLE <- "Single-cell transcriptomic modelling of dopaminergic graft-like competence and safety-risk-associated states in Parkinsonian cell replacement datasets"

cat("\n============================================================\n")
cat("06C：manuscript results text draft\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 06C。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_dataset <- file.path(tables_dir, "06A_figure_table_prep", "06A_dataset_overview_DA_projection_safety.csv")
input_a9a10 <- file.path(tables_dir, "06A_figure_table_prep", "06A_A9_A10_bias_summary_by_dataset.csv")
input_candidate_class <- file.path(tables_dir, "06A_figure_table_prep", "06A_candidate_class_summary_by_dataset.csv")
input_story_groups <- file.path(tables_dir, "06A_figure_table_prep", "06A_top_story_candidate_groups.csv")
input_numbers <- file.path(tables_dir, "06A_figure_table_prep", "06A_manuscript_key_numbers.csv")
input_05A_audit <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_V2_final_audit_summary.csv")
input_05A_missing <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_V2_missing_unscored_objects.csv")
input_05B_qc <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_QC_audit_summary.csv")

input_06B_v2_index <- file.path(tables_dir, "06B_publication_figure_drafts_V2", "06B_V2_figure_index.csv")
input_06B_index <- file.path(tables_dir, "06B_publication_figure_drafts", "06B_figure_index.csv")

out_tables_dir <- file.path(tables_dir, "06C_manuscript_results_text")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

results_en_md <- file.path(out_tables_dir, "06C_results_draft_EN.md")
results_cn_md <- file.path(out_tables_dir, "06C_results_draft_CN.md")
figure2_legend_md <- file.path(out_tables_dir, "06C_Figure2_legend_draft.md")
claims_cautions_csv <- file.path(out_tables_dir, "06C_key_claims_and_cautions.csv")
manuscript_outline_md <- file.path(out_tables_dir, "06C_manuscript_story_outline.md")
report_txt <- file.path(reports_dir, "06C_manuscript_results_text_draft_report.txt")

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

fmt <- function(x, digits = 3) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits, trim = TRUE))
}

get_number <- function(numbers_dt, metric_name, default = "NA") {
  if (nrow(numbers_dt) == 0 || !"metric" %in% colnames(numbers_dt) || !"value" %in% colnames(numbers_dt)) {
    return(default)
  }
  val <- numbers_dt$value[numbers_dt$metric == metric_name]
  if (length(val) == 0) return(default)
  as.character(val[[1]])
}

safe_first <- function(x, default = NA_character_) {
  if (length(x) == 0) return(default)
  x[[1]]
}

dataset_sentence <- function(row) {
  paste0(
    row[["dataset"]],
    " showed a mean DA/projection competence score of ",
    fmt(row[["mean_DA_projection_competence"]]),
    " and a mean safety-risk-associated score of ",
    fmt(row[["mean_safety_risk_composite_05B"]]),
    " (favorable index = ",
    fmt(row[["favorable_index_06A"]]),
    ")."
  )
}

dataset_sentence_cn <- function(row) {
  paste0(
    row[["dataset"]],
    " 的 mean DA/projection competence score 为 ",
    fmt(row[["mean_DA_projection_competence"]]),
    "，mean safety-risk-associated score 为 ",
    fmt(row[["mean_safety_risk_composite_05B"]]),
    "，favorable index 为 ",
    fmt(row[["favorable_index_06A"]]),
    "。"
  )
}

stamp("读取 06A / 06B / 05A / 05B 输出。")

dataset_dt <- as.data.table(read_csv_required(input_dataset))
a9a10_dt <- as.data.table(read_csv_required(input_a9a10))
candidate_dt <- as.data.table(read_csv_required(input_candidate_class))
story_dt <- as.data.table(read_csv_required(input_story_groups))
numbers_dt <- as.data.table(read_csv_required(input_numbers))
audit05A <- as.data.table(read_csv_optional(input_05A_audit))
missing05A <- as.data.table(read_csv_optional(input_05A_missing))
qc05B <- as.data.table(read_csv_optional(input_05B_qc))

if (file.exists(input_06B_v2_index)) {
  fig_index <- as.data.table(read_csv_optional(input_06B_v2_index))
  fig_source <- "06B_V2"
} else {
  fig_index <- as.data.table(read_csv_optional(input_06B_index))
  fig_source <- "06B"
}

stamp("提取 key results。")

dataset_dt[, favorable_index_06A := num(favorable_index_06A)]
dataset_dt[, mean_DA_projection_competence := num(mean_DA_projection_competence)]
dataset_dt[, mean_safety_risk_composite_05B := num(mean_safety_risk_composite_05B)]

best_dataset <- dataset_dt[order(-favorable_index_06A)][1]
worst_dataset <- dataset_dt[order(favorable_index_06A)][1]
highest_da_dataset <- dataset_dt[order(-mean_DA_projection_competence)][1]
highest_safety_dataset <- dataset_dt[order(-mean_safety_risk_composite_05B)][1]
lowest_safety_dataset <- dataset_dt[order(mean_safety_risk_composite_05B)][1]

n_scored_objects <- get_number(numbers_dt, "successfully_scored_objects_for_05A_05B")
n_scored_cells <- get_number(numbers_dt, "successfully_scored_cells_for_05A")
n_group_rows <- get_number(numbers_dt, "group_level_DA_projection_score_rows")
n_safety_group_rows <- get_number(numbers_dt, "group_level_safety_score_rows")
n_contrast_groups <- get_number(numbers_dt, "DA_projection_vs_safety_contrast_groups")
n_story_groups <- get_number(numbers_dt, "story_candidate_groups")
n_ideal <- get_number(numbers_dt, "ideal_DA_projection_high_safety_low_groups")
n_high_risk <- get_number(numbers_dt, "high_safety_risk_low_DA_groups")
n_mixed <- get_number(numbers_dt, "mixed_DA_or_projection_with_safety_risk_groups")
n_datasets <- get_number(numbers_dt, "datasets_in_05B_summary")

if (nrow(a9a10_dt) > 0 && all(c("dataset", "A9_A10_bias_label_05B", "n_groups") %in% colnames(a9a10_dt))) {
  a9a10_leading <- a9a10_dt[
    order(dataset, -n_groups),
    .SD[1],
    by = dataset
  ]
} else {
  a9a10_leading <- data.table()
}

if (nrow(candidate_dt) > 0 && all(c("dataset", "safety_contrast_class_05B", "n_groups") %in% colnames(candidate_dt))) {
  candidate_leading <- candidate_dt[
    order(dataset, -n_groups),
    .SD[1],
    by = dataset
  ]
} else {
  candidate_leading <- data.table()
}

n_unscored <- if (nrow(missing05A) > 0) nrow(missing05A) else 0L

stamp("生成 key claims and cautions table。")

claims <- data.frame(
  claim_level = c(
    "Primary result",
    "Primary result",
    "Primary result",
    "Secondary result",
    "Quality control",
    "Boundary condition",
    "Boundary condition"
  ),
  claim = c(
    "The DA reference dataset and GSE233885 showed the most favorable DA/projection competence versus safety-risk profiles.",
    "GSE204796 and GSE132758 showed stronger safety-risk-associated transcriptional signals.",
    "A9/A10-like molecular bias was dataset-dependent rather than uniform across all graft-related datasets.",
    "Candidate groups could be separated into ideal-like, high-safety-risk, and mixed DA/projection-with-risk categories.",
    "05A/05B downstream scoring used successfully scored objects only.",
    "Projection-associated molecular competence is not evidence of real anatomical projection.",
    "Safety-risk-associated transcriptional state is not proof of tumorigenicity or clinical safety."
  ),
  supporting_output = c(
    "06A_dataset_overview_DA_projection_safety.csv; Figure 2A-B",
    "06A_dataset_overview_DA_projection_safety.csv; Figure 2A-B",
    "06A_A9_A10_bias_summary_by_dataset.csv; Figure 2D",
    "06A_candidate_class_summary_by_dataset.csv; Figure 2C",
    "05A_V2_final_audit_summary.csv",
    "05A_signature_gene_sets.csv; methods wording",
    "05B_safety_signature_definition.csv; methods wording"
  ),
  manuscript_safe_wording = c(
    "favorable DA/projection-associated molecular competence and low safety-risk-associated transcriptional signal",
    "elevated progenitor/cycling/stress-associated safety-risk transcriptional signal",
    "dataset-dependent A9-like or A10-like molecular bias",
    "transcriptionally defined candidate states",
    "analyses were restricted to successfully scored objects",
    "projection-associated molecular competence",
    "safety-risk-associated transcriptional state"
  ),
  forbidden_wording = c(
    "best therapeutic graft; proven functional integration",
    "tumorigenic cells; unsafe grafts",
    "true A9/A10 identity proven in vivo",
    "final cell type labels without validation",
    "all 54 objects were scored successfully",
    "real projection; retrograde projection",
    "proven tumorigenicity; proven clinical safety"
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(claims, claims_cautions_csv)

stamp("生成英文 results draft。")

dataset_lines_en <- apply(as.data.frame(dataset_dt[order(-favorable_index_06A)]), 1, dataset_sentence)

a9_lines_en <- if (nrow(a9a10_leading) > 0) {
  apply(as.data.frame(a9a10_leading), 1, function(x) {
    paste0(
      x[["dataset"]],
      " was dominated by ",
      x[["A9_A10_bias_label_05B"]],
      " groups among scored groups."
    )
  })
} else {
  character()
}

class_lines_en <- if (nrow(candidate_leading) > 0) {
  apply(as.data.frame(candidate_leading), 1, function(x) {
    paste0(
      x[["dataset"]],
      " was dominated by the ",
      x[["safety_contrast_class_05B"]],
      " class among scored groups."
    )
  })
} else {
  character()
}

results_en <- c(
  paste0("# Results draft"),
  "",
  paste0("## Overview of scored single-cell objects"),
  "",
  paste0(
    "After quality control, marker-based annotation and final audit, downstream DA/projection and safety-risk scoring was performed on ",
    n_scored_objects,
    " successfully scored objects representing ",
    n_scored_cells,
    " cells. The analysis generated ",
    n_group_rows,
    " group-level DA/A9/A10/projection score rows and ",
    n_safety_group_rows,
    " group-level safety-risk score rows. Two objects were retained in the audit record but not used for downstream quantitative claims because no valid 05A score was generated."
  ),
  "",
  paste0("## Dataset-level DA/projection competence and safety-risk profiles"),
  "",
  paste0(
    "To compare graft-associated transcriptional states, we calculated a composite DA/projection-associated molecular competence score and contrasted it with a safety-risk-associated transcriptional score. ",
    best_dataset$dataset,
    " showed the most favorable profile, with a DA/projection competence score of ",
    fmt(best_dataset$mean_DA_projection_competence),
    " and a safety-risk-associated score of ",
    fmt(best_dataset$mean_safety_risk_composite_05B),
    " (favorable index = ",
    fmt(best_dataset$favorable_index_06A),
    "). ",
    highest_da_dataset$dataset,
    " had the highest DA/projection competence score, whereas ",
    highest_safety_dataset$dataset,
    " showed the highest safety-risk-associated score."
  ),
  "",
  paste(dataset_lines_en, collapse = "\n\n"),
  "",
  paste0("## Candidate state classes across datasets"),
  "",
  paste0(
    "Across scored groups, ",
    n_ideal,
    " groups were classified as ideal-like DA/projection-high and safety-low candidates, ",
    n_high_risk,
    " groups showed a high safety-risk/low-DA profile, and ",
    n_mixed,
    " groups showed mixed DA/projection signal together with elevated safety-risk signal. These classes should be interpreted as transcriptionally defined candidate states rather than final validated cell identities."
  ),
  "",
  paste(class_lines_en, collapse = "\n\n"),
  "",
  paste0("## Dataset-dependent A9/A10-like molecular bias"),
  "",
  paste0(
    "We next examined whether DA-like states showed A9-like or A10-like molecular bias. The distribution of A9/A10-like bias was dataset-dependent rather than uniform across all datasets. In particular, the favorable datasets showed stronger A9-like tendency, whereas other datasets contained mixed or A10-like-biased groups."
  ),
  "",
  paste(a9_lines_en, collapse = "\n\n"),
  "",
  paste0("## Interpretation and boundary of the evidence"),
  "",
  paste0(
    "These results support a working model in which DA/projection-associated molecular competence and safety-risk-associated transcriptional state can be jointly used to prioritize graft-like cell states. Importantly, the projection score represents molecular competence related to neurite maturation, synaptic machinery and axon-guidance-associated genes, and does not demonstrate real anatomical projection or functional integration. Likewise, the safety-risk score captures proliferation, progenitor, pluripotency/immature, stress and stromal-associated transcriptional signals, and should not be interpreted as direct proof of tumorigenicity or clinical safety."
  )
)

writeLines(results_en, results_en_md)

stamp("生成中文 results draft。")

dataset_lines_cn <- apply(as.data.frame(dataset_dt[order(-favorable_index_06A)]), 1, dataset_sentence_cn)

a9_lines_cn <- if (nrow(a9a10_leading) > 0) {
  apply(as.data.frame(a9a10_leading), 1, function(x) {
    paste0(
      x[["dataset"]],
      " 在已评分 groups 中主要表现为 ",
      x[["A9_A10_bias_label_05B"]],
      "。"
    )
  })
} else {
  character()
}

class_lines_cn <- if (nrow(candidate_leading) > 0) {
  apply(as.data.frame(candidate_leading), 1, function(x) {
    paste0(
      x[["dataset"]],
      " 在已评分 groups 中主要以 ",
      x[["safety_contrast_class_05B"]],
      " 为主。"
    )
  })
} else {
  character()
}

results_cn <- c(
  "# 中文 Results 草稿",
  "",
  "## 已评分对象概览",
  "",
  paste0(
    "经过 QC、marker-based annotation 和最终审计后，下游 DA/projection 与 safety-risk scoring 使用了 ",
    n_scored_objects,
    " 个成功评分对象，覆盖 ",
    n_scored_cells,
    " 个细胞。分析共产生 ",
    n_group_rows,
    " 行 group-level DA/A9/A10/projection score，以及 ",
    n_safety_group_rows,
    " 行 group-level safety-risk score。另有 2 个对象被保留在审计记录中，但由于未产生有效 05A score，不用于后续定量结论。"
  ),
  "",
  "## Dataset-level DA/projection competence 与 safety-risk profile",
  "",
  paste0(
    "为了比较不同 graft-associated transcriptional states，我们计算了 DA/projection-associated molecular competence composite score，并与 safety-risk-associated transcriptional score 进行对照。结果显示，",
    best_dataset$dataset,
    " 的 overall profile 最有利：DA/projection competence score 为 ",
    fmt(best_dataset$mean_DA_projection_competence),
    "，safety-risk-associated score 为 ",
    fmt(best_dataset$mean_safety_risk_composite_05B),
    "，favorable index 为 ",
    fmt(best_dataset$favorable_index_06A),
    "。其中 ",
    highest_da_dataset$dataset,
    " 具有最高 DA/projection competence，而 ",
    highest_safety_dataset$dataset,
    " 具有最高 safety-risk-associated score。"
  ),
  "",
  paste(dataset_lines_cn, collapse = "\n\n"),
  "",
  "## Candidate state classes across datasets",
  "",
  paste0(
    "在已评分 groups 中，",
    n_ideal,
    " 个 groups 被归为 ideal-like DA/projection-high and safety-low candidates，",
    n_high_risk,
    " 个 groups 表现为 high safety-risk/low-DA profile，",
    n_mixed,
    " 个 groups 同时具有 DA/projection signal 和较高 safety-risk signal。这里的 class 是 transcriptionally defined candidate states，不是最终不可更改的细胞类型标签。"
  ),
  "",
  paste(class_lines_cn, collapse = "\n\n"),
  "",
  "## Dataset-dependent A9/A10-like molecular bias",
  "",
  "随后我们分析了 DA-like states 的 A9-like / A10-like molecular bias。结果显示，A9/A10-like bias 并不是所有数据集一致，而是具有明显 dataset-dependent heterogeneity。整体更 favorable 的 dataset 更偏 A9-like tendency，而其他 dataset 则含 mixed 或 A10-like-biased groups。",
  "",
  paste(a9_lines_cn, collapse = "\n\n"),
  "",
  "## 证据边界",
  "",
  "这些结果支持一个工作模型：DA/projection-associated molecular competence 与 safety-risk-associated transcriptional state 可以联合用于筛选更理想的 graft-like cell states。但必须注意，projection score 只是基于 neurite maturation、synaptic machinery 和 axon-guidance-associated genes 的分子能力评分，并不能证明真实解剖投射或功能整合。同样，safety-risk score 反映 proliferation、progenitor、pluripotency/immature、stress 和 stromal-associated transcriptional signals，不能直接等同于肿瘤形成风险或临床安全性证明。"
)

writeLines(results_cn, results_cn_md)

stamp("生成 Figure 2 legend draft。")

figure_legend <- c(
  "# Figure 2 legend draft",
  "",
  "## Figure 2. Joint modelling of DA/projection-associated molecular competence and safety-risk-associated transcriptional states.",
  "",
  "**(A)** Dataset-level scatter plot comparing the mean safety-risk-associated transcriptional score and the mean DA/projection-associated molecular competence score. Dashed lines indicate median values across datasets. The projection score reflects molecular competence associated with neurite maturation, synaptic machinery and axon-guidance-associated genes, and should not be interpreted as direct evidence of anatomical projection.",
  "",
  "**(B)** Dataset-level favorable index, calculated as mean DA/projection-associated molecular competence score minus mean safety-risk-associated transcriptional score. Higher values indicate a more favorable balance between DA/projection-associated molecular competence and lower safety-risk-associated signal.",
  "",
  "**(C)** Candidate class composition across datasets. Groups were classified as ideal-like DA/projection-high and safety-low, high safety-risk and low-DA, mixed DA/projection-with-risk, projection-competent but DA-low, or lower-priority/mixed states. These classes represent transcriptionally defined candidate states.",
  "",
  "**(D)** A9/A10-like molecular bias composition across datasets. Bias labels were inferred from relative A9-like and A10-like molecular signature scores and should be interpreted as molecular bias rather than definitive anatomical subtype identity.",
  "",
  "**(E)** Heatmap-like summary of selected high-priority story candidate groups. Rows represent selected groups, and columns show DA-like score, projection competence score, safety-risk score and A9-minus-A10 bias score. Full group identifiers are provided in the corresponding 06B output table.",
  "",
  "All panels are based on successfully scored objects only. Unscored objects were retained in the audit record but excluded from downstream quantitative claims."
)

writeLines(figure_legend, figure2_legend_md)

stamp("生成 manuscript story outline。")

outline <- c(
  paste0("# Manuscript story outline"),
  "",
  paste0("## Working title"),
  PROJECT_TITLE,
  "",
  "## Central hypothesis",
  "Public single-cell graft-related datasets can be jointly modelled to identify cell states with favorable DA/projection-associated molecular competence and low safety-risk-associated transcriptional signal.",
  "",
  "## Main result modules",
  "",
  "### Module 1: DA/A9/A10/projection-associated molecular competence",
  "- DA-like identity and DA functional machinery were scored using curated dopaminergic marker sets.",
  "- A9-like and A10-like molecular bias were evaluated as relative molecular tendencies.",
  "- Projection-associated molecular competence was scored using neurite maturation, synaptic machinery and axon-guidance-associated genes.",
  "- This module must not be described as proof of real anatomical projection.",
  "",
  "### Module 2: Cell-state fate propensity and safety-risk-associated modelling",
  "- Safety-risk-associated transcriptional states were scored using proliferation, progenitor, pluripotency/immature, stress, ECM and vascular/mesenchymal components.",
  "- The score is a transcriptomic risk-associated state score, not proof of tumorigenicity.",
  "- Joint DA/projection versus safety-risk contrast identified ideal-like, high-risk and mixed states.",
  "",
  "## Current story direction",
  paste0("- Most favorable dataset: ", best_dataset$dataset, "."),
  paste0("- Highest DA/projection competence dataset: ", highest_da_dataset$dataset, "."),
  paste0("- Highest safety-risk-associated dataset: ", highest_safety_dataset$dataset, "."),
  paste0("- Lowest safety-risk-associated dataset: ", lowest_safety_dataset$dataset, "."),
  "",
  "## Suggested next section",
  "06D can generate a Discussion draft and figure-by-figure manuscript structure after the final figure drafts are reviewed."
)

writeLines(outline, manuscript_outline_md)

stamp("生成 06C report。")

report_lines <- c(
  "06C manuscript results text draft report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input summary:",
  paste0("Datasets in overview: ", nrow(dataset_dt)),
  paste0("Scored objects: ", n_scored_objects),
  paste0("Scored cells: ", n_scored_cells),
  paste0("Group-level DA/projection rows: ", n_group_rows),
  paste0("Group-level safety rows: ", n_safety_group_rows),
  paste0("Unscored objects retained in audit: ", n_unscored),
  "",
  "Key dataset results:",
  paste0("Best favorable index dataset: ", best_dataset$dataset, " (", fmt(best_dataset$favorable_index_06A), ")"),
  paste0("Highest DA/projection dataset: ", highest_da_dataset$dataset, " (", fmt(highest_da_dataset$mean_DA_projection_competence), ")"),
  paste0("Highest safety-risk dataset: ", highest_safety_dataset$dataset, " (", fmt(highest_safety_dataset$mean_safety_risk_composite_05B), ")"),
  "",
  "Output files:",
  paste0("English results draft: ", results_en_md),
  paste0("Chinese results draft: ", results_cn_md),
  paste0("Figure 2 legend draft: ", figure2_legend_md),
  paste0("Claims and cautions table: ", claims_cautions_csv),
  paste0("Manuscript story outline: ", manuscript_outline_md),
  "",
  "Next step:",
  "06D_DISCUSSION_AND_ABSTRACT_DRAFT.R",
  "",
  "Journal-rigor note:",
  "The draft intentionally uses projection-associated molecular competence and safety-risk-associated transcriptional state to avoid overclaiming real projection, functional integration, tumorigenicity or clinical safety."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("06C manuscript results text draft 运行结束\n")
cat("============================================================\n\n")

cat("Datasets in overview：", nrow(dataset_dt), "\n")
cat("Scored objects：", n_scored_objects, "\n")
cat("Scored cells：", n_scored_cells, "\n")
cat("Best favorable dataset：", best_dataset$dataset, "\n")
cat("Highest DA/projection dataset：", highest_da_dataset$dataset, "\n")
cat("Highest safety-risk dataset：", highest_safety_dataset$dataset, "\n")
cat("Unscored audit objects：", n_unscored, "\n\n")

cat("输出文件：\n")
cat(results_en_md, "\n")
cat(results_cn_md, "\n")
cat(figure2_legend_md, "\n")
cat(claims_cautions_csv, "\n")
cat(manuscript_outline_md, "\n")
cat(report_txt, "\n\n")

cat("✅ 06C manuscript results text draft 完成。\n")
cat("下一步：先打开 06C_results_draft_CN.md 和 06C_results_draft_EN.md，看故事是否符合预期。\n")
