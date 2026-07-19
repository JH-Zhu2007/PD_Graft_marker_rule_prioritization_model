
PROJECT_DIR <- "D:/PD_Graft_Project"

WORKING_TITLE <- "Single-cell transcriptomic modelling identifies dopaminergic graft-like competence and safety-risk-associated states in Parkinsonian cell replacement datasets"

SHORT_TITLE <- "Transcriptomic modelling of dopaminergic graft competence and safety-risk states"

cat("\n============================================================\n")
cat("06D：discussion, abstract and manuscript structure draft\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 06D。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_dataset <- file.path(tables_dir, "06A_figure_table_prep", "06A_dataset_overview_DA_projection_safety.csv")
input_numbers <- file.path(tables_dir, "06A_figure_table_prep", "06A_manuscript_key_numbers.csv")
input_claims <- file.path(tables_dir, "06C_manuscript_results_text", "06C_key_claims_and_cautions.csv")
input_results_en <- file.path(tables_dir, "06C_manuscript_results_text", "06C_results_draft_EN.md")
input_results_cn <- file.path(tables_dir, "06C_manuscript_results_text", "06C_results_draft_CN.md")
input_figure2_legend <- file.path(tables_dir, "06C_manuscript_results_text", "06C_Figure2_legend_draft.md")
input_story_outline <- file.path(tables_dir, "06C_manuscript_results_text", "06C_manuscript_story_outline.md")

out_tables_dir <- file.path(tables_dir, "06D_discussion_abstract")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

abstract_en_md <- file.path(out_tables_dir, "06D_abstract_draft_EN.md")
abstract_cn_md <- file.path(out_tables_dir, "06D_abstract_draft_CN.md")
discussion_en_md <- file.path(out_tables_dir, "06D_discussion_draft_EN.md")
discussion_cn_md <- file.path(out_tables_dir, "06D_discussion_draft_CN.md")
manuscript_structure_md <- file.path(out_tables_dir, "06D_manuscript_structure_and_figure_plan.md")
limitations_csv <- file.path(out_tables_dir, "06D_limitations_and_safe_wording_table.csv")
title_keywords_md <- file.path(out_tables_dir, "06D_title_keywords_highlights.md")
report_txt <- file.path(reports_dir, "06D_discussion_abstract_and_structure_report.txt")

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

read_text_optional <- function(path) {
  if (!file.exists(path)) return(character())
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

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

stamp("读取 06C / 06A 输出。")

dataset_dt <- as.data.table(read_csv_required(input_dataset))
numbers_dt <- as.data.table(read_csv_required(input_numbers))
claims_dt <- as.data.table(read_csv_optional(input_claims))

results_en <- read_text_optional(input_results_en)
results_cn <- read_text_optional(input_results_cn)
figure2_legend <- read_text_optional(input_figure2_legend)
story_outline <- read_text_optional(input_story_outline)

stamp("提取 manuscript key numbers。")

dataset_dt[, favorable_index_06A := suppressWarnings(as.numeric(favorable_index_06A))]
dataset_dt[, mean_DA_projection_competence := suppressWarnings(as.numeric(mean_DA_projection_competence))]
dataset_dt[, mean_safety_risk_composite_05B := suppressWarnings(as.numeric(mean_safety_risk_composite_05B))]

best_dataset <- dataset_dt[order(-favorable_index_06A)][1]
second_dataset <- dataset_dt[order(-favorable_index_06A)][2]
worst_dataset <- dataset_dt[order(favorable_index_06A)][1]
highest_da_dataset <- dataset_dt[order(-mean_DA_projection_competence)][1]
highest_safety_dataset <- dataset_dt[order(-mean_safety_risk_composite_05B)][1]

n_scored_objects <- get_number(numbers_dt, "successfully_scored_objects_for_05A_05B")
n_scored_cells <- get_number(numbers_dt, "successfully_scored_cells_for_05A")
n_contrast_groups <- get_number(numbers_dt, "DA_projection_vs_safety_contrast_groups")
n_story_groups <- get_number(numbers_dt, "story_candidate_groups")
n_ideal <- get_number(numbers_dt, "ideal_DA_projection_high_safety_low_groups")
n_high_risk <- get_number(numbers_dt, "high_safety_risk_low_DA_groups")
n_mixed <- get_number(numbers_dt, "mixed_DA_or_projection_with_safety_risk_groups")
n_datasets <- get_number(numbers_dt, "datasets_in_05B_summary")

stamp("生成 limitations and safe wording table。")

limitations <- data.frame(
  issue = c(
    "Projection evidence boundary",
    "Safety evidence boundary",
    "Annotation boundary",
    "Dataset heterogeneity",
    "Unscored objects",
    "Species/platform differences",
    "No direct functional validation",
    "Scoring threshold dependence"
  ),
  limitation = c(
    "Projection-associated score is inferred from axon guidance, neurite maturation and synaptic machinery genes.",
    "Safety-risk score is inferred from proliferation, progenitor, pluripotency/immature, stress and stromal marker programs.",
    "Annotation labels are conservative marker-supported labels and remain preliminary.",
    "Datasets differ in source, protocol, cell composition and biological context.",
    "Two objects were recorded as unscored and excluded from downstream quantitative claims.",
    "Cross-dataset comparison may be influenced by species, gene-symbol mapping and platform effects.",
    "No independent imaging, tracing, electrophysiology or transplantation outcome validation is included in this computational analysis.",
    "Candidate classes depend on curated signatures and predefined thresholds."
  ),
  safe_wording = c(
    "projection-associated molecular competence",
    "safety-risk-associated transcriptional state",
    "marker-supported candidate state",
    "dataset-dependent heterogeneity",
    "downstream claims were restricted to successfully scored objects",
    "cross-dataset transcriptional comparison",
    "computational prioritization framework",
    "threshold-based candidate prioritization"
  ),
  avoid_wording = c(
    "real projection; retrograde projection; proven anatomical integration",
    "tumorigenic; clinically unsafe; proven safety risk",
    "final cell type identity; definitive graft fate",
    "all datasets are directly equivalent",
    "all objects were scored successfully",
    "species-independent universal conclusion",
    "validated therapeutic function",
    "objective ground-truth classification"
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(limitations, limitations_csv)

stamp("生成英文 abstract draft。")

abstract_en <- c(
  "# Abstract draft",
  "",
  "**Background:** Cell replacement therapy for Parkinson's disease requires grafted cells to acquire dopaminergic neuronal identity while minimizing immature, proliferative or off-target transcriptional states. However, public single-cell datasets are rarely evaluated using a unified framework that jointly models dopaminergic graft-like competence and safety-risk-associated cell states.",
  "",
  paste0(
    "**Methods:** We assembled and processed public single-cell and bulk transcriptomic datasets related to dopaminergic neurons and graft-associated cell replacement models. After quality control, marker-based annotation and final audit, downstream scoring was performed on ",
    n_scored_objects,
    " successfully scored objects representing ",
    n_scored_cells,
    " cells. We curated transcriptional signatures for DA-like identity, A9/A10-like molecular bias, neuronal maturation, projection-associated molecular competence and safety-risk-associated states. Safety-risk scoring integrated proliferation, progenitor, pluripotency/immature, stress and stromal-associated marker programs."
  ),
  "",
  paste0(
    "**Results:** Joint DA/projection and safety-risk modelling identified strong dataset-dependent heterogeneity. ",
    best_dataset$dataset,
    " showed the most favorable overall balance, with the highest favorable index (",
    fmt(best_dataset$favorable_index_06A),
    "), while ",
    highest_da_dataset$dataset,
    " showed the highest DA/projection-associated molecular competence score. Among graft-associated datasets, ",
    second_dataset$dataset,
    " displayed a favorable DA/projection-high and safety-low profile, whereas ",
    highest_safety_dataset$dataset,
    " showed the highest safety-risk-associated transcriptional score. Across ",
    n_contrast_groups,
    " contrasted groups, ",
    n_ideal,
    " groups were classified as ideal-like DA/projection-high and safety-low candidates, ",
    n_high_risk,
    " groups showed high safety-risk and low DA signal, and ",
    n_mixed,
    " groups showed mixed DA/projection signal with concurrent safety-risk-associated features."
  ),
  "",
  "**Conclusions:** This study establishes a transcriptomic framework for prioritizing graft-like dopaminergic cell states by jointly modelling DA/projection-associated molecular competence and safety-risk-associated transcriptional programs. The analysis supports the use of public single-cell datasets to nominate favorable and risk-associated graft cell states, while emphasizing that projection-associated scores do not prove anatomical projection and safety-risk scores do not prove tumorigenicity or clinical safety."
)

writeLines(abstract_en, abstract_en_md)

stamp("生成中文摘要解释版。")

abstract_cn <- c(
  "# 中文摘要草稿",
  "",
  "**背景：** 帕金森病细胞替代治疗要求移植细胞获得多巴胺能神经元样身份，同时尽量避免未成熟、增殖性或 off-target 转录组状态。然而，公开单细胞数据很少被放在一个统一框架下，同时评估 dopaminergic graft-like competence 和 safety-risk-associated cell states。",
  "",
  paste0(
    "**方法：** 本研究整合并处理了与多巴胺神经元和 graft-associated cell replacement models 相关的公开单细胞及 bulk 转录组数据。经过 QC、marker-based annotation 和最终审计后，下游 scoring 使用了 ",
    n_scored_objects,
    " 个成功评分对象，共代表 ",
    n_scored_cells,
    " 个细胞。我们构建了 DA-like identity、A9/A10-like molecular bias、neuronal maturation、projection-associated molecular competence 和 safety-risk-associated states 的转录组 signature。Safety-risk score 整合了 proliferation、progenitor、pluripotency/immature、stress 和 stromal-associated marker programs。"
  ),
  "",
  paste0(
    "**结果：** DA/projection 与 safety-risk 联合建模显示出明显 dataset-dependent heterogeneity。",
    best_dataset$dataset,
    " 具有最有利的整体平衡，favorable index 最高（",
    fmt(best_dataset$favorable_index_06A),
    "），而 ",
    highest_da_dataset$dataset,
    " 具有最高 DA/projection-associated molecular competence score。在 graft-associated datasets 中，",
    second_dataset$dataset,
    " 表现出较有利的 DA/projection-high and safety-low profile，而 ",
    highest_safety_dataset$dataset,
    " 具有最高 safety-risk-associated transcriptional score。在 ",
    n_contrast_groups,
    " 个 contrast groups 中，",
    n_ideal,
    " 个 groups 被归为 ideal-like DA/projection-high and safety-low candidates，",
    n_high_risk,
    " 个 groups 表现为 high safety-risk and low DA signal，",
    n_mixed,
    " 个 groups 同时具有 DA/projection signal 和 safety-risk-associated features。"
  ),
  "",
  "**结论：** 本研究建立了一个 transcriptomic framework，用于通过 DA/projection-associated molecular competence 和 safety-risk-associated transcriptional programs 的联合建模，筛选更理想的 dopaminergic graft-like cell states。需要强调的是，projection-associated score 不能证明真实解剖投射，safety-risk score 也不能证明肿瘤形成或临床安全性。"
)

writeLines(abstract_cn, abstract_cn_md)

stamp("生成英文 discussion draft。")

discussion_en <- c(
  "# Discussion draft",
  "",
  "## Principal findings",
  "",
  paste0(
    "In this study, we developed a single-cell transcriptomic framework to jointly evaluate dopaminergic graft-like molecular competence and safety-risk-associated transcriptional states across public Parkinsonian cell replacement datasets. The analysis was performed on ",
    n_scored_objects,
    " successfully scored objects representing ",
    n_scored_cells,
    " cells, after excluding unscored objects from downstream quantitative claims. The main finding is that DA/projection-associated molecular competence and safety-risk-associated transcriptional programs are not uniformly distributed across datasets. Instead, they form distinct dataset-level and group-level profiles that can be used to prioritize candidate graft-like states."
  ),
  "",
  paste0(
    "The DA reference dataset, ",
    best_dataset$dataset,
    ", showed the most favorable overall profile and the highest favorable index. This was expected because it represents a dopaminergic target/reference population, but it also provided an important positive anchor for evaluating graft-associated datasets. Among graft-associated datasets, ",
    second_dataset$dataset,
    " showed a more favorable balance between DA/projection-associated molecular competence and low safety-risk-associated signal. In contrast, ",
    highest_safety_dataset$dataset,
    " displayed the highest safety-risk-associated transcriptional score, supporting the presence of mixed or risk-associated transcriptional states in at least a subset of graft-related cells."
  ),
  "",
  "## DA-like identity and A9/A10-like molecular bias",
  "",
  "A major goal of dopaminergic cell replacement therapy is to generate grafted neurons with appropriate DA-like molecular identity. Our scoring framework separated DA-like identity, DA functional machinery, A9-like molecular bias, A10-like molecular bias, neuronal maturation and projection-associated molecular competence. This separation is important because a cell state may show partial DA-like marker expression without showing a favorable projection-associated or low-risk profile. The results suggest that A9/A10-like molecular bias is dataset-dependent rather than uniform, with some datasets showing stronger A9-like tendency and others showing mixed or A10-like-biased profiles.",
  "",
  "Importantly, A9-like or A10-like labels in this analysis refer to relative molecular bias based on curated marker signatures. They should not be interpreted as definitive substantia nigra or ventral tegmental area identity, because such anatomical and functional identity would require independent spatial, tracing or functional validation.",
  "",
  "## Projection-associated molecular competence",
  "",
  "The projection-associated molecular competence score was designed to capture transcriptional programs related to neurite maturation, axon guidance and synaptic machinery. This provides a computational way to ask whether graft-like cell states express molecular features consistent with the capacity for neuronal maturation and potential connectivity. However, this score does not demonstrate real anatomical projection, retrograde connectivity or functional integration. Therefore, the appropriate interpretation is that certain candidate groups show projection-associated molecular competence, not that they have formed verified projections in vivo.",
  "",
  "## Safety-risk-associated transcriptional states",
  "",
  paste0(
    "The second major module quantified safety-risk-associated transcriptional states using proliferation, progenitor, pluripotency/immature, stress and stromal-associated components. Across ",
    n_contrast_groups,
    " contrasted groups, the framework identified ",
    n_ideal,
    " ideal-like DA/projection-high and safety-low groups, ",
    n_high_risk,
    " high safety-risk and low-DA groups, and ",
    n_mixed,
    " mixed DA/projection-with-risk groups. This separation is biologically useful because it distinguishes potentially favorable graft-like states from immature or proliferative states that may require additional review."
  ),
  "",
  "Nevertheless, safety-risk-associated transcriptional scores should not be equated with direct tumorigenicity or clinical safety outcomes. A high score indicates enrichment of transcriptional programs associated with proliferation, progenitor identity or immature/pluripotency-related signals, but experimental validation would be required to determine actual safety risk.",
  "",
  "## Biological and translational implications",
  "",
  "This framework provides a practical approach for prioritizing cell states in public graft-related single-cell datasets. Rather than asking only whether cells express DA markers, the analysis evaluates whether DA-like identity is accompanied by projection-associated molecular competence and a low safety-risk-associated transcriptional state. This joint view is more informative for graft-quality assessment because a favorable therapeutic cell state should ideally combine dopaminergic maturation with low immature/proliferative risk.",
  "",
  "The results also highlight that graft-related datasets may contain heterogeneous mixtures of favorable, risk-associated and mixed states. Such heterogeneity could reflect differences in differentiation protocols, graft maturation stage, host environment, sampling time or dataset-specific technical factors. Future work could use this framework to compare new differentiation conditions, screen candidate graft preparations or build predictive models for favorable versus risk-associated graft-like states.",
  "",
  "## Limitations",
  "",
  "Several limitations should be considered. First, the study is computational and based on public transcriptomic datasets; it does not include direct experimental validation. Second, projection-associated molecular competence is inferred from gene expression and cannot prove anatomical projection or functional connectivity. Third, safety-risk-associated transcriptional state is not proof of tumorigenicity or clinical safety. Fourth, cross-dataset comparisons may be influenced by differences in species, protocols, sequencing platforms, annotation depth and cell composition. Fifth, scoring results depend on curated marker sets and thresholds, which should be refined as additional reference datasets become available.",
  "",
  "## Conclusion",
  "",
  "In summary, this study provides a reproducible transcriptomic framework for evaluating dopaminergic graft-like competence and safety-risk-associated states across public single-cell datasets. By jointly modelling DA/A9/A10-like molecular identity, projection-associated molecular competence and safety-risk-associated transcriptional programs, the framework prioritizes candidate graft-like states while preserving clear boundaries around what can and cannot be concluded from transcriptomic data alone."
)

writeLines(discussion_en, discussion_en_md)

stamp("生成中文 discussion 解释版。")

discussion_cn <- c(
  "# 中文 Discussion 草稿",
  "",
  "## 主要发现",
  "",
  paste0(
    "本研究建立了一个单细胞转录组分析框架，用于在公开 PD cell replacement / graft 相关数据集中联合评估 dopaminergic graft-like molecular competence 和 safety-risk-associated transcriptional states。经过审计后，下游定量结论基于 ",
    n_scored_objects,
    " 个成功评分对象，共 ",
    n_scored_cells,
    " 个细胞。核心发现是：DA/projection-associated molecular competence 和 safety-risk-associated transcriptional programs 在不同数据集中并不均一，而是形成了明显的 dataset-level 和 group-level 差异。"
  ),
  "",
  paste0(
    "DA reference dataset ",
    best_dataset$dataset,
    " 表现出最有利的 overall profile 和最高 favorable index。这个结果符合预期，因为它是 DA target/reference population，同时也为 graft-associated datasets 的比较提供了 positive anchor。在 graft-associated datasets 中，",
    second_dataset$dataset,
    " 展现出较有利的 DA/projection competence 与低 safety-risk signal 的平衡。相反，",
    highest_safety_dataset$dataset,
    " 展现最高 safety-risk-associated transcriptional score，说明其中至少部分 graft-related cells 具有 mixed 或 risk-associated transcriptional states。"
  ),
  "",
  "## DA-like identity 和 A9/A10-like molecular bias",
  "",
  "多巴胺能细胞替代治疗的核心目标之一，是获得具有合适 DA-like molecular identity 的 grafted neurons。本研究将 DA-like identity、DA functional machinery、A9-like molecular bias、A10-like molecular bias、neuronal maturation 和 projection-associated molecular competence 分开评分。这样做很重要，因为一个细胞状态可以表达部分 DA marker，但不一定同时具有良好的 projection-associated molecular competence 或低 safety-risk profile。",
  "",
  "需要注意的是，本研究中的 A9-like / A10-like 只是基于 marker signature 的 molecular bias，不能等同于已经证明了 substantia nigra 或 VTA 的真实解剖/功能身份。",
  "",
  "## Projection-associated molecular competence",
  "",
  "Projection-associated molecular competence score 主要用于捕捉 neurite maturation、axon guidance 和 synaptic machinery 相关转录程序。它可以从转录组层面判断某些 graft-like states 是否具备与神经元成熟和潜在连接能力相关的分子特征。但它不能证明真实解剖投射、retrograde connectivity 或 functional integration。因此，写文章时只能说 projection-associated molecular competence，不能说 real projection。",
  "",
  "## Safety-risk-associated transcriptional states",
  "",
  paste0(
    "第二个核心模块使用 proliferation、progenitor、pluripotency/immature、stress 和 stromal-associated components 计算 safety-risk-associated transcriptional states。在 ",
    n_contrast_groups,
    " 个 contrasted groups 中，我们识别到 ",
    n_ideal,
    " 个 ideal-like DA/projection-high and safety-low groups，",
    n_high_risk,
    " 个 high safety-risk and low-DA groups，以及 ",
    n_mixed,
    " 个 mixed DA/projection-with-risk groups。这个分类有助于区分更理想的 graft-like states 和需要重点审查的 immature/proliferative states。"
  ),
  "",
  "但是 safety-risk score 不能直接等同于肿瘤形成风险或临床安全性。它只能说明这些 groups 富集了 proliferation、progenitor 或 immature/pluripotency-related transcriptional programs，真实安全性仍需要实验验证。",
  "",
  "## 生物学和转化意义",
  "",
  "这个框架的价值在于，它不是只问细胞有没有 DA marker，而是进一步问：DA-like identity 是否同时伴随 projection-associated molecular competence，以及是否缺乏明显 safety-risk-associated transcriptional signal。这种联合评估更适合用于 graft quality assessment。",
  "",
  "结果也说明，不同 graft-related datasets 可能包含 favorable、risk-associated 和 mixed states 的不同组合。这些差异可能来自 differentiation protocol、graft maturation stage、host environment、sampling time 或技术差异。未来可以用这个框架比较新的 differentiation conditions，筛选 candidate graft preparations，或者训练 favorable vs risk-associated graft-like state 的预测模型。",
  "",
  "## 局限性",
  "",
  "本研究有几个局限。第一，这是一个计算分析，基于公开 transcriptomic datasets，没有直接实验验证。第二，projection-associated molecular competence 来自基因表达，不能证明真实投射或功能连接。第三，safety-risk-associated transcriptional state 不能证明肿瘤形成或临床安全性。第四，跨数据集比较可能受到 species、protocol、sequencing platform、annotation depth 和 cell composition 的影响。第五，评分依赖 curated marker sets 和 thresholds，后续可以随着更多 reference datasets 继续优化。",
  "",
  "## 总结",
  "",
  "总之，本研究提供了一个可复现的转录组框架，用于评估公开单细胞数据中的 dopaminergic graft-like competence 和 safety-risk-associated states。通过联合建模 DA/A9/A10-like molecular identity、projection-associated molecular competence 和 safety-risk-associated transcriptional programs，该框架可以帮助筛选候选 graft-like states，同时避免超出 transcriptomic evidence 的过度结论。"
)

writeLines(discussion_cn, discussion_cn_md)

stamp("生成 manuscript structure and figure plan。")

structure <- c(
  "# Manuscript structure and figure plan",
  "",
  paste0("## Working title"),
  WORKING_TITLE,
  "",
  "## Short title",
  SHORT_TITLE,
  "",
  "## Proposed manuscript structure",
  "",
  "### Introduction",
  "1. Parkinson's disease and the need for dopaminergic cell replacement.",
  "2. Challenge: grafted cells must acquire DA-like neuronal competence while minimizing immature/proliferative/off-target states.",
  "3. Gap: public single-cell graft datasets lack a unified framework for joint competence and safety-risk modelling.",
  "4. Aim: build a reproducible transcriptomic framework for DA/A9/A10-like competence, projection-associated molecular competence and safety-risk-associated state scoring.",
  "",
  "### Results",
  "1. Construction and QC of a multi-dataset PD graft/cell replacement transcriptomic resource.",
  "2. Marker panel construction and conservative annotation of candidate cell states.",
  "3. DA/A9/A10-like identity and projection-associated molecular competence scoring.",
  "4. Safety-risk-associated transcriptional state scoring.",
  "5. Joint DA/projection versus safety-risk contrast identifies favorable, risk-associated and mixed graft-like states.",
  "",
  "### Discussion",
  "1. Joint competence/risk modelling provides more information than DA markers alone.",
  "2. GSE233885 emerges as a favorable graft-associated dataset profile, while GSE204796/GSE132758 contain stronger mixed/risk-associated signals.",
  "3. A9/A10-like bias is heterogeneous and should be described as molecular bias, not definitive anatomical identity.",
  "4. Safety-risk scoring is a prioritization framework, not proof of tumorigenicity.",
  "5. Limitations and future validation.",
  "",
  "## Figure plan",
  "",
  "### Figure 1. Dataset processing and annotation workflow",
  "- Panel A: project workflow from public datasets to scoring modules.",
  "- Panel B: object QC and retained cells.",
  "- Panel C: marker panel categories.",
  "- Panel D: conservative annotation summary.",
  "",
  "### Figure 2. DA/projection competence and safety-risk contrast",
  "- Panel A: dataset-level DA/projection competence versus safety-risk scatter.",
  "- Panel B: favorable index ranking.",
  "- Panel C: candidate class composition by dataset.",
  "- Panel D: A9/A10-like molecular bias composition.",
  "- Panel E: top story candidate groups heatmap/tile plot.",
  "",
  "### Figure 3. Detailed DA/A9/A10/projection-associated molecular competence",
  "- Candidate DA-like groups and their DA core/A9/A10/projection scores.",
  "- Could include dotplot/heatmap of TH, DDC, SLC6A3, SLC18A2, ALDH1A1, KCNJ6, SOX6, CALB1, OTX2, SNAP25, SYT1, STMN2.",
  "",
  "### Figure 4. Safety-risk-associated transcriptional states",
  "- Candidate risk groups and their cell-cycle/progenitor/pluripotency/stress scores.",
  "- Could include MKI67, TOP2A, PCNA, SOX2, NES, POU5F1, NANOG, FOS/JUN/HSPA genes.",
  "",
  "### Figure 5. Predictive modelling module",
  "- Later module: ideal graft-like classifier and safety-risk classifier.",
  "- Include feature importance, ROC/AUC, cross-validation and external validation if available.",
  "",
  "## Tables",
  "- Table 1: Dataset sources and roles.",
  "- Table 2: Signature gene sets.",
  "- Table 3: Candidate DA/projection-high safety-low groups.",
  "- Table 4: Candidate safety-risk-associated groups.",
  "- Supplementary tables: all marker scores, audit records and failed/unscored objects."
)

writeLines(structure, manuscript_structure_md)

stamp("生成 title, keywords and highlights。")

title_keywords <- c(
  "# Title, keywords and highlights",
  "",
  "## Candidate titles",
  "",
  "1. Single-cell transcriptomic modelling identifies dopaminergic graft-like competence and safety-risk-associated states in Parkinsonian cell replacement datasets",
  "",
  "2. Joint modelling of dopaminergic molecular competence and safety-risk states in public Parkinson's disease graft single-cell datasets",
  "",
  "3. A transcriptomic framework for prioritizing dopaminergic graft-like cell states by DA/projection competence and safety-risk-associated signatures",
  "",
  "## Keywords",
  "",
  "- Parkinson's disease",
  "- dopaminergic neuron",
  "- cell replacement therapy",
  "- single-cell RNA-seq",
  "- graft safety",
  "- A9/A10 molecular identity",
  "- projection-associated molecular competence",
  "- safety-risk-associated transcriptional state",
  "- transcriptomic modelling",
  "",
  "## Highlights",
  "",
  "- A reproducible single-cell framework was built to evaluate dopaminergic graft-like competence across public datasets.",
  "- DA-like, A9/A10-like and projection-associated molecular competence scores were jointly modelled with safety-risk-associated transcriptional signatures.",
  "- GSE178265_DA_01B provided a DA-like reference anchor, while GSE233885 showed a favorable graft-associated DA/projection-high and safety-low profile.",
  "- GSE204796 and GSE132758 displayed stronger mixed or safety-risk-associated transcriptional states.",
  "- Projection-associated molecular competence and safety-risk-associated transcriptional state are computational evidence layers and require experimental validation."
)

writeLines(title_keywords, title_keywords_md)

stamp("生成 06D report。")

report_lines <- c(
  "06D discussion, abstract and manuscript structure draft report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input summary:",
  paste0("Datasets in overview: ", nrow(dataset_dt)),
  paste0("Scored objects: ", n_scored_objects),
  paste0("Scored cells: ", n_scored_cells),
  paste0("Contrasted groups: ", n_contrast_groups),
  paste0("Ideal groups: ", n_ideal),
  paste0("High-risk groups: ", n_high_risk),
  paste0("Mixed groups: ", n_mixed),
  "",
  "Key dataset results:",
  paste0("Best favorable dataset: ", best_dataset$dataset, " (", fmt(best_dataset$favorable_index_06A), ")"),
  paste0("Second favorable dataset: ", second_dataset$dataset, " (", fmt(second_dataset$favorable_index_06A), ")"),
  paste0("Highest DA/projection dataset: ", highest_da_dataset$dataset, " (", fmt(highest_da_dataset$mean_DA_projection_competence), ")"),
  paste0("Highest safety-risk dataset: ", highest_safety_dataset$dataset, " (", fmt(highest_safety_dataset$mean_safety_risk_composite_05B), ")"),
  "",
  "Output files:",
  paste0("Abstract EN: ", abstract_en_md),
  paste0("Abstract CN: ", abstract_cn_md),
  paste0("Discussion EN: ", discussion_en_md),
  paste0("Discussion CN: ", discussion_cn_md),
  paste0("Manuscript structure: ", manuscript_structure_md),
  paste0("Limitations/safe wording: ", limitations_csv),
  paste0("Title/keywords/highlights: ", title_keywords_md),
  "",
  "Next step:",
  "07A_ML_DATASET_PREPARATION_FOR_IDEAL_AND_SAFETY_MODELS.R",
  "",
  "Journal-rigor note:",
  "06D drafts intentionally avoid real projection/proven safety claims. They should be manually edited before manuscript use."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("06D discussion, abstract and manuscript structure 运行结束\n")
cat("============================================================\n\n")

cat("Datasets in overview：", nrow(dataset_dt), "\n")
cat("Scored objects：", n_scored_objects, "\n")
cat("Scored cells：", n_scored_cells, "\n")
cat("Best favorable dataset：", best_dataset$dataset, "\n")
cat("Second favorable dataset：", second_dataset$dataset, "\n")
cat("Highest DA/projection dataset：", highest_da_dataset$dataset, "\n")
cat("Highest safety-risk dataset：", highest_safety_dataset$dataset, "\n\n")

cat("输出文件：\n")
cat(abstract_en_md, "\n")
cat(abstract_cn_md, "\n")
cat(discussion_en_md, "\n")
cat(discussion_cn_md, "\n")
cat(manuscript_structure_md, "\n")
cat(limitations_csv, "\n")
cat(title_keywords_md, "\n")
cat(report_txt, "\n\n")

cat("✅ 06D discussion, abstract and manuscript structure draft 完成。\n")
cat("下一步进入 07A：准备 ML 数据集，用于 ideal graft-like model 和 safety-risk model。\n")
