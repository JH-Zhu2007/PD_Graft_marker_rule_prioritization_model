# ============================================================
# 04A_MARKER_PANEL_AND_ANNOTATION_PREP.R
# ============================================================
# 目的：
#   接在 03C 后运行。
#
#   04A 不做最终注释。
#   04A 只做期刊级 annotation 前置准备：
#     1. 建立 curated marker panel
#     2. 为 human / rat / mouse-like gene symbol 生成 aliases
#     3. 检查每个 object 中 marker 的覆盖情况
#     4. 输出 dataset-level marker coverage
#     5. 输出后续 04B 注释表达分析所需输入表
#
# 重要严谨性：
#   marker panel 只是 annotation 的证据框架。
#   最终 cell type annotation 必须结合：
#     - marker expression
#     - dataset context
#     - cluster-level enrichment
#     - 多 marker 共同支持
#
#   不能只因为单个 TH 或 MKI67 表达就武断定义细胞类型。
#
# 成功标志：
#   ✅ 04A marker panel and annotation prep 完成。
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"

# 读取哪个对象检查 gene universe：
#   "03A" = 使用 03A reduced/PCA objects，推荐；
#   "02B" = 使用 02B full filtered objects。
GENE_UNIVERSE_SOURCE <- "03A"

# 是否强制重建
REBUILD_EXISTING <- TRUE


# ============================================================
# 1. 加载包
# ============================================================

cat("\n============================================================\n")
cat("04A：marker panel and annotation prep\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "data.table")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 04A。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
  library(data.table)
})


# ============================================================
# 2. 路径
# ============================================================

metadata_dir <- file.path(PROJECT_DIR, "01_metadata")
objects_dir <- file.path(PROJECT_DIR, "02_objects")
tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_main_manifest <- file.path(tables_dir, "03C_strategy", "03C_main_analysis_object_manifest.csv")
input_dataset_role <- file.path(tables_dir, "03C_strategy", "03C_dataset_role_and_usage.csv")

out_tables_dir <- file.path(tables_dir, "04A_annotation_prep")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

marker_master_csv <- file.path(out_tables_dir, "04A_marker_panel_master.csv")
marker_alias_csv <- file.path(out_tables_dir, "04A_marker_panel_alias_long.csv")
object_gene_summary_csv <- file.path(out_tables_dir, "04A_object_gene_universe_summary.csv")
object_marker_coverage_csv <- file.path(out_tables_dir, "04A_object_marker_coverage.csv")
dataset_marker_coverage_csv <- file.path(out_tables_dir, "04A_dataset_marker_coverage_summary.csv")
recommended_marker_csv <- file.path(out_tables_dir, "04A_recommended_marker_sets_for_04B.csv")
report_txt <- file.path(reports_dir, "04A_marker_panel_and_annotation_prep_report.txt")


# ============================================================
# 3. 工具函数
# ============================================================

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path)
  }
  data.table::fread(path, data.table = FALSE)
}

title_case_gene <- function(x) {
  x <- as.character(x)
  out <- ifelse(
    nchar(x) <= 1,
    x,
    paste0(substr(x, 1, 1), tolower(substr(x, 2, nchar(x))))
  )
  out
}

make_gene_aliases <- function(gene) {
  gene <- unique(as.character(gene))
  out <- unique(c(
    gene,
    toupper(gene),
    title_case_gene(gene),
    tolower(gene)
  ))
  out[!is.na(out) & nzchar(out)]
}

get_gene_universe <- function(obj) {
  genes <- tryCatch({
    rownames(obj)
  }, error = function(e) {
    character()
  })

  genes <- unique(as.character(genes))
  genes[!is.na(genes) & nzchar(genes)]
}


# ============================================================
# 4. 建立 curated marker panel
# ============================================================

stamp("建立 marker panel。")

marker_panel <- data.frame(
  category = c(
    rep("DA_core_identity", 10),
    rep("A9_like_DA_identity", 7),
    rep("A10_like_DA_identity", 6),
    rep("midbrain_floor_plate_progenitor", 9),
    rep("neuronal_maturation_synapse", 10),
    rep("progenitor_neuroepithelial", 8),
    rep("cell_cycle_proliferation", 8),
    rep("pluripotency_immature_risk", 6),
    rep("astrocyte_glial", 7),
    rep("oligodendrocyte_OPC", 9),
    rep("microglia_macrophage_immune", 8),
    rep("vascular_pericyte_meningeal", 10),
    rep("GABAergic_neuron", 5),
    rep("glutamatergic_neuron", 5),
    rep("serotonergic_neuron", 4),
    rep("cholinergic_neuron", 3),
    rep("stress_apoptosis_response", 8),
    rep("extracellular_matrix_fibroblast", 8)
  ),
  gene_symbol = c(
    # DA core identity
    "TH", "DDC", "SLC6A3", "SLC18A2", "NR4A2", "FOXA2", "LMX1A", "LMX1B", "PITX3", "EN1",

    # A9-like DA identity
    "ALDH1A1", "KCNJ6", "SOX6", "DCLK3", "GCH1", "SLC10A4", "KCND3",

    # A10-like DA identity
    "CALB1", "OTX2", "CCK", "SLC17A6", "VIP", "NRIP3",

    # midbrain floor plate/progenitor
    "FOXA2", "LMX1A", "LMX1B", "OTX2", "EN1", "EN2", "CORIN", "SHH", "WNT1",

    # neuronal maturation/synapse
    "RBFOX3", "MAP2", "TUBB3", "DCX", "STMN2", "SNAP25", "SYT1", "SYN1", "NEFL", "NEFM",

    # progenitor/neuroepithelial
    "SOX2", "NES", "PAX6", "HES1", "HES5", "VIM", "ASCL1", "DCX",

    # cell cycle/proliferation
    "MKI67", "TOP2A", "PCNA", "MCM2", "MCM5", "CENPF", "UBE2C", "CCNB1",

    # pluripotency/immature risk
    "POU5F1", "NANOG", "LIN28A", "DPPA4", "TERT", "PROM1",

    # astrocyte/glial
    "GFAP", "AQP4", "ALDH1L1", "SLC1A3", "S100B", "SOX9", "CLU",

    # oligodendrocyte/OPC
    "OLIG1", "OLIG2", "PDGFRA", "CSPG4", "SOX10", "MBP", "PLP1", "MOG", "MAG",

    # microglia/macrophage/immune
    "PTPRC", "AIF1", "C1QA", "C1QB", "CX3CR1", "TYROBP", "LST1", "CD74",

    # vascular/pericyte/meningeal
    "PECAM1", "VWF", "KDR", "CLDN5", "PDGFRB", "RGS5", "ACTA2", "COL1A1", "COL1A2", "DCN",

    # GABAergic
    "GAD1", "GAD2", "SLC32A1", "DLX1", "DLX2",

    # glutamatergic
    "SLC17A6", "SLC17A7", "SLC17A8", "TBR1", "NEUROD6",

    # serotonergic
    "TPH2", "SLC6A4", "FEV", "GATA3",

    # cholinergic
    "CHAT", "SLC18A3", "ACHE",

    # stress/apoptosis
    "FOS", "JUN", "JUNB", "HSPA1A", "HSPA1B", "DDIT3", "ATF3", "BAX",

    # ECM/fibroblast
    "COL1A1", "COL1A2", "COL3A1", "DCN", "LUM", "FN1", "THY1", "TAGLN"
  ),
  interpretation = c(
    rep("Dopaminergic neuronal transcriptional identity; requires multi-marker support.", 10),
    rep("A9/substantia-nigra-like DA molecular identity; not proof of in vivo A9 function.", 7),
    rep("A10/VTA-like DA molecular identity; interpret relative to DA core markers.", 6),
    rep("Midbrain floor-plate/progenitor pattern; can indicate developmental stage.", 9),
    rep("Pan-neuronal maturation and synaptic features.", 10),
    rep("Neural progenitor / immature neuroepithelial-like state.", 8),
    rep("Cycling/proliferating cell state; important for graft safety-risk assessment.", 8),
    rep("Pluripotency/immature-risk-associated markers; absence/presence must be validated carefully.", 6),
    rep("Astrocytic or glial-associated identity.", 7),
    rep("OPC/oligodendrocyte lineage markers.", 9),
    rep("Immune/microglia/macrophage-like markers; species/context dependent.", 8),
    rep("Vascular, pericyte, mesenchymal or meningeal-associated markers.", 10),
    rep("GABAergic neuron markers.", 5),
    rep("Glutamatergic neuron markers.", 5),
    rep("Serotonergic neuron markers.", 4),
    rep("Cholinergic neuron markers.", 3),
    rep("Stress/apoptosis/immediate early response; not a cell type alone.", 8),
    rep("Extracellular matrix / fibroblast-like or mesenchymal markers.", 8)
  ),
  use_for = c(
    rep("DA annotation and DA-like score", 10),
    rep("A9-like score", 7),
    rep("A10-like score", 6),
    rep("Developmental-state annotation", 9),
    rep("Neuronal maturation score", 10),
    rep("Progenitor score", 8),
    rep("Cell-cycle/safety-risk score", 8),
    rep("Immature-risk score", 6),
    rep("Off-target/glial annotation", 7),
    rep("Off-target/oligodendrocyte annotation", 9),
    rep("Immune/off-target annotation", 8),
    rep("Vascular/mesenchymal/off-target annotation", 10),
    rep("Neuronal subtype annotation", 5),
    rep("Neuronal subtype annotation", 5),
    rep("Neuronal subtype annotation", 4),
    rep("Neuronal subtype annotation", 3),
    rep("Stress state score", 8),
    rep("ECM/mesenchymal score", 8)
  ),
  stringsAsFactors = FALSE
)

marker_panel$marker_id <- paste0(marker_panel$category, "__", marker_panel$gene_symbol)

atomic_write_csv(marker_panel, marker_master_csv)

# alias long
alias_list <- list()

for (i in seq_len(nrow(marker_panel))) {
  aliases <- make_gene_aliases(marker_panel$gene_symbol[[i]])

  alias_list[[length(alias_list) + 1L]] <- data.frame(
    category = marker_panel$category[[i]],
    gene_symbol = marker_panel$gene_symbol[[i]],
    alias_symbol = aliases,
    marker_id = marker_panel$marker_id[[i]],
    stringsAsFactors = FALSE
  )
}

alias_df <- data.table::rbindlist(alias_list, fill = TRUE)
atomic_write_csv(alias_df, marker_alias_csv)


# ============================================================
# 5. 读取 03C manifest 并检查 marker coverage
# ============================================================

stamp("读取 03C main manifest。")

main_manifest <- read_csv_required(input_main_manifest)
dataset_role_df <- read_csv_required(input_dataset_role)

if (!all(c("dataset", "object_id") %in% colnames(main_manifest))) {
  stop("03C main manifest 缺少 dataset/object_id。")
}

if (GENE_UNIVERSE_SOURCE == "03A") {
  object_path_col <- "initial_pca_object"
} else {
  object_path_col <- "final_expression_object"
}

if (!object_path_col %in% colnames(main_manifest)) {
  stop("main manifest 中缺少对象路径列：", object_path_col)
}

main_manifest <- main_manifest[file.exists(main_manifest[[object_path_col]]), , drop = FALSE]

stamp("准备检查对象数量：", nrow(main_manifest))

object_summary_list <- list()
coverage_list <- list()

for (i in seq_len(nrow(main_manifest))) {
  ds <- main_manifest$dataset[[i]]
  oid <- main_manifest$object_id[[i]]
  path <- main_manifest[[object_path_col]][[i]]

  stamp("检查 gene universe：", i, " / ", nrow(main_manifest), "：", ds, " :: ", oid)

  obj <- tryCatch({
    readRDS(path)
  }, error = function(e) {
    NULL
  })

  if (is.null(obj)) {
    object_summary_list[[length(object_summary_list) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      object_path = path,
      status = "FAILED_READ_OBJECT",
      n_genes = NA_integer_,
      n_cells = NA_integer_,
      stringsAsFactors = FALSE
    )
    next
  }

  genes <- get_gene_universe(obj)

  object_summary_list[[length(object_summary_list) + 1L]] <- data.frame(
    dataset = ds,
    object_id = oid,
    object_path = path,
    status = "SUCCESS",
    n_genes = length(genes),
    n_cells = tryCatch(ncol(obj), error = function(e) NA_integer_),
    stringsAsFactors = FALSE
  )

  genes_upper <- toupper(genes)

  for (catg in unique(marker_panel$category)) {
    panel_genes <- unique(marker_panel$gene_symbol[marker_panel$category == catg])
    panel_upper <- toupper(panel_genes)

    present_upper <- panel_upper[panel_upper %in% genes_upper]
    missing_upper <- setdiff(panel_upper, genes_upper)

    # match actual symbols from object
    present_actual <- genes[match(present_upper, genes_upper)]

    coverage_list[[length(coverage_list) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      category = catg,
      n_panel_genes = length(panel_upper),
      n_present = length(present_upper),
      coverage_fraction = ifelse(length(panel_upper) > 0, length(present_upper) / length(panel_upper), NA_real_),
      present_genes = paste(unique(present_actual), collapse = ";"),
      missing_genes = paste(missing_upper, collapse = ";"),
      stringsAsFactors = FALSE
    )
  }

  rm(obj)
  gc(verbose = FALSE)
}

object_summary_df <- data.table::rbindlist(object_summary_list, fill = TRUE)
coverage_df <- data.table::rbindlist(coverage_list, fill = TRUE)

atomic_write_csv(object_summary_df, object_gene_summary_csv)
atomic_write_csv(coverage_df, object_marker_coverage_csv)


# ============================================================
# 6. dataset-level coverage summary
# ============================================================

coverage_dt <- data.table::as.data.table(coverage_df)

dataset_coverage <- coverage_dt[
  ,
  .(
    n_objects = .N,
    median_coverage = median(coverage_fraction, na.rm = TRUE),
    min_coverage = min(coverage_fraction, na.rm = TRUE),
    max_coverage = max(coverage_fraction, na.rm = TRUE),
    objects_with_coverage_ge_50pct = sum(coverage_fraction >= 0.5, na.rm = TRUE),
    objects_with_coverage_ge_80pct = sum(coverage_fraction >= 0.8, na.rm = TRUE)
  ),
  by = .(dataset, category)
]

atomic_write_csv(as.data.frame(dataset_coverage), dataset_marker_coverage_csv)


# ============================================================
# 7. 推荐 04B 使用的 marker sets
# ============================================================

recommended_categories <- c(
  "DA_core_identity",
  "A9_like_DA_identity",
  "A10_like_DA_identity",
  "midbrain_floor_plate_progenitor",
  "neuronal_maturation_synapse",
  "progenitor_neuroepithelial",
  "cell_cycle_proliferation",
  "pluripotency_immature_risk",
  "astrocyte_glial",
  "oligodendrocyte_OPC",
  "GABAergic_neuron",
  "glutamatergic_neuron",
  "serotonergic_neuron",
  "vascular_pericyte_meningeal",
  "stress_apoptosis_response"
)

recommended_df <- marker_panel[
  marker_panel$category %in% recommended_categories,
  c("category", "gene_symbol", "interpretation", "use_for"),
  drop = FALSE
]

atomic_write_csv(recommended_df, recommended_marker_csv)


# ============================================================
# 8. 报告
# ============================================================

n_objects <- nrow(object_summary_df)
n_success <- sum(object_summary_df$status == "SUCCESS", na.rm = TRUE)
n_failed <- sum(object_summary_df$status != "SUCCESS", na.rm = TRUE)
n_marker_categories <- length(unique(marker_panel$category))
n_markers <- length(unique(marker_panel$gene_symbol))

# key coverage for core datasets
core_cov <- dataset_coverage[
  dataset %in% c("GSE178265_DA_01B", "GSE132758", "GSE204796", "GSE233885") &
    category %in% c("DA_core_identity", "A9_like_DA_identity", "cell_cycle_proliferation", "pluripotency_immature_risk"),
]

core_cov_lines <- if (nrow(core_cov) > 0) {
  apply(
    as.data.frame(core_cov),
    1,
    function(x) paste0(
      x[["dataset"]],
      " / ",
      x[["category"]],
      ": median coverage=",
      round(as.numeric(x[["median_coverage"]]) * 100, 1),
      "%"
    )
  )
} else {
  character()
}

report_lines <- c(
  "04A marker panel and annotation prep report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("Objects checked: ", n_objects),
  paste0("Successfully read objects: ", n_success),
  paste0("Failed objects: ", n_failed),
  paste0("Marker categories: ", n_marker_categories),
  paste0("Unique marker genes: ", n_markers),
  paste0("Gene universe source: ", GENE_UNIVERSE_SOURCE),
  "",
  "Core marker coverage snapshot:",
  core_cov_lines,
  "",
  "Output files:",
  paste0("Marker panel master: ", marker_master_csv),
  paste0("Marker alias long table: ", marker_alias_csv),
  paste0("Object gene universe summary: ", object_gene_summary_csv),
  paste0("Object marker coverage: ", object_marker_coverage_csv),
  paste0("Dataset marker coverage: ", dataset_marker_coverage_csv),
  paste0("Recommended marker sets for 04B: ", recommended_marker_csv),
  "",
  "Next step:",
  "04B_MARKER_EXPRESSION_AND_PRELIMINARY_ANNOTATION.R",
  "",
  "Journal-rigor note:",
  "Marker-based annotation must be supported by multiple markers per cell state. DA-like, A9-like, and projection-associated terms describe transcriptomic identity/competence, not direct functional proof."
)

writeLines(report_lines, report_txt)


# ============================================================
# 9. 结束
# ============================================================

cat("\n============================================================\n")
cat("04A marker panel and annotation prep 运行结束\n")
cat("============================================================\n\n")

cat("检查对象数量：", n_objects, "\n")
cat("成功读取对象：", n_success, "\n")
cat("失败对象：", n_failed, "\n")
cat("marker categories：", n_marker_categories, "\n")
cat("unique marker genes：", n_markers, "\n\n")

cat("输出文件：\n")
cat(marker_master_csv, "\n")
cat(marker_alias_csv, "\n")
cat(object_gene_summary_csv, "\n")
cat(object_marker_coverage_csv, "\n")
cat(dataset_marker_coverage_csv, "\n")
cat(recommended_marker_csv, "\n")
cat(report_txt, "\n\n")

if (n_failed == 0L) {
  cat("✅ 04A marker panel and annotation prep 完成。\n")
  cat("下一步进入 04B：marker expression 和 preliminary annotation。\n")
} else {
  cat("⚠️ 04A 完成，但有对象读取失败。请查看 object gene universe summary。\n")
}
