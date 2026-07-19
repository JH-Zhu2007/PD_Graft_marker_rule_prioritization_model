
PROJECT_DIR <- "D:/PD_Graft_Project"

INPUT_08C_DEG_ALL <- file.path(
  PROJECT_DIR,
  "03_tables",
  "08C_JOURNAL_STANDARD_all_filtered_genes_chunked",
  "08C_JOURNAL_all_filtered_genes_DEG_table.csv"
)

INPUT_08C_CHUNK_AUDIT <- file.path(
  PROJECT_DIR,
  "03_tables",
  "08C_JOURNAL_STANDARD_all_filtered_genes_chunked",
  "08C_JOURNAL_chunk_audit.csv"
)

REQUIRE_08C_CHUNKS_FAILED_ZERO <- TRUE

SPECIES_FOR_MSIGDB <- "Homo sapiens"

MIN_GS_SIZE <- 10
MAX_GS_SIZE <- 500

GSEA_PVALUE_CUTOFF <- 1
GSEA_PADJUST_METHOD <- "BH"
PLOT_PADJ_CUTOFF <- 0.05

PLOT_TOP_N_POSITIVE_NES <- 15
PLOT_TOP_N_NEGATIVE_NES <- 15

PDF_WIDTH <- 10.8
PDF_HEIGHT <- 8.2

SEED <- 20260714

options(timeout = 60000)

cat("\n============================================================\n")
cat("08E Hallmark GSEA FINAL V4\n")
cat("============================================================\n\n")

required_pkgs <- c(
  "data.table",
  "dplyr",
  "ggplot2",
  "fgsea",
  "BiocParallel",
  "msigdbr"
)

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop(
    "缺少 R 包，请先手动安装：",
    paste(missing_pkgs, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(fgsea)
  library(BiocParallel)
  library(msigdbr)
})

BiocParallel::register(BiocParallel::SerialParam(), default = TRUE)

options(error = NULL)
options(bitmapType = "cairo")
set.seed(SEED)

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(tables_dir, "08E_HALLMARK_GSEA_FINAL_V4")
out_figures_dir <- file.path(figures_dir, "08E_HALLMARK_GSEA_FINAL_V4_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

ranked_gene_table_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_ranked_gene_list.csv")
ranked_gene_duplicate_audit_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_ranked_gene_duplicate_audit.csv")
hallmark_term2gene_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_MSigDB_Hallmark_TERM2GENE.csv")
hallmark_geneset_summary_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_geneset_summary.csv")

gsea_all_results_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_all_results.csv")
gsea_significant_results_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_significant_results.csv")
gsea_plot_table_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_plot_table.csv")
gsea_direction_summary_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_direction_summary.csv")

figure_index_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_figure_index.csv")
method_note_txt <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_method_and_claim_boundary_note.txt")
output_check_csv <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_output_verification.csv")
session_info_txt <- file.path(out_tables_dir, "08E_HALLMARK_GSEA_FINAL_V4_sessionInfo.txt")
report_txt <- file.path(reports_dir, "08E_HALLMARK_GSEA_FINAL_V4_report.txt")

gsea_dotplot_pdf <- file.path(out_figures_dir, "08E_HALLMARK_GSEA_FINAL_V4_NES_DOTPLOT.pdf")
gsea_barplot_pdf <- file.path(out_figures_dir, "08E_HALLMARK_GSEA_FINAL_V4_NES_BARPLOT.pdf")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }

  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }

  data.table::fwrite(df, path)

  if (!file.exists(path)) {
    stop("CSV 未生成：", path)
  }

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes <= 0) {
    stop("CSV 已创建但为空或无效：", path)
  }

  invisible(path)
}

save_pdf_plot <- function(plot_obj, file_path, width, height) {
  if (is.null(plot_obj)) {
    stop("plot_obj 是 NULL，不能保存：", file_path)
  }

  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(file_path)) {
    removed <- file.remove(file_path)
    if (!isTRUE(removed)) {
      stop(
        "旧 PDF 正在被占用，无法覆盖：", file_path,
        "\n请关闭 Edge/Adobe/RStudio Viewer/文件资源管理器预览窗口后重跑，或删除旧 PDF。"
      )
    }
  }

  ggplot2::ggsave(
    filename = file_path,
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    device = grDevices::pdf,
    useDingbats = FALSE,
    limitsize = FALSE
  )

  if (!file.exists(file_path)) {
    stop("PDF 未生成：", file_path)
  }

  size_bytes <- file.info(file_path)$size
  if (!is.finite(size_bytes) || size_bytes < 1000) {
    stop("PDF 已创建但文件过小或无效：", file_path, "；size = ", size_bytes)
  }

  message("已保存 PDF：", normalizePath(file_path, winslash = "/", mustWork = TRUE),
          " | size = ", round(size_bytes / 1024, 1), " KB")

  invisible(file_path)
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

clean_gene_keep_length <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- sub("\\..*$", "", x)
  x
}

clean_hallmark_name <- function(x) {
  x <- as.character(x)
  x <- gsub("^HALLMARK_", "", x)
  x <- gsub("_", " ", x)
  x <- tolower(trimws(x))
  x
}

shorten_term <- function(x, max_chars = 70) {
  x <- as.character(x)
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 3), "..."), x)
}

stamp("读取 08C JOURNAL all-filtered DEG table。")

if (!file.exists(INPUT_08C_DEG_ALL)) {
  stop("找不到 08C all-filtered DEG table：", INPUT_08C_DEG_ALL)
}

deg <- fread(INPUT_08C_DEG_ALL, data.table = TRUE, showProgress = FALSE)

required_cols <- c("gene", "avg_log2FC", "p_val_adj")
missing_cols <- setdiff(required_cols, names(deg))

if (length(missing_cols) > 0) {
  stop("08C DEG table 缺少列：", paste(missing_cols, collapse = ", "))
}

if ("p_val" %in% names(deg)) {
  p_rank_col <- "p_val"
} else {
  p_rank_col <- "p_val_adj"
  warning("08C DEG table 没有 p_val 列；08E ranking 将使用 p_val_adj。")
}

deg[, gene := clean_gene_keep_length(gene)]
deg[, avg_log2FC := safe_num(avg_log2FC)]
deg[, p_val_adj := safe_num(p_val_adj)]
deg[, p_for_rank := safe_num(get(p_rank_col))]

deg <- deg[
  !is.na(gene) &
    gene != "" &
    !is.na(avg_log2FC) &
    is.finite(avg_log2FC) &
    !is.na(p_for_rank) &
    is.finite(p_for_rank)
]

if (nrow(deg) == 0) {
  stop("08C DEG table 过滤后没有有效 genes。")
}

if (file.exists(INPUT_08C_CHUNK_AUDIT)) {
  chunk_audit <- fread(INPUT_08C_CHUNK_AUDIT, data.table = TRUE)
  chunks_failed <- sum(grepl("^failed", chunk_audit$status), na.rm = TRUE)

  if (isTRUE(REQUIRE_08C_CHUNKS_FAILED_ZERO) && chunks_failed > 0) {
    stop(
      "08C chunk audit 显示有 failed chunks：", chunks_failed,
      "。期刊标准下不继续 08E。请先修复 08C。"
    )
  }
} else {
  chunks_failed <- NA_integer_
  warning("没有找到 08C chunk audit；08E 会继续，但 final 解释时需要人工确认 08C 完整性。")
}

dataset_name <- if ("dataset" %in% names(deg)) unique(deg$dataset)[1] else "Dataset"
contrast_name <- if ("contrast_name" %in% names(deg)) unique(deg$contrast_name)[1] else "contrast"

stamp("Dataset：", dataset_name)
stamp("Contrast：", contrast_name)
stamp("All tested filtered DEG rows after basic cleaning：", nrow(deg))
stamp("08C chunks failed：", chunks_failed)
stamp("Ranking p-value column：", p_rank_col)

stamp("构建 Hallmark GSEA ranked gene list。")

deg[p_for_rank <= 0 | is.na(p_for_rank), p_for_rank := .Machine$double.xmin]

deg[, signed_rank_metric_raw := sign(avg_log2FC) * (-log10(p_for_rank))]

deg[!is.finite(signed_rank_metric_raw), signed_rank_metric_raw := 0]

duplicate_audit <- deg[, .N, by = gene][N > 1][order(-N, gene)]

deg[, abs_signed_rank_metric := abs(signed_rank_metric_raw)]
deg[, abs_avg_log2FC_for_order := abs(avg_log2FC)]

setorder(
  deg,
  gene,
  -abs_signed_rank_metric,
  p_for_rank,
  -abs_avg_log2FC_for_order
)

rank_dt <- deg[, .SD[1], by = gene]

rank_dt[, tie_breaker := rank(avg_log2FC, ties.method = "first") * 1e-12]
rank_dt[, signed_rank_metric := signed_rank_metric_raw + tie_breaker]

rank_dt <- rank_dt[
  !is.na(gene) &
    gene != "" &
    is.finite(signed_rank_metric)
]

setorder(rank_dt, -signed_rank_metric)

gene_list <- rank_dt$signed_rank_metric
names(gene_list) <- rank_dt$gene

if (any(duplicated(names(gene_list)))) {
  stop("Ranked gene list 仍然存在重复 gene names。")
}

if (length(gene_list) < 100) {
  stop("Ranked gene list 过短，不适合 Hallmark GSEA：", length(gene_list))
}

ranked_gene_out <- rank_dt[, .(
  gene,
  avg_log2FC,
  p_for_rank,
  p_val_adj,
  signed_rank_metric_raw,
  signed_rank_metric,
  rank_position = seq_len(.N)
)]

atomic_write_csv(as.data.frame(ranked_gene_out), ranked_gene_table_csv)
atomic_write_csv(as.data.frame(duplicate_audit), ranked_gene_duplicate_audit_csv)

stamp("Ranked genes used for GSEA：", length(gene_list))
stamp("Duplicate gene symbols resolved：", nrow(duplicate_audit))

stamp("读取 MSigDB Hallmark gene sets。")

get_hallmark_msigdb <- function() {
  out <- tryCatch({
    suppressWarnings(msigdbr::msigdbr(species = SPECIES_FOR_MSIGDB, collection = "H"))
  }, error = function(e1) {
    tryCatch({
      suppressWarnings(msigdbr::msigdbr(species = SPECIES_FOR_MSIGDB, category = "H"))
    }, error = function(e2) {
      NULL
    })
  })

  out
}

msig_h <- get_hallmark_msigdb()

if (is.null(msig_h) || nrow(msig_h) == 0) {
  stop("无法读取 MSigDB Hallmark H collection。")
}

if (!"gs_name" %in% names(msig_h) || !"gene_symbol" %in% names(msig_h)) {
  stop("MSigDB Hallmark 数据缺少 gs_name 或 gene_symbol 列。")
}

hallmark_term2gene <- as.data.table(msig_h)[, .(
  term = as.character(gs_name),
  gene = as.character(gene_symbol)
)]

hallmark_term2gene <- unique(hallmark_term2gene[
  !is.na(term) &
    term != "" &
    !is.na(gene) &
    gene != ""
])

hallmark_term2gene <- hallmark_term2gene[gene %in% names(gene_list)]

geneset_summary <- hallmark_term2gene[, .(
  genes_in_ranked_list = uniqueN(gene)
), by = term][order(term)]

geneset_summary[, pass_size_filter := genes_in_ranked_list >= MIN_GS_SIZE & genes_in_ranked_list <= MAX_GS_SIZE]

atomic_write_csv(as.data.frame(hallmark_term2gene), hallmark_term2gene_csv)
atomic_write_csv(as.data.frame(geneset_summary), hallmark_geneset_summary_csv)

stamp("Hallmark terms total in ranked list：", uniqueN(hallmark_term2gene$term))
stamp("Hallmark TERM2GENE rows in ranked list：", nrow(hallmark_term2gene))
stamp("Hallmark terms passing size filter：", nrow(geneset_summary[pass_size_filter == TRUE]))

if (nrow(geneset_summary[pass_size_filter == TRUE]) < 5) {
  stop("通过 size filter 的 Hallmark gene sets 太少，不适合 GSEA。")
}

stamp("运行 fgsea Hallmark GSEA serial。")

pathway_list <- split(hallmark_term2gene$gene, hallmark_term2gene$term)
pathway_list <- lapply(pathway_list, unique)

pathway_sizes <- vapply(pathway_list, length, numeric(1))
pathway_list <- pathway_list[
  pathway_sizes >= MIN_GS_SIZE &
    pathway_sizes <= MAX_GS_SIZE
]

if (length(pathway_list) < 5) {
  stop("通过 size filter 的 Hallmark pathways 太少，不适合 fgsea。")
}

gene_list <- sort(gene_list, decreasing = TRUE)

fgsea_res <- tryCatch({
  fgsea::fgsea(
    pathways = pathway_list,
    stats = gene_list,
    minSize = MIN_GS_SIZE,
    maxSize = MAX_GS_SIZE,
    eps = 0,
    gseaParam = 1,
    BPPARAM = BiocParallel::SerialParam()
  )
}, error = function(e1) {

  warning("fgsea with BPPARAM failed; retrying with nproc = 0. Error: ", conditionMessage(e1))

  tryCatch({
    fgsea::fgsea(
      pathways = pathway_list,
      stats = gene_list,
      minSize = MIN_GS_SIZE,
      maxSize = MAX_GS_SIZE,
      eps = 0,
      gseaParam = 1,
      nproc = 0
    )
  }, error = function(e2) {
    warning("fgsea with nproc = 0 failed; retrying with nproc = 1. Error: ", conditionMessage(e2))

    fgsea::fgsea(
      pathways = pathway_list,
      stats = gene_list,
      minSize = MIN_GS_SIZE,
      maxSize = MAX_GS_SIZE,
      eps = 0,
      gseaParam = 1,
      nproc = 1
    )
  })
})

gsea_all <- as.data.table(fgsea_res)

if (nrow(gsea_all) == 0) {
  stop("Hallmark fgsea 没有返回任何结果。")
}

setnames(
  gsea_all,
  old = intersect(c("pathway", "pval", "padj", "ES", "size"), names(gsea_all)),
  new = c(
    pathway = "ID",
    pval = "pvalue",
    padj = "p.adjust",
    ES = "enrichmentScore",
    size = "setSize"
  )[intersect(c("pathway", "pval", "padj", "ES", "size"), names(gsea_all))]
)

if (!"ID" %in% names(gsea_all)) {
  stop("fgsea 结果缺少 pathway/ID 列。")
}

gsea_all[, Description := ID]

if ("leadingEdge" %in% names(gsea_all)) {
  gsea_all[, leadingEdge_genes := vapply(
    leadingEdge,
    function(x) paste(as.character(x), collapse = "/"),
    character(1)
  )]
  gsea_all[, leadingEdge := NULL]
}

for (cc in c("NES", "enrichmentScore", "pvalue", "p.adjust", "setSize")) {
  if (cc %in% names(gsea_all)) {
    gsea_all[, (cc) := safe_num(get(cc))]
  }
}

if (!"p.adjust" %in% names(gsea_all)) {
  stop("fgsea 结果缺少 p.adjust / padj 列。")
}

if (!"NES" %in% names(gsea_all)) {
  stop("fgsea 结果缺少 NES 列。")
}

gsea_all[, qvalue := p.adjust]
gsea_all[, Description_clean := shorten_term(clean_hallmark_name(Description), max_chars = 70)]
gsea_all[, NES_direction := ifelse(NES >= 0, "positive_NES_ideal_like_enriched", "negative_NES_lower_priority_enriched")]
gsea_all[, abs_NES := abs(NES)]
gsea_all[, neg_log10_padj := -log10(pmax(p.adjust, .Machine$double.xmin))]
gsea_all[, dataset := dataset_name]
gsea_all[, contrast := contrast_name]
gsea_all[, ranked_gene_n := length(gene_list)]
gsea_all[, hallmark_source := "MSigDB_H_collection"]
gsea_all[, gsea_engine := "fgsea_serial"]
gsea_all[, ranking_metric := paste0("sign(avg_log2FC) * -log10(", p_rank_col, ")")]

setorder(gsea_all, p.adjust, -abs_NES)

gsea_sig <- gsea_all[!is.na(p.adjust) & p.adjust <= PLOT_PADJ_CUTOFF]

atomic_write_csv(as.data.frame(gsea_all), gsea_all_results_csv)
atomic_write_csv(as.data.frame(gsea_sig), gsea_significant_results_csv)

stamp("Hallmark GSEA results all：", nrow(gsea_all))
stamp("Hallmark GSEA significant FDR <= ", PLOT_PADJ_CUTOFF, "：", nrow(gsea_sig))

if (nrow(gsea_sig) == 0) {
  stop(
    "没有 FDR <= ", PLOT_PADJ_CUTOFF,
    " 的 Hallmark GSEA 结果。正式图不使用 non-significant fallback。"
  )
}

plot_pos <- gsea_sig[NES > 0][order(p.adjust, -NES)]
plot_neg <- gsea_sig[NES < 0][order(p.adjust, NES)]

plot_pos <- head(plot_pos, PLOT_TOP_N_POSITIVE_NES)
plot_neg <- head(plot_neg, PLOT_TOP_N_NEGATIVE_NES)

plot_dt <- rbindlist(list(plot_neg, plot_pos), fill = TRUE)

if (nrow(plot_dt) == 0) {
  stop("Hallmark GSEA plot table 为空。")
}

plot_dt[, plot_label := Description_clean]
plot_dt[, plot_order_metric := NES]
setorder(plot_dt, plot_order_metric)
plot_dt[, plot_label_factor := factor(plot_label, levels = unique(plot_label))]

direction_summary <- gsea_all[, .(
  n_terms = .N,
  n_significant_FDR_0_05 = sum(p.adjust <= PLOT_PADJ_CUTOFF, na.rm = TRUE),
  top_term_by_padj = Description[which.min(p.adjust)],
  top_NES = max(NES, na.rm = TRUE),
  bottom_NES = min(NES, na.rm = TRUE)
), by = NES_direction]

atomic_write_csv(as.data.frame(plot_dt), gsea_plot_table_csv)
atomic_write_csv(as.data.frame(direction_summary), gsea_direction_summary_csv)

stamp("生成 Hallmark GSEA PDF 图。")

theme_gsea <- theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "plain", color = "black", size = 14),
    axis.text = element_text(color = "black", size = 11),
    axis.text.y = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    plot.margin = margin(10, 20, 10, 25)
  )

x_max_abs <- max(abs(plot_dt$NES), na.rm = TRUE)
x_lim <- c(-x_max_abs * 1.18, x_max_abs * 1.18)

gsea_dotplot <- ggplot(plot_dt, aes(x = NES, y = plot_label_factor)) +
  geom_vline(xintercept = 0, linewidth = 0.55, color = "grey35") +
  geom_point(aes(size = setSize, color = p.adjust), alpha = 0.95) +
  scale_color_gradient(low = "red", high = "blue", name = "FDR") +
  scale_size_continuous(name = "Set size", range = c(3.2, 8.8)) +
  scale_x_continuous(limits = x_lim) +
  labs(
    title = "Hallmark GSEA: ideal-like vs lower-priority",
    x = "Normalized enrichment score (NES)",
    y = "Hallmark gene set"
  ) +
  annotate(
    "text",
    x = x_lim[2] * 0.98,
    y = nrow(plot_dt) + 0.65,
    label = "ideal-like enriched",
    hjust = 1,
    size = 4.0
  ) +
  annotate(
    "text",
    x = x_lim[1] * 0.98,
    y = nrow(plot_dt) + 0.65,
    label = "lower-priority enriched",
    hjust = 0,
    size = 4.0
  ) +
  theme_gsea

gsea_barplot <- ggplot(plot_dt, aes(x = NES, y = plot_label_factor)) +
  geom_vline(xintercept = 0, linewidth = 0.55, color = "grey35") +
  geom_col(aes(fill = NES_direction), width = 0.72, alpha = 0.90) +
  scale_fill_manual(
    values = c(
      positive_NES_ideal_like_enriched = "red",
      negative_NES_lower_priority_enriched = "darkgreen"
    ),
    name = "Direction",
    labels = c(
      positive_NES_ideal_like_enriched = "ideal-like enriched",
      negative_NES_lower_priority_enriched = "lower-priority enriched"
    )
  ) +
  scale_x_continuous(limits = x_lim) +
  labs(
    title = "Hallmark GSEA NES",
    x = "Normalized enrichment score (NES)",
    y = "Hallmark gene set"
  ) +
  theme_gsea

save_pdf_plot(gsea_dotplot, gsea_dotplot_pdf, width = PDF_WIDTH, height = PDF_HEIGHT)
save_pdf_plot(gsea_barplot, gsea_barplot_pdf, width = PDF_WIDTH, height = PDF_HEIGHT)

figure_index <- data.table(
  figure_id = c("Hallmark_GSEA_NES_dotplot", "Hallmark_GSEA_NES_barplot"),
  title = c(
    "Hallmark GSEA NES dotplot",
    "Hallmark GSEA NES barplot"
  ),
  pdf_path = c(gsea_dotplot_pdf, gsea_barplot_pdf),
  table_path = c(gsea_plot_table_csv, gsea_plot_table_csv),
  pdf_size_bytes = c(file.info(gsea_dotplot_pdf)$size, file.info(gsea_barplot_pdf)$size),
  plot_type = c("GSEA_NES_dotplot", "GSEA_NES_barplot"),
  plot_engine = "ggplot2"
)

atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

method_lines <- c(
  "08E Hallmark GSEA FINAL V4 method and claim-boundary note",
  "",
  "Method-ready wording:",
  paste0(
    "Hallmark gene set enrichment analysis was performed using fgsea with a serial BiocParallel backend and MSigDB Hallmark gene sets obtained through the msigdbr R package. ",
    "The ranked gene list included all genes tested in the 08C differential expression analysis after objective expression filtering. ",
    "Genes were ranked by sign(avg_log2FC) multiplied by -log10(", p_rank_col, "). ",
    "Duplicate gene symbols were resolved by retaining the record with the largest absolute ranking metric. ",
    "The enrichment universe was therefore not restricted to significant DEGs. ",
    "GSEA results were adjusted using the Benjamini-Hochberg method, and final visualization included Hallmark terms with FDR <= ", PLOT_PADJ_CUTOFF, "."
  ),
  "",
  "Strict parameters:",
  paste0("Ranked genes: ", length(gene_list)),
  paste0("Ranking metric: sign(avg_log2FC) * -log10(", p_rank_col, ")"),
  paste0("GSEA method: fgsea serial backend; pAdjustMethod = ", GSEA_PADJUST_METHOD),
  paste0("minGSSize = ", MIN_GS_SIZE, "; maxGSSize = ", MAX_GS_SIZE),
  paste0("Plot threshold: FDR <= ", PLOT_PADJ_CUTOFF),
  "",
  "Claim boundary:",
  "08E inherits the claim boundary of 08C. If 08C is a single-object cell-state contrast, then 08E supports ranked-gene pathway-level interpretation of that contrast, not sample-level pseudo-bulk validation, functional proof, treatment efficacy, or clinical safety."
)

writeLines(method_lines, method_note_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

report_lines <- c(
  "08E Hallmark GSEA FINAL V4 report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input:",
  INPUT_08C_DEG_ALL,
  "",
  "Dataset / contrast:",
  paste0("Dataset: ", dataset_name),
  paste0("Contrast: ", contrast_name),
  "",
  "Ranked gene list:",
  paste0("Ranking p-value column: ", p_rank_col),
  paste0("Ranked genes: ", length(gene_list)),
  paste0("Duplicate gene symbols resolved: ", nrow(duplicate_audit)),
  "",
  "Hallmark gene sets:",
  paste0("Hallmark terms in ranked list: ", uniqueN(hallmark_term2gene$term)),
  paste0("Hallmark TERM2GENE rows in ranked list: ", nrow(hallmark_term2gene)),
  paste0("Hallmark terms passing size filter: ", nrow(geneset_summary[pass_size_filter == TRUE])),
  "",
  "GSEA results:",
  paste0("All Hallmark GSEA rows: ", nrow(gsea_all)),
  paste0("Significant Hallmark GSEA rows FDR <= ", PLOT_PADJ_CUTOFF, ": ", nrow(gsea_sig)),
  "",
  "Figures:",
  capture.output(print(figure_index)),
  "",
  "Output directories:",
  out_tables_dir,
  out_figures_dir
)

writeLines(report_lines, report_txt)

required_output_files <- c(
  ranked_gene_table_csv,
  ranked_gene_duplicate_audit_csv,
  hallmark_term2gene_csv,
  hallmark_geneset_summary_csv,
  gsea_all_results_csv,
  gsea_significant_results_csv,
  gsea_plot_table_csv,
  gsea_direction_summary_csv,
  figure_index_csv,
  method_note_txt,
  report_txt,
  session_info_txt,
  gsea_dotplot_pdf,
  gsea_barplot_pdf
)

output_check <- data.table(
  file = required_output_files,
  exists = file.exists(required_output_files),
  size_bytes = ifelse(file.exists(required_output_files), file.info(required_output_files)$size, NA_real_)
)

atomic_write_csv(as.data.frame(output_check), output_check_csv)

missing_required <- output_check[!exists | is.na(size_bytes) | size_bytes <= 0]

if (nrow(missing_required) > 0) {
  print(missing_required)
  stop("08E Hallmark GSEA FINAL V4 未通过输出验证。")
}

cat("\n============================================================\n")
cat("08E Hallmark GSEA FINAL V4 运行结束\n")
cat("============================================================\n\n")

cat("Dataset：", dataset_name, "\n")
cat("Contrast：", contrast_name, "\n")
cat("Ranked genes：", length(gene_list), "\n")
cat("Ranking p-value column：", p_rank_col, "\n")
cat("Hallmark terms in ranked list：", uniqueN(hallmark_term2gene$term), "\n")
cat("All Hallmark GSEA rows：", nrow(gsea_all), "\n")
cat("Significant Hallmark GSEA rows FDR <= ", PLOT_PADJ_CUTOFF, "：", nrow(gsea_sig), "\n\n", sep = "")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("主要 PDF 图：\n")
cat(gsea_dotplot_pdf, "\n")
cat(gsea_barplot_pdf, "\n\n")

cat("关键文件：\n")
cat(ranked_gene_table_csv, "\n")
cat(gsea_all_results_csv, "\n")
cat(gsea_significant_results_csv, "\n")
cat(gsea_plot_table_csv, "\n")
cat(method_note_txt, "\n")
cat(report_txt, "\n\n")

cat("✅ 08E Hallmark GSEA FINAL V4 完成。\n")
