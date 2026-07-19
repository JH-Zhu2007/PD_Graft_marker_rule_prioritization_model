# ============================================================
# FINAL MINIMAL SAFE NOTE
# ============================================================
# 这是基于用户上传并确认可运行的 08D1 GO V2 脚本做的最小修改版。
# 明确没有修改：
#   - make_go_gene_term_plot()
#   - ggplot layers
#   - aes()
#   - representative gene-term link selection logic
# 只修改：
#   - 输出目录/文件名前缀，避免旧 PDF 被占用
#   - 删除全局 repos 覆盖，避免 Bioconductor repository warning
#   - GO_ALLOW_TOP_TERMS_IF_NO_SIGNIFICANT = FALSE
#   - 连线透明度参数轻微调低
# ============================================================

# ============================================================
# 08D1_GO_FINAL_VERIFIED_V2_MINIMAL_SAFE.R
# ============================================================
# 08D1：GO only
#
# 目的：
#   只做 GO enrichment + GO gene-term relationship 图。
#   不再包含 KEGG。
#
# 设计：
#   - GO UP 单独一张 gene-term relationship 图
#   - GO DOWN 单独一张 gene-term relationship 图
#   - BP / CC / MF 在右侧分区展示
#   - 左侧 gene 节点
#   - 中间 GO term 节点
#   - 连线为真实 gene-term membership
#   - 右侧气泡为 GeneRatio / Count / -log10(p.adjust)
#
# V2 关键修复：
#   1. 保留 V1 的 GO_UP representative-link 逻辑。
#   2. 修复 GO_DOWN 在 geom_text 第 3 层计算 aesthetics 时触发 fetch_ggproto / locked 'res' 的问题：
#      不再在 aes() 里写 (strip_xmin + strip_xmax)/2、min(panel_df$ymin) 这类表达式；
#      全部提前预计算成普通列。
#
# V1 关键修复：
#   GO_UP 中 OXPHOS / respiratory-chain term 高度重叠。
#   旧版若优先选择 hub genes，会让 GO_UP 看起来像 every term -> every gene。
#   本版改为：
#     1. 每个 term 先选代表性 genes
#     2. 每个 term 最多显示 GO_MAX_GENES_PER_TERM 个 genes
#     3. 每个 gene 最多连接 GO_MAX_TERMS_PER_GENE 个 terms
#     4. 全图最多 GO_MAX_GENES 个 genes
#   所有显示连线仍然是真实 gene-term membership。
#
# 输出：
#   D:/PD_Graft_Project/03_tables/08D1_GO_FINAL_VERIFIED_V2/
#   D:/PD_Graft_Project/04_figures/08D1_GO_FINAL_VERIFIED_V2_pdf/
#
# 成功标志：
#   ✅ 08D1 GO FINAL VERIFIED V2 完成。
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

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

DEG_PADJ_CUTOFF <- 0.05
DEG_LOG2FC_CUTOFF <- 0.25

ENRICH_PVALUE_CUTOFF <- 1
ENRICH_QVALUE_CUTOFF <- 1

PLOT_PADJ_CUTOFF <- 0.05
MIN_GENES_FOR_ENRICHMENT <- 10
MIN_GS_SIZE <- 10
MAX_GS_SIZE <- 500

GO_TOP_TERMS_PER_ONTOLOGY <- 5
GO_MAX_GENES <- 42
GO_MAX_GENES_PER_TERM <- 8
GO_MAX_TERMS_PER_GENE <- 4
GO_ALLOW_TOP_TERMS_IF_NO_SIGNIFICANT <- FALSE

# 连线：比你当前 GO_DOWN 再淡一点点，但仍然清楚。
GO_LINK_ALPHA_IN_COLOR <- 0.26
GO_LINK_LAYER_ALPHA <- 0.55
GO_LINK_WIDTH <- 0.42

GO_PDF_WIDTH <- 13.5
GO_PDF_HEIGHT <- 12.5

SEED <- 20260714


# ============================================================
# 1. 加载包
# ============================================================

# 不覆盖 Bioconductor repositories，避免反复出现：
# 'getOption("repos")' replaces Bioconductor standard repositories
options(timeout = 60000)

cat("\n============================================================\n")
cat("08D1 GO FINAL VERIFIED V2：GO gene-term relationship UP/DOWN\n")
cat("============================================================\n\n")

required_pkgs <- c(
  "data.table",
  "dplyr",
  "ggplot2",
  "clusterProfiler",
  "msigdbr"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  cran_pkgs <- intersect(missing_pkgs, c("data.table", "dplyr", "ggplot2", "msigdbr"))
  bioc_pkgs <- setdiff(missing_pkgs, cran_pkgs)

  if (length(cran_pkgs) > 0) {
    install.packages(cran_pkgs, repos = "https://cloud.r-project.org")
  }

  if (length(bioc_pkgs) > 0) {
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager", repos = "https://cloud.r-project.org")
    }
    BiocManager::install(bioc_pkgs, ask = FALSE, update = FALSE, force = FALSE)
  }
}

missing_pkgs2 <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs2) > 0) {
  stop("以下 R 包仍然缺失：", paste(missing_pkgs2, collapse = ", "))
}

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(clusterProfiler)
  library(msigdbr)
})

options(error = NULL)
options(bitmapType = "cairo")
set.seed(SEED)


# ============================================================
# 2. 输出路径
# ============================================================

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(tables_dir, "08D1_GO_FINAL_VERIFIED_V2")
out_figures_dir <- file.path(figures_dir, "08D1_GO_FINAL_VERIFIED_V2_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

gene_list_summary_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_gene_list_summary.csv")
gene_list_long_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_gene_lists_long.csv")
go_term2gene_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_TERM2GENE_MSigDB_C5_GO.csv")

go_enrichment_all_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_all_GO_enrichment_results.csv")
go_enrichment_sig_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_significant_GO_enrichment_results.csv")
go_task_status_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_enrichment_task_status.csv")

go_up_plot_table_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_UP_gene_term_plot_table.csv")
go_down_plot_table_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_DOWN_gene_term_plot_table.csv")
go_up_link_table_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_UP_gene_term_links.csv")
go_down_link_table_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_DOWN_gene_term_links.csv")

figure_index_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_figure_index.csv")
method_note_txt <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_method_and_claim_boundary_note.txt")
output_check_csv <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_output_verification.csv")
session_info_txt <- file.path(out_tables_dir, "08D1_GO_FINAL_VERIFIED_V2_sessionInfo.txt")
report_txt <- file.path(reports_dir, "08D1_GO_FINAL_VERIFIED_V2_report.txt")

go_up_pdf <- file.path(out_figures_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_UP_gene_term_relationship_BP_CC_MF.pdf")
go_down_pdf <- file.path(out_figures_dir, "08D1_GO_FINAL_VERIFIED_V2_GO_DOWN_gene_term_relationship_BP_CC_MF.pdf")


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
    unlink(file_path, force = TRUE)
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

clean_gene <- function(x) {
  x <- unique(as.character(x))
  x <- trimws(x)
  x <- x[!is.na(x)]
  x <- x[x != "" & x != "-"]
  x <- sub("\\..*$", "", x)
  unique(x)
}

clean_gene_keep_length <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- sub("\\..*$", "", x)
  x
}

parse_ratio <- function(x) {
  sapply(strsplit(as.character(x), "/", fixed = TRUE), function(z) {
    if (length(z) == 2) {
      return(as.numeric(z[1]) / as.numeric(z[2]))
    } else {
      return(as.numeric(z[1]))
    }
  })
}

detect_go_ontology <- function(x) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  out[grepl("^GOBP_|BIOLOGICAL_PROCESS|\\bBP\\b", x, ignore.case = TRUE)] <- "BP"
  out[grepl("^GOCC_|CELLULAR_COMPONENT|\\bCC\\b", x, ignore.case = TRUE)] <- "CC"
  out[grepl("^GOMF_|MOLECULAR_FUNCTION|\\bMF\\b", x, ignore.case = TRUE)] <- "MF"
  out
}

clean_go_term <- function(x) {
  x <- as.character(x)
  x <- gsub("^GOBP_", "", x)
  x <- gsub("^GOCC_", "", x)
  x <- gsub("^GOMF_", "", x)
  x <- gsub("_", " ", x)
  x <- tolower(trimws(x))
  x
}

shorten_term <- function(x, max_chars = 72) {
  x <- as.character(x)
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 3), "..."), x)
}

split_gene_id <- function(x) {
  x <- as.character(x)
  x <- strsplit(x, "/", fixed = TRUE)
  unique(unlist(x))
}

ontology_strip_fill <- c(
  BP = "#E6EEF7",
  CC = "#EAF4EA",
  MF = "#F3E6F3"
)

make_distinct_palette <- function(n, alpha = 1) {
  base_cols <- c(
    "#7FCDBB", "#B2DF8A", "#FDBF6F", "#CAB2D6", "#A6CEE3",
    "#FB9A99", "#FFFF99", "#B15928", "#8DD3C7", "#BEBADA",
    "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5",
    "#BC80BD", "#CCEBC5", "#FFED6F", "#1F78B4", "#33A02C",
    "#E31A1C", "#FF7F00", "#6A3D9A", "#A6761D", "#666666"
  )

  if (n <= length(base_cols)) {
    cols <- base_cols[seq_len(n)]
  } else {
    cols <- grDevices::colorRampPalette(base_cols)(n)
  }

  grDevices::adjustcolor(cols, alpha.f = alpha)
}


# ============================================================
# 4. 读取 08C DEG
# ============================================================

stamp("读取 08C JOURNAL DEG table。")

if (!file.exists(INPUT_08C_DEG_ALL)) {
  stop("找不到 08C all-filtered DEG table：", INPUT_08C_DEG_ALL)
}

deg <- fread(INPUT_08C_DEG_ALL, data.table = TRUE, showProgress = FALSE)

required_deg_cols <- c("gene", "avg_log2FC", "p_val_adj")
missing_cols <- setdiff(required_deg_cols, names(deg))
if (length(missing_cols) > 0) {
  stop("08C DEG table 缺少列：", paste(missing_cols, collapse = ", "))
}

deg[, gene := clean_gene_keep_length(gene)]
deg[, avg_log2FC := safe_num(avg_log2FC)]
deg[, p_val_adj := safe_num(p_val_adj)]

deg <- deg[
  !is.na(gene) &
    gene != "" &
    !is.na(avg_log2FC) &
    !is.na(p_val_adj)
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
      "。期刊标准下不继续 08D1。请先修复 08C。"
    )
  }
} else {
  chunks_failed <- NA_integer_
  warning("没有找到 08C chunk audit；08D1 会继续，但 final 解释时需要人工确认 08C 完整性。")
}

dataset_name <- if ("dataset" %in% names(deg)) unique(deg$dataset)[1] else "Dataset"
contrast_name <- if ("contrast_name" %in% names(deg)) unique(deg$contrast_name)[1] else "contrast"

stamp("Dataset：", dataset_name)
stamp("Contrast：", contrast_name)
stamp("All tested filtered genes rows：", nrow(deg))
stamp("08C chunks failed：", chunks_failed)


# ============================================================
# 5. 定义 universe / UP / DOWN gene lists
# ============================================================

universe_genes <- clean_gene(deg$gene)

up_genes <- clean_gene(deg[
  p_val_adj < DEG_PADJ_CUTOFF &
    avg_log2FC >= DEG_LOG2FC_CUTOFF,
  gene
])

down_genes <- clean_gene(deg[
  p_val_adj < DEG_PADJ_CUTOFF &
    avg_log2FC <= -DEG_LOG2FC_CUTOFF,
  gene
])

gene_list_summary <- data.table(
  list_name = c("universe_all_tested_filtered_genes", "UP_in_A_ideal_like", "DOWN_in_A_up_in_B_lower_priority"),
  definition = c(
    "All genes tested in 08C after objective expression filtering",
    paste0("p_adj < ", DEG_PADJ_CUTOFF, " and avg_log2FC >= ", DEG_LOG2FC_CUTOFF),
    paste0("p_adj < ", DEG_PADJ_CUTOFF, " and avg_log2FC <= -", DEG_LOG2FC_CUTOFF)
  ),
  n_gene_symbols = c(length(universe_genes), length(up_genes), length(down_genes))
)

gene_list_long <- rbindlist(list(
  data.table(list_name = "universe_all_tested_filtered_genes", gene = universe_genes),
  data.table(list_name = "UP_in_A_ideal_like", gene = up_genes),
  data.table(list_name = "DOWN_in_A_up_in_B_lower_priority", gene = down_genes)
), fill = TRUE)

atomic_write_csv(as.data.frame(gene_list_summary), gene_list_summary_csv)
atomic_write_csv(as.data.frame(gene_list_long), gene_list_long_csv)

stamp("Universe genes：", length(universe_genes))
stamp("UP genes：", length(up_genes))
stamp("DOWN genes：", length(down_genes))

if (length(up_genes) < MIN_GENES_FOR_ENRICHMENT && length(down_genes) < MIN_GENES_FOR_ENRICHMENT) {
  stop("UP 和 DOWN genes 都少于 MIN_GENES_FOR_ENRICHMENT，无法做稳定 GO 富集。")
}


# ============================================================
# 6. 读取 MSigDB GO gene sets
# ============================================================

stamp("读取 MSigDB GO gene sets。")

get_msigdb_safe <- function(collection, subcollection = NULL) {
  out <- tryCatch({
    if (is.null(subcollection)) {
      suppressWarnings(msigdbr::msigdbr(species = "Homo sapiens", collection = collection))
    } else {
      suppressWarnings(msigdbr::msigdbr(species = "Homo sapiens", collection = collection, subcollection = subcollection))
    }
  }, error = function(e1) {
    tryCatch({
      if (is.null(subcollection)) {
        suppressWarnings(msigdbr::msigdbr(species = "Homo sapiens", category = collection))
      } else {
        suppressWarnings(msigdbr::msigdbr(species = "Homo sapiens", category = collection, subcategory = subcollection))
      }
    }, error = function(e2) {
      NULL
    })
  })

  out
}

msig_go_raw <- get_msigdb_safe("C5")
if (is.null(msig_go_raw) || nrow(msig_go_raw) == 0) {
  stop("无法读取 MSigDB C5 GO gene sets。")
}

msig_go <- msig_go_raw

go_keep <- rep(FALSE, nrow(msig_go))

if ("gs_name" %in% names(msig_go)) {
  go_keep <- go_keep |
    grepl("^GOBP_", msig_go$gs_name) |
    grepl("^GOCC_", msig_go$gs_name) |
    grepl("^GOMF_", msig_go$gs_name)
}

if ("gs_subcat" %in% names(msig_go)) {
  go_keep <- go_keep | grepl("GO", msig_go$gs_subcat, ignore.case = TRUE)
}

if ("gs_subcollection" %in% names(msig_go)) {
  go_keep <- go_keep | grepl("GO", msig_go$gs_subcollection, ignore.case = TRUE)
}

if (any(go_keep)) {
  msig_go <- msig_go[go_keep, ]
}

if (!"gs_name" %in% names(msig_go) || !"gene_symbol" %in% names(msig_go)) {
  stop("MSigDB GO 数据缺少 gs_name 或 gene_symbol 列。")
}

go_term2gene <- as.data.table(msig_go)[, .(term = as.character(gs_name), gene = as.character(gene_symbol))]
go_term2gene <- unique(go_term2gene[!is.na(term) & !is.na(gene) & gene != ""])
go_term2gene <- go_term2gene[gene %in% universe_genes]

atomic_write_csv(as.data.frame(go_term2gene), go_term2gene_csv)

stamp("GO terms in universe：", length(unique(go_term2gene$term)))
stamp("GO TERM2GENE rows：", nrow(go_term2gene))

if (nrow(go_term2gene) < MIN_GENES_FOR_ENRICHMENT) {
  stop("GO TERM2GENE 太少，无法做 GO 富集。")
}


# ============================================================
# 7. clusterProfiler::enricher GO enrichment
# ============================================================

stamp("运行 clusterProfiler::enricher GO enrichment。")

run_go_enricher_task <- function(task_id, Direction, biological_direction, genes) {
  out <- tryCatch({
    obj <- clusterProfiler::enricher(
      gene = unique(genes),
      universe = unique(universe_genes),
      TERM2GENE = as.data.frame(go_term2gene[, .(term, gene)]),
      pAdjustMethod = "BH",
      pvalueCutoff = ENRICH_PVALUE_CUTOFF,
      qvalueCutoff = ENRICH_QVALUE_CUTOFF,
      minGSSize = MIN_GS_SIZE,
      maxGSSize = MAX_GS_SIZE
    )

    dt <- as.data.table(as.data.frame(obj))

    if (nrow(dt) > 0) {
      dt[, task_id := task_id]
      dt[, enrichment_type := "GO"]
      dt[, Direction := Direction]
      dt[, biological_direction := biological_direction]
      dt[, input_gene_symbol_n := length(unique(genes))]
      dt[, universe_gene_symbol_n := length(unique(universe_genes))]
      dt[, gene_set_source := "MSigDB_C5_GO"]
      dt[, enrichment_method := "clusterProfiler::enricher"]
      dt[, GeneRatioNum := parse_ratio(GeneRatio)]
      dt[, p.adjust := as.numeric(p.adjust)]
      dt[, Count := as.numeric(Count)]
      dt[, Description := as.character(Description)]
      dt[, ID := as.character(ID)]
      dt[, ontology := detect_go_ontology(ID)]
      dt[is.na(ontology), ontology := detect_go_ontology(Description)]
    }

    list(
      task_id = task_id,
      Direction = Direction,
      biological_direction = biological_direction,
      status = "ok",
      message = NA_character_,
      n_terms = nrow(dt),
      result = dt
    )
  }, error = function(e) {
    list(
      task_id = task_id,
      Direction = Direction,
      biological_direction = biological_direction,
      status = "failed",
      message = conditionMessage(e),
      n_terms = 0L,
      result = data.table()
    )
  })

  out
}

go_results <- list()

if (length(up_genes) >= MIN_GENES_FOR_ENRICHMENT) {
  stamp("GO enrichment task：GO_UP")
  go_results[["GO_UP"]] <- run_go_enricher_task("GO_UP", "UP", "UP_in_A_ideal_like", up_genes)
}

if (length(down_genes) >= MIN_GENES_FOR_ENRICHMENT) {
  stamp("GO enrichment task：GO_DOWN")
  go_results[["GO_DOWN"]] <- run_go_enricher_task("GO_DOWN", "DOWN", "DOWN_in_A_up_in_B_lower_priority", down_genes)
}

go_task_status <- rbindlist(lapply(go_results, function(x) {
  data.table(
    task_id = x$task_id,
    enrichment_type = "GO",
    Direction = x$Direction,
    biological_direction = x$biological_direction,
    status = x$status,
    message = x$message,
    n_terms = x$n_terms
  )
}), fill = TRUE)

go_enrich_all <- rbindlist(lapply(go_results, function(x) x$result), fill = TRUE)

if (nrow(go_enrich_all) > 0) {
  go_enrich_all <- go_enrich_all[order(Direction, ontology, p.adjust)]
}

go_enrich_sig <- if (nrow(go_enrich_all) > 0) {
  go_enrich_all[!is.na(p.adjust) & p.adjust <= PLOT_PADJ_CUTOFF]
} else {
  data.table()
}

atomic_write_csv(as.data.frame(go_task_status), go_task_status_csv)
atomic_write_csv(as.data.frame(go_enrich_all), go_enrichment_all_csv)
atomic_write_csv(as.data.frame(go_enrich_sig), go_enrichment_sig_csv)

stamp("GO tasks failed：", nrow(go_task_status[status == "failed"]))
stamp("GO enrichment rows all：", nrow(go_enrich_all))
stamp("GO enrichment rows significant：", nrow(go_enrich_sig))


# ============================================================
# 8. GO Gene-Term relationship plot
# ============================================================

select_go_terms_for_relation <- function(enrich_dt, direction_value) {
  dt <- as.data.table(enrich_dt)
  dt <- dt[
    Direction == direction_value &
      ontology %in% c("BP", "CC", "MF") &
      !is.na(geneID) &
      geneID != ""
  ]

  if (nrow(dt) == 0) return(data.table())

  dt_sig <- dt[p.adjust <= PLOT_PADJ_CUTOFF]
  display_mode <- "significant_FDR_le_0.05"

  if (nrow(dt_sig) == 0) {
    if (!isTRUE(GO_ALLOW_TOP_TERMS_IF_NO_SIGNIFICANT)) {
      return(data.table())
    }
    dt_sig <- copy(dt)
    display_mode <- "fallback_top_ranked_not_significant"
  }

  out <- rbindlist(lapply(c("BP", "CC", "MF"), function(ont) {
    sub <- dt_sig[ontology == ont]
    if (nrow(sub) == 0) return(data.table())
    sub <- sub[order(p.adjust, -Count)]
    head(sub, GO_TOP_TERMS_PER_ONTOLOGY)
  }), fill = TRUE)

  if (nrow(out) > 0) {
    out[, display_mode := display_mode]
    out[, term_clean := shorten_term(clean_go_term(Description), max_chars = 68)]
    out[, neg_log10_padj := -log10(p.adjust)]
  }

  out
}

build_go_relation_tables <- function(go_terms_dt, direction_value) {
  if (nrow(go_terms_dt) == 0) {
    return(list(term_df = data.table(), gene_df = data.table(), link_df = data.table()))
  }

  go_terms_dt <- copy(go_terms_dt)
  go_terms_dt[, ontology := factor(ontology, levels = c("BP", "CC", "MF"))]
  setorder(go_terms_dt, ontology, p.adjust, -Count)

  links <- rbindlist(lapply(seq_len(nrow(go_terms_dt)), function(i) {
    genes_i <- split_gene_id(go_terms_dt$geneID[i])
    genes_i <- genes_i[genes_i %in% if (direction_value == "UP") up_genes else down_genes]

    if (length(genes_i) == 0) return(data.table())

    data.table(
      term_id = go_terms_dt$ID[i],
      term_clean = go_terms_dt$term_clean[i],
      ontology = as.character(go_terms_dt$ontology[i]),
      Direction = direction_value,
      gene = genes_i
    )
  }), fill = TRUE)

  if (nrow(links) == 0) {
    return(list(term_df = data.table(), gene_df = data.table(), link_df = data.table()))
  }

  gene_stats <- unique(deg[, .(gene, avg_log2FC, p_val_adj)])

  links <- merge(
    links,
    gene_stats,
    by = "gene",
    all.x = TRUE
  )

  links[is.na(avg_log2FC), avg_log2FC := 0]
  links[is.na(p_val_adj), p_val_adj := 1]

  links[, direction_log2FC := if (direction_value == "UP") avg_log2FC else -avg_log2FC]
  links[is.na(direction_log2FC) | direction_log2FC < 0, direction_log2FC := 0]
  links[, gene_priority := -log10(pmax(p_val_adj, .Machine$double.xmin)) * (direction_log2FC + 0.01)]

  term_priority <- go_terms_dt[, .(
    term_id = ID,
    term_p_adjust = p.adjust,
    term_count = Count,
    term_gene_ratio = GeneRatioNum
  )]

  links <- merge(
    links,
    term_priority,
    by = "term_id",
    all.x = TRUE
  )

  setorder(links, term_id, -gene_priority, p_val_adj, -direction_log2FC, gene)
  links <- links[, head(.SD, GO_MAX_GENES_PER_TERM), by = term_id]

  setorder(links, gene, term_p_adjust, -gene_priority, term_id)
  links <- links[, head(.SD, GO_MAX_TERMS_PER_GENE), by = gene]

  gene_rank <- links[, .(
    link_count = .N,
    best_gene_priority = max(gene_priority, na.rm = TRUE),
    best_p = min(p_val_adj, na.rm = TRUE)
  ), by = gene][order(-best_gene_priority, best_p, -link_count, gene)]

  keep_genes <- head(gene_rank$gene, GO_MAX_GENES)
  links <- links[gene %in% keep_genes]

  keep_terms <- unique(links$term_id)
  go_terms_dt <- go_terms_dt[ID %in% keep_terms]

  term_df_list <- list()
  y_current <- 1

  for (ont in c("MF", "CC", "BP")) {
    sub <- go_terms_dt[as.character(ontology) == ont]
    if (nrow(sub) == 0) next

    sub <- sub[order(p.adjust, -Count)]
    sub[, y := seq(from = y_current, length.out = .N)]
    y_current <- max(sub$y) + 1.6
    term_df_list[[ont]] <- sub
  }

  term_df <- rbindlist(term_df_list, fill = TRUE)

  if (nrow(term_df) == 0) {
    return(list(term_df = data.table(), gene_df = data.table(), link_df = data.table()))
  }

  term_palette <- make_distinct_palette(nrow(term_df), alpha = 1)
  term_df[, term_color := term_palette]

  links <- merge(
    links,
    term_df[, .(term_id = ID, term_y = y, GeneRatioNum, Count, p.adjust, neg_log10_padj, display_mode, term_color)],
    by = "term_id",
    all.x = TRUE
  )

  links <- links[!is.na(term_y)]

  gene_rank <- links[, .N, by = gene][order(-N, gene)]
  gene_rank[, y := seq(from = min(term_df$y), to = max(term_df$y), length.out = .N)]

  gene_df <- gene_rank[, .(gene, gene_y = y, link_count = N)]

  gene_palette <- make_distinct_palette(nrow(gene_df), alpha = 0.85)
  gene_df[, gene_color := gene_palette]

  links <- merge(links, gene_df, by = "gene", all.x = TRUE)
  links[, link_color := grDevices::adjustcolor(term_color, alpha.f = GO_LINK_ALPHA_IN_COLOR)]

  list(term_df = term_df, gene_df = gene_df, link_df = links)
}

make_go_gene_term_plot <- function(go_terms_dt, direction_value, plot_title) {
  tbl <- build_go_relation_tables(go_terms_dt, direction_value)

  term_df <- tbl$term_df
  gene_df <- tbl$gene_df
  link_df <- tbl$link_df

  if (nrow(term_df) == 0 || nrow(gene_df) == 0 || nrow(link_df) == 0) {
    stop("GO ", direction_value, " gene-term plot table 为空，拒绝输出白图。")
  }

  dot_x_min <- 0.68
  dot_x_max <- 0.96
  dot_width <- dot_x_max - dot_x_min

  gr_max <- max(term_df$GeneRatioNum, na.rm = TRUE)
  if (!is.finite(gr_max) || gr_max <= 0) gr_max <- 0.01

  term_df[, dot_x := dot_x_min + (GeneRatioNum / gr_max) * dot_width]

  panel_df <- term_df[, .(
    ymin = min(y) - 0.5,
    ymax = max(y) + 0.5,
    ymid = mean(range(y))
  ), by = ontology]

  panel_df[, xmin := dot_x_min - 0.02]
  panel_df[, xmax := dot_x_max + 0.02]
  panel_df[, strip_xmin := dot_x_max + 0.025]
  panel_df[, strip_xmax := dot_x_max + 0.055]

  # V2：提前预计算 aes 需要的所有坐标/标签，避免在 aes() 中使用表达式导致 locked 'res'。
  panel_df[, strip_xmid := (strip_xmin + strip_xmax) / 2]
  panel_df[, ontology_label := as.character(ontology)]
  panel_df[, strip_fill_col := ontology_strip_fill[as.character(ontology)]]

  y_grid_min_value <- min(panel_df$ymin)
  y_grid_max_value <- max(panel_df$ymax)

  gene_df[, xmin := 0.035]
  gene_df[, xmax := 0.058]
  gene_df[, ymin := gene_y - 0.33]
  gene_df[, ymax := gene_y + 0.33]

  term_df[, xmin := 0.580]
  term_df[, xmax := 0.618]
  term_df[, ymin := y - 0.38]
  term_df[, ymax := y + 0.38]

  y_min <- min(c(term_df$y, gene_df$gene_y), na.rm = TRUE) - 1.0
  y_max <- max(c(term_df$y, gene_df$gene_y), na.rm = TRUE) + 1.0

  ratio_ticks <- pretty(c(0, gr_max), n = 4)
  ratio_ticks <- ratio_ticks[ratio_ticks >= 0 & ratio_ticks <= gr_max]
  axis_df <- data.table(
    ratio = ratio_ticks,
    x = dot_x_min + (ratio_ticks / gr_max) * dot_width,
    y = y_min + 0.15,
    label = formatC(ratio_ticks, format = "f", digits = 2)
  )

  axis_df[, y_grid_min := y_grid_min_value]
  axis_df[, y_grid_max := y_grid_max_value]

  term_df[, grid_xmin := dot_x_min - 0.02]
  term_df[, grid_xmax := dot_x_max + 0.02]

  p <- ggplot() +
    geom_rect(
      data = panel_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white",
      color = "black",
      linewidth = 0.45
    ) +
    geom_rect(
      data = panel_df,
      aes(xmin = strip_xmin, xmax = strip_xmax, ymin = ymin, ymax = ymax),
      fill = panel_df$strip_fill_col,
      color = "black",
      linewidth = 0.35
    ) +
    geom_text(
      data = panel_df,
      aes(x = strip_xmid, y = ymid, label = ontology_label),
      angle = 90,
      fontface = "bold",
      size = 5.0
    ) +
    geom_segment(
      data = axis_df,
      aes(x = x, xend = x, y = y_grid_min, yend = y_grid_max),
      color = "grey92",
      linewidth = 0.25
    ) +
    geom_segment(
      data = term_df,
      aes(x = grid_xmin, xend = grid_xmax, y = y, yend = y),
      color = "grey94",
      linewidth = 0.25
    )

  p <- p +
    geom_curve(
      data = link_df,
      aes(x = 0.06, y = gene_y, xend = 0.585, yend = term_y, color = I(link_color)),
      curvature = 0.22,
      alpha = GO_LINK_LAYER_ALPHA,
      linewidth = GO_LINK_WIDTH
    )

  p <- p +
    geom_rect(
      data = gene_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = I(gene_color)),
      color = NA
    ) +
    geom_text(
      data = gene_df,
      aes(x = 0.028, y = gene_y, label = gene),
      hjust = 1,
      size = 3.2,
      fontface = "bold"
    )

  p <- p +
    geom_rect(
      data = term_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = I(term_color)),
      color = NA
    )

  p <- p +
    geom_text(
      data = term_df,
      aes(x = 0.575, y = y, label = term_clean),
      hjust = 1,
      size = 4.0,
      fontface = "bold"
    ) +
    geom_point(
      data = term_df,
      aes(x = dot_x, y = y, size = Count, color = neg_log10_padj),
      alpha = 0.90
    ) +
    scale_size_continuous(name = "Count", range = c(3.5, 9.5)) +
    scale_color_gradient(
      name = "-log10(p.adjust)",
      low = "#4EDAC0",
      high = "red"
    ) +
    geom_segment(
      aes(x = dot_x_min, xend = dot_x_max, y = y_min + 0.35, yend = y_min + 0.35),
      color = "black",
      linewidth = 0.45
    ) +
    geom_segment(
      data = axis_df,
      aes(x = x, xend = x, y = y_min + 0.27, yend = y_min + 0.43),
      color = "black",
      linewidth = 0.35
    ) +
    geom_text(
      data = axis_df,
      aes(x = x, y = y_min + 0.05, label = label),
      size = 3.6
    ) +
    annotate(
      "text",
      x = 0.31,
      y = y_min - 0.45,
      label = "Gene-Term relationship",
      fontface = "bold",
      size = 5.3
    ) +
    annotate(
      "text",
      x = (dot_x_min + dot_x_max) / 2,
      y = y_min - 0.45,
      label = "GeneRatio",
      fontface = "bold",
      size = 5.0
    ) +
    labs(title = plot_title) +
    coord_cartesian(xlim = c(0, 1.08), ylim = c(y_min - 0.8, y_max), clip = "off") +
    theme_void(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      plot.margin = margin(8, 25, 25, 8)
    )

  list(plot = p, term_df = term_df, gene_df = gene_df, link_df = link_df)
}

stamp("生成 GO gene-term relationship 图。")

go_up_terms <- select_go_terms_for_relation(go_enrich_all, "UP")
go_down_terms <- select_go_terms_for_relation(go_enrich_all, "DOWN")

go_up_obj <- make_go_gene_term_plot(
  go_up_terms,
  direction_value = "UP",
  plot_title = "GO gene-term relationship: ideal-like upregulated"
)

go_down_obj <- make_go_gene_term_plot(
  go_down_terms,
  direction_value = "DOWN",
  plot_title = "GO gene-term relationship: lower-priority upregulated"
)

atomic_write_csv(as.data.frame(go_up_obj$term_df), go_up_plot_table_csv)
atomic_write_csv(as.data.frame(go_down_obj$term_df), go_down_plot_table_csv)
atomic_write_csv(as.data.frame(go_up_obj$link_df), go_up_link_table_csv)
atomic_write_csv(as.data.frame(go_down_obj$link_df), go_down_link_table_csv)

save_pdf_plot(go_up_obj$plot, go_up_pdf, width = GO_PDF_WIDTH, height = GO_PDF_HEIGHT)
save_pdf_plot(go_down_obj$plot, go_down_pdf, width = GO_PDF_WIDTH, height = GO_PDF_HEIGHT)


# ============================================================
# 9. method note / report / verification
# ============================================================

figure_index <- data.table(
  figure_id = c("GO_UP_gene_term", "GO_DOWN_gene_term"),
  title = c(
    "GO gene-term relationship: ideal-like upregulated",
    "GO gene-term relationship: lower-priority upregulated"
  ),
  pdf_path = c(go_up_pdf, go_down_pdf),
  table_path = c(go_up_plot_table_csv, go_down_plot_table_csv),
  pdf_size_bytes = c(file.info(go_up_pdf)$size, file.info(go_down_pdf)$size),
  plot_type = c("GO_gene_term_relationship_BP_CC_MF", "GO_gene_term_relationship_BP_CC_MF"),
  plot_engine = "ggplot2"
)

atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

method_lines <- c(
  "08D1 GO FINAL VERIFIED V2 method and claim-boundary note",
  "",
  "Method-ready wording:",
  paste0(
    "GO over-representation analysis was performed using clusterProfiler::enricher with MSigDB C5 GO gene sets obtained through the msigdbr R package. ",
    "GO gene sets were separated into BP, CC, and MF categories according to the GO term prefix. ",
    "Genes upregulated in the ideal-like DA/projection-high safety-low state and genes upregulated in the lower-priority/mixed state were analysed separately. ",
    "The enrichment universe was defined as all genes tested in the 08C differential expression analysis after objective expression filtering. ",
    "GO results were visualized as gene-term relationship plots linking selected representative genes to enriched GO terms, with gene-specific node colors, term-specific node colors, semi-transparent term-colored links, aligned GeneRatio bubbles, and BP/CC/MF category strips. ",
    "For visualization clarity, representative genes were selected per term based on differential-expression evidence, and each gene was allowed to connect to at most a limited number of GO terms; all displayed links represent true gene-term membership. The GO_UP panel should be interpreted as a coherent mitochondrial oxidative phosphorylation/electron-transport module because multiple enriched GO terms share overlapping respiratory-chain genes."
  ),
  "",
  "Strict parameters:",
  paste0("DEG list cutoff: FDR < ", DEG_PADJ_CUTOFF, " and |log2FC| >= ", DEG_LOG2FC_CUTOFF),
  paste0("Enrichment method: clusterProfiler::enricher; BH correction; minGSSize = ", MIN_GS_SIZE, "; maxGSSize = ", MAX_GS_SIZE),
  paste0("Universe: all ", length(universe_genes), " genes tested in 08C after objective expression filtering"),
  "",
  "Claim boundary:",
  "08D1 inherits the claim boundary of 08C. If 08C is a single-object cell-state contrast, then 08D1 supports pathway-level interpretation of that contrast, not sample-level pseudo-bulk validation or clinical/functional proof."
)

writeLines(method_lines, method_note_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

report_lines <- c(
  "08D1 GO FINAL VERIFIED V2 report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input:",
  INPUT_08C_DEG_ALL,
  "",
  "Gene lists:",
  paste0("Universe gene symbols: ", length(universe_genes)),
  paste0("UP gene symbols: ", length(up_genes)),
  paste0("DOWN gene symbols: ", length(down_genes)),
  "",
  "GO gene sets:",
  paste0("GO terms in universe: ", length(unique(go_term2gene$term))),
  paste0("GO TERM2GENE rows: ", nrow(go_term2gene)),
  "",
  "GO enrichment results:",
  paste0("GO tasks failed: ", nrow(go_task_status[status == "failed"])),
  paste0("All GO enrichment rows: ", nrow(go_enrich_all)),
  paste0("Significant GO enrichment rows: ", nrow(go_enrich_sig)),
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
  gene_list_summary_csv,
  gene_list_long_csv,
  go_term2gene_csv,
  go_enrichment_all_csv,
  go_enrichment_sig_csv,
  go_task_status_csv,
  go_up_plot_table_csv,
  go_down_plot_table_csv,
  go_up_link_table_csv,
  go_down_link_table_csv,
  figure_index_csv,
  method_note_txt,
  report_txt,
  session_info_txt,
  go_up_pdf,
  go_down_pdf
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
  stop("08D1 GO FINAL VERIFIED V2 未通过输出验证。")
}


# ============================================================
# 10. 完成
# ============================================================

cat("\n============================================================\n")
cat("08D1 GO FINAL VERIFIED V2 运行结束\n")
cat("============================================================\n\n")

cat("GO：gene-term relationship UP/DOWN\n")
cat("Universe genes：", length(universe_genes), "\n")
cat("UP genes：", length(up_genes), "\n")
cat("DOWN genes：", length(down_genes), "\n")
cat("GO terms in universe：", length(unique(go_term2gene$term)), "\n")
cat("GO tasks failed：", nrow(go_task_status[status == "failed"]), "\n")
cat("All GO enrichment rows：", nrow(go_enrich_all), "\n")
cat("Significant GO enrichment rows：", nrow(go_enrich_sig), "\n\n")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("主要 PDF 图：\n")
cat(go_up_pdf, "\n")
cat(go_down_pdf, "\n\n")

cat("✅ 08D1 GO FINAL VERIFIED V2 完成。\n")
