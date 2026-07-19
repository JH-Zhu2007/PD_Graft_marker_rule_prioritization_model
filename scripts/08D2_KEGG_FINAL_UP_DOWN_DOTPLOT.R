
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
KEGG_ALLOW_TOP_TERMS_IF_NO_SIGNIFICANT <- FALSE

MIN_GENES_FOR_ENRICHMENT <- 10
MIN_GS_SIZE <- 10
MAX_GS_SIZE <- 500

KEGG_TOP_N_PER_DIRECTION <- 15

KEGG_PDF_WIDTH <- 11.8
KEGG_PDF_HEIGHT <- 8.8

SEED <- 20260714

options(timeout = 60000)

cat("\n============================================================\n")
cat("08D2 KEGG FINAL：UP/DOWN dotplot\n")
cat("============================================================\n\n")

required_pkgs <- c(
  "data.table",
  "dplyr",
  "ggplot2",
  "clusterProfiler",
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
  library(clusterProfiler)
  library(msigdbr)
})

options(error = NULL)
options(bitmapType = "cairo")
set.seed(SEED)

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(tables_dir, "08D2_KEGG_FINAL")
out_figures_dir <- file.path(figures_dir, "08D2_KEGG_FINAL_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

gene_list_summary_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_gene_list_summary.csv")
gene_list_long_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_gene_lists_long.csv")
kegg_term2gene_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_KEGG_TERM2GENE_MSigDB_C2_CP_KEGG_no_MEDICUS.csv")

kegg_enrichment_all_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_all_KEGG_enrichment_results.csv")
kegg_enrichment_sig_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_significant_KEGG_enrichment_results.csv")
kegg_task_status_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_KEGG_enrichment_task_status.csv")
kegg_plot_table_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_KEGG_UP_DOWN_DOTPLOT_table.csv")

figure_index_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_figure_index.csv")
method_note_txt <- file.path(out_tables_dir, "08D2_KEGG_FINAL_method_and_claim_boundary_note.txt")
output_check_csv <- file.path(out_tables_dir, "08D2_KEGG_FINAL_output_verification.csv")
session_info_txt <- file.path(out_tables_dir, "08D2_KEGG_FINAL_sessionInfo.txt")
report_txt <- file.path(reports_dir, "08D2_KEGG_FINAL_report.txt")

kegg_pdf <- file.path(out_figures_dir, "08D2_KEGG_FINAL_KEGG_UP_DOWN_DOTPLOT.pdf")

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

clean_kegg_term <- function(x) {
  x <- as.character(x)
  x <- gsub("^KEGG_", "", x)
  x <- gsub("_", " ", x)
  x <- tolower(trimws(x))
  x
}

shorten_term <- function(x, max_chars = 78) {
  x <- as.character(x)
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 3), "..."), x)
}

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
      "。期刊标准下不继续 08D2。请先修复 08C。"
    )
  }
} else {
  chunks_failed <- NA_integer_
  warning("没有找到 08C chunk audit；08D2 会继续，但 final 解释时需要人工确认 08C 完整性。")
}

dataset_name <- if ("dataset" %in% names(deg)) unique(deg$dataset)[1] else "Dataset"
contrast_name <- if ("contrast_name" %in% names(deg)) unique(deg$contrast_name)[1] else "contrast"

stamp("Dataset：", dataset_name)
stamp("Contrast：", contrast_name)
stamp("All tested filtered genes rows：", nrow(deg))
stamp("08C chunks failed：", chunks_failed)

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
  stop("UP 和 DOWN genes 都少于 MIN_GENES_FOR_ENRICHMENT，无法做稳定 KEGG 富集。")
}

stamp("读取 MSigDB KEGG gene sets。")

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

msig_kegg <- get_msigdb_safe("C2", "CP:KEGG")

if (is.null(msig_kegg) || nrow(msig_kegg) == 0) {
  msig_c2 <- get_msigdb_safe("C2")
  if (!is.null(msig_c2) && nrow(msig_c2) > 0) {
    keep <- rep(FALSE, nrow(msig_c2))
    if ("gs_name" %in% names(msig_c2)) {
      keep <- keep | grepl("KEGG", msig_c2$gs_name, ignore.case = TRUE)
    }
    if ("gs_subcat" %in% names(msig_c2)) {
      keep <- keep | grepl("KEGG", msig_c2$gs_subcat, ignore.case = TRUE)
    }
    if ("gs_subcollection" %in% names(msig_c2)) {
      keep <- keep | grepl("KEGG", msig_c2$gs_subcollection, ignore.case = TRUE)
    }
    msig_kegg <- msig_c2[keep, ]
  }
}

if (is.null(msig_kegg) || nrow(msig_kegg) == 0) {
  stop("无法读取 MSigDB KEGG gene sets。")
}

if (!"gs_name" %in% names(msig_kegg) || !"gene_symbol" %in% names(msig_kegg)) {
  stop("MSigDB KEGG 数据缺少 gs_name 或 gene_symbol 列。")
}

kegg_term2gene <- as.data.table(msig_kegg)[, .(term = as.character(gs_name), gene = as.character(gene_symbol))]
kegg_term2gene <- unique(kegg_term2gene[!is.na(term) & !is.na(gene) & gene != ""])

kegg_term2gene <- kegg_term2gene[!grepl("MEDICUS", term, ignore.case = TRUE)]

kegg_term2gene <- kegg_term2gene[gene %in% universe_genes]

atomic_write_csv(as.data.frame(kegg_term2gene), kegg_term2gene_csv)

stamp("KEGG terms in universe：", length(unique(kegg_term2gene$term)))
stamp("KEGG TERM2GENE rows：", nrow(kegg_term2gene))

if (nrow(kegg_term2gene) < MIN_GENES_FOR_ENRICHMENT) {
  stop("KEGG TERM2GENE 太少，无法做 KEGG 富集。")
}

stamp("运行 clusterProfiler::enricher KEGG enrichment。")

run_kegg_enricher_task <- function(task_id, Direction, biological_direction, genes) {
  out <- tryCatch({
    obj <- clusterProfiler::enricher(
      gene = unique(genes),
      universe = unique(universe_genes),
      TERM2GENE = as.data.frame(kegg_term2gene[, .(term, gene)]),
      pAdjustMethod = "BH",
      pvalueCutoff = ENRICH_PVALUE_CUTOFF,
      qvalueCutoff = ENRICH_QVALUE_CUTOFF,
      minGSSize = MIN_GS_SIZE,
      maxGSSize = MAX_GS_SIZE
    )

    dt <- as.data.table(as.data.frame(obj))

    if (nrow(dt) > 0) {
      dt[, task_id := task_id]
      dt[, enrichment_type := "KEGG"]
      dt[, Direction := Direction]
      dt[, biological_direction := biological_direction]
      dt[, input_gene_symbol_n := length(unique(genes))]
      dt[, universe_gene_symbol_n := length(unique(universe_genes))]
      dt[, gene_set_source := "MSigDB_C2_CP_KEGG_no_MEDICUS"]
      dt[, enrichment_method := "clusterProfiler::enricher"]
      dt[, GeneRatioNum := parse_ratio(GeneRatio)]
      dt[, p.adjust := as.numeric(p.adjust)]
      dt[, Count := as.numeric(Count)]
      dt[, Description := as.character(Description)]
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

kegg_results <- list()

if (length(up_genes) >= MIN_GENES_FOR_ENRICHMENT) {
  stamp("KEGG enrichment task：KEGG_UP")
  kegg_results[["KEGG_UP"]] <- run_kegg_enricher_task("KEGG_UP", "UP", "UP_in_A_ideal_like", up_genes)
}

if (length(down_genes) >= MIN_GENES_FOR_ENRICHMENT) {
  stamp("KEGG enrichment task：KEGG_DOWN")
  kegg_results[["KEGG_DOWN"]] <- run_kegg_enricher_task("KEGG_DOWN", "DOWN", "DOWN_in_A_up_in_B_lower_priority", down_genes)
}

kegg_task_status <- rbindlist(lapply(kegg_results, function(x) {
  data.table(
    task_id = x$task_id,
    enrichment_type = "KEGG",
    Direction = x$Direction,
    biological_direction = x$biological_direction,
    status = x$status,
    message = x$message,
    n_terms = x$n_terms
  )
}), fill = TRUE)

kegg_enrich_all <- rbindlist(lapply(kegg_results, function(x) x$result), fill = TRUE)

if (nrow(kegg_enrich_all) > 0) {
  kegg_enrich_all <- kegg_enrich_all[order(Direction, p.adjust)]
}

kegg_enrich_sig <- if (nrow(kegg_enrich_all) > 0) {
  kegg_enrich_all[!is.na(p.adjust) & p.adjust <= PLOT_PADJ_CUTOFF]
} else {
  data.table()
}

atomic_write_csv(as.data.frame(kegg_task_status), kegg_task_status_csv)
atomic_write_csv(as.data.frame(kegg_enrich_all), kegg_enrichment_all_csv)
atomic_write_csv(as.data.frame(kegg_enrich_sig), kegg_enrichment_sig_csv)

stamp("KEGG tasks failed：", nrow(kegg_task_status[status == "failed"]))
stamp("KEGG enrichment rows all：", nrow(kegg_enrich_all))
stamp("KEGG enrichment rows significant：", nrow(kegg_enrich_sig))

theme_dot_original <- theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 19),
    axis.title = element_text(face = "plain", color = "black", size = 16),
    axis.text = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.0),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.25),
    legend.title = element_text(face = "plain", size = 14),
    legend.text = element_text(size = 11),
    legend.position = "right",
    plot.margin = margin(10, 25, 10, 55)
  )

make_kegg_subtable <- function(enrich_dt, direction, top_n, allow_fallback) {
  dt <- as.data.table(enrich_dt)
  dt <- dt[Direction == direction]
  dt <- dt[!is.na(p.adjust) & !is.na(GeneRatioNum) & !is.na(Count)]

  if (nrow(dt) == 0) return(data.table())

  dt_sig <- dt[p.adjust <= PLOT_PADJ_CUTOFF]
  display_mode <- "significant_FDR_le_0.05"

  if (nrow(dt_sig) == 0) {
    if (!isTRUE(allow_fallback)) return(data.table())
    dt_sig <- copy(dt)
    display_mode <- "fallback_top_ranked_not_significant"
  }

  dt_sig <- dt_sig[order(p.adjust, -Count)]
  dt_sig <- head(dt_sig, top_n)
  dt_sig[, Term := shorten_term(clean_kegg_term(Description), max_chars = 78)]
  dt_sig[, display_mode := display_mode]
  dt_sig[, significant_for_display := p.adjust <= PLOT_PADJ_CUTOFF]

  dt_sig
}

make_kegg_plot_table <- function(enrich_dt) {
  down <- make_kegg_subtable(enrich_dt, "DOWN", KEGG_TOP_N_PER_DIRECTION, KEGG_ALLOW_TOP_TERMS_IF_NO_SIGNIFICANT)
  up <- make_kegg_subtable(enrich_dt, "UP", KEGG_TOP_N_PER_DIRECTION, KEGG_ALLOW_TOP_TERMS_IF_NO_SIGNIFICANT)

  if (nrow(down) > 0) {
    down <- down[rev(seq_len(nrow(down)))]
    down[, y := seq_len(.N)]
  }

  if (nrow(up) > 0) {
    up <- up[rev(seq_len(nrow(up)))]
    up[, y := seq(from = nrow(down) + 3, length.out = .N)]
  }

  rbindlist(list(down, up), fill = TRUE)
}

make_kegg_dotplot_original <- function(plot_df, title) {
  if (nrow(plot_df) == 0) stop("KEGG plot table 为空，不画白图。")

  x_max <- max(plot_df$GeneRatioNum, na.rm = TRUE)
  if (!is.finite(x_max) || x_max <= 0) x_max <- 0.01

  x_upper <- x_max * 1.12

  min_point_x <- min(plot_df$GeneRatioNum[plot_df$GeneRatioNum > 0], na.rm = TRUE)
  if (!is.finite(min_point_x) || min_point_x <= 0) min_point_x <- x_upper * 0.08

  line_x <- max(x_upper * 0.012, min_point_x * 0.35)
  label_x <- line_x

  x_breaks <- pretty(c(0, x_upper), n = 4)
  x_breaks <- x_breaks[x_breaks >= 0 & x_breaks <= x_upper]

  if (any(plot_df$display_mode == "fallback_top_ranked_not_significant", na.rm = TRUE)) {
    title <- paste0(title, " (top-ranked terms; none FDR <= 0.05)")
  }

  p <- ggplot(plot_df, aes(x = GeneRatioNum, y = y)) +
    geom_point(aes(size = Count, color = p.adjust), alpha = 0.95) +
    scale_color_gradient(low = "red", high = "blue", name = "p.adj") +
    scale_size_continuous(name = "Count", range = c(3.2, 9.5)) +
    scale_y_continuous(
      breaks = plot_df$y,
      labels = plot_df$Term,
      expand = expansion(add = c(0.8, 1.2))
    ) +
    scale_x_continuous(
      limits = c(0, x_upper),
      breaks = x_breaks,
      expand = expansion(mult = c(0.02, 0.06))
    ) +
    labs(title = title, x = "GeneRatio", y = "Pathway name") +
    coord_cartesian(clip = "off") +
    theme_dot_original

  up_df <- plot_df[Direction == "UP"]
  down_df <- plot_df[Direction == "DOWN"]

  if (nrow(up_df) > 0) {
    p <- p +
      annotate("segment", x = line_x, xend = line_x, y = min(up_df$y), yend = max(up_df$y), color = "red", linewidth = 1.1) +
      annotate("text", x = label_x, y = max(up_df$y) + 0.85, label = "Up-regulated", color = "red", size = 4.0, hjust = 0)
  }

  if (nrow(down_df) > 0) {
    p <- p +
      annotate("segment", x = line_x, xend = line_x, y = min(down_df$y), yend = max(down_df$y), color = "darkgreen", linewidth = 1.1) +
      annotate("text", x = label_x, y = max(down_df$y) + 0.85, label = "Down-regulated", color = "darkgreen", size = 4.0, hjust = 0)
  }

  p
}

stamp("生成 KEGG UP/DOWN dotplot。")

kegg_plot_table <- make_kegg_plot_table(kegg_enrich_all)

if (nrow(kegg_plot_table) == 0) {
  stop("KEGG plot table 为空，不生成白图。")
}

atomic_write_csv(as.data.frame(kegg_plot_table), kegg_plot_table_csv)

kegg_plot <- make_kegg_dotplot_original(
  kegg_plot_table,
  title = "KEGG: ideal-like vs lower-priority"
)

save_pdf_plot(kegg_plot, kegg_pdf, width = KEGG_PDF_WIDTH, height = KEGG_PDF_HEIGHT)

figure_index <- data.table(
  figure_id = "KEGG_UP_DOWN_dotplot",
  title = "KEGG: ideal-like vs lower-priority",
  pdf_path = kegg_pdf,
  table_path = kegg_plot_table_csv,
  pdf_size_bytes = file.info(kegg_pdf)$size,
  plot_type = "KEGG_original_UP_DOWN_dotplot",
  plot_engine = "ggplot2"
)

atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

method_lines <- c(
  "08D2 KEGG FINAL method and claim-boundary note",
  "",
  "Method-ready wording:",
  paste0(
    "KEGG over-representation analysis was performed using clusterProfiler::enricher with MSigDB C2 CP:KEGG gene sets obtained through the msigdbr R package after excluding MEDICUS-specific entries. ",
    "Genes upregulated in the ideal-like DA/projection-high safety-low state and genes upregulated in the lower-priority/mixed state were analysed separately. ",
    "The enrichment universe was defined as all genes tested in the 08C differential expression analysis after objective expression filtering. ",
    "KEGG results were visualized using the original UP/DOWN dotplot layout."
  ),
  "",
  "Strict parameters:",
  paste0("DEG list cutoff: FDR < ", DEG_PADJ_CUTOFF, " and |log2FC| >= ", DEG_LOG2FC_CUTOFF),
  paste0("Enrichment method: clusterProfiler::enricher; BH correction; minGSSize = ", MIN_GS_SIZE, "; maxGSSize = ", MAX_GS_SIZE),
  paste0("Universe: all ", length(universe_genes), " genes tested in 08C after objective expression filtering"),
  "",
  "Claim boundary:",
  "08D2 inherits the claim boundary of 08C. If 08C is a single-object cell-state contrast, then 08D2 supports pathway-level interpretation of that contrast, not sample-level pseudo-bulk validation or clinical/functional proof."
)

writeLines(method_lines, method_note_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

report_lines <- c(
  "08D2 KEGG FINAL report",
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
  "KEGG gene sets:",
  paste0("KEGG terms in universe: ", length(unique(kegg_term2gene$term))),
  paste0("KEGG TERM2GENE rows: ", nrow(kegg_term2gene)),
  "",
  "KEGG enrichment results:",
  paste0("KEGG tasks failed: ", nrow(kegg_task_status[status == "failed"])),
  paste0("All KEGG enrichment rows: ", nrow(kegg_enrich_all)),
  paste0("Significant KEGG enrichment rows: ", nrow(kegg_enrich_sig)),
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
  kegg_term2gene_csv,
  kegg_enrichment_all_csv,
  kegg_enrichment_sig_csv,
  kegg_task_status_csv,
  kegg_plot_table_csv,
  figure_index_csv,
  method_note_txt,
  report_txt,
  session_info_txt,
  kegg_pdf
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
  stop("08D2 KEGG FINAL 未通过输出验证。")
}

cat("\n============================================================\n")
cat("08D2 KEGG FINAL 运行结束\n")
cat("============================================================\n\n")

cat("KEGG：UP/DOWN dotplot\n")
cat("Universe genes：", length(universe_genes), "\n")
cat("UP genes：", length(up_genes), "\n")
cat("DOWN genes：", length(down_genes), "\n")
cat("KEGG terms in universe：", length(unique(kegg_term2gene$term)), "\n")
cat("KEGG tasks failed：", nrow(kegg_task_status[status == "failed"]), "\n")
cat("All KEGG enrichment rows：", nrow(kegg_enrich_all), "\n")
cat("Significant KEGG enrichment rows：", nrow(kegg_enrich_sig), "\n\n")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("主要 PDF 图：\n")
cat(kegg_pdf, "\n\n")

cat("✅ 08D2 KEGG FINAL 完成。\n")
