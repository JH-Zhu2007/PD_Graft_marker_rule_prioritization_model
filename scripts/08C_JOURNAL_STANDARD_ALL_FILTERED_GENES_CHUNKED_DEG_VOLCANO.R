# ============================================================
# 08C_JOURNAL_STANDARD_ALL_FILTERED_GENES_CHUNKED_DEG_VOLCANO.R
# ============================================================
# 目的：
#   用更接近投稿标准的方式重做 08C gene-level DEG volcano。
#
# 核心原则：
#   1. 不使用任意 top-N gene cap，例如不再只测试 10000 genes。
#   2. 使用客观表达过滤：保留在 A 组或 B 组至少 MIN_PCT 细胞中表达的 genes。
#   3. 对所有通过过滤的 genes 做统计检验。
#   4. p.adjust 在所有被测试 genes 上统一进行。
#   5. 使用 chunk 分批计算，降低 Windows/RStudio 内存崩溃风险。
#   6. 输出完整 DEG table、significant DEG table、chunk audit、replicate audit、publication-style volcano。
#
# 重要学术边界：
#   - 如果当前对象没有 biological replicates / sample-level replicates，
#     本脚本的 DEG 只能作为 single-object cell-state gene-level exploratory validation。
#   - 真正最强的期刊级样本层面差异结论，需要后续 08E pseudo-bulk replicate-level validation。
#   - 本脚本会自动检查 meta.data 中是否存在 sample/replicate/donor/animal 等列，
#     并输出 replicate audit，防止把 cell-level contrast 误写成 sample-level validation。
#
# 成功标志：
#   ✅ 08C JOURNAL-STANDARD all-filtered-genes chunked DEG volcano 完成。
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"

TARGET_DATASET <- "GSE132758"
TARGET_OBJECT_ID_CONTAINS <- "rat45_1a"

CLASS_A <- c("ideal_DA_projection_high_safety_low")
CLASS_B <- c("lower_priority_or_mixed")
CONTRAST_NAME <- "ideal_vs_lower_priority"

# 投稿友好的客观表达过滤。
# 解释：只测试在任一比较组中至少 5% 细胞表达的基因。
MIN_PCT <- 0.05

# DEG table 判定阈值。
TABLE_LOG2FC_CUTOFF <- 0.25
TABLE_PADJ_CUTOFF <- 0.05

# Volcano 展示阈值。
# 按用户原 bulk volcano_nature 风格，图上用 |log2FC| >= 1 标 UP/DOWN。
VOLCANO_LOG2FC_CUTOFF <- 1
VOLCANO_PADJ_CUTOFF <- 0.05

# chunk 大小。越小越稳，越大越快。
GENE_CHUNK_SIZE <- 1000

# 只影响图像展示，不影响统计或 DEG table。
# single-cell 稀疏表达容易出现极端 logFC；展示 cap 必须在 caption 中说明。
X_CAP <- 8
Y_CAP <- 45

TOP_LABEL_GENES <- 15

# 用户原始 volcano_nature 风格点大小：
POINT_SIZE <- 1.5
POINT_ALPHA <- 0.75

SEED <- 20260714


# ============================================================
# 1. 加载包
# ============================================================

cat("\n============================================================\n")
cat("08C JOURNAL-STANDARD：all-filtered-genes chunked DEG volcano\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table", "Matrix", "Seurat", "ggplot2", "ggrepel")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 08C journal-standard script。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(Seurat)
  library(ggplot2)
  library(ggrepel)
})


# ============================================================
# 2. 路径
# ============================================================

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_manifest <- file.path(tables_dir, "04D_annotations", "04D_annotated_object_manifest.csv")
input_05B <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_DA_projection_vs_safety_contrast_groups.csv")

out_tables_dir <- file.path(tables_dir, "08C_JOURNAL_STANDARD_all_filtered_genes_chunked")
out_figures_dir <- file.path(figures_dir, "08C_JOURNAL_STANDARD_all_filtered_genes_chunked_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

deg_all_csv <- file.path(out_tables_dir, "08C_JOURNAL_all_filtered_genes_DEG_table.csv")
deg_sig_csv <- file.path(out_tables_dir, "08C_JOURNAL_significant_DEG_table.csv")
plot_table_csv <- file.path(out_tables_dir, "08C_JOURNAL_volcano_plot_table.csv")
label_table_csv <- file.path(out_tables_dir, "08C_JOURNAL_labeled_genes.csv")
summary_csv <- file.path(out_tables_dir, "08C_JOURNAL_summary.csv")
chunk_audit_csv <- file.path(out_tables_dir, "08C_JOURNAL_chunk_audit.csv")
replicate_audit_csv <- file.path(out_tables_dir, "08C_JOURNAL_replicate_level_audit.csv")
figure_index_csv <- file.path(out_tables_dir, "08C_JOURNAL_figure_index.csv")
method_note_txt <- file.path(out_tables_dir, "08C_JOURNAL_method_and_claim_boundary_note.txt")
report_txt <- file.path(reports_dir, "08C_JOURNAL_STANDARD_all_filtered_genes_chunked_report.txt")


# ============================================================
# 3. 工具函数
# ============================================================

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

choose_group_col <- function(meta_cols) {
  candidates <- c(
    "annotation_04D_group_id",
    "group_id",
    "cluster_id",
    "seurat_clusters",
    "RNA_snn_res.0.5",
    "RNA_snn_res.0.3",
    "RNA_snn_res.0.8"
  )

  hit <- intersect(candidates, meta_cols)
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

detect_replicate_columns <- function(meta_dt) {
  # 这些列名只是候选，不自动等同 biological replicate。
  candidates <- c(
    "sample_id", "sample", "Sample", "orig.ident", "orig_ident",
    "donor", "Donor", "patient", "Patient",
    "animal", "Animal", "rat", "mouse",
    "replicate", "replicate_id", "bio_rep", "biological_replicate",
    "batch", "Batch", "GSM", "gsm", "library", "Library"
  )

  hit <- intersect(candidates, colnames(meta_dt))

  # fuzzy 匹配
  nm <- colnames(meta_dt)
  fuzzy <- nm[grepl("sample|donor|patient|animal|replicate|batch|orig|gsm|library", nm, ignore.case = TRUE)]
  unique(c(hit, fuzzy))
}

safe_padj <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  out <- rep(NA_real_, length(p))
  ok <- !is.na(p)
  out[ok] <- p.adjust(p[ok], method = "BH")
  out
}

sparse_row_vars <- function(m, row_means = NULL) {
  n <- ncol(m)
  if (is.null(row_means)) row_means <- Matrix::rowMeans(m)

  if (inherits(m, "sparseMatrix")) {
    m2 <- m
    m2@x <- m2@x ^ 2
    row_sq_means <- Matrix::rowMeans(m2)
    rm(m2)
    gc(verbose = FALSE)
  } else {
    row_sq_means <- rowMeans(m ^ 2)
  }

  vars <- (row_sq_means - row_means ^ 2) * n / max(n - 1, 1)
  vars[!is.finite(vars)] <- 0
  pmax(vars, 0)
}

get_expr_matrix <- function(obj) {
  assay_use <- DefaultAssay(obj)
  assay_obj <- obj[[assay_use]]

  if ("layers" %in% slotNames(assay_obj)) {
    layer_names <- names(assay_obj@layers)

    if ("data" %in% layer_names) {
      mat <- SeuratObject::LayerData(obj, assay = assay_use, layer = "data")
      return(list(mat = mat, layer = "data", is_counts = FALSE, assay = assay_use))
    }

    if ("counts" %in% layer_names) {
      mat <- SeuratObject::LayerData(obj, assay = assay_use, layer = "counts")
      return(list(mat = mat, layer = "counts", is_counts = TRUE, assay = assay_use))
    }

    mat <- SeuratObject::LayerData(obj, assay = assay_use, layer = layer_names[[1]])
    return(list(
      mat = mat,
      layer = layer_names[[1]],
      is_counts = grepl("count", layer_names[[1]], ignore.case = TRUE),
      assay = assay_use
    ))
  }

  mat_data <- tryCatch(GetAssayData(obj, assay = assay_use, slot = "data"), error = function(e) NULL)
  if (!is.null(mat_data) && nrow(mat_data) > 0 && ncol(mat_data) > 0) {
    return(list(mat = mat_data, layer = "data", is_counts = FALSE, assay = assay_use))
  }

  mat_counts <- GetAssayData(obj, assay = assay_use, slot = "counts")
  list(mat = mat_counts, layer = "counts", is_counts = TRUE, assay = assay_use)
}

save_plot_pdf_base <- function(p, path, width = 9, height = 7) {
  if (file.exists(path)) file.remove(path)

  ok <- FALSE
  msg <- NA_character_

  tryCatch({
    grDevices::pdf(file = path, width = width, height = height, useDingbats = FALSE, paper = "special")
    print(p)
    grDevices::dev.off()

    ok <- file.exists(path) && file.info(path)$size > 1000
    if (!ok) msg <- "PDF missing or too small"
  }, error = function(e) {
    msg <<- conditionMessage(e)
    tryCatch(grDevices::dev.off(), error = function(e2) NULL)
  })

  list(success = ok, message = msg)
}


# ============================================================
# 4. 选择对象和细胞
# ============================================================

set.seed(SEED)

stamp("读取 manifest 和 05B candidate class table。")

manifest <- fread(input_manifest, data.table = TRUE)
contrast <- fread(input_05B, data.table = TRUE)

if (!all(c("dataset", "object_id", "annotated_rds") %in% names(manifest))) {
  stop("04D manifest 缺少 dataset/object_id/annotated_rds。")
}

if (!all(c("dataset", "object_id", "group_id", "safety_contrast_class_05B") %in% names(contrast))) {
  stop("05B contrast table 缺少 dataset/object_id/group_id/safety_contrast_class_05B。")
}

cand <- manifest[
  dataset == TARGET_DATASET &
    grepl(TARGET_OBJECT_ID_CONTAINS, object_id, fixed = TRUE) &
    file.exists(annotated_rds)
]

if (nrow(cand) == 0) {
  cand <- manifest[dataset == TARGET_DATASET & file.exists(annotated_rds)]
}

if (nrow(cand) == 0) {
  stop("找不到可用对象：", TARGET_DATASET, " ; contains=", TARGET_OBJECT_ID_CONTAINS)
}

target <- cand[1]

stamp("目标对象：", target$dataset, " :: ", target$object_id)
stamp("RDS：", target$annotated_rds)

stamp("读取单个 Seurat RDS。")
obj <- readRDS(target$annotated_rds)

meta <- as.data.table(obj@meta.data)
meta[, cell := rownames(obj@meta.data)]

group_col <- choose_group_col(names(meta))
if (is.na(group_col)) stop("对象 meta.data 找不到 group column。")

sub_05B <- contrast[dataset == target$dataset & object_id == target$object_id]
if (nrow(sub_05B) == 0) {
  stop("05B 里面找不到该对象 class：", target$dataset, " :: ", target$object_id)
}

sub_05B[, group_id := as.character(group_id)]

groups_A <- unique(sub_05B[safety_contrast_class_05B %in% CLASS_A, group_id])
groups_B <- unique(sub_05B[safety_contrast_class_05B %in% CLASS_B, group_id])

if (length(groups_A) == 0 || length(groups_B) == 0) {
  stop("该对象没有足够 group 做 contrast。A groups=", length(groups_A), "; B groups=", length(groups_B))
}

meta[, group_for_08C := as.character(get(group_col))]
meta[, comparison_class_08C := "other"]
meta[group_for_08C %in% groups_A, comparison_class_08C := "A"]
meta[group_for_08C %in% groups_B, comparison_class_08C := "B"]

cells_A <- meta[comparison_class_08C == "A", cell]
cells_B <- meta[comparison_class_08C == "B", cell]

if (length(cells_A) < 20 || length(cells_B) < 20) {
  stop("细胞数不足。A=", length(cells_A), "; B=", length(cells_B))
}

stamp("meta group column：", group_col)
stamp("groups A：", paste(groups_A, collapse = ";"))
stamp("groups B：", paste(groups_B, collapse = ";"))
stamp("cells A：", length(cells_A))
stamp("cells B：", length(cells_B))


# ============================================================
# 5. replicate-level audit
# ============================================================

stamp("检查 biological replicate / sample-level metadata。")

rep_cols <- detect_replicate_columns(meta)

rep_audit_list <- list()

if (length(rep_cols) == 0) {
  rep_audit <- data.table(
    candidate_replicate_column = NA_character_,
    unique_values_total = NA_integer_,
    unique_values_A = NA_integer_,
    unique_values_B = NA_integer_,
    usable_for_pseudobulk = FALSE,
    note = "No candidate sample/replicate/donor/animal metadata column detected in this object."
  )
} else {
  for (cc in rep_cols) {
    vals_all <- as.character(meta[[cc]])
    vals_A <- as.character(meta[comparison_class_08C == "A", get(cc)])
    vals_B <- as.character(meta[comparison_class_08C == "B", get(cc)])

    u_all <- unique(vals_all[!is.na(vals_all) & vals_all != ""])
    u_A <- unique(vals_A[!is.na(vals_A) & vals_A != ""])
    u_B <- unique(vals_B[!is.na(vals_B) & vals_B != ""])

    # 只有 A/B 各至少 2 个不同 biological units，才可能用于 pseudo-bulk。
    usable <- length(u_A) >= 2 && length(u_B) >= 2

    rep_audit_list[[length(rep_audit_list) + 1L]] <- data.table(
      candidate_replicate_column = cc,
      unique_values_total = length(u_all),
      unique_values_A = length(u_A),
      unique_values_B = length(u_B),
      usable_for_pseudobulk = usable,
      note = ifelse(
        usable,
        "Potentially usable for downstream pseudo-bulk validation, but must confirm it represents biological replicates.",
        "Not sufficient for pseudo-bulk validation of this contrast."
      )
    )
  }

  rep_audit <- rbindlist(rep_audit_list, fill = TRUE)
}

atomic_write_csv(as.data.frame(rep_audit), replicate_audit_csv)

has_potential_pseudobulk <- any(rep_audit$usable_for_pseudobulk, na.rm = TRUE)

if (has_potential_pseudobulk) {
  stamp("检测到可能可用于 pseudo-bulk 的 replicate column；后续建议做 08E pseudo-bulk validation。")
} else {
  stamp("未检测到足够 biological replicate metadata；08C 只能作为 single-object gene-level exploratory validation。")
}


# ============================================================
# 6. 提取表达矩阵并 chunk 计算所有 filtered genes
# ============================================================

stamp("提取表达矩阵。")

expr <- get_expr_matrix(obj)
mat <- expr$mat

if (is.null(rownames(mat)) || is.null(colnames(mat))) {
  stop("表达矩阵缺少 rownames/colnames。")
}

cells_A <- intersect(cells_A, colnames(mat))
cells_B <- intersect(cells_B, colnames(mat))

if (length(cells_A) < 20 || length(cells_B) < 20) {
  stop("和 matrix 取交集后细胞数不足。")
}

genes_all <- rownames(mat)
n_genes_total <- length(genes_all)
nA <- length(cells_A)
nB <- length(cells_B)

stamp("matrix dims：", nrow(mat), " genes x ", ncol(mat), " cells")
stamp("assay/layer：", expr$assay, " / ", expr$layer)
stamp("有效 matrix cells A：", nA)
stamp("有效 matrix cells B：", nB)
stamp("总 genes：", n_genes_total)
stamp("chunk size：", GENE_CHUNK_SIZE)

chunks <- split(genes_all, ceiling(seq_along(genes_all) / GENE_CHUNK_SIZE))

deg_records <- list()
chunk_records <- list()

for (i in seq_along(chunks)) {
  genes_chunk <- chunks[[i]]

  stamp("chunk ", i, " / ", length(chunks), "；genes=", length(genes_chunk))

  res_chunk <- tryCatch({
    mat_a <- mat[genes_chunk, cells_A, drop = FALSE]
    mat_b <- mat[genes_chunk, cells_B, drop = FALSE]

    if (expr$is_counts) {
      mat_a_work <- log1p(mat_a)
      mat_b_work <- log1p(mat_b)
    } else {
      mat_a_work <- mat_a
      mat_b_work <- mat_b
    }

    mean_a_log <- Matrix::rowMeans(mat_a_work)
    mean_b_log <- Matrix::rowMeans(mat_b_work)

    pct_a <- Matrix::rowSums(mat_a_work > 0) / nA
    pct_b <- Matrix::rowSums(mat_b_work > 0) / nB

    mean_a_raw <- expm1(mean_a_log)
    mean_b_raw <- expm1(mean_b_log)

    avg_log2FC <- log2((mean_a_raw + 1e-6) / (mean_b_raw + 1e-6))

    dt0 <- data.table(
      gene = rownames(mat_a_work),
      mean_expr_A = as.numeric(mean_a_log),
      mean_expr_B = as.numeric(mean_b_log),
      pct_A = as.numeric(pct_a),
      pct_B = as.numeric(pct_b),
      avg_log2FC = as.numeric(avg_log2FC)
    )

    dt0[, max_pct := pmax(pct_A, pct_B)]
    dt <- dt0[max_pct >= MIN_PCT & !is.na(avg_log2FC)]

    if (nrow(dt) == 0) {
      rm(mat_a, mat_b, mat_a_work, mat_b_work, dt0)
      gc(verbose = FALSE)

      list(
        deg = data.table(),
        audit = data.table(
          chunk_id = i,
          genes_in_chunk = length(genes_chunk),
          genes_tested_after_min_pct = 0,
          status = "no_genes_after_min_pct"
        )
      )
    } else {
      genes_test <- dt$gene

      a_test <- mat_a_work[genes_test, , drop = FALSE]
      b_test <- mat_b_work[genes_test, , drop = FALSE]

      mean_a <- Matrix::rowMeans(a_test)
      mean_b <- Matrix::rowMeans(b_test)

      var_a <- sparse_row_vars(a_test, mean_a)
      var_b <- sparse_row_vars(b_test, mean_b)

      se <- sqrt(var_a / nA + var_b / nB)
      t_stat <- (mean_a - mean_b) / se
      t_stat[!is.finite(t_stat)] <- 0

      num <- (var_a / nA + var_b / nB) ^ 2
      den <- ((var_a / nA) ^ 2) / max(nA - 1, 1) + ((var_b / nB) ^ 2) / max(nB - 1, 1)
      df <- num / den
      df[!is.finite(df) | df < 1] <- 1

      pvals <- 2 * stats::pt(-abs(t_stat), df = df)
      pvals[!is.finite(pvals)] <- 1

      dt <- dt[match(genes_test, gene)]
      dt[, p_val := pvals]
      dt[, t_stat := as.numeric(t_stat)]
      dt[, df_welch := as.numeric(df)]

      rm(mat_a, mat_b, mat_a_work, mat_b_work, a_test, b_test, dt0)
      gc(verbose = FALSE)

      list(
        deg = dt,
        audit = data.table(
          chunk_id = i,
          genes_in_chunk = length(genes_chunk),
          genes_tested_after_min_pct = nrow(dt),
          status = "ok"
        )
      )
    }
  }, error = function(e) {
    gc(verbose = FALSE)
    list(
      deg = data.table(),
      audit = data.table(
        chunk_id = i,
        genes_in_chunk = length(genes_chunk),
        genes_tested_after_min_pct = 0,
        status = paste0("failed: ", conditionMessage(e))
      )
    )
  })

  deg_records[[length(deg_records) + 1L]] <- res_chunk$deg
  chunk_records[[length(chunk_records) + 1L]] <- res_chunk$audit

  # 实时写 chunk audit，崩了也能知道崩在哪。
  atomic_write_csv(as.data.frame(rbindlist(chunk_records, fill = TRUE)), chunk_audit_csv)
}

deg <- rbindlist(deg_records, fill = TRUE)
chunk_audit <- rbindlist(chunk_records, fill = TRUE)

if (nrow(deg) == 0) {
  stop("所有 chunk 完成后没有任何 gene 通过 MIN_PCT 过滤。")
}

# 关键投稿标准：BH correction 在所有被测试 filtered genes 上统一进行。
deg[, p_val_adj := safe_padj(p_val)]
deg[is.na(p_val_adj), p_val_adj := 1]
deg[, neg_log10_padj := -log10(pmax(p_val_adj, 1e-300))]

deg[, DEG_call_table := fifelse(
  p_val_adj < TABLE_PADJ_CUTOFF & avg_log2FC >= TABLE_LOG2FC_CUTOFF,
  "Up_in_A",
  fifelse(
    p_val_adj < TABLE_PADJ_CUTOFF & avg_log2FC <= -TABLE_LOG2FC_CUTOFF,
    "Up_in_B",
    "Not_significant"
  )
)]

deg[, volcano_group := "NS"]
deg[p_val_adj < VOLCANO_PADJ_CUTOFF & avg_log2FC >= VOLCANO_LOG2FC_CUTOFF, volcano_group := "UP"]
deg[p_val_adj < VOLCANO_PADJ_CUTOFF & avg_log2FC <= -VOLCANO_LOG2FC_CUTOFF, volcano_group := "DOWN"]
deg[, volcano_group := factor(volcano_group, levels = c("DOWN", "NS", "UP"))]

deg[, dataset := target$dataset]
deg[, object_id := target$object_id]
deg[, contrast_name := CONTRAST_NAME]
deg[, class_A := paste(CLASS_A, collapse = ";")]
deg[, class_B := paste(CLASS_B, collapse = ";")]
deg[, n_cells_A := nA]
deg[, n_cells_B := nB]
deg[, assay := expr$assay]
deg[, layer := expr$layer]
deg[, min_pct_filter := MIN_PCT]
deg[, replicate_metadata_available_for_pseudobulk := has_potential_pseudobulk]
deg[, test_method := "all_filtered_genes_chunked_matrix_based_Welch_style_single_object_exploratory"]

setcolorder(
  deg,
  c(
    "dataset", "object_id", "contrast_name",
    "gene", "class_A", "class_B", "n_cells_A", "n_cells_B",
    "avg_log2FC", "p_val", "p_val_adj", "neg_log10_padj",
    "mean_expr_A", "mean_expr_B", "pct_A", "pct_B", "max_pct",
    "t_stat", "df_welch",
    "DEG_call_table", "volcano_group",
    "assay", "layer", "min_pct_filter",
    "replicate_metadata_available_for_pseudobulk",
    "test_method"
  )
)

deg <- deg[order(p_val_adj, -abs(avg_log2FC))]
sig <- deg[DEG_call_table != "Not_significant"]

atomic_write_csv(as.data.frame(deg), deg_all_csv)
atomic_write_csv(as.data.frame(sig), deg_sig_csv)
atomic_write_csv(as.data.frame(chunk_audit), chunk_audit_csv)


# ============================================================
# 7. volcano plot table
# ============================================================

stamp("生成 volcano plot table。")

plot_dt <- copy(deg)
plot_dt[, log2FoldChange := avg_log2FC]
plot_dt[, padj := p_val_adj]
plot_dt[, mlog10 := neg_log10_padj]

plot_dt[, x_capped := pmax(pmin(log2FoldChange, X_CAP), -X_CAP)]
plot_dt[, y_capped := pmin(mlog10, Y_CAP)]

label_pool <- plot_dt[volcano_group != "NS"]
if (nrow(label_pool) == 0) label_pool <- copy(plot_dt)
label_pool <- label_pool[order(padj, -abs(log2FoldChange))]
label_genes <- head(unique(label_pool$gene), TOP_LABEL_GENES)

plot_dt[, label := NA_character_]
plot_dt[gene %in% label_genes, label := gene]
label_dt <- plot_dt[!is.na(label)]

atomic_write_csv(as.data.frame(plot_dt), plot_table_csv)
atomic_write_csv(as.data.frame(label_dt), label_table_csv)


# ============================================================
# 8. publication-style volcano
# ============================================================

stamp("输出 volcano PDF。")

DEG_COLORS <- c(
  "UP" = "#E64B35",
  "DOWN" = "#4DBBD5",
  "NS" = "grey75"
)

plot_title <- paste0(target$dataset, " | ", CONTRAST_NAME)
plot_subtitle <- paste0(
  "A: ideal-like DA/projection-high safety-low vs B: lower-priority/mixed | all filtered genes: ",
  nrow(plot_dt)
)

make_volcano <- function(df, labels, capped = TRUE) {
  df_plot <- copy(df)
  lab_plot <- copy(labels)

  if (capped) {
    df_plot[, x_plot := x_capped]
    df_plot[, y_plot := y_capped]
    lab_plot[, x_plot := x_capped]
    lab_plot[, y_plot := y_capped]

    x_limits <- c(-X_CAP, X_CAP)
    y_limits <- c(0, Y_CAP)
    x_breaks <- seq(-X_CAP, X_CAP, by = 2)
    caption <- paste0(
      "Display capped at |log2FC| <= ", X_CAP,
      " and -log10(padj) <= ", Y_CAP,
      "; testing included all genes with MIN_PCT >= ", MIN_PCT,
      "."
    )
  } else {
    df_plot[, x_plot := log2FoldChange]
    df_plot[, y_plot := mlog10]
    lab_plot[, x_plot := log2FoldChange]
    lab_plot[, y_plot := mlog10]

    x_limits <- NULL
    y_limits <- NULL
    x_breaks <- pretty(df_plot$x_plot, n = 6)
    caption <- "Full-axis audit plot; no display cap."
  }

  ggplot(df_plot, aes(x = x_plot, y = y_plot)) +
    geom_point(aes(color = volcano_group), size = POINT_SIZE, alpha = POINT_ALPHA) +
    scale_color_manual(
      values = DEG_COLORS,
      breaks = c("UP", "DOWN", "NS"),
      name = NULL
    ) +
    geom_vline(
      xintercept = c(-VOLCANO_LOG2FC_CUTOFF, VOLCANO_LOG2FC_CUTOFF),
      linetype = "dashed",
      linewidth = 0.60,
      color = "grey40"
    ) +
    geom_hline(
      yintercept = -log10(VOLCANO_PADJ_CUTOFF),
      linetype = "dashed",
      linewidth = 0.60,
      color = "grey40"
    ) +
    geom_text_repel(
      data = lab_plot,
      aes(x = x_plot, y = y_plot, label = label),
      inherit.aes = FALSE,
      size = 3,
      max.overlaps = Inf,
      box.padding = 0.50,
      point.padding = 0.25,
      segment.color = "grey50",
      min.segment.length = 0
    ) +
    scale_x_continuous(limits = x_limits, breaks = x_breaks) +
    coord_cartesian(ylim = y_limits, clip = "off") +
    theme_classic(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.line = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.minor = element_line(color = "grey95", linewidth = 0.3),
      legend.title = element_blank(),
      legend.position = "right",
      legend.text = element_text(color = "black"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 9.5),
      plot.caption = element_text(size = 8, color = "grey35", hjust = 0.5),
      axis.title = element_text(face = "bold", color = "black"),
      axis.text = element_text(color = "black"),
      plot.margin = margin(12, 34, 14, 12)
    ) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "log2 Fold Change",
      y = "-log10 adjusted p-value",
      caption = caption
    )
}

pdf_capped <- file.path(out_figures_dir, "08C_JOURNAL_all_filtered_genes_volcano_capped_publication.pdf")
pdf_full <- file.path(out_figures_dir, "08C_JOURNAL_all_filtered_genes_volcano_full_axis_audit.pdf")

p_capped <- make_volcano(plot_dt, label_dt, capped = TRUE)
p_full <- make_volcano(plot_dt, label_dt, capped = FALSE)

res_capped <- save_plot_pdf_base(p_capped, pdf_capped, width = 9, height = 7)
res_full <- save_plot_pdf_base(p_full, pdf_full, width = 9, height = 7)

figure_index <- data.table(
  figure_type = c("all_filtered_genes_capped_publication", "all_filtered_genes_full_axis_audit"),
  pdf_path = c(pdf_capped, pdf_full),
  success = c(res_capped$success, res_full$success),
  message = c(res_capped$message, res_full$message)
)

atomic_write_csv(as.data.frame(figure_index), figure_index_csv)


# ============================================================
# 9. summary / method note / report
# ============================================================

summary_dt <- data.table(
  metric = c(
    "dataset",
    "object_id",
    "contrast_name",
    "class_A",
    "class_B",
    "group_col",
    "groups_A",
    "groups_B",
    "cells_A",
    "cells_B",
    "matrix_genes_total",
    "min_pct_filter",
    "genes_tested_after_min_pct",
    "arbitrary_top_gene_cap_used",
    "p_adjust_scope",
    "chunk_size",
    "chunks_total",
    "chunks_failed",
    "DEG_table_up_in_A_log2FC_0.25",
    "DEG_table_up_in_B_log2FC_0.25",
    "volcano_UP_log2FC_1",
    "volcano_DOWN_log2FC_1",
    "volcano_NS",
    "assay",
    "layer",
    "potential_replicate_metadata_for_pseudobulk",
    "capped_pdf",
    "full_axis_pdf",
    "claim_boundary"
  ),
  value = c(
    target$dataset,
    target$object_id,
    CONTRAST_NAME,
    paste(CLASS_A, collapse = ";"),
    paste(CLASS_B, collapse = ";"),
    group_col,
    paste(groups_A, collapse = ";"),
    paste(groups_B, collapse = ";"),
    nA,
    nB,
    n_genes_total,
    MIN_PCT,
    nrow(deg),
    "FALSE",
    "BH correction across all genes tested after MIN_PCT filtering",
    GENE_CHUNK_SIZE,
    length(chunks),
    sum(grepl("^failed", chunk_audit$status)),
    nrow(deg[DEG_call_table == "Up_in_A"]),
    nrow(deg[DEG_call_table == "Up_in_B"]),
    nrow(plot_dt[volcano_group == "UP"]),
    nrow(plot_dt[volcano_group == "DOWN"]),
    nrow(plot_dt[volcano_group == "NS"]),
    expr$assay,
    expr$layer,
    has_potential_pseudobulk,
    pdf_capped,
    pdf_full,
    "Journal-aware gene-level single-object exploratory validation; not a sample-level pseudo-bulk validation unless 08E confirms replicate-level results."
  )
)

atomic_write_csv(as.data.frame(summary_dt), summary_csv)

method_lines <- c(
  "08C method and claim-boundary note",
  "",
  "Method-ready wording:",
  paste0(
    "For the selected object-level candidate-state comparison, genes expressed in at least ",
    MIN_PCT * 100,
    "% of cells in either comparison group were retained. Differential expression was tested across all retained genes using a chunked matrix-based Welch-style procedure, and Benjamini-Hochberg correction was applied across all tested genes."
  ),
  "",
  "Figure caption-ready wording:",
  paste0(
    "Volcano plot of all genes passing the expression filter (MIN_PCT >= ",
    MIN_PCT,
    ") for ideal-like DA/projection-high safety-low versus lower-priority/mixed states. Displayed log2FC and -log10 adjusted P values were capped for visualization only; statistical classification used the uncapped values."
  ),
  "",
  "Claim boundary:",
  "This analysis is appropriate as a gene-level exploratory validation of transcriptional separation between candidate cell states within a selected object. It should not be described as sample-level pseudo-bulk validation, clinical prediction, functional integration, anatomical-projection claim, tumorigenicity proof, or therapeutic outcome validation.",
  "",
  "Replicate audit:",
  paste0("Potential replicate metadata usable for pseudo-bulk: ", has_potential_pseudobulk),
  paste0("Replicate audit table: ", replicate_audit_csv)
)

writeLines(method_lines, method_note_txt)

report_lines <- c(
  "08C JOURNAL-STANDARD all-filtered-genes chunked DEG volcano report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Target:",
  paste0("Dataset: ", target$dataset),
  paste0("Object ID: ", target$object_id),
  paste0("Contrast: ", CONTRAST_NAME),
  paste0("Cells A: ", nA),
  paste0("Cells B: ", nB),
  "",
  "Academic-standard correction:",
  paste0("Matrix genes total: ", n_genes_total),
  paste0("MIN_PCT filter: ", MIN_PCT),
  paste0("Genes tested after MIN_PCT: ", nrow(deg)),
  "No arbitrary top-gene cap was used.",
  "p.adjust was performed across all genes tested after objective expression filtering.",
  "",
  "Result summary:",
  paste0("DEG table Up_in_A at |log2FC| >= ", TABLE_LOG2FC_CUTOFF, ": ", nrow(deg[DEG_call_table == "Up_in_A"])),
  paste0("DEG table Up_in_B at |log2FC| >= ", TABLE_LOG2FC_CUTOFF, ": ", nrow(deg[DEG_call_table == "Up_in_B"])),
  paste0("Volcano style UP at |log2FC| >= ", VOLCANO_LOG2FC_CUTOFF, ": ", nrow(plot_dt[volcano_group == "UP"])),
  paste0("Volcano style DOWN at |log2FC| >= ", VOLCANO_LOG2FC_CUTOFF, ": ", nrow(plot_dt[volcano_group == "DOWN"])),
  paste0("Chunks failed: ", sum(grepl("^failed", chunk_audit$status))),
  paste0("Potential replicate metadata for pseudo-bulk: ", has_potential_pseudobulk),
  "",
  "Outputs:",
  paste0("All filtered genes DEG: ", deg_all_csv),
  paste0("Significant DEG: ", deg_sig_csv),
  paste0("Replicate audit: ", replicate_audit_csv),
  paste0("Chunk audit: ", chunk_audit_csv),
  paste0("Capped volcano PDF: ", pdf_capped),
  paste0("Full-axis volcano PDF: ", pdf_full),
  paste0("Method note: ", method_note_txt),
  paste0("Summary: ", summary_csv),
  "",
  "Next step:",
  "Use the significant DEG table for 08D GO/KEGG only if chunks_failed = 0. For stronger journal-level biological claims, add 08E pseudo-bulk replicate-level validation if replicate metadata are available.",
  "",
  "Journal-rigor note:",
  "This script corrects the earlier 10000-gene exploratory compromise. It tests all genes passing objective expression filtering and adjusts P values across all tested genes. However, it remains a selected single-object cell-state contrast unless replicated by a pseudo-bulk/sample-level analysis."
)

writeLines(report_lines, report_txt)


# ============================================================
# 10. 清理并结束
# ============================================================

rm(obj, mat)
gc(verbose = FALSE)

cat("\n============================================================\n")
cat("08C JOURNAL-STANDARD all-filtered-genes chunked DEG volcano 运行结束\n")
cat("============================================================\n\n")

cat("Dataset：", target$dataset, "\n")
cat("Object ID：", target$object_id, "\n")
cat("Contrast：", CONTRAST_NAME, "\n")
cat("Cells A：", nA, "\n")
cat("Cells B：", nB, "\n")
cat("Matrix genes total：", n_genes_total, "\n")
cat("MIN_PCT：", MIN_PCT, "\n")
cat("Genes tested after MIN_PCT：", nrow(deg), "\n")
cat("Arbitrary top-gene cap used：FALSE\n")
cat("p.adjust scope：all tested filtered genes\n")
cat("Chunks total：", length(chunks), "\n")
cat("Chunks failed：", sum(grepl('^failed', chunk_audit$status)), "\n")
cat("DEG table Up_in_A：", nrow(deg[DEG_call_table == "Up_in_A"]), "\n")
cat("DEG table Up_in_B：", nrow(deg[DEG_call_table == "Up_in_B"]), "\n")
cat("Volcano style UP / DOWN / NS：", nrow(plot_dt[volcano_group == "UP"]), " / ", nrow(plot_dt[volcano_group == "DOWN"]), " / ", nrow(plot_dt[volcano_group == "NS"]), "\n")
cat("Potential replicate metadata for pseudo-bulk：", has_potential_pseudobulk, "\n")
cat("Capped PDF success：", res_capped$success, "\n")
cat("Full-axis PDF success：", res_full$success, "\n\n")

cat("输出文件：\n")
cat(deg_all_csv, "\n")
cat(deg_sig_csv, "\n")
cat(plot_table_csv, "\n")
cat(label_table_csv, "\n")
cat(summary_csv, "\n")
cat(chunk_audit_csv, "\n")
cat(replicate_audit_csv, "\n")
cat(figure_index_csv, "\n")
cat(method_note_txt, "\n")
cat(report_txt, "\n\n")

cat("输出 PDF：\n")
cat(pdf_capped, "\n")
cat(pdf_full, "\n\n")

cat("✅ 08C JOURNAL-STANDARD all-filtered-genes chunked DEG volcano 完成。\n")
cat("下一步：检查 summary 里的 Chunks failed 是否为 0，并检查 capped PDF；若通过，再用 significant DEG table 做 08D GO/KEGG。\n")
