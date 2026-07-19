# ============================================================
# 08B_FINAL_V3_FIX_HEATMAP_COLORBAR_LAYOUT.R
# ============================================================
# 目的：
#   08B FINAL 已经跑通，但是三个 PDF 图里有长标题/长标签被裁切。
#   V3 只重新生成布局修正版 PDF，不重新计算 08B 表格。
#
# 输入：
#   D:/PD_Graft_Project/03_tables/08B_FINAL_candidate_state_signature_interpretation/
#     08B_FINAL_class_category_program_summary.csv
#     08B_FINAL_state_vs_rest_category_direction.csv
#     08B_FINAL_top_marker_genes_by_state.csv
#
# 输出：
#   D:/PD_Graft_Project/04_figures/08B_FINAL_V3_heatmap_colorbar_fixed_pdf/
#
# 修复：
#   1. 缩短图标题，避免右侧被裁切
#   2. 增大 PDF 宽度和边距
#   3. 缩短/换行 candidate state 标签
#   4. 减小部分坐标轴字体
#   5. 保留 PDF 输出，不输出 PNG
#
# 成功标志：
#   ✅ 08B FINAL V3 heatmap colorbar-fixed PDF figures 完成。
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"

TOP_GENES_PER_STATE <- 20
SEED <- 20260714


# ============================================================
# 1. 加载包
# ============================================================

cat("\n============================================================\n")
cat("08B FINAL V3：heatmap colorbar layout fixed PDF figures only\n")
cat("============================================================\n\n")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("缺少 data.table，请先安装。")
}

suppressPackageStartupMessages({
  library(data.table)
})


# ============================================================
# 2. 路径
# ============================================================

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_tables_dir <- file.path(tables_dir, "08B_FINAL_candidate_state_signature_interpretation")

input_class_category <- file.path(input_tables_dir, "08B_FINAL_class_category_program_summary.csv")
input_state_category <- file.path(input_tables_dir, "08B_FINAL_state_vs_rest_category_direction.csv")
input_top_marker <- file.path(input_tables_dir, "08B_FINAL_top_marker_genes_by_state.csv")

out_tables_dir <- file.path(tables_dir, "08B_FINAL_V3_heatmap_colorbar_fixed_figures")
out_figures_dir <- file.path(figures_dir, "08B_FINAL_V3_heatmap_colorbar_fixed_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

figure_index_csv <- file.path(out_tables_dir, "08B_FINAL_V3_figure_index.csv")
layout_audit_csv <- file.path(out_tables_dir, "08B_FINAL_V3_layout_audit.csv")
report_txt <- file.path(reports_dir, "08B_FINAL_V3_heatmap_colorbar_fixed_figures_report.txt")


# ============================================================
# 3. 工具函数
# ============================================================

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_required <- function(path) {
  if (!file.exists(path)) stop("找不到输入表：", path)
  data.table::fread(path, data.table = TRUE, showProgress = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

pretty_category <- function(x) {
  x <- as.character(x)
  x <- gsub("_", " ", x)
  x
}

state_order <- c(
  "ideal_DA_projection_high_safety_low",
  "mixed_DA_or_projection_with_safety_risk",
  "projection_competence_without_DA_low_safety",
  "high_safety_risk_low_DA",
  "lower_priority_or_mixed"
)

state_short_title <- function(x) {
  x <- as.character(x)
  out <- x
  out[x == "ideal_DA_projection_high_safety_low"] <- "Ideal-like"
  out[x == "mixed_DA_or_projection_with_safety_risk"] <- "Mixed-risk"
  out[x == "projection_competence_without_DA_low_safety"] <- "Projection DA-low"
  out[x == "high_safety_risk_low_DA"] <- "High-risk low-DA"
  out[x == "lower_priority_or_mixed"] <- "Lower-priority"
  out
}

state_short_axis <- function(x) {
  x <- as.character(x)
  out <- x
  out[x == "ideal_DA_projection_high_safety_low"] <- "Ideal-like\nDA/proj high\nsafety-low"
  out[x == "mixed_DA_or_projection_with_safety_risk"] <- "Mixed\nDA/proj +\nrisk"
  out[x == "projection_competence_without_DA_low_safety"] <- "Projection\nDA-low\nsafety-low"
  out[x == "high_safety_risk_low_DA"] <- "High risk\nlow DA"
  out[x == "lower_priority_or_mixed"] <- "Lower\npriority/mixed"
  out
}

safe_pdf <- function(path, width, height) {
  grDevices::pdf(path, width = width, height = height, useDingbats = FALSE)
}

xlim_with_padding <- function(vals, force_zero = TRUE, pad_frac = 0.12) {
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(c(-1, 1))

  rng <- range(vals)
  if (force_zero) rng <- range(c(rng, 0))

  span <- diff(rng)
  if (!is.finite(span) || span == 0) {
    span <- max(abs(rng), 1)
    rng <- c(rng[1] - span * 0.5, rng[2] + span * 0.5)
  }

  pad <- span * pad_frac
  c(rng[1] - pad, rng[2] + pad)
}


# ============================================================
# 4. 读取 08B FINAL 表格
# ============================================================

set.seed(SEED)

stamp("读取 08B FINAL 输出表。")

class_category <- read_required(input_class_category)
state_category <- read_required(input_state_category)
top_marker <- read_required(input_top_marker)

states <- intersect(state_order, unique(state_category$safety_contrast_class_05B))
states <- c(states, setdiff(unique(state_category$safety_contrast_class_05B), states))

stamp("states：", paste(state_short_title(states), collapse = "; "))
stamp("category rows：", nrow(state_category))
stamp("top marker rows：", nrow(top_marker))


# ============================================================
# 5. Figure 1: layout-fixed heatmap
# ============================================================

plot_heatmap_fixed <- function(summary_dt, pdf_path) {
  dt <- copy(summary_dt)
  dt <- dt[safety_contrast_class_05B %in% states]

  cats <- sort(unique(dt$category))
  sts <- states

  value_col <- "mean_expr_z_category_object"
  if (!value_col %in% names(dt)) {
    value_col <- "mean_expr_z_gene_object"
  }

  mat <- matrix(
    NA_real_,
    nrow = length(cats),
    ncol = length(sts),
    dimnames = list(pretty_category(cats), state_short_axis(sts))
  )

  for (i in seq_along(cats)) {
    for (j in seq_along(sts)) {
      val <- dt[category == cats[[i]] & safety_contrast_class_05B == sts[[j]], mean(get(value_col), na.rm = TRUE)]
      if (length(val) == 1 && is.finite(val)) mat[i, j] <- val
    }
  }

  mat[is.na(mat)] <- 0
  mat <- mat[order(rownames(mat)), , drop = FALSE]

  pdf_height <- max(7.8, 0.30 * nrow(mat) + 2.9)

  # V3: 加宽 PDF，并把 colorbar 数字放到设备内部，避免右侧数字被裁切。
  safe_pdf(pdf_path, width = 13.8, height = pdf_height)
  on.exit(grDevices::dev.off(), add = TRUE)

  # 右边距不用太大，因为 colorbar 直接在 heatmap 右侧的 plot region 内绘制。
  par(mar = c(10.2, 11.2, 3.4, 5.0), xpd = FALSE)

  zlim <- max(abs(mat), na.rm = TRUE)
  if (!is.finite(zlim) || zlim == 0) zlim <- 1

  pal <- grDevices::colorRampPalette(c("#2166AC", "white", "#B2182B"))(101)
  breaks <- seq(-zlim, zlim, length.out = length(pal) + 1)

  # 扩大 xlim，在热图右边预留 colorbar 空间。
  x_heat <- seq_len(ncol(mat))
  y_heat <- seq_len(nrow(mat))
  xlim_full <- c(0.5, ncol(mat) + 1.35)
  ylim_full <- c(0.5, nrow(mat) + 0.5)

  image(
    x = x_heat,
    y = y_heat,
    z = t(mat),
    col = pal,
    breaks = breaks,
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = "08B candidate-state marker program heatmap",
    xlim = xlim_full,
    ylim = ylim_full
  )

  axis(1, at = seq_len(ncol(mat)), labels = colnames(mat), las = 2, cex.axis = 0.70, tick = FALSE)
  axis(2, at = seq_len(nrow(mat)), labels = rownames(mat), las = 2, cex.axis = 0.66, tick = FALSE)

  # 只框住 heatmap 主体，不框住 colorbar 空间。
  rect(0.5, 0.5, ncol(mat) + 0.5, nrow(mat) + 0.5, border = "black", lwd = 1)

  # colorbar: 画在 plot region 里面，所有文字都不会越界。
  legend_x1 <- ncol(mat) + 0.72
  legend_x2 <- ncol(mat) + 0.92
  legend_y <- seq(1.0, nrow(mat), length.out = length(pal))

  rect(
    xleft = legend_x1,
    ybottom = legend_y[-length(legend_y)],
    xright = legend_x2,
    ytop = legend_y[-1],
    col = pal,
    border = NA
  )

  text(legend_x2 + 0.16, min(legend_y), labels = round(-zlim, 2), cex = 0.58, adj = 0)
  text(legend_x2 + 0.16, max(legend_y), labels = round(zlim, 2), cex = 0.58, adj = 0)
  text(legend_x2 + 0.16, mean(range(legend_y)), labels = "z", cex = 0.65, adj = 0)

  invisible(TRUE)
}


# ============================================================
# 6. Figure 2: layout-fixed category barplots
# ============================================================

plot_state_category_bars_fixed <- function(cat_dt, pdf_path) {
  safe_pdf(pdf_path, width = 11.4, height = 7.2)
  on.exit(grDevices::dev.off(), add = TRUE)

  for (st in states) {
    sub <- cat_dt[safety_contrast_class_05B == st]
    if (nrow(sub) == 0) next

    sub <- sub[order(delta_state_vs_rest)]
    vals <- sub$delta_state_vs_rest
    labs <- pretty_category(sub$category)
    cols <- ifelse(vals >= 0, "#B2182B", "#2166AC")

    par(mar = c(5.3, 12.2, 3.4, 2.2), xpd = FALSE)

    barplot(
      vals,
      horiz = TRUE,
      names.arg = labs,
      las = 2,
      col = cols,
      border = NA,
      cex.names = 0.68,
      cex.axis = 0.78,
      xlab = "State-vs-rest category program difference",
      main = paste0("08B category programs | ", state_short_title(st)),
      cex.main = 0.95,
      cex.lab = 0.88,
      xlim = xlim_with_padding(vals, force_zero = TRUE, pad_frac = 0.16)
    )

    abline(v = 0, lty = 2, col = "grey40")
  }

  invisible(TRUE)
}


# ============================================================
# 7. Figure 3: layout-fixed top marker gene barplots
# ============================================================

plot_top_marker_genes_fixed <- function(top_dt, pdf_path) {
  safe_pdf(pdf_path, width = 10.4, height = 7.3)
  on.exit(grDevices::dev.off(), add = TRUE)

  for (st in states) {
    sub <- top_dt[safety_contrast_class_05B == st]
    if (nrow(sub) == 0) next

    sub <- sub[order(delta_state_vs_rest)]
    if (nrow(sub) > TOP_GENES_PER_STATE) {
      sub <- tail(sub, TOP_GENES_PER_STATE)
    }

    vals <- sub$delta_state_vs_rest
    labs <- sub$gene
    cols <- ifelse(vals >= 0, "#B2182B", "#2166AC")

    par(mar = c(5.2, 7.6, 3.4, 2.0), xpd = FALSE)

    barplot(
      vals,
      horiz = TRUE,
      names.arg = labs,
      las = 2,
      col = cols,
      border = NA,
      cex.names = 0.72,
      cex.axis = 0.78,
      xlab = "State-vs-rest marker gene difference",
      main = paste0("08B top marker genes | ", state_short_title(st)),
      cex.main = 0.95,
      cex.lab = 0.88,
      xlim = xlim_with_padding(vals, force_zero = TRUE, pad_frac = 0.16)
    )

    abline(v = 0, lty = 2, col = "grey40")
  }

  invisible(TRUE)
}


# ============================================================
# 8. 生成 PDF
# ============================================================

stamp("生成布局修正版 PDF。")

figure_records <- list()

# heatmap
pdf1 <- file.path(out_figures_dir, "08B_FINAL_V3_candidate_state_category_program_heatmap_layout_fixed.pdf")
ok <- FALSE; msg <- NA_character_
tryCatch({
  ok <- plot_heatmap_fixed(class_category, pdf1)
}, error = function(e) {
  msg <<- conditionMessage(e)
})
figure_records[[length(figure_records) + 1L]] <- data.table(
  figure_type = "category_program_heatmap_layout_fixed",
  pdf_path = ifelse(isTRUE(ok) && file.exists(pdf1), pdf1, NA_character_),
  success = isTRUE(ok) && file.exists(pdf1),
  message = msg
)

# category bars
pdf2 <- file.path(out_figures_dir, "08B_FINAL_V3_state_vs_rest_category_program_barplots_layout_fixed.pdf")
ok <- FALSE; msg <- NA_character_
tryCatch({
  ok <- plot_state_category_bars_fixed(state_category, pdf2)
}, error = function(e) {
  msg <<- conditionMessage(e)
})
figure_records[[length(figure_records) + 1L]] <- data.table(
  figure_type = "state_vs_rest_category_barplots_layout_fixed",
  pdf_path = ifelse(isTRUE(ok) && file.exists(pdf2), pdf2, NA_character_),
  success = isTRUE(ok) && file.exists(pdf2),
  message = msg
)

# top genes
pdf3 <- file.path(out_figures_dir, "08B_FINAL_V3_top_marker_genes_by_candidate_state_layout_fixed.pdf")
ok <- FALSE; msg <- NA_character_
tryCatch({
  ok <- plot_top_marker_genes_fixed(top_marker, pdf3)
}, error = function(e) {
  msg <<- conditionMessage(e)
})
figure_records[[length(figure_records) + 1L]] <- data.table(
  figure_type = "top_marker_genes_layout_fixed",
  pdf_path = ifelse(isTRUE(ok) && file.exists(pdf3), pdf3, NA_character_),
  success = isTRUE(ok) && file.exists(pdf3),
  message = msg
)

figure_index <- rbindlist(figure_records, fill = TRUE)
atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

layout_audit <- data.table(
  metric = c(
    "input_class_category_rows",
    "input_state_category_rows",
    "input_top_marker_rows",
    "states",
    "successful_figures",
    "output_figure_directory",
    "claim_boundary"
  ),
  value = c(
    nrow(class_category),
    nrow(state_category),
    nrow(top_marker),
    paste(state_short_title(states), collapse = "; "),
    sum(figure_index$success, na.rm = TRUE),
    out_figures_dir,
    "Layout-only figure regeneration; no change to 08B numerical results."
  )
)

atomic_write_csv(as.data.frame(layout_audit), layout_audit_csv)

report_lines <- c(
  "08B FINAL V3 heatmap colorbar-fixed figures report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Purpose:",
  "Regenerate the three 08B FINAL PDF figures with shorter titles, larger margins, wider PDF layout, and a heatmap colorbar that stays inside the PDF canvas.",
  "",
  "Successful figures:",
  paste0(sum(figure_index$success, na.rm = TRUE), " / ", nrow(figure_index)),
  "",
  "Output figures:",
  figure_index$pdf_path,
  "",
  "Output tables:",
  paste0("Figure index: ", figure_index_csv),
  paste0("Layout audit: ", layout_audit_csv),
  "",
  "Note:",
  "This script does not recalculate 08B results. It only fixes PDF layout and label clipping."
)

writeLines(report_lines, report_txt)


# ============================================================
# 9. 结束
# ============================================================

cat("\n============================================================\n")
cat("08B FINAL V3 heatmap colorbar-fixed PDF figures 运行结束\n")
cat("============================================================\n\n")

cat("Successful figures：", sum(figure_index$success, na.rm = TRUE), " / ", nrow(figure_index), "\n\n")

cat("输出 PDF 图片目录：\n")
cat(out_figures_dir, "\n\n")

cat("输出文件：\n")
cat(figure_index_csv, "\n")
cat(layout_audit_csv, "\n")
cat(report_txt, "\n\n")

cat("✅ 08B FINAL V3 heatmap colorbar-fixed PDF figures 完成。\n")
cat("下一步：检查 V3_heatmap_colorbar_fixed_pdf 文件夹里的三个 PDF，重点看 heatmap 右侧 colorbar 数字是否完整。\n")
