# ============================================================
# 09A_SCRNA_CELL_STATE_PROPORTION_FINAL_PUBLICATION_V6_MANUAL_PDF_DEVICE.R
# ============================================================
# V6 修复：
#   V5 已通过数据清洗、补零和比例计算，并成功保存第一个 PDF。
#   失败点是保存后续 ggplot PDF 时出现：
#     Error in fetch_ggproto(super(), name):
#       无法改变被锁定的联编 'res' 的值
#
#   这属于 ggplot2::ggsave / 当前 RStudio 图形设备环境中的图形设备冲突，
#   不是 09A 数据问题。
#
# V6 改动：
#   1. 不改统计逻辑。
#   2. 不改 fraction / priority index / class mapping。
#   3. 不再使用 ggplot2::ggsave 保存 PDF。
#   4. 改用 grDevices::pdf() + print(plot) + dev.off() 的手动 PDF device。
#   5. 每次保存前关闭非必要图形设备，避免 RStudio device 锁定冲突。
#
# 严谨性原则不变：
#   - 使用 full 05B group-level table
#   - 按 n_cells_05B 加权计算 fraction
#   - 不使用 03B downsampled object
#   - 不做任意 top-N 删除
#   - PDF only
# ============================================================

# ============================================================
# 09A_SCRNA_CELL_STATE_PROPORTION_FINAL_PUBLICATION_V6_MANUAL_PDF_DEVICE.R
# ============================================================
# V5 修复：
#   V4 preflight 发现：
#     dataset_class_fraction_NA = 10
#     object_class_fraction_NA = 141
#     dataset_priority_index_NA = 3
#
#   原因：
#     为了补齐缺失 class 组合，脚本创建了 dataset/object × class 的完整网格。
#     对某些 dataset/object 中不存在的 class，class_cells 已正确设为 0，
#     但 total cell count 在二次 merge 后没有从 *_totalcopy 列回填，
#     导致 0 / NA = NA。
#
# V5 改动：
#   1. 对缺失 class 组合，回填 dataset_total_cells / object_total_cells。
#   2. 对缺失 group rows，设为 0。
#   3. class_fraction = 0 / total_cells = 0，而不是 NA。
#   4. 保留 V4 strict preflight；如果还有 NA，会继续 stop。
#
# 严谨性原则不变：
#   - 使用 full 05B group-level table
#   - 按 n_cells_05B 加权计算 fraction
#   - 不使用 03B downsampled object
#   - 不做任意 top-N 删除
#   - PDF only
# ============================================================

# ============================================================
# 09A_SCRNA_CELL_STATE_PROPORTION_FINAL_PUBLICATION_V6_MANUAL_PDF_DEVICE.R
# ============================================================
# V4 修复：
#   V3 已经完成全部输出，但 ggplot 仍提示 geom_col Removed rows。
#   V4 不改 09A 的统计逻辑，只在作图前建立严格的 plot-ready tables：
#     - 检查 fraction 是否 NA / outside 0-1
#     - 检查 priority index 是否 NA
#     - 检查 factor labels 是否 NA
#     - 作图只使用通过检查的 plot-ready table
#   如果真实数据有问题，直接 stop，不再让 ggplot 静默删除行。
#
# 严谨性原则不变：
#   - 使用 full 05B group-level table
#   - 按 n_cells_05B 加权计算 fraction
#   - 不使用 03B downsampled object
#   - 不做任意 top-N 删除
#   - PDF only
# ============================================================

# ============================================================
# 09A_SCRNA_CELL_STATE_PROPORTION_FINAL_PUBLICATION_V6_MANUAL_PDF_DEVICE.R
# ============================================================
# V3 修复：
#   V2 已完成所有表格和 PDF，但 ggplot 出现：
#     Removed rows containing missing values or values outside the scale range (`geom_col()`).
#
#   这不是比例计算失败，而是 stacked bar / barplot 使用 scale_y_continuous(limits=c(0,1))
#   时，ggplot 可能在堆叠或边界处删除图层。
#
# V3 改动：
#   1. 不改 09A 的统计逻辑。
#   2. 不改 class mapping / fraction / priority index 计算。
#   3. 把 stacked bar 的 y 轴硬 limits 改为 coord_cartesian(ylim=c(0,1))。
#   4. dataset priority index 不再对 y 轴设 0-1 限制，因为 index 可以是 -1 到 1。
#   5. object scatter 先保留 0-1 view，但用 coord_cartesian，避免删点。
#   6. 输出目录改成 V3，避免覆盖 V2。
# ============================================================

# ============================================================
# 09A_SCRNA_CELL_STATE_PROPORTION_FINAL_PUBLICATION_V6_MANUAL_PDF_DEVICE.R
# ============================================================
# V2 修复：
#   1. 修复 object_order / dataset_order 中可能存在重复 level，
#      导致 factor level duplicated 的问题。
#   2. 增强 05B class 列识别：
#      优先选择同时包含 ideal/lower/mixed/safety 语义的 contrast/priority class 列；
#      避免只选到 safety_risk_label_05B 而丢失 ideal-like / lower-priority 信息。
#
# 严谨性原则不变：
#   - 使用 full 05B group-level table
#   - 按 n_cells_05B 加权计算 fraction
#   - 不使用 03B downsampled object
#   - PDF only
# ============================================================

# ============================================================
# 09A_SCRNA_CELL_STATE_PROPORTION_FINAL_PUBLICATION_V6_MANUAL_PDF_DEVICE.R
# ============================================================
# 09A：scRNA cell-state proportion / frequency analysis
# FINAL publication candidate for SCI Q2-Q3 target
#
# 目的：
#   在 05B 已完成的 DA/projection-vs-safety cell-state classification 基础上，
#   做正式的 cell-state proportion / frequency 分析。
#
# 为什么这是 09A：
#   08C/08D/08E 已经证明 ideal-like vs lower-priority 的 pathway 差异；
#   09A 回到 scRNA 主线，回答：
#     - 每个 dataset / object 中 ideal-like 状态占多少？
#     - lower-priority / safety-risk-associated 状态占多少？
#     - 哪些 dataset / object 更 favorable？
#     - 哪些 dataset / object 更 safety-risk-associated？
#
# 严谨性原则：
#   1. 使用 05B 的 full group-level classification table。
#   2. 不使用 03B downsampled object 做最终比例结论。
#   3. 不做任意 top-N cell/state 删除。
#   4. 比例计算必须基于 group cell count；如果找不到 cell-count 列，脚本会停止。
#   5. 所有 class fraction 使用 weighted cell counts，而不是简单 group 数。
#   6. 输出完整表格、audit、PDF figures。
#   7. PDF only，不输出 PNG。
#
# 解释边界：
#   09A 是 transcriptomic cell-state composition analysis。
#   它不能证明真实移植疗效、解剖投射、功能整合或临床安全性。
#
# 输入：
#   D:/PD_Graft_Project/03_tables/05B_safety_risk_scoring/
#       05B_DA_projection_vs_safety_contrast_groups.csv
#
# 输出：
#   D:/PD_Graft_Project/03_tables/09A_scRNA_cell_state_proportion_final_V6/
#   D:/PD_Graft_Project/04_figures/09A_scRNA_cell_state_proportion_final_V6_pdf/
#
# 成功标志：
#   ✅ 09A scRNA cell-state proportion FINAL V6 完成。
# ============================================================


# ============================================================
# 0. 用户设置
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"

INPUT_05B_GROUPS <- file.path(
  PROJECT_DIR,
  "03_tables",
  "05B_safety_risk_scoring",
  "05B_DA_projection_vs_safety_contrast_groups.csv"
)

PDF_WIDTH_WIDE <- 12.5
PDF_HEIGHT_MEDIUM <- 7.8
PDF_WIDTH_SQUARE <- 9.5
PDF_HEIGHT_SQUARE <- 8.5

MIN_TOTAL_CELLS_FOR_OBJECT_SUMMARY <- 1

SEED <- 20260714


# ============================================================
# 1. 加载包
# ============================================================

# 不覆盖 Bioconductor repositories；不自动安装包。
options(timeout = 60000)

cat("\n============================================================\n")
cat("09A scRNA cell-state proportion FINAL V6\n")
cat("============================================================\n\n")

required_pkgs <- c(
  "data.table",
  "ggplot2"
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
  library(ggplot2)
})

options(error = NULL)
# V6: PDF only; avoid bitmap device options that can interact with RStudio graphics
# options(bitmapType = "cairo")
set.seed(SEED)


# ============================================================
# 2. 输出路径
# ============================================================

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(tables_dir, "09A_scRNA_cell_state_proportion_final_V6")
out_figures_dir <- file.path(figures_dir, "09A_scRNA_cell_state_proportion_final_V6_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

standardized_input_csv <- file.path(out_tables_dir, "09A_standardized_05B_group_input.csv")
column_audit_csv <- file.path(out_tables_dir, "09A_input_column_detection_audit.csv")
class_mapping_audit_csv <- file.path(out_tables_dir, "09A_class_mapping_audit.csv")

dataset_class_fraction_csv <- file.path(out_tables_dir, "09A_dataset_class_fraction.csv")
object_class_fraction_csv <- file.path(out_tables_dir, "09A_object_class_fraction.csv")
object_priority_summary_csv <- file.path(out_tables_dir, "09A_object_priority_summary.csv")
dataset_priority_summary_csv <- file.path(out_tables_dir, "09A_dataset_priority_summary.csv")

figure_index_csv <- file.path(out_tables_dir, "09A_figure_index.csv")
method_note_txt <- file.path(out_tables_dir, "09A_method_and_claim_boundary_note.txt")
output_check_csv <- file.path(out_tables_dir, "09A_output_verification.csv")
session_info_txt <- file.path(out_tables_dir, "09A_sessionInfo.txt")
report_txt <- file.path(reports_dir, "09A_scRNA_cell_state_proportion_final_V6_report.txt")

fig_dataset_stacked_pdf <- file.path(out_figures_dir, "09A_dataset_cell_state_composition_stacked_bar.pdf")
fig_object_stacked_pdf <- file.path(out_figures_dir, "09A_object_cell_state_composition_stacked_bar.pdf")
fig_dataset_priority_pdf <- file.path(out_figures_dir, "09A_dataset_priority_index_barplot.pdf")
fig_object_scatter_pdf <- file.path(out_figures_dir, "09A_object_ideal_vs_safety_fraction_scatter.pdf")
fig_heatmap_pdf <- file.path(out_figures_dir, "09A_dataset_class_fraction_heatmap.pdf")


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
    removed <- file.remove(file_path)
    if (!isTRUE(removed)) {
      stop(
        "旧 PDF 正在被占用，无法覆盖：", file_path,
        "\n请关闭 Edge/Adobe/RStudio Viewer/文件资源管理器预览窗口后重跑，或删除旧 PDF。"
      )
    }
  }

  # V6：不要用 ggsave，避免 RStudio/ggplot2 device 触发 locked binding 'res'。
  # 关闭非必要图形设备，仅保留 null device。
  while (grDevices::dev.cur() > 1) {
    try(grDevices::dev.off(), silent = TRUE)
    if (grDevices::dev.cur() <= 1) break
  }

  grDevices::pdf(
    file = file_path,
    width = width,
    height = height,
    useDingbats = FALSE,
    onefile = TRUE
  )

  ok <- FALSE
  tryCatch({
    print(plot_obj)
    ok <- TRUE
  }, error = function(e) {
    message("PDF print failed for: ", file_path)
    message("Error: ", conditionMessage(e))
    ok <<- FALSE
  }, finally = {
    try(grDevices::dev.off(), silent = TRUE)
  })

  if (!isTRUE(ok)) {
    stop("PDF 绘图失败：", file_path)
  }

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

pick_col_exact <- function(dt, candidates, required = TRUE, label = "column") {
  nm <- names(dt)
  nm_lower <- tolower(nm)

  for (cand in candidates) {
    idx <- which(nm_lower == tolower(cand))
    if (length(idx) > 0) {
      return(nm[idx[1]])
    }
  }

  if (isTRUE(required)) {
    stop("找不到必要列：", label, "。候选名：", paste(candidates, collapse = ", "))
  }

  NA_character_
}

pick_col_regex <- function(dt, patterns, required = TRUE, label = "column") {
  nm <- names(dt)
  nm_lower <- tolower(nm)

  hit <- rep(FALSE, length(nm_lower))
  for (pat in patterns) {
    hit <- hit | grepl(pat, nm_lower, perl = TRUE)
  }

  idx <- which(hit)
  if (length(idx) > 0) {
    return(nm[idx[1]])
  }

  if (isTRUE(required)) {
    stop("找不到必要列：", label, "。regex：", paste(patterns, collapse = " | "))
  }

  NA_character_
}

clean_class_simple <- function(x) {
  x0 <- as.character(x)
  x_low <- tolower(x0)

  out <- rep("other_or_unclassified", length(x_low))

  out[
    grepl("ideal", x_low) |
      grepl("favorable", x_low) |
      (grepl("da", x_low) & grepl("projection", x_low) & grepl("safety", x_low) & grepl("low", x_low))
  ] <- "ideal_like"

  out[
    grepl("lower", x_low) |
      grepl("mixed", x_low) |
      grepl("low_priority", x_low) |
      grepl("lower_priority", x_low)
  ] <- "lower_priority_mixed"

  out[
    grepl("risk_high", x_low) |
      grepl("safety_high", x_low) |
      (grepl("safety", x_low) & grepl("risk", x_low) & grepl("high", x_low)) |
      (grepl("prolifer", x_low) & grepl("risk", x_low))
  ] <- "safety_risk_high"

  out
}

class_order <- c(
  "ideal_like",
  "lower_priority_mixed",
  "safety_risk_high",
  "other_or_unclassified"
)

class_labels <- c(
  ideal_like = "Ideal-like",
  lower_priority_mixed = "Lower-priority/mixed",
  safety_risk_high = "Safety-risk-high",
  other_or_unclassified = "Other/unclassified"
)

class_colors <- c(
  ideal_like = "#D73027",
  lower_priority_mixed = "#4575B4",
  safety_risk_high = "#1A9850",
  other_or_unclassified = "grey70"
)

theme_pub <- theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    axis.title = element_text(face = "plain", color = "black", size = 13),
    axis.text = element_text(color = "black", size = 10),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.2),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    plot.margin = margin(8, 16, 8, 8)
  )


# ============================================================
# 4. 读取 05B group table
# ============================================================

stamp("读取 05B DA/projection-vs-safety contrast group table。")

if (!file.exists(INPUT_05B_GROUPS)) {
  stop("找不到 05B group table：", INPUT_05B_GROUPS)
}

dt0 <- fread(INPUT_05B_GROUPS, data.table = TRUE, showProgress = FALSE)

if (nrow(dt0) == 0) {
  stop("05B group table 是空的。")
}

stamp("05B group rows：", nrow(dt0))
stamp("05B columns：", length(names(dt0)))


# ============================================================
# 5. 自动识别关键列
# ============================================================

dataset_col <- pick_col_exact(
  dt0,
  candidates = c("dataset", "dataset_id", "geo", "GEO", "study", "source_dataset"),
  required = FALSE,
  label = "dataset"
)

if (is.na(dataset_col)) {
  dataset_col <- pick_col_regex(
    dt0,
    patterns = c("^dataset$", "geo", "study"),
    required = TRUE,
    label = "dataset"
  )
}

object_col <- pick_col_exact(
  dt0,
  candidates = c("object_id", "object", "rds_id", "sample_object", "seurat_object", "sample_id", "sample", "orig.ident", "orig_ident"),
  required = FALSE,
  label = "object"
)

if (is.na(object_col)) {
  object_col <- dataset_col
}

group_col <- pick_col_exact(
  dt0,
  candidates = c("group_id", "group", "cluster", "cluster_id", "seurat_clusters", "cell_state", "annotation", "annotation_v1_conservative"),
  required = FALSE,
  label = "group"
)

if (is.na(group_col)) {
  group_col <- object_col
}

class_col <- pick_col_exact(
  dt0,
  candidates = c(
    "DA_projection_vs_safety_contrast_class",
    "da_projection_vs_safety_contrast_class",
    "DA_projection_vs_safety_contrast_class_05B",
    "da_projection_vs_safety_contrast_class_05B",
    "projection_vs_safety_contrast_class",
    "projection_vs_safety_contrast_class_05B",
    "safety_contrast_class",
    "safety_contrast_class_05B",
    "contrast_class",
    "contrast_class_05B",
    "final_priority_label",
    "priority_label",
    "priority_label_05B",
    "priority_class",
    "priority_class_05B",
    "state_class",
    "cell_state_class",
    "story_class",
    "class"
  ),
  required = FALSE,
  label = "cell-state class"
)

if (is.na(class_col)) {
  # fallback: find a character column containing expected keywords
  char_cols <- names(dt0)[vapply(dt0, function(z) is.character(z) || is.factor(z), logical(1))]
  best_col <- NA_character_
  best_score <- -Inf

  for (cc in char_cols) {
    vals <- tolower(as.character(dt0[[cc]]))
    vals <- vals[!is.na(vals)]
    score <- sum(grepl("ideal|lower|mixed|safety|risk|priority|favorable", vals))
    if (score > best_score) {
      best_score <- score
      best_col <- cc
    }
  }

  if (!is.na(best_col) && best_score > 0) {
    class_col <- best_col
  } else {
    stop("找不到 cell-state class 列。请检查 05B table 中是否有 ideal/lower/safety/risk 分类列。")
  }
}


# V2：即使 exact candidate 选到了 safety_risk_label_05B，
# 也再扫描所有 character/factor 列，优先选择同时包含 ideal/lower/mixed/safety/priority 的列。
score_class_column_09A <- function(values) {
  vals <- tolower(as.character(values))
  vals <- vals[!is.na(vals) & vals != ""]
  if (length(vals) == 0) return(data.table(
    score = -Inf,
    ideal_n = 0L,
    lower_n = 0L,
    safety_n = 0L,
    priority_n = 0L,
    unique_n = 0L
  ))

  ideal_n <- sum(grepl("ideal|favorable", vals))
  lower_n <- sum(grepl("lower|mixed|low_priority|lower_priority", vals))
  safety_n <- sum(grepl("safety|risk", vals))
  priority_n <- sum(grepl("priority|class|state|contrast", vals))
  unique_n <- length(unique(vals))

  score <- ideal_n * 10 + lower_n * 10 + safety_n * 2 + priority_n * 1 + min(unique_n, 20) * 0.1

  data.table(
    score = score,
    ideal_n = ideal_n,
    lower_n = lower_n,
    safety_n = safety_n,
    priority_n = priority_n,
    unique_n = unique_n
  )
}

char_cols_for_class <- names(dt0)[vapply(dt0, function(z) is.character(z) || is.factor(z), logical(1))]

class_score_table <- rbindlist(lapply(char_cols_for_class, function(cc) {
  ss <- score_class_column_09A(dt0[[cc]])
  ss[, column := cc]
  ss
}), fill = TRUE)

if (nrow(class_score_table) > 0) {
  setorder(class_score_table, -score, -ideal_n, -lower_n, -safety_n, column)

  best_class_col <- class_score_table$column[1]
  best_score <- class_score_table$score[1]

  current_score <- if (!is.na(class_col)) {
    score_class_column_09A(dt0[[class_col]])$score[1]
  } else {
    -Inf
  }

  # 只要找到同时具有 ideal 和 lower 语义的列，就优先于纯 safety label。
  best_has_ideal_lower <- class_score_table$ideal_n[1] > 0 && class_score_table$lower_n[1] > 0

  if (!is.na(best_class_col) && isTRUE(best_has_ideal_lower) && best_score > current_score) {
    class_col <- best_class_col
  }
}

cell_count_col <- pick_col_exact(
  dt0,
  candidates = c(
    "n_cells", "cell_count", "cells", "n_cell", "cell_n",
    "group_n_cells", "group_cell_n", "n_cells_group",
    "nCells", "NumberOfCells", "num_cells",
    "n_cells_05B"
  ),
  required = FALSE,
  label = "cell count"
)

if (is.na(cell_count_col)) {
  numeric_cols <- names(dt0)[vapply(dt0, is.numeric, logical(1))]
  candidate_num <- numeric_cols[
    grepl("cell|cells|ncell|count|freq", tolower(numeric_cols), perl = TRUE)
  ]

  if (length(candidate_num) > 0) {
    cell_count_col <- candidate_num[1]
  }
}

if (is.na(cell_count_col)) {
  stop(
    "找不到 cell-count 列，不能做 journal-level cell-state proportion。\n",
    "需要 05B group table 中存在 n_cells / cell_count / group_n_cells 等列。"
  )
}

column_audit <- data.table(
  role = c("dataset", "object", "group", "class", "cell_count"),
  detected_column = c(dataset_col, object_col, group_col, class_col, cell_count_col)
)

if (exists("class_score_table")) {
  class_score_audit_csv <- file.path(out_tables_dir, "09A_class_column_score_audit.csv")
  atomic_write_csv(as.data.frame(class_score_table), class_score_audit_csv)
}

atomic_write_csv(as.data.frame(column_audit), column_audit_csv)

stamp("Detected dataset column：", dataset_col)
stamp("Detected object column：", object_col)
stamp("Detected group column：", group_col)
stamp("Detected class column：", class_col)
stamp("Detected cell-count column：", cell_count_col)


# ============================================================
# 6. 标准化输入
# ============================================================

dt <- data.table(
  dataset = as.character(dt0[[dataset_col]]),
  object_id = as.character(dt0[[object_col]]),
  group_id = as.character(dt0[[group_col]]),
  original_class = as.character(dt0[[class_col]]),
  n_cells = safe_num(dt0[[cell_count_col]])
)

dt[is.na(dataset) | dataset == "", dataset := "unknown_dataset"]
dt[is.na(object_id) | object_id == "", object_id := dataset]
dt[is.na(group_id) | group_id == "", group_id := object_id]
dt[is.na(original_class) | original_class == "", original_class := "unknown_class"]

dt <- dt[!is.na(n_cells) & is.finite(n_cells) & n_cells > 0]

if (nrow(dt) == 0) {
  stop("标准化后没有 n_cells > 0 的 group rows。")
}

dt[, object_key := paste(dataset, object_id, sep = " | ")]
dt[, group_key := paste(dataset, object_id, group_id, sep = " | ")]
dt[, class_simple := clean_class_simple(original_class)]
dt[, class_simple := factor(class_simple, levels = class_order)]
dt[, class_label := class_labels[as.character(class_simple)]]

# audit original class -> simplified class
class_mapping <- dt[, .(
  n_group_rows = .N,
  total_cells = sum(n_cells, na.rm = TRUE)
), by = .(original_class, class_simple, class_label)][order(class_simple, -total_cells)]

atomic_write_csv(as.data.frame(dt), standardized_input_csv)
atomic_write_csv(as.data.frame(class_mapping), class_mapping_audit_csv)

stamp("Standardized valid group rows：", nrow(dt))
stamp("Total cells represented：", sum(dt$n_cells, na.rm = TRUE))
stamp("Objects represented：", uniqueN(dt$object_key))
stamp("Datasets represented：", uniqueN(dt$dataset))


# ============================================================
# 7. Dataset-level and object-level fractions
# ============================================================

dataset_total <- dt[, .(
  dataset_total_cells = sum(n_cells, na.rm = TRUE),
  dataset_group_rows = .N,
  dataset_objects = uniqueN(object_key)
), by = dataset]

dataset_class <- dt[, .(
  class_cells = sum(n_cells, na.rm = TRUE),
  class_group_rows = .N
), by = .(dataset, class_simple, class_label)]

dataset_class <- merge(dataset_class, dataset_total, by = "dataset", all.x = TRUE)
dataset_class[, class_fraction := class_cells / dataset_total_cells]
dataset_class[, class_simple := factor(as.character(class_simple), levels = class_order)]
setorder(dataset_class, dataset, class_simple)

object_total <- dt[, .(
  object_total_cells = sum(n_cells, na.rm = TRUE),
  object_group_rows = .N
), by = .(dataset, object_id, object_key)]

object_class <- dt[, .(
  class_cells = sum(n_cells, na.rm = TRUE),
  class_group_rows = .N
), by = .(dataset, object_id, object_key, class_simple, class_label)]

object_class <- merge(object_class, object_total, by = c("dataset", "object_id", "object_key"), all.x = TRUE)
object_class[, class_fraction := class_cells / object_total_cells]
object_class[, class_simple := factor(as.character(class_simple), levels = class_order)]
setorder(object_class, dataset, object_id, class_simple)

# fill missing class combinations with zero for robust plotting/table
all_dataset_grid <- CJ(
  dataset = unique(dataset_class$dataset),
  class_simple = factor(class_order, levels = class_order),
  unique = TRUE
)
all_dataset_grid[, class_label := class_labels[as.character(class_simple)]]

dataset_class <- merge(
  all_dataset_grid,
  dataset_class,
  by = c("dataset", "class_simple", "class_label"),
  all.x = TRUE
)

dataset_class[is.na(class_cells), class_cells := 0]
dataset_class[is.na(class_group_rows), class_group_rows := 0]
dataset_class <- merge(dataset_class, dataset_total, by = "dataset", all.x = TRUE, suffixes = c("", "_totalcopy"))

# V5：补齐 absent class 组合的 total columns。
# 对不存在的 class，class_cells = 0，但 total_cells 必须继承对应 dataset 的总细胞数。
if ("dataset_total_cells_totalcopy" %in% names(dataset_class)) {
  dataset_class[is.na(dataset_total_cells), dataset_total_cells := dataset_total_cells_totalcopy]
}
if ("dataset_group_rows_totalcopy" %in% names(dataset_class)) {
  dataset_class[is.na(dataset_group_rows), dataset_group_rows := 0]
}
if ("dataset_objects_totalcopy" %in% names(dataset_class)) {
  dataset_class[is.na(dataset_objects), dataset_objects := dataset_objects_totalcopy]
}

dataset_class[is.na(class_cells), class_cells := 0]
dataset_class[is.na(class_group_rows), class_group_rows := 0]

dataset_class[, class_fraction := fifelse(
  !is.na(dataset_total_cells) & dataset_total_cells > 0,
  class_cells / dataset_total_cells,
  NA_real_
)]

dataset_class[, class_simple := factor(as.character(class_simple), levels = class_order)]

all_object_grid <- CJ(
  object_key = unique(object_class$object_key),
  class_simple = factor(class_order, levels = class_order),
  unique = TRUE
)

object_key_meta <- unique(object_class[, .(object_key, dataset, object_id)])
all_object_grid <- merge(all_object_grid, object_key_meta, by = "object_key", all.x = TRUE)
all_object_grid[, class_label := class_labels[as.character(class_simple)]]

object_class <- merge(
  all_object_grid,
  object_class,
  by = c("object_key", "dataset", "object_id", "class_simple", "class_label"),
  all.x = TRUE
)

object_class[is.na(class_cells), class_cells := 0]
object_class[is.na(class_group_rows), class_group_rows := 0]
object_class <- merge(object_class, object_total, by = c("dataset", "object_id", "object_key"), all.x = TRUE, suffixes = c("", "_totalcopy"))

# V5：补齐 absent class 组合的 object total columns。
if ("object_total_cells_totalcopy" %in% names(object_class)) {
  object_class[is.na(object_total_cells), object_total_cells := object_total_cells_totalcopy]
}
if ("object_group_rows_totalcopy" %in% names(object_class)) {
  object_class[is.na(object_group_rows), object_group_rows := 0]
}

object_class[is.na(class_cells), class_cells := 0]
object_class[is.na(class_group_rows), class_group_rows := 0]

object_class[, class_fraction := fifelse(
  !is.na(object_total_cells) & object_total_cells > 0,
  class_cells / object_total_cells,
  NA_real_
)]

object_class[, class_simple := factor(as.character(class_simple), levels = class_order)]

# object priority summary
object_wide <- dcast(
  object_class,
  dataset + object_id + object_key + object_total_cells + object_group_rows ~ class_simple,
  value.var = "class_fraction",
  fill = 0
)

for (cc in class_order) {
  if (!cc %in% names(object_wide)) object_wide[, (cc) := 0]
}

object_wide[, ideal_fraction := ideal_like]
object_wide[, lower_priority_fraction := lower_priority_mixed]
object_wide[, safety_risk_fraction := safety_risk_high]
object_wide[, other_fraction := other_or_unclassified]
object_wide[, favorable_minus_safety_index := ideal_fraction - safety_risk_fraction]
object_wide[, favorable_minus_lower_priority_index := ideal_fraction - lower_priority_fraction]

object_wide[, predominant_class := class_order[
  max.col(as.matrix(.SD), ties.method = "first")
], .SDcols = class_order]

object_wide[, predominant_class_label := class_labels[predominant_class]]

object_wide <- object_wide[object_total_cells >= MIN_TOTAL_CELLS_FOR_OBJECT_SUMMARY]
setorder(object_wide, -favorable_minus_safety_index, safety_risk_fraction)

# dataset priority summary
dataset_wide <- dcast(
  dataset_class,
  dataset + dataset_total_cells + dataset_group_rows + dataset_objects ~ class_simple,
  value.var = "class_fraction",
  fill = 0
)

for (cc in class_order) {
  if (!cc %in% names(dataset_wide)) dataset_wide[, (cc) := 0]
}

dataset_wide[, ideal_fraction := ideal_like]
dataset_wide[, lower_priority_fraction := lower_priority_mixed]
dataset_wide[, safety_risk_fraction := safety_risk_high]
dataset_wide[, other_fraction := other_or_unclassified]
dataset_wide[, favorable_minus_safety_index := ideal_fraction - safety_risk_fraction]
dataset_wide[, favorable_minus_lower_priority_index := ideal_fraction - lower_priority_fraction]

dataset_wide[, predominant_class := class_order[
  max.col(as.matrix(.SD), ties.method = "first")
], .SDcols = class_order]

dataset_wide[, predominant_class_label := class_labels[predominant_class]]
setorder(dataset_wide, -favorable_minus_safety_index, safety_risk_fraction)

# V3 plot-value audit：检查 fraction / index 是否存在 NA 或超范围。
plot_value_audit_csv <- file.path(out_tables_dir, "09A_plot_value_audit.csv")

plot_value_audit <- data.table(
  metric = c(
    "dataset_class_fraction_NA",
    "dataset_class_fraction_outside_0_1",
    "object_class_fraction_NA",
    "object_class_fraction_outside_0_1",
    "dataset_priority_index_NA",
    "object_ideal_fraction_outside_0_1",
    "object_safety_fraction_outside_0_1"
  ),
  n = c(
    sum(is.na(dataset_class$class_fraction)),
    sum(!is.na(dataset_class$class_fraction) & (dataset_class$class_fraction < -1e-9 | dataset_class$class_fraction > 1 + 1e-9)),
    sum(is.na(object_class$class_fraction)),
    sum(!is.na(object_class$class_fraction) & (object_class$class_fraction < -1e-9 | object_class$class_fraction > 1 + 1e-9)),
    sum(is.na(dataset_wide$favorable_minus_safety_index)),
    sum(!is.na(object_wide$ideal_fraction) & (object_wide$ideal_fraction < -1e-9 | object_wide$ideal_fraction > 1 + 1e-9)),
    sum(!is.na(object_wide$safety_risk_fraction) & (object_wide$safety_risk_fraction < -1e-9 | object_wide$safety_risk_fraction > 1 + 1e-9))
  )
)

atomic_write_csv(as.data.frame(plot_value_audit), plot_value_audit_csv)

atomic_write_csv(as.data.frame(dataset_class), dataset_class_fraction_csv)
atomic_write_csv(as.data.frame(object_class), object_class_fraction_csv)
atomic_write_csv(as.data.frame(object_wide), object_priority_summary_csv)
atomic_write_csv(as.data.frame(dataset_wide), dataset_priority_summary_csv)

stamp("Dataset class fraction rows：", nrow(dataset_class))
stamp("Object class fraction rows：", nrow(object_class))



# ============================================================
# 7B. V4 plot-ready strict preflight
# ============================================================

# class columns must be clean before plotting
dataset_class[, class_simple_chr := as.character(class_simple)]
dataset_class[is.na(class_simple_chr) | class_simple_chr == "", class_simple_chr := "other_or_unclassified"]
dataset_class[, class_simple := factor(class_simple_chr, levels = class_order)]
dataset_class[, class_label := class_labels[class_simple_chr]]
dataset_class[is.na(class_label), class_label := class_labels["other_or_unclassified"]]

object_class[, class_simple_chr := as.character(class_simple)]
object_class[is.na(class_simple_chr) | class_simple_chr == "", class_simple_chr := "other_or_unclassified"]
object_class[, class_simple := factor(class_simple_chr, levels = class_order)]
object_class[, class_label := class_labels[class_simple_chr]]
object_class[is.na(class_label), class_label := class_labels["other_or_unclassified"]]

# numeric validity
dataset_class[, class_fraction := safe_num(class_fraction)]
object_class[, class_fraction := safe_num(class_fraction)]

plot_strict_audit_csv <- file.path(out_tables_dir, "09A_plot_strict_preflight_audit.csv")

plot_strict_audit <- data.table(
  check = c(
    "dataset_class_fraction_NA",
    "dataset_class_fraction_outside_0_1",
    "dataset_class_simple_NA",
    "dataset_class_label_NA",
    "object_class_fraction_NA",
    "object_class_fraction_outside_0_1",
    "object_class_simple_NA",
    "object_class_label_NA",
    "dataset_priority_index_NA",
    "dataset_priority_index_nonfinite",
    "object_ideal_fraction_NA",
    "object_safety_fraction_NA",
    "object_ideal_fraction_outside_0_1",
    "object_safety_fraction_outside_0_1"
  ),
  n = c(
    sum(is.na(dataset_class$class_fraction)),
    sum(!is.na(dataset_class$class_fraction) & (dataset_class$class_fraction < -1e-8 | dataset_class$class_fraction > 1 + 1e-8)),
    sum(is.na(dataset_class$class_simple)),
    sum(is.na(dataset_class$class_label)),
    sum(is.na(object_class$class_fraction)),
    sum(!is.na(object_class$class_fraction) & (object_class$class_fraction < -1e-8 | object_class$class_fraction > 1 + 1e-8)),
    sum(is.na(object_class$class_simple)),
    sum(is.na(object_class$class_label)),
    sum(is.na(dataset_wide$favorable_minus_safety_index)),
    sum(!is.finite(dataset_wide$favorable_minus_safety_index)),
    sum(is.na(object_wide$ideal_fraction)),
    sum(is.na(object_wide$safety_risk_fraction)),
    sum(!is.na(object_wide$ideal_fraction) & (object_wide$ideal_fraction < -1e-8 | object_wide$ideal_fraction > 1 + 1e-8)),
    sum(!is.na(object_wide$safety_risk_fraction) & (object_wide$safety_risk_fraction < -1e-8 | object_wide$safety_risk_fraction > 1 + 1e-8))
  )
)

# V5：额外检查每个 dataset/object 的 class_fraction 加和是否约等于 1。
dataset_fraction_sum_audit <- dataset_class[, .(
  fraction_sum = sum(class_fraction, na.rm = TRUE),
  n_class = .N
), by = dataset]

object_fraction_sum_audit <- object_class[, .(
  fraction_sum = sum(class_fraction, na.rm = TRUE),
  n_class = .N
), by = object_key]

fraction_sum_audit_csv <- file.path(out_tables_dir, "09A_fraction_sum_audit.csv")

fraction_sum_audit <- rbindlist(list(
  dataset_fraction_sum_audit[, .(level = "dataset", id = dataset, fraction_sum, n_class)],
  object_fraction_sum_audit[, .(level = "object", id = object_key, fraction_sum, n_class)]
), fill = TRUE)

fraction_sum_audit[, deviation_from_1 := abs(fraction_sum - 1)]

atomic_write_csv(as.data.frame(fraction_sum_audit), fraction_sum_audit_csv)
atomic_write_csv(as.data.frame(plot_strict_audit), plot_strict_audit_csv)

# fraction sums should be approximately 1 for every dataset/object.
bad_fraction_sums <- fraction_sum_audit[!is.finite(fraction_sum) | deviation_from_1 > 1e-6]
if (nrow(bad_fraction_sums) > 0) {
  print(head(bad_fraction_sums, 30))
  stop("09A V5 fraction-sum audit 未通过：某些 dataset/object 的 class fractions 加和不等于 1。")
}

bad_plot_checks <- plot_strict_audit[n > 0]
if (nrow(bad_plot_checks) > 0) {
  print(bad_plot_checks)
  stop("09A V4 plot-ready preflight 未通过。存在 NA / 非有限值 / 超范围 fraction，停止作图，避免 ggplot 删除行。")
}

# plot-ready copies
dataset_class_plot <- copy(dataset_class)
dataset_class_plot[, class_fraction_plot := pmin(pmax(class_fraction, 0), 1)]
dataset_class_plot[, dataset_factor := factor(
  dataset,
  levels = unique(dataset_class_plot[order(dataset_total_cells)]$dataset)
)]

object_class_plot <- copy(object_class)
object_class_plot[, class_fraction_plot := pmin(pmax(class_fraction, 0), 1)]

object_wide_plot <- copy(object_wide)
object_wide_plot <- object_wide_plot[
  is.finite(ideal_fraction) &
    is.finite(safety_risk_fraction) &
    ideal_fraction >= 0 & ideal_fraction <= 1 &
    safety_risk_fraction >= 0 & safety_risk_fraction <= 1 &
    is.finite(object_total_cells)
]

dataset_wide_plot <- copy(dataset_wide)
dataset_wide_plot <- dataset_wide_plot[
  is.finite(favorable_minus_safety_index)
]

# These plot-ready tables are not statistical recalculations; they are exact validated plotting inputs.
plot_ready_dataset_class_csv <- file.path(out_tables_dir, "09A_plot_ready_dataset_class_fraction.csv")
plot_ready_object_class_csv <- file.path(out_tables_dir, "09A_plot_ready_object_class_fraction.csv")
plot_ready_dataset_priority_csv <- file.path(out_tables_dir, "09A_plot_ready_dataset_priority_summary.csv")
plot_ready_object_priority_csv <- file.path(out_tables_dir, "09A_plot_ready_object_priority_summary.csv")

atomic_write_csv(as.data.frame(dataset_class_plot), plot_ready_dataset_class_csv)
atomic_write_csv(as.data.frame(object_class_plot), plot_ready_object_class_csv)
atomic_write_csv(as.data.frame(dataset_wide_plot), plot_ready_dataset_priority_csv)
atomic_write_csv(as.data.frame(object_wide_plot), plot_ready_object_priority_csv)



# ============================================================
# 8. Figures
# ============================================================

stamp("生成 09A scRNA cell-state proportion PDF figures。")

# Dataset stacked bar
p_dataset_stack <- ggplot(
  dataset_class_plot,
  aes(x = dataset_factor, y = class_fraction_plot, fill = class_simple)
) +
  geom_col(color = "grey25", linewidth = 0.18, width = 0.78) +
  scale_fill_manual(
    values = class_colors,
    breaks = class_order,
    labels = class_labels,
    name = "Cell-state class"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = c("0", "0.25", "0.50", "0.75", "1.00"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  labs(
    title = "Dataset-level cell-state composition",
    x = "Dataset",
    y = "Cell fraction"
  ) +
  theme_pub +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Object stacked bar
object_plot_dt <- copy(object_class_plot)
object_order <- unique(object_wide_plot[order(-favorable_minus_safety_index, safety_risk_fraction)]$object_key)
object_plot_dt[, object_key_factor := factor(object_key, levels = object_order)]

p_object_stack <- ggplot(
  object_plot_dt,
  aes(x = object_key_factor, y = class_fraction_plot, fill = class_simple)
) +
  geom_col(color = "grey25", linewidth = 0.12, width = 0.78) +
  scale_fill_manual(
    values = class_colors,
    breaks = class_order,
    labels = class_labels,
    name = "Cell-state class"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = c("0", "0.25", "0.50", "0.75", "1.00"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  labs(
    title = "Object-level cell-state composition",
    x = "Object",
    y = "Cell fraction"
  ) +
  theme_pub +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Dataset priority index
dataset_wide_plot[, priority_direction := ifelse(
  favorable_minus_safety_index >= 0,
  "favorable_index_positive",
  "favorable_index_negative"
)]

p_dataset_priority <- ggplot(
  dataset_wide_plot,
  aes(x = reorder(dataset, favorable_minus_safety_index), y = favorable_minus_safety_index, fill = priority_direction)
) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.55) +
  geom_col(color = "grey25", linewidth = 0.20, width = 0.72) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      favorable_index_positive = "#D73027",
      favorable_index_negative = "#1A9850"
    ),
    labels = c(
      favorable_index_positive = "Ideal-like > safety-risk",
      favorable_index_negative = "Safety-risk > ideal-like"
    ),
    name = "Index direction"
  ) +
  labs(
    title = "Dataset-level favorable-minus-safety index",
    x = "Dataset",
    y = "Ideal-like fraction - safety-risk-high fraction"
  ) +
  theme_pub

# Object scatter
p_object_scatter <- ggplot(
  object_wide_plot,
  aes(x = ideal_fraction, y = safety_risk_fraction)
) +
  geom_abline(slope = 1, intercept = 0, color = "grey45", linetype = "dashed", linewidth = 0.55) +
  geom_point(
    aes(size = object_total_cells, shape = predominant_class_label),
    alpha = 0.78,
    color = "black"
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.25), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), expand = expansion(mult = c(0.02, 0.02))) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
  scale_size_continuous(name = "Cells", range = c(2.4, 8.0)) +
  labs(
    title = "Object-level ideal-like versus safety-risk fraction",
    x = "Ideal-like cell fraction",
    y = "Safety-risk-high cell fraction",
    shape = "Predominant class"
  ) +
  theme_pub

# Dataset heatmap
heatmap_dt <- copy(dataset_class_plot)
dataset_order_levels <- unique(dataset_wide_plot[order(-favorable_minus_safety_index)]$dataset)
class_label_levels <- unique(unname(class_labels[class_order]))
heatmap_dt[, dataset_ordered := factor(dataset, levels = dataset_order_levels)]
heatmap_dt[, class_label_factor := factor(class_label, levels = class_label_levels)]

p_heatmap <- ggplot(
  heatmap_dt,
  aes(x = class_label_factor, y = dataset_ordered, fill = class_fraction_plot)
) +
  geom_tile(color = "white", linewidth = 0.8) +
  scale_fill_gradient(low = "white", high = "black", name = "Cell fraction") +
  labs(
    title = "Dataset-level cell-state fraction heatmap",
    x = "Cell-state class",
    y = "Dataset"
  ) +
  theme_pub +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

save_pdf_plot(p_dataset_stack, fig_dataset_stacked_pdf, width = PDF_WIDTH_WIDE, height = PDF_HEIGHT_MEDIUM)
save_pdf_plot(p_object_stack, fig_object_stacked_pdf, width = PDF_WIDTH_WIDE, height = PDF_HEIGHT_MEDIUM)
save_pdf_plot(p_dataset_priority, fig_dataset_priority_pdf, width = PDF_WIDTH_SQUARE, height = PDF_HEIGHT_SQUARE)
save_pdf_plot(p_object_scatter, fig_object_scatter_pdf, width = PDF_WIDTH_SQUARE, height = PDF_HEIGHT_SQUARE)
save_pdf_plot(p_heatmap, fig_heatmap_pdf, width = PDF_WIDTH_SQUARE, height = PDF_HEIGHT_SQUARE)


# ============================================================
# 9. Method note / report / verification
# ============================================================

figure_index <- data.table(
  figure_id = c(
    "dataset_cell_state_composition",
    "object_cell_state_composition",
    "dataset_priority_index",
    "object_ideal_vs_safety_fraction",
    "dataset_class_fraction_heatmap"
  ),
  title = c(
    "Dataset-level cell-state composition",
    "Object-level cell-state composition",
    "Dataset-level favorable-minus-safety index",
    "Object-level ideal-like versus safety-risk fraction",
    "Dataset-level cell-state fraction heatmap"
  ),
  pdf_path = c(
    fig_dataset_stacked_pdf,
    fig_object_stacked_pdf,
    fig_dataset_priority_pdf,
    fig_object_scatter_pdf,
    fig_heatmap_pdf
  ),
  source_table = c(
    dataset_class_fraction_csv,
    object_class_fraction_csv,
    dataset_priority_summary_csv,
    object_priority_summary_csv,
    dataset_class_fraction_csv
  ),
  pdf_size_bytes = c(
    file.info(fig_dataset_stacked_pdf)$size,
    file.info(fig_object_stacked_pdf)$size,
    file.info(fig_dataset_priority_pdf)$size,
    file.info(fig_object_scatter_pdf)$size,
    file.info(fig_heatmap_pdf)$size
  ),
  plot_engine = "ggplot2",
  figure_role = c(
    "main_or_supplement",
    "supplement",
    "main_candidate",
    "main_or_supplement",
    "supplement"
  )
)

atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

method_lines <- c(
  "09A scRNA cell-state proportion FINAL V6 method and claim-boundary note",
  "",
  "Method-ready wording:",
  paste0(
    "Cell-state composition analysis was performed using the 05B DA/projection-vs-safety group-level classification table. ",
    "For each dataset and object, cell-state fractions were calculated using group-level cell counts rather than unweighted group counts. ",
    "Cell-state classes were harmonized into ideal-like, lower-priority/mixed, safety-risk-high and other/unclassified categories. ",
    "A favorable-minus-safety index was defined as the ideal-like cell fraction minus the safety-risk-high cell fraction. ",
    "All proportion estimates were based on the full 05B group-level table and were not derived from downsampled 03B objects."
  ),
  "",
  "Detected input columns:",
  paste0("dataset column: ", dataset_col),
  paste0("object column: ", object_col),
  paste0("group column: ", group_col),
  paste0("class column: ", class_col),
  paste0("cell-count column: ", cell_count_col),
  "",
  "Claim boundary:",
  "09A supports transcriptomic cell-state composition analysis. It does not prove anatomical projection, functional integration, treatment efficacy, tumorigenicity, or clinical safety."
)

writeLines(method_lines, method_note_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

report_lines <- c(
  "09A scRNA cell-state proportion FINAL V6 report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Input:",
  INPUT_05B_GROUPS,
  "",
  "Detected columns:",
  capture.output(print(column_audit)),
  "",
  "Data summary:",
  paste0("Valid group rows: ", nrow(dt)),
  paste0("Total cells represented: ", sum(dt$n_cells, na.rm = TRUE)),
  paste0("Datasets represented: ", uniqueN(dt$dataset)),
  paste0("Objects represented: ", uniqueN(dt$object_key)),
  "",
  "Class mapping:",
  capture.output(print(class_mapping)),
  "",
  "Top datasets by favorable-minus-safety index:",
  capture.output(print(head(dataset_wide, 20))),
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
  standardized_input_csv,
  column_audit_csv,
  class_mapping_audit_csv,
  dataset_class_fraction_csv,
  object_class_fraction_csv,
  object_priority_summary_csv,
  dataset_priority_summary_csv,
  plot_value_audit_csv,
  plot_strict_audit_csv,
  fraction_sum_audit_csv,
  plot_ready_dataset_class_csv,
  plot_ready_object_class_csv,
  plot_ready_dataset_priority_csv,
  plot_ready_object_priority_csv,
  figure_index_csv,
  method_note_txt,
  report_txt,
  session_info_txt,
  fig_dataset_stacked_pdf,
  fig_object_stacked_pdf,
  fig_dataset_priority_pdf,
  fig_object_scatter_pdf,
  fig_heatmap_pdf
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
  stop("09A scRNA cell-state proportion FINAL V6 未通过输出验证。")
}


# ============================================================
# 10. 完成
# ============================================================

cat("\n============================================================\n")
cat("09A scRNA cell-state proportion FINAL V6 运行结束\n")
cat("============================================================\n\n")

cat("Input rows：", nrow(dt0), "\n")
cat("Valid group rows：", nrow(dt), "\n")
cat("Total cells represented：", sum(dt$n_cells, na.rm = TRUE), "\n")
cat("Datasets represented：", uniqueN(dt$dataset), "\n")
cat("Objects represented：", uniqueN(dt$object_key), "\n\n")

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("主要 PDF 图：\n")
cat(fig_dataset_stacked_pdf, "\n")
cat(fig_object_stacked_pdf, "\n")
cat(fig_dataset_priority_pdf, "\n")
cat(fig_object_scatter_pdf, "\n")
cat(fig_heatmap_pdf, "\n\n")

cat("关键表格：\n")
cat(dataset_class_fraction_csv, "\n")
cat(object_class_fraction_csv, "\n")
cat(object_priority_summary_csv, "\n")
cat(dataset_priority_summary_csv, "\n")
cat(plot_value_audit_csv, "\n")
cat(plot_strict_audit_csv, "\n")
cat(fraction_sum_audit_csv, "\n")
cat(method_note_txt, "\n\n")

cat("✅ 09A scRNA cell-state proportion FINAL V6 完成。\n")
