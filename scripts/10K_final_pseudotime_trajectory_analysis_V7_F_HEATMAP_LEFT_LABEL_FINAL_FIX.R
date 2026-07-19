
cat("\n[10K V7] Starting figure export fix for completed 10K V4 analysis...\n")

PROJECT_ROOT <- "D:/PD_Graft_Project"
V4_TAG <- "10K_final_pseudotime_trajectory_analysis_V4_MATRIX_SAFE_NO_SEURAT_SUBSET"
SCRIPT_TAG <- "10K_final_pseudotime_trajectory_analysis_V7_F_HEATMAP_LEFT_LABEL_FINAL_FIX"

IN_TABLE_DIR <- file.path(PROJECT_ROOT, "03_tables", V4_TAG)
OUT_TABLE_DIR <- file.path(PROJECT_ROOT, "03_tables", SCRIPT_TAG)
OUT_FIG_DIR <- file.path(PROJECT_ROOT, "04_figures", paste0(SCRIPT_TAG, "_pdf"))
OUT_TEXT_DIR <- file.path(PROJECT_ROOT, "09_manuscript", SCRIPT_TAG)

dir.create(OUT_TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TEXT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("[10K V7] Input V4 table dir:", IN_TABLE_DIR, "\n")
cat("[10K V7] Output figure dir:", OUT_FIG_DIR, "\n")
cat("[10K V7] Output text dir  :", OUT_TEXT_DIR, "\n")
cat("[10K V7] Mode             : figure-only recovery; no analysis recomputation\n")

required_files <- c(
  cell = file.path(IN_TABLE_DIR, "10K_V4_cell_pseudotime_and_scores.csv"),
  time_summary = file.path(IN_TABLE_DIR, "10K_V4_timepoint_pseudotime_summary.csv"),
  cluster_summary = file.path(IN_TABLE_DIR, "10K_V4_cluster_pseudotime_summary.csv"),
  program_binned = file.path(IN_TABLE_DIR, "10K_V4_program_score_binned_summary.csv"),
  priority_binned = file.path(IN_TABLE_DIR, "10K_V4_priority_proxy_binned_summary.csv"),
  heatmap = file.path(IN_TABLE_DIR, "10K_V4_marker_trend_heatmap_zscore_matrix.csv"),
  execution = file.path(IN_TABLE_DIR, "10K_V4_execution_summary.csv")
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("[10K V7] Missing required V4 table(s):\n", paste(names(missing_files), missing_files, sep = " = ", collapse = "\n"))
}

close_devices <- function() {
  while (grDevices::dev.cur() > 1) {
    try(grDevices::dev.off(), silent = TRUE)
  }
}

open_pdf_safe <- function(path, width = 5.4, height = 4.6) {
  close_devices()
  grDevices::pdf(file = path, width = width, height = height, useDingbats = FALSE, onefile = FALSE, family = "sans")
}

safe_dev_off <- function() {
  if (grDevices::dev.cur() > 1) try(grDevices::dev.off(), silent = TRUE)
}

write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "")
  cat("[10K V7] Wrote:", path, "\n")
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

safe_range <- function(x, pad_fraction = 0.08, fallback = c(-1, 1)) {
  x <- safe_num(x)
  x <- x[is.finite(x)]
  if (length(x) < 1) return(fallback)
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || diff(r) == 0) return(r + c(-1, 1))
  pad <- diff(r) * pad_fraction
  r + c(-pad, pad)
}

plot_dotrange <- function(summary_df, xlab, ylab, title, ordered_groups = NULL, y_lim = c(0, 1)) {
  summary_df$group <- as.character(summary_df$group)
  if (!is.null(ordered_groups)) {
    summary_df$group <- factor(summary_df$group, levels = ordered_groups)
    summary_df <- summary_df[order(summary_df$group), , drop = FALSE]
  }
  x <- seq_len(nrow(summary_df))
  plot(x, summary_df$median, ylim = y_lim, xlab = xlab, ylab = ylab, xaxt = "n",
       pch = 16, cex = 0.85, main = title, bty = "l", las = 1)
  grid_y <- pretty(y_lim, n = 5)
  abline(h = grid_y, col = "grey92", lwd = 0.7)
  segments(x, summary_df$q10, x, summary_df$q90, col = "grey65", lwd = 1.1)
  segments(x - 0.14, summary_df$q25, x + 0.14, summary_df$q25, col = "grey30", lwd = 1.1)
  segments(x - 0.14, summary_df$q75, x + 0.14, summary_df$q75, col = "grey30", lwd = 1.1)
  points(x, summary_df$median, pch = 16, cex = 0.85)
  axis(1, at = x, labels = as.character(summary_df$group), las = 2, cex.axis = 0.78)
}

plot_binned_line <- function(bs, xlab, ylab, title, ylim = NULL) {
  bs$x_mid <- safe_num(bs$x_mid); bs$q25 <- safe_num(bs$q25); bs$median <- safe_num(bs$median); bs$q75 <- safe_num(bs$q75)
  ok <- is.finite(bs$median)
  if (!any(ok)) {
    plot.new(); title(main = paste(title, "no finite bins")); return(invisible(NULL))
  }
  if (is.null(ylim)) ylim <- safe_range(c(bs$q25, bs$q75, bs$median), pad_fraction = 0.12)
  plot(bs$x_mid, bs$median, type = "b", pch = 16, lwd = 1.6, cex = 0.75,
       xlim = c(0, 1), ylim = ylim, xlab = xlab, ylab = ylab, main = title, bty = "l", las = 1)
  abline(h = 0, col = "grey90", lty = 2)
  segments(bs$x_mid, bs$q25, bs$x_mid, bs$q75, col = "grey50", lwd = 1.2)
}

cell_table <- utils::read.csv(required_files[["cell"]], stringsAsFactors = FALSE, check.names = FALSE)
time_summary <- utils::read.csv(required_files[["time_summary"]], stringsAsFactors = FALSE, check.names = FALSE)
cluster_summary <- utils::read.csv(required_files[["cluster_summary"]], stringsAsFactors = FALSE, check.names = FALSE)
program_binned <- utils::read.csv(required_files[["program_binned"]], stringsAsFactors = FALSE, check.names = FALSE)
priority_binned <- utils::read.csv(required_files[["priority_binned"]], stringsAsFactors = FALSE, check.names = FALSE)
execution <- utils::read.csv(required_files[["execution"]], stringsAsFactors = FALSE, check.names = FALSE)
heat_z <- utils::read.csv(required_files[["heatmap"]], row.names = 1, check.names = FALSE)
heat_z <- as.matrix(heat_z)
mode(heat_z) <- "numeric"

num_cols <- c("pseudotime", "day_numeric", "priority_proxy_z", "dim1", "dim2")
for (cc in intersect(num_cols, colnames(cell_table))) cell_table[[cc]] <- safe_num(cell_table[[cc]])
for (cc in intersect(c("q10", "q25", "median", "q75", "q90", "day_numeric"), colnames(time_summary))) time_summary[[cc]] <- safe_num(time_summary[[cc]])
for (cc in intersect(c("q10", "q25", "median", "q75", "q90"), colnames(cluster_summary))) cluster_summary[[cc]] <- safe_num(cluster_summary[[cc]])

rho_day <- if ("spearman_day" %in% colnames(execution)) safe_num(execution$spearman_day[1]) else NA_real_
rho_priority <- if ("spearman_priority_proxy" %in% colnames(execution)) safe_num(execution$spearman_priority_proxy[1]) else NA_real_
decision <- if ("decision" %in% colnames(execution)) as.character(execution$decision[1]) else "UNKNOWN"
embedding_source <- if ("embedding_source" %in% colnames(execution)) as.character(execution$embedding_source[1]) else "embedding"

write_csv(data.frame(
  source_v4_table_dir = IN_TABLE_DIR,
  figure_export_dir = OUT_FIG_DIR,
  spearman_day = rho_day,
  spearman_priority_proxy = rho_priority,
  decision = decision,
  embedding_source = embedding_source,
  stringsAsFactors = FALSE
), file.path(OUT_TABLE_DIR, "10K_V7_figure_export_summary.csv"))

known_time_order <- c("D8", "D14", "D21", "D28", "D35")
time_groups <- as.character(time_summary$group)
time_order <- known_time_order[known_time_order %in% time_groups]
if (length(time_order) == 0) time_order <- time_groups

cluster_order <- as.character(cluster_summary$group)

set.seed(20260716)
tp_levels <- unique(as.character(cell_table$timepoint))
tp_levels <- known_time_order[known_time_order %in% tp_levels]
if (length(tp_levels) == 0) tp_levels <- unique(as.character(cell_table$timepoint))
tp_cols <- grDevices::hcl.colors(length(tp_levels), palette = "Dark 3")
names(tp_cols) <- tp_levels
pt_pal <- grDevices::hcl.colors(101, palette = "Viridis")
pt <- safe_num(cell_table$pseudotime)
pt_idx <- pmax(1, pmin(101, round(pt * 100) + 1))

xlab_embed <- if (grepl("UMAP", embedding_source, ignore.case = TRUE)) "UMAP 1" else "Dimension 1"
ylab_embed <- if (grepl("UMAP", embedding_source, ignore.case = TRUE)) "UMAP 2" else "Dimension 2"

fig_A <- file.path(OUT_FIG_DIR, "10K_V7_A_embedding_by_timepoint.pdf")
tryCatch({
  open_pdf_safe(fig_A, 5.8, 5.0)
  par(mar = c(4.3, 4.4, 2.5, 1.0), mgp = c(2.4, 0.75, 0), tck = -0.015, las = 1, cex.main = 1.0)
  plot(cell_table$dim1, cell_table$dim2, pch = 16, cex = 0.23, col = tp_cols[as.character(cell_table$timepoint)],
       xlab = xlab_embed, ylab = ylab_embed, main = "GSE204796 time-course cell-state layout", bty = "l")
  legend("topright", legend = tp_levels, col = tp_cols[tp_levels], pch = 16, bty = "n", cex = 0.72, title = "Timepoint")
  safe_dev_off(); cat("[10K V7] Wrote figure:", fig_A, "\n")
}, error = function(e) { safe_dev_off(); cat("[10K V7] WARNING figure A failed:", conditionMessage(e), "\n") })

fig_B <- file.path(OUT_FIG_DIR, "10K_V7_B_embedding_pseudotime.pdf")
tryCatch({
  open_pdf_safe(fig_B, 5.8, 5.0)
  par(mar = c(4.3, 4.4, 2.5, 1.0), mgp = c(2.4, 0.75, 0), tck = -0.015, las = 1, cex.main = 1.0)
  plot(cell_table$dim1, cell_table$dim2, pch = 16, cex = 0.23, col = pt_pal[pt_idx],
       xlab = xlab_embed, ylab = ylab_embed, main = "Graph-based pseudotime", bty = "l")
  legend("topright", legend = c("0", "0.25", "0.50", "0.75", "1.00"),
         col = pt_pal[c(1,26,51,76,101)], pch = 16, bty = "n", cex = 0.72, title = "Pseudotime")
  safe_dev_off(); cat("[10K V7] Wrote figure:", fig_B, "\n")
}, error = function(e) { safe_dev_off(); cat("[10K V7] WARNING figure B failed:", conditionMessage(e), "\n") })

fig_C <- file.path(OUT_FIG_DIR, "10K_V7_C_pseudotime_by_timepoint_dotrange.pdf")
tryCatch({
  open_pdf_safe(fig_C, 5.2, 4.7)
  par(mar = c(4.8, 4.5, 2.4, 1.0), mgp = c(2.5, 0.8, 0), tck = -0.015, las = 1, cex.main = 1.0)
  plot_dotrange(time_summary, xlab = "Chronological timepoint", ylab = "Pseudotime", title = "Pseudotime ordering by timepoint", ordered_groups = time_order)
  safe_dev_off(); cat("[10K V7] Wrote figure:", fig_C, "\n")
}, error = function(e) { safe_dev_off(); cat("[10K V7] WARNING figure C failed:", conditionMessage(e), "\n") })

fig_D <- file.path(OUT_FIG_DIR, "10K_V7_D_program_trends_clean.pdf")
tryCatch({
  open_pdf_safe(fig_D, 8.8, 5.8)
  par(mfrow = c(2,2), mar = c(3.6, 4.0, 2.2, 1.0), mgp = c(2.2, 0.65, 0), tck = -0.015, las = 1, cex.main = 0.95)
  for (pn in c("DA maturation", "Neuronal maturation", "Progenitor/cell-cycle", "Stress/risk")) {
    bs <- program_binned[program_binned$program == pn, , drop = FALSE]
    plot_binned_line(bs, xlab = "Pseudotime", ylab = "Program score (z)", title = pn)
  }
  safe_dev_off(); cat("[10K V7] Wrote figure:", fig_D, "\n")
}, error = function(e) { safe_dev_off(); cat("[10K V7] WARNING figure D failed:", conditionMessage(e), "\n") })

fig_E <- file.path(OUT_FIG_DIR, "10K_V7_E_priority_proxy_clean.pdf")
tryCatch({
  open_pdf_safe(fig_E, 5.2, 4.6)
  par(mar = c(4.4, 4.6, 2.7, 1.0), mgp = c(2.6, 0.75, 0), tck = -0.015, las = 1, cex.main = 0.95)
  plot_binned_line(priority_binned, xlab = "Pseudotime", ylab = "Priority proxy (z)",
                   title = paste0("Priority proxy along pseudotime\nSpearman rho = ", round(rho_priority, 3)))
  safe_dev_off(); cat("[10K V7] Wrote figure:", fig_E, "\n")
}, error = function(e) { safe_dev_off(); cat("[10K V7] WARNING figure E failed:", conditionMessage(e), "\n") })

fig_F <- file.path(OUT_FIG_DIR, "10K_V7_F_marker_trend_heatmap_clean.pdf")
tryCatch({

  open_pdf_safe(fig_F, 6.8, 7.7)
  par(
    mar = c(4.8, 6.9, 2.55, 1.25),
    mgp = c(2.15, 0.55, 0),
    tck = -0.010,
    las = 1,
    cex.main = 0.90,
    xpd = FALSE
  )
  if (nrow(heat_z) < 2 || ncol(heat_z) < 2) stop("Too few marker/bin values for heatmap.")
  plot_mat <- heat_z[nrow(heat_z):1, , drop = FALSE]
  zlim <- c(-2.5, 2.5)
  plot_mat[plot_mat < zlim[1]] <- zlim[1]
  plot_mat[plot_mat > zlim[2]] <- zlim[2]
  pal <- grDevices::colorRampPalette(c("#2b6cb0", "white", "#b91c1c"))(101)

  image(
    x = seq_len(ncol(plot_mat)),
    y = seq_len(nrow(plot_mat)),
    z = t(plot_mat),
    col = pal,
    zlim = zlim,
    axes = FALSE,
    xlab = "Pseudotime bin",
    ylab = "",
    main = "Marker trends across pseudotime bins"
  )

  axis(1,
       at = seq_len(ncol(plot_mat)),
       labels = gsub("^B", "", colnames(plot_mat)),
       las = 1,
       cex.axis = 0.74,
       tick = TRUE,
       line = 0)

  axis(2,
       at = seq_len(nrow(plot_mat)),
       labels = rownames(plot_mat),
       las = 1,
       cex.axis = 0.46,
       tick = FALSE,
       line = 0.05)

  box(lwd = 0.75)
  mtext("z-score: blue low, white mid, red high", side = 3, line = 0.15, cex = 0.56)
  safe_dev_off(); cat("[10K V7] Wrote figure:", fig_F, "\n")
}, error = function(e) { safe_dev_off(); cat("[10K V7] WARNING figure F failed:", conditionMessage(e), "\n") })

fig_paths <- c(fig_A, fig_B, fig_C, fig_D, fig_E, fig_F)
fig_status <- data.frame(
  figure = basename(fig_paths),
  path = fig_paths,
  exists = file.exists(fig_paths),
  size_bytes = ifelse(file.exists(fig_paths), file.info(fig_paths)$size, NA_real_),
  stringsAsFactors = FALSE
)
write_csv(fig_status, file.path(OUT_TABLE_DIR, "10K_V7_figure_export_status.csv"))

report_lines <- c(
  "10K V7 figure export and marker-label fix report",
  paste0("Source V4 table directory: ", IN_TABLE_DIR),
  paste0("Output figure directory: ", OUT_FIG_DIR),
  paste0("Spearman pseudotime vs chronological day: ", round(rho_day, 4)),
  paste0("Spearman pseudotime vs priority proxy: ", round(rho_priority, 4)),
  paste0("V4 decision: ", decision),
  "",
  "This script only regenerated figures from completed 10K V4 tables; Figure F uses final fixed left marker/gene label spacing with no y-axis title overflow.",
  "It did not rerun matrix extraction, PCA, UMAP, kNN graph construction, or pseudotime.",
  "Allowed wording: graph-based transcriptomic pseudotime / time-course-associated cell-state ordering.",
  "Not allowed: true lineage tracing, real fate mapping, functional graft maturation, clinical prediction."
)
writeLines(report_lines, con = file.path(OUT_TEXT_DIR, "10K_V7_figure_export_fix_report.txt"))
writeLines(c(decision), con = file.path(OUT_TEXT_DIR, "10K_V7_decision_from_V4.txt"))

cat("\n[10K V7] Completed figure export fix from completed V4 tables.\n")
cat("[10K V7] Figures written:", sum(fig_status$exists & fig_status$size_bytes > 1000, na.rm = TRUE), "/", nrow(fig_status), "\n")
cat("[10K V7] Spearman day:", round(rho_day, 4), "\n")
cat("[10K V7] Spearman priority proxy:", round(rho_priority, 4), "\n")
cat("[10K V7] Decision inherited from V4:", decision, "\n")
cat("[10K V7] Output figures:", OUT_FIG_DIR, "\n")
cat("[10K V7] Next: inspect 10K V5 PDFs. If clean, lock 10K final = V4 analysis + V7 figure export heatmap left-label final fix.\n")

close_devices()
