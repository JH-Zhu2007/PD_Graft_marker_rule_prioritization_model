
options(stringsAsFactors = FALSE)
options(warn = 1)
set.seed(20260716)

MODULE_TAG <- "10J_pseudotime_pilot_V15_FINAL_SEPARATE_PUBLICATION_SAFE"
PROJECT_ROOT <- "D:/PD_Graft_Project"
INPUT_OBJECT <- file.path(
  PROJECT_ROOT,
  "02_objects/04D_annotated_objects/GSE204796/01A_GSE204796_GSM6194008_D8_04D_annotated.rds"
)
MAX_PILOT_CELLS <- 6000
KNN_K <- 18
ROOT_QUANTILE <- 0.03
N_BINS <- 10

OUT_TABLE_DIR <- file.path(PROJECT_ROOT, "03_tables", MODULE_TAG)
OUT_FIG_DIR   <- file.path(PROJECT_ROOT, "04_figures", paste0(MODULE_TAG, "_pdf"))
OUT_TEXT_DIR  <- file.path(PROJECT_ROOT, "09_manuscript", MODULE_TAG)
dir.create(OUT_TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TEXT_DIR, recursive = TRUE, showWarnings = FALSE)

while (grDevices::dev.cur() > 1) {
  try(grDevices::dev.off(), silent = TRUE)
}

cat("\n[10J V15] Starting FINAL separate publication-safe pseudotime pilot...\n")
cat("[10J V15] Project root      :", PROJECT_ROOT, "\n")
cat("[10J V15] Input object      :", INPUT_OBJECT, "\n")
cat("[10J V15] Output table dir  :", OUT_TABLE_DIR, "\n")
cat("[10J V15] Output figure dir :", OUT_FIG_DIR, "\n")
cat("[10J V15] Output text dir   :", OUT_TEXT_DIR, "\n")
cat("[10J V15] Plot engine       : base R only; separate safe PDF devices\n")

required_pkgs <- c("Seurat", "Matrix", "FNN", "igraph")
pkg_status <- data.frame(
  package = required_pkgs,
  available = vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE),
  stringsAsFactors = FALSE
)
write.csv(pkg_status, file.path(OUT_TABLE_DIR, "10J_V15_package_status.csv"), row.names = FALSE)
if (!all(pkg_status$available)) {
  stop("[10J V15] Missing required package(s): ", paste(pkg_status$package[!pkg_status$available], collapse = ", "))
}
suppressPackageStartupMessages({
  library(Seurat)
  library(Matrix)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

zsafe <- function(x) {
  x <- as.numeric(x)
  if (length(x) == 0L || all(is.na(x))) return(rep(NA_real_, length(x)))
  s <- stats::sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  (x - m) / s
}

winsor <- function(x, probs = c(0.01, 0.99)) {
  x <- as.numeric(x)
  q <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  x
}

match_genes <- function(requested, rn) {
  rn_upper <- toupper(rn)
  out <- character(0)
  for (g in requested) {
    hit <- which(rn_upper == toupper(g))
    if (length(hit) > 0) out <- c(out, rn[hit[1]])
  }
  unique(out)
}

safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 5) return(NA_real_)
  if (stats::sd(x[ok]) == 0 || stats::sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = "spearman"))
}

safe_pdf <- function(filename, width, height, plot_fun) {
  ok <- FALSE
  err <- NA_character_

  while (grDevices::dev.cur() > 1) {
    try(grDevices::dev.off(), silent = TRUE)
  }
  tryCatch({
    grDevices::pdf(filename, width = width, height = height, onefile = FALSE, useDingbats = FALSE)
    on.exit({
      if (grDevices::dev.cur() > 1) grDevices::dev.off()
    }, add = TRUE)
    graphics::par(family = "sans")
    plot_fun()
    ok <- TRUE
  }, error = function(e) {
    err <<- conditionMessage(e)
  })
  if (grDevices::dev.cur() > 1) {
    try(grDevices::dev.off(), silent = TRUE)
  }
  if (!ok) {
    cat("[10J V15] Figure failed:", filename, "|", err, "\n")
    writeLines(err, paste0(filename, ".ERROR.txt"))
  } else {
    cat("[10J V15] Wrote figure:", filename, "\n")
  }
  ok
}

get_assay_data_safe <- function(obj) {
  assay <- Seurat::DefaultAssay(obj)
  x <- tryCatch(
    Seurat::GetAssayData(obj, assay = assay, layer = "data"),
    error = function(e1) {
      tryCatch(
        Seurat::GetAssayData(obj, assay = assay, slot = "data"),
        error = function(e2) {
          tryCatch(
            Seurat::GetAssayData(obj, assay = assay, layer = "counts"),
            error = function(e3) Seurat::GetAssayData(obj, assay = assay, slot = "counts")
          )
        }
      )
    }
  )
  x
}

get_reduction_safe <- function(obj, candidates) {
  red_names <- names(obj@reductions)
  for (nm in candidates) {
    if (nm %in% red_names) {
      emb <- tryCatch(Seurat::Embeddings(obj, reduction = nm), error = function(e) NULL)
      if (!is.null(emb) && ncol(emb) >= 2) return(list(name = nm, emb = emb))
    }
  }
  NULL
}

summ_by_bin <- function(x, y, n_bins = N_BINS) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  bins <- cut(x, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE, labels = FALSE)
  do.call(rbind, lapply(seq_len(n_bins), function(b) {
    yy <- y[bins == b & is.finite(y)]
    mid <- (b - 0.5) / n_bins
    if (length(yy) == 0) {
      return(data.frame(bin = b, x_mid = mid, n = 0, q10 = NA, q25 = NA, median = NA, q75 = NA, q90 = NA))
    }
    data.frame(
      bin = b,
      x_mid = mid,
      n = length(yy),
      q10 = as.numeric(stats::quantile(yy, 0.10, na.rm = TRUE)),
      q25 = as.numeric(stats::quantile(yy, 0.25, na.rm = TRUE)),
      median = stats::median(yy, na.rm = TRUE),
      q75 = as.numeric(stats::quantile(yy, 0.75, na.rm = TRUE)),
      q90 = as.numeric(stats::quantile(yy, 0.90, na.rm = TRUE))
    )
  }))
}

if (!file.exists(INPUT_OBJECT)) stop("[10J V15] Input object does not exist: ", INPUT_OBJECT)
obj <- readRDS(INPUT_OBJECT)
if (!inherits(obj, "Seurat")) stop("[10J V15] Input object is not a Seurat object.")
cat("[10J V15] Loaded Seurat object cells:", ncol(obj), "genes:", nrow(obj), "\n")

umap_info <- get_reduction_safe(obj, c("umap", "UMAP", "wnn.umap", "ref.umap"))
pca_info  <- get_reduction_safe(obj, c("pca", "PCA", "harmony", "integrated.pca"))
if (is.null(umap_info)) stop("[10J V15] No usable UMAP reduction found.")
if (is.null(pca_info)) {
  warning("[10J V15] No PCA-like reduction found; using UMAP coordinates for graph pilot.")
  pca_info <- list(name = umap_info$name, emb = scale(umap_info$emb[, 1:2, drop = FALSE]))
}

cells_common <- Reduce(intersect, list(colnames(obj), rownames(umap_info$emb), rownames(pca_info$emb)))
if (length(cells_common) < 100) stop("[10J V15] Too few cells shared by object/reductions.")
if (length(cells_common) > MAX_PILOT_CELLS) {
  cells_use <- sample(cells_common, MAX_PILOT_CELLS)
} else {
  cells_use <- cells_common
}
cat("[10J V15] Pilot cells selected:", length(cells_use), "\n")

umap <- umap_info$emb[cells_use, 1:2, drop = FALSE]
graph_emb <- pca_info$emb[cells_use, , drop = FALSE]
graph_emb <- graph_emb[, seq_len(min(ncol(graph_emb), 30)), drop = FALSE]

meta <- obj@meta.data[cells_use, , drop = FALSE]
cluster_candidates <- c("seurat_clusters", "SCT_snn_res.0.8", "RNA_snn_res.0.8", "cluster", "clusters", "cell_state")
cluster_col <- cluster_candidates[cluster_candidates %in% colnames(meta)][1]
if (is.na(cluster_col) || is.null(cluster_col)) {
  cluster_col <- "pilot_cluster"
  meta[[cluster_col]] <- "0"
}
clusters <- as.factor(meta[[cluster_col]])
cat("[10J V15] Cluster column selected:", cluster_col, "\n")

expr <- get_assay_data_safe(obj)
expr <- expr[, cells_use, drop = FALSE]
rn <- rownames(expr)

program_genes <- list(
  DA_maturation = c("TH", "DDC", "SLC6A3", "SLC18A2", "NR4A2", "PITX3", "FOXA2", "LMX1A", "LMX1B", "EN1", "EN2", "ALDH1A1", "KCNJ6"),
  Neuronal_maturation = c("MAP2", "RBFOX3", "TUBB3", "SNAP25", "SYN1", "SYT1", "NEFL", "NEFM", "STMN2", "GAP43", "DCX"),
  Progenitor_cell_cycle = c("SOX2", "SOX1", "NES", "PAX6", "VIM", "HES1", "ASCL1", "MKI67", "TOP2A", "PCNA", "HMGB2"),
  Stress_risk = c("JUN", "FOS", "HSPA1A", "HSPA1B", "HSPB1", "DDIT3", "ATF3", "HMOX1", "HIF1A", "TXNIP", "DNAJB1", "HSP90AA1")
)

program_presence <- do.call(rbind, lapply(names(program_genes), function(nm) {
  matched <- match_genes(program_genes[[nm]], rn)
  data.frame(
    program = nm,
    requested_n = length(program_genes[[nm]]),
    matched_n = length(matched),
    matched_genes = paste(matched, collapse = ";"),
    stringsAsFactors = FALSE
  )
}))
write.csv(program_presence, file.path(OUT_TABLE_DIR, "10J_V15_program_gene_presence.csv"), row.names = FALSE)
cat("[10J V15] Wrote:", file.path(OUT_TABLE_DIR, "10J_V15_program_gene_presence.csv"), "\n")

score_program <- function(genes) {
  matched <- match_genes(genes, rn)
  if (length(matched) == 0) return(rep(NA_real_, length(cells_use)))
  vals <- Matrix::colMeans(expr[matched, , drop = FALSE])
  zsafe(vals)
}

score_DA <- score_program(program_genes$DA_maturation)
score_neur <- score_program(program_genes$Neuronal_maturation)
score_prog <- score_program(program_genes$Progenitor_cell_cycle)
score_stress <- score_program(program_genes$Stress_risk)
priority_proxy_raw <- score_DA + score_neur - score_prog - score_stress
priority_proxy <- zsafe(priority_proxy_raw)
if (all(is.na(priority_proxy)) || stats::sd(priority_proxy, na.rm = TRUE) == 0) {
  warning("[10J V15] Priority proxy was non-informative; falling back to negative progenitor/stress proxy.")
  priority_proxy <- zsafe(-score_prog - score_stress)
}

root_threshold <- stats::quantile(priority_proxy, ROOT_QUANTILE, na.rm = TRUE, names = FALSE)
root_idx <- which(priority_proxy <= root_threshold & is.finite(priority_proxy))
if (length(root_idx) < 10) {
  fallback_root_score <- zsafe(score_prog + score_stress)
  root_threshold <- stats::quantile(fallback_root_score, 1 - ROOT_QUANTILE, na.rm = TRUE, names = FALSE)
  root_idx <- which(fallback_root_score >= root_threshold & is.finite(fallback_root_score))
}
if (length(root_idx) < 1) root_idx <- seq_len(min(20, length(cells_use)))
cat("[10J V15] Root cells selected:", length(root_idx), "\n")

knn <- FNN::get.knn(as.matrix(graph_emb), k = min(KNN_K, nrow(graph_emb) - 1))
from <- rep(seq_len(nrow(graph_emb)), each = ncol(knn$nn.index))
to   <- as.vector(t(knn$nn.index))
w    <- as.vector(t(knn$nn.dist))
edge_df <- data.frame(from = from, to = to, weight = w)

g <- igraph::graph_from_edgelist(as.matrix(edge_df[, c("from", "to")]), directed = FALSE)
igraph::E(g)$weight <- edge_df$weight
if (igraph::ecount(g) > 0) {
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight = "min"))
}

dmat <- tryCatch(
  igraph::distances(g, v = root_idx, to = seq_len(nrow(graph_emb)), weights = igraph::E(g)$weight),
  error = function(e) NULL
)
if (is.null(dmat)) {
  warning("[10J V15] igraph distance failed; using PCA distance from root centroid.")
  centroid <- colMeans(graph_emb[root_idx, , drop = FALSE])
  pseudo_dist <- sqrt(rowSums(sweep(graph_emb, 2, centroid, "-")^2))
} else {
  pseudo_dist <- apply(as.matrix(dmat), 2, min, na.rm = TRUE)
}
finite_dist <- is.finite(pseudo_dist)
if (!any(finite_dist)) stop("[10J V15] No finite pseudotime distances.")
pseudotime <- rep(NA_real_, length(pseudo_dist))
pseudotime[finite_dist] <- pseudo_dist[finite_dist]

pseudotime[!finite_dist] <- max(pseudotime[finite_dist], na.rm = TRUE)
pseudotime <- (pseudotime - min(pseudotime, na.rm = TRUE)) / (max(pseudotime, na.rm = TRUE) - min(pseudotime, na.rm = TRUE))

rho <- safe_cor(pseudotime, priority_proxy)
if (is.finite(rho) && rho < 0) {
  pseudotime <- 1 - pseudotime
  rho <- safe_cor(pseudotime, priority_proxy)
}
valid_frac <- mean(is.finite(pseudotime))
cat(sprintf("[10J V15] Valid pseudotime fraction: %.4f\n", valid_frac))
cat(sprintf("[10J V15] Spearman correlation with priority proxy: %.4f\n", rho))

cell_table <- data.frame(
  cell_id = cells_use,
  cluster = as.character(clusters),
  UMAP_1 = as.numeric(umap[, 1]),
  UMAP_2 = as.numeric(umap[, 2]),
  pseudotime = pseudotime,
  DA_maturation_z = score_DA,
  neuronal_maturation_z = score_neur,
  progenitor_cell_cycle_z = score_prog,
  stress_risk_z = score_stress,
  priority_proxy_z = priority_proxy,
  stringsAsFactors = FALSE
)
write.csv(cell_table, file.path(OUT_TABLE_DIR, "10J_V15_pilot_cell_pseudotime_and_scores.csv"), row.names = FALSE)

cluster_summary <- do.call(rbind, lapply(split(cell_table, cell_table$cluster), function(dd) {
  data.frame(
    cluster = dd$cluster[1],
    n_cells = nrow(dd),
    pseudotime_q10 = as.numeric(stats::quantile(dd$pseudotime, 0.10, na.rm = TRUE)),
    pseudotime_q25 = as.numeric(stats::quantile(dd$pseudotime, 0.25, na.rm = TRUE)),
    pseudotime_median = stats::median(dd$pseudotime, na.rm = TRUE),
    pseudotime_q75 = as.numeric(stats::quantile(dd$pseudotime, 0.75, na.rm = TRUE)),
    pseudotime_q90 = as.numeric(stats::quantile(dd$pseudotime, 0.90, na.rm = TRUE)),
    priority_proxy_median = stats::median(dd$priority_proxy_z, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
cluster_summary <- cluster_summary[order(cluster_summary$pseudotime_median), , drop = FALSE]
write.csv(cluster_summary, file.path(OUT_TABLE_DIR, "10J_V15_pilot_cluster_pseudotime_summary.csv"), row.names = FALSE)

program_long <- rbind(
  data.frame(program = "DA maturation", pseudotime = pseudotime, score = score_DA),
  data.frame(program = "Neuronal maturation", pseudotime = pseudotime, score = score_neur),
  data.frame(program = "Progenitor/cell-cycle", pseudotime = pseudotime, score = score_prog),
  data.frame(program = "Stress/risk", pseudotime = pseudotime, score = score_stress)
)
program_summary <- do.call(rbind, lapply(split(program_long, program_long$program), function(dd) {
  ss <- summ_by_bin(dd$pseudotime, dd$score, N_BINS)
  ss$program <- dd$program[1]
  ss
}))
program_summary <- program_summary[, c("program", "bin", "x_mid", "n", "q10", "q25", "median", "q75", "q90")]
write.csv(program_long, file.path(OUT_TABLE_DIR, "10J_V15_pilot_program_scores_long.csv"), row.names = FALSE)
write.csv(program_summary, file.path(OUT_TABLE_DIR, "10J_V15_pilot_program_score_binned_summary.csv"), row.names = FALSE)

priority_summary <- summ_by_bin(pseudotime, priority_proxy, N_BINS)
write.csv(priority_summary, file.path(OUT_TABLE_DIR, "10J_V15_pilot_priority_proxy_binned_summary.csv"), row.names = FALSE)

cluster_median_spread <- max(cluster_summary$pseudotime_median, na.rm = TRUE) - min(cluster_summary$pseudotime_median, na.rm = TRUE)
if (valid_frac >= 0.95 && is.finite(cluster_median_spread) && cluster_median_spread >= 0.15) {
  if (is.finite(rho) && rho >= 0.25) {
    decision <- "PROCEED_TO_10K_FINAL_PSEUDOTIME_ANALYSIS"
  } else {
    decision <- "PROCEED_WITH_CAUTION_TO_10K"
  }
} else {
  decision <- "DO_NOT_PROMOTE_10J_TO_MAIN_FIGURE_USE_DIAGNOSTIC_ONLY"
}

cluster_levels <- unique(as.character(cluster_summary$cluster))
cluster_palette <- grDevices::hcl.colors(length(unique(clusters)), palette = "Dark 3")
names(cluster_palette) <- sort(unique(as.character(clusters)))

fig_A <- file.path(OUT_FIG_DIR, "10J_V15_A_embedding_clusters.pdf")
safe_pdf(fig_A, width = 6.6, height = 5.4, function() {
  graphics::par(mar = c(4.5, 4.7, 3.0, 5.4), las = 1, xpd = NA, cex = 0.95)
  graphics::plot(
    umap[, 1], umap[, 2], pch = 16, cex = 0.32,
    col = grDevices::adjustcolor(cluster_palette[as.character(clusters)], alpha.f = 0.75),
    xlab = "UMAP 1", ylab = "UMAP 2",
    main = "10J pilot cell-state layout",
    bty = "l"
  )
  graphics::legend(
    "right", inset = c(-0.23, 0), legend = names(cluster_palette),
    col = cluster_palette, pch = 16, bty = "n", title = cluster_col, cex = 0.8
  )
})

fig_B <- file.path(OUT_FIG_DIR, "10J_V15_B_embedding_pseudotime.pdf")
safe_pdf(fig_B, width = 6.5, height = 5.4, function() {
  graphics::par(mar = c(4.5, 4.7, 3.0, 4.8), las = 1, xpd = NA, cex = 0.95)
  pal <- grDevices::colorRampPalette(c("#edf8fb", "#b2e2e2", "#66c2a4", "#238b45", "#00441b"))(100)
  idx <- pmax(1, pmin(100, as.integer(round(pseudotime * 99 + 1))))
  graphics::plot(
    umap[, 1], umap[, 2], pch = 16, cex = 0.32,
    col = grDevices::adjustcolor(pal[idx], alpha.f = 0.78),
    xlab = "UMAP 1", ylab = "UMAP 2",
    main = "10J pilot graph-based pseudotime",
    bty = "l"
  )

  legend_cols <- pal[c(1, 25, 50, 75, 100)]
  graphics::legend(
    "right", inset = c(-0.20, 0), legend = c("0", "0.25", "0.50", "0.75", "1.00"),
    col = legend_cols, pch = 16, pt.cex = 1.2, bty = "n", title = "Pseudotime", cex = 0.75
  )
})

fig_C <- file.path(OUT_FIG_DIR, "10J_V15_C_cluster_pseudotime_dotrange.pdf")
safe_pdf(fig_C, width = 6.3, height = 4.8, function() {
  graphics::par(mar = c(5.2, 4.7, 3.0, 1.2), las = 1, cex = 0.95)
  x <- seq_len(nrow(cluster_summary))
  graphics::plot(
    x, cluster_summary$pseudotime_median,
    ylim = c(0, 1), pch = 16, cex = 1.0, xaxt = "n",
    xlab = "Cluster ordered by median pseudotime", ylab = "Pseudotime",
    main = "Cluster-level pseudotime ordering", bty = "l"
  )
  graphics::segments(x, cluster_summary$pseudotime_q25, x, cluster_summary$pseudotime_q75, lwd = 3, col = "grey45")
  graphics::segments(x, cluster_summary$pseudotime_q10, x, cluster_summary$pseudotime_q90, lwd = 1, col = "grey65")
  graphics::points(x, cluster_summary$pseudotime_median, pch = 16, cex = 0.9)
  graphics::axis(1, at = x, labels = cluster_summary$cluster, las = 1)
  graphics::abline(h = seq(0, 1, 0.25), col = "grey90", lwd = 0.8)
})

fig_D <- file.path(OUT_FIG_DIR, "10J_V15_D_program_trends_clean.pdf")
safe_pdf(fig_D, width = 7.4, height = 5.9, function() {
  graphics::par(mfrow = c(2, 2), mar = c(4.0, 4.4, 2.6, 1.0), oma = c(0, 0, 2.0, 0), las = 1, cex = 0.88)
  progs <- c("DA maturation", "Neuronal maturation", "Progenitor/cell-cycle", "Stress/risk")
  for (pg in progs) {
    ss <- program_summary[program_summary$program == pg, , drop = FALSE]
    yy <- c(ss$q25, ss$q75, ss$median)
    y_lim <- range(yy[is.finite(yy)], na.rm = TRUE)
    if (!all(is.finite(y_lim)) || diff(y_lim) == 0) y_lim <- c(-1, 1)
    pad <- diff(y_lim) * 0.15
    y_lim <- y_lim + c(-pad, pad)
    graphics::plot(
      ss$x_mid, ss$median, type = "n", xlim = c(0, 1), ylim = y_lim,
      xlab = "Pseudotime", ylab = "Program score (z)", main = pg, bty = "l"
    )
    graphics::segments(ss$x_mid, ss$q25, ss$x_mid, ss$q75, col = "grey60", lwd = 2)
    graphics::lines(ss$x_mid, ss$median, lwd = 2, col = "black")
    graphics::points(ss$x_mid, ss$median, pch = 16, cex = 0.7, col = "black")
    graphics::abline(h = 0, col = "grey85", lty = 2)
  }
  graphics::mtext("10J pilot program trends along pseudotime", outer = TRUE, cex = 1.05, font = 2)
})

fig_E <- file.path(OUT_FIG_DIR, "10J_V15_E_priority_proxy_clean.pdf")
safe_pdf(fig_E, width = 5.8, height = 4.8, function() {
  graphics::par(mar = c(4.7, 4.8, 3.2, 1.0), las = 1, cex = 0.92)
  ss <- priority_summary
  yy <- c(ss$q25, ss$q75, ss$median)
  y_lim <- range(yy[is.finite(yy)], na.rm = TRUE)
  if (!all(is.finite(y_lim)) || diff(y_lim) == 0) y_lim <- c(-1, 1)
  pad <- diff(y_lim) * 0.2
  y_lim <- y_lim + c(-pad, pad)
  graphics::plot(
    ss$x_mid, ss$median, type = "n", xlim = c(0, 1), ylim = y_lim,
    xlab = "Pseudotime", ylab = "Priority proxy (z)",
    main = "Priority proxy along pseudotime", bty = "l"
  )
  graphics::segments(ss$x_mid, ss$q25, ss$x_mid, ss$q75, col = "grey55", lwd = 2)
  graphics::lines(ss$x_mid, ss$median, lwd = 2, col = "black")
  graphics::points(ss$x_mid, ss$median, pch = 16, cex = 0.8, col = "black")
  graphics::abline(h = 0, col = "grey85", lty = 2)
  graphics::mtext(sprintf("Binned median/IQR; Spearman rho = %.3f", rho), side = 3, line = 0.2, cex = 0.65)
})

marker_order <- c(
  "TH", "DDC", "SLC6A3", "SLC18A2", "NR4A2", "PITX3", "FOXA2", "LMX1A", "LMX1B", "EN1", "EN2", "ALDH1A1", "KCNJ6",
  "MAP2", "RBFOX3", "TUBB3", "SNAP25", "SYN1", "DCX",
  "SOX2", "NES", "PAX6", "VIM", "ASCL1", "MKI67", "TOP2A", "PCNA",
  "JUN", "FOS", "DDIT3", "ATF3", "HMOX1", "HSPA1A"
)
marker_matched <- match_genes(marker_order, rn)
marker_matched <- marker_matched[seq_len(min(length(marker_matched), 32))]
heat_z <- NULL
if (length(marker_matched) >= 5) {
  bins <- cut(pseudotime, breaks = seq(0, 1, length.out = N_BINS + 1), include.lowest = TRUE, labels = FALSE)
  raw_bin <- sapply(seq_len(N_BINS), function(b) {
    idx <- which(bins == b)
    if (length(idx) == 0) return(rep(NA_real_, length(marker_matched)))
    Matrix::rowMeans(expr[marker_matched, idx, drop = FALSE])
  })
  rownames(raw_bin) <- marker_matched
  colnames(raw_bin) <- paste0("B", seq_len(N_BINS))
  heat_z <- t(apply(raw_bin, 1, zsafe))
  heat_z[!is.finite(heat_z)] <- 0
  write.csv(heat_z, file.path(OUT_TABLE_DIR, "10J_V15_marker_trend_heatmap_zscore_matrix.csv"))
}

fig_F <- file.path(OUT_FIG_DIR, "10J_V15_F_marker_trend_heatmap_clean.pdf")
if (!is.null(heat_z) && nrow(heat_z) >= 5 && ncol(heat_z) >= 2) {
  safe_pdf(fig_F, width = 5.9, height = 7.7, function() {
    graphics::par(mar = c(4.8, 6.7, 3.2, 1.3), las = 1, cex = 0.82)
    vals <- pmax(pmin(heat_z, 2.5), -2.5)
    pal <- grDevices::colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(101)
    br <- seq(-2.5, 2.5, length.out = 102)
    col_idx <- pmax(1, pmin(101, findInterval(vals, br, all.inside = TRUE)))

    image_mat <- t(vals[nrow(vals):1, , drop = FALSE])
    graphics::image(
      x = seq_len(ncol(vals)), y = seq_len(nrow(vals)), z = image_mat,
      col = pal, breaks = br, axes = FALSE,
      xlab = "Pseudotime bin", ylab = "Marker",
      main = "Marker trends across pseudotime bins"
    )
    graphics::axis(1, at = seq_len(ncol(vals)), labels = seq_len(ncol(vals)), cex.axis = 0.75)
    graphics::axis(2, at = seq_len(nrow(vals)), labels = rev(rownames(vals)), las = 2, cex.axis = 0.58)
    graphics::box(lwd = 0.8)

    graphics::mtext("z-score: blue low, white mid, red high", side = 3, line = 0.25, cex = 0.62)
  })
} else {
  writeLines("Marker heatmap skipped: fewer than 5 marker genes were matched.", paste0(fig_F, ".SKIPPED.txt"))
}

execution_summary <- data.frame(
  module = MODULE_TAG,
  input_object = INPUT_OBJECT,
  n_cells_loaded = ncol(obj),
  n_genes_loaded = nrow(obj),
  n_pilot_cells = length(cells_use),
  cluster_column = cluster_col,
  root_cells = length(root_idx),
  valid_pseudotime_fraction = valid_frac,
  pseudotime_cluster_median_spread = cluster_median_spread,
  spearman_priority_proxy = rho,
  decision = decision,
  stringsAsFactors = FALSE
)
write.csv(execution_summary, file.path(OUT_TABLE_DIR, "10J_V15_execution_summary.csv"), row.names = FALSE)

report_lines <- c(
  "10J V15 FINAL SEPARATE PUBLICATION-SAFE PSEUDOTIME PILOT",
  "========================================================",
  paste("Input object:", INPUT_OBJECT),
  paste("Pilot cells:", length(cells_use)),
  paste("Cluster column:", cluster_col),
  paste("Root cells:", length(root_idx)),
  sprintf("Valid pseudotime fraction: %.4f", valid_frac),
  sprintf("Pseudotime cluster median spread: %.4f", cluster_median_spread),
  sprintf("Spearman priority proxy: %.4f", rho),
  paste("Decision:", decision),
  "",
  "Claim boundary:",
  "This is a graph-based transcriptomic pseudotime pilot. It is not true lineage tracing, clone-aware fate tracking, functional maturation evidence, or clinical prediction.",
  "10J should be treated as pilot/diagnostic evidence unless strengthened by the multi-timepoint 10K analysis.",
  "",
  "Final figure outputs:",
  basename(c(fig_A, fig_B, fig_C, fig_D, fig_E, fig_F))
)
writeLines(report_lines, file.path(OUT_TEXT_DIR, "10J_V15_execution_report.txt"))
writeLines(decision, file.path(OUT_TEXT_DIR, "10J_V15_pilot_decision.txt"))

cat("\n[10J V15] Completed FINAL separate publication-safe pseudotime pilot.\n")
cat("[10J V15] Input object:", INPUT_OBJECT, "\n")
cat("[10J V15] Pilot cells:", length(cells_use), "\n")
cat(sprintf("[10J V15] Valid pseudotime fraction: %.4f\n", valid_frac))
cat(sprintf("[10J V15] Pseudotime cluster median spread: %.4f\n", cluster_median_spread))
cat(sprintf("[10J V15] Spearman priority proxy: %.4f\n", rho))
cat("[10J V15] Decision:", decision, "\n")
cat("[10J V15] Output tables:", OUT_TABLE_DIR, "\n")
cat("[10J V15] Output figures:", OUT_FIG_DIR, "\n")
cat("[10J V15] Output text:", OUT_TEXT_DIR, "\n")
cat("[10J V15] Next: inspect the six separate PDFs. If clean, lock 10J V15 and run 10K_final_pseudotime_trajectory_analysis.\n")
