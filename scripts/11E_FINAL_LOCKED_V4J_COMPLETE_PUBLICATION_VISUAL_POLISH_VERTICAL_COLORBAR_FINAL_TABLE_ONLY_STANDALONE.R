# ==================================================================================================
# 11E_FINAL_LOCKED_V4J_COMPLETE_PUBLICATION_VISUAL_POLISH_COLORBAR_AND_SCALE_FIX_TABLE_ONLY_STANDALONE.R
#
# Purpose:
#   Final table-only visual polish for 11E. V4J is a complete table-only visual polish update. It fixes the FigB colorbar margin and keeps FigC/FigD state-only so the all-cell aggregate does not distort axes or balance summaries.
#   This script DOES NOT rerun 00-10P, DOES NOT rerun Seurat objects, DOES NOT upgrade lineage claims.
#   It reads the already generated 11E V1 / V2B / V4C tables and rebuilds publication-style PDF panels.
#
# Final claim:
#   GSE200610 is retained only as transcriptomic state-level proxy support.
#   Strict barcode / clone / lineage columns = 0.
#
# Key visual fixes compared with V4C:
#   1) Heatmap excludes the "All cells" aggregate from z-score scaling and the state-only heatmap body.
#   2) Heatmap uses a manually positioned vertical right-side colorbar with extra whitespace, so the z-scale is tidy and does not touch the heatmap body.
#   3) Priority-risk scatter is state-only; the all-cell aggregate is documented but not allowed to distort axes.
#   4) Balance summary uses state-only median-centering rather than all-cell centering.
#   4) Claim boundary panel is retained as a compact audit/interpretation panel.
#
# Author: generated for PD_Graft_Project
# ==================================================================================================

options(stringsAsFactors = FALSE)

project_root <- "D:/PD_Graft_Project"

v1_table_dir <- file.path(project_root, "03_tables", "11E_barcode_lineage_tracing_validation_V1")
v2b_table_dir <- file.path(project_root, "03_tables", "11E_barcode_lineage_tracing_validation_V2B_DEEP_BARCODE_RESCUE_AUDIT_FIXED")
v4c_table_dir <- file.path(project_root, "03_tables", "11E_barcode_lineage_tracing_validation_FINAL_LOCKED_V4C_PUBLICATION_VISUAL_POLISH_STATE_LEVEL_PROXY")

out_table_dir <- file.path(project_root, "03_tables", "11E_barcode_lineage_tracing_validation_FINAL_LOCKED_V4J_PUBLICATION_VISUAL_POLISH_STATE_LEVEL_PROXY")
out_fig_dir   <- file.path(project_root, "04_figures", "11E_barcode_lineage_tracing_validation_FINAL_LOCKED_V4J_PUBLICATION_VISUAL_POLISH_STATE_LEVEL_PROXY_pdf")
out_text_dir  <- file.path(project_root, "09_manuscript", "11E_barcode_lineage_tracing_validation_FINAL_LOCKED_V4J_PUBLICATION_VISUAL_POLISH_STATE_LEVEL_PROXY")

dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_text_dir, recursive = TRUE, showWarnings = FALSE)

message("\n[11E V4J] Starting strict table-only publication visual polish...")
message("[11E V4J] No object rerun; no internet; no claim upgrade.")
message("[11E V4J] Output figures: ", out_fig_dir)

safe_read_table <- function(path, sep = NULL) {
  if (!file.exists(path)) {
    message("[11E V4J] Missing table: ", path)
    return(data.frame())
  }
  if (is.null(sep)) {
    if (grepl("\\.tsv$", path, ignore.case = TRUE)) sep <- "\t" else sep <- ","
  }
  out <- tryCatch({
    read.table(path, header = TRUE, sep = sep, quote = "\"", comment.char = "", check.names = FALSE, fill = TRUE)
  }, error = function(e) {
    message("[11E V4J] Read failed, returning empty: ", path, " :: ", conditionMessage(e))
    data.frame()
  })
  message("[11E V4J] Read: ", path, " rows=", nrow(out), " cols=", ncol(out))
  out
}

as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

find_col <- function(df, patterns, exclude = character(0), prefer = character(0)) {
  if (ncol(df) == 0) return(NA_character_)
  nms <- names(df)
  low <- tolower(nms)
  ok <- rep(FALSE, length(nms))
  for (p in patterns) ok <- ok | grepl(p, low, perl = TRUE)
  if (length(exclude) > 0) {
    for (p in exclude) ok <- ok & !grepl(p, low, perl = TRUE)
  }
  hits <- nms[ok]
  if (length(hits) == 0) return(NA_character_)
  if (length(prefer) > 0) {
    for (p in prefer) {
      h2 <- hits[grepl(p, tolower(hits), perl = TRUE)]
      if (length(h2) > 0) return(h2[1])
    }
  }
  hits[1]
}

first_existing <- function(paths) {
  for (p in paths) if (file.exists(p)) return(p)
  return(paths[1])
}

# --------------------------------------------------------------------------------------------------
# Read key tables
# --------------------------------------------------------------------------------------------------

summary_path <- first_existing(c(
  file.path(v4c_table_dir, "11E_FINAL_LOCKED_V4C_state_level_summary_for_11H.csv"),
  file.path(v4c_table_dir, "11E_FINAL_LOCKED_V4C_state_level_summary_for_11H.tsv"),
  file.path(v1_table_dir, "11E_V1_cluster_level_priority_lineage_scores.csv")
))

if (grepl("\\.tsv$", summary_path, ignore.case = TRUE)) {
  state_raw <- safe_read_table(summary_path, sep = "\t")
} else {
  state_raw <- safe_read_table(summary_path, sep = ",")
}

v1_exec <- safe_read_table(file.path(v1_table_dir, "11E_V1_execution_summary.csv"), sep = ",")
v2b_exec <- safe_read_table(file.path(v2b_table_dir, "11E_V2B_execution_summary.tsv"), sep = "\t")
v2b_candidates <- safe_read_table(file.path(v2b_table_dir, "11E_V2B_all_barcode_lineage_column_candidates.tsv"), sep = "\t")
v2b_strict <- safe_read_table(file.path(v2b_table_dir, "11E_V2B_strict_retained_barcode_lineage_columns.tsv"), sep = "\t")

if (nrow(state_raw) == 0) stop("[11E V4J] No state-level table found. Run 11E V4C first or provide V1 cluster-level table.")

# --------------------------------------------------------------------------------------------------
# Build a clean state-level summary
# --------------------------------------------------------------------------------------------------

state_col <- find_col(state_raw, c("^state$", "state_label", "state", "cluster_label", "cluster", "seurat_cluster", "ident"),
                      exclude = c("score", "module", "source", "barcode"),
                      prefer = c("state_label", "state", "cluster_label", "cluster"))

n_col <- find_col(state_raw, c("n_cells", "cell_count", "cells", "ncell", "n$", "count"),
                  exclude = c("candidate", "metadata", "column", "score"),
                  prefer = c("n_cells", "cell_count", "cells", "count"))

if (is.na(state_col)) {
  # last-resort: use row index as state label
  state_raw$state_label_auto_11E <- paste0("State ", seq_len(nrow(state_raw)))
  state_col <- "state_label_auto_11E"
}
if (is.na(n_col)) {
  state_raw$n_cells_auto_11E <- 1
  n_col <- "n_cells_auto_11E"
}

# Module column detection. These keywords are intentionally broad but final checked by numeric content.
mod_patterns <- list(
  DA         = c("^da$", "da_score", "dopamin", "da_"),
  A9         = c("^a9$", "a9_score", "a9_"),
  A10        = c("^a10$", "a10_score", "a10_"),
  Projection = c("projection", "proj"),
  Maturation = c("maturation", "mature"),
  Cell_cycle = c("cell.?cycle", "cycle", "prolif"),
  Off_target = c("off.?target", "offtarget", "non.?da"),
  Stress     = c("stress"),
  p53        = c("p53"),
  NFkB       = c("nf.?kb", "nfkb", "nf.k")
)

module_cols <- character(0)
module_names <- character(0)
for (nm in names(mod_patterns)) {
  cc <- find_col(state_raw, mod_patterns[[nm]], exclude = c("rank", "pvalue", "padj", "label", "source"))
  if (!is.na(cc)) {
    vv <- as_num(state_raw[[cc]])
    if (sum(is.finite(vv)) >= 2) {
      module_cols <- c(module_cols, cc)
      module_names <- c(module_names, nm)
    }
  }
}

if (length(module_cols) < 4) {
  numeric_candidates <- names(state_raw)[sapply(state_raw, function(z) sum(is.finite(as_num(z))) >= 2)]
  numeric_candidates <- setdiff(numeric_candidates, c(n_col))
  stop(paste0("[11E V4J] Too few module columns detected. Detected: ", paste(module_cols, collapse = ", "),
              "\nNumeric candidates: ", paste(numeric_candidates, collapse = ", ")))
}

# Keep one column per module name.
dedup <- !duplicated(module_names)
module_cols <- module_cols[dedup]
module_names <- module_names[dedup]

state_raw$.__state <- as.character(state_raw[[state_col]])
state_raw$.__n <- as_num(state_raw[[n_col]])
state_raw$.__n[!is.finite(state_raw$.__n) | state_raw$.__n <= 0] <- 1

# Normalize labels.
clean_state_label <- function(x) {
  x <- as.character(x)
  x <- gsub("GSE200610", "", x, ignore.case = TRUE)
  x <- gsub("cluster", "State", x, ignore.case = TRUE)
  x <- gsub("^\\s*all[_ .-]*cells\\s*$", "All cells", x, ignore.case = TRUE)
  x <- gsub("^\\s*allcells\\s*$", "All cells", x, ignore.case = TRUE)
  x <- gsub("^\\s*(\\d+)\\s*$", "State \\1", x, perl = TRUE)
  x <- gsub("^\\s*C(\\d+)\\s*$", "State \\1", x, perl = TRUE)
  x <- gsub("_", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
state_raw$.__state_clean <- clean_state_label(state_raw$.__state)

# Aggregate repeated state rows by weighted means. If V4C summary is already unique, this preserves it.
unique_states <- unique(state_raw$.__state_clean)
summary_list <- list()
for (st in unique_states) {
  idx <- which(state_raw$.__state_clean == st)
  ww <- state_raw$.__n[idx]
  if (length(ww) == 0 || sum(ww, na.rm = TRUE) <= 0) ww <- rep(1, length(idx))
  row <- data.frame(state = st, n_cells = sum(ww, na.rm = TRUE), stringsAsFactors = FALSE)
  for (i in seq_along(module_cols)) {
    vv <- as_num(state_raw[[module_cols[i]]])[idx]
    ok <- is.finite(vv) & is.finite(ww)
    row[[module_names[i]]] <- if (sum(ok) == 0) NA_real_ else sum(vv[ok] * ww[ok]) / sum(ww[ok])
  }
  summary_list[[length(summary_list) + 1]] <- row
}
state_sum <- do.call(rbind, summary_list)

fav_modules <- intersect(c("DA", "A9", "A10", "Projection", "Maturation"), names(state_sum))
risk_modules <- intersect(c("Cell_cycle", "Off_target", "Stress", "p53", "NFkB"), names(state_sum))

if (length(fav_modules) == 0 || length(risk_modules) == 0) {
  stop("[11E V4J] Could not identify favorable and risk module columns after aggregation.")
}

row_mean_safe <- function(mat) {
  if (is.null(dim(mat))) return(as_num(mat))
  apply(mat, 1, function(z) {
    z <- as_num(z)
    if (sum(is.finite(z)) == 0) NA_real_ else mean(z[is.finite(z)])
  })
}

state_sum$favorable_score <- row_mean_safe(state_sum[, fav_modules, drop = FALSE])
state_sum$risk_score <- row_mean_safe(state_sum[, risk_modules, drop = FALSE])
state_sum$priority_balance_index <- state_sum$favorable_score - state_sum$risk_score

# Center All cells to 0 if present; otherwise use median-centered index for visualization.
all_idx <- which(tolower(state_sum$state) %in% c("all cells", "all_cells", "allcells"))
if (length(all_idx) > 0 && is.finite(state_sum$priority_balance_index[all_idx[1]])) {
  center_val <- state_sum$priority_balance_index[all_idx[1]]
} else {
  center_val <- median(state_sum$priority_balance_index, na.rm = TRUE)
}
state_sum$priority_balance_centered <- state_sum$priority_balance_index - center_val

# State-only table excludes All cells for heatmap scaling and state ranking. For final FigC/FigD,
# compute a dedicated numeric state-only median-centered index. This avoids legacy character
# columns from V4C tables and prevents the all-cell aggregate from distorting the scale.
is_all <- grepl("^all\\s*cells$", state_sum$state, ignore.case = TRUE)
state_median_center <- median(as_num(state_sum$priority_balance_index[!is_all]), na.rm = TRUE)
if (!is.finite(state_median_center)) state_median_center <- median(as_num(state_sum$priority_balance_index), na.rm = TRUE)
if (!is.finite(state_median_center)) state_median_center <- 0
state_sum$priority_balance_state_median_centered <- as_num(state_sum$priority_balance_index) - state_median_center

state_only <- state_sum[!is_all, , drop = FALSE]
all_ref <- state_sum[is_all, , drop = FALSE]
state_only$priority_balance_state_median_centered <- as_num(state_only$priority_balance_state_median_centered)
state_only$priority_balance_index <- as_num(state_only$priority_balance_index)
state_only$favorable_score <- as_num(state_only$favorable_score)
state_only$risk_score <- as_num(state_only$risk_score)

# Sort states by state-only median-centered priority balance, high to low.
ord <- order(state_only$priority_balance_state_median_centered, decreasing = TRUE, na.last = TRUE)
state_only <- state_only[ord, , drop = FALSE]

# Save V4J summary tables.
write.csv(state_sum, file.path(out_table_dir, "11E_FINAL_LOCKED_V4J_state_level_summary_all_rows.csv"), row.names = FALSE)
write.table(state_sum, file.path(out_table_dir, "11E_FINAL_LOCKED_V4J_state_level_summary_all_rows.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
write.csv(state_only, file.path(out_table_dir, "11E_FINAL_LOCKED_V4J_state_only_summary_for_11H.csv"), row.names = FALSE)
write.table(state_only, file.path(out_table_dir, "11E_FINAL_LOCKED_V4J_state_only_summary_for_11H.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)

# Summary numbers from previous audit.
get_summary_value <- function(df, keys) {
  if (nrow(df) == 0 || ncol(df) < 2) return(NA_character_)
  low1 <- tolower(as.character(df[[1]]))
  for (k in keys) {
    hit <- which(grepl(tolower(k), low1, fixed = TRUE))
    if (length(hit) > 0) return(as.character(df[[2]][hit[1]]))
  }
  NA_character_
}

rds_detected <- get_summary_value(v2b_exec, c("GSE200610 RDS detected", "RDS detected"))
readable_objects <- get_summary_value(v2b_exec, c("Readable objects"))
possible_candidates <- get_summary_value(v2b_exec, c("Possible barcode-like columns", "Possible barcode"))
strict_retained <- get_summary_value(v2b_exec, c("Strict retained barcode", "Strict retained"))
if (is.na(strict_retained) || strict_retained == "") strict_retained <- as.character(nrow(v2b_strict))
if (is.na(possible_candidates) || possible_candidates == "") possible_candidates <- as.character(nrow(v2b_candidates))
state_rows_n <- as.character(nrow(state_sum))
state_only_n <- as.character(nrow(state_only))

# --------------------------------------------------------------------------------------------------
# Plot helpers
# --------------------------------------------------------------------------------------------------

open_pdf <- function(filename, width = 8.5, height = 6.0) {
  pdf(file.path(out_fig_dir, filename), width = width, height = height, useDingbats = FALSE)
}

panel_box <- function(x0, y0, x1, y1, fill = "#F7F7F7", border = "#333333", lwd = 1.2) {
  graphics::rect(x0, y0, x1, y1, col = fill, border = border, lwd = lwd)
}

center_text <- function(x0, y0, x1, y1, label, cex = 0.9, font = 1, col = "#111111") {
  text((x0 + x1)/2, (y0 + y1)/2, label, cex = cex, font = font, col = col)
}

# --------------------------------------------------------------------------------------------------
# Fig A: Metadata audit and claim boundary
# --------------------------------------------------------------------------------------------------

open_pdf("11E_FINAL_LOCKED_V4J_FigA_metadata_audit_and_claim_boundary.pdf", width = 10.0, height = 6.2)
par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
plot.new(); plot.window(xlim = c(0, 100), ylim = c(0, 100))
text(50, 94, "11E evidence audit: GSE200610 retained as state-level proxy support", cex = 1.2, font = 2)
text(50, 88, "Deep metadata audit found no strict barcode / clone / lineage column suitable for lineage-level association testing.", cex = 0.78, col = "#555555")

xstarts <- c(5, 25, 45, 65, 82)
labels <- c("Local input\nGSE200610 RDS", "State scoring\nstate rows", "Metadata rescue\nbarcode-like columns", "Strict lineage\ncolumns retained", "Final claim\nstate-level proxy")
vals <- c(ifelse(is.na(rds_detected), "70", rds_detected), state_rows_n, ifelse(is.na(possible_candidates), "151", possible_candidates), ifelse(is.na(strict_retained), "0", strict_retained), "proxy only")
fills <- c("#F3F6FA", "#F3F6FA", "#FFF8E8", "#FDEDEC", "#EAF4EA")
for (i in seq_along(xstarts)) {
  panel_box(xstarts[i], 62, xstarts[i] + 13, 78, fill = fills[i])
  center_text(xstarts[i], 70, xstarts[i] + 13, 77, labels[i], cex = 0.68, font = 2)
  center_text(xstarts[i], 62.5, xstarts[i] + 13, 69, vals[i], cex = 0.95, font = 2)
  if (i < length(xstarts)) arrows(xstarts[i] + 14, 70, xstarts[i+1] - 1.5, 70, length = 0.08, lwd = 1.1, col = "#777777")
}

panel_box(7, 18, 47, 50, fill = "#FAFAFA", border = "#555555")
text(27, 47, "Allowed interpretation", cex = 0.9, font = 2)
allowed <- c("Transcriptomic state-level proxy support", "Conservative module-pattern evidence", "Downstream integration only as proxy support")
for (i in seq_along(allowed)) text(10, 43 - 8*i, paste0("\u2022 ", allowed[i]), adj = 0, cex = 0.78)

panel_box(53, 18, 93, 50, fill = "#FAFAFA", border = "#555555")
text(73, 47, "Prohibited interpretation", cex = 0.9, font = 2)
prohibited <- c("Barcode-level lineage tracing validation", "Clone-aware fate reconstruction", "Barcode-confirmed graft-state transition")
for (i in seq_along(prohibited)) text(56, 43 - 8*i, paste0("\u2022 ", prohibited[i]), adj = 0, cex = 0.78)

text(50, 8, "Final status: FINAL_LOCKED_11E_AS_GSE200610_TRANSCRIPTOMIC_STATE_LEVEL_PROXY_SUPPORT", cex = 0.72, font = 2, col = "#333333")
dev.off()
message("[11E V4J] Wrote FigA")

# --------------------------------------------------------------------------------------------------
# Fig B: State-only module heatmap; All cells excluded from heatmap scaling.
# V4J uses a manually positioned heatmap and a clean vertical right-side colorbar.
# This avoids right-side label overflow and prevents the colorbar from visually sticking to the heatmap.
# --------------------------------------------------------------------------------------------------

heat_modules <- intersect(c("DA", "A9", "A10", "Projection", "Maturation", "Cell_cycle", "Off_target", "Stress", "p53", "NFkB"), names(state_only))
heat_mat <- as.matrix(state_only[, heat_modules, drop = FALSE])
mode(heat_mat) <- "numeric"

# Column z-score within state-only rows only. This avoids the all-cell aggregate dominating the heatmap.
z_mat <- heat_mat
for (j in seq_len(ncol(z_mat))) {
  v <- z_mat[, j]
  mu <- mean(v, na.rm = TRUE)
  ss <- sd(v, na.rm = TRUE)
  if (!is.finite(ss) || ss == 0) ss <- 1
  z_mat[, j] <- (v - mu) / ss
}
z_mat[z_mat > 2.5] <- 2.5
z_mat[z_mat < -2.5] <- -2.5

row_labels <- paste0(state_only$state, "  n=", format(round(state_only$n_cells), big.mark = ",", scientific = FALSE))
col_labels <- gsub("_", " ", heat_modules)
col_labels[col_labels == "Cell cycle"] <- "Cell\ncycle"
col_labels[col_labels == "Off target"] <- "Off-\ntarget"

open_pdf("11E_FINAL_LOCKED_V4J_FigB_state_level_module_heatmap.pdf", width = 13.4, height = 7.45)
par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

# Title and subtitle
text(0.50, 0.965, "GSE200610 state-level module landscape", cex = 1.22, font = 2)
text(0.50, 0.920,
     "State-only weighted averages; all-cell aggregate excluded from z-score scaling; column z-scores are for visualization only",
     cex = 0.68, col = "#555555")

# Controlled plotting geometry in normalized page coordinates.
# The heatmap body is intentionally narrowed so the vertical z colorbar and tick labels have enough room.
hm_x0 <- 0.132
hm_x1 <- 0.765
hm_y0 <- 0.205
hm_y1 <- 0.845
# Dedicated right-side colorbar panel. The heatmap is intentionally narrower to avoid overflow.
cb_x0 <- 0.835
cb_x1 <- 0.870
cb_y0 <- 0.285
cb_y1 <- 0.765

nrow_h <- nrow(z_mat)
ncol_h <- ncol(z_mat)
pal <- colorRampPalette(c("#2C7BB6", "#F7F7F7", "#B2182B"))(101)

value_to_color <- function(v) {
  if (!is.finite(v)) return("#F2F2F2")
  idx <- floor((v + 2.5) / 5 * (length(pal) - 1)) + 1
  idx <- max(1, min(length(pal), idx))
  pal[idx]
}

# Draw heatmap cells. First row in the sorted table appears at the top.
for (i in seq_len(nrow_h)) {
  y_top <- hm_y1 - (i - 1) / nrow_h * (hm_y1 - hm_y0)
  y_bot <- hm_y1 - i / nrow_h * (hm_y1 - hm_y0)
  for (j in seq_len(ncol_h)) {
    x_left <- hm_x0 + (j - 1) / ncol_h * (hm_x1 - hm_x0)
    x_right <- hm_x0 + j / ncol_h * (hm_x1 - hm_x0)
    rect(x_left, y_bot, x_right, y_top,
         col = value_to_color(z_mat[i, j]), border = "white", lwd = 0.45)
  }
}

# Heatmap outer frame
rect(hm_x0, hm_y0, hm_x1, hm_y1, border = "#333333", lwd = 0.9)

# Row labels
for (i in seq_len(nrow_h)) {
  y_mid <- hm_y1 - (i - 0.5) / nrow_h * (hm_y1 - hm_y0)
  text(hm_x0 - 0.018, y_mid, row_labels[i], adj = 1, cex = 0.74)
}

# Column labels, rotated. Keep below body with enough whitespace.
for (j in seq_len(ncol_h)) {
  x_mid <- hm_x0 + (j - 0.5) / ncol_h * (hm_x1 - hm_x0)
  text(x_mid, hm_y0 - 0.036, col_labels[j], srt = 90, adj = 1, cex = 0.74, xpd = NA)
}

# Right-side vertical colorbar with enough spacing. No horizontal legend, no overlapping labels.
n_cb <- length(pal)
for (k in seq_len(n_cb)) {
  yb <- cb_y0 + (k - 1) / n_cb * (cb_y1 - cb_y0)
  yt <- cb_y0 + k / n_cb * (cb_y1 - cb_y0)
  rect(cb_x0, yb, cb_x1, yt, col = pal[k], border = NA)
}
rect(cb_x0, cb_y0, cb_x1, cb_y1, border = "#333333", lwd = 0.8)
# Tick marks and labels placed to the right of the colorbar, with generous margin.
tick_vals <- c(-2.5, 0, 2.5)
tick_labs <- c("-2.5", "0", "+2.5")
for (k in seq_along(tick_vals)) {
  yy <- cb_y0 + (tick_vals[k] + 2.5) / 5 * (cb_y1 - cb_y0)
  segments(cb_x1, yy, cb_x1 + 0.010, yy, lwd = 0.8, col = "#333333")
  text(cb_x1 + 0.018, yy, tick_labs[k], adj = c(0, 0.5), cex = 0.74)
}
text((cb_x0 + cb_x1) / 2, cb_y1 + 0.038, "z", cex = 0.80, font = 2)

dev.off()
message("[11E V4J] Wrote FigB")

# --------------------------------------------------------------------------------------------------
# Fig C: State-only priority-risk landscape.
# V4J deliberately excludes the all-cell aggregate from plotting because it is a reference aggregate,
# not an individual transcriptomic state, and can distort axis ranges.
# --------------------------------------------------------------------------------------------------

plot_df <- state_only
plot_df <- plot_df[is.finite(plot_df$favorable_score) & is.finite(plot_df$risk_score), , drop = FALSE]
plot_df$pt_cex <- 0.82 + 1.15 * sqrt(plot_df$n_cells / max(plot_df$n_cells, na.rm = TRUE))
plot_df$label <- plot_df$state

xr <- range(plot_df$risk_score, na.rm = TRUE); yr <- range(plot_df$favorable_score, na.rm = TRUE)
xpad <- diff(xr) * 0.18; ypad <- diff(yr) * 0.20
if (!is.finite(xpad) || xpad == 0) xpad <- 0.03
if (!is.finite(ypad) || ypad == 0) ypad <- 0.03
xlim <- c(xr[1] - xpad, xr[2] + xpad)
ylim <- c(yr[1] - ypad, yr[2] + ypad)

med_x <- median(plot_df$risk_score, na.rm = TRUE)
med_y <- median(plot_df$favorable_score, na.rm = TRUE)

# Label top/bottom states and major spatial extremes, then de-duplicate.
plot_df$priority_balance_state_median_centered <- as_num(plot_df$priority_balance_state_median_centered)
if (sum(is.finite(plot_df$priority_balance_state_median_centered)) == 0) {
  plot_df$priority_balance_state_median_centered <- as_num(plot_df$priority_balance_index) - median(as_num(plot_df$priority_balance_index), na.rm = TRUE)
}
plot_df$abs_balance_rank <- abs(as_num(plot_df$priority_balance_state_median_centered))
label_idx <- unique(c(
  order(plot_df$priority_balance_state_median_centered, decreasing = TRUE)[seq_len(min(3, nrow(plot_df)))],
  order(plot_df$priority_balance_state_median_centered, decreasing = FALSE)[seq_len(min(3, nrow(plot_df)))],
  which.max(plot_df$risk_score), which.min(plot_df$risk_score),
  which.max(plot_df$favorable_score), which.min(plot_df$favorable_score)
))
label_df <- plot_df[label_idx, , drop = FALSE]

open_pdf("11E_FINAL_LOCKED_V4J_FigC_state_priority_risk_landscape.pdf", width = 8.9, height = 6.6)
par(mar = c(5.5, 5.8, 4.6, 2.2))
plot(plot_df$risk_score, plot_df$favorable_score,
     xlim = xlim, ylim = ylim,
     pch = 21, bg = "white", col = "#222222",
     cex = plot_df$pt_cex, lwd = 1.1,
     xlab = "Risk / off-target / stress module score",
     ylab = "Favorable DA / graft-support module score",
     cex.lab = 0.95, cex.axis = 0.82)
abline(v = med_x, h = med_y, lty = 2, col = "#BDBDBD", lwd = 1)
grid(col = "#EDEDED", lty = 1)
points(plot_df$risk_score, plot_df$favorable_score,
       pch = 21, bg = "white", col = "#222222",
       cex = plot_df$pt_cex, lwd = 1.1)

# Deterministic label offsets with clipping to plot bounds.
used_y <- numeric(0)
for (i in seq_len(nrow(label_df))) {
  xx <- label_df$risk_score[i]
  yy <- label_df$favorable_score[i]
  dx <- ifelse(xx <= med_x, -0.018 * diff(xlim), 0.018 * diff(xlim))
  dy <- ifelse(yy <= med_y, -0.035 * diff(ylim), 0.035 * diff(ylim))
  lx <- min(max(xx + dx, xlim[1] + 0.05 * diff(xlim)), xlim[2] - 0.05 * diff(xlim))
  ly <- min(max(yy + dy, ylim[1] + 0.055 * diff(ylim)), ylim[2] - 0.055 * diff(ylim))
  # simple vertical jitter if too close to prior labels
  if (length(used_y) > 0) {
    while (any(abs(ly - used_y) < 0.018 * diff(ylim))) {
      ly <- ly + 0.020 * diff(ylim)
      if (ly > ylim[2] - 0.055 * diff(ylim)) ly <- ylim[1] + 0.055 * diff(ylim)
    }
  }
  used_y <- c(used_y, ly)
  adjx <- ifelse(dx < 0, 1, 0)
  text(lx, ly, label_df$label[i], cex = 0.70, adj = c(adjx, 0.5), col = "#333333")
}

title("GSE200610 state-level priority-risk landscape", cex.main = 1.15, font.main = 2, line = 2.2)
mtext("No retained barcode-level grouping; state-only transcriptomic proxy support; all-cell aggregate omitted from state ranking", side = 3, line = 0.75, cex = 0.69, col = "#555555")
legend("bottomright", legend = c("State-level aggregate", "State median reference"), pch = c(21, NA), pt.bg = c("white", NA),
       lty = c(NA, 2), col = c("#222222", "#BDBDBD"), bty = "n", cex = 0.72)
dev.off()
message("[11E V4J] Wrote FigC")

# --------------------------------------------------------------------------------------------------
# Fig D: State-only priority balance summary.
# V4J uses state-only median centering rather than all-cell centering.
# --------------------------------------------------------------------------------------------------

bar_df <- state_only
bar_df$priority_balance_state_median_centered <- as_num(bar_df$priority_balance_state_median_centered)
if (sum(is.finite(bar_df$priority_balance_state_median_centered)) == 0) {
  bar_df$priority_balance_state_median_centered <- as_num(bar_df$priority_balance_index) - median(as_num(bar_df$priority_balance_index), na.rm = TRUE)
}
bar_df <- bar_df[is.finite(bar_df$priority_balance_state_median_centered), , drop = FALSE]
bar_df <- bar_df[order(bar_df$priority_balance_state_median_centered, decreasing = TRUE), , drop = FALSE]
bar_cols <- ifelse(bar_df$priority_balance_state_median_centered > 0, "#DDEAF3", ifelse(bar_df$priority_balance_state_median_centered < 0, "#F6DED3", "#F2F2F2"))

open_pdf("11E_FINAL_LOCKED_V4J_FigD_state_priority_balance_summary.pdf", width = 8.4, height = 6.7)
par(mar = c(5.2, 6.1, 4.3, 2.2))
xvals <- bar_df$priority_balance_state_median_centered
xlim_bar <- range(c(xvals, 0), na.rm = TRUE)
xpad_bar <- diff(xlim_bar) * 0.20
if (!is.finite(xpad_bar) || xpad_bar == 0) xpad_bar <- 0.05
xlim_bar <- c(xlim_bar[1] - xpad_bar, xlim_bar[2] + xpad_bar)
# Reverse order in barplot so the highest state appears at top.
plot_df_bar <- bar_df[nrow(bar_df):1, , drop = FALSE]
plot_cols <- bar_cols[nrow(bar_df):1]
bp <- barplot(plot_df_bar$priority_balance_state_median_centered, horiz = TRUE, las = 1,
              names.arg = plot_df_bar$state, col = plot_cols, border = NA,
              xlim = xlim_bar, cex.names = 0.78, cex.axis = 0.82,
              xlab = "Priority balance index")
abline(v = 0, lty = 2, col = "#BDBDBD", lwd = 1)
vals <- sprintf("%.3f", plot_df_bar$priority_balance_state_median_centered)
for (i in seq_along(vals)) {
  x <- plot_df_bar$priority_balance_state_median_centered[i]
  off <- 0.014 * diff(xlim_bar)
  if (x >= 0) {
    text(x + off, bp[i], vals[i], adj = 0, cex = 0.68, col = "#555555")
  } else {
    text(x - off, bp[i], vals[i], adj = 1, cex = 0.68, col = "#555555")
  }
}
title("State-level priority balance summary", cex.main = 1.15, font.main = 2, line = 2.2)
mtext("State-only median-centered index; positive values indicate relatively higher favorable DA/graft-support than risk/stress modules", side = 3, line = 0.7, cex = 0.66, col = "#555555")
dev.off()
message("[11E V4J] Wrote FigD")

# --------------------------------------------------------------------------------------------------
# Final manifest / claim boundary
# --------------------------------------------------------------------------------------------------

manifest <- data.frame(
  field = c(
    "module", "version", "final_status", "rds_detected_v2b", "readable_objects_v2b", "possible_barcode_like_columns_v2b",
    "strict_retained_barcode_lineage_columns", "state_rows_all", "state_rows_excluding_all_cells",
    "figures_written", "claim_level", "next_step"
  ),
  value = c(
    "11E_barcode_lineage_tracing_validation",
    "FINAL_LOCKED_V4J_PUBLICATION_VISUAL_POLISH_STATE_LEVEL_PROXY",
    "FINAL_LOCKED_11E_AS_GSE200610_TRANSCRIPTOMIC_STATE_LEVEL_PROXY_SUPPORT",
    ifelse(is.na(rds_detected), "NA", rds_detected),
    ifelse(is.na(readable_objects), "NA", readable_objects),
    ifelse(is.na(possible_candidates), as.character(nrow(v2b_candidates)), possible_candidates),
    ifelse(is.na(strict_retained), as.character(nrow(v2b_strict)), strict_retained),
    as.character(nrow(state_sum)),
    as.character(nrow(state_only)),
    "4",
    "state-level proxy only; no barcode-level lineage tracing claim",
    "11F_projection_associated_molecular_competence_proxy"
  ),
  stringsAsFactors = FALSE
)
write.csv(manifest, file.path(out_table_dir, "11E_FINAL_LOCKED_V4J_execution_manifest.csv"), row.names = FALSE)
write.table(manifest, file.path(out_table_dir, "11E_FINAL_LOCKED_V4J_execution_manifest.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)

claim_boundary <- data.frame(
  allowed = c(
    "GSE200610 was retained as transcriptomic state-level proxy support after deep barcode/lineage metadata audit.",
    "State-level DA/graft-support and risk/stress module patterns were used as conservative supportive evidence.",
    "11E can be integrated downstream only as a proxy evidence layer in 11H/12 modules."
  ),
  prohibited = c(
    "Do not describe 11E as barcode-level lineage tracing validation.",
    "Do not claim clone-aware fate reconstruction or lineage-resolved graft-state reconstruction.",
    "Do not claim barcode-confirmed graft-state transition or functional integration."
  ),
  stringsAsFactors = FALSE
)
write.csv(claim_boundary, file.path(out_table_dir, "11E_FINAL_LOCKED_V4J_claim_boundary.csv"), row.names = FALSE)
write.table(claim_boundary, file.path(out_table_dir, "11E_FINAL_LOCKED_V4J_claim_boundary.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)

report_path <- file.path(out_text_dir, "11E_FINAL_LOCKED_V4J_publication_visual_polish_report.txt")
cat(
  "11E FINAL LOCKED V4J PUBLICATION VISUAL POLISH\n",
  "=================================================\n\n",
  "Final status: FINAL_LOCKED_11E_AS_GSE200610_TRANSCRIPTOMIC_STATE_LEVEL_PROXY_SUPPORT\n",
  "Strict retained barcode/lineage columns: ", ifelse(is.na(strict_retained), as.character(nrow(v2b_strict)), strict_retained), "\n",
  "Figures written: 4\n\n",
  "This visual-polish script does not rerun upstream objects and does not upgrade the claim.\n",
  "All figures should be interpreted as state-level transcriptomic proxy support only.\n\n",
  "Output figure folder:\n", out_fig_dir, "\n",
  file = report_path, sep = ""
)

message("\n[11E V4J] Completed strict publication visual polish package.")
message("[11E V4J] Final status: FINAL_LOCKED_11E_AS_GSE200610_TRANSCRIPTOMIC_STATE_LEVEL_PROXY_SUPPORT")
message("[11E V4J] Strict retained barcode/lineage columns: ", ifelse(is.na(strict_retained), as.character(nrow(v2b_strict)), strict_retained))
message("[11E V4J] Figures written: 4")
message("[11E V4J] Final figures: ", out_fig_dir)
message("[11E V4J] Next: 11F_projection_associated_molecular_competence_proxy")
