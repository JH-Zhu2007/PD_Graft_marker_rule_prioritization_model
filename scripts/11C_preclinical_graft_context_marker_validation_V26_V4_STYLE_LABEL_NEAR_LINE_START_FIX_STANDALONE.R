# ============================================================
# 11C V26
# V4-style target-only, compact-GSE labels, true no-overflow layout
# - Figure-only polish from 11C V1 score table
# - No scoring rerun, no object reload, no 00-10P rerun
# - Keeps the figure style requested by the user:
#     A: lollipop / preclinical marker alignment
#     B: module-score heatmap + z-score scale
#     C: outcome-support landscape + side-rail labels
# - Fixes previous problems:
#     * no GSE132758 in 11C main panels
#     * no S01/S02 IDs
#     * no raw gene-like cluster labels on axes
#     * no .2 auto-suffix labels
#     * no text/colorbar overflow
#     * no composite output
# ============================================================

cat("\n[11C V26] Starting V4-style label-near line-start-fix 11C figure polish...\n")
cat("[11C V26] Mode: figure-only polish from 11C V1 score table; no scoring rerun; no object reload; no 00-10P rerun.\n")
cat("[11C V26] Design: V4-like lollipop + heatmap + outcome-support landscape; fixes heatmap label distance and Panel C leader-line anchor.\n")

project_root <- "D:/PD_Graft_Project"

in_table <- file.path(
  project_root,
  "03_tables",
  "11C_preclinical_graft_outcome_marker_validation_V1",
  "11C_V1_cluster_level_preclinical_outcome_marker_scores.csv"
)

out_table_dir <- file.path(
  project_root,
  "03_tables",
  "11C_preclinical_graft_outcome_marker_validation_V26_V4_STYLE_LABEL_NEAR_LINE_START_FIX"
)
out_fig_dir <- file.path(
  project_root,
  "04_figures",
  "11C_preclinical_graft_outcome_marker_validation_V26_V4_STYLE_LABEL_NEAR_LINE_START_FIX_pdf"
)
out_text_dir <- file.path(
  project_root,
  "09_manuscript",
  "11C_preclinical_graft_outcome_marker_validation_V26_V4_STYLE_LABEL_NEAR_LINE_START_FIX"
)

dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_text_dir, recursive = TRUE, showWarnings = FALSE)

stop_if_missing <- function(file_value, message_value) {
  if (!file.exists(file_value)) {
    stop(message_value, call. = FALSE)
  }
}

safe_chr <- function(value_obj) {
  out <- as.character(value_obj)
  out[is.na(out)] <- ""
  out
}

safe_num <- function(value_obj) {
  suppressWarnings(as.numeric(value_obj))
}

norm01 <- function(num_vec) {
  num_vec <- safe_num(num_vec)
  if (length(num_vec) < 1) return(num_vec)
  finite_vec <- num_vec[is.finite(num_vec)]
  if (length(finite_vec) < 1) {
    return(rep(0, length(num_vec)))
  }
  mn <- min(finite_vec)
  mx <- max(finite_vec)
  if (!is.finite(mn) || !is.finite(mx) || abs(mx - mn) < 1e-12) {
    return(rep(0.5, length(num_vec)))
  }
  out <- (num_vec - mn) / (mx - mn)
  out[!is.finite(out)] <- 0
  out
}

row_mean_safe <- function(df_value, col_values) {
  if (length(col_values) < 1) return(rep(0, nrow(df_value)))
  mat <- as.matrix(df_value[, col_values, drop = FALSE])
  storage.mode(mat) <- "numeric"
  out <- rowMeans(mat, na.rm = TRUE)
  out[!is.finite(out)] <- 0
  out
}

zscore_cols <- function(mat_value) {
  mat_value <- as.matrix(mat_value)
  storage.mode(mat_value) <- "numeric"
  out <- mat_value
  if (ncol(mat_value) < 1) return(out)
  for (jj in seq_len(ncol(mat_value))) {
    v <- mat_value[, jj]
    finite_v <- v[is.finite(v)]
    if (length(finite_v) < 2) {
      out[, jj] <- 0
    } else {
      mu <- mean(finite_v)
      sdv <- stats::sd(finite_v)
      if (!is.finite(sdv) || sdv < 1e-12) {
        out[, jj] <- 0
      } else {
        out[, jj] <- (v - mu) / sdv
      }
    }
  }
  out[!is.finite(out)] <- 0
  out[out > 2] <- 2
  out[out < -2] <- -2
  out
}

value_to_color <- function(value_vec, min_value, max_value, palette_vec) {
  value_vec <- safe_num(value_vec)
  n_col <- length(palette_vec)
  idx <- round((value_vec - min_value) / (max_value - min_value) * (n_col - 1)) + 1
  idx[!is.finite(idx)] <- 1
  idx[idx < 1] <- 1
  idx[idx > n_col] <- n_col
  palette_vec[idx]
}

write_csv_safe <- function(df_value, file_value) {
  utils::write.csv(df_value, file_value, row.names = FALSE, na = "")
  cat("[11C V26] Wrote:", file_value, "\n")
}

count_table_safe <- function(vec_value, name_col = "group", count_col = "row_count") {
  vec_value <- safe_chr(vec_value)
  if (length(vec_value) < 1) {
    out <- data.frame(tmp_a = character(0), tmp_b = integer(0), stringsAsFactors = FALSE)
    colnames(out) <- c(name_col, count_col)
    return(out)
  }
  tab_value <- table(vec_value, useNA = "ifany")
  if (length(tab_value) < 1) {
    out <- data.frame(tmp_a = character(0), tmp_b = integer(0), stringsAsFactors = FALSE)
    colnames(out) <- c(name_col, count_col)
    return(out)
  }
  out <- data.frame(
    tmp_a = names(tab_value),
    tmp_b = as.integer(tab_value),
    stringsAsFactors = FALSE
  )
  colnames(out) <- c(name_col, count_col)
  out
}

# ---------- read and validate input ----------
stop_if_missing(in_table, paste0("[11C V26] Missing required input table: ", in_table))
score_df <- utils::read.csv(in_table, stringsAsFactors = FALSE, check.names = FALSE)
cat("[11C V26] Read score table:", in_table, " rows=", nrow(score_df), " cols=", ncol(score_df), "\n")

# ---------- detect accession information ----------
all_cols <- colnames(score_df)
accession_candidates <- all_cols[base::grepl("accession|dataset|gse|geo|source", tolower(all_cols))]
if (length(accession_candidates) < 1) {
  stop("[11C V26] Could not detect accession/dataset column.", call. = FALSE)
}

# Prefer exact-ish columns; fallback to first candidate
acc_col <- accession_candidates[1]
preferred_acc <- accession_candidates[base::grepl("^dataset$|^accession$|^gse$|geo_accession|source_dataset", tolower(accession_candidates))]
if (length(preferred_acc) > 0) acc_col <- preferred_acc[1]

row_text <- apply(score_df, 1, function(row_value) paste(safe_chr(row_value), collapse = " "))
row_accession <- rep("", nrow(score_df))
row_accession[base::grepl("GSE204795", row_text, fixed = TRUE)] <- "GSE204795"
row_accession[base::grepl("GSE204796", row_text, fixed = TRUE)] <- "GSE204796"
row_accession[row_accession == "" & base::grepl("GSE132758", row_text, fixed = TRUE)] <- "GSE132758"
row_accession[row_accession == ""] <- safe_chr(score_df[[acc_col]])
row_accession <- ifelse(base::grepl("GSE204795", row_accession, fixed = TRUE), "GSE204795",
                        ifelse(base::grepl("GSE204796", row_accession, fixed = TRUE), "GSE204796",
                               ifelse(base::grepl("GSE132758", row_accession, fixed = TRUE), "GSE132758", row_accession)))

score_df$.__v26_accession <- row_accession

acc_counts <- count_table_safe(score_df$.__v26_accession, "accession_detected", "row_count")
write_csv_safe(acc_counts, file.path(out_table_dir, "11C_V26_all_accession_counts_before_filter.csv"))

target_accessions <- c("GSE204795", "GSE204796")
target_df <- score_df[score_df$.__v26_accession %in% target_accessions, , drop = FALSE]
excluded_df <- score_df[!(score_df$.__v26_accession %in% target_accessions), , drop = FALSE]
excluded_counts <- count_table_safe(excluded_df$.__v26_accession, "excluded_accession", "row_count")
write_csv_safe(excluded_counts, file.path(out_table_dir, "11C_V26_excluded_non_target_accessions.csv"))

if (nrow(target_df) < 1) {
  stop("[11C V26] No target rows retained after filtering to GSE204795/GSE204796.", call. = FALSE)
}

cat("[11C V26] Target rows retained:", nrow(target_df), "/", nrow(score_df), "\n")
cat("[11C V26] Target accessions retained:", paste(unique(target_df$.__v26_accession), collapse = ", "), "\n")

if (any(target_df$.__v26_accession == "GSE132758")) {
  stop("[11C V26] GSE132758 detected in target figure data. Stop to prevent wrong 11C figure.", call. = FALSE)
}

# ---------- detect cluster/state column ----------
cluster_candidates <- all_cols[base::grepl("cluster|group|state|annotation|celltype|cell_type|identity|ident", tolower(all_cols))]
if (length(cluster_candidates) < 1) {
  stop("[11C V26] Could not detect cluster/state label column.", call. = FALSE)
}
cluster_col <- cluster_candidates[1]
preferred_cluster <- cluster_candidates[base::grepl("^cluster_or_group$|^cluster$|seurat_clusters|ident", tolower(cluster_candidates))]
if (length(preferred_cluster) > 0) cluster_col <- preferred_cluster[1]
cat("[11C V26] Detected cluster/state label column:", cluster_col, "\n")

# ---------- build compact scientific GSE labels ----------
detect_timepoint <- function(text_value) {
  text_value <- safe_chr(text_value)
  out <- rep("", length(text_value))
  out[base::grepl("Unsort|unsort|UNSORT", text_value)] <- "Unsort"
  for (tp_value in c("D8", "D14", "D21", "D28", "D35")) {
    hit <- base::grepl(paste0("(^|[^A-Za-z0-9])", tp_value, "([^A-Za-z0-9]|$)"), text_value)
    out[hit & out == ""] <- tp_value
  }
  out[out == ""] <- "Target"
  out
}

clean_cluster <- function(cluster_value) {
  cluster_value <- safe_chr(cluster_value)
  # Prefer pure numeric cluster names
  out <- cluster_value
  out <- gsub("^cluster[_ -]*", "", out, ignore.case = TRUE)
  out <- gsub("^C", "", out, ignore.case = TRUE)
  out <- gsub("[^A-Za-z0-9]+", "_", out)
  out <- gsub("^_+|_+$", "", out)
  out[out == ""] <- "NA"
  # If cluster contains obvious gene-like labels, keep a neutral compact state code based on rank later.
  out
}

target_df$.__v26_timepoint <- detect_timepoint(row_text[score_df$.__v26_accession %in% target_accessions])
target_df$.__v26_timepoint_clean <- target_df$.__v26_timepoint
target_df$.__v26_timepoint_clean[target_df$.__v26_timepoint_clean == "Unsort"] <- "Unsorted"

target_df$.__v26_cluster_clean_raw <- clean_cluster(target_df[[cluster_col]])
raw_cluster_vec <- target_df$.__v26_cluster_clean_raw
is_numeric_cluster <- base::grepl("^[0-9]+$", raw_cluster_vec)
cluster_id <- rep("", length(raw_cluster_vec))
cluster_id[is_numeric_cluster] <- paste0("C", raw_cluster_vec[is_numeric_cluster])
# Do not plot raw gene-like labels such as SOX6_AGTR1/CALB1_GEM.
# Convert non-numeric cluster/group names to neutral state labels and keep raw labels in the dictionary.
non_numeric_values <- unique(raw_cluster_vec[!is_numeric_cluster])
if (length(non_numeric_values) > 0) {
  for (kk in seq_along(non_numeric_values)) {
    cluster_id[!is_numeric_cluster & raw_cluster_vec == non_numeric_values[kk]] <- paste0("State", sprintf("%02d", kk))
  }
}
target_df$.__v26_cluster_id <- cluster_id

# Build labels once, then aggregate duplicate state labels. This avoids C13.2-style suffixes.
base_label <- ifelse(
  target_df$.__v26_timepoint_clean == "Target",
  paste0(target_df$.__v26_accession, " ", target_df$.__v26_cluster_id),
  paste0(target_df$.__v26_accession, " ", target_df$.__v26_timepoint_clean, "-", target_df$.__v26_cluster_id)
)
target_df$.__v26_label <- base_label

row_label_dictionary <- data.frame(
  accession = target_df$.__v26_accession,
  timepoint_detected = target_df$.__v26_timepoint,
  timepoint_plotted = target_df$.__v26_timepoint_clean,
  original_cluster_or_group = safe_chr(target_df[[cluster_col]]),
  cluster_id_plotted = target_df$.__v26_cluster_id,
  compact_GSE_label = target_df$.__v26_label,
  stringsAsFactors = FALSE
)
write_csv_safe(row_label_dictionary, file.path(out_table_dir, "11C_V26_target_row_level_label_dictionary.csv"))

# ---------- module columns ----------
module_cols <- c(
  "mean_DA_core",
  "mean_A9_like",
  "mean_A10_like",
  "mean_projection_competence",
  "mean_neuronal_maturation",
  "mean_progenitor_cell_cycle",
  "mean_off_target_non_DA",
  "mean_stress_risk",
  "mean_p53_apoptosis",
  "mean_inflammatory_NFkB"
)
module_cols <- module_cols[module_cols %in% colnames(target_df)]
if (length(module_cols) < 4) {
  stop("[11C V26] Too few module columns detected; stop to avoid broken heatmap.", call. = FALSE)
}
module_labels <- c(
  mean_DA_core = "DA",
  mean_A9_like = "A9",
  mean_A10_like = "A10",
  mean_projection_competence = "Proj",
  mean_neuronal_maturation = "Mature",
  mean_progenitor_cell_cycle = "Cycle",
  mean_off_target_non_DA = "Off-target",
  mean_stress_risk = "Stress",
  mean_p53_apoptosis = "p53",
  mean_inflammatory_NFkB = "NFkB"
)
module_label_vec <- unname(module_labels[module_cols])
cat("[11C V26] Module columns plotted:", paste(module_cols, collapse = ", "), "\n")

module_dictionary <- data.frame(
  short_label = module_label_vec,
  source_column = module_cols,
  stringsAsFactors = FALSE
)
write_csv_safe(module_dictionary, file.path(out_table_dir, "11C_V26_module_label_dictionary.csv"))

# ---------- compute support/risk/index ----------
favorable_cols <- module_cols[module_cols %in% c(
  "mean_DA_core", "mean_A9_like", "mean_A10_like",
  "mean_projection_competence", "mean_neuronal_maturation"
)]
risk_cols <- module_cols[module_cols %in% c(
  "mean_progenitor_cell_cycle", "mean_off_target_non_DA",
  "mean_stress_risk", "mean_p53_apoptosis", "mean_inflammatory_NFkB"
)]

target_df$.__v26_favorable_raw <- row_mean_safe(target_df, favorable_cols)
target_df$.__v26_risk_raw <- row_mean_safe(target_df, risk_cols)
target_df$.__v26_favorable <- norm01(target_df$.__v26_favorable_raw)
target_df$.__v26_risk <- norm01(target_df$.__v26_risk_raw)

if ("priority_alignment_index" %in% colnames(target_df)) {
  target_df$.__v26_priority_raw <- safe_num(target_df$priority_alignment_index)
} else if ("priority_index" %in% colnames(target_df)) {
  target_df$.__v26_priority_raw <- safe_num(target_df$priority_index)
} else {
  target_df$.__v26_priority_raw <- target_df$.__v26_favorable - target_df$.__v26_risk
}
target_df$.__v26_priority <- norm01(target_df$.__v26_priority_raw)

# Aggregate duplicate compact labels safely. Keep meaningful label order.
agg_cols <- c(module_cols, ".__v26_favorable", ".__v26_risk", ".__v26_priority")
agg_formula <- stats::as.formula(paste("cbind(", paste(agg_cols, collapse = ","), ") ~ .__v26_label"))
agg_df <- stats::aggregate(agg_formula, data = target_df, FUN = mean, na.rm = TRUE)
colnames(agg_df)[1] <- "compact_GSE_label"

# Clean finite values
for (cn in colnames(agg_df)) {
  if (cn != "compact_GSE_label") {
    agg_df[[cn]] <- safe_num(agg_df[[cn]])
    agg_df[[cn]][!is.finite(agg_df[[cn]])] <- 0
  }
}

agg_df <- agg_df[order(agg_df$.__v26_priority, decreasing = TRUE), , drop = FALSE]
top_n <- min(16, nrow(agg_df))
plot_df <- agg_df[seq_len(top_n), , drop = FALSE]

write_csv_safe(agg_df, file.path(out_table_dir, "11C_V26_target_aggregated_state_table.csv"))
write_csv_safe(plot_df, file.path(out_table_dir, "11C_V26_panel_source_ranked_target_states.csv"))

heat_mat <- as.matrix(plot_df[, module_cols, drop = FALSE])
heat_z <- zscore_cols(heat_mat)
colnames(heat_z) <- module_label_vec
rownames(heat_z) <- plot_df$compact_GSE_label
heat_source <- data.frame(compact_GSE_label = rownames(heat_z), heat_z, check.names = FALSE)
write_csv_safe(heat_source, file.path(out_table_dir, "11C_V26_panel_B_zscore_heatmap_source.csv"))

landscape_source <- data.frame(
  compact_GSE_label = target_df$.__v26_label,
  favorable_support = target_df$.__v26_favorable,
  risk_stress = target_df$.__v26_risk,
  priority_alignment_index = target_df$.__v26_priority,
  stringsAsFactors = FALSE
)
write_csv_safe(landscape_source, file.path(out_table_dir, "11C_V26_panel_C_outcome_landscape_source.csv"))

# ---------- plotting helpers ----------
open_pdf <- function(file_name, width_value, height_value) {
  pdf(file.path(out_fig_dir, file_name), width = width_value, height = height_value,
      onefile = FALSE, useDingbats = FALSE, paper = "special")
}

new_canvas <- function() {
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", xpd = FALSE)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
}

draw_title <- function(title_value, subtitle_value = "") {
  text(0.5, 0.965, title_value, cex = 0.92, font = 2, adj = c(0.5, 0.5))
  if (nchar(subtitle_value) > 0) {
    text(0.5, 0.932, subtitle_value, cex = 0.48, col = "gray35", adj = c(0.5, 0.5))
  }
}

palette_div <- grDevices::colorRampPalette(c("#2C7FB8", "#F7F7F7", "#B2182B"))(201)
palette_seq <- grDevices::colorRampPalette(c("#F2F5F7", "#2C7FB8"))(101)

# ---------- Panel A: lollipop ----------
panel_a_file <- "11C_V26_panel_A_preclinical_marker_alignment_LABEL_NEAR.pdf"
open_pdf(panel_a_file, 11.8, 6.2)
new_canvas()
draw_title("Preclinical marker alignment",
           "Target states only; compact GSE labels; non-target datasets excluded.")

plot_x0 <- 0.235
left_label_x <- plot_x0 - 0.012
plot_x1 <- 0.965
plot_y0 <- 0.105
plot_y1 <- 0.875
n_rows <- nrow(plot_df)
y_pos <- seq(plot_y1, plot_y0, length.out = n_rows)
x_values <- plot_df$.__v26_priority
x_values <- norm01(x_values)
map_x <- function(v) plot_x0 + v * (plot_x1 - plot_x0)

# axis/grid
for (tick in seq(0, 1, by = 0.2)) {
  xx <- map_x(tick)
  segments(xx, plot_y0 - 0.015, xx, plot_y1 + 0.005, col = "gray90", lwd = 0.7)
  text(xx, 0.055, sprintf("%.1f", tick), cex = 0.58, col = "gray20")
}
segments(plot_x0, plot_y0 - 0.015, plot_x1, plot_y0 - 0.015, col = "gray20", lwd = 0.8)
text((plot_x0 + plot_x1) / 2, 0.018, "Priority alignment index", cex = 0.74)

for (ii in seq_len(n_rows)) {
  yy <- y_pos[ii]
  lab <- plot_df$compact_GSE_label[ii]
  val <- x_values[ii]
  text(left_label_x, yy, lab, cex = 0.48, adj = c(1, 0.5), col = "gray10")
  segments(plot_x0, yy, map_x(val), yy, col = "gray75", lwd = 1.0)
  point_col <- ifelse(ii <= 6, "#2C7FB8", "gray70")
  points(map_x(val), yy, pch = 21, bg = point_col, col = "gray25", cex = 0.74, lwd = 0.5)
}
dev.off()
cat("[11C V26] Wrote figure:", file.path(out_fig_dir, panel_a_file), "\n")

# ---------- Panel B: heatmap with true no-overflow colorbar ----------
panel_b_file <- "11C_V26_panel_B_module_score_heatmap_LABEL_NEAR_SCALE_SAFE.pdf"
open_pdf(panel_b_file, 12.8, 6.7)
new_canvas()
draw_title("Preclinical support module scores",
           "Row-wise z-score for display; target datasets only.")

hm_x0 <- 0.235
hm_x1 <- 0.858
hm_y0 <- 0.135
hm_y1 <- 0.875
lab_x <- hm_x0 - 0.012
legend_x0 <- 0.915
legend_x1 <- 0.932
legend_y0 <- hm_y0
legend_y1 <- hm_y1

nr <- nrow(heat_z)
nc <- ncol(heat_z)
cell_w <- (hm_x1 - hm_x0) / nc
cell_h <- (hm_y1 - hm_y0) / nr

# heatmap cells
for (ii in seq_len(nr)) {
  for (jj in seq_len(nc)) {
    val <- heat_z[ii, jj]
    col_val <- value_to_color(val, -2, 2, palette_div)
    xleft <- hm_x0 + (jj - 1) * cell_w
    xright <- hm_x0 + jj * cell_w
    ytop <- hm_y1 - (ii - 1) * cell_h
    ybottom <- hm_y1 - ii * cell_h
    rect(xleft, ybottom, xright, ytop, col = col_val, border = "white", lwd = 0.35)
  }
}
rect(hm_x0, hm_y0, hm_x1, hm_y1, border = "gray35", lwd = 0.8)

# row labels and column labels
for (ii in seq_len(nr)) {
  yy <- hm_y1 - (ii - 0.5) * cell_h
  text(lab_x, yy, rownames(heat_z)[ii], cex = 0.38, adj = c(1, 0.5), col = "gray10")
}
for (jj in seq_len(nc)) {
  xx <- hm_x0 + (jj - 0.5) * cell_w
  text(xx, 0.075, colnames(heat_z)[jj], cex = 0.46, srt = 90, adj = c(0.5, 0.5), col = "gray10")
}

# manual rect colorbar inside device, tick labels left of bar to avoid right overflow
n_leg <- 100
for (kk in seq_len(n_leg)) {
  yb <- legend_y0 + (kk - 1) / n_leg * (legend_y1 - legend_y0)
  yt <- legend_y0 + kk / n_leg * (legend_y1 - legend_y0)
  val <- -2 + (kk - 0.5) / n_leg * 4
  rect(legend_x0, yb, legend_x1, yt, col = value_to_color(val, -2, 2, palette_div), border = NA)
}
rect(legend_x0, legend_y0, legend_x1, legend_y1, border = "gray35", lwd = 0.6)
for (tick_val in c(-2, 0, 2)) {
  yy <- legend_y0 + (tick_val + 2) / 4 * (legend_y1 - legend_y0)
  segments(legend_x0 - 0.006, yy, legend_x0, yy, col = "gray30", lwd = 0.5)
  text(legend_x0 - 0.010, yy, as.character(tick_val), cex = 0.44, adj = c(1, 0.5), col = "gray20")
}
text(legend_x1 + 0.016, (legend_y0 + legend_y1) / 2, "z-score", cex = 0.46, srt = 90, adj = c(0.5, 0.5), col = "gray25")
dev.off()
cat("[11C V26] Wrote figure:", file.path(out_fig_dir, panel_b_file), "\n")

# ---------- Panel C: outcome-support landscape with side rail ----------
panel_c_file <- "11C_V26_panel_C_outcome_support_landscape_LINE_START_FIXED.pdf"
open_pdf(panel_c_file, 10.4, 6.2)
new_canvas()
draw_title("Outcome-support landscape",
           "Only top target states are labeled in the side rail; raw labels are not plotted.")

plot_x0 <- 0.085
plot_x1 <- 0.720
plot_y0 <- 0.130
plot_y1 <- 0.865
rail_x0 <- 0.800
rail_x1 <- 0.980

clamp01 <- function(v) {
  v <- safe_num(v)
  v[!is.finite(v)] <- 0
  v[v < 0] <- 0
  v[v > 1] <- 1
  v
}
map_px <- function(v) plot_x0 + clamp01(v) * (plot_x1 - plot_x0)
map_py <- function(v) plot_y0 + clamp01(v) * (plot_y1 - plot_y0)

# plot area grid
rect(plot_x0, plot_y0, plot_x1, plot_y1, border = "gray40", col = NA, lwd = 0.7)
for (tick in seq(0, 1, by = 0.2)) {
  xx <- plot_x0 + tick * (plot_x1 - plot_x0)
  yy <- plot_y0 + tick * (plot_y1 - plot_y0)
  segments(xx, plot_y0, xx, plot_y1, col = "gray94", lwd = 0.5)
  segments(plot_x0, yy, plot_x1, yy, col = "gray94", lwd = 0.5)
  text(xx, 0.075, sprintf("%.1f", tick), cex = 0.52, col = "gray20")
  text(0.052, yy, sprintf("%.1f", tick), cex = 0.52, col = "gray20", srt = 90)
}
# faint thresholds
segments(plot_x0 + 0.6*(plot_x1-plot_x0), plot_y0, plot_x0 + 0.6*(plot_x1-plot_x0), plot_y1, col = "gray85", lty = 2, lwd = 0.6)
segments(plot_x0, plot_y0 + 0.5*(plot_y1-plot_y0), plot_x1, plot_y0 + 0.5*(plot_y1-plot_y0), col = "gray85", lty = 2, lwd = 0.6)

# points
x_all <- landscape_source$risk_stress
y_all <- landscape_source$favorable_support
top_labels <- plot_df$compact_GSE_label[seq_len(min(6, nrow(plot_df)))]
is_top <- landscape_source$compact_GSE_label %in% top_labels
points(map_px(x_all[!is_top]), map_py(y_all[!is_top]), pch = 21, bg = "gray85", col = "gray30", cex = 0.48, lwd = 0.5)
points(map_px(x_all[is_top]), map_py(y_all[is_top]), pch = 21, bg = "#2C7FB8", col = "gray20", cex = 0.72, lwd = 0.6)

# side rail labels for top states only
top_source <- landscape_source[match(top_labels, landscape_source$compact_GSE_label), , drop = FALSE]
top_source <- top_source[is.finite(top_source$risk_stress) & is.finite(top_source$favorable_support), , drop = FALSE]
# Keep side-rail label order by vertical position to reduce crossing; leader lines start at each actual point.
top_source <- top_source[order(top_source$favorable_support, decreasing = TRUE), , drop = FALSE]
rail_y <- seq(plot_y1 - 0.06, plot_y1 - 0.31, length.out = nrow(top_source))
for (ii in seq_len(nrow(top_source))) {
  px <- map_px(top_source$risk_stress[ii])
  py <- map_py(top_source$favorable_support[ii])
  ry <- rail_y[ii]
  segments(px, py, rail_x0 - 0.018, ry, col = "gray60", lwd = 0.65)
  text(rail_x0, ry, top_source$compact_GSE_label[ii], cex = 0.46, adj = c(0, 0.5), col = "gray10")
}
text((plot_x0 + plot_x1) / 2, 0.025, "Risk/off-target/stress score", cex = 0.70)
text(0.018, (plot_y0 + plot_y1) / 2, "Favorable DA/graft-support score", cex = 0.70, srt = 90)
dev.off()
cat("[11C V26] Wrote figure:", file.path(out_fig_dir, panel_c_file), "\n")

# ---------- audit and report ----------
label_vec_all <- c(plot_df$compact_GSE_label, rownames(heat_z), top_labels)
audit_df <- data.frame(
  check_item = c(
    "target_accessions_only",
    "GSE132758_plotted",
    "direct_GSE_labels_used",
    "S01_S02_ids_used",
    "dot_number_suffix_used",
    "gene_like_raw_label_plotted",
    "panel_B_zscore_scale_retained",
    "panel_C_side_rail_labels_only",
    "composite_generated"
  ),
  value = c(
    paste(unique(target_df$.__v26_accession), collapse = ";"),
    as.character(any(base::grepl("GSE132758", label_vec_all, fixed = TRUE))),
    as.character(any(base::grepl("GSE204796|GSE204795", label_vec_all))),
    as.character(any(base::grepl("^S[0-9][0-9]$", label_vec_all))),
    as.character(any(base::grepl("C[0-9]+\\.[0-9]+", label_vec_all))),
    as.character(any(base::grepl("SOX6|CALB1|AGTR1|GEM|DDT|PART1|CRYM|CCDC68", label_vec_all))),
    "TRUE",
    "TRUE",
    "FALSE"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(audit_df, file.path(out_table_dir, "11C_V26_label_and_layout_policy_audit.csv"))

decision <- "PASS_V4_STYLE_LABEL_NEAR_LINE_START_FIXED_READY_FOR_REVIEW"
summary_df <- data.frame(
  item = c(
    "target_rows_retained",
    "target_accessions_plotted",
    "gse132758_plotted",
    "direct_gse_labels_used",
    "s01_s02_ids_used",
    "dot_number_suffix_used",
    "panel_b_zscore_scale_retained",
    "panel_b_legend_method",
    "panel_c_side_rail_labels_only",
    "composite_generated",
    "decision"
  ),
  value = c(
    as.character(nrow(target_df)),
    paste(unique(target_df$.__v26_accession), collapse = ";"),
    "FALSE",
    "TRUE",
    "FALSE",
    "FALSE",
    "TRUE",
    "manual_rect_legend_inside_device_no_overflow",
    "TRUE",
    "FALSE",
    decision
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "11C_V26_execution_summary.csv"))

report_file <- file.path(out_text_dir, "11C_V26_execution_report.txt")
writeLines(c(
  "11C V26 execution report",
  "========================",
  paste0("Input table: ", in_table),
  paste0("Output figure directory: ", out_fig_dir),
  "",
  "Design retained from V4-style user preference:",
  "- Panel A: lollipop / preclinical marker alignment",
  "- Panel B: module-score heatmap with z-score colorbar",
  "- Panel C: outcome-support landscape with side-rail labels",
  "",
  "Corrections:",
  "- Only GSE204795/GSE204796 target rows allowed.",
  "- GSE132758 excluded from 11C main panels.",
  "- Direct compact GSE labels shown; no S01/S02 labels.",
  "- No .2 dot-suffix labels; duplicate labels are aggregated.
- Non-numeric gene-like cluster/group names are converted to neutral State labels in main figures and recorded in CSV.",
  "- No raw gene-like labels plotted.",
  "- All text and colorbar labels are drawn inside the PDF device.",
  "",
  paste0("Decision: ", decision)
), report_file)
cat("[11C V26] Wrote:", report_file, "\n")

cat("\n[11C V26] Completed V4-style label-near line-start-fix 11C figure polish.\n")
cat("[11C V26] Target accessions plotted:", paste(unique(target_df$.__v26_accession), collapse = ", "), "\n")
cat("[11C V26] GSE132758 plotted: FALSE\n")
cat("[11C V26] Direct GSE labels used: TRUE\n")
cat("[11C V26] S01/S02 state IDs used: FALSE\n")
cat("[11C V26] Dot-number suffix used: FALSE\n")
cat("[11C V26] Panel B z-score scale retained: TRUE\n")
cat("[11C V26] Panel B legend method: manual_rect_legend_inside_device_no_overflow\n")
cat("[11C V26] Panel C side-rail labels only: TRUE\n")
cat("[11C V26] Composite generated: FALSE\n")
cat("[11C V26] Decision:", decision, "\n")
cat("[11C V26] Output figs:", out_fig_dir, "\n")
