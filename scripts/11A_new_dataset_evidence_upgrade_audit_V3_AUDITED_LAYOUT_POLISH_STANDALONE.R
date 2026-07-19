# ============================================================
# 11A_new_dataset_evidence_upgrade_audit_V3_AUDITED_LAYOUT_POLISH_STANDALONE.R
# Project: PD_Graft_Project
# Purpose:
#   Re-draw 11A evidence-upgrade map as a clean, publication-style, audited-layout PDF.
#   This script does NOT rerun 00-10P, does NOT download data, and does NOT change analysis tables.
#
# Fixes compared with V1/V2:
#   - Shorter node text; no dense paragraphs inside boxes.
#   - Fixed canvas-safe coordinates.
#   - Explicit layout audit for label boxes and out-of-bound checks.
#   - No overlapping arrows through text.
#   - Writes only polished source-panel PDFs and audit tables.
# ============================================================

options(stringsAsFactors = FALSE)
set.seed(110103)

cat("\n[11A V3] Starting audited-layout figure polish...\n")
cat("[11A V3] No 00-10P rerun. No download. Figure-only polish.\n")

project_root <- "D:/PD_Graft_Project"
dir_in_tables <- file.path(project_root, "03_tables", "11A_new_dataset_evidence_upgrade_audit_V1")
dir_tables <- file.path(project_root, "03_tables", "11A_new_dataset_evidence_upgrade_audit_V3_AUDITED_LAYOUT")
dir_figs <- file.path(project_root, "04_figures", "11A_new_dataset_evidence_upgrade_audit_V3_AUDITED_LAYOUT_pdf")
dir_text <- file.path(project_root, "09_manuscript", "11A_new_dataset_evidence_upgrade_audit_V3_AUDITED_LAYOUT")

dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_figs, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_text, recursive = TRUE, showWarnings = FALSE)

cat("[11A V3] Project root:", project_root, "\n")
cat("[11A V3] Figures     :", dir_figs, "\n")

safe_chr <- function(val_in) {
  out <- as.character(val_in)
  out[is.na(out)] <- ""
  out
}

write_csv_safe <- function(data_in, file_out) {
  dir.create(dirname(file_out), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data_in, file_out, row.names = FALSE, fileEncoding = "UTF-8")
  cat("[11A V3] Wrote:", file_out, "\n")
}

write_text_safe <- function(lines_in, file_out) {
  dir.create(dirname(file_out), recursive = TRUE, showWarnings = FALSE)
  writeLines(safe_chr(lines_in), con = file_out, useBytes = TRUE)
  cat("[11A V3] Wrote:", file_out, "\n")
}

wrap_label <- function(text_in, width_in = 16) {
  text_in <- safe_chr(text_in)
  out <- character(length(text_in))
  for (ii in seq_along(text_in)) {
    tmp <- strwrap(text_in[ii], width = width_in)
    if (length(tmp) == 0) tmp <- ""
    out[ii] <- paste(tmp, collapse = "\n")
  }
  out
}

box_overlap <- function(a_left, a_right, a_bottom, a_top, b_left, b_right, b_bottom, b_top) {
  !(a_right <= b_left || b_right <= a_left || a_top <= b_bottom || b_top <= a_bottom)
}

make_layout_audit <- function(nodes_in, canvas_left = 0, canvas_right = 1, canvas_bottom = 0, canvas_top = 1) {
  audit_rows <- list()
  for (ii in seq_len(nrow(nodes_in))) {
    n_lines <- length(strsplit(safe_chr(nodes_in$label_wrapped[ii]), "\n", fixed = TRUE)[[1]])
    est_chars <- max(nchar(strsplit(safe_chr(nodes_in$label_wrapped[ii]), "\n", fixed = TRUE)[[1]]), na.rm = TRUE)
    if (!is.finite(est_chars)) est_chars <- 0
    left <- nodes_in$x[ii] - nodes_in$w[ii] / 2
    right <- nodes_in$x[ii] + nodes_in$w[ii] / 2
    bottom <- nodes_in$y[ii] - nodes_in$h[ii] / 2
    top <- nodes_in$y[ii] + nodes_in$h[ii] / 2
    out_bound <- left < canvas_left || right > canvas_right || bottom < canvas_bottom || top > canvas_top
    audit_rows[[length(audit_rows) + 1]] <- data.frame(
      figure = "11A_V3_evidence_upgrade_map",
      element_id = nodes_in$id[ii],
      label = nodes_in$label[ii],
      n_lines = n_lines,
      max_chars_per_line = est_chars,
      box_left = left,
      box_right = right,
      box_bottom = bottom,
      box_top = top,
      out_of_bounds = out_bound,
      overlap_with = "",
      overlap_flag = FALSE,
      stringsAsFactors = FALSE
    )
  }
  audit_df <- do.call(rbind, audit_rows)
  if (nrow(audit_df) > 1) {
    for (ii in seq_len(nrow(audit_df))) {
      hits <- character(0)
      for (jj in seq_len(nrow(audit_df))) {
        if (jj == ii) next
        ov <- box_overlap(audit_df$box_left[ii], audit_df$box_right[ii], audit_df$box_bottom[ii], audit_df$box_top[ii],
                          audit_df$box_left[jj], audit_df$box_right[jj], audit_df$box_bottom[jj], audit_df$box_top[jj])
        if (ov) hits <- c(hits, audit_df$element_id[jj])
      }
      if (length(hits) > 0) {
        audit_df$overlap_with[ii] <- paste(hits, collapse = ";")
        audit_df$overlap_flag[ii] <- TRUE
      }
    }
  }
  audit_df$pass <- !audit_df$out_of_bounds & !audit_df$overlap_flag
  audit_df
}

nodes <- data.frame(
  id = c("baseline", "outcome", "survival", "tracing", "gwas", "integrated", "ml_audit", "final"),
  label = c(
    "00-10P baseline\ntranscriptomic priority",
    "11C\npreclinical outcome markers",
    "11D\nsurvival + CRISPR",
    "11E-F\nbarcode/projection tracing",
    "11G\nhuman PD GWAS context",
    "11H-I\nevidence tier + correlation",
    "11J-K\nROC/PR + source panels",
    "12A-N\nfinal figures + manuscript"
  ),
  x = c(0.12, 0.33, 0.54, 0.76, 0.33, 0.54, 0.76, 0.54),
  y = c(0.73, 0.73, 0.73, 0.73, 0.47, 0.47, 0.47, 0.20),
  w = c(0.17, 0.17, 0.17, 0.18, 0.17, 0.17, 0.17, 0.26),
  h = c(0.115, 0.115, 0.115, 0.115, 0.105, 0.105, 0.105, 0.125),
  group = c("baseline", "evidence", "evidence", "evidence", "context", "integration", "audit", "final"),
  stringsAsFactors = FALSE
)
nodes$label_wrapped <- wrap_label(nodes$label, width_in = 18)

edges <- data.frame(
  from = c("baseline", "outcome", "survival", "outcome", "survival", "tracing", "gwas", "integrated", "ml_audit"),
  to = c("outcome", "survival", "tracing", "gwas", "integrated", "ml_audit", "final", "final", "final"),
  stringsAsFactors = FALSE
)

layout_audit <- make_layout_audit(nodes)
write_csv_safe(nodes, file.path(dir_tables, "11A_V3_evidence_map_node_source_table.csv"))
write_csv_safe(edges, file.path(dir_tables, "11A_V3_evidence_map_edge_source_table.csv"))
write_csv_safe(layout_audit, file.path(dir_tables, "11A_V3_layout_audit.csv"))

fig_out <- file.path(dir_figs, "11A_V3_evidence_upgrade_map_audited_layout.pdf")
grDevices::pdf(fig_out, width = 11.2, height = 5.8, useDingbats = FALSE)
par(mar = c(1.2, 1.0, 2.4, 1.0), xaxs = "i", yaxs = "i")
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

# quiet background bands
rect(0.02, 0.64, 0.98, 0.83, col = "#F7F7F7", border = NA)
rect(0.02, 0.39, 0.98, 0.54, col = "#FBFBFB", border = NA)
rect(0.38, 0.105, 0.70, 0.295, col = "#F7F7F7", border = NA)

# arrows first, below boxes
node_lookup <- setNames(seq_len(nrow(nodes)), nodes$id)
for (ii in seq_len(nrow(edges))) {
  fi <- node_lookup[[edges$from[ii]]]
  ti <- node_lookup[[edges$to[ii]]]
  x0 <- nodes$x[fi]
  y0 <- nodes$y[fi]
  x1 <- nodes$x[ti]
  y1 <- nodes$y[ti]
  # route to avoid passing through labels: vertical/horizontal split for lower nodes
  if (abs(y0 - y1) < 0.05) {
    arrows(x0 + nodes$w[fi]/2 + 0.004, y0, x1 - nodes$w[ti]/2 - 0.004, y1,
           length = 0.08, lwd = 1.1, col = "#5A5A5A")
  } else {
    arrows(x0, y0 - nodes$h[fi]/2 - 0.006, x1, y1 + nodes$h[ti]/2 + 0.006,
           length = 0.08, lwd = 1.1, col = "#5A5A5A")
  }
}

# boxes
fill_map <- c(baseline = "#E9ECEF", evidence = "#E8F1FA", context = "#F3EFE6", integration = "#EAF4EA", audit = "#F4EAF4", final = "#EDEDED")
border_map <- c(baseline = "#4A4A4A", evidence = "#386FA4", context = "#8A6D3B", integration = "#4F7F4F", audit = "#7B4F8A", final = "#4A4A4A")
for (ii in seq_len(nrow(nodes))) {
  left <- nodes$x[ii] - nodes$w[ii]/2
  right <- nodes$x[ii] + nodes$w[ii]/2
  bottom <- nodes$y[ii] - nodes$h[ii]/2
  top <- nodes$y[ii] + nodes$h[ii]/2
  rect(left, bottom, right, top, col = fill_map[[nodes$group[ii]]], border = border_map[[nodes$group[ii]]], lwd = 1.2)
  text(nodes$x[ii], nodes$y[ii], labels = nodes$label_wrapped[ii], cex = 0.74, font = 2, col = "#1F1F1F")
}

text(0.5, 0.94, "Evidence-upgrade roadmap: baseline retained, orthogonal support added", font = 2, cex = 1.15)
text(0.5, 0.895, "Figure-only polish; no analysis rerun. Modules 11C-11G add outcome, perturbation, tracing and genetic-context evidence.", cex = 0.72, col = "#3A3A3A")
text(0.5, 0.045, "Boundary: supports transcriptomic prioritization; does not claim clinical efficacy, clinical safety, or functional host integration.", cex = 0.68, col = "#4D4D4D")
box(col = "#FFFFFF")
dev.off()
cat("[11A V3] Wrote figure:", fig_out, "\n")

execution_summary <- data.frame(
  item = c("nodes", "edges", "out_of_bounds_flags", "overlap_flags", "layout_audit_decision", "figure_file"),
  value = c(
    as.character(nrow(nodes)),
    as.character(nrow(edges)),
    as.character(sum(layout_audit$out_of_bounds, na.rm = TRUE)),
    as.character(sum(layout_audit$overlap_flag, na.rm = TRUE)),
    ifelse(all(layout_audit$pass), "PASS_NO_TEXT_BOX_OVERLAP_OR_OUT_OF_BOUNDS", "REVIEW_LAYOUT_AUDIT"),
    fig_out
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(execution_summary, file.path(dir_tables, "11A_V3_execution_summary.csv"))
write_text_safe(c(
  "11A V3 audited-layout polish report",
  "",
  paste0("Figure: ", fig_out),
  paste0("Out-of-bounds flags: ", sum(layout_audit$out_of_bounds, na.rm = TRUE)),
  paste0("Overlap flags: ", sum(layout_audit$overlap_flag, na.rm = TRUE)),
  paste0("Decision: ", execution_summary$value[execution_summary$item == "layout_audit_decision"]),
  "",
  "This script only redraws the 11A evidence map. It does not rerun analysis or download data."
), file.path(dir_text, "11A_V3_audited_layout_report.txt"))

cat("\n[11A V3] Completed.\n")
cat("[11A V3] Decision:", execution_summary$value[execution_summary$item == "layout_audit_decision"], "\n")
