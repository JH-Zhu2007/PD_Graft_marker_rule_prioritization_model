# 03B_dataset_merge_figure_repair_V1_COMPLETE_STANDALONE.R
# Purpose:
#   Repair empty 03B dataset-merge figure folders by exporting safe, review-grade PDFs
#   from existing local Seurat/RDS objects.
#
# What this script does:
#   - Does NOT rerun 00-10P.
#   - Does NOT modify objects.
#   - Does NOT overwrite analysis tables.
#   - Scans existing 02_objects RDS files for expected 03B datasets.
#   - Writes per-dataset object inventory/count summaries.
#   - If UMAP/TSNE/PCA embeddings exist in RDS objects, exports representative
#     reduction plots to fill empty 03B figure folders.
#
# Output:
#   D:/PD_Graft_Project/03_tables/03B_dataset_merge_figure_repair_V1
#   D:/PD_Graft_Project/09_manuscript/03B_dataset_merge_figure_repair_V1
#   D:/PD_Graft_Project/04_figures/03B_dataset_merge/<dataset>/03B_REPAIR_V1_*.pdf
#
# Notes:
#   These are REPAIR/REVIEW figures, not final submission panels.
#   Final paper panels remain controlled by 10P/11/12 figure-lock workflow.

options(stringsAsFactors = FALSE)

cat("\n[03B REPAIR V1] Starting 03B dataset-merge figure repair...\n")
cat("[03B REPAIR V1] Mode: figure-export repair only; no object modification; no 00-10P rerun.\n")

project_root <- "D:/PD_Graft_Project"
objects_root <- file.path(project_root, "02_objects")
fig_root_03B <- file.path(project_root, "04_figures", "03B_dataset_merge")
table_out <- file.path(project_root, "03_tables", "03B_dataset_merge_figure_repair_V1")
text_out <- file.path(project_root, "09_manuscript", "03B_dataset_merge_figure_repair_V1")

dir.create(table_out, recursive = TRUE, showWarnings = FALSE)
dir.create(text_out, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_root_03B, recursive = TRUE, showWarnings = FALSE)

expected <- data.frame(
  folder = c("GSE132758", "GSE157783", "GSE178265_DA_01B", "GSE200610", "GSE204796", "GSE233885"),
  accession_match = c("GSE132758", "GSE157783", "GSE178265", "GSE200610", "GSE204796", "GSE233885"),
  repair_role = c(
    "baseline/reference single-cell object set",
    "baseline/reference human midbrain object set",
    "dopaminergic-neuron focused baseline object set",
    "barcode/graft-related object set",
    "time-course/preclinical differentiation object set",
    "projection-tracing related object set"
  ),
  stringsAsFactors = FALSE
)

write_csv_safe <- function(dat, path) {
  tryCatch({
    write.csv(dat, path, row.names = FALSE, fileEncoding = "UTF-8")
    cat("[03B REPAIR V1] Wrote:", path, "\n")
  }, error = function(e) {
    cat("[03B REPAIR V1] Failed writing:", path, " :: ", conditionMessage(e), "\n")
  })
}

safe_chr <- function(v) {
  out <- as.character(v)
  out[is.na(out)] <- ""
  out
}

is_nonempty_file <- function(p) {
  file.exists(p) && !is.na(file.info(p)$size) && file.info(p)$size > 0
}

shorten_path <- function(p, max_chars = 95) {
  p <- gsub("\\\\", "/", as.character(p))
  ifelse(nchar(p) > max_chars, paste0("...", substr(p, nchar(p) - max_chars + 4, nchar(p))), p)
}

list_rds_safe <- function(root_dir) {
  if (!dir.exists(root_dir)) return(character(0))
  out <- tryCatch(
    list.files(root_dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE),
    error = function(e) character(0)
  )
  out
}

get_object_ncells <- function(obj) {
  n <- NA_integer_
  n <- tryCatch({
    if (!is.null(dim(obj))) {
      as.integer(dim(obj)[2])
    } else {
      NA_integer_
    }
  }, error = function(e) NA_integer_)
  if (is.na(n)) {
    n <- tryCatch({
      if (isS4(obj) && "meta.data" %in% slotNames(obj)) {
        as.integer(nrow(obj@meta.data))
      } else {
        NA_integer_
      }
    }, error = function(e) NA_integer_)
  }
  n
}

get_object_meta <- function(obj) {
  meta <- NULL
  meta <- tryCatch({
    if (isS4(obj) && "meta.data" %in% slotNames(obj)) {
      obj@meta.data
    } else {
      NULL
    }
  }, error = function(e) NULL)
  if (is.null(meta)) meta <- data.frame()
  meta
}

get_reduction_names <- function(obj) {
  rn <- character(0)
  rn <- tryCatch({
    if (isS4(obj) && "reductions" %in% slotNames(obj)) {
      names(obj@reductions)
    } else {
      character(0)
    }
  }, error = function(e) character(0))
  rn
}

get_embedding <- function(obj) {
  red_names <- get_reduction_names(obj)
  if (length(red_names) == 0) {
    return(list(method = NA_character_, emb = NULL))
  }
  priority <- c("umap", "UMAP", "tsne", "TSNE", "pca", "PCA")
  chosen <- red_names[match(tolower(priority), tolower(red_names), nomatch = 0)]
  chosen <- chosen[chosen != ""]
  if (length(chosen) == 0) chosen <- red_names[1]
  chosen <- chosen[1]
  emb <- tryCatch({
    obj@reductions[[chosen]]@cell.embeddings
  }, error = function(e) NULL)
  if (is.null(emb) || ncol(emb) < 2 || nrow(emb) < 2) {
    return(list(method = chosen, emb = NULL))
  }
  list(method = chosen, emb = as.matrix(emb[, 1:2, drop = FALSE]))
}

plot_empty_message_pdf <- function(pdf_path, title, lines) {
  grDevices::pdf(pdf_path, width = 9, height = 5.5, useDingbats = FALSE)
  par(mar = c(1, 1, 2, 1))
  plot.new()
  text(0.03, 0.92, title, adj = 0, font = 2, cex = 1.2)
  yy <- 0.78
  for (ln in lines) {
    text(0.03, yy, ln, adj = 0, cex = 0.85)
    yy <- yy - 0.08
  }
  dev.off()
}

make_count_summary_pdf <- function(df, pdf_path, folder_label) {
  grDevices::pdf(pdf_path, width = 8.5, height = 5.2, useDingbats = FALSE)
  par(mar = c(5.0, 5.2, 3.5, 1.2), xaxs = "i")
  if (nrow(df) == 0) {
    plot.new()
    title(main = paste0("03B repair summary: ", folder_label))
    text(0.5, 0.5, "No candidate RDS objects detected.", cex = 1.0)
  } else {
    vals <- df$n_cells
    vals[is.na(vals)] <- 0
    ord <- order(vals, decreasing = TRUE)
    vals <- vals[ord]
    labs <- df$object_short_label[ord]
    if (length(vals) > 12) {
      vals <- vals[1:12]
      labs <- labs[1:12]
    }
    ymax <- max(vals, na.rm = TRUE)
    if (!is.finite(ymax) || ymax <= 0) ymax <- 1
    bp <- barplot(vals, horiz = TRUE, las = 1, xlim = c(0, ymax * 1.18),
                  col = "grey80", border = "grey30", names.arg = labs,
                  cex.names = 0.62, xlab = "Detected cells / nuclei", main = paste0("03B repair summary: ", folder_label))
    text(vals + ymax * 0.02, bp, labels = vals, cex = 0.62, adj = 0)
    mtext("Top detected RDS objects only; repair figure, not final manuscript panel.", side = 3, line = 0.35, cex = 0.65)
  }
  dev.off()
}

make_umap_pages_pdf <- function(readiness_df, pdf_path, folder_label, max_objects = 8, max_points = 6000) {
  ok_df <- readiness_df[readiness_df$read_status == "READ_OK" & readiness_df$has_embedding == TRUE, , drop = FALSE]
  if (nrow(ok_df) == 0) {
    plot_empty_message_pdf(
      pdf_path,
      paste0("03B representative reduction: ", folder_label),
      c("No readable RDS object with UMAP/TSNE/PCA embeddings was detected.",
        "This does not invalidate later modules if downstream tables/figures exist.",
        "Review object inventory before deciding whether to rerun early reductions.")
    )
    return(data.frame(pdf_path = pdf_path, pages = 1, status = "NO_EMBEDDING_AVAILABLE", stringsAsFactors = FALSE))
  }
  ok_df <- ok_df[order(ok_df$n_cells, decreasing = TRUE), , drop = FALSE]
  ok_df <- ok_df[seq_len(min(nrow(ok_df), max_objects)), , drop = FALSE]
  grDevices::pdf(pdf_path, width = 7.5, height = 6.5, useDingbats = FALSE)
  pages <- 0
  for (ii in seq_len(nrow(ok_df))) {
    rds_path <- ok_df$rds_path[ii]
    obj <- tryCatch(readRDS(rds_path), error = function(e) NULL)
    if (is.null(obj)) next
    emb_info <- get_embedding(obj)
    emb <- emb_info$emb
    if (is.null(emb)) next
    n <- nrow(emb)
    idx <- seq_len(n)
    if (n > max_points) {
      set.seed(1000 + ii)
      idx <- sort(sample(idx, max_points))
    }
    x <- emb[idx, 1]
    y <- emb[idx, 2]
    meta <- get_object_meta(obj)
    col_vec <- rep("grey45", length(idx))
    color_by <- "all cells"
    if (nrow(meta) >= nrow(emb)) {
      cn <- colnames(meta)
      candidate_cols <- c("seurat_clusters", "cluster", "celltype", "cell_type", "annotation", "orig.ident", "sample", "timepoint", "day")
      chosen_col <- candidate_cols[candidate_cols %in% cn]
      if (length(chosen_col) > 0) {
        color_by <- chosen_col[1]
        vals <- as.factor(safe_chr(meta[idx, color_by]))
        lv <- levels(vals)
        pal <- grDevices::hcl.colors(max(3, length(lv)), palette = "Dark 3")
        names(pal) <- lv
        col_vec <- pal[as.character(vals)]
      }
    }
    par(mar = c(4.2, 4.2, 3.2, 1.0))
    plot(x, y, pch = 16, cex = 0.18, col = col_vec,
         xlab = paste0(emb_info$method, "_1"), ylab = paste0(emb_info$method, "_2"),
         main = paste0(folder_label, " | representative ", emb_info$method))
    mtext(paste0("Object: ", basename(rds_path), " | color: ", color_by,
                 " | plotted cells: ", length(idx), "/", n),
          side = 3, line = 0.15, cex = 0.55)
    pages <- pages + 1
  }
  dev.off()
  if (pages == 0) {
    plot_empty_message_pdf(
      pdf_path,
      paste0("03B representative reduction: ", folder_label),
      c("Embedding extraction failed after reading candidate objects.",
        "Check object structure in the readiness table.")
    )
  }
  data.frame(pdf_path = pdf_path, pages = max(1, pages), status = "PDF_WRITTEN", stringsAsFactors = FALSE)
}

# Scan RDS files --------------------------------------------------------------
all_rds <- list_rds_safe(objects_root)
cat("[03B REPAIR V1] RDS files scanned:", length(all_rds), "\n")

inventory <- data.frame()
for (ii in seq_len(nrow(expected))) {
  folder <- expected$folder[ii]
  acc <- expected$accession_match[ii]
  matched <- all_rds[grepl(acc, all_rds, fixed = TRUE)]
  if (length(matched) == 0) {
    inventory <- rbind(inventory, data.frame(
      folder = folder,
      accession_match = acc,
      rds_path = NA_character_,
      file_size_mb = NA_real_,
      object_short_label = NA_character_,
      stringsAsFactors = FALSE
    ))
  } else {
    for (p in matched) {
      inventory <- rbind(inventory, data.frame(
        folder = folder,
        accession_match = acc,
        rds_path = p,
        file_size_mb = round(file.info(p)$size / 1024 / 1024, 3),
        object_short_label = substr(gsub("\\.rds$", "", basename(p), ignore.case = TRUE), 1, 45),
        stringsAsFactors = FALSE
      ))
    }
  }
}

write_csv_safe(expected, file.path(table_out, "03B_REPAIR_V1_expected_dataset_folders.csv"))
write_csv_safe(inventory, file.path(table_out, "03B_REPAIR_V1_candidate_RDS_inventory.csv"))

# Readiness: read each RDS lightly -------------------------------------------
readiness <- data.frame()
valid_paths <- inventory$rds_path[!is.na(inventory$rds_path) & file.exists(inventory$rds_path)]
for (p in valid_paths) {
  folder <- inventory$folder[match(p, inventory$rds_path)]
  acc <- inventory$accession_match[match(p, inventory$rds_path)]
  cat("[03B REPAIR V1] Reading:", p, "\n")
  obj <- tryCatch(readRDS(p), error = function(e) e)
  if (inherits(obj, "error")) {
    readiness <- rbind(readiness, data.frame(
      folder = folder,
      accession_match = acc,
      rds_path = p,
      basename = basename(p),
      read_status = paste0("READ_FAIL: ", conditionMessage(obj)),
      object_class = NA_character_,
      n_cells = NA_integer_,
      n_meta_cols = NA_integer_,
      reduction_names = NA_character_,
      has_embedding = FALSE,
      embedding_method = NA_character_,
      stringsAsFactors = FALSE
    ))
  } else {
    meta <- get_object_meta(obj)
    emb <- get_embedding(obj)
    readiness <- rbind(readiness, data.frame(
      folder = folder,
      accession_match = acc,
      rds_path = p,
      basename = basename(p),
      read_status = "READ_OK",
      object_class = paste(class(obj), collapse = ";"),
      n_cells = get_object_ncells(obj),
      n_meta_cols = ncol(meta),
      reduction_names = paste(get_reduction_names(obj), collapse = ";"),
      has_embedding = !is.null(emb$emb),
      embedding_method = safe_chr(emb$method),
      stringsAsFactors = FALSE
    ))
  }
}

write_csv_safe(readiness, file.path(table_out, "03B_REPAIR_V1_RDS_object_readiness.csv"))

# Generate per-folder repair PDFs --------------------------------------------
figure_manifest <- data.frame()
for (ii in seq_len(nrow(expected))) {
  folder <- expected$folder[ii]
  out_dir <- file.path(fig_root_03B, folder)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  df <- readiness[readiness$folder == folder, , drop = FALSE]
  count_pdf <- file.path(out_dir, paste0("03B_REPAIR_V1_", folder, "_object_count_summary.pdf"))
  umap_pdf <- file.path(out_dir, paste0("03B_REPAIR_V1_", folder, "_representative_reduction_pages.pdf"))
  make_count_summary_pdf(df, count_pdf, folder)
  cat("[03B REPAIR V1] Wrote figure:", count_pdf, "\n")
  figure_manifest <- rbind(figure_manifest, data.frame(
    folder = folder,
    figure_type = "object_count_summary",
    pdf_path = count_pdf,
    status = ifelse(is_nonempty_file(count_pdf), "WRITTEN", "FAILED_OR_EMPTY"),
    stringsAsFactors = FALSE
  ))
  info <- make_umap_pages_pdf(df, umap_pdf, folder)
  cat("[03B REPAIR V1] Wrote figure:", umap_pdf, "\n")
  figure_manifest <- rbind(figure_manifest, data.frame(
    folder = folder,
    figure_type = "representative_reduction_pages",
    pdf_path = umap_pdf,
    status = ifelse(is_nonempty_file(umap_pdf), info$status[1], "FAILED_OR_EMPTY"),
    stringsAsFactors = FALSE
  ))
}

write_csv_safe(figure_manifest, file.path(table_out, "03B_REPAIR_V1_figure_manifest.csv"))

# Re-audit original 03B after repair -----------------------------------------
fig_files_after <- list.files(fig_root_03B, pattern = "\\.(pdf|png|jpg|jpeg|tif|tiff)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
empty_dirs <- data.frame()
subdirs <- list.dirs(fig_root_03B, recursive = TRUE, full.names = TRUE)
for (d in subdirs) {
  f <- list.files(d, all.files = FALSE, recursive = FALSE, full.names = TRUE)
  if (length(f) == 0) {
    empty_dirs <- rbind(empty_dirs, data.frame(dir = d, stringsAsFactors = FALSE))
  }
}
write_csv_safe(data.frame(figure_file = fig_files_after, stringsAsFactors = FALSE),
               file.path(table_out, "03B_REPAIR_V1_03B_figure_files_after_repair.csv"))
write_csv_safe(empty_dirs, file.path(table_out, "03B_REPAIR_V1_empty_dirs_after_repair.csv"))

# Summary --------------------------------------------------------------------
datasets_with_readable <- unique(readiness$folder[readiness$read_status == "READ_OK"])
datasets_with_embeddings <- unique(readiness$folder[readiness$read_status == "READ_OK" & readiness$has_embedding == TRUE])
n_fig_written <- sum(figure_manifest$status != "FAILED_OR_EMPTY")

decision <- if (length(datasets_with_embeddings) >= 1 && n_fig_written >= 1) {
  "03B_REPAIR_FIGURES_WRITTEN_REVIEW_VISUALLY_DO_NOT_RERUN_00_TO_10P"
} else if (length(datasets_with_readable) >= 1) {
  "RDS_OBJECTS_READABLE_BUT_EMBEDDINGS_MISSING_REVIEW_OBJECT_STRUCTURE"
} else {
  "NO_READABLE_OBJECTS_FOUND_REVIEW_02_OBJECTS_BEFORE_REPAIR"
}

execution_summary <- data.frame(
  project_root = project_root,
  rds_files_scanned = length(all_rds),
  expected_03B_folders = nrow(expected),
  candidate_rds_rows = nrow(inventory[!is.na(inventory$rds_path), , drop = FALSE]),
  readable_rds_objects = sum(readiness$read_status == "READ_OK"),
  datasets_with_readable_objects = length(datasets_with_readable),
  datasets_with_embeddings = length(datasets_with_embeddings),
  figures_written_or_placeholder = n_fig_written,
  empty_03B_dirs_after_repair = nrow(empty_dirs),
  decision = decision,
  stringsAsFactors = FALSE
)

write_csv_safe(execution_summary, file.path(table_out, "03B_REPAIR_V1_execution_summary.csv"))

report_path <- file.path(text_out, "03B_REPAIR_V1_execution_report.txt")
sink(report_path)
cat("03B dataset-merge figure repair V1\n")
cat("==================================\n\n")
print(execution_summary)
cat("\nImportant interpretation:\n")
cat("- This repair exports review-grade 03B PDFs from existing local objects.\n")
cat("- It does not change any object, table, or downstream result.\n")
cat("- If later modules are already locked, do not rerun 00-10P solely because early 03B folders were empty.\n")
cat("- Use this as a provenance repair and visual completeness patch.\n")
sink()
cat("[03B REPAIR V1] Wrote:", report_path, "\n")

cat("\n[03B REPAIR V1] Completed.\n")
cat("[03B REPAIR V1] RDS files scanned:", length(all_rds), "\n")
cat("[03B REPAIR V1] Readable RDS objects:", sum(readiness$read_status == "READ_OK"), "\n")
cat("[03B REPAIR V1] Datasets with embeddings:", length(datasets_with_embeddings), "\n")
cat("[03B REPAIR V1] Figures written/placeholders:", n_fig_written, "\n")
cat("[03B REPAIR V1] Decision:", decision, "\n")
cat("[03B REPAIR V1] Figure root:", fig_root_03B, "\n")
cat("[03B REPAIR V1] Tables:", table_out, "\n")
