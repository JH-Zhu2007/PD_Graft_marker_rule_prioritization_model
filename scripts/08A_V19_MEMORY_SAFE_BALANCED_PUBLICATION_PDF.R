
PROJECT_DIR <- "D:/PD_Graft_Project"

KEY_DATASETS <- c(
  "GSE178265_DA_01B",
  "GSE233885",
  "GSE204796",
  "GSE132758"
)

MAX_OBJECTS_PER_DATASET <- 1
SEED <- 20260714

PDF_WIDTH <- 9.2
PDF_HEIGHT <- 6.2

RENDER_DPI <- 300
RENDER_WIDTH_PX <- 2760
RENDER_HEIGHT_PX <- 1860

POINT_SIZE_DISCRETE <- 0.58
POINT_SIZE_CONTINUOUS <- 0.60
CLUSTER_LABEL_SIZE <- 3.2

USE_FULL_CELLS_FOR_FIGURE <- TRUE

cat("\n============================================================\n")
cat("08A V19：memory-safe balanced publication PDF complete UMAP\n")
cat("============================================================\n\n")

required_pkgs <- c("data.table", "Seurat", "ggplot2", "png")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 08A V18。")
  }
}

suppressPackageStartupMessages({
  library(data.table)
  library(Seurat)
  library(ggplot2)
  library(png)
  library(grid)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_manifest <- file.path(tables_dir, "04D_annotations", "04D_annotated_object_manifest.csv")
input_05A_object <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_object_level_scores.csv")
input_05A_cell <- file.path(tables_dir, "05A_DA_projection_scoring", "05A_cell_level_scores.csv")
input_05B_contrast <- file.path(tables_dir, "05B_safety_risk_scoring", "05B_DA_projection_vs_safety_contrast_groups.csv")
input_07A_master <- file.path(tables_dir, "07A_ML_dataset_preparation", "07A_group_level_ML_master_table.csv")

out_tables_dir <- file.path(tables_dir, "08A_complete_umap_score_annotation_V19_memory_safe_pdf")
out_figures_dir <- file.path(figures_dir, "08A_complete_umap_score_annotation_V19_memory_safe_pdf")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

selected_objects_csv <- file.path(out_tables_dir, "08A_V19_selected_validation_objects.csv")
score_mapping_audit_csv <- file.path(out_tables_dir, "08A_V19_score_mapping_audit.csv")
plot_audit_csv <- file.path(out_tables_dir, "08A_V19_plot_audit.csv")
object_error_csv <- file.path(out_tables_dir, "08A_V19_object_error_audit.csv")
figure_index_csv <- file.path(out_tables_dir, "08A_V19_figure_index.csv")
figure_notes_csv <- file.path(out_tables_dir, "08A_V19_figure_caution_notes.csv")
report_txt <- file.path(reports_dir, "08A_V19_final_publication_pdf_complete_umap_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("找不到必要输入文件：", path)
  data.table::fread(path, data.table = FALSE)
}

read_csv_optional <- function(path) {
  if (!file.exists(path)) return(data.frame())
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) df <- data.frame(empty = character())
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

sanitize <- function(x) {
  x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
  x <- gsub("_+", "_", x)
  x
}

wrap_label <- function(x, width = 24) {
  vapply(as.character(x), function(s) paste(strwrap(s, width = width), collapse = "\n"), character(1))
}

short_dataset_label <- function(x) {
  x <- as.character(x)
  ifelse(x == "GSE178265_DA_01B", "GSE178265 DA reference", x)
}

short_safety_class <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "Unassigned"

  x <- ifelse(x == "ideal_DA_projection_high_safety_low", "Ideal-like DA/projection-high safety-low", x)
  x <- ifelse(x == "mixed_DA_or_projection_with_safety_risk", "Mixed DA/projection with safety-risk signal", x)
  x <- ifelse(x == "high_safety_risk_low_DA", "High safety-risk, low DA", x)
  x <- ifelse(x == "projection_competence_without_DA_low_safety", "Projection-associated, DA-low safety-low", x)
  x <- ifelse(x == "lower_priority_or_mixed", "Lower-priority or mixed", x)

  wrap_label(x, width = 20)
}

short_annotation_label <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "Unassigned"

  x <- ifelse(x == "cycling_or_progenitor_safety_risk_state", "Cycling/progenitor risk", x)
  x <- ifelse(x == "immature_pluripotency_risk_signal_state", "Immature/pluripotency risk", x)
  x <- ifelse(x == "DA_projection_competence_candidate_low_safety_signal", "DA/projection-high safety-low", x)
  x <- ifelse(x == "DA_like_with_A9_molecular_bias", "DA-like A9-biased", x)
  x <- ifelse(x == "DA_like_with_A10_molecular_bias", "DA-like A10-biased", x)
  x <- ifelse(x == "projection_competence_without_strong_DA_identity", "Projection-associated DA-low", x)
  x <- ifelse(x == "lower_priority_or_mixed_signal", "Lower-priority/mixed", x)
  x <- ifelse(x == "unassigned_no_04B_group_score", "Unassigned", x)
  x <- ifelse(x == "unassigned_low_confidence", "Unassigned/low confidence", x)

  wrap_label(x, width = 20)
}

pretty_score_label <- function(x) {
  x <- as.character(x)
  x <- ifelse(x == "DA_like_composite_score", "DA-like score", x)
  x <- ifelse(x == "projection_competence_composite_score", "Projection-associated competence score", x)
  x <- ifelse(x == "DA_projection_competence_composite_score", "DA/projection-associated composite score", x)
  x <- ifelse(x == "A9_minus_A10_score_05A", "A9-minus-A10 molecular bias score", x)
  x <- ifelse(x == "safety_risk_composite_05B", "Safety-risk-associated score", x)
  x
}

short_plot_title <- function(dataset_label, plot_kind) {
  paste0(dataset_label, " | ", plot_kind)
}

detect_cell_column <- function(dt) {
  candidates <- c("cell", "cell_id", "cell_name", "barcode", "cell_barcode", "Cell", "cells", "cell_key")
  hit <- intersect(candidates, colnames(dt))
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
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
  if (length(hit) > 0) return(hit[[1]])
  NA_character_
}

choose_annotation_col <- function(meta_cols) {
  candidates <- c(
    "annotation_04D_v1",
    "annotation_v1_conservative",
    "annotation_04D",
    "celltype_04D",
    "preliminary_annotation",
    "cell_state_04D",
    "cell_state",
    "cell_type",
    "celltype"
  )

  hit <- intersect(candidates, meta_cols)
  if (length(hit) > 0) return(hit[[1]])
  NA_character_
}

get_umap_dt <- function(obj) {
  if (!"umap" %in% names(obj@reductions)) return(NULL)

  emb <- Embeddings(obj, "umap")

  data.table(
    cell = rownames(emb),
    UMAP_1 = as.numeric(emb[, 1]),
    UMAP_2 = as.numeric(emb[, 2])
  )
}

add_05A_scores_to_meta <- function(meta_dt, dataset, object_id, score_dt, cell_col) {
  if (nrow(score_dt) == 0 || is.na(cell_col)) {
    return(list(meta = meta_dt, matched = 0L, cols = character(), message = "no_cell_score_table_or_no_cell_column"))
  }

  if (!all(c("dataset", "object_id") %in% colnames(score_dt))) {
    return(list(meta = meta_dt, matched = 0L, cols = character(), message = "score_table_missing_dataset_object_id"))
  }

  sub <- as.data.table(score_dt[score_dt$dataset == dataset & score_dt$object_id == object_id, , drop = FALSE])
  if (nrow(sub) == 0) {
    return(list(meta = meta_dt, matched = 0L, cols = character(), message = "no_scores_for_object"))
  }

  sub[[cell_col]] <- as.character(sub[[cell_col]])

  score_cols <- intersect(
    c(
      "DA_like_composite_score",
      "projection_competence_composite_score",
      "DA_projection_competence_composite_score",
      "A9_minus_A10_score_05A",
      "DA_core_identity_score",
      "DA_functional_machinery_score",
      "A9_like_DA_identity_score",
      "A10_like_DA_identity_score"
    ),
    colnames(sub)
  )

  if (length(score_cols) == 0) {
    return(list(meta = meta_dt, matched = 0L, cols = character(), message = "no_expected_score_columns"))
  }

  idx <- match(meta_dt$cell, sub[[cell_col]])
  matched <- sum(!is.na(idx))

  for (cc in score_cols) {
    vals <- rep(NA_real_, nrow(meta_dt))
    vals[!is.na(idx)] <- suppressWarnings(as.numeric(sub[[cc]][idx[!is.na(idx)]]))
    meta_dt[[cc]] <- vals
  }

  list(meta = meta_dt, matched = matched, cols = score_cols, message = "ok")
}

add_05B_scores_to_meta <- function(meta_dt, dataset, object_id, contrast_dt) {
  if (nrow(contrast_dt) == 0) {
    return(list(meta = meta_dt, matched = 0L, group_col = NA_character_, message = "no_contrast_table"))
  }

  group_col <- choose_group_col(colnames(meta_dt))

  if (is.na(group_col)) {
    return(list(meta = meta_dt, matched = 0L, group_col = NA_character_, message = "no_group_col_in_meta"))
  }

  sub <- as.data.table(contrast_dt[contrast_dt$dataset == dataset & contrast_dt$object_id == object_id, , drop = FALSE])
  if (nrow(sub) == 0 || !"group_id" %in% colnames(sub)) {
    return(list(meta = meta_dt, matched = 0L, group_col = group_col, message = "no_contrast_for_object_or_no_group_id"))
  }

  sub$group_id <- as.character(sub$group_id)
  mg <- as.character(meta_dt[[group_col]])

  score_map <- setNames(suppressWarnings(as.numeric(sub$safety_risk_composite_05B)), sub$group_id)
  class_map <- setNames(as.character(sub$safety_contrast_class_05B), sub$group_id)

  meta_dt$safety_risk_composite_05B <- score_map[mg]
  meta_dt$safety_contrast_class_05B_raw <- class_map[mg]
  meta_dt$safety_contrast_class_05B_short <- short_safety_class(class_map[mg])

  matched <- sum(!is.na(meta_dt$safety_risk_composite_05B))

  list(meta = meta_dt, matched = matched, group_col = group_col, message = "ok")
}

open_highres_png_device <- function(filename) {

  grDevices::png(
    filename = filename,
    width = RENDER_WIDTH_PX,
    height = RENDER_HEIGHT_PX,
    units = "px",
    res = RENDER_DPI,
    type = ifelse(isTRUE(capabilities("cairo")), "cairo", "windows"),
    bg = "white"
  )
}

save_pdf_atomic <- function(plot_obj, pdf_path, width = PDF_WIDTH, height = PDF_HEIGHT) {
  tmp_png <- paste0(pdf_path, ".tmp_render.png")
  tmp_pdf <- paste0(pdf_path, ".tmp.pdf")

  if (file.exists(tmp_png)) file.remove(tmp_png)
  if (file.exists(tmp_pdf)) file.remove(tmp_pdf)
  if (file.exists(pdf_path)) file.remove(pdf_path)

  ok <- FALSE
  msg <- NA_character_

  tryCatch({
    open_highres_png_device(tmp_png)
    print(plot_obj)
    grDevices::dev.off()

    if (!file.exists(tmp_png) || file.info(tmp_png)$size < 1000) {
      stop("temporary high-resolution render PNG missing or too small")
    }

    img <- png::readPNG(tmp_png, native = TRUE)

    grDevices::pdf(file = tmp_pdf, width = width, height = height, useDingbats = FALSE, paper = "special")
    grid::grid.newpage()
    grid::grid.raster(img, x = 0.5, y = 0.5, width = 1, height = 1, interpolate = TRUE)
    grDevices::dev.off()

    if (file.exists(tmp_pdf) && file.info(tmp_pdf)$size > 1000) {
      file.rename(tmp_pdf, pdf_path)
      ok <- TRUE
    } else {
      stop("temporary PDF missing or too small")
    }
  }, error = function(e) {
    msg <<- conditionMessage(e)
    tryCatch(grDevices::dev.off(), error = function(e2) NULL)
  })

  if (file.exists(tmp_png)) file.remove(tmp_png)
  if (file.exists(tmp_pdf)) file.remove(tmp_pdf)

  gc(verbose = FALSE)

  list(success = ok, message = msg)
}

base_theme <- function() {
  theme_classic(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      axis.title = element_text(face = "bold", size = 10),
      axis.text = element_text(size = 8),
      legend.title = element_text(face = "bold", size = 8.5),
      legend.text = element_text(size = 7.5),
      legend.key.size = unit(0.55, "lines"),
      legend.spacing.y = unit(0.18, "lines"),
      plot.margin = margin(8, 18, 8, 8)
    )
}

make_discrete_palette <- function(vals, type = "generic") {
  lev <- sort(unique(as.character(vals)))
  lev <- lev[!is.na(lev)]

  if (type == "safety_class") {
    out <- setNames(rep("grey70", length(lev)), lev)

    for (l in lev) {
      if (grepl("Ideal-like", l)) out[[l]] <- "#D95F8D"
      if (grepl("Lower", l)) out[[l]] <- "#4E9A06"
      if (grepl("Mixed", l)) out[[l]] <- "#0099CC"
      if (grepl("High safety", l)) out[[l]] <- "#D55E00"
      if (grepl("Projection", l)) out[[l]] <- "#7570B3"
      if (grepl("Unassigned", l)) out[[l]] <- "grey70"
    }

    return(out)
  }

  pal <- grDevices::hcl.colors(max(length(lev), 3), palette = "Dark 3")
  names(pal) <- lev
  pal
}

make_umap_discrete_plot <- function(umap_dt, meta_dt, color_col, title, legend_title, legend_type = "generic", label_clusters = FALSE) {
  df <- merge(umap_dt, meta_dt[, .(cell, value = get(color_col))], by = "cell", all.x = TRUE)
  df[is.na(value) | value == "", value := "Unassigned"]
  df[, value := as.character(value)]

  pal <- make_discrete_palette(df$value, type = legend_type)

  p <- ggplot(df, aes(x = UMAP_1, y = UMAP_2, color = value)) +
    geom_point(size = POINT_SIZE_DISCRETE, alpha = 1, stroke = 0) +
    coord_fixed() +
    scale_color_manual(values = pal, drop = FALSE) +
    labs(title = title, x = "UMAP 1", y = "UMAP 2", color = legend_title) +
    guides(color = guide_legend(override.aes = list(size = 2.4), ncol = 1)) +
    base_theme()

  if (label_clusters) {
    cent <- df[, .(UMAP_1 = median(UMAP_1), UMAP_2 = median(UMAP_2), n = .N), by = value][n >= 30]
    if (nrow(cent) > 0 && nrow(cent) <= 20) {
      p <- p + geom_text(
        data = cent,
        aes(x = UMAP_1, y = UMAP_2, label = value),
        inherit.aes = FALSE,
        size = CLUSTER_LABEL_SIZE,
        fontface = "bold",
        color = "black"
      )
    }
  }

  p
}

make_umap_continuous_plot <- function(umap_dt, meta_dt, color_col, title) {
  df <- merge(umap_dt, meta_dt[, .(cell, value_raw = get(color_col))], by = "cell", all.x = TRUE)
  df[, value_raw := suppressWarnings(as.numeric(value_raw))]
  df <- df[!is.na(value_raw)]

  if (nrow(df) == 0) stop("No non-NA values for ", color_col)

  q <- suppressWarnings(quantile(df$value_raw, probs = c(0.01, 0.99), na.rm = TRUE))
  if (all(is.finite(q)) && q[1] < q[2]) {
    df[, value := pmin(pmax(value_raw, q[1]), q[2])]
  } else {
    df[, value := value_raw]
  }

  ggplot(df, aes(x = UMAP_1, y = UMAP_2, color = value)) +
    geom_point(size = POINT_SIZE_CONTINUOUS, alpha = 1, stroke = 0) +
    coord_fixed() +
    scale_color_gradientn(
      colours = grDevices::hcl.colors(100, palette = "Viridis"),
      name = pretty_score_label(color_col)
    ) +
    labs(title = title, x = "UMAP 1", y = "UMAP 2") +
    guides(color = guide_colorbar(barheight = unit(2.3, "cm"), barwidth = unit(0.30, "cm"))) +
    base_theme()
}

safe_plot_record <- function(dataset, object_id, plot_type, pdf_path, plot_obj) {
  res <- save_pdf_atomic(plot_obj, pdf_path)

  data.frame(
    dataset = dataset,
    object_id = object_id,
    plot_type = plot_type,
    pdf_path = ifelse(res$success, pdf_path, NA_character_),
    success = res$success,
    message = res$message,
    stringsAsFactors = FALSE
  )
}

safe_make_plot <- function(dataset, object_id, plot_type, pdf_path, expr) {
  tryCatch({
    p <- force(expr)
    safe_plot_record(dataset, object_id, plot_type, pdf_path, p)
  }, error = function(e) {
    data.frame(
      dataset = dataset,
      object_id = object_id,
      plot_type = plot_type,
      pdf_path = NA_character_,
      success = FALSE,
      message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

set.seed(SEED)

stamp("读取 manifest / scores / contrast tables。")

manifest <- as.data.table(read_csv_required(input_manifest))
object_scores <- as.data.table(read_csv_optional(input_05A_object))
cell_scores <- as.data.table(read_csv_optional(input_05A_cell))
contrast <- as.data.table(read_csv_optional(input_05B_contrast))
ml_master <- as.data.table(read_csv_optional(input_07A_master))

if (!all(c("dataset", "object_id", "annotated_rds") %in% colnames(manifest))) {
  stop("04D annotated manifest 缺少 dataset/object_id/annotated_rds。")
}

cell_col_05A <- detect_cell_column(cell_scores)

stamp("05A cell score cell column：", ifelse(is.na(cell_col_05A), "not_found", cell_col_05A))
stamp("Render DPI：", RENDER_DPI)

stamp("选择 key dataset representative objects。")

manifest2 <- manifest[dataset %in% KEY_DATASETS]
manifest2[, file_exists := file.exists(annotated_rds)]

if (nrow(object_scores) > 0 && all(c("dataset", "object_id") %in% colnames(object_scores))) {
  obj_n_cols <- intersect(c("n_cells", "total_cells", "cells"), colnames(object_scores))
  if (length(obj_n_cols) > 0) {
    ncol_use <- obj_n_cols[[1]]
    obj_counts <- as.data.table(object_scores[, c("dataset", "object_id", ncol_use), with = FALSE])
    setnames(obj_counts, ncol_use, "n_cells_score")
  } else {
    obj_counts <- data.table(dataset = character(), object_id = character(), n_cells_score = numeric())
  }
} else {
  obj_counts <- data.table(dataset = character(), object_id = character(), n_cells_score = numeric())
}

if (nrow(ml_master) > 0 && all(c("dataset", "object_id", "n_cells") %in% colnames(ml_master))) {
  ml_counts <- ml_master[, .(n_cells_ml = sum(n_cells, na.rm = TRUE)), by = .(dataset, object_id)]
} else {
  ml_counts <- data.table(dataset = character(), object_id = character(), n_cells_ml = numeric())
}

manifest2 <- merge(manifest2, obj_counts, by = c("dataset", "object_id"), all.x = TRUE)
manifest2 <- merge(manifest2, ml_counts, by = c("dataset", "object_id"), all.x = TRUE)

manifest2[, n_cells_for_selection := fifelse(!is.na(n_cells_score), n_cells_score, n_cells_ml)]
manifest2[is.na(n_cells_for_selection), n_cells_for_selection := 0]

selected <- manifest2[file_exists == TRUE, head(.SD[order(-n_cells_for_selection)], MAX_OBJECTS_PER_DATASET), by = dataset]
selected <- selected[order(match(dataset, KEY_DATASETS), -n_cells_for_selection)]

atomic_write_csv(as.data.frame(selected), selected_objects_csv)

figure_notes <- data.frame(
  plot_family = c(
    "Cluster UMAP",
    "04D conservative annotation UMAP",
    "05B class UMAP",
    "DA-like score UMAP",
    "Projection-associated competence score UMAP",
    "DA/projection-associated composite score UMAP",
    "Safety-risk-associated score UMAP",
    "A9-minus-A10 score UMAP"
  ),
  caution_note = c(
    "UMAP and clusters are visualization outputs and do not define final cell identity by themselves.",
    "04D annotations are conservative marker-supported transcriptomic labels and should not be treated as experimentally validated cell fate labels.",
    "05B classes are rule-derived transcriptional candidate states, not experimentally validated graft outcomes.",
    "DA-like score reflects curated transcriptomic signatures, not definitive functional dopaminergic identity.",
    "Projection-associated competence is inferred from transcriptomic signatures and does not prove anatomical projection.",
    "DA/projection-associated composite score is a transcriptomic prioritization score and does not prove functional integration.",
    "Safety-risk-associated score reflects proliferation/progenitor/stress-like transcriptional programs and does not prove tumorigenicity or clinical safety.",
    "A9/A10-like score indicates molecular bias only, not definitive anatomical subtype identity."
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(figure_notes, figure_notes_csv)

plot_records <- list()
score_records <- list()
object_error_records <- list()

if (nrow(selected) == 0) {
  stop("没有选中任何可用对象。请检查 04D annotated manifest。")
}

for (i in seq_len(nrow(selected))) {
  ds <- selected$dataset[[i]]
  oid <- selected$object_id[[i]]
  path <- selected$annotated_rds[[i]]

  stamp("处理对象 ", i, " / ", nrow(selected), "：", ds, " :: ", oid)

  base_name <- paste0(sanitize(ds), "__", sanitize(oid))
  ds_label <- short_dataset_label(ds)

  tryCatch({
    obj <- readRDS(path)

    if (!"umap" %in% names(obj@reductions)) {
      stop("No UMAP reduction found in this object.")
    }

    umap_dt <- get_umap_dt(obj)
    if (is.null(umap_dt) || nrow(umap_dt) == 0) {
      stop("UMAP dataframe unavailable.")
    }

    meta_dt <- as.data.table(obj@meta.data)
    meta_dt[, cell := rownames(obj@meta.data)]

    add05A <- add_05A_scores_to_meta(meta_dt, ds, oid, cell_scores, cell_col_05A)
    meta_dt <- add05A$meta

    add05B <- add_05B_scores_to_meta(meta_dt, ds, oid, contrast)
    meta_dt <- add05B$meta

    annotation_col_raw <- choose_annotation_col(colnames(meta_dt))
    annotation_mapping_message <- "no_annotation_column_found"
    if (!is.na(annotation_col_raw) && annotation_col_raw %in% colnames(meta_dt)) {
      meta_dt[, annotation_08A_short := short_annotation_label(get(annotation_col_raw))]
      annotation_mapping_message <- paste0("annotation_col=", annotation_col_raw)
    }

    score_records[[length(score_records) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      n_cells_full = ncol(obj),
      n_cells_visualized = nrow(umap_dt),
      downsampled_for_visualization = FALSE,
      cell_score_column_detected = ifelse(is.na(cell_col_05A), NA_character_, cell_col_05A),
      matched_05A_cells = add05A$matched,
      available_05A_score_columns = paste(add05A$cols, collapse = ";"),
      add05A_message = add05A$message,
      group_col_for_05B = add05B$group_col,
      matched_05B_cells = add05B$matched,
      add05B_message = add05B$message,
      annotation_col_raw = ifelse(is.na(annotation_col_raw), NA_character_, annotation_col_raw),
      annotation_mapping_message = annotation_mapping_message,
      stringsAsFactors = FALSE
    )

    if ("seurat_clusters" %in% colnames(meta_dt)) {
      pdf_path <- file.path(out_figures_dir, paste0(base_name, "__V19_UMAP_clusters.pdf"))
      rec <- safe_make_plot(
        ds, oid, "V19_UMAP_clusters", pdf_path,
        make_umap_discrete_plot(
          umap_dt, meta_dt,
          color_col = "seurat_clusters",
          title = short_plot_title(ds_label, "UMAP clusters"),
          legend_title = "Cluster",
          legend_type = "generic",
          label_clusters = TRUE
        )
      )
      plot_records[[length(plot_records) + 1L]] <- rec
    }

    if ("annotation_08A_short" %in% colnames(meta_dt) && sum(!is.na(meta_dt$annotation_08A_short)) > 0) {
      pdf_path <- file.path(out_figures_dir, paste0(base_name, "__V19_UMAP_04D_conservative_annotation.pdf"))
      rec <- safe_make_plot(
        ds, oid, "V19_UMAP_04D_conservative_annotation", pdf_path,
        make_umap_discrete_plot(
          umap_dt, meta_dt,
          color_col = "annotation_08A_short",
          title = short_plot_title(ds_label, "04D conservative annotation"),
          legend_title = "04D annotation",
          legend_type = "generic",
          label_clusters = FALSE
        )
      )
      plot_records[[length(plot_records) + 1L]] <- rec
    }

    if ("safety_contrast_class_05B_short" %in% colnames(meta_dt) && sum(!is.na(meta_dt$safety_contrast_class_05B_short)) > 0) {
      pdf_path <- file.path(out_figures_dir, paste0(base_name, "__V19_UMAP_05B_safety_contrast_class.pdf"))
      rec <- safe_make_plot(
        ds, oid, "V19_UMAP_05B_safety_contrast_class", pdf_path,
        make_umap_discrete_plot(
          umap_dt, meta_dt,
          color_col = "safety_contrast_class_05B_short",
          title = short_plot_title(ds_label, "DA/projection-associated class"),
          legend_title = "05B class",
          legend_type = "safety_class",
          label_clusters = FALSE
        )
      )
      plot_records[[length(plot_records) + 1L]] <- rec
    }

    score_cols <- intersect(
      c(
        "DA_like_composite_score",
        "projection_competence_composite_score",
        "DA_projection_competence_composite_score",
        "A9_minus_A10_score_05A",
        "safety_risk_composite_05B"
      ),
      colnames(meta_dt)
    )

    score_cols <- score_cols[vapply(score_cols, function(cc) sum(!is.na(meta_dt[[cc]])) > 0, logical(1))]

    for (sc in score_cols) {
      pdf_path <- file.path(out_figures_dir, paste0(base_name, "__V19_UMAP_score_", sanitize(sc), ".pdf"))
      rec <- safe_make_plot(
        ds, oid, paste0("V19_UMAP_score_", sc), pdf_path,
        make_umap_continuous_plot(
          umap_dt, meta_dt,
          color_col = sc,
          title = short_plot_title(ds_label, pretty_score_label(sc))
        )
      )
      plot_records[[length(plot_records) + 1L]] <- rec
    }

    rm(obj)
    gc(verbose = FALSE)

  }, error = function(e) {
    object_error_records[[length(object_error_records) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      stage = "object_loop",
      message = conditionMessage(e),
      stringsAsFactors = FALSE
    )

    plot_records[[length(plot_records) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      plot_type = "object_failed",
      pdf_path = NA_character_,
      success = FALSE,
      message = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

plot_audit <- if (length(plot_records) > 0) rbindlist(plot_records, fill = TRUE) else data.table()
score_audit <- if (length(score_records) > 0) rbindlist(score_records, fill = TRUE) else data.table()
object_error_audit <- if (length(object_error_records) > 0) rbindlist(object_error_records, fill = TRUE) else data.table()

required_plot_cols <- c("dataset", "object_id", "plot_type", "pdf_path", "success", "message")
for (cc in required_plot_cols) {
  if (!cc %in% colnames(plot_audit)) {
    if (cc == "success") plot_audit[[cc]] <- FALSE else plot_audit[[cc]] <- NA_character_
  }
}

figure_index <- plot_audit[
  success == TRUE,
  c("dataset", "object_id", "plot_type", "pdf_path", "message"),
  with = FALSE
]

atomic_write_csv(as.data.frame(plot_audit), plot_audit_csv)
atomic_write_csv(as.data.frame(score_audit), score_mapping_audit_csv)
atomic_write_csv(as.data.frame(object_error_audit), object_error_csv)
atomic_write_csv(as.data.frame(figure_index), figure_index_csv)

n_success_plots <- sum(plot_audit$success, na.rm = TRUE)
n_failed_plots <- sum(!plot_audit$success, na.rm = TRUE)

selected_lines <- apply(as.data.frame(selected), 1, function(x) {
  paste0(x[["dataset"]], " :: ", x[["object_id"]], " ; n_cells_for_selection=", x[["n_cells_for_selection"]])
})

score_lines <- if (nrow(score_audit) > 0) {
  apply(as.data.frame(score_audit), 1, function(x) {
    paste0(
      x[["dataset"]],
      " :: ",
      x[["object_id"]],
      " ; cells=",
      x[["n_cells_visualized"]],
      " ; matched_05A=",
      x[["matched_05A_cells"]],
      " ; matched_05B=",
      x[["matched_05B_cells"]],
      " ; ",
      x[["annotation_mapping_message"]]
    )
  })
} else {
  "none"
}

error_lines <- if (nrow(object_error_audit) > 0) {
  apply(as.data.frame(object_error_audit), 1, function(x) {
    paste0(x[["dataset"]], " :: ", x[["object_id"]], " ; ", x[["stage"]], " ; ", x[["message"]])
  })
} else {
  "none"
}

report_lines <- c(
  "08A V19 memory-safe balanced publication PDF complete UMAP report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Selected objects:",
  selected_lines,
  "",
  "Score / annotation mapping summary:",
  score_lines,
  "",
  "Object errors:",
  error_lines,
  "",
  "Plot summary:",
  paste0("Successful plots: ", n_success_plots),
  paste0("Failed plots: ", n_failed_plots),
  paste0("Output figure directory: ", out_figures_dir),
  paste0("Render DPI: ", RENDER_DPI),
  paste0("Render size px: ", RENDER_WIDTH_PX, "x", RENDER_HEIGHT_PX),
  "",
  "Output tables:",
  paste0("Selected objects: ", selected_objects_csv),
  paste0("Plot audit: ", plot_audit_csv),
  paste0("Figure index: ", figure_index_csv),
  paste0("Score mapping audit: ", score_mapping_audit_csv),
  paste0("Object error audit: ", object_error_csv),
  paste0("Figure caution notes: ", figure_notes_csv),
  "",
  "Next step:",
  "Check V19 PDFs. If clean, readable, and visually acceptable, keep V19 as final complete 08A UMAP validation output.",
  "",
  "Journal-rigor note:",
  "08A V19 figures are visualization outputs only. Projection-associated molecular competence does not prove anatomical projection. Safety-risk-associated transcriptional state does not prove tumorigenicity or clinical safety."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("08A V19 memory-safe balanced publication PDF complete UMAP 运行结束\n")
cat("============================================================\n\n")

cat("Selected validation objects：", nrow(selected), "\n")
cat("Successful plots：", n_success_plots, "\n")
cat("Failed plots：", n_failed_plots, "\n")
cat("Object error rows：", nrow(object_error_audit), "\n")
cat("Score mapping rows：", nrow(score_audit), "\n")
cat("Render DPI：", RENDER_DPI, "\n")
cat("Render size px：", RENDER_WIDTH_PX, "x", RENDER_HEIGHT_PX, "\n\n")

cat("输出表格：\n")
cat(selected_objects_csv, "\n")
cat(plot_audit_csv, "\n")
cat(figure_index_csv, "\n")
cat(score_mapping_audit_csv, "\n")
cat(object_error_csv, "\n")
cat(figure_notes_csv, "\n")
cat(report_txt, "\n\n")

cat("输出 PDF 图片目录：\n")
cat(out_figures_dir, "\n\n")

cat("✅ 08A V19 memory-safe balanced publication PDF complete UMAP 完成。\n")
cat("下一步：检查 V19 输出数量、点颜色深浅、以及 GSE204796/GSE132758 关键 PDF 是否正常。\n")
