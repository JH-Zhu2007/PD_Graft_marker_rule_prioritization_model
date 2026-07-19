
PROJECT_DIR <- "D:/PD_Graft_Project"

WRITE_CELL_LEVEL_SCORES <- TRUE

DA_LIKE_GROUP_MIN <- 0.08
PROJECTION_COMPETENCE_GROUP_MIN <- 0.08
A9_A10_BIAS_DELTA <- 0.02
SAFETY_RISK_LOW_MAX <- 0.20

set.seed(20260714)

cat("\n============================================================\n")
cat("05A：DA/A9/A10/projection competence scoring\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "data.table", "Matrix")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 05A。")
  }
}

suppressPackageStartupMessages({
  library(Seurat)
  library(SeuratObject)
  library(data.table)
  library(Matrix)
})

tables_dir <- file.path(PROJECT_DIR, "03_tables")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

input_annotated_manifest <- file.path(tables_dir, "04D_annotations", "04D_annotated_object_manifest.csv")

out_tables_dir <- file.path(tables_dir, "05A_DA_projection_scoring")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

signature_gene_sets_csv <- file.path(out_tables_dir, "05A_signature_gene_sets.csv")
cell_level_scores_csv <- file.path(out_tables_dir, "05A_cell_level_scores.csv")
group_level_scores_csv <- file.path(out_tables_dir, "05A_group_level_scores.csv")
object_level_scores_csv <- file.path(out_tables_dir, "05A_object_level_scores.csv")
candidate_groups_csv <- file.path(out_tables_dir, "05A_DA_A9_A10_projection_candidate_groups.csv")
failed_objects_csv <- file.path(out_tables_dir, "05A_failed_objects.csv")
matrix_source_csv <- file.path(out_tables_dir, "05A_matrix_source_audit.csv")
report_txt <- file.path(reports_dir, "05A_DA_A9_A10_projection_competence_scoring_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path)
  }
  data.table::fread(path, data.table = FALSE)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }
  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
}

get_assay_for_analysis <- function(obj) {
  assays <- names(obj@assays)

  if ("RNA" %in% assays) return("RNA")

  da <- tryCatch(DefaultAssay(obj), error = function(e) NA_character_)

  if (!is.na(da) && da %in% assays) return(da)

  if (length(assays) > 0L) return(assays[[1L]])

  NA_character_
}

match_genes_case_insensitive <- function(query_genes, object_genes) {
  query_genes <- unique(as.character(query_genes))
  object_genes <- unique(as.character(object_genes))

  q_upper <- toupper(query_genes)
  g_upper <- toupper(object_genes)

  idx <- match(q_upper, g_upper)

  data.frame(
    query_gene = query_genes,
    query_upper = q_upper,
    matched_gene = ifelse(is.na(idx), NA_character_, object_genes[idx]),
    present = !is.na(idx),
    stringsAsFactors = FALSE
  )
}

get_assay5_layer_names <- function(assay_obj) {
  out <- tryCatch({
    names(slot(assay_obj, "layers"))
  }, error = function(e) {
    character()
  })

  out[!is.na(out) & nzchar(out)]
}

get_logmap_names <- function(logmap_obj, layer_name) {
  out <- tryCatch({
    if (layer_name %in% colnames(logmap_obj)) {
      rownames(logmap_obj)[as.logical(logmap_obj[, layer_name])]
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })

  out
}

repair_layer_dimnames <- function(mat, obj, assay_obj, layer_name) {
  if (is.null(mat)) return(NULL)

  if (!inherits(mat, "dgCMatrix")) {
    mat <- tryCatch(as(mat, "dgCMatrix"), error = function(e) NULL)
  }

  if (is.null(mat)) return(NULL)
  if (nrow(mat) == 0L || ncol(mat) == 0L) return(NULL)

  obj_features <- tryCatch(rownames(obj), error = function(e) NULL)
  obj_cells <- tryCatch(colnames(obj), error = function(e) NULL)

  fmap <- tryCatch(slot(assay_obj, "features"), error = function(e) NULL)
  cmap <- tryCatch(slot(assay_obj, "cells"), error = function(e) NULL)

  layer_features <- if (!is.null(fmap)) get_logmap_names(fmap, layer_name) else NULL
  layer_cells <- if (!is.null(cmap)) get_logmap_names(cmap, layer_name) else NULL

  candidate_features <- list(layer_features, obj_features)
  candidate_cells <- list(layer_cells, obj_cells)

  feature_lengths <- unique(na.omit(vapply(candidate_features, function(x) if (is.null(x)) NA_integer_ else length(x), integer(1))))
  cell_lengths <- unique(na.omit(vapply(candidate_cells, function(x) if (is.null(x)) NA_integer_ else length(x), integer(1))))

  if (length(feature_lengths) > 0L && length(cell_lengths) > 0L) {
    if (
      nrow(mat) %in% cell_lengths &&
        ncol(mat) %in% feature_lengths &&
        !(nrow(mat) %in% feature_lengths && ncol(mat) %in% cell_lengths)
    ) {
      mat <- Matrix::t(mat)
    }
  }

  if (
    is.null(rownames(mat)) ||
      length(rownames(mat)) != nrow(mat) ||
      any(is.na(rownames(mat))) ||
      any(rownames(mat) == "")
  ) {
    rn <- NULL

    if (!is.null(layer_features) && length(layer_features) == nrow(mat)) {
      rn <- layer_features
    } else if (!is.null(obj_features) && length(obj_features) == nrow(mat)) {
      rn <- obj_features
    }

    if (!is.null(rn)) {
      rownames(mat) <- make.unique(as.character(rn), sep = "__dupGene")
    }
  }

  if (
    is.null(colnames(mat)) ||
      length(colnames(mat)) != ncol(mat) ||
      any(is.na(colnames(mat))) ||
      any(colnames(mat) == "")
  ) {
    cn <- NULL

    if (!is.null(layer_cells) && length(layer_cells) == ncol(mat)) {
      cn <- layer_cells
    } else if (!is.null(obj_cells) && length(obj_cells) == ncol(mat)) {
      cn <- obj_cells
    }

    if (!is.null(cn)) {
      colnames(mat) <- make.unique(as.character(cn), sep = "__dupCell")
    }
  }

  if (is.null(rownames(mat)) || is.null(colnames(mat))) return(NULL)
  if (length(rownames(mat)) != nrow(mat) || length(colnames(mat)) != ncol(mat)) return(NULL)

  mat
}

extract_expression_matrix_direct <- function(obj) {
  assay <- get_assay_for_analysis(obj)

  if (is.na(assay) || !assay %in% names(obj@assays)) {
    stop("No valid assay found.")
  }

  assay_obj <- obj[[assay]]
  layer_names <- get_assay5_layer_names(assay_obj)

  if (length(layer_names) > 0L) {
    candidate_layers <- unique(c(
      "counts",
      grep("^counts", layer_names, value = TRUE),
      "data",
      grep("^data", layer_names, value = TRUE)
    ))

    candidate_layers <- candidate_layers[candidate_layers %in% layer_names]

    for (lyr in candidate_layers) {
      raw_mat <- tryCatch({
        slot(assay_obj, "layers")[[lyr]]
      }, error = function(e) NULL)

      mat <- repair_layer_dimnames(raw_mat, obj, assay_obj, lyr)

      if (!is.null(mat)) {
        return(list(
          matrix = mat,
          assay = assay,
          layer = lyr,
          layer_type = ifelse(grepl("^counts", lyr), "counts", "data"),
          method = "direct_Assay5_layers"
        ))
      }
    }
  }

  for (lyr in c("counts", "data")) {
    mat <- tryCatch({
      SeuratObject::LayerData(obj, assay = assay, layer = lyr, fast = FALSE)
    }, error = function(e) NULL)

    mat <- repair_layer_dimnames(mat, obj, assay_obj, lyr)

    if (!is.null(mat)) {
      return(list(
        matrix = mat,
        assay = assay,
        layer = lyr,
        layer_type = ifelse(lyr == "counts", "counts", "data"),
        method = "LayerData"
      ))
    }
  }

  for (sl in c("counts", "data")) {
    mat <- tryCatch({
      slot(assay_obj, sl)
    }, error = function(e) NULL)

    mat <- repair_layer_dimnames(mat, obj, assay_obj, sl)

    if (!is.null(mat)) {
      return(list(
        matrix = mat,
        assay = assay,
        layer = sl,
        layer_type = ifelse(sl == "counts", "counts", "data"),
        method = "Assay_slot"
      ))
    }
  }

  stop(
    "Cannot extract expression matrix. assay=",
    assay,
    "; layers=",
    paste(layer_names, collapse = ",")
  )
}

score_signatures_from_matrix <- function(expr_info, signature_sets) {
  mat <- expr_info$matrix

  all_sig_genes <- unique(unlist(signature_sets, use.names = FALSE))
  all_sig_genes <- all_sig_genes[!is.na(all_sig_genes) & nzchar(all_sig_genes)]

  gene_match <- match_genes_case_insensitive(all_sig_genes, rownames(mat))
  present_all <- unique(gene_match$matched_gene[gene_match$present])
  present_all <- present_all[!is.na(present_all)]

  if (length(present_all) == 0L) {
    return(NULL)
  }

  mat_sub_all <- mat[present_all, , drop = FALSE]

  if (expr_info$layer_type == "counts") {
    lib <- Matrix::colSums(mat)
    lib[is.na(lib) | lib <= 0] <- 1
    mat_sub_all <- t(t(mat_sub_all) / lib * 10000)
    mat_sub_all <- log1p(mat_sub_all)
  }

  score_dt <- data.table(cell = colnames(mat_sub_all))

  coverage_list <- list()

  for (sig_name in names(signature_sets)) {
    sig_genes <- unique(signature_sets[[sig_name]])
    matched <- match_genes_case_insensitive(sig_genes, rownames(mat_sub_all))

    sig_present <- unique(matched$matched_gene[matched$present])
    sig_present <- sig_present[!is.na(sig_present)]

    if (length(sig_present) == 0L) {
      score_dt[[paste0(sig_name, "_score")]] <- NA_real_
    } else {
      sig_mat <- mat_sub_all[sig_present, , drop = FALSE]
      score_dt[[paste0(sig_name, "_score")]] <- Matrix::colMeans(sig_mat)
    }

    coverage_list[[length(coverage_list) + 1L]] <- data.frame(
      signature = sig_name,
      n_signature_genes = length(sig_genes),
      n_present_genes = length(sig_present),
      coverage_fraction = ifelse(length(sig_genes) > 0L, length(sig_present) / length(sig_genes), NA_real_),
      present_genes = paste(sig_present, collapse = ";"),
      missing_genes = paste(setdiff(toupper(sig_genes), toupper(sig_present)), collapse = ";"),
      stringsAsFactors = FALSE
    )
  }

  list(
    scores = score_dt,
    coverage = rbindlist(coverage_list, fill = TRUE)
  )
}

safe_meta_col <- function(meta, col, default = NA_character_) {
  if (col %in% colnames(meta)) {
    return(meta[[col]])
  }
  rep(default, nrow(meta))
}

compute_composites <- function(dt) {
  needed <- c(
    "DA_core_identity_score",
    "DA_functional_machinery_score",
    "A9_like_DA_identity_score",
    "A10_like_DA_identity_score",
    "neuronal_maturation_synapse_score",
    "projection_associated_molecular_competence_score"
  )

  for (col in needed) {
    if (!col %in% colnames(dt)) {
      dt[[col]] <- NA_real_
    }
  }

  dt[
    ,
    DA_like_composite_score := rowMeans(
      cbind(
        DA_core_identity_score,
        DA_functional_machinery_score,
        neuronal_maturation_synapse_score
      ),
      na.rm = TRUE
    )
  ]

  dt[
    ,
    A9_minus_A10_score_05A := A9_like_DA_identity_score - A10_like_DA_identity_score
  ]

  dt[
    ,
    projection_competence_composite_score := rowMeans(
      cbind(
        projection_associated_molecular_competence_score,
        neuronal_maturation_synapse_score
      ),
      na.rm = TRUE
    )
  ]

  dt[
    ,
    DA_projection_competence_composite_score := rowMeans(
      cbind(
        DA_like_composite_score,
        projection_competence_composite_score
      ),
      na.rm = TRUE
    )
  ]

  dt[
    ,
    A9_A10_bias_label_05A := fifelse(
      is.na(A9_minus_A10_score_05A),
      "unknown",
      fifelse(
        A9_minus_A10_score_05A > A9_A10_BIAS_DELTA,
        "A9_like_bias",
        fifelse(
          A9_minus_A10_score_05A < -A9_A10_BIAS_DELTA,
          "A10_like_bias",
          "A9_A10_mixed_or_unclear"
        )
      )
    )
  ]

  dt
}

write_partial_outputs <- function(
  cell_score_list,
  group_score_list,
  object_score_list,
  candidate_list,
  failed_list,
  matrix_source_list
) {
  cell_df <- if (length(cell_score_list) > 0L && WRITE_CELL_LEVEL_SCORES) {
    rbindlist(cell_score_list, fill = TRUE)
  } else {
    data.frame()
  }

  group_df <- if (length(group_score_list) > 0L) rbindlist(group_score_list, fill = TRUE) else data.frame()
  object_df <- if (length(object_score_list) > 0L) rbindlist(object_score_list, fill = TRUE) else data.frame()
  candidate_df <- if (length(candidate_list) > 0L) rbindlist(candidate_list, fill = TRUE) else data.frame()
  failed_df <- if (length(failed_list) > 0L) rbindlist(failed_list, fill = TRUE) else data.frame()
  matrix_df <- if (length(matrix_source_list) > 0L) rbindlist(matrix_source_list, fill = TRUE) else data.frame()

  if (WRITE_CELL_LEVEL_SCORES) atomic_write_csv(cell_df, cell_level_scores_csv)
  atomic_write_csv(group_df, group_level_scores_csv)
  atomic_write_csv(object_df, object_level_scores_csv)
  atomic_write_csv(candidate_df, candidate_groups_csv)
  atomic_write_csv(failed_df, failed_objects_csv)
  atomic_write_csv(matrix_df, matrix_source_csv)

  invisible(list(
    cell_df = cell_df,
    group_df = group_df,
    object_df = object_df,
    candidate_df = candidate_df,
    failed_df = failed_df,
    matrix_df = matrix_df
  ))
}

stamp("建立 05A signature gene sets。")

signature_sets <- list(
  DA_core_identity = c(
    "TH", "DDC", "SLC6A3", "SLC18A2", "NR4A2",
    "FOXA2", "LMX1A", "LMX1B", "PITX3", "EN1"
  ),

  DA_functional_machinery = c(
    "TH", "DDC", "SLC6A3", "SLC18A2",
    "GCH1", "ALDH1A1", "KCNJ6", "DRD2"
  ),

  A9_like_DA_identity = c(
    "ALDH1A1", "KCNJ6", "SOX6", "DCLK3", "GCH1", "SLC10A4", "KCND3"
  ),

  A10_like_DA_identity = c(
    "CALB1", "OTX2", "CCK", "SLC17A6", "VIP", "NRIP3"
  ),

  neuronal_maturation_synapse = c(
    "RBFOX3", "MAP2", "TUBB3", "DCX", "STMN2",
    "SNAP25", "SYT1", "SYN1", "NEFL", "NEFM"
  ),

  projection_associated_molecular_competence = c(

    "STMN2", "GAP43", "DCX", "MAP2", "NEFL", "NEFM",

    "SNAP25", "SYT1", "SYN1", "RAB3A", "STX1A",

    "L1CAM", "CNTN2", "ROBO1", "ROBO2", "DCC",
    "NTN1", "SEMA3A", "SEMA3C", "NRP1", "PLXNA1",
    "EPHB1", "EPHB2", "EFNB2", "SLIT2"
  )
)

sig_df <- rbindlist(lapply(names(signature_sets), function(sig) {
  data.frame(
    signature = sig,
    gene_symbol = signature_sets[[sig]],
    stringsAsFactors = FALSE
  )
}), fill = TRUE)

sig_df$interpretation <- ifelse(
  sig_df$signature == "projection_associated_molecular_competence",
  "Projection-associated molecular competence only; not proof of real anatomical projection.",
  ifelse(
    grepl("A9", sig_df$signature),
    "A9-like molecular identity bias; not proof of substantia nigra functional identity.",
    ifelse(
      grepl("A10", sig_df$signature),
      "A10-like molecular identity bias; not proof of VTA functional identity.",
      "Transcriptomic signature score."
    )
  )
)

atomic_write_csv(sig_df, signature_gene_sets_csv)

stamp("读取 04D annotated object manifest。")

manifest <- read_csv_required(input_annotated_manifest)

if (!all(c("dataset", "object_id", "annotated_rds") %in% colnames(manifest))) {
  stop("04D annotated manifest 缺少 dataset/object_id/annotated_rds。")
}

manifest <- manifest[file.exists(manifest$annotated_rds), , drop = FALSE]

stamp("准备 scoring 对象数量：", nrow(manifest))

cell_score_list <- list()
group_score_list <- list()
object_score_list <- list()
candidate_list <- list()
failed_list <- list()
matrix_source_list <- list()

for (i in seq_len(nrow(manifest))) {
  ds <- manifest$dataset[[i]]
  oid <- manifest$object_id[[i]]
  path <- manifest$annotated_rds[[i]]

  stamp("05A 处理对象 ", i, " / ", nrow(manifest), "：", ds, " :: ", oid)

  tryCatch({
    obj <- readRDS(path)

    expr_info <- extract_expression_matrix_direct(obj)

    stamp(
      "  matrix：",
      nrow(expr_info$matrix), " genes x ", ncol(expr_info$matrix),
      " cells；method=", expr_info$method, "；layer=", expr_info$layer
    )

    score_out <- score_signatures_from_matrix(expr_info, signature_sets)

    if (is.null(score_out)) {
      stop("No signature genes could be extracted.")
    }

    scores <- score_out$scores
    cov <- score_out$coverage

    scores <- compute_composites(scores)

    meta <- obj@meta.data
    common_cells <- intersect(scores$cell, rownames(meta))

    if (length(common_cells) == 0L) {
      stop("No overlapping cells between score table and object metadata.")
    }

    scores <- scores[cell %in% common_cells]
    meta_sub <- meta[scores$cell, , drop = FALSE]

    scores[, dataset := ds]
    scores[, object_id := oid]
    scores[, annotation_04D_v1 := as.character(safe_meta_col(meta_sub, "annotation_04D_v1", "unknown"))]
    scores[, annotation_04D_confidence := as.character(safe_meta_col(meta_sub, "annotation_04D_confidence", "unknown"))]
    scores[, annotation_04D_subtype_bias := as.character(safe_meta_col(meta_sub, "annotation_04D_subtype_bias", "unknown"))]
    scores[, annotation_04D_group_id := as.character(safe_meta_col(meta_sub, "annotation_04D_group_id", "object_all"))]
    scores[, annotation_04D_group_source := as.character(safe_meta_col(meta_sub, "annotation_04D_group_source", "object_all"))]
    scores[, annotation_04D_safety_risk_combined := suppressWarnings(as.numeric(safe_meta_col(meta_sub, "annotation_04D_safety_risk_combined", NA_real_)))]

    front_cols <- c(
      "dataset", "object_id", "cell",
      "annotation_04D_v1", "annotation_04D_confidence",
      "annotation_04D_subtype_bias", "annotation_04D_group_source", "annotation_04D_group_id"
    )
    scores <- scores[, c(front_cols, setdiff(colnames(scores), front_cols)), with = FALSE]

    if (WRITE_CELL_LEVEL_SCORES) {
      cell_score_list[[length(cell_score_list) + 1L]] <- as.data.frame(scores)
    }

    group_dt <- copy(scores)

    group_cols <- c(
      "dataset", "object_id", "annotation_04D_group_source",
      "annotation_04D_group_id", "annotation_04D_v1"
    )

    numeric_score_cols <- grep("_score$|_composite_score$|A9_minus_A10_score_05A|annotation_04D_safety_risk_combined", colnames(group_dt), value = TRUE)

    group_summary <- group_dt[
      ,
      c(
        list(
          n_cells = .N,
          dominant_confidence = paste(unique(annotation_04D_confidence), collapse = ";"),
          dominant_subtype_bias_04D = paste(unique(annotation_04D_subtype_bias), collapse = ";")
        ),
        lapply(.SD, function(x) mean(as.numeric(x), na.rm = TRUE)),
        lapply(.SD, function(x) mean(as.numeric(x) > 0, na.rm = TRUE))
      ),
      by = group_cols,
      .SDcols = numeric_score_cols
    ]

    old_names <- colnames(group_summary)
    n_base <- length(group_cols) + 3
    score_names <- numeric_score_cols
    pct_names <- paste0("pct_cells_", numeric_score_cols, "_gt0")
    colnames(group_summary) <- c(
      group_cols,
      "n_cells",
      "dominant_confidence",
      "dominant_subtype_bias_04D",
      score_names,
      pct_names
    )

    group_summary <- compute_composites(group_summary)

    group_summary[
      ,
      candidate_class_05A := fifelse(
        DA_like_composite_score >= DA_LIKE_GROUP_MIN &
          projection_competence_composite_score >= PROJECTION_COMPETENCE_GROUP_MIN &
          (is.na(annotation_04D_safety_risk_combined) | annotation_04D_safety_risk_combined <= SAFETY_RISK_LOW_MAX),
        "DA_projection_competence_candidate_low_safety_signal",
        fifelse(
          DA_like_composite_score >= DA_LIKE_GROUP_MIN &
            A9_minus_A10_score_05A > A9_A10_BIAS_DELTA,
          "DA_like_with_A9_molecular_bias",
          fifelse(
            DA_like_composite_score >= DA_LIKE_GROUP_MIN &
              A9_minus_A10_score_05A < -A9_A10_BIAS_DELTA,
            "DA_like_with_A10_molecular_bias",
            fifelse(
              projection_competence_composite_score >= PROJECTION_COMPETENCE_GROUP_MIN &
                DA_like_composite_score < DA_LIKE_GROUP_MIN,
              "projection_competence_without_strong_DA_identity",
              "lower_priority_or_mixed_signal"
            )
          )
        )
      )
    ]

    group_score_list[[length(group_score_list) + 1L]] <- as.data.frame(group_summary)

    object_summary <- group_dt[
      ,
      c(
        list(
          n_cells = .N,
          n_annotation_labels = length(unique(annotation_04D_v1)),
          dominant_annotation = names(sort(table(annotation_04D_v1), decreasing = TRUE))[1]
        ),
        lapply(.SD, function(x) mean(as.numeric(x), na.rm = TRUE))
      ),
      by = .(dataset, object_id),
      .SDcols = numeric_score_cols
    ]

    object_summary <- compute_composites(object_summary)

    object_score_list[[length(object_score_list) + 1L]] <- as.data.frame(object_summary)

    candidate <- group_summary[
      candidate_class_05A != "lower_priority_or_mixed_signal"
    ][
      order(-DA_projection_competence_composite_score, -DA_like_composite_score, -projection_competence_composite_score)
    ]

    if (nrow(candidate) > 0L) {
      candidate_list[[length(candidate_list) + 1L]] <- as.data.frame(candidate)
    }

    cov[, dataset := ds]
    cov[, object_id := oid]
    cov[, assay := expr_info$assay]
    cov[, layer := expr_info$layer]
    cov[, layer_type := expr_info$layer_type]
    cov[, extraction_method := expr_info$method]
    cov[, n_matrix_genes := nrow(expr_info$matrix)]
    cov[, n_matrix_cells := ncol(expr_info$matrix)]

    matrix_source_list[[length(matrix_source_list) + 1L]] <- as.data.frame(cov)

    rm(obj, expr_info, score_out, scores, meta, meta_sub, group_dt, group_summary)
    gc(verbose = FALSE)

  }, error = function(e) {
    msg <- conditionMessage(e)
    stamp("  对象失败但不中断：", msg)

    failed_list[[length(failed_list) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      annotated_rds = path,
      stage = "05A_scoring",
      message = msg,
      stringsAsFactors = FALSE
    )
  })

  write_partial_outputs(
    cell_score_list,
    group_score_list,
    object_score_list,
    candidate_list,
    failed_list,
    matrix_source_list
  )
}

final_outputs <- write_partial_outputs(
  cell_score_list,
  group_score_list,
  object_score_list,
  candidate_list,
  failed_list,
  matrix_source_list
)

cell_df <- final_outputs$cell_df
group_df <- final_outputs$group_df
object_df <- final_outputs$object_df
candidate_df <- final_outputs$candidate_df
failed_df <- final_outputs$failed_df
matrix_df <- final_outputs$matrix_df

n_objects_total <- nrow(manifest)

n_objects_success <- if (nrow(object_df) > 0L) {
  length(unique(paste(object_df$dataset, object_df$object_id, sep = "||")))
} else {
  0L
}

n_objects_failed <- if (nrow(failed_df) > 0L) {
  length(unique(paste(failed_df$dataset, failed_df$object_id, sep = "||")))
} else {
  0L
}

candidate_summary <- if (nrow(candidate_df) > 0L) {
  as.data.table(candidate_df)[
    ,
    .(
      n_groups = .N,
      total_cells = sum(n_cells, na.rm = TRUE),
      median_DA_like = median(DA_like_composite_score, na.rm = TRUE),
      median_projection_competence = median(projection_competence_composite_score, na.rm = TRUE),
      median_A9_minus_A10 = median(A9_minus_A10_score_05A, na.rm = TRUE)
    ),
    by = .(dataset, candidate_class_05A)
  ][order(dataset, -n_groups)]
} else {
  data.table()
}

candidate_lines <- if (nrow(candidate_summary) > 0L) {
  apply(as.data.frame(candidate_summary), 1, function(x) {
    paste0(
      x[["dataset"]],
      " / ",
      x[["candidate_class_05A"]],
      ": groups=",
      x[["n_groups"]],
      "; cells=",
      x[["total_cells"]]
    )
  })
} else {
  "none"
}

coverage_summary <- if (nrow(matrix_df) > 0L) {
  as.data.table(matrix_df)[
    ,
    .(
      median_coverage = median(coverage_fraction, na.rm = TRUE),
      min_coverage = min(coverage_fraction, na.rm = TRUE),
      max_coverage = max(coverage_fraction, na.rm = TRUE)
    ),
    by = signature
  ][order(signature)]
} else {
  data.table()
}

coverage_lines <- if (nrow(coverage_summary) > 0L) {
  apply(as.data.frame(coverage_summary), 1, function(x) {
    paste0(
      x[["signature"]],
      ": median coverage=",
      round(as.numeric(x[["median_coverage"]]) * 100, 1),
      "%"
    )
  })
} else {
  character()
}

failed_lines <- if (nrow(failed_df) > 0L) {
  apply(as.data.frame(failed_df), 1, function(x) {
    paste0(x[["dataset"]], " :: ", x[["object_id"]], " — ", x[["message"]])
  })
} else {
  "none"
}

report_lines <- c(
  "05A DA/A9/A10/projection competence scoring report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("Objects in 04D annotated manifest: ", n_objects_total),
  paste0("Objects scored successfully: ", n_objects_success),
  paste0("Objects failed: ", n_objects_failed),
  paste0("Cell-level score rows: ", nrow(cell_df)),
  paste0("Group-level score rows: ", nrow(group_df)),
  paste0("Object-level score rows: ", nrow(object_df)),
  paste0("Candidate group rows: ", nrow(candidate_df)),
  "",
  "Signature coverage summary:",
  coverage_lines,
  "",
  "Candidate group summary:",
  candidate_lines,
  "",
  "Failed objects:",
  failed_lines,
  "",
  "Output files:",
  paste0("Signature gene sets: ", signature_gene_sets_csv),
  paste0("Cell-level scores: ", cell_level_scores_csv),
  paste0("Group-level scores: ", group_level_scores_csv),
  paste0("Object-level scores: ", object_level_scores_csv),
  paste0("Candidate groups: ", candidate_groups_csv),
  paste0("Matrix source audit: ", matrix_source_csv),
  paste0("Failed objects: ", failed_objects_csv),
  "",
  "Next step:",
  "05B_SAFETY_RISK_SCORING_AND_CONTRAST.R",
  "",
  "Journal-rigor note:",
  "Projection-associated molecular competence is based on transcriptomic expression of axon guidance, neurite maturation, and synaptic machinery genes. It is not evidence of real anatomical projection or functional integration."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("05A DA/A9/A10/projection competence scoring 运行结束\n")
cat("============================================================\n\n")

cat("Objects in manifest：", n_objects_total, "\n")
cat("Objects scored successfully：", n_objects_success, "\n")
cat("Objects failed：", n_objects_failed, "\n")
cat("Cell-level score rows：", nrow(cell_df), "\n")
cat("Group-level score rows：", nrow(group_df), "\n")
cat("Object-level score rows：", nrow(object_df), "\n")
cat("Candidate group rows：", nrow(candidate_df), "\n\n")

cat("输出文件：\n")
cat(signature_gene_sets_csv, "\n")
cat(cell_level_scores_csv, "\n")
cat(group_level_scores_csv, "\n")
cat(object_level_scores_csv, "\n")
cat(candidate_groups_csv, "\n")
cat(matrix_source_csv, "\n")
cat(failed_objects_csv, "\n")
cat(report_txt, "\n\n")

if (n_objects_failed == 0L) {
  cat("✅ 05A DA/A9/A10/projection competence scoring 完成。\n")
  cat("下一步进入 05B：safety-risk scoring and contrast。\n")
} else {
  cat("⚠️ 05A 完成，但有对象失败。请查看 05A_failed_objects.csv。\n")
}
