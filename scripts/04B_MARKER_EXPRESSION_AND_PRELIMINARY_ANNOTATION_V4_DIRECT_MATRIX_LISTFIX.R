
PROJECT_DIR <- "D:/PD_Graft_Project"

EXPRESSION_OBJECT_COL <- "final_expression_object"
GROUP_OBJECT_COL <- "initial_pca_object"

WRITE_GENE_LEVEL_TABLE <- TRUE
ALLOW_OBJECT_LEVEL_IF_NO_CLUSTER <- TRUE

MIN_CATEGORY_COVERAGE_FOR_SUGGESTION <- 0.4
MIN_MEAN_SCORE_FOR_ACTIVE_SIGNAL <- 0.05

set.seed(20260714)

cat("\n============================================================\n")
cat("04B V4：direct-matrix marker expression list-fix\n")
cat("============================================================\n\n")

required_pkgs <- c("Seurat", "SeuratObject", "data.table", "Matrix")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("缺少 R 包：", pkg, "。请先安装后再运行 04B V4。")
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

input_main_manifest <- file.path(tables_dir, "03C_strategy", "03C_main_analysis_object_manifest.csv")
input_marker_panel <- file.path(tables_dir, "04A_annotation_prep", "04A_marker_panel_master.csv")

out_tables_dir <- file.path(tables_dir, "04B_marker_expression")
dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

object_category_scores_csv <- file.path(out_tables_dir, "04B_object_marker_category_scores.csv")
group_category_scores_csv <- file.path(out_tables_dir, "04B_group_marker_category_scores.csv")
group_gene_expression_csv <- file.path(out_tables_dir, "04B_group_marker_gene_expression.csv")
preliminary_annotation_csv <- file.path(out_tables_dir, "04B_preliminary_annotation_suggestions.csv")
failed_objects_csv <- file.path(out_tables_dir, "04B_failed_objects.csv")
matrix_source_csv <- file.path(out_tables_dir, "04B_matrix_source_audit.csv")
report_txt <- file.path(reports_dir, "04B_marker_expression_and_preliminary_annotation_report.txt")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
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

read_csv_required <- function(path) {
  if (!file.exists(path)) {
    stop("找不到必要输入文件：", path)
  }
  data.table::fread(path, data.table = FALSE)
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
    if (nrow(mat) %in% cell_lengths && ncol(mat) %in% feature_lengths &&
        !(nrow(mat) %in% feature_lengths && ncol(mat) %in% cell_lengths)) {
      mat <- Matrix::t(mat)
    }
  }

  if (is.null(rownames(mat)) || length(rownames(mat)) != nrow(mat) ||
      any(is.na(rownames(mat))) || any(rownames(mat) == "")) {

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

  if (is.null(colnames(mat)) || length(colnames(mat)) != ncol(mat) ||
      any(is.na(colnames(mat))) || any(colnames(mat) == "")) {

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

marker_expression_from_matrix <- function(expr_info, marker_genes) {
  mat <- expr_info$matrix

  object_genes <- rownames(mat)
  gene_match <- match_genes_case_insensitive(marker_genes, object_genes)

  present <- unique(gene_match$matched_gene[gene_match$present])
  present <- present[!is.na(present)]

  if (length(present) == 0L) return(NULL)

  mat_sub <- mat[present, , drop = FALSE]

  if (expr_info$layer_type == "counts") {
    lib <- Matrix::colSums(mat)
    lib[is.na(lib) | lib <= 0] <- 1
    mat_sub <- t(t(mat_sub) / lib * 10000)
    mat_sub <- log1p(mat_sub)
  }

  df <- as.data.frame(t(as.matrix(mat_sub)))
  df <- df[, present, drop = FALSE]

  df
}

choose_group_column <- function(obj) {
  md <- obj@meta.data
  cn <- colnames(md)

  preferred <- c("seurat_clusters", "RNA_snn_res.0.5", "SCT_snn_res.0.5", "integrated_snn_res.0.5")

  hit <- preferred[preferred %in% cn]
  if (length(hit) > 0L) return(hit[[1L]])

  hit2 <- cn[grepl("cluster|snn_res|louvain|leiden", cn, ignore.case = TRUE)]
  if (length(hit2) > 0L) return(hit2[[1L]])

  NA_character_
}

get_group_vector <- function(expr_obj, expr_cells, group_path) {
  if (!is.na(group_path) && file.exists(group_path)) {
    gobj <- tryCatch(readRDS(group_path), error = function(e) NULL)

    if (!is.null(gobj)) {
      group_col <- choose_group_column(gobj)

      if (!is.na(group_col) && group_col %in% colnames(gobj@meta.data)) {
        md <- gobj@meta.data
        common <- intersect(expr_cells, rownames(md))

        if (length(common) >= max(10L, floor(0.5 * length(expr_cells)))) {
          groups <- rep(NA_character_, length(expr_cells))
          names(groups) <- expr_cells
          groups[common] <- as.character(md[common, group_col])
          groups[is.na(groups)] <- "unmatched_cell"
          return(list(groups = groups, group_source = paste0("03A_", group_col)))
        }
      }
    }
  }

  group_col2 <- choose_group_column(expr_obj)

  if (!is.na(group_col2) && group_col2 %in% colnames(expr_obj@meta.data)) {
    md2 <- expr_obj@meta.data
    common2 <- intersect(expr_cells, rownames(md2))

    if (length(common2) > 0L) {
      groups <- rep(NA_character_, length(expr_cells))
      names(groups) <- expr_cells
      groups[common2] <- as.character(md2[common2, group_col2])
      groups[is.na(groups)] <- "unmatched_cell"
      return(list(groups = groups, group_source = paste0("expression_", group_col2)))
    }
  }

  if (ALLOW_OBJECT_LEVEL_IF_NO_CLUSTER) {
    groups <- rep("object_all", length(expr_cells))
    names(groups) <- expr_cells
    return(list(groups = groups, group_source = "object_all_no_cluster"))
  }

  stop("No grouping information found.")
}

suggest_label_from_scores <- function(score_dt) {
  if (nrow(score_dt) == 0L) {
    return(data.frame(
      preliminary_suggestion = "unassigned",
      supporting_categories = NA_character_,
      caution = "No score data.",
      stringsAsFactors = FALSE
    ))
  }

  score_dt <- score_dt[coverage_fraction >= MIN_CATEGORY_COVERAGE_FOR_SUGGESTION]
  score_dt <- score_dt[mean_score >= MIN_MEAN_SCORE_FOR_ACTIVE_SIGNAL]

  if (nrow(score_dt) == 0L) {
    return(data.frame(
      preliminary_suggestion = "unassigned_low_marker_signal",
      supporting_categories = NA_character_,
      caution = "No category passed coverage/signal threshold.",
      stringsAsFactors = FALSE
    ))
  }

  score_dt <- score_dt[order(-mean_score)]
  top_cats <- score_dt$category[seq_len(min(5L, nrow(score_dt)))]

  has <- function(cat) cat %in% score_dt$category

  suggestion <- "mixed_or_unassigned_marker_signal"

  if (has("DA_core_identity") && has("neuronal_maturation_synapse")) {
    suggestion <- "DA_like_neuronal_candidate"
  } else if (has("midbrain_floor_plate_progenitor") && has("progenitor_neuroepithelial")) {
    suggestion <- "midbrain_progenitor_like_candidate"
  } else if (has("cell_cycle_proliferation") && has("progenitor_neuroepithelial")) {
    suggestion <- "cycling_progenitor_safety_risk_candidate"
  } else if (has("pluripotency_immature_risk")) {
    suggestion <- "immature_pluripotency_risk_signal_candidate"
  } else if (has("astrocyte_glial")) {
    suggestion <- "astrocyte_glial_candidate"
  } else if (has("oligodendrocyte_OPC")) {
    suggestion <- "oligodendrocyte_OPC_candidate"
  } else if (has("microglia_macrophage_immune")) {
    suggestion <- "immune_microglia_macrophage_candidate"
  } else if (has("vascular_pericyte_meningeal") || has("extracellular_matrix_fibroblast")) {
    suggestion <- "vascular_mesenchymal_candidate"
  } else if (has("GABAergic_neuron")) {
    suggestion <- "GABAergic_neuronal_candidate"
  } else if (has("glutamatergic_neuron")) {
    suggestion <- "glutamatergic_neuronal_candidate"
  } else if (has("serotonergic_neuron")) {
    suggestion <- "serotonergic_neuronal_candidate"
  } else if (has("cholinergic_neuron")) {
    suggestion <- "cholinergic_neuronal_candidate"
  } else if (has("stress_apoptosis_response")) {
    suggestion <- "stress_response_high_candidate"
  }

  data.frame(
    preliminary_suggestion = suggestion,
    supporting_categories = paste(top_cats, collapse = ";"),
    caution = "Preliminary marker-based suggestion only; requires multi-marker validation and dataset-context review.",
    stringsAsFactors = FALSE
  )
}

write_outputs <- function(
  object_category_list,
  group_category_list,
  group_gene_list,
  prelim_list,
  failed_list,
  matrix_source_list
) {
  object_category_df <- if (length(object_category_list) > 0L) data.table::rbindlist(object_category_list, fill = TRUE) else data.frame()
  group_category_df <- if (length(group_category_list) > 0L) data.table::rbindlist(group_category_list, fill = TRUE) else data.frame()
  group_gene_df <- if (length(group_gene_list) > 0L) data.table::rbindlist(group_gene_list, fill = TRUE) else data.frame()
  prelim_df <- if (length(prelim_list) > 0L) data.table::rbindlist(prelim_list, fill = TRUE) else data.frame()
  failed_df <- if (length(failed_list) > 0L) data.table::rbindlist(failed_list, fill = TRUE) else data.frame()
  matrix_source_df <- if (length(matrix_source_list) > 0L) data.table::rbindlist(matrix_source_list, fill = TRUE) else data.frame()

  atomic_write_csv(object_category_df, object_category_scores_csv)
  atomic_write_csv(group_category_df, group_category_scores_csv)
  atomic_write_csv(group_gene_df, group_gene_expression_csv)
  atomic_write_csv(prelim_df, preliminary_annotation_csv)
  atomic_write_csv(failed_df, failed_objects_csv)
  atomic_write_csv(matrix_source_df, matrix_source_csv)

  invisible(list(
    object_category_df = object_category_df,
    group_category_df = group_category_df,
    group_gene_df = group_gene_df,
    prelim_df = prelim_df,
    failed_df = failed_df,
    matrix_source_df = matrix_source_df
  ))
}

process_one_object <- function(row, marker_panel, all_marker_genes, all_categories) {
  ds <- row$dataset
  oid <- row$object_id
  expr_path <- row[[EXPRESSION_OBJECT_COL]]
  group_path <- row[[GROUP_OBJECT_COL]]

  obj <- readRDS(expr_path)

  expr_info <- extract_expression_matrix_direct(obj)
  expr_mat <- expr_info$matrix

  stamp(
    "  matrix：",
    nrow(expr_mat), " genes x ", ncol(expr_mat), " cells；",
    "method=", expr_info$method, "；layer=", expr_info$layer
  )

  expr_df <- marker_expression_from_matrix(expr_info, all_marker_genes)

  if (is.null(expr_df) || ncol(expr_df) == 0L) {
    stop("No marker expression could be extracted from matrix.")
  }

  group_info <- get_group_vector(obj, rownames(expr_df), group_path)
  groups <- as.character(group_info$groups)
  group_source <- group_info$group_source

  matrix_source_df <- data.frame(
    dataset = ds,
    object_id = oid,
    expression_object = expr_path,
    group_object = group_path,
    assay = expr_info$assay,
    layer = expr_info$layer,
    layer_type = expr_info$layer_type,
    extraction_method = expr_info$method,
    n_matrix_genes = nrow(expr_mat),
    n_matrix_cells = ncol(expr_mat),
    n_marker_genes_extracted = ncol(expr_df),
    group_source = group_source,
    stringsAsFactors = FALSE
  )

  object_category_list_one <- list()
  group_category_list_one <- list()
  group_gene_list_one <- list()
  prelim_list_one <- list()

  category_score_dt_list <- list()

  for (catg in all_categories) {
    panel_genes <- unique(marker_panel$gene_symbol[marker_panel$category == catg])
    matched <- match_genes_case_insensitive(panel_genes, colnames(expr_df))

    cat_genes <- unique(matched$matched_gene[matched$present])
    cat_genes <- cat_genes[!is.na(cat_genes)]

    coverage_fraction <- length(cat_genes) / length(unique(panel_genes))

    if (length(cat_genes) == 0L) next

    mat_cat <- as.matrix(expr_df[, cat_genes, drop = FALSE])
    score <- rowMeans(mat_cat, na.rm = TRUE)

    tmp_dt <- data.table(
      cell = rownames(expr_df),
      group_id = groups,
      category = catg,
      marker_score = score
    )

    group_score <- tmp_dt[
      ,
      .(
        n_cells = .N,
        mean_score = mean(marker_score, na.rm = TRUE),
        median_score = median(marker_score, na.rm = TRUE),
        pct_cells_score_gt0 = mean(marker_score > 0, na.rm = TRUE)
      ),
      by = .(group_id, category)
    ]

    group_score[, dataset := ds]
    group_score[, object_id := oid]
    group_score[, group_source := group_source]
    group_score[, n_panel_genes := length(unique(panel_genes))]
    group_score[, n_present_genes := length(cat_genes)]
    group_score[, coverage_fraction := coverage_fraction]
    group_score[, present_genes := paste(cat_genes, collapse = ";")]

    group_category_list_one[[length(group_category_list_one) + 1L]] <- as.data.frame(group_score)
    category_score_dt_list[[length(category_score_dt_list) + 1L]] <- group_score

    object_category_list_one[[length(object_category_list_one) + 1L]] <- data.frame(
      dataset = ds,
      object_id = oid,
      category = catg,
      n_cells = nrow(expr_df),
      n_panel_genes = length(unique(panel_genes)),
      n_present_genes = length(cat_genes),
      coverage_fraction = coverage_fraction,
      mean_score = mean(score, na.rm = TRUE),
      median_score = median(score, na.rm = TRUE),
      pct_cells_score_gt0 = mean(score > 0, na.rm = TRUE),
      present_genes = paste(cat_genes, collapse = ";"),
      stringsAsFactors = FALSE
    )

    if (WRITE_GENE_LEVEL_TABLE) {
      for (g in cat_genes) {
        gene_dt <- data.table(
          group_id = groups,
          expr = as.numeric(expr_df[[g]])
        )

        gene_sum <- gene_dt[
          ,
          .(
            n_cells = .N,
            mean_expression = mean(expr, na.rm = TRUE),
            median_expression = median(expr, na.rm = TRUE),
            pct_expressing = mean(expr > 0, na.rm = TRUE)
          ),
          by = group_id
        ]

        gene_sum[, dataset := ds]
        gene_sum[, object_id := oid]
        gene_sum[, group_source := group_source]
        gene_sum[, category := catg]
        gene_sum[, gene_symbol_matched := g]

        group_gene_list_one[[length(group_gene_list_one) + 1L]] <- as.data.frame(gene_sum)
      }
    }
  }

  if (length(category_score_dt_list) > 0L) {
    all_scores_obj <- data.table::rbindlist(category_score_dt_list, fill = TRUE)

    for (gid in unique(all_scores_obj$group_id)) {
      score_sub <- all_scores_obj[group_id == gid]
      sug <- suggest_label_from_scores(score_sub)
      top_score <- score_sub[order(-mean_score)][1]

      prelim_list_one[[length(prelim_list_one) + 1L]] <- data.frame(
        dataset = ds,
        object_id = oid,
        group_source = group_source,
        group_id = gid,
        n_cells_group = unique(score_sub$n_cells)[1],
        preliminary_suggestion = sug$preliminary_suggestion,
        supporting_categories = sug$supporting_categories,
        top_category = top_score$category,
        top_category_mean_score = top_score$mean_score,
        top_category_coverage_fraction = top_score$coverage_fraction,
        caution = sug$caution,
        stringsAsFactors = FALSE
      )
    }
  } else {
    stop("No category scores generated.")
  }

  rm(obj, expr_info, expr_mat, expr_df)
  gc(verbose = FALSE)

  list(
    object_category = object_category_list_one,
    group_category = group_category_list_one,
    group_gene = group_gene_list_one,
    prelim = prelim_list_one,
    matrix_source = list(matrix_source_df)
  )
}

stamp("读取 main manifest 和 marker panel。")

main_manifest <- read_csv_required(input_main_manifest)
marker_panel <- read_csv_required(input_marker_panel)

if (!EXPRESSION_OBJECT_COL %in% colnames(main_manifest)) {
  stop("main manifest 缺少表达对象路径列：", EXPRESSION_OBJECT_COL)
}

if (!GROUP_OBJECT_COL %in% colnames(main_manifest)) {
  main_manifest[[GROUP_OBJECT_COL]] <- NA_character_
}

if (!all(c("category", "gene_symbol") %in% colnames(marker_panel))) {
  stop("marker panel 缺少 category/gene_symbol。")
}

main_manifest <- main_manifest[file.exists(main_manifest[[EXPRESSION_OBJECT_COL]]), , drop = FALSE]

marker_panel$gene_symbol <- as.character(marker_panel$gene_symbol)
marker_panel$category <- as.character(marker_panel$category)

all_marker_genes <- unique(marker_panel$gene_symbol)
all_categories <- unique(marker_panel$category)

stamp("准备处理对象数量：", nrow(main_manifest))
stamp("marker categories：", length(all_categories), "；marker genes：", length(all_marker_genes))
stamp("V4 使用直接 matrix extraction，并修复 list 作用域。")

object_category_list <- list()
group_category_list <- list()
group_gene_list <- list()
prelim_list <- list()
failed_list <- list()
matrix_source_list <- list()

for (i in seq_len(nrow(main_manifest))) {
  ds <- main_manifest$dataset[[i]]
  oid <- main_manifest$object_id[[i]]

  stamp("04B V4 处理对象 ", i, " / ", nrow(main_manifest), "：", ds, " :: ", oid)

  row <- main_manifest[i, , drop = FALSE]

  result <- tryCatch({
    process_one_object(
      row = row,
      marker_panel = marker_panel,
      all_marker_genes = all_marker_genes,
      all_categories = all_categories
    )
  }, error = function(e) {
    msg <- conditionMessage(e)
    stamp("  对象失败但不中断：", msg)

    failed_list[[length(failed_list) + 1L]] <<- data.frame(
      dataset = ds,
      object_id = oid,
      expression_object = row[[EXPRESSION_OBJECT_COL]],
      group_object = row[[GROUP_OBJECT_COL]],
      stage = "object_processing",
      message = msg,
      stringsAsFactors = FALSE
    )

    NULL
  })

  if (!is.null(result)) {
    object_category_list <- c(object_category_list, result$object_category)
    group_category_list <- c(group_category_list, result$group_category)
    group_gene_list <- c(group_gene_list, result$group_gene)
    prelim_list <- c(prelim_list, result$prelim)
    matrix_source_list <- c(matrix_source_list, result$matrix_source)
  }

  write_outputs(
    object_category_list,
    group_category_list,
    group_gene_list,
    prelim_list,
    failed_list,
    matrix_source_list
  )
}

final_outputs <- write_outputs(
  object_category_list,
  group_category_list,
  group_gene_list,
  prelim_list,
  failed_list,
  matrix_source_list
)

object_category_df <- final_outputs$object_category_df
group_category_df <- final_outputs$group_category_df
group_gene_df <- final_outputs$group_gene_df
prelim_df <- final_outputs$prelim_df
failed_df <- final_outputs$failed_df
matrix_source_df <- final_outputs$matrix_source_df

n_objects_total <- nrow(main_manifest)

n_objects_success <- if (nrow(object_category_df) > 0L) {
  length(unique(paste(object_category_df$dataset, object_category_df$object_id, sep = "||")))
} else {
  0L
}

n_objects_failed <- if (nrow(failed_df) > 0L) {
  length(unique(paste(failed_df$dataset, failed_df$object_id, sep = "||")))
} else {
  0L
}

n_group_scores <- nrow(group_category_df)
n_gene_records <- nrow(group_gene_df)
n_prelim <- nrow(prelim_df)

suggestion_counts <- if (nrow(prelim_df) > 0L) {
  as.data.table(prelim_df)[, .N, by = preliminary_suggestion][order(-N)]
} else {
  data.table(preliminary_suggestion = character(), N = integer())
}

suggestion_lines <- if (nrow(suggestion_counts) > 0L) {
  paste0(suggestion_counts$preliminary_suggestion, ": ", suggestion_counts$N)
} else {
  character()
}

matrix_methods <- if (nrow(matrix_source_df) > 0L) {
  as.data.table(matrix_source_df)[, .N, by = .(extraction_method, layer_type, layer)][order(-N)]
} else {
  data.table()
}

matrix_method_lines <- if (nrow(matrix_methods) > 0L) {
  apply(as.data.frame(matrix_methods), 1, function(x) {
    paste0(x[["extraction_method"]], " / ", x[["layer_type"]], " / ", x[["layer"]], ": ", x[["N"]])
  })
} else {
  character()
}

report_lines <- c(
  "04B V4 direct-matrix marker expression and preliminary annotation report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Summary:",
  paste0("Objects in manifest: ", n_objects_total),
  paste0("Objects with marker scores: ", n_objects_success),
  paste0("Objects failed: ", n_objects_failed),
  paste0("Group-category score rows: ", n_group_scores),
  paste0("Group-marker gene expression rows: ", n_gene_records),
  paste0("Preliminary suggestion rows: ", n_prelim),
  "",
  "Matrix extraction methods:",
  matrix_method_lines,
  "",
  "Preliminary suggestion counts:",
  suggestion_lines,
  "",
  "Output files:",
  paste0("Object category scores: ", object_category_scores_csv),
  paste0("Group category scores: ", group_category_scores_csv),
  paste0("Group marker gene expression: ", group_gene_expression_csv),
  paste0("Preliminary annotation suggestions: ", preliminary_annotation_csv),
  paste0("Matrix source audit: ", matrix_source_csv),
  paste0("Failed objects: ", failed_objects_csv),
  "",
  "Next step:",
  "04C_REVIEW_MARKER_EXPRESSION_AND_DEFINE_ANNOTATION_RULES.R",
  "",
  "Journal-rigor note:",
  "04B V4 directly extracts matrices from full filtered objects and uses log-normalized counts where counts layers are used. Outputs are preliminary marker-supported suggestions, not final annotation labels."
)

writeLines(report_lines, report_txt)

cat("\n============================================================\n")
cat("04B V4 direct-matrix marker expression 运行结束\n")
cat("============================================================\n\n")

cat("manifest 对象数量：", n_objects_total, "\n")
cat("成功生成 marker scores 的对象：", n_objects_success, "\n")
cat("失败对象：", n_objects_failed, "\n")
cat("group-category score rows：", n_group_scores, "\n")
cat("group-marker gene expression rows：", n_gene_records, "\n")
cat("preliminary suggestion rows：", n_prelim, "\n\n")

cat("输出文件：\n")
cat(object_category_scores_csv, "\n")
cat(group_category_scores_csv, "\n")
cat(group_gene_expression_csv, "\n")
cat(preliminary_annotation_csv, "\n")
cat(matrix_source_csv, "\n")
cat(failed_objects_csv, "\n")
cat(report_txt, "\n\n")

if (n_objects_failed == 0L) {
  cat("✅ 04B V4 direct-matrix marker expression 完成。\n")
  cat("下一步进入 04C：review marker expression and define annotation rules。\n")
} else {
  cat("⚠️ 04B V4 完成，但有对象失败。请查看 04B_failed_objects.csv。\n")
}
