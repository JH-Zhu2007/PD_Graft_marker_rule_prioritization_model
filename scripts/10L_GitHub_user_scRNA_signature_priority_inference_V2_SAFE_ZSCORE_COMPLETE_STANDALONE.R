
options(stringsAsFactors = FALSE)
set.seed(20260716)

PROJECT_ROOT <- "D:/PD_Graft_Project"
OUTPUT_TAG   <- "10L_user_scRNA_frozen_predictor_inference_V2_SAFE_ZSCORE_COMPLETE_STANDALONE"
VERSION_TAG  <- "10L_V2"

USER_INPUT_RDS <- NA_character_

ENV_INPUT_RDS <- Sys.getenv("PD_GRAFT_USER_SCRNA_RDS", unset = "")
if (nzchar(ENV_INPUT_RDS)) USER_INPUT_RDS <- ENV_INPUT_RDS

DEMO_INPUT_RDS <- file.path(
  PROJECT_ROOT,
  "02_objects/04D_annotated_objects/GSE204796/01A_GSE204796_GSM6194008_D8_04D_annotated.rds"
)
RUN_DEMO_IF_USER_INPUT_MISSING <- TRUE

MAX_CELLS_FOR_INFERENCE <- Inf
MAX_MODEL_FILE_MB       <- 750
MAKE_FIGURES            <- TRUE

WRITE_FULL_PER_CELL_TABLE <- TRUE

PROJECT_ROOT <- normalizePath(PROJECT_ROOT, winslash = "/", mustWork = FALSE)
TABLE_DIR <- file.path(PROJECT_ROOT, "03_tables", OUTPUT_TAG)
FIG_DIR   <- file.path(PROJECT_ROOT, "04_figures", paste0(OUTPUT_TAG, "_pdf"))
TEXT_DIR  <- file.path(PROJECT_ROOT, "09_manuscript", OUTPUT_TAG)
MODEL_OUT_DIR <- file.path(PROJECT_ROOT, "05_models", OUTPUT_TAG)

dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TEXT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL_OUT_DIR, recursive = TRUE, showWarnings = FALSE)

log_lines <- character()
log_msg <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  log_lines <<- c(log_lines, msg)
}

write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
  log_msg("[10L] Wrote: ", path)
}

safe_head <- function(x, n = 6) {
  if (length(x) == 0) return(character())
  utils::head(x, n)
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

zscore_vec <- function(x) {

  x <- safe_numeric(x)
  ok <- is.finite(x)
  if (!any(ok)) return(rep(0, length(x)))
  m <- sum(x[ok]) / sum(ok)
  if (sum(ok) <= 1) return(rep(0, length(x)))
  ss <- sum((x[ok] - m)^2) / (sum(ok) - 1)
  s <- sqrt(ss)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  out <- (x - m) / s
  out[!is.finite(out)] <- 0
  out
}

logistic <- function(x) 1 / (1 + exp(-pmax(pmin(x, 30), -30)))

open_pdf <- function(path, width = 6, height = 5) {
  grDevices::pdf(file = path, width = width, height = height, useDingbats = FALSE, onefile = FALSE)
}
close_pdf <- function() {
  try(grDevices::dev.off(), silent = TRUE)
}

required_pkgs <- c("Matrix", "stats", "utils", "graphics", "grDevices")
optional_pkgs <- c("Seurat", "SeuratObject", "randomForest", "ranger", "caret", "glmnet", "xgboost")

pkg_status <- data.frame(
  package = c(required_pkgs, optional_pkgs),
  required = c(rep(TRUE, length(required_pkgs)), rep(FALSE, length(optional_pkgs))),
  available = vapply(c(required_pkgs, optional_pkgs), requireNamespace, logical(1), quietly = TRUE),
  stringsAsFactors = FALSE
)
write_csv(pkg_status, file.path(TABLE_DIR, paste0(VERSION_TAG, "_package_status.csv")))

if (!all(pkg_status$available[pkg_status$required])) {
  missing <- pkg_status$package[pkg_status$required & !pkg_status$available]
  stop("Missing required packages: ", paste(missing, collapse = ", "))
}

if (is.na(USER_INPUT_RDS) || !nzchar(USER_INPUT_RDS)) {
  if (RUN_DEMO_IF_USER_INPUT_MISSING && file.exists(DEMO_INPUT_RDS)) {
    INPUT_RDS <- DEMO_INPUT_RDS
    INPUT_MODE <- "demo_10I_recommended_object"
  } else {
    stop("USER_INPUT_RDS is NA and DEMO_INPUT_RDS was not found. Set USER_INPUT_RDS or Sys.setenv(PD_GRAFT_USER_SCRNA_RDS=...).")
  }
} else {
  INPUT_RDS <- USER_INPUT_RDS
  INPUT_MODE <- "user_supplied_object"
}
INPUT_RDS <- normalizePath(INPUT_RDS, winslash = "/", mustWork = FALSE)
if (!file.exists(INPUT_RDS)) stop("Input RDS not found: ", INPUT_RDS)

log_msg("[10L V2] Starting user scRNA signature priority inference with safe manual z-score/gene matching fixes...")
log_msg("[10L] Project root : ", PROJECT_ROOT)
log_msg("[10L] Input object  : ", INPUT_RDS)
log_msg("[10L] Input mode    : ", INPUT_MODE)
log_msg("[10L] Output tables : ", TABLE_DIR)
log_msg("[10L] Output figures: ", FIG_DIR)
log_msg("[10L] Output text   : ", TEXT_DIR)

model_search_dirs <- unique(c(
  file.path(PROJECT_ROOT, "05_models"),
  file.path(PROJECT_ROOT, "03_tables"),
  file.path(PROJECT_ROOT, "09_manuscript")
))
model_search_dirs <- model_search_dirs[dir.exists(model_search_dirs)]

all_project_files <- unlist(lapply(model_search_dirs, function(d) {
  list.files(d, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
}), use.names = FALSE)
all_project_files <- normalizePath(all_project_files, winslash = "/", mustWork = FALSE)

file_info <- data.frame(
  file_path = all_project_files,
  file_name = basename(all_project_files),
  file_ext = tolower(tools::file_ext(all_project_files)),
  size_mb = suppressWarnings(file.info(all_project_files)$size / 1024^2),
  stringsAsFactors = FALSE
)

model_pattern <- "09C|weak|label|ML|model|classifier|rf|random|forest|ranger|xgb|glm|elastic|frozen|priority|ideal|risk"
model_candidates <- file_info[
  grepl(model_pattern, file_info$file_path, ignore.case = TRUE) &
    file_info$file_ext %in% c("rds", "rda", "rdata", "csv", "txt", "tsv"),
]

write_csv(model_candidates, file.path(TABLE_DIR, paste0(VERSION_TAG, "_09C_model_candidate_file_manifest.csv")))

flatten_objects <- function(x, name = "object", depth = 0, max_depth = 4) {
  out <- list()
  out[[name]] <- x
  if (depth >= max_depth) return(out)
  if (is.list(x) && !inherits(x, c("data.frame", "Matrix", "dgCMatrix"))) {
    nms <- names(x)
    if (is.null(nms)) nms <- paste0("item", seq_along(x))
    for (i in seq_along(x)) {
      child_name <- paste0(name, "$", nms[i])
      out <- c(out, flatten_objects(x[[i]], child_name, depth + 1, max_depth))
    }
  }
  out
}

is_predict_like <- function(obj) {
  cl <- class(obj)
  any(cl %in% c("randomForest", "ranger", "train", "glm", "cv.glmnet", "glmnet", "xgb.Booster", "lm", "multinom", "nnet"))
}

extract_model_features <- function(obj) {
  feats <- character()

  if (!is.null(obj$xNames)) feats <- c(feats, obj$xNames)
  if (!is.null(obj$coefnames)) feats <- c(feats, obj$coefnames)

  if (!is.null(obj$forest$independent.variable.names)) feats <- c(feats, obj$forest$independent.variable.names)
  if (!is.null(obj$independent.variable.names)) feats <- c(feats, obj$independent.variable.names)
  if (!is.null(obj$variable.importance)) feats <- c(feats, names(obj$variable.importance))

  if (!is.null(obj$importance)) feats <- c(feats, rownames(obj$importance))
  if (!is.null(obj$forest$xlevels)) feats <- c(feats, names(obj$forest$xlevels))

  term_try <- try(stats::terms(obj), silent = TRUE)
  if (!inherits(term_try, "try-error")) feats <- c(feats, attr(term_try, "term.labels"))

  if (!is.null(obj$glmnet.fit$beta)) feats <- c(feats, rownames(obj$glmnet.fit$beta))
  if (!is.null(obj$beta)) feats <- c(feats, rownames(obj$beta))
  feats <- unique(gsub("`", "", feats))
  feats <- feats[!is.na(feats) & nzchar(feats)]

  feats <- feats[!grepl("\\(|\\)|:|Intercept|^\\.", feats, ignore.case = TRUE)]
  feats
}

model_objects <- list()
model_object_manifest <- data.frame()
loadable_files <- model_candidates[model_candidates$file_ext %in% c("rds", "rda", "rdata") & !is.na(model_candidates$size_mb) & model_candidates$size_mb <= MAX_MODEL_FILE_MB,]

for (i in seq_len(nrow(loadable_files))) {
  f <- loadable_files$file_path[i]
  ext <- loadable_files$file_ext[i]
  loaded <- list()
  err <- NA_character_
  if (ext == "rds") {
    obj <- try(readRDS(f), silent = TRUE)
    if (inherits(obj, "try-error")) {
      err <- as.character(obj)
    } else {
      loaded <- list(rds_object = obj)
    }
  } else {
    env <- new.env(parent = emptyenv())
    obj_names <- try(load(f, envir = env), silent = TRUE)
    if (inherits(obj_names, "try-error")) {
      err <- as.character(obj_names)
    } else {
      loaded <- mget(obj_names, envir = env)
    }
  }
  if (length(loaded) > 0) {
    flat <- unlist(lapply(names(loaded), function(nm) flatten_objects(loaded[[nm]], nm)), recursive = FALSE)
    for (nm in names(flat)) {
      obj <- flat[[nm]]
      pred <- is_predict_like(obj)
      feats <- extract_model_features(obj)
      row <- data.frame(
        file_path = f,
        object_name = nm,
        class = paste(class(obj), collapse = ";"),
        is_predict_like = pred,
        n_extracted_features = length(feats),
        first_features = paste(safe_head(feats, 12), collapse = ";"),
        load_error = NA_character_,
        stringsAsFactors = FALSE
      )
      model_object_manifest <- rbind(model_object_manifest, row)
      if (pred || length(feats) >= 3) {
        key <- paste0("model_", length(model_objects) + 1)
        model_objects[[key]] <- list(path = f, object_name = nm, object = obj, features = feats)
      }
    }
  } else {
    model_object_manifest <- rbind(model_object_manifest, data.frame(
      file_path = f,
      object_name = NA_character_,
      class = NA_character_,
      is_predict_like = FALSE,
      n_extracted_features = 0,
      first_features = NA_character_,
      load_error = err,
      stringsAsFactors = FALSE
    ))
  }
}

if (nrow(model_object_manifest) == 0) {
  model_object_manifest <- data.frame(
    file_path = character(), object_name = character(), class = character(),
    is_predict_like = logical(), n_extracted_features = integer(),
    first_features = character(), load_error = character()
  )
}
write_csv(model_object_manifest, file.path(TABLE_DIR, paste0(VERSION_TAG, "_loaded_model_object_manifest.csv")))

input_obj <- readRDS(INPUT_RDS)
input_class <- paste(class(input_obj), collapse = ";")

get_seurat_meta <- function(obj) {
  if (!is.null(obj@meta.data)) return(obj@meta.data)
  data.frame(row.names = colnames(obj))
}

get_seurat_embedding <- function(obj) {
  emb <- NULL
  if (inherits(obj, "Seurat")) {
    if ("umap" %in% names(obj@reductions)) {
      emb <- try(obj@reductions$umap@cell.embeddings[, 1:2, drop = FALSE], silent = TRUE)
    }
    if (inherits(emb, "try-error") || is.null(emb)) {
      if ("pca" %in% names(obj@reductions)) {
        emb <- try(obj@reductions$pca@cell.embeddings[, 1:2, drop = FALSE], silent = TRUE)
      }
    }
  }
  if (inherits(emb, "try-error")) emb <- NULL
  emb
}

get_expr_matrix <- function(obj) {
  if (inherits(obj, "Seurat")) {
    assay_name <- try(SeuratObject::DefaultAssay(obj), silent = TRUE)
    if (inherits(assay_name, "try-error") || is.null(assay_name) || !nzchar(assay_name)) {
      assay_name <- names(obj@assays)[1]
    }
    assay <- obj@assays[[assay_name]]

    if ("layers" %in% slotNames(assay)) {
      layer_names <- names(assay@layers)
      for (layer in c("data", "counts", "scale.data")) {
        if (layer %in% layer_names) {
          mat <- try(SeuratObject::LayerData(obj, assay = assay_name, layer = layer), silent = TRUE)
          if (!inherits(mat, "try-error") && !is.null(mat) && nrow(mat) > 0) return(mat)
        }
      }
    }

    for (slot_name in c("data", "counts", "scale.data")) {
      mat <- try(SeuratObject::GetAssayData(obj, assay = assay_name, slot = slot_name), silent = TRUE)
      if (!inherits(mat, "try-error") && !is.null(mat) && nrow(mat) > 0) return(mat)
    }
    stop("Could not extract expression matrix from Seurat object.")
  }
  if (inherits(obj, c("matrix", "dgCMatrix", "dgTMatrix", "data.frame"))) {
    mat <- as.matrix(obj)
    return(mat)
  }
  stop("Unsupported input object class: ", paste(class(obj), collapse = ";"))
}

expr <- get_expr_matrix(input_obj)
if (!inherits(expr, "dgCMatrix")) {
  expr <- try(Matrix::Matrix(as.matrix(expr), sparse = TRUE), silent = TRUE)
}
if (inherits(expr, "try-error")) stop("Failed to coerce expression matrix.")

meta <- if (inherits(input_obj, "Seurat")) get_seurat_meta(input_obj) else data.frame(row.names = colnames(expr))
emb <- if (inherits(input_obj, "Seurat")) get_seurat_embedding(input_obj) else NULL

cell_ids <- colnames(expr)
if (is.null(cell_ids)) cell_ids <- paste0("cell_", seq_len(ncol(expr)))
colnames(expr) <- cell_ids
if (nrow(meta) == ncol(expr)) {
  meta <- meta[cell_ids, , drop = FALSE]
} else {
  meta <- data.frame(row.names = cell_ids)
}

if (is.finite(MAX_CELLS_FOR_INFERENCE) && ncol(expr) > MAX_CELLS_FOR_INFERENCE) {
  keep <- sample(seq_len(ncol(expr)), MAX_CELLS_FOR_INFERENCE)
  expr <- expr[, keep, drop = FALSE]
  cell_ids <- colnames(expr)
  meta <- meta[cell_ids, , drop = FALSE]
  if (!is.null(emb)) emb <- emb[cell_ids, , drop = FALSE]
}

cluster_candidates <- c(
  "seurat_clusters", "cluster", "clusters", "cell_state", "celltype", "cell_type",
  "annotation", "annot", "predicted.id", "ident", "orig.ident"
)
cluster_col <- cluster_candidates[cluster_candidates %in% colnames(meta)][1]
if (is.na(cluster_col) || length(cluster_col) == 0) {
  cluster_col <- "cluster_auto_all_cells"
  meta[[cluster_col]] <- "all_cells"
}

input_summary <- data.frame(
  input_rds = INPUT_RDS,
  input_mode = INPUT_MODE,
  input_class = input_class,
  n_cells = ncol(expr),
  n_genes = nrow(expr),
  cluster_column = cluster_col,
  has_embedding = !is.null(emb),
  stringsAsFactors = FALSE
)
write_csv(input_summary, file.path(TABLE_DIR, paste0(VERSION_TAG, "_input_object_summary.csv")))

gene_sets <- list(
  DA_maturation = c("TH", "DDC", "SLC6A3", "SLC18A2", "NR4A2", "PITX3", "FOXA2", "LMX1A", "LMX1B", "EN1", "EN2", "ALDH1A1", "KCNJ6"),
  neuronal_maturation = c("MAP2", "RBFOX3", "TUBB3", "SYN1", "SNAP25", "DCX", "GAP43", "STMN2", "NEFL", "NEFM", "SYT1"),
  progenitor = c("SOX2", "SOX1", "NES", "PAX6", "VIM", "HES1", "ASCL1"),
  cell_cycle = c("MKI67", "TOP2A", "PCNA", "HMGB2"),
  stress_risk = c("JUN", "FOS", "DDIT3", "ATF3", "HSPA1A", "HSPA1B", "HSPB1", "HMOX1", "TXNIP", "HIF1A")
)

match_genes <- function(query_genes, matrix_rownames) {

  rn <- matrix_rownames
  rn_upper <- toupper(rn)
  q_upper <- toupper(unique(query_genes[!is.na(query_genes) & nzchar(query_genes)]))
  idx <- match(q_upper, rn_upper)
  matched <- rn[idx[!is.na(idx)]]
  unique(matched)
}

gene_set_coverage <- do.call(rbind, lapply(names(gene_sets), function(gs) {
  matched <- match_genes(gene_sets[[gs]], rownames(expr))
  data.frame(
    gene_set = gs,
    requested_n = length(gene_sets[[gs]]),
    matched_n = length(matched),
    matched_fraction = ifelse(length(gene_sets[[gs]]) == 0, NA, length(matched) / length(gene_sets[[gs]])),
    requested_genes = paste(gene_sets[[gs]], collapse = ";"),
    matched_genes = paste(matched, collapse = ";"),
    stringsAsFactors = FALSE
  )
}))
write_csv(gene_set_coverage, file.path(TABLE_DIR, paste0(VERSION_TAG, "_gene_set_coverage.csv")))

score_gene_set <- function(mat, genes) {
  present <- match_genes(genes, rownames(mat))
  if (length(present) == 0) return(rep(NA_real_, ncol(mat)))
  sub <- mat[present, , drop = FALSE]

  dense <- as.matrix(sub)
  if (is.null(dim(dense)) || nrow(dense) == 0) return(rep(NA_real_, ncol(mat)))
  score_acc <- rep(0, ncol(dense))
  n_used <- 0L
  for (ii in seq_len(nrow(dense))) {
    zz <- zscore_vec(dense[ii, ])
    if (any(is.finite(zz))) {
      score_acc <- score_acc + zz
      n_used <- n_used + 1L
    }
  }
  if (n_used == 0L) return(rep(NA_real_, ncol(mat)))
  score_acc / n_used
}

DA_score        <- score_gene_set(expr, gene_sets$DA_maturation)
neuron_score    <- score_gene_set(expr, gene_sets$neuronal_maturation)
prog_score      <- score_gene_set(expr, gene_sets$progenitor)
cycle_score     <- score_gene_set(expr, gene_sets$cell_cycle)
stress_score    <- score_gene_set(expr, gene_sets$stress_risk)

replace_na_score <- function(x) { if (all(is.na(x))) rep(0, length(x)) else ifelse(is.na(x), 0, x) }
DA_score     <- replace_na_score(DA_score)
neuron_score <- replace_na_score(neuron_score)
prog_score   <- replace_na_score(prog_score)
cycle_score  <- replace_na_score(cycle_score)
stress_score <- replace_na_score(stress_score)

maturation_proxy <- zscore_vec(DA_score + neuron_score)
risk_proxy       <- zscore_vec(prog_score + cycle_score + stress_score)
priority_proxy   <- zscore_vec(DA_score + neuron_score - prog_score - cycle_score - stress_score)
ideal_like_proxy_probability <- logistic(priority_proxy)
risk_like_proxy_probability  <- logistic(risk_proxy - maturation_proxy)

gene_upper <- toupper(rownames(expr))

make_newdata_for_model <- function(features, mat) {
  features <- unique(features[!is.na(features) & nzchar(features)])
  if (length(features) == 0) return(NULL)
  f_upper <- toupper(features)
  idx <- match(f_upper, toupper(rownames(mat)))
  present <- !is.na(idx)
  if (sum(present) == 0) return(NULL)

  nd <- matrix(0, nrow = ncol(mat), ncol = length(features))
  colnames(nd) <- features
  rownames(nd) <- colnames(mat)
  dense_present <- as.matrix(mat[idx[present], , drop = FALSE])
  nd[, present] <- t(dense_present)
  as.data.frame(nd, check.names = FALSE)
}

safe_predict_model <- function(model_obj, newdata) {
  if (is.null(newdata) || nrow(newdata) == 0) return(list(ok = FALSE, pred = NULL, error = "empty_newdata"))
  attempts <- list(
    quote(stats::predict(model_obj, newdata = newdata, type = "prob")),
    quote(stats::predict(model_obj, newdata = newdata, type = "response")),
    quote(stats::predict(model_obj, newdata = newdata)),
    quote(predict(model_obj, data = as.matrix(newdata))$predictions),
    quote(predict(model_obj, newdata = as.matrix(newdata)))
  )
  for (expr_call in attempts) {
    res <- try(eval(expr_call), silent = TRUE)
    if (!inherits(res, "try-error") && !is.null(res)) {
      return(list(ok = TRUE, pred = res, error = NA_character_))
    }
  }
  list(ok = FALSE, pred = NULL, error = "all_predict_attempts_failed")
}

extract_numeric_prediction <- function(pred, task_hint = "") {

  if (is.list(pred) && !is.data.frame(pred) && !is.matrix(pred)) {
    if (!is.null(pred$predictions)) pred <- pred$predictions
  }
  if (is.factor(pred)) return(as.numeric(pred))
  if (is.vector(pred) && is.numeric(pred)) return(as.numeric(pred))
  if (is.matrix(pred) || is.data.frame(pred)) {
    p <- as.data.frame(pred, check.names = FALSE)
    cn <- colnames(p)
    if (!is.null(cn)) {

      if (grepl("risk", task_hint, ignore.case = TRUE)) {
        hit <- grep("risk|positive|yes|true|1", cn, ignore.case = TRUE)[1]
      } else {
        hit <- grep("ideal|priority|positive|yes|true|1", cn, ignore.case = TRUE)[1]
      }
      if (!is.na(hit)) return(safe_numeric(p[[hit]]))
    }
    num_cols <- which(vapply(p, is.numeric, logical(1)))
    if (length(num_cols) > 0) return(safe_numeric(p[[num_cols[length(num_cols)]]]))
  }
  rep(NA_real_, ifelse(is.null(nrow(pred)), length(pred), nrow(pred)))
}

model_prediction_cols <- list()
feature_coverage_rows <- data.frame()
model_prediction_manifest <- data.frame()

if (length(model_objects) > 0) {
  for (nm in names(model_objects)) {
    rec <- model_objects[[nm]]
    feats <- rec$features
    matched <- match_genes(feats, rownames(expr))
    coverage <- ifelse(length(feats) == 0, 0, length(matched) / length(unique(feats)))
    task_hint <- paste(rec$path, rec$object_name, collapse = " ")
    feature_coverage_rows <- rbind(feature_coverage_rows, data.frame(
      model_key = nm,
      file_path = rec$path,
      object_name = rec$object_name,
      class = paste(class(rec$object), collapse = ";"),
      n_features_extracted = length(unique(feats)),
      n_features_matched = length(unique(matched)),
      feature_coverage = coverage,
      task_hint = task_hint,
      matched_features_preview = paste(safe_head(unique(matched), 20), collapse = ";"),
      stringsAsFactors = FALSE
    ))
    if (length(unique(feats)) >= 3 && coverage >= 0.25 && is_predict_like(rec$object)) {
      nd <- make_newdata_for_model(unique(feats), expr)
      pred_res <- safe_predict_model(rec$object, nd)
      if (pred_res$ok) {
        pred_num <- extract_numeric_prediction(pred_res$pred, task_hint)
        if (length(pred_num) == ncol(expr) && any(is.finite(pred_num))) {
          col_nm <- paste0("frozen09C_model_prediction_", nm)
          model_prediction_cols[[col_nm]] <- pred_num
          model_prediction_manifest <- rbind(model_prediction_manifest, data.frame(
            model_key = nm,
            file_path = rec$path,
            object_name = rec$object_name,
            prediction_column = col_nm,
            prediction_status = "success",
            prediction_error = NA_character_,
            stringsAsFactors = FALSE
          ))
        } else {
          model_prediction_manifest <- rbind(model_prediction_manifest, data.frame(
            model_key = nm, file_path = rec$path, object_name = rec$object_name,
            prediction_column = NA_character_, prediction_status = "failed_numeric_length_or_all_NA",
            prediction_error = "prediction_not_cell_length_or_nonfinite", stringsAsFactors = FALSE
          ))
        }
      } else {
        model_prediction_manifest <- rbind(model_prediction_manifest, data.frame(
          model_key = nm, file_path = rec$path, object_name = rec$object_name,
          prediction_column = NA_character_, prediction_status = "predict_failed",
          prediction_error = pred_res$error, stringsAsFactors = FALSE
        ))
      }
    }
  }
}

if (nrow(feature_coverage_rows) == 0) {
  feature_coverage_rows <- data.frame(
    model_key = character(), file_path = character(), object_name = character(), class = character(),
    n_features_extracted = integer(), n_features_matched = integer(), feature_coverage = numeric(),
    task_hint = character(), matched_features_preview = character()
  )
}
if (nrow(model_prediction_manifest) == 0) {
  model_prediction_manifest <- data.frame(
    model_key = character(), file_path = character(), object_name = character(),
    prediction_column = character(), prediction_status = character(), prediction_error = character()
  )
}
write_csv(feature_coverage_rows, file.path(TABLE_DIR, paste0(VERSION_TAG, "_model_feature_coverage.csv")))
write_csv(model_prediction_manifest, file.path(TABLE_DIR, paste0(VERSION_TAG, "_model_prediction_manifest.csv")))

has_successful_frozen_model_prediction <- length(model_prediction_cols) > 0
prediction_mode <- if (has_successful_frozen_model_prediction) {
  "serialized_09C_model_prediction_plus_signature_proxy"
} else {
  "signature_priority_proxy_only_no_serialized_09C_model_prediction"
}

per_cell <- data.frame(
  cell_id = cell_ids,
  cluster = as.character(meta[[cluster_col]]),
  DA_maturation_score = DA_score,
  neuronal_maturation_score = neuron_score,
  progenitor_score = prog_score,
  cell_cycle_score = cycle_score,
  stress_risk_score = stress_score,
  maturation_proxy_z = maturation_proxy,
  risk_proxy_z = risk_proxy,
  priority_proxy_z = priority_proxy,
  ideal_like_proxy_probability = ideal_like_proxy_probability,
  risk_like_proxy_probability = risk_like_proxy_probability,
  stringsAsFactors = FALSE
)

if (!is.null(emb)) {
  emb <- emb[cell_ids, , drop = FALSE]
  per_cell$embedding_1 <- emb[, 1]
  per_cell$embedding_2 <- emb[, 2]
}

if (length(model_prediction_cols) > 0) {
  for (nm in names(model_prediction_cols)) {
    per_cell[[nm]] <- model_prediction_cols[[nm]]
  }
}

model_pred_names <- names(model_prediction_cols)
if (length(model_pred_names) > 0) {

  pred_mat <- as.data.frame(model_prediction_cols)
  pred_z <- as.data.frame(lapply(pred_mat, zscore_vec))
  per_cell$frozen09C_prediction_ensemble_z <- rowMeans(as.matrix(pred_z), na.rm = TRUE)
  per_cell$final_10L_priority_score <- zscore_vec(per_cell$frozen09C_prediction_ensemble_z + per_cell$priority_proxy_z)
  final_score_source <- "frozen09C_prediction_ensemble_plus_signature_proxy"
} else {
  per_cell$final_10L_priority_score <- per_cell$priority_proxy_z
  final_score_source <- "signature_priority_proxy_only"
}
per_cell$final_10L_priority_probability <- logistic(per_cell$final_10L_priority_score)

if (WRITE_FULL_PER_CELL_TABLE) {
  write_csv(per_cell, file.path(TABLE_DIR, paste0(VERSION_TAG, "_per_cell_inference.csv")))
}

summary_fun <- function(x) {
  x <- safe_numeric(x)
  c(
    n = sum(is.finite(x)),
    mean = mean(x, na.rm = TRUE),
    median = stats::median(x, na.rm = TRUE),
    q25 = as.numeric(stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE)),
    q75 = as.numeric(stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE))
  )
}

clusters <- sort(unique(as.character(per_cell$cluster)))
cluster_rows <- lapply(clusters, function(cl) {
  idx <- per_cell$cluster == cl
  data.frame(
    cluster = cl,
    n_cells = sum(idx),
    final_priority_mean = mean(per_cell$final_10L_priority_score[idx], na.rm = TRUE),
    final_priority_median = stats::median(per_cell$final_10L_priority_score[idx], na.rm = TRUE),
    final_priority_q25 = as.numeric(stats::quantile(per_cell$final_10L_priority_score[idx], 0.25, na.rm = TRUE, names = FALSE)),
    final_priority_q75 = as.numeric(stats::quantile(per_cell$final_10L_priority_score[idx], 0.75, na.rm = TRUE, names = FALSE)),
    ideal_like_proxy_probability_mean = mean(per_cell$ideal_like_proxy_probability[idx], na.rm = TRUE),
    risk_like_proxy_probability_mean = mean(per_cell$risk_like_proxy_probability[idx], na.rm = TRUE),
    DA_maturation_mean = mean(per_cell$DA_maturation_score[idx], na.rm = TRUE),
    neuronal_maturation_mean = mean(per_cell$neuronal_maturation_score[idx], na.rm = TRUE),
    progenitor_mean = mean(per_cell$progenitor_score[idx], na.rm = TRUE),
    cell_cycle_mean = mean(per_cell$cell_cycle_score[idx], na.rm = TRUE),
    stress_risk_mean = mean(per_cell$stress_risk_score[idx], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
})
cluster_summary <- do.call(rbind, cluster_rows)
cluster_summary <- cluster_summary[order(-cluster_summary$final_priority_median), , drop = FALSE]
cluster_summary$priority_rank <- seq_len(nrow(cluster_summary))
write_csv(cluster_summary, file.path(TABLE_DIR, paste0(VERSION_TAG, "_cluster_level_inference_summary.csv")))

if (MAKE_FIGURES) {

  if (!is.null(emb)) {
    fig_a <- file.path(FIG_DIR, paste0(VERSION_TAG, "_A_embedding_final_priority_score.pdf"))
    open_pdf(fig_a, width = 5.6, height = 5.2)
    par(mar = c(4.5, 4.5, 2.8, 1.2), xaxs = "i", yaxs = "i")
    z <- per_cell$final_10L_priority_score
    cuts <- cut(z, breaks = stats::quantile(z, probs = seq(0, 1, length.out = 6), na.rm = TRUE), include.lowest = TRUE, labels = FALSE)
    pal <- c("#313695", "#74add1", "#ffffbf", "#f46d43", "#a50026")
    cols <- pal[pmax(1, pmin(5, cuts))]
    plot(per_cell$embedding_1, per_cell$embedding_2, pch = 16, cex = 0.22, col = cols,
         xlab = "Embedding 1", ylab = "Embedding 2", main = "10L final priority score")
    legend("topright", legend = c("low", "", "mid", "", "high"), col = pal, pch = 16, bty = "n", cex = 0.75, title = "Priority")
    close_pdf()
    log_msg("[10L] Wrote figure: ", fig_a)
  }

  fig_b <- file.path(FIG_DIR, paste0(VERSION_TAG, "_B_cluster_priority_dotrange.pdf"))
  open_pdf(fig_b, width = max(5.8, 0.35 * nrow(cluster_summary) + 3.5), height = 4.8)
  par(mar = c(7.5, 4.8, 3.0, 1.0))
  x <- seq_len(nrow(cluster_summary))
  y <- cluster_summary$final_priority_median
  plot(x, y, type = "n", xaxt = "n", xlab = "Cluster ordered by median priority", ylab = "Final 10L priority score",
       main = "Cluster-level transcriptomic priority")
  arrows(x, cluster_summary$final_priority_q25, x, cluster_summary$final_priority_q75, angle = 90, code = 3, length = 0.04, col = "grey55", lwd = 1.2)
  points(x, y, pch = 16, cex = 0.9)
  axis(1, at = x, labels = cluster_summary$cluster, las = 2, cex.axis = 0.8)
  abline(h = 0, lty = 3, col = "grey80")
  close_pdf()
  log_msg("[10L] Wrote figure: ", fig_b)

  fig_c <- file.path(FIG_DIR, paste0(VERSION_TAG, "_C_cluster_program_summary_heatmap.pdf"))
  heat_cols <- c("DA_maturation_mean", "neuronal_maturation_mean", "progenitor_mean", "cell_cycle_mean", "stress_risk_mean", "final_priority_median")
  heat_mat <- as.matrix(cluster_summary[, heat_cols, drop = FALSE])
  rownames(heat_mat) <- paste0("Cluster ", cluster_summary$cluster)

  heat_mat_z <- matrix(0, nrow = nrow(heat_mat), ncol = ncol(heat_mat), dimnames = dimnames(heat_mat))
  for (jj in seq_len(ncol(heat_mat))) heat_mat_z[, jj] <- zscore_vec(heat_mat[, jj])
  open_pdf(fig_c, width = 7.2, height = max(4.8, 0.28 * nrow(heat_mat_z) + 2.5))
  par(mar = c(7.5, 7.8, 3.0, 2.0))
  pal <- grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(101)
  zlim <- c(-2.5, 2.5)
  image(t(heat_mat_z[nrow(heat_mat_z):1, , drop = FALSE]), axes = FALSE, col = pal, zlim = zlim,
        main = "Cluster-level 10L program summary", xlab = "", ylab = "")
  axis(1, at = seq(0, 1, length.out = length(heat_cols)), labels = heat_cols, las = 2, cex.axis = 0.75)
  axis(2, at = seq(0, 1, length.out = nrow(heat_mat_z)), labels = rev(rownames(heat_mat_z)), las = 2, cex.axis = 0.75)
  box()
  close_pdf()
  log_msg("[10L] Wrote figure: ", fig_c)
}

execution_summary <- data.frame(
  version = VERSION_TAG,
  output_tag = OUTPUT_TAG,
  input_rds = INPUT_RDS,
  input_mode = INPUT_MODE,
  n_cells = ncol(expr),
  n_genes = nrow(expr),
  cluster_column = cluster_col,
  n_model_candidate_files = nrow(model_candidates),
  n_loaded_model_objects = nrow(model_object_manifest),
  n_candidate_model_records = length(model_objects),
  successful_serialized_model_prediction = has_successful_frozen_model_prediction,
  prediction_mode = prediction_mode,
  final_score_source = final_score_source,
  n_clusters = nrow(cluster_summary),
  top_cluster_by_priority = ifelse(nrow(cluster_summary) > 0, cluster_summary$cluster[1], NA_character_),
  claim_boundary = "transcriptomic prioritization only; not clinical prediction; not graft efficacy/safety; not lineage tracing",
  stringsAsFactors = FALSE
)
write_csv(execution_summary, file.path(TABLE_DIR, paste0(VERSION_TAG, "_execution_summary.csv")))

report_path <- file.path(TEXT_DIR, paste0(VERSION_TAG, "_user_scRNA_inference_report.txt"))
report <- c(
  "10L user scRNA signature priority inference report",
  "================================================",
  paste0("Project root: ", PROJECT_ROOT),
  paste0("Input object: ", INPUT_RDS),
  paste0("Input mode: ", INPUT_MODE),
  paste0("Cells: ", ncol(expr)),
  paste0("Genes: ", nrow(expr)),
  paste0("Cluster column: ", cluster_col),
  "",
  "Prediction mode:",
  paste0("  ", prediction_mode),
  paste0("Final score source: ", final_score_source),
  paste0("Successful serialized 09C model prediction: ", has_successful_frozen_model_prediction),
  "",
  "Important boundary:",
  "  The 10L output is a transcriptomic prioritization result only.",
  "  It must not be described as clinical prediction, graft efficacy prediction,",
  "  real safety prediction, tumorigenicity prediction, or lineage tracing.",
  "",
  "Main output files:",
  paste0("  ", file.path(TABLE_DIR, paste0(VERSION_TAG, "_per_cell_inference.csv"))),
  paste0("  ", file.path(TABLE_DIR, paste0(VERSION_TAG, "_cluster_level_inference_summary.csv"))),
  paste0("  ", file.path(TABLE_DIR, paste0(VERSION_TAG, "_model_feature_coverage.csv"))),
  paste0("  ", file.path(TABLE_DIR, paste0(VERSION_TAG, "_model_prediction_manifest.csv"))),
  "",
  "Next module:",
  "  10M_advanced_figure_plan_V2"
)
writeLines(report, report_path, useBytes = TRUE)
log_msg("[10L] Wrote report: ", report_path)

writeLines(log_lines, file.path(TEXT_DIR, paste0(VERSION_TAG, "_execution_log.txt")), useBytes = TRUE)

log_msg("")
log_msg("[10L V2] Completed user scRNA signature priority inference.")
log_msg("[10L] Input cells: ", ncol(expr), " genes: ", nrow(expr))
log_msg("[10L] Prediction mode: ", prediction_mode)
log_msg("[10L] Final score source: ", final_score_source)
log_msg("[10L] Successful serialized 09C model prediction: ", has_successful_frozen_model_prediction)
log_msg("[10L] Top cluster by priority: ", ifelse(nrow(cluster_summary) > 0, cluster_summary$cluster[1], "NA"))
log_msg("[10L] Output tables: ", TABLE_DIR)
log_msg("[10L] Output figures: ", FIG_DIR)
log_msg("[10L] Output text: ", TEXT_DIR)
log_msg("[10L] Next: 10M_advanced_figure_plan_V2")
