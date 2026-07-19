# ============================================================
# 11F FINAL
# FULL RESCAN projection-associated molecular support
# COMPLETE STANDALONE / NO PREVIOUS 11F TABLE DEPENDENCY
#
# Purpose:
# - Re-scan local RDS objects directly; do not read previous 11F V1/V2 tables.
# - Robustly extract expression matrices from Seurat / SummarizedExperiment / matrix / list objects.
# - Score projection-associated molecular competence modules.
# - De-duplicate repeated object-version state rows for final figures and 11H integration.
# - Output conservative projection-associated transcriptomic proxy support.
#
# Critical claim boundary:
# - This is NOT anatomical projection validation.
# - This is NOT retrograde tracing-confirmed graft integration.
# - This is NOT functional host integration proof.
# - Use only as projection-associated transcriptomic / molecular proxy support.
#
# No internet.
# No 00-10P rerun.
# No previous 11F table dependency.
# No claim upgrade.
# ============================================================

cat("\n[11F FINAL] Starting FULL RESCAN projection-associated molecular support + visual polish...\n")
cat("[11F FINAL] Mode: FULL local RDS scan + robust expression extraction + polished figures; no internet; no 00-10P rerun; no previous 11F table dependency.\n")
cat("[11F FINAL] Claim boundary: molecular projection-associated proxy support only; no anatomical-projection claim.\n")

project_root <- "D:/PD_Graft_Project"
object_root  <- file.path(project_root, "02_objects")

out_table_dir <- file.path(project_root, "03_tables", "11F_projection_associated_molecular_competence_proxy_FINAL_FULL_RESCAN_PUBLICATION_VISUAL_POLISH")
out_fig_dir   <- file.path(project_root, "04_figures", "11F_projection_associated_molecular_competence_proxy_FINAL_FULL_RESCAN_PUBLICATION_VISUAL_POLISH_pdf")
out_text_dir  <- file.path(project_root, "09_manuscript", "11F_projection_associated_molecular_competence_proxy_FINAL_FULL_RESCAN_PUBLICATION_VISUAL_POLISH")

dir.create(out_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_text_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------- safe helpers -------------------------
safe_chr <- function(x) {
  y <- as.character(x)
  y[is.na(y)] <- ""
  y
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

write_csv_safe <- function(df, file) {
  utils::write.csv(df, file, row.names = FALSE, na = "")
  cat("[11F FINAL] Wrote:", file, "\n")
}

write_tsv_safe <- function(df, file) {
  utils::write.table(df, file, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  cat("[11F FINAL] Wrote:", file, "\n")
}

norm01 <- function(v) {
  v <- safe_num(v)
  finite_v <- v[is.finite(v)]
  if (length(finite_v) < 1) return(rep(0, length(v)))
  mn <- min(finite_v)
  mx <- max(finite_v)
  if (!is.finite(mn) || !is.finite(mx) || abs(mx - mn) < 1e-12) {
    return(rep(0.5, length(v)))
  }
  out <- (v - mn) / (mx - mn)
  out[!is.finite(out)] <- 0
  out
}

zscore_cols <- function(mat) {
  mat <- as.matrix(mat)
  storage.mode(mat) <- "numeric"
  out <- mat
  if (ncol(mat) < 1) return(out)
  for (jj in seq_len(ncol(mat))) {
    v <- mat[, jj]
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
  out[out > 2.5] <- 2.5
  out[out < -2.5] <- -2.5
  out
}

value_to_color <- function(v, minv, maxv, pal) {
  v <- safe_num(v)
  idx <- round((v - minv) / (maxv - minv) * (length(pal) - 1)) + 1
  idx[!is.finite(idx)] <- 1
  idx[idx < 1] <- 1
  idx[idx > length(pal)] <- length(pal)
  pal[idx]
}

extract_accession_from_path <- function(path_value) {
  accession_hits <- regmatches(path_value, gregexpr("GSE[0-9]+", path_value))[[1]]
  if (length(accession_hits) > 0) return(accession_hits[1])
  return("unknown")
}

short_object_label <- function(path_value) {
  bn <- basename(path_value)
  bn <- sub("[.]rds$", "", bn, ignore.case = TRUE)
  bn <- gsub("^01A_", "", bn)
  bn <- gsub("_04D_annotated|_03A_reduced|_02B_filtered|_02A_qc", "", bn)
  bn <- gsub("GSM[0-9]+_", "", bn)
  bn <- gsub("[^A-Za-z0-9]+", "_", bn)
  if (nchar(bn) > 45) bn <- substr(bn, 1, 45)
  bn
}

# ------------------------- module definitions -------------------------
modules <- list(
  DA = c("TH", "SLC6A3", "DDC", "SLC18A2", "NR4A2", "FOXA2", "LMX1A", "LMX1B", "EN1", "EN2", "PITX3"),
  A9 = c("SOX6", "ALDH1A1", "KCNJ6", "GIRK2", "DACH1", "FOXP2", "SLC10A4"),
  A10 = c("CALB1", "CALB2", "OTX2", "TAC1", "NPY", "CARTPT"),
  Projection = c("ROBO1", "ROBO2", "SLIT1", "SLIT2", "DCC", "NTN1", "NRP1", "NRP2", "PLXNA1", "PLXNA2", "SEMA3A", "SEMA3C", "EPHA4", "EPHB1", "UNC5C"),
  Axon = c("GAP43", "STMN2", "TUBB3", "MAP1B", "DPYSL2", "L1CAM", "NCAM1", "CNTN2", "NEFL", "NEFM", "NEFH", "DCX"),
  Synaptic = c("SYN1", "SYN2", "SYP", "SNAP25", "SYT1", "VAMP2", "STXBP1", "DLG4", "RIMS1"),
  Maturation = c("RBFOX3", "MAP2", "TUBB3", "DCX", "STMN2", "SYN1", "SNAP25", "CAMK2A"),
  Risk = c("MKI67", "TOP2A", "PCNA", "MCM6", "HSPA1A", "DDIT3", "ATF3", "FOS", "JUN", "TP53", "BAX", "CASP3")
)

module_df <- data.frame(
  module = rep(names(modules), lengths(modules)),
  gene = unlist(modules, use.names = FALSE),
  stringsAsFactors = FALSE
)
write_csv_safe(module_df, file.path(out_table_dir, "11F_FINAL_projection_module_definitions.csv"))

# ------------------------- robust object extraction -------------------------
get_meta <- function(obj) {
  out <- data.frame(row.names = character(0))
  if ("Seurat" %in% class(obj)) {
    tmp <- tryCatch(obj@meta.data, error = function(e) NULL)
    if (!is.null(tmp) && is.data.frame(tmp)) return(tmp)
  }
  tmp <- tryCatch({
    if (!is.null(obj$meta.data) && is.data.frame(obj$meta.data)) obj$meta.data else NULL
  }, error = function(e) NULL)
  if (!is.null(tmp)) return(tmp)
  tmp <- tryCatch({
    if (!is.null(obj$metadata) && is.data.frame(obj$metadata)) obj$metadata else NULL
  }, error = function(e) NULL)
  if (!is.null(tmp)) return(tmp)
  return(out)
}

get_expr <- function(obj) {
  # Seurat route
  if ("Seurat" %in% class(obj)) {
    expr <- tryCatch({
      if (requireNamespace("Seurat", quietly = TRUE)) {
        assay_names <- names(obj@assays)
        assay_try <- c("RNA", "SCT", "integrated", assay_names)
        assay_try <- unique(assay_try[assay_try %in% assay_names])
        for (aa in assay_try) {
          m <- tryCatch(Seurat::GetAssayData(obj, assay = aa, slot = "data"), error = function(e) NULL)
          if (!is.null(m) && length(dim(m)) == 2 && nrow(m) > 0 && ncol(m) > 0) return(m)
          m <- tryCatch(Seurat::GetAssayData(obj, assay = aa, slot = "counts"), error = function(e) NULL)
          if (!is.null(m) && length(dim(m)) == 2 && nrow(m) > 0 && ncol(m) > 0) return(m)
        }
      }
      NULL
    }, error = function(e) NULL)
    if (!is.null(expr)) return(expr)
  }

  # SummarizedExperiment route
  if ("SummarizedExperiment" %in% class(obj)) {
    expr <- tryCatch({
      assay_names <- SummarizedExperiment::assayNames(obj)
      nm <- assay_names[1]
      if ("logcounts" %in% assay_names) nm <- "logcounts"
      if ("counts" %in% assay_names) nm <- "counts"
      SummarizedExperiment::assay(obj, nm)
    }, error = function(e) NULL)
    if (!is.null(expr) && length(dim(expr)) == 2) return(expr)
  }

  # Matrix / data.frame route
  if (is.matrix(obj) || inherits(obj, "Matrix")) {
    if (length(dim(obj)) == 2 && nrow(obj) > 0 && ncol(obj) > 0) return(obj)
  }
  if (is.data.frame(obj)) {
    rn <- rownames(obj)
    if (length(rn) == nrow(obj)) {
      numeric_cols <- sapply(obj, is.numeric)
      if (sum(numeric_cols) >= 2) {
        mat <- as.matrix(obj[, numeric_cols, drop = FALSE])
        rownames(mat) <- rn
        return(mat)
      }
    }
  }

  # list route
  if (is.list(obj)) {
    possible_names <- c("expr", "expression", "counts", "data", "matrix", "logcounts", "norm", "normalized")
    for (nm in possible_names) {
      if (!is.null(obj[[nm]])) {
        m <- obj[[nm]]
        if ((is.matrix(m) || inherits(m, "Matrix")) && length(dim(m)) == 2 && nrow(m) > 0 && ncol(m) > 0) return(m)
      }
    }
  }

  return(NULL)
}

pick_group <- function(meta, expr_cols) {
  if (!is.data.frame(meta) || nrow(meta) < 1) {
    return(rep("all_cells", length(expr_cols)))
  }
  # align metadata rows if possible
  if (!is.null(rownames(meta)) && length(intersect(rownames(meta), expr_cols)) > 0) {
    common <- intersect(expr_cols, rownames(meta))
    meta2 <- meta[common, , drop = FALSE]
    out <- rep("all_cells", length(expr_cols))
    names(out) <- expr_cols
    candidate_cols <- colnames(meta2)
  } else {
    meta2 <- meta
    out <- rep("all_cells", length(expr_cols))
    names(out) <- expr_cols
    candidate_cols <- colnames(meta2)
  }

  lower_cols <- tolower(candidate_cols)
  priorities <- c(
    "seurat_clusters", "cluster", "clusters", "state", "cell_state",
    "annotation", "celltype", "cell_type", "ident", "orig.ident", "sample"
  )
  selected <- ""
  for (pp in priorities) {
    hit <- candidate_cols[lower_cols == pp]
    if (length(hit) > 0) { selected <- hit[1]; break }
  }
  if (selected == "") {
    hit <- candidate_cols[grepl("cluster|state|annotation|celltype|cell_type|ident|sample|orig", lower_cols)]
    if (length(hit) > 0) selected <- hit[1]
  }
  if (selected == "") return(unname(out))

  v <- safe_chr(meta2[[selected]])
  v[v == ""] <- "all_cells"
  if (exists("common")) {
    out[common] <- v
    return(unname(out))
  } else {
    if (length(v) == length(expr_cols)) return(v)
    return(rep("all_cells", length(expr_cols)))
  }
}

detect_projection_metadata_cols <- function(meta, object_path) {
  if (!is.data.frame(meta) || nrow(meta) < 1) {
    return(data.frame())
  }
  cn <- colnames(meta)
  lower <- tolower(cn)
  hit <- grepl("retro|projection|project|tracer|trace|axon|target|injection|connect|pathway|ptpro|graft|host|striat|nigra|snc|snr", lower)
  if (!any(hit)) return(data.frame())
  out_list <- list()
  jj <- 1
  for (cc in cn[hit]) {
    v <- safe_chr(meta[[cc]])
    non_empty <- v[v != ""]
    out_list[[jj]] <- data.frame(
      object_path = object_path,
      column = cc,
      non_empty_count = length(non_empty),
      unique_count = length(unique(non_empty)),
      example_values = paste(head(unique(non_empty), 5), collapse = ";"),
      strict_projection_candidate = as.character(length(non_empty) >= 10 && length(unique(non_empty)) >= 2 && grepl("retro|tracer|trace|projection|injection|target", tolower(cc))),
      stringsAsFactors = FALSE
    )
    jj <- jj + 1
  }
  do.call(rbind, out_list)
}

score_module_for_cells <- function(expr, genes) {
  if (is.null(expr) || length(dim(expr)) != 2 || nrow(expr) < 1 || ncol(expr) < 1) {
    return(rep(NA_real_, 0))
  }
  rn <- rownames(expr)
  if (is.null(rn)) return(rep(NA_real_, ncol(expr)))
  rn_upper <- toupper(rn)
  gene_upper <- toupper(genes)
  hit_idx <- which(rn_upper %in% gene_upper)
  if (length(hit_idx) < 1) return(rep(NA_real_, ncol(expr)))
  sub <- expr[hit_idx, , drop = FALSE]
  out <- tryCatch({
    if (inherits(sub, "Matrix")) {
      Matrix::colMeans(sub)
    } else {
      colMeans(as.matrix(sub), na.rm = TRUE)
    }
  }, error = function(e) rep(NA_real_, ncol(expr)))
  safe_num(out)
}

# ------------------------- inventory -------------------------
if (!dir.exists(object_root)) {
  stop("[11F FINAL] Object root not found: ", object_root, call. = FALSE)
}

all_rds <- list.files(object_root, pattern = "[.]rds$", recursive = TRUE, full.names = TRUE)
candidate_accessions <- c("GSE132758", "GSE157783", "GSE178265", "GSE183248", "GSE200610", "GSE204796", "GSE233885")
candidate_rds <- all_rds[grepl(paste(candidate_accessions, collapse = "|"), all_rds)]

priority_score <- rep(0, length(candidate_rds))
priority_score <- priority_score + ifelse(grepl("04D_annotated_objects", candidate_rds), 100, 0)
priority_score <- priority_score + ifelse(grepl("03A_normalized_reduced", candidate_rds), 70, 0)
priority_score <- priority_score + ifelse(grepl("02B_qc_filtered", candidate_rds), 45, 0)
priority_score <- priority_score + ifelse(grepl("09E_external", candidate_rds), 40, 0)
priority_score <- priority_score + ifelse(grepl("GSE204796|GSE233885|GSE178265|GSE132758", candidate_rds), 20, 0)

candidate_rds <- candidate_rds[order(priority_score, decreasing = TRUE)]
max_scan <- min(120, length(candidate_rds))
selected_rds <- candidate_rds[seq_len(max_scan)]

inventory <- data.frame(
  object_path = candidate_rds,
  accession = vapply(candidate_rds, extract_accession_from_path, character(1)),
  priority_score = priority_score[order(priority_score, decreasing = TRUE)],
  selected_for_scan = candidate_rds %in% selected_rds,
  stringsAsFactors = FALSE
)
write_csv_safe(inventory, file.path(out_table_dir, "11F_FINAL_local_candidate_RDS_inventory.csv"))

cat("[11F FINAL] Candidate RDS detected:", length(candidate_rds), "\n")
cat("[11F FINAL] Selected RDS for robust scan:", length(selected_rds), "\n")

# ------------------------- scan and score -------------------------
readiness_list <- list()
presence_list <- list()
metadata_list <- list()
state_list <- list()

for (ii in seq_along(selected_rds)) {
  fp <- selected_rds[ii]
  cat("[11F FINAL] Reading candidate RDS ", ii, "/", length(selected_rds), ": ", fp, "\n", sep = "")
  obj <- tryCatch(readRDS(fp), error = function(e) e)
  readable <- !inherits(obj, "error")
  expr <- NULL
  meta <- data.frame()
  n_genes <- 0
  n_cells <- 0
  scorable <- FALSE
  object_class <- if (readable) paste(class(obj), collapse = ";") else "read_error"

  if (readable) {
    meta <- tryCatch(get_meta(obj), error = function(e) data.frame())
    expr <- tryCatch(get_expr(obj), error = function(e) NULL)
    if (!is.null(expr) && length(dim(expr)) == 2) {
      n_genes <- nrow(expr)
      n_cells <- ncol(expr)
    }
  }

  # metadata audit
  md <- tryCatch(detect_projection_metadata_cols(meta, fp), error = function(e) data.frame())
  if (nrow(md) > 0) {
    metadata_list[[length(metadata_list) + 1]] <- md
  }

  gene_presence <- data.frame()
  if (!is.null(expr) && length(dim(expr)) == 2 && !is.null(rownames(expr))) {
    rn_upper <- toupper(rownames(expr))
    pres_rows <- list()
    for (mn in names(modules)) {
      hits <- unique(modules[[mn]][toupper(modules[[mn]]) %in% rn_upper])
      pres_rows[[length(pres_rows) + 1]] <- data.frame(
        object_path = fp,
        accession = extract_accession_from_path(fp),
        module = mn,
        genes_defined = length(modules[[mn]]),
        genes_detected = length(hits),
        detected_genes = paste(hits, collapse = ";"),
        stringsAsFactors = FALSE
      )
    }
    gene_presence <- do.call(rbind, pres_rows)
    presence_list[[length(presence_list) + 1]] <- gene_presence
  }

  if (!is.null(expr) && length(dim(expr)) == 2 && nrow(expr) > 0 && ncol(expr) > 0 && !is.null(rownames(expr))) {
    module_scores <- list()
    detected_counts <- c()
    for (mn in names(modules)) {
      rn_upper <- toupper(rownames(expr))
      detected_counts[mn] <- sum(toupper(modules[[mn]]) %in% rn_upper)
      module_scores[[mn]] <- score_module_for_cells(expr, modules[[mn]])
    }
    enough_modules <- sum(detected_counts >= 1) >= 4
    if (enough_modules) {
      group_vec <- pick_group(meta, colnames(expr))
      if (length(group_vec) != ncol(expr)) group_vec <- rep("all_cells", ncol(expr))
      group_vec <- safe_chr(group_vec)
      group_vec[group_vec == ""] <- "all_cells"

      cell_df <- data.frame(
        cell_group = group_vec,
        stringsAsFactors = FALSE
      )
      for (mn in names(modules)) {
        vv <- module_scores[[mn]]
        if (length(vv) != nrow(cell_df)) vv <- rep(NA_real_, nrow(cell_df))
        cell_df[[mn]] <- safe_num(vv)
      }

      # Aggregate by cell_group
      agg_rows <- list()
      groups <- unique(cell_df$cell_group)
      for (gg in groups) {
        idx <- which(cell_df$cell_group == gg)
        if (length(idx) < 10) next
        row <- data.frame(
          accession = extract_accession_from_path(fp),
          object_label = short_object_label(fp),
          object_path = fp,
          state_label = gg,
          n_cells = length(idx),
          stringsAsFactors = FALSE
        )
        for (mn in names(modules)) {
          vv <- safe_num(cell_df[[mn]][idx])
          row[[paste0("mean_", mn)]] <- ifelse(sum(is.finite(vv)) > 0, mean(vv[is.finite(vv)]), NA_real_)
          row[[paste0("detected_", mn)]] <- detected_counts[mn]
        }
        agg_rows[[length(agg_rows) + 1]] <- row
      }
      if (length(agg_rows) > 0) {
        st <- do.call(rbind, agg_rows)
        state_list[[length(state_list) + 1]] <- st
        scorable <- TRUE
      }
    }
  }

  readiness_list[[length(readiness_list) + 1]] <- data.frame(
    object_path = fp,
    accession = extract_accession_from_path(fp),
    object_class = object_class,
    readable = readable,
    n_genes = n_genes,
    n_cells = n_cells,
    metadata_cols = ifelse(is.data.frame(meta), ncol(meta), 0),
    projection_metadata_candidate_cols = nrow(md),
    scorable = scorable,
    stringsAsFactors = FALSE
  )
}

readiness_df <- if (length(readiness_list) > 0) do.call(rbind, readiness_list) else data.frame()
presence_df <- if (length(presence_list) > 0) do.call(rbind, presence_list) else data.frame()
metadata_df <- if (length(metadata_list) > 0) do.call(rbind, metadata_list) else data.frame()
state_raw <- if (length(state_list) > 0) do.call(rbind, state_list) else data.frame()

write_csv_safe(readiness_df, file.path(out_table_dir, "11F_FINAL_object_readiness_projection_metadata_audit.csv"))
write_csv_safe(presence_df, file.path(out_table_dir, "11F_FINAL_projection_marker_gene_presence_by_object.csv"))
write_csv_safe(metadata_df, file.path(out_table_dir, "11F_FINAL_projection_retrograde_metadata_column_candidates.csv"))
write_csv_safe(state_raw, file.path(out_table_dir, "11F_FINAL_state_level_projection_identity_scores_raw.csv"))

# ------------------------- process state scores -------------------------
if (nrow(state_raw) > 0) {
  processed <- state_raw
  mean_cols <- grep("^mean_", colnames(processed), value = TRUE)
  for (cn in mean_cols) {
    processed[[cn]] <- safe_num(processed[[cn]])
  }

  favorable_cols <- c("mean_DA", "mean_A9", "mean_A10", "mean_Projection", "mean_Axon", "mean_Synaptic", "mean_Maturation")
  favorable_cols <- favorable_cols[favorable_cols %in% colnames(processed)]
  projection_cols <- c("mean_Projection", "mean_Axon", "mean_Synaptic")
  projection_cols <- projection_cols[projection_cols %in% colnames(processed)]
  risk_cols <- c("mean_Risk")
  risk_cols <- risk_cols[risk_cols %in% colnames(processed)]

  rowmean_cols <- function(df, cols) {
    if (length(cols) < 1) return(rep(0, nrow(df)))
    mat <- as.matrix(df[, cols, drop = FALSE])
    storage.mode(mat) <- "numeric"
    out <- rowMeans(mat, na.rm = TRUE)
    out[!is.finite(out)] <- 0
    out
  }

  processed$projection_identity_raw <- rowmean_cols(processed, projection_cols)
  processed$favorable_identity_raw <- rowmean_cols(processed, favorable_cols)
  processed$risk_raw <- rowmean_cols(processed, risk_cols)
  processed$projection_identity_score <- norm01(processed$projection_identity_raw)
  processed$favorable_identity_score <- norm01(processed$favorable_identity_raw)
  processed$risk_score <- norm01(processed$risk_raw)
  processed$projection_priority_balance <- processed$projection_identity_score - processed$risk_score

  # Compact unique state label
  processed$compact_state_label <- paste0(
    processed$accession, " ",
    gsub("[^A-Za-z0-9]+", "_", safe_chr(processed$state_label)),
    " n=", processed$n_cells
  )
  processed <- processed[order(processed$projection_priority_balance, decreasing = TRUE), , drop = FALSE]

  q75 <- as.numeric(stats::quantile(processed$projection_identity_score, 0.75, na.rm = TRUE))
  q50risk <- as.numeric(stats::quantile(processed$risk_score, 0.50, na.rm = TRUE))
  processed$projection_evidence_tier <- "projection_state_proxy_low"
  processed$projection_evidence_tier[processed$projection_identity_score >= q75 & processed$risk_score <= q50risk] <- "projection_state_proxy_high"
  processed$projection_evidence_tier[processed$projection_identity_score >= q75 & processed$risk_score > q50risk] <- "projection_state_proxy_high_with_risk_context"
  processed$projection_evidence_tier[processed$projection_identity_score < q75 & processed$projection_identity_score >= median(processed$projection_identity_score, na.rm = TRUE)] <- "projection_state_proxy_intermediate"

  evidence <- processed[, c(
    "accession", "object_label", "state_label", "compact_state_label",
    "n_cells", "projection_identity_score", "favorable_identity_score",
    "risk_score", "projection_priority_balance", "projection_evidence_tier"
  ), drop = FALSE]
} else {
  processed <- data.frame()
  evidence <- data.frame()
}

write_csv_safe(processed, file.path(out_table_dir, "11F_FINAL_state_level_projection_identity_scores_processed.csv"))
write_csv_safe(evidence, file.path(out_table_dir, "11F_FINAL_projection_evidence_tier_table_for_11H.csv"))

if (nrow(evidence) > 0) {
  tier_counts <- as.data.frame(table(evidence$projection_evidence_tier), stringsAsFactors = FALSE)
  colnames(tier_counts) <- c("projection_evidence_tier", "state_count")
} else {
  tier_counts <- data.frame(projection_evidence_tier = character(0), state_count = integer(0), stringsAsFactors = FALSE)
}
write_csv_safe(tier_counts, file.path(out_table_dir, "11F_FINAL_projection_evidence_tier_counts.csv"))


# ------------------------- FINAL full-rescan de-duplication + final figures -------------------------
cat("[11F FINAL] Starting de-duplicated 11H table construction and final visual polish...\n")

clean_state_label <- function(state_vec) {
  state_vec <- safe_chr(state_vec)
  state_vec <- gsub("[^A-Za-z0-9_]+", "_", state_vec)
  state_vec <- gsub("^_+|_+$", "", state_vec)
  state_vec[state_vec == ""] <- "state"
  state_vec
}

extract_sample_tag <- function(object_label_vec) {
  object_label_vec <- safe_chr(object_label_vec)
  out <- rep("", length(object_label_vec))
  for (tp in c("D8", "D14", "D21", "D28", "D35")) {
    hit <- grepl(paste0("(^|_)" , tp, "(_|$)"), object_label_vec, ignore.case = FALSE)
    out[hit & out == ""] <- tp
  }
  out[grepl("Unsort|Unsorted", object_label_vec, ignore.case = TRUE) & out == ""] <- "Unsorted"
  out[grepl("PTPRO", object_label_vec, ignore.case = TRUE) & out == ""] <- "PTPRO"
  out[grepl("CLSTN2", object_label_vec, ignore.case = TRUE) & out == ""] <- "CLSTN2"
  out[grepl("hESgraft12months|12m|12months", object_label_vec, ignore.case = TRUE) & out == ""] <- "12m graft"
  out[grepl("9m", object_label_vec, ignore.case = TRUE) & out == ""] <- "9m graft"
  out[out == ""] <- "state"
  out
}

make_display_label <- function(accession_vec, object_label_vec, state_vec, n_cells_vec) {
  accession_vec <- safe_chr(accession_vec)
  object_label_vec <- safe_chr(object_label_vec)
  state_vec <- clean_state_label(state_vec)
  n_cells_vec <- safe_num(n_cells_vec)
  sample_tag <- extract_sample_tag(object_label_vec)
  state_short <- state_vec
  numeric_state <- grepl("^[0-9]+$", state_short)
  state_short[numeric_state] <- paste0("C", state_short[numeric_state])
  out <- paste0(accession_vec, " ", sample_tag, "-", state_short, " n=", format(n_cells_vec, big.mark = ",", scientific = FALSE))
  # GSE178265 state labels are biologically meaningful; keep them compact without sample tag.
  g178 <- accession_vec == "GSE178265"
  out[g178] <- paste0(accession_vec[g178], " ", state_short[g178], " n=", format(n_cells_vec[g178], big.mark = ",", scientific = FALSE))
  # If sample tag is generic, avoid awkward state-state duplication.
  generic <- sample_tag == "state" & !g178
  out[generic] <- paste0(accession_vec[generic], " ", state_short[generic], " n=", format(n_cells_vec[generic], big.mark = ",", scientific = FALSE))
  out
}

shorten_label <- function(label_vec, max_n = 42) {
  label_vec <- safe_chr(label_vec)
  too_long <- nchar(label_vec) > max_n
  label_vec[too_long] <- paste0(substr(label_vec[too_long], 1, max_n), "...")
  label_vec
}

row_mean_from_df <- function(df, cols) {
  if (length(cols) < 1 || nrow(df) < 1) return(rep(0, nrow(df)))
  mat <- as.matrix(df[, cols, drop = FALSE])
  storage.mode(mat) <- "numeric"
  out <- rowMeans(mat, na.rm = TRUE)
  out[!is.finite(out)] <- 0
  out
}

aggregate_v3_rows <- function(df, module_cols) {
  if (nrow(df) < 1) return(data.frame())
  df$accession <- safe_chr(df$accession)
  df$object_label <- safe_chr(df$object_label)
  df$state_label_clean <- clean_state_label(df$state_label)
  df$n_cells <- safe_num(df$n_cells)
  df$sample_tag <- extract_sample_tag(df$object_label)
  df$dedup_key <- paste(df$accession, df$object_label, df$state_label_clean, sep = "||")
  for (cn in c(module_cols, "projection_identity_score", "favorable_identity_score", "risk_score", "projection_priority_balance")) {
    if (cn %in% colnames(df)) df[[cn]] <- safe_num(df[[cn]])
  }
  keys <- unique(df$dedup_key)
  out_list <- list()
  for (kk in seq_along(keys)) {
    sub_df <- df[df$dedup_key == keys[kk], , drop = FALSE]
    if (nrow(sub_df) < 1) next
    n_cell_value <- max(sub_df$n_cells, na.rm = TRUE)
    if (!is.finite(n_cell_value)) n_cell_value <- 0
    row <- data.frame(
      accession = sub_df$accession[1],
      object_label = sub_df$object_label[1],
      sample_tag = sub_df$sample_tag[1],
      state_label = sub_df$state_label_clean[1],
      n_cells = n_cell_value,
      duplicate_object_rows_merged = nrow(sub_df),
      stringsAsFactors = FALSE
    )
    for (cn in c(module_cols, "projection_identity_score", "favorable_identity_score", "risk_score", "projection_priority_balance")) {
      if (cn %in% colnames(sub_df)) {
        vv <- safe_num(sub_df[[cn]])
        row[[cn]] <- ifelse(sum(is.finite(vv)) > 0, mean(vv[is.finite(vv)]), 0)
      }
    }
    if ("projection_evidence_tier" %in% colnames(sub_df)) {
      tt <- table(safe_chr(sub_df$projection_evidence_tier))
      row$projection_evidence_tier <- names(tt)[which.max(as.integer(tt))]
    }
    out_list[[length(out_list) + 1]] <- row
  }
  out <- if (length(out_list) > 0) do.call(rbind, out_list) else data.frame()
  if (nrow(out) > 0) {
    out$compact_state_label <- make_display_label(out$accession, out$object_label, out$state_label, out$n_cells)
    if (!("projection_priority_balance" %in% colnames(out))) {
      out$projection_priority_balance <- safe_num(out$projection_identity_score) - safe_num(out$risk_score)
    }
    out <- out[order(out$projection_priority_balance, decreasing = TRUE), , drop = FALSE]
  }
  out
}

# Build FINAL de-duplicated table from the freshly scanned processed object.
module_cols <- c("mean_DA", "mean_A9", "mean_A10", "mean_Projection", "mean_Axon", "mean_Synaptic", "mean_Maturation", "mean_Risk")
module_cols <- module_cols[module_cols %in% colnames(processed)]

if (nrow(processed) > 0) {
  processed_for_plot <- aggregate_v3_rows(processed, module_cols)
  if (nrow(processed_for_plot) > 0) {
    # Recompute normalized identity/risk/balance from the de-duplicated module means.
    projection_cols_v3 <- c("mean_Projection", "mean_Axon", "mean_Synaptic")
    projection_cols_v3 <- projection_cols_v3[projection_cols_v3 %in% colnames(processed_for_plot)]
    favorable_cols_v3 <- c("mean_DA", "mean_A9", "mean_A10", "mean_Projection", "mean_Axon", "mean_Synaptic", "mean_Maturation")
    favorable_cols_v3 <- favorable_cols_v3[favorable_cols_v3 %in% colnames(processed_for_plot)]
    risk_cols_v3 <- c("mean_Risk")
    risk_cols_v3 <- risk_cols_v3[risk_cols_v3 %in% colnames(processed_for_plot)]
    processed_for_plot$projection_identity_raw_dedup <- row_mean_from_df(processed_for_plot, projection_cols_v3)
    processed_for_plot$favorable_identity_raw_dedup <- row_mean_from_df(processed_for_plot, favorable_cols_v3)
    processed_for_plot$risk_raw_dedup <- row_mean_from_df(processed_for_plot, risk_cols_v3)
    processed_for_plot$projection_identity_score <- norm01(processed_for_plot$projection_identity_raw_dedup)
    processed_for_plot$favorable_identity_score <- norm01(processed_for_plot$favorable_identity_raw_dedup)
    processed_for_plot$risk_score <- norm01(processed_for_plot$risk_raw_dedup)
    processed_for_plot$projection_priority_balance <- processed_for_plot$projection_identity_score - processed_for_plot$risk_score
    q75 <- as.numeric(stats::quantile(processed_for_plot$projection_identity_score, 0.75, na.rm = TRUE))
    q50risk <- as.numeric(stats::quantile(processed_for_plot$risk_score, 0.50, na.rm = TRUE))
    med_id <- as.numeric(stats::median(processed_for_plot$projection_identity_score, na.rm = TRUE))
    processed_for_plot$projection_evidence_tier <- "projection_state_proxy_low"
    processed_for_plot$projection_evidence_tier[processed_for_plot$projection_identity_score >= q75 & processed_for_plot$risk_score <= q50risk] <- "projection_state_proxy_high"
    processed_for_plot$projection_evidence_tier[processed_for_plot$projection_identity_score >= q75 & processed_for_plot$risk_score > q50risk] <- "projection_state_proxy_high_with_risk_context"
    processed_for_plot$projection_evidence_tier[processed_for_plot$projection_identity_score < q75 & processed_for_plot$projection_identity_score >= med_id] <- "projection_state_proxy_intermediate"
    processed_for_plot <- processed_for_plot[order(processed_for_plot$projection_priority_balance, decreasing = TRUE), , drop = FALSE]
  }
} else {
  processed_for_plot <- data.frame()
}

if (nrow(processed_for_plot) > 0) {
  evidence_for_11H <- processed_for_plot[, c(
    "accession", "object_label", "sample_tag", "state_label", "compact_state_label",
    "n_cells", "duplicate_object_rows_merged", "projection_identity_score",
    "favorable_identity_score", "risk_score", "projection_priority_balance", "projection_evidence_tier"
  ), drop = FALSE]
} else {
  evidence_for_11H <- data.frame()
}

write_csv_safe(processed_for_plot, file.path(out_table_dir, "11F_FINAL_state_level_projection_identity_scores_processed_DEDUP.csv"))
write_csv_safe(evidence_for_11H, file.path(out_table_dir, "11F_FINAL_projection_evidence_tier_table_for_11H_DEDUP.csv"))

if (nrow(evidence_for_11H) > 0) {
  tier_counts_final <- as.data.frame(table(evidence_for_11H$projection_evidence_tier), stringsAsFactors = FALSE)
  colnames(tier_counts_final) <- c("projection_evidence_tier", "state_count")
  tier_counts_final <- tier_counts_final[order(tier_counts_final$state_count, decreasing = TRUE), , drop = FALSE]
} else {
  tier_counts_final <- data.frame(projection_evidence_tier = character(0), state_count = integer(0), stringsAsFactors = FALSE)
}
write_csv_safe(tier_counts_final, file.path(out_table_dir, "11F_FINAL_projection_evidence_tier_counts_DEDUP.csv"))

# ------------------------- FINAL figures -------------------------
open_pdf <- function(filename, w, h) {
  grDevices::pdf(file.path(out_fig_dir, filename), width = w, height = h, onefile = FALSE, useDingbats = FALSE, paper = "special")
}

new_canvas <- function() {
  par(mar = c(0,0,0,0), oma = c(0,0,0,0), xaxs = "i", yaxs = "i", xpd = FALSE)
  plot.new()
  plot.window(xlim = c(0,1), ylim = c(0,1), xaxs = "i", yaxs = "i")
}

draw_title <- function(main, sub = "") {
  text(0.5, 0.965, main, cex = 0.96, font = 2, adj = c(0.5,0.5))
  if (nchar(sub) > 0) text(0.5, 0.932, sub, cex = 0.48, col = "gray35", adj = c(0.5,0.5))
}

panel_box <- function(x0, y0, x1, y1, label, value, note = "") {
  rect(x0, y0, x1, y1, border = "gray40", col = "gray98", lwd = 0.8)
  text((x0+x1)/2, y1-0.045, label, cex = 0.50, col = "gray30", font = 2)
  text((x0+x1)/2, (y0+y1)/2+0.008, value, cex = 1.00, font = 2)
  if (nchar(note) > 0) text((x0+x1)/2, y0+0.035, note, cex = 0.42, col = "gray35")
}

palette_div <- grDevices::colorRampPalette(c("#2C7FB8", "#F7F7F7", "#B2182B"))(201)
palette_tier <- grDevices::colorRampPalette(c("gray88", "gray45"))(max(2, nrow(tier_counts_final)))

# FigA
figA <- "11F_FINAL_FigA_projection_metadata_audit_and_claim_boundary.pdf"
open_pdf(figA, 13.8, 7.2)
new_canvas()
draw_title("11F evidence audit: projection-associated support layer",
           "Full RDS rescan; transcriptomic module support only; no anatomical projection claim.")

detected_n <- length(candidate_rds)
attempt_n <- length(selected_rds)
readable_n <- ifelse(nrow(readiness_df) > 0, sum(readiness_df$readable, na.rm = TRUE), 0)
scorable_n <- ifelse(nrow(readiness_df) > 0, sum(readiness_df$scorable, na.rm = TRUE), 0)
metadata_n <- nrow(metadata_df)
strict_n <- ifelse(nrow(metadata_df) > 0, sum(metadata_df$strict_projection_candidate == "TRUE"), 0)
state_n_raw <- nrow(processed)
state_n_dedup <- nrow(processed_for_plot)
tier_n_dedup <- nrow(evidence_for_11H)

# Compact boxed flow. All text is drawn inside boxes; arrows are outside box borders.
flow_x0 <- 0.045
flow_x1 <- 0.955
flow_y0 <- 0.625
flow_y1 <- 0.835
flow_gap <- 0.015
flow_n <- 6
box_w <- (flow_x1 - flow_x0 - flow_gap * (flow_n - 1)) / flow_n
box_x <- flow_x0 + (seq_len(flow_n) - 1) * (box_w + flow_gap)
labels <- c("Local input", "Object scan", "Metadata audit", "State scoring", "Strict tracing", "Final claim")
values <- c(
  paste0(detected_n, " RDS"),
  paste0(readable_n, "/", attempt_n, " readable"),
  paste0(metadata_n, " candidates"),
  paste0(state_n_raw, " raw\n", state_n_dedup, " dedup"),
  paste0(strict_n, " retained"),
  "projection-associated\nproxy support"
)
for (i in seq_len(flow_n)) {
  x0 <- box_x[i]
  x1 <- x0 + box_w
  rect(x0, flow_y0, x1, flow_y1, border = "gray45", col = "gray98", lwd = 0.75)
  text((x0+x1)/2, flow_y1 - 0.038, labels[i], cex = 0.48, col = "gray28", font = 2)
  value_lines <- strsplit(values[i], "\\n", fixed = FALSE)[[1]]
  line_y <- seq((flow_y0 + flow_y1)/2 + 0.022, (flow_y0 + flow_y1)/2 - 0.032, length.out = length(value_lines))
  for (jj in seq_along(value_lines)) {
    text((x0+x1)/2, line_y[jj], value_lines[jj], cex = 0.66, font = 2, col = "gray10")
  }
  if (i < flow_n) {
    arrows(x1 + 0.003, (flow_y0+flow_y1)/2, box_x[i+1] - 0.003, (flow_y0+flow_y1)/2,
           length = 0.045, lwd = 0.55, col = "gray60")
  }
}

# Claim-boundary cards. They are deliberately larger than previous versions to avoid overflow.
allowed_x0 <- 0.050; allowed_x1 <- 0.480
prohib_x0  <- 0.520; prohib_x1  <- 0.950
card_y0 <- 0.165; card_y1 <- 0.535
rect(allowed_x0, card_y0, allowed_x1, card_y1, border = "gray55", col = "white", lwd = 0.75)
rect(prohib_x0,  card_y0, prohib_x1,  card_y1, border = "gray55", col = "white", lwd = 0.75)
text(allowed_x0 + 0.018, card_y1 - 0.045, "Allowed interpretation", adj = c(0,0.5), cex = 0.56, font = 2)
text(prohib_x0  + 0.018, card_y1 - 0.045, "Prohibited interpretation", adj = c(0,0.5), cex = 0.56, font = 2)
allowed <- c(
  "Projection-associated transcriptomic support",
  "Molecular competence / axon-guidance module evidence",
  "A9/A10-like functional identity support",
  "Downstream 11H integration only as conservative evidence"
)
prohibited <- c(
  "True anatomical projection validation",
  "Host integration confirmed",
  "Functional projection proven",
  "Retrograde tracing-confirmed graft integration"
)
for (i in seq_along(allowed)) {
  text(allowed_x0 + 0.023, card_y1 - 0.100 - (i-1)*0.062, paste0("- ", allowed[i]),
       adj = c(0,0.5), cex = 0.48, col = "gray10")
}
for (i in seq_along(prohibited)) {
  text(prohib_x0 + 0.023, card_y1 - 0.100 - (i-1)*0.062, paste0("- ", prohibited[i]),
       adj = c(0,0.5), cex = 0.48, col = "gray10")
}
decision_text <- ifelse(tier_n_dedup > 0,
                        "Decision: PROJECTION_MODULE_PROXY_ROWS_RESCUED_AND_DEDUPED_FOR_11H",
                        "Decision: NO_SCORABLE_STATE_ROWS_RETAINED")
text(0.5, 0.080, decision_text, cex = 0.52, font = 2, col = "gray25")
dev.off()
cat("[11F FINAL] Wrote figure:", file.path(out_fig_dir, figA), "\n")

# FigB heatmap
figB <- "11F_FINAL_FigB_projection_identity_module_heatmap_DEDUP_FULL_RESCAN.pdf"
open_pdf(figB, 13.2, 7.2)
new_canvas()
if (nrow(processed_for_plot) < 2) {
  draw_title("Projection-associated module landscape", "No sufficient scorable state rows detected.")
  text(0.5, 0.50, "No sufficient scorable state rows detected.", cex = 0.85, col = "gray35")
} else {
  draw_title("Projection-associated module landscape",
             "De-duplicated state-level weighted averages; column z-scores shown for visualization only.")
  plot_df <- processed_for_plot[seq_len(min(18, nrow(processed_for_plot))), , drop = FALSE]
  heat_cols <- c("mean_DA", "mean_A9", "mean_A10", "mean_Projection", "mean_Axon", "mean_Synaptic", "mean_Maturation", "mean_Risk")
  heat_cols <- heat_cols[heat_cols %in% colnames(plot_df)]
  module_labs <- gsub("^mean_", "", heat_cols)
  mat <- as.matrix(plot_df[, heat_cols, drop = FALSE])
  zmat <- zscore_cols(mat)
  colnames(zmat) <- module_labs
  rownames(zmat) <- plot_df$compact_state_label
  heat_source <- data.frame(compact_state_label = rownames(zmat), zmat, check.names = FALSE)
  write_csv_safe(heat_source, file.path(out_table_dir, "11F_FINAL_FigB_heatmap_zscore_source_DEDUP.csv"))

  hm_x0 <- 0.245; hm_x1 <- 0.800; hm_y0 <- 0.150; hm_y1 <- 0.860
  leg_x0 <- 0.875; leg_x1 <- 0.895; leg_y0 <- hm_y0; leg_y1 <- hm_y1
  nr <- nrow(zmat); nc <- ncol(zmat); cw <- (hm_x1-hm_x0)/nc; ch <- (hm_y1-hm_y0)/nr
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      xl <- hm_x0 + (j-1)*cw; xr <- hm_x0 + j*cw
      yt <- hm_y1 - (i-1)*ch; yb <- hm_y1 - i*ch
      rect(xl, yb, xr, yt, col = value_to_color(zmat[i,j], -2.5, 2.5, palette_div), border = "white", lwd = 0.3)
    }
  }
  rect(hm_x0, hm_y0, hm_x1, hm_y1, border = "gray35", lwd = 0.8)
  for (i in seq_len(nr)) {
    yy <- hm_y1 - (i-0.5)*ch
    text(hm_x0 - 0.012, yy, shorten_label(rownames(zmat)[i], 44), cex = 0.34, adj = c(1,0.5), col = "gray10")
  }
  for (j in seq_len(nc)) {
    xx <- hm_x0 + (j-0.5)*cw
    text(xx, 0.088, colnames(zmat)[j], cex = 0.48, srt = 90, adj = c(0.5,0.5), col = "gray10")
  }
  nleg <- 120
  for (k in seq_len(nleg)) {
    yb <- leg_y0 + (k-1)/nleg*(leg_y1-leg_y0)
    yt <- leg_y0 + k/nleg*(leg_y1-leg_y0)
    val <- -2.5 + (k-0.5)/nleg*5
    rect(leg_x0, yb, leg_x1, yt, col = value_to_color(val, -2.5, 2.5, palette_div), border = NA)
  }
  rect(leg_x0, leg_y0, leg_x1, leg_y1, border = "gray35", lwd = 0.6)
  for (tv in c(-2.5, 0, 2.5)) {
    yy <- leg_y0 + (tv + 2.5)/5*(leg_y1-leg_y0)
    segments(leg_x1, yy, leg_x1 + 0.008, yy, col = "gray30", lwd = 0.5)
    text(leg_x1 + 0.015, yy, ifelse(tv > 0, paste0("+", tv), as.character(tv)), cex = 0.44, adj = c(0,0.5), col = "gray20")
  }
  text(leg_x1 + 0.055, (leg_y0 + leg_y1)/2, "z-score", cex = 0.47, srt = 90, adj = c(0.5,0.5), col = "gray25")
}
dev.off()
cat("[11F FINAL] Wrote figure:", file.path(out_fig_dir, figB), "\n")

# FigC side-rail landscape
figC <- "11F_FINAL_FigC_projection_identity_risk_landscape_SIDE_RAIL_FULL_RESCAN.pdf"
open_pdf(figC, 11.2, 6.6)
new_canvas()
if (nrow(evidence_for_11H) < 2) {
  draw_title("Projection-associated identity-risk landscape", "No sufficient evidence-tier rows detected.")
  text(0.5, 0.50, "No sufficient evidence-tier rows detected.", cex = 0.85, col = "gray35")
} else {
  draw_title("Projection-associated identity-risk landscape",
             "De-duplicated transcriptomic proxy support; top states labeled in side rail; no anatomical projection claim.")
  ev <- evidence_for_11H
  ev$risk_score <- safe_num(ev$risk_score)
  ev$projection_identity_score <- safe_num(ev$projection_identity_score)
  ev$projection_priority_balance <- safe_num(ev$projection_priority_balance)
  px0 <- 0.095; px1 <- 0.690; py0 <- 0.145; py1 <- 0.850
  rail_x0 <- 0.755; rail_x1 <- 0.975
  xvals <- ev$risk_score; yvals <- ev$projection_identity_score
  x_min <- 0; x_max <- 1; y_min <- 0; y_max <- 1
  map_x <- function(v) px0 + (pmax(pmin(v, x_max), x_min)-x_min)/(x_max-x_min)*(px1-px0)
  map_y <- function(v) py0 + (pmax(pmin(v, y_max), y_min)-y_min)/(y_max-y_min)*(py1-py0)
  rect(px0, py0, px1, py1, border = "gray35", col = NA, lwd = 0.8)
  for (tick in seq(0, 1, by = 0.2)) {
    xx <- map_x(tick); yy <- map_y(tick)
    segments(xx, py0, xx, py1, col = "gray92", lwd = 0.5)
    segments(px0, yy, px1, yy, col = "gray92", lwd = 0.5)
    text(xx, 0.090, sprintf("%.1f", tick), cex = 0.50, col = "gray20")
    text(0.060, yy, sprintf("%.1f", tick), cex = 0.50, srt = 90, col = "gray20")
  }
  x_med <- stats::median(xvals, na.rm = TRUE); y_med <- stats::median(yvals, na.rm = TRUE)
  segments(map_x(x_med), py0, map_x(x_med), py1, col = "gray75", lty = 2, lwd = 0.75)
  segments(px0, map_y(y_med), px1, map_y(y_med), col = "gray75", lty = 2, lwd = 0.75)
  top <- ev[order(ev$projection_priority_balance, decreasing = TRUE), , drop = FALSE]
  top <- top[seq_len(min(8, nrow(top))), , drop = FALSE]
  is_top <- ev$compact_state_label %in% top$compact_state_label
  point_cex <- 0.46 + 0.55 * norm01(log10(safe_num(ev$n_cells) + 1))
  points(map_x(xvals[!is_top]), map_y(yvals[!is_top]), pch = 21, bg = "gray82", col = "gray35", cex = point_cex[!is_top], lwd = 0.45)
  points(map_x(xvals[is_top]), map_y(yvals[is_top]), pch = 21, bg = "#2C7FB8", col = "gray20", cex = point_cex[is_top] + 0.16, lwd = 0.6)
  top <- top[order(top$projection_identity_score, decreasing = TRUE), , drop = FALSE]
  rail_y <- seq(py1 - 0.040, py1 - 0.340, length.out = nrow(top))
  for (i in seq_len(nrow(top))) {
    xx <- map_x(top$risk_score[i]); yy <- map_y(top$projection_identity_score[i]); ry <- rail_y[i]
    segments(xx, yy, rail_x0 - 0.018, ry, col = "gray55", lwd = 0.60)
    text(rail_x0, ry, shorten_label(top$compact_state_label[i], 35), cex = 0.43, adj = c(0,0.5), col = "gray10")
  }
  text((px0 + px1)/2, 0.035, "Risk / stress module score", cex = 0.70)
  text(0.020, (py0 + py1)/2, "Projection-associated identity score", cex = 0.70, srt = 90)
  text(rail_x0, py0 + 0.030, "Top states by priority balance", cex = 0.43, adj = c(0,0.5), col = "gray35")
}
dev.off()
cat("[11F FINAL] Wrote figure:", file.path(out_fig_dir, figC), "\n")

# FigD tier summary
figD <- "11F_FINAL_FigD_projection_evidence_tier_summary_DEDUP.pdf"
open_pdf(figD, 10.8, 6.4)
new_canvas()
if (nrow(tier_counts_final) < 1) {
  draw_title("Projection evidence tier summary", "No evidence-tier rows detected.")
  text(0.5, 0.50, "No evidence-tier rows detected.", cex = 0.85, col = "gray35")
} else {
  draw_title("Projection evidence tier summary",
             "De-duplicated tier assignment for downstream 11H integration; conservative proxy evidence only.")
  tc <- tier_counts_final[order(tier_counts_final$state_count, decreasing = TRUE), , drop = FALSE]
  label_map <- c(
    projection_state_proxy_low = "low",
    projection_state_proxy_intermediate = "intermediate",
    projection_state_proxy_high = "high",
    projection_state_proxy_high_with_risk_context = "high + risk context"
  )
  tier_short <- unname(label_map[tc$projection_evidence_tier])
  tier_short[is.na(tier_short) | tier_short == ""] <- tc$projection_evidence_tier[is.na(tier_short) | tier_short == ""]

  # Use a clean bar panel with compact labels to prevent any text crossing the left device border.
  plot_x0 <- 0.295; plot_x1 <- 0.800; plot_y0 <- 0.215; plot_y1 <- 0.800
  max_count <- max(tc$state_count, na.rm = TRUE)
  if (!is.finite(max_count) || max_count < 1) max_count <- 1
  y_pos <- seq(plot_y1, plot_y0, length.out = nrow(tc))
  bar_h <- min(0.090, (plot_y1 - plot_y0) / max(1, nrow(tc)) * 0.50)
  rect(plot_x0, plot_y0 - 0.020, plot_x1, plot_y1 + 0.020, border = "gray88", col = NA, lwd = 0.45)
  for (i in seq_len(nrow(tc))) {
    yy <- y_pos[i]
    w <- (tc$state_count[i] / max_count) * (plot_x1 - plot_x0)
    rect(plot_x0, yy - bar_h/2, plot_x0 + w, yy + bar_h/2,
         col = palette_tier[i], border = "gray35", lwd = 0.55)
    text(plot_x0 - 0.025, yy, tier_short[i], cex = 0.60, adj = c(1,0.5), col = "gray10")
    text(min(plot_x0 + w + 0.014, 0.905), yy, tc$state_count[i], cex = 0.60, adj = c(0,0.5), col = "gray15")
  }
  axis_max <- ceiling(max_count / 50) * 50
  if (!is.finite(axis_max) || axis_max < max_count) axis_max <- max_count
  tick_values <- pretty(c(0, axis_max), n = 5)
  tick_values <- tick_values[tick_values >= 0 & tick_values <= axis_max]
  for (tick in tick_values) {
    xx <- plot_x0 + (tick / axis_max) * (plot_x1 - plot_x0)
    segments(xx, plot_y0 - 0.040, xx, plot_y0 - 0.030, col = "gray35", lwd = 0.55)
    text(xx, plot_y0 - 0.070, tick, cex = 0.50, col = "gray20")
  }
  segments(plot_x0, plot_y0 - 0.035, plot_x1, plot_y0 - 0.035, col = "gray35", lwd = 0.55)
  text((plot_x0 + plot_x1)/2, 0.075, "De-duplicated state count", cex = 0.68)
  text(0.055, 0.130,
       "Full tier names are retained in 11F_FINAL_projection_evidence_tier_counts_DEDUP.csv",
       cex = 0.43, adj = c(0,0.5), col = "gray35")
}
dev.off()
cat("[11F FINAL] Wrote figure:", file.path(out_fig_dir, figD), "\n")

# ------------------------- FINAL summary/report -------------------------
decision <- ifelse(nrow(evidence_for_11H) > 0,
                   "INPUT_READY_FOR_11F_REVIEW_AND_11H_INTEGRATION_AS_PROJECTION_MODULE_STATE_LEVEL_PROXY_SUPPORT_FULL_RESCAN_DEDUP",
                   "NO_SCORABLE_STATE_ROWS_RETAINED_KEEP_11F_AS_NEGATIVE_AUDIT_ONLY")

summary_df <- data.frame(
  item = c(
    "candidate_rds_detected",
    "rds_attempted",
    "readable_objects",
    "scorable_objects",
    "projection_metadata_candidate_rows",
    "strict_projection_metadata_rows_retained",
    "raw_state_score_rows",
    "deduplicated_state_score_rows_for_11H",
    "deduplicated_evidence_tier_rows_for_11H",
    "figures_written",
    "decision"
  ),
  value = c(
    as.character(length(candidate_rds)),
    as.character(length(selected_rds)),
    as.character(readable_n),
    as.character(scorable_n),
    as.character(nrow(metadata_df)),
    as.character(strict_n),
    as.character(nrow(processed)),
    as.character(nrow(processed_for_plot)),
    as.character(nrow(evidence_for_11H)),
    "4",
    decision
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(out_table_dir, "11F_FINAL_execution_summary.csv"))
write_tsv_safe(summary_df, file.path(out_table_dir, "11F_FINAL_execution_summary.tsv"))

claim_boundary <- data.frame(
  allowed = c(
    "Projection-associated transcriptomic support",
    "Molecular competence / axon-guidance module evidence",
    "A9/A10-like functional identity support",
    "Downstream 11H integration only as conservative evidence"
  ),
  prohibited = c(
    "True anatomical projection validation",
    "Host integration confirmed",
    "Functional projection proven",
    "Retrograde tracing-confirmed graft integration"
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(claim_boundary, file.path(out_table_dir, "11F_FINAL_claim_boundary.csv"))

report_file <- file.path(out_text_dir, "11F_FINAL_full_rescan_projection_module_proxy_report.txt")
writeLines(c(
  "11F FINAL full rescan projection-associated molecular support and visual polish",
  "==========================================================================",
  paste0("Candidate RDS detected: ", length(candidate_rds)),
  paste0("RDS attempted: ", length(selected_rds)),
  paste0("Readable objects: ", readable_n),
  paste0("Scorable objects: ", scorable_n),
  paste0("Projection metadata candidate rows: ", nrow(metadata_df)),
  paste0("Strict projection metadata rows retained: ", strict_n),
  paste0("Raw state score rows: ", nrow(processed)),
  paste0("De-duplicated state rows for 11H: ", nrow(processed_for_plot)),
  paste0("De-duplicated evidence tier rows for 11H: ", nrow(evidence_for_11H)),
  "",
  "Claim boundary:",
  "- Use as projection-associated molecular / transcriptomic proxy support only.",
  "- Do not claim anatomical projection, host integration, functional projection, or retrograde tracing-confirmed graft integration.",
  "",
  "FINAL full-rescan visual fixes:",
  "- Does not read previous 11F V2 tables.",
  "- Re-scans local RDS objects directly.",
  "- De-duplicates repeated object-version state rows before final figures and 11H table.",
  "- Uses side-rail labels in FigC to avoid clipped or overlapping labels.",
  "",
  paste0("Decision: ", decision)
), report_file)
cat("[11F FINAL] Wrote:", report_file, "\n")

cat("\n[11F FINAL] Completed FULL RESCAN projection-associated molecular support + publication visual polish.\n")
cat("[11F FINAL] Candidate RDS detected:", length(candidate_rds), "\n")
cat("[11F FINAL] RDS attempted:", length(selected_rds), "\n")
cat("[11F FINAL] Readable objects:", readable_n, "\n")
cat("[11F FINAL] Scorable objects:", scorable_n, "\n")
cat("[11F FINAL] Projection metadata candidate rows:", nrow(metadata_df), "\n")
cat("[11F FINAL] Strict projection metadata rows retained:", strict_n, "\n")
cat("[11F FINAL] Raw state score rows:", nrow(processed), "\n")
cat("[11F FINAL] Deduplicated state score rows for 11H:", nrow(processed_for_plot), "\n")
cat("[11F FINAL] Deduplicated evidence tier rows for 11H:", nrow(evidence_for_11H), "\n")
cat("[11F FINAL] Figures written: 4\n")
cat("[11F FINAL] Decision:", decision, "\n")
cat("[11F FINAL] Output tables:", out_table_dir, "\n")
cat("[11F FINAL] Output figs  :", out_fig_dir, "\n")
cat("[11F FINAL] Output text  :", out_text_dir, "\n")
cat("[11F FINAL] Next         : review 11F FINAL PDFs; if accepted, lock 11F then proceed to 11G_PD_GWAS_genetic_context_support\n")
