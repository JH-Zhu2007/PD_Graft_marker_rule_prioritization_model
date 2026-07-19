# ============================================================
# 10I_pseudotime_input_readiness_audit_V2_COMPLETE_STANDALONE.R
# Project: PD_Graft_Project / Dopaminergic neuron graft cell-state prioritization
# Purpose:
#   Complete 10I module in ONE script.
#   1) Discover all RDS objects in D:/PD_Graft_Project
#   2) Inspect Seurat object readiness for pseudotime/trajectory analysis
#   3) Rank candidate input objects
#   4) Recommend the best object for 10J pseudotime pilot
#   5) Write audit tables and human-readable notes
#
# Important:
#   - This script DOES NOT run pseudotime.
#   - This script DOES NOT modify 10D/10E/10F outputs.
#   - This script DOES NOT retrain ML models.
#   - This script is complete for 10I; no 10I0/10I1 split.
# ============================================================

options(stringsAsFactors = FALSE)

# -----------------------------
# User settings
# -----------------------------
PROJECT_ROOT <- "D:/PD_Graft_Project"
MODULE_ID <- "10I_pseudotime_input_readiness_audit_V2_COMPLETE_STANDALONE"

OUT_TABLE_DIR <- file.path(PROJECT_ROOT, "03_tables", MODULE_ID)
OUT_TEXT_DIR  <- file.path(PROJECT_ROOT, "09_manuscript", MODULE_ID)

# Safety limit: very large RDS objects can crash Windows/RStudio.
# Increase only if needed.
MAX_RDS_SIZE_MB_TO_READ <- 2500
MAX_RDS_FILES_TO_INSPECT <- Inf

# Candidate accessions from 10H. If 10H output exists, script will read it.
DEFAULT_PSEUDOTIME_SCOPE <- c(
  "GSE132758", "GSE178265", "GSE183248", "GSE200610", "GSE204796", "GSE233885"
)

# -----------------------------
# Helpers
# -----------------------------
safe_dir_create <- function(x) {
  if (!dir.exists(x)) dir.create(x, recursive = TRUE, showWarnings = FALSE)
}

write_csv_safe <- function(df, path) {
  utils::write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
  message("[10I] Wrote: ", path)
}

write_lines_safe <- function(x, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(x, con = con, useBytes = TRUE)
  message("[10I] Wrote: ", path)
}

collapse_or_none <- function(x, sep = "; ") {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return("NONE_DETECTED")
  paste(x, collapse = sep)
}

strict_gse_extract <- function(x) {
  if (length(x) == 0 || is.na(x)) return(character(0))
  m <- gregexpr("GSE[0-9]{5,7}", x, perl = TRUE)
  hits <- regmatches(x, m)[[1]]
  hits <- unique(hits[!is.na(hits) & nzchar(hits)])
  hits
}

safe_read_csv <- function(path) {
  tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
           error = function(e) NULL)
}

safe_slot <- function(obj, slot_name) {
  tryCatch(slot(obj, slot_name), error = function(e) NULL)
}

safe_ncol <- function(obj) {
  tryCatch(ncol(obj), error = function(e) NA_integer_)
}

safe_nrow <- function(obj) {
  tryCatch(nrow(obj), error = function(e) NA_integer_)
}

has_any_pattern <- function(x, pattern) {
  if (length(x) == 0) return(FALSE)
  any(grepl(pattern, x, ignore.case = TRUE))
}

find_cols <- function(x, pattern) {
  if (length(x) == 0) return(character(0))
  x[grepl(pattern, x, ignore.case = TRUE)]
}

score_stage_hint <- function(path) {
  p <- tolower(path)
  score <- 0
  if (grepl("03a|03b|reduction|merge|merged", p)) score <- score + 10
  if (grepl("04a|04b|04c|04d|annotation|annotated", p)) score <- score + 12
  if (grepl("05a|05b|scoring|score", p)) score <- score + 14
  if (grepl("08|09|final|publication|model|priority", p)) score <- score + 8
  if (grepl("qc|raw|download|import", p)) score <- score - 5
  score
}

# -----------------------------
# Initialize
# -----------------------------
safe_dir_create(OUT_TABLE_DIR)
safe_dir_create(OUT_TEXT_DIR)

message("[10I] Starting COMPLETE standalone pseudotime input readiness audit...")
message("[10I] Project root     : ", PROJECT_ROOT)
message("[10I] Output table dir: ", OUT_TABLE_DIR)
message("[10I] Output text dir : ", OUT_TEXT_DIR)
message("[10I] RDS size read cap: ", MAX_RDS_SIZE_MB_TO_READ, " MB")

# -----------------------------
# Optional package loading
# -----------------------------
seurat_available <- requireNamespace("Seurat", quietly = TRUE)
if (seurat_available) {
  suppressPackageStartupMessages(library(Seurat))
  message("[10I] Seurat package available: TRUE")
} else {
  message("[10I] Seurat package available: FALSE. Will still try base/S4 inspection.")
}

# -----------------------------
# Read 10H candidate scope if available
# -----------------------------
path_10h_scope <- file.path(PROJECT_ROOT, "03_tables", "10H_dataset_role_and_model_scope_freeze_V1", "10H_V1_pseudotime_candidate_scope.csv")

pseudotime_scope <- DEFAULT_PSEUDOTIME_SCOPE
scope_source <- "DEFAULT_SCOPE_IN_SCRIPT"

if (file.exists(path_10h_scope)) {
  scope_df <- safe_read_csv(path_10h_scope)
  if (!is.null(scope_df) && nrow(scope_df) > 0) {
    possible_cols <- intersect(c("accession", "GSE", "dataset", "geo_accession"), names(scope_df))
    if (length(possible_cols) > 0) {
      temp <- unique(as.character(scope_df[[possible_cols[1]]]))
    } else {
      temp <- unique(as.character(scope_df[[1]]))
    }
    temp <- unique(unlist(lapply(temp, strict_gse_extract)))
    if (length(temp) > 0) {
      pseudotime_scope <- temp
      scope_source <- path_10h_scope
    }
  }
}

message("[10I] Pseudotime candidate scope source: ", scope_source)
message("[10I] Pseudotime candidate accessions: ", paste(pseudotime_scope, collapse = ", "))

# -----------------------------
# Discover RDS files
# -----------------------------
if (!dir.exists(PROJECT_ROOT)) stop("Project root not found: ", PROJECT_ROOT)

rds_files <- list.files(
  PROJECT_ROOT,
  pattern = "\\.[Rr][Dd][Ss]$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# Avoid inspecting this module's output dirs if any RDS appears there.
rds_files <- rds_files[!grepl("10I_pseudotime_input_readiness_audit", rds_files, ignore.case = TRUE)]

if (length(rds_files) == 0) {
  stop("No RDS files found under project root: ", PROJECT_ROOT)
}

fi <- file.info(rds_files)
inventory <- data.frame(
  rds_path = normalizePath(rds_files, winslash = "/", mustWork = FALSE),
  file_name = basename(rds_files),
  parent_dir = basename(dirname(rds_files)),
  size_mb = round(fi$size / 1024^2, 3),
  modified_time = as.character(fi$mtime),
  accession_from_path = vapply(rds_files, function(z) collapse_or_none(strict_gse_extract(z), sep = ";"), character(1)),
  stage_hint_score = vapply(rds_files, score_stage_hint, numeric(1)),
  stringsAsFactors = FALSE
)

# Prioritize likely useful files first, but keep full inventory.
inventory <- inventory[order(-inventory$stage_hint_score, inventory$size_mb), , drop = FALSE]
write_csv_safe(inventory, file.path(OUT_TABLE_DIR, "10I_V2_all_rds_file_inventory.csv"))

if (is.finite(MAX_RDS_FILES_TO_INSPECT) && nrow(inventory) > MAX_RDS_FILES_TO_INSPECT) {
  inspect_inventory <- inventory[seq_len(MAX_RDS_FILES_TO_INSPECT), , drop = FALSE]
} else {
  inspect_inventory <- inventory
}

message("[10I] RDS files found: ", nrow(inventory))
message("[10I] RDS files selected for inspection: ", nrow(inspect_inventory))

# -----------------------------
# Inspect RDS files
# -----------------------------
inspection_rows <- list()

for (i in seq_len(nrow(inspect_inventory))) {
  path <- inspect_inventory$rds_path[i]
  size_mb <- inspect_inventory$size_mb[i]
  message("[10I] Inspecting RDS ", i, "/", nrow(inspect_inventory), ": ", path, " (", size_mb, " MB)")

  base_row <- list(
    rds_path = path,
    file_name = inspect_inventory$file_name[i],
    parent_dir = inspect_inventory$parent_dir[i],
    size_mb = size_mb,
    modified_time = inspect_inventory$modified_time[i],
    accession_from_path = inspect_inventory$accession_from_path[i],
    stage_hint_score = inspect_inventory$stage_hint_score[i],
    read_status = "NOT_READ",
    read_error = "",
    object_class = "UNKNOWN",
    is_seurat = FALSE,
    n_cells = NA_integer_,
    n_features = NA_integer_,
    assay_names = "UNKNOWN",
    default_assay = "UNKNOWN",
    reduction_names = "UNKNOWN",
    has_pca = FALSE,
    has_umap = FALSE,
    metadata_ncol = NA_integer_,
    metadata_columns_preview = "UNKNOWN",
    cluster_columns = "NONE_DETECTED",
    annotation_columns = "NONE_DETECTED",
    dataset_columns = "NONE_DETECTED",
    score_columns = "NONE_DETECTED",
    has_cluster_metadata = FALSE,
    has_annotation_metadata = FALSE,
    has_dataset_metadata = FALSE,
    has_score_metadata = FALSE,
    has_candidate_accession = FALSE,
    candidate_accession_overlap = "NONE_DETECTED",
    pseudotime_readiness_score = NA_real_,
    pseudotime_readiness_tier = "NOT_SCORED",
    recommendation_reason = ""
  )

  if (is.na(size_mb) || size_mb > MAX_RDS_SIZE_MB_TO_READ) {
    base_row$read_status <- "SKIPPED_TOO_LARGE"
    base_row$read_error <- paste0("size_mb > ", MAX_RDS_SIZE_MB_TO_READ)
    inspection_rows[[length(inspection_rows) + 1]] <- as.data.frame(base_row, stringsAsFactors = FALSE)
    next
  }

  obj <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(obj, "error")) {
    base_row$read_status <- "READ_FAILED"
    base_row$read_error <- conditionMessage(obj)
    inspection_rows[[length(inspection_rows) + 1]] <- as.data.frame(base_row, stringsAsFactors = FALSE)
    next
  }

  base_row$read_status <- "READ_OK"
  base_row$object_class <- paste(class(obj), collapse = ";")
  base_row$is_seurat <- inherits(obj, "Seurat")

  if (base_row$is_seurat) {
    base_row$n_cells <- safe_ncol(obj)
    base_row$n_features <- safe_nrow(obj)

    assays <- safe_slot(obj, "assays")
    reds <- safe_slot(obj, "reductions")
    meta <- safe_slot(obj, "meta.data")

    if (!is.null(assays)) base_row$assay_names <- collapse_or_none(names(assays))
    active_assay <- tryCatch(as.character(slot(obj, "active.assay")), error = function(e) "UNKNOWN")
    base_row$default_assay <- ifelse(length(active_assay) > 0 && nzchar(active_assay[1]), active_assay[1], "UNKNOWN")

    red_names <- if (!is.null(reds)) names(reds) else character(0)
    base_row$reduction_names <- collapse_or_none(red_names)
    base_row$has_pca <- any(tolower(red_names) %in% c("pca", "rpca", "lsi"))
    base_row$has_umap <- any(grepl("umap", red_names, ignore.case = TRUE))

    meta_cols <- if (!is.null(meta) && is.data.frame(meta)) names(meta) else character(0)
    base_row$metadata_ncol <- length(meta_cols)
    base_row$metadata_columns_preview <- collapse_or_none(head(meta_cols, 80))

    cluster_cols <- find_cols(meta_cols, "seurat_clusters|(^cluster$)|cluster_|_cluster|clusters|res\\.|louvain|leiden")
    annotation_cols <- find_cols(meta_cols, "annotation|celltype|cell_type|cell.type|cell_state|state|identity|ident|subtype|class")
    dataset_cols <- find_cols(meta_cols, "orig.ident|dataset|sample|batch|donor|patient|condition|group|GSE|geo")
    score_cols <- find_cols(meta_cols, "score|priority|prob|probability|ideal|risk|safety|projection|A9|A10|dopaminergic|DA")

    base_row$cluster_columns <- collapse_or_none(cluster_cols)
    base_row$annotation_columns <- collapse_or_none(annotation_cols)
    base_row$dataset_columns <- collapse_or_none(dataset_cols)
    base_row$score_columns <- collapse_or_none(score_cols)

    base_row$has_cluster_metadata <- length(cluster_cols) > 0
    base_row$has_annotation_metadata <- length(annotation_cols) > 0
    base_row$has_dataset_metadata <- length(dataset_cols) > 0
    base_row$has_score_metadata <- length(score_cols) > 0

    all_text <- paste(path, paste(meta_cols, collapse = " "), collapse = " ")
    accessions_here <- unique(c(strict_gse_extract(path), strict_gse_extract(all_text)))
    overlap <- intersect(accessions_here, pseudotime_scope)
    base_row$has_candidate_accession <- length(overlap) > 0
    base_row$candidate_accession_overlap <- collapse_or_none(overlap)

    # Readiness scoring
    score <- 0
    reasons <- character(0)

    score <- score + 50
    reasons <- c(reasons, "Seurat object")

    if (!is.na(base_row$n_cells)) {
      if (base_row$n_cells >= 500 && base_row$n_cells <= 120000) {
        score <- score + 20
        reasons <- c(reasons, "cell count feasible")
      } else if (base_row$n_cells > 120000 && base_row$n_cells <= 250000) {
        score <- score + 8
        reasons <- c(reasons, "large but potentially subsettable")
      } else if (base_row$n_cells > 250000) {
        score <- score - 10
        reasons <- c(reasons, "very large; subset likely needed")
      } else {
        score <- score - 10
        reasons <- c(reasons, "too few cells")
      }
    }

    if (base_row$has_pca) {
      score <- score + 15
      reasons <- c(reasons, "has PCA/latent reduction")
    } else {
      score <- score - 10
      reasons <- c(reasons, "no PCA-like reduction detected")
    }

    if (base_row$has_umap) {
      score <- score + 15
      reasons <- c(reasons, "has UMAP")
    } else {
      reasons <- c(reasons, "no UMAP detected")
    }

    if (base_row$has_cluster_metadata) {
      score <- score + 15
      reasons <- c(reasons, "has cluster metadata")
    } else {
      score <- score - 12
      reasons <- c(reasons, "no cluster metadata detected")
    }

    if (base_row$has_annotation_metadata) {
      score <- score + 10
      reasons <- c(reasons, "has annotation/state metadata")
    }

    if (base_row$has_dataset_metadata) {
      score <- score + 8
      reasons <- c(reasons, "has dataset/sample metadata")
    }

    if (base_row$has_score_metadata) {
      score <- score + 12
      reasons <- c(reasons, "has score/priority metadata")
    } else {
      reasons <- c(reasons, "score metadata may need joining from 08/09 tables")
    }

    if (base_row$has_candidate_accession) {
      score <- score + 12
      reasons <- c(reasons, "matches 10H pseudotime candidate scope")
    }

    score <- score + base_row$stage_hint_score

    base_row$pseudotime_readiness_score <- score
    base_row$pseudotime_readiness_tier <- if (score >= 120) {
      "A_READY_FOR_10J_PILOT"
    } else if (score >= 95) {
      "B_GOOD_CANDIDATE_CHECK_METADATA"
    } else if (score >= 70) {
      "C_POSSIBLE_BUT_NEEDS_PREP"
    } else {
      "D_NOT_RECOMMENDED_AS_PRIMARY_INPUT"
    }
    base_row$recommendation_reason <- collapse_or_none(reasons)
  } else {
    base_row$pseudotime_readiness_score <- -100
    base_row$pseudotime_readiness_tier <- "D_NOT_SEURAT"
    base_row$recommendation_reason <- "Object is not a Seurat object. Not recommended for pseudotime input."
  }

  inspection_rows[[length(inspection_rows) + 1]] <- as.data.frame(base_row, stringsAsFactors = FALSE)
  rm(obj)
  invisible(gc(verbose = FALSE))
}

inspection <- do.call(rbind, inspection_rows)

# Fix types after rbind
num_cols <- c("size_mb", "stage_hint_score", "n_cells", "n_features", "metadata_ncol", "pseudotime_readiness_score")
for (cc in intersect(num_cols, names(inspection))) inspection[[cc]] <- suppressWarnings(as.numeric(inspection[[cc]]))
logical_cols <- c("is_seurat", "has_pca", "has_umap", "has_cluster_metadata", "has_annotation_metadata", "has_dataset_metadata", "has_score_metadata", "has_candidate_accession")
for (cc in intersect(logical_cols, names(inspection))) inspection[[cc]] <- as.logical(inspection[[cc]])

inspection_ranked <- inspection[order(-inspection$pseudotime_readiness_score, inspection$size_mb), , drop = FALSE]
write_csv_safe(inspection_ranked, file.path(OUT_TABLE_DIR, "10I_V2_seurat_object_inspection_and_candidate_rank.csv"))

candidate_rank <- inspection_ranked[inspection_ranked$is_seurat %in% TRUE & inspection_ranked$pseudotime_readiness_score > 0, , drop = FALSE]
write_csv_safe(candidate_rank, file.path(OUT_TABLE_DIR, "10I_V2_pseudotime_candidate_rank_table.csv"))

# Required fields audit summary
required_audit <- data.frame(
  requirement = c(
    "Seurat object readable",
    "Feasible cell count",
    "PCA or latent reduction available",
    "UMAP available",
    "Cluster metadata available",
    "Annotation/cell-state metadata available",
    "Dataset/sample metadata available",
    "Score/priority metadata available",
    "Matches 10H pseudotime candidate scope"
  ),
  why_it_matters = c(
    "Trajectory workflow needs an expression object.",
    "Very large objects may require downsampling/subsetting for stable Windows execution.",
    "Pseudotime methods usually require a reduced representation or one can be recomputed.",
    "UMAP supports visualizing trajectories and states.",
    "Cluster-level state comparison requires cluster labels.",
    "Biological interpretation requires cell-state labels.",
    "Cross-dataset interpretation needs sample/dataset provenance.",
    "Priority-index and risk-score trends along pseudotime require score metadata or joinable score tables.",
    "10H restricts pseudotime to dopaminergic neuron/graft-relevant candidate scope."
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(required_audit, file.path(OUT_TABLE_DIR, "10I_V2_pseudotime_required_fields_audit.csv"))

# Recommendation
if (nrow(candidate_rank) > 0) {
  rec <- candidate_rank[1, , drop = FALSE]
  rec_lines <- c(
    "10I V2 recommended pseudotime input object",
    "==========================================",
    paste0("Recommended RDS path: ", rec$rds_path),
    paste0("Readiness score: ", rec$pseudotime_readiness_score),
    paste0("Readiness tier: ", rec$pseudotime_readiness_tier),
    paste0("Cells: ", rec$n_cells),
    paste0("Features: ", rec$n_features),
    paste0("Assays: ", rec$assay_names),
    paste0("Reductions: ", rec$reduction_names),
    paste0("Cluster columns: ", rec$cluster_columns),
    paste0("Annotation columns: ", rec$annotation_columns),
    paste0("Dataset/sample columns: ", rec$dataset_columns),
    paste0("Score/priority columns: ", rec$score_columns),
    paste0("Candidate GSE overlap: ", rec$candidate_accession_overlap),
    "",
    "Interpretation:",
    "This object is recommended only as the input candidate for 10J pseudotime pilot.",
    "10I does not prove that pseudotime is biologically meaningful; it only checks technical readiness.",
    "The next module should run a pilot trajectory and decide whether the pseudotime result is suitable for the advanced V2 figure plan.",
    "",
    "Claim boundary:",
    "Allowed: transcriptomic pseudotime, maturation-associated trajectory pattern, dopaminergic state ordering.",
    "Not allowed: true lineage tracing, clone-aware fate tracking, functional graft integration, clinical prediction."
  )
} else {
  rec_lines <- c(
    "10I V2 recommended pseudotime input object",
    "==========================================",
    "No suitable Seurat object candidate was identified.",
    "Check whether RDS files exist and whether Seurat objects are readable in this R environment.",
    "If objects are too large, create a smaller annotated/scored Seurat object and rerun 10I."
  )
}

write_lines_safe(rec_lines, file.path(OUT_TEXT_DIR, "10I_V2_recommended_input_object.txt"))

# Human-readable summary
summary_df <- data.frame(
  metric = c(
    "rds_files_found",
    "rds_files_inspected",
    "read_ok",
    "read_failed",
    "skipped_too_large",
    "seurat_objects_readable",
    "candidate_objects_ranked",
    "top_candidate_path",
    "top_candidate_score",
    "top_candidate_tier",
    "scope_source"
  ),
  value = c(
    nrow(inventory),
    nrow(inspect_inventory),
    sum(inspection$read_status == "READ_OK", na.rm = TRUE),
    sum(inspection$read_status == "READ_FAILED", na.rm = TRUE),
    sum(inspection$read_status == "SKIPPED_TOO_LARGE", na.rm = TRUE),
    sum(inspection$is_seurat %in% TRUE, na.rm = TRUE),
    nrow(candidate_rank),
    if (nrow(candidate_rank) > 0) candidate_rank$rds_path[1] else "NONE",
    if (nrow(candidate_rank) > 0) as.character(candidate_rank$pseudotime_readiness_score[1]) else "NONE",
    if (nrow(candidate_rank) > 0) candidate_rank$pseudotime_readiness_tier[1] else "NONE",
    scope_source
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(OUT_TABLE_DIR, "10I_V2_execution_summary.csv"))

report_lines <- c(
  "10I V2 complete standalone pseudotime input readiness audit",
  "============================================================",
  paste0("Run time: ", as.character(Sys.time())),
  paste0("Project root: ", PROJECT_ROOT),
  paste0("Output table dir: ", OUT_TABLE_DIR),
  paste0("Output text dir: ", OUT_TEXT_DIR),
  "",
  "Scope:",
  paste0("Pseudotime candidate scope source: ", scope_source),
  paste0("Pseudotime candidate accessions: ", paste(pseudotime_scope, collapse = ", ")),
  "",
  "Summary:",
  paste0("RDS files found: ", nrow(inventory)),
  paste0("RDS files inspected: ", nrow(inspect_inventory)),
  paste0("Readable objects: ", sum(inspection$read_status == "READ_OK", na.rm = TRUE)),
  paste0("Readable Seurat objects: ", sum(inspection$is_seurat %in% TRUE, na.rm = TRUE)),
  paste0("Candidate objects ranked: ", nrow(candidate_rank)),
  "",
  "Main outputs:",
  "10I_V2_all_rds_file_inventory.csv",
  "10I_V2_seurat_object_inspection_and_candidate_rank.csv",
  "10I_V2_pseudotime_candidate_rank_table.csv",
  "10I_V2_pseudotime_required_fields_audit.csv",
  "10I_V2_recommended_input_object.txt",
  "",
  "Next module:",
  "10J_pseudotime_pilot",
  "",
  "Boundary:",
  "10I only identifies the technically best candidate input object. It does not run pseudotime and does not add any new biological claim."
)
write_lines_safe(report_lines, file.path(OUT_TEXT_DIR, "10I_V2_execution_report.txt"))

message("")
message("[10I] Completed COMPLETE standalone pseudotime input readiness audit.")
message("[10I] RDS files found: ", nrow(inventory))
message("[10I] RDS files inspected: ", nrow(inspect_inventory))
message("[10I] Readable Seurat objects: ", sum(inspection$is_seurat %in% TRUE, na.rm = TRUE))
message("[10I] Candidate objects ranked: ", nrow(candidate_rank))
if (nrow(candidate_rank) > 0) {
  message("[10I] Recommended input: ", candidate_rank$rds_path[1])
  message("[10I] Recommended score: ", candidate_rank$pseudotime_readiness_score[1])
  message("[10I] Recommended tier : ", candidate_rank$pseudotime_readiness_tier[1])
}
message("[10I] Output tables: ", OUT_TABLE_DIR)
message("[10I] Output text  : ", OUT_TEXT_DIR)
message("[10I] Next         : 10J_pseudotime_pilot")
