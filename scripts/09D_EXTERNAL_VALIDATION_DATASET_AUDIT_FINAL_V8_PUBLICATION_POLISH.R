
PROJECT_DIR <- "D:/PD_Graft_Project"

SEED <- 20260714

DISCOVERY_GSE_USED <- c(
  "GSE178265",
  "GSE132758",
  "GSE200610",
  "GSE204795",
  "GSE204796",
  "GSE233885",
  "GSE157783"
)

CANDIDATE_GSE_TABLE <- data.frame(
  gse_id = c(
    "GSE183248",
    "GSE184950",
    "GSE243639",
    "GSE128040",
    "GSE148434"
  ),
  candidate_source = c(
    "seed_candidate_PD_iPSC_DA_differentiation_scRNA",
    "seed_candidate_human_substantia_nigra_PD_single_cell",
    "seed_candidate_large_human_SNpc_PD_snRNA",
    "seed_candidate_LRRK2_PD_neural_stem_cell_single_cell",
    "seed_candidate_PD_single_cell_transcriptome_epigenome"
  ),
  intended_validation_role = c(
    "primary_frozen_scoring_validation_candidate_if_processed_matrix_available",
    "human_disease_context_validation_candidate",
    "human_disease_context_validation_candidate",
    "mechanistic_iPSC_PD_context_candidate",
    "multiomic_PD_cell_state_context_candidate"
  ),
  manual_reason_before_GEO_audit = c(
    "External human iPSC differentiation / dopaminergic / Parkinson-related scRNA candidate; not used in 00-09C.",
    "External human substantia nigra Parkinson single-cell/nucleus candidate; not used in 00-09C.",
    "External large-scale human SNpc Parkinson snRNA candidate; not used in 00-09C.",
    "External LRRK2 / PD neural stem cell candidate; not used in 00-09C.",
    "External PD single-cell transcriptome/epigenome candidate; not used in 00-09C."
  ),
  stringsAsFactors = FALSE
)

CURATED_EXTERNAL_ROLE_TABLE <- data.frame(
  gse_id = c(
    "GSE183248",
    "GSE184950",
    "GSE243639",
    "GSE128040",
    "GSE148434"
  ),
  curated_role = c(
    "tier1_primary_frozen_validation_candidate",
    "tier1_context_validation_candidate",
    "tier1_context_validation_candidate",
    "tier2_backup_or_mechanistic_context_candidate",
    "tier2_backup_or_mechanistic_context_candidate"
  ),
  curated_recommended_role = c(
    "primary_frozen_scoring_validation_candidate_for_09E",
    "disease_context_external_validation_candidate_for_09E",
    "disease_context_external_validation_candidate_for_09E",
    "backup_or_mechanistic_context_candidate_not_primary",
    "backup_or_mechanistic_context_candidate_not_primary"
  ),
  curated_priority_rank = c(1, 2, 3, 4, 5),
  curated_reason = c(
    "Closest candidate for primary external validation because it is an iPSC differentiation / dopaminergic-related single-cell candidate independent of 00-09C.",
    "Human substantia nigra / PD single-cell context; useful for disease-context validation, not direct graft-like validation.",
    "Human PD cell-type-specific response / SNpc-like disease context; useful for context validation, not direct graft-like validation.",
    "Neural stem-cell / LRRK2/PD mechanistic context; backup only, not a primary graft-like/DA differentiation validation dataset.",
    "Broad PD single-cell/multiomic context; backup/mechanistic context unless manual matrix and biology review supports stronger use."
  ),
  stringsAsFactors = FALSE
)

EXTRA_CANDIDATE_GSE <- character(0)

MIN_RECOMMENDED_ELIGIBILITY_SCORE <- 8
MIN_SAMPLE_COUNT_RECOMMENDED <- 2

PDF_WIDTH <- 11.5
PDF_HEIGHT <- 7.5

cat("\n============================================================\n")
cat("09D：external validation dataset eligibility audit V8\n")
cat("============================================================\n\n")

options(stringsAsFactors = FALSE)
options(timeout = 60000)

required_pkgs <- c("data.table")

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop("缺少 R 包，请先手动安装：", paste(missing_pkgs, collapse = ", "))
}

suppressPackageStartupMessages({
  library(data.table)
})

HAS_GEOQUERY <- requireNamespace("GEOquery", quietly = TRUE)

if (HAS_GEOQUERY) {
  suppressPackageStartupMessages({
    library(GEOquery)
  })
  message("检测到 GEOquery：09D V2 将优先使用 GEOquery 获取 GEO metadata。")
} else {
  message("未检测到 GEOquery：09D V2 将使用 NCBI GEO SOFT fallback，不需要安装 GEOquery。")
}

set.seed(SEED)

tables_dir <- file.path(PROJECT_DIR, "03_tables")
figures_dir <- file.path(PROJECT_DIR, "04_figures")
reports_dir <- file.path(PROJECT_DIR, "06_reports")

out_tables_dir <- file.path(
  tables_dir,
  "09D_external_validation_dataset_audit_V8_PUBLICATION_POLISH"
)

out_figures_dir <- file.path(
  figures_dir,
  "09D_external_validation_dataset_audit_V8_PUBLICATION_POLISH_pdf"
)

soft_cache_dir <- file.path(PROJECT_DIR, "05_temp", "09D_GEO_SOFT_cache_V2")

dir.create(out_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(soft_cache_dir, recursive = TRUE, showWarnings = FALSE)

candidate_seed_csv <- file.path(out_tables_dir, "09D_candidate_seed_table.csv")
curated_role_table_csv <- file.path(out_tables_dir, "09D_curated_external_role_table.csv")
geo_metadata_csv <- file.path(out_tables_dir, "09D_online_GEO_metadata_audit.csv")
gsm_summary_csv <- file.path(out_tables_dir, "09D_GSM_sample_metadata_summary.csv")
eligibility_csv <- file.path(out_tables_dir, "09D_external_dataset_eligibility_audit.csv")
frozen_manifest_csv <- file.path(out_tables_dir, "09D_frozen_framework_manifest.csv")
decision_txt <- file.path(out_tables_dir, "09D_external_validation_dataset_decision_report.txt")
method_note_txt <- file.path(out_tables_dir, "09D_method_and_claim_boundary_note.txt")
next_step_plan_txt <- file.path(out_tables_dir, "09D_to_09E_frozen_external_validation_plan.txt")
session_info_txt <- file.path(out_tables_dir, "09D_sessionInfo.txt")
output_check_csv <- file.path(out_tables_dir, "09D_output_verification.csv")
report_txt <- file.path(reports_dir, "09D_external_validation_dataset_audit_V8_PUBLICATION_POLISH_report.txt")

fig_score_pdf <- file.path(out_figures_dir, "09D_external_candidate_eligibility_score_barplot.pdf")
fig_role_pdf <- file.path(out_figures_dir, "09D_external_candidate_validation_role_summary.pdf")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(empty = character())
  }

  if (file.exists(path)) unlink(path, force = TRUE)

  data.table::fwrite(df, path)

  if (!file.exists(path)) stop("CSV 未生成：", path)

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes <= 0) {
    stop("CSV 已创建但为空或无效：", path)
  }

  invisible(path)
}

safe_pdf <- function(path, width = PDF_WIDTH, height = PDF_HEIGHT) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(path)) {
    removed <- file.remove(path)
    if (!isTRUE(removed)) {
      stop(
        "旧 PDF 正在被占用，无法覆盖：", path,
        "\n请关闭 Edge/Adobe/RStudio Viewer/文件资源管理器预览窗口后重跑。"
      )
    }
  }

  while (grDevices::dev.cur() > 1) {
    try(grDevices::dev.off(), silent = TRUE)
    if (grDevices::dev.cur() <= 1) break
  }

  grDevices::pdf(path, width = width, height = height, useDingbats = FALSE, onefile = TRUE)
}

finish_pdf <- function(path) {
  try(grDevices::dev.off(), silent = TRUE)

  if (!file.exists(path)) stop("PDF 未生成：", path)

  size_bytes <- file.info(path)$size
  if (!is.finite(size_bytes) || size_bytes < 1000) {
    stop("PDF 已创建但文件过小或无效：", path, "；size = ", size_bytes)
  }

  message("已保存 PDF：", normalizePath(path, winslash = "/", mustWork = TRUE),
          " | size = ", round(size_bytes / 1024, 1), " KB")
}

safe_meta <- function(meta_list, key) {
  x <- meta_list[[key]]
  if (is.null(x) || length(x) == 0) return(NA_character_)
  paste(unique(as.character(x)), collapse = " ; ")
}

collapse_text <- function(...) {
  x <- c(...)
  x <- x[!is.na(x)]
  x <- paste(x, collapse = " ; ")
  x <- gsub("\\s+", " ", x)
  tolower(x)
}

yesno <- function(x) ifelse(isTRUE(x), "yes", "no")

has_any_pattern <- function(text, patterns) {

  text <- as.character(text)
  text <- text[!is.na(text)]
  if (length(text) == 0) return(FALSE)
  text <- paste(text, collapse = " ; ")
  text <- gsub("\\s+", " ", text)

  any(vapply(
    patterns,
    function(p) {
      isTRUE(grepl(p, text, ignore.case = TRUE, perl = TRUE))
    },
    logical(1)
  ))
}

extract_first_nonempty <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else x[1]
}

md5_or_missing <- function(path) {
  if (file.exists(path)) {
    as.character(tools::md5sum(path))
  } else {
    NA_character_
  }
}

short_label <- function(x, n = 45) {
  x <- as.character(x)
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 3), "..."), x)
}

tier_short_label_09D <- function(x) {
  x <- as.character(x)
  out <- ifelse(x == "tier1_primary_frozen_validation_candidate", "Primary",
                ifelse(x == "tier1_context_validation_candidate", "Context",
                       ifelse(x == "tier2_backup_or_mechanistic_context_candidate", "Backup",
                              ifelse(x == "tier3_manual_review_only", "Manual",
                                     ifelse(x == "exclude", "Exclude", x)))))
  out
}

role_short_label_09D <- function(tier, role) {
  tier <- as.character(tier)
  role <- as.character(role)

  out <- ifelse(
    tier == "tier1_primary_frozen_validation_candidate",
    "Primary frozen validation",
    ifelse(
      tier == "tier1_context_validation_candidate",
      "Disease-context validation",
      ifelse(
        tier == "tier2_backup_or_mechanistic_context_candidate",
        "Backup/mechanistic context",
        ifelse(
          tier == "tier3_manual_review_only",
          "Manual review only",
          ifelse(tier == "exclude", "Excluded", role)
        )
      )
    )
  )

  out
}

gse_series_dir <- function(gse_id) {
  sub("[0-9]{3}$", "nnn", toupper(gse_id), perl = TRUE)
}

gse_soft_url <- function(gse_id) {
  gse_id <- toupper(gse_id)
  paste0(
    "https://ftp.ncbi.nlm.nih.gov/geo/series/",
    gse_series_dir(gse_id),
    "/",
    gse_id,
    "/soft/",
    gse_id,
    "_family.soft.gz"
  )
}

download_gse_soft <- function(gse_id, cache_dir) {
  gse_id <- toupper(gse_id)
  url <- gse_soft_url(gse_id)
  out_gz <- file.path(cache_dir, paste0(gse_id, "_family.soft.gz"))

  if (!file.exists(out_gz) || file.info(out_gz)$size <= 0) {
    status <- tryCatch({
      utils::download.file(
        url = url,
        destfile = out_gz,
        mode = "wb",
        quiet = TRUE
      )
      "success"
    }, error = function(e) {
      paste0("failed: ", conditionMessage(e))
    }, warning = function(w) {
      paste0("warning: ", conditionMessage(w))
    })

    if (!identical(status, "success")) {
      if (file.exists(out_gz) && file.info(out_gz)$size <= 0) {
        unlink(out_gz, force = TRUE)
      }
      stop(status)
    }
  }

  out_gz
}

parse_soft_field <- function(lines, prefix, collapse = TRUE) {
  hits <- grep(paste0("^", prefix, "\\s*=\\s*"), lines, value = TRUE)
  vals <- sub(paste0("^", prefix, "\\s*=\\s*"), "", hits)
  vals <- vals[!is.na(vals) & vals != ""]
  if (length(vals) == 0) return(NA_character_)
  vals <- unique(vals)
  if (collapse) paste(vals, collapse = " ; ") else vals
}

extract_sample_blocks_from_soft <- function(lines) {
  starts <- grep("^\\^SAMPLE\\s*=\\s*GSM", lines)
  if (length(starts) == 0) return(list())

  ends <- c(starts[-1] - 1, length(lines))
  blocks <- vector("list", length(starts))

  for (i in seq_along(starts)) {
    sample_id <- sub("^\\^SAMPLE\\s*=\\s*", "", lines[starts[i]])
    blocks[[i]] <- list(
      gsm_id = sample_id,
      lines = lines[starts[i]:ends[i]]
    )
  }

  blocks
}

parse_gse_soft_fallback <- function(gse_id, cache_dir) {
  gse_id <- toupper(gse_id)
  soft_gz <- download_gse_soft(gse_id, cache_dir)

  read_soft_lines_once <- function(path) {
    warned <- NULL

    lines <- tryCatch({
      con <- gzfile(path, open = "rt")
      on.exit(try(close(con), silent = TRUE), add = TRUE)

      withCallingHandlers(
        readLines(con, warn = TRUE),
        warning = function(w) {
          warned <<- conditionMessage(w)
          invokeRestart("muffleWarning")
        }
      )
    }, error = function(e) {
      structure(list(error = conditionMessage(e)), class = "soft_read_error")
    })

    if (inherits(lines, "soft_read_error")) return(lines)

    if (!is.null(warned) && grepl("invalid|incomplete|compressed", warned, ignore.case = TRUE)) {
      return(structure(list(error = warned), class = "soft_read_error"))
    }

    lines
  }

  lines <- read_soft_lines_once(soft_gz)

  if (inherits(lines, "soft_read_error")) {
    unlink(soft_gz, force = TRUE)
    soft_gz <- download_gse_soft(gse_id, cache_dir)
    lines <- read_soft_lines_once(soft_gz)
  }

  if (inherits(lines, "soft_read_error")) {
    stop("read SOFT failed: ", lines$error)
  }

  series_lines <- lines[seq_len(min(length(lines), max(grep("^\\^SAMPLE\\s*=", lines)[1] - 1, 1), na.rm = TRUE))]
  if (length(series_lines) == 0 || any(is.na(series_lines))) {
    series_lines <- lines[!grepl("^\\^SAMPLE\\s*=", lines)]
  }

  sample_blocks <- extract_sample_blocks_from_soft(lines)

  sample_organisms <- character()
  if (length(sample_blocks) > 0) {
    sample_organisms <- unique(vapply(sample_blocks, function(b) {
      parse_soft_field(b$lines, "!Sample_organism_ch1")
    }, character(1)))
    sample_organisms <- sample_organisms[!is.na(sample_organisms) & sample_organisms != ""]
  }

  series_dt <- data.table(
    gse_id = gse_id,
    geo_query_status = "success_soft_fallback",
    geo_error_message = NA_character_,
    title = parse_soft_field(series_lines, "!Series_title"),
    organism = if (length(sample_organisms) > 0) paste(sample_organisms, collapse = " ; ") else NA_character_,
    experiment_type = parse_soft_field(series_lines, "!Series_type"),
    summary = parse_soft_field(series_lines, "!Series_summary"),
    overall_design = parse_soft_field(series_lines, "!Series_overall_design"),
    sample_count = length(sample_blocks),
    supplementary_file = parse_soft_field(series_lines, "!Series_supplementary_file"),
    pubmed_id = parse_soft_field(series_lines, "!Series_pubmed_id"),
    status = parse_soft_field(series_lines, "!Series_status"),
    last_update_date = parse_soft_field(series_lines, "!Series_last_update_date"),
    metadata_source = "NCBI_GEO_SOFT_fallback",
    soft_url = gse_soft_url(gse_id),
    soft_cache_file = soft_gz
  )

  gsm_dt_list <- list()

  if (length(sample_blocks) > 0) {
    max_gsm_to_record <- min(length(sample_blocks), 80)

    for (i in seq_len(max_gsm_to_record)) {
      b <- sample_blocks[[i]]
      bl <- b$lines

      gsm_text <- collapse_text(
        parse_soft_field(bl, "!Sample_title"),
        parse_soft_field(bl, "!Sample_source_name_ch1"),
        parse_soft_field(bl, "!Sample_characteristics_ch1"),
        parse_soft_field(bl, "!Sample_description"),
        parse_soft_field(bl, "!Sample_relation"),
        parse_soft_field(bl, "!Sample_supplementary_file")
      )

      gsm_dt_list[[length(gsm_dt_list) + 1L]] <- data.table(
        gse_id = gse_id,
        gsm_id = b$gsm_id,
        sample_title = parse_soft_field(bl, "!Sample_title"),
        source_name = parse_soft_field(bl, "!Sample_source_name_ch1"),
        characteristics = parse_soft_field(bl, "!Sample_characteristics_ch1"),
        relation = parse_soft_field(bl, "!Sample_relation"),
        sample_text_has_sra = has_any_pattern(gsm_text, c("sra", "srx", "srr")),
        sample_text_has_10x = has_any_pattern(gsm_text, c("10x", "chromium")),
        sample_text_has_single_cell = has_any_pattern(gsm_text, c("single.cell", "single cell", "scrna", "snrna", "single nuclei", "single nucleus")),
        sample_text_has_dopamine = has_any_pattern(gsm_text, c("dopamin", "\\bda\\b", "midbrain", "substantia nigra", "snpc")),
        sample_text_has_pd = has_any_pattern(gsm_text, c("parkinson", "\\bpd\\b", "lrrk2", "pink1", "snca")),
        metadata_source = "NCBI_GEO_SOFT_fallback"
      )
    }
  }

  gsm_dt <- rbindlist(gsm_dt_list, fill = TRUE)

  list(series = series_dt, gsm = gsm_dt)
}

stamp("记录 frozen 00–09C framework manifest。")

frozen_files <- data.table(
  framework_component = c(
    "09B_primary_reduced_ideal_training_table",
    "09B_primary_reduced_safety_training_table",
    "09B_feature_leakage_dictionary",
    "09C_final_V4_performance_summary",
    "09C_final_V4_feature_preflight_audit",
    "09C_final_V4_method_boundary_note"
  ),
  path = c(
    file.path(tables_dir, "09B_ML_ready_dataset_and_leakage_audit_V3", "09B_ideal_like_training_reduced_non_direct_features.csv"),
    file.path(tables_dir, "09B_ML_ready_dataset_and_leakage_audit_V3", "09B_safety_risk_training_reduced_non_direct_features.csv"),
    file.path(tables_dir, "09B_ML_ready_dataset_and_leakage_audit_V3", "09B_feature_dictionary_with_leakage_risk.csv"),
    file.path(tables_dir, "09C_primary_reduced_feature_weak_label_ML_V4_FULL_PUBLICATION_LAYOUT", "09C_model_performance_summary.csv"),
    file.path(tables_dir, "09C_primary_reduced_feature_weak_label_ML_V4_FULL_PUBLICATION_LAYOUT", "09C_feature_preflight_audit.csv"),
    file.path(tables_dir, "09C_primary_reduced_feature_weak_label_ML_V4_FULL_PUBLICATION_LAYOUT", "09C_method_and_claim_boundary_note.txt")
  )
)

frozen_files[, exists := file.exists(path)]
frozen_files[, size_bytes := ifelse(exists, file.info(path)$size, NA_real_)]
frozen_files[, md5 := vapply(path, md5_or_missing, character(1))]
frozen_files[, frozen_status := ifelse(exists, "frozen_available", "missing_check_before_09E")]

atomic_write_csv(as.data.frame(frozen_files), frozen_manifest_csv)

stamp("构建 candidate seed table。")

candidate_dt <- as.data.table(CANDIDATE_GSE_TABLE)

if (length(EXTRA_CANDIDATE_GSE) > 0) {
  extra_dt <- data.table(
    gse_id = unique(EXTRA_CANDIDATE_GSE),
    candidate_source = "user_extra_candidate",
    intended_validation_role = "to_be_audited",
    manual_reason_before_GEO_audit = "User-added external candidate; requires GEO metadata audit."
  )
  candidate_dt <- rbindlist(list(candidate_dt, extra_dt), fill = TRUE)
}

candidate_dt[, gse_id := toupper(gse_id)]
candidate_dt <- unique(candidate_dt, by = "gse_id")

candidate_dt[, already_used_in_00_to_09C := gse_id %in% DISCOVERY_GSE_USED]
candidate_dt[, independence_rule_status := ifelse(
  already_used_in_00_to_09C,
  "exclude_used_in_discovery_pipeline",
  "independent_candidate"
)]

curated_role_dt <- as.data.table(CURATED_EXTERNAL_ROLE_TABLE)
curated_role_dt[, gse_id := toupper(gse_id)]

candidate_dt <- merge(candidate_dt, curated_role_dt, by = "gse_id", all.x = TRUE)

candidate_dt[is.na(curated_role), curated_role := "not_curated_manual_review"]
candidate_dt[is.na(curated_recommended_role), curated_recommended_role := "manual_review_only"]
candidate_dt[is.na(curated_priority_rank), curated_priority_rank := 999L]
candidate_dt[is.na(curated_reason), curated_reason := "No pre-specified curated role; manual review required."]

atomic_write_csv(as.data.frame(candidate_dt), candidate_seed_csv)
atomic_write_csv(as.data.frame(curated_role_dt), curated_role_table_csv)

stamp("开始在线 GEO metadata audit。")

geo_meta_list <- list()
gsm_meta_list <- list()

for (acc in candidate_dt$gse_id) {
  stamp("Query GEO metadata: ", acc)

  gse_obj <- NULL
  use_soft_fallback <- FALSE
  geoquery_error <- NA_character_

  if (HAS_GEOQUERY) {
    gse_obj <- tryCatch({
      GEOquery::getGEO(acc, GSEMatrix = FALSE, AnnotGPL = FALSE)
    }, error = function(e) {
      geoquery_error <<- conditionMessage(e)
      e
    })

    if (inherits(gse_obj, "error")) {
      use_soft_fallback <- TRUE
    }
  } else {
    use_soft_fallback <- TRUE
  }

  if (!use_soft_fallback && !is.null(gse_obj) && !inherits(gse_obj, "error")) {
    meta <- GEOquery::Meta(gse_obj)
    gsm_list <- tryCatch(GEOquery::GSMList(gse_obj), error = function(e) list())

    geo_meta_list[[length(geo_meta_list) + 1L]] <- data.table(
      gse_id = acc,
      geo_query_status = "success_GEOquery",
      geo_error_message = NA_character_,
      title = safe_meta(meta, "title"),
      organism = safe_meta(meta, "sample_organism"),
      experiment_type = safe_meta(meta, "type"),
      summary = safe_meta(meta, "summary"),
      overall_design = safe_meta(meta, "overall_design"),
      sample_count = length(gsm_list),
      supplementary_file = safe_meta(meta, "supplementary_file"),
      pubmed_id = safe_meta(meta, "pubmed_id"),
      status = safe_meta(meta, "status"),
      last_update_date = safe_meta(meta, "last_update_date"),
      metadata_source = "GEOquery",
      soft_url = gse_soft_url(acc),
      soft_cache_file = NA_character_
    )

    if (length(gsm_list) > 0) {
      gsm_names <- names(gsm_list)
      max_gsm_to_record <- min(length(gsm_list), 80)

      for (gsm_id in gsm_names[seq_len(max_gsm_to_record)]) {
        gsm <- gsm_list[[gsm_id]]
        gm <- tryCatch(GEOquery::Meta(gsm), error = function(e) list())

        gsm_text <- collapse_text(
          safe_meta(gm, "title"),
          safe_meta(gm, "source_name_ch1"),
          safe_meta(gm, "characteristics_ch1"),
          safe_meta(gm, "description"),
          safe_meta(gm, "relation")
        )

        gsm_meta_list[[length(gsm_meta_list) + 1L]] <- data.table(
          gse_id = acc,
          gsm_id = gsm_id,
          sample_title = safe_meta(gm, "title"),
          source_name = safe_meta(gm, "source_name_ch1"),
          characteristics = safe_meta(gm, "characteristics_ch1"),
          relation = safe_meta(gm, "relation"),
          sample_text_has_sra = has_any_pattern(gsm_text, c("sra", "srx", "srr")),
          sample_text_has_10x = has_any_pattern(gsm_text, c("10x", "chromium")),
          sample_text_has_single_cell = has_any_pattern(gsm_text, c("single.cell", "single cell", "scrna", "snrna", "single nuclei", "single nucleus")),
          sample_text_has_dopamine = has_any_pattern(gsm_text, c("dopamin", "\\bda\\b", "midbrain", "substantia nigra", "snpc")),
          sample_text_has_pd = has_any_pattern(gsm_text, c("parkinson", "\\bpd\\b", "lrrk2", "pink1", "snca")),
          metadata_source = "GEOquery"
        )
      }
    }
  } else {
    soft_res <- tryCatch({
      parse_gse_soft_fallback(acc, soft_cache_dir)
    }, error = function(e) {
      e
    })

    if (inherits(soft_res, "error")) {
      geo_meta_list[[length(geo_meta_list) + 1L]] <- data.table(
        gse_id = acc,
        geo_query_status = "failed_GEOquery_and_SOFT_fallback",
        geo_error_message = paste(
          "GEOquery:",
          ifelse(is.na(geoquery_error), "not_available_or_not_used", geoquery_error),
          "| SOFT:",
          conditionMessage(soft_res)
        ),
        title = NA_character_,
        organism = NA_character_,
        experiment_type = NA_character_,
        summary = NA_character_,
        overall_design = NA_character_,
        sample_count = NA_integer_,
        supplementary_file = NA_character_,
        pubmed_id = NA_character_,
        status = NA_character_,
        last_update_date = NA_character_,
        metadata_source = ifelse(HAS_GEOQUERY, "GEOquery_failed_SOFT_failed", "SOFT_fallback_failed"),
        soft_url = gse_soft_url(acc),
        soft_cache_file = NA_character_
      )
    } else {
      geo_meta_list[[length(geo_meta_list) + 1L]] <- soft_res$series
      if (nrow(soft_res$gsm) > 0) {
        gsm_meta_list[[length(gsm_meta_list) + 1L]] <- soft_res$gsm
      }
    }
  }
}

geo_meta <- rbindlist(geo_meta_list, fill = TRUE)
gsm_meta <- rbindlist(gsm_meta_list, fill = TRUE)

atomic_write_csv(as.data.frame(geo_meta), geo_metadata_csv)
atomic_write_csv(as.data.frame(gsm_meta), gsm_summary_csv)

stamp("计算 external validation eligibility score。")

audit <- merge(
  candidate_dt,
  geo_meta,
  by = "gse_id",
  all.x = TRUE
)

audit[, searchable_text := collapse_text(
  title,
  organism,
  experiment_type,
  summary,
  overall_design,
  supplementary_file,
  candidate_source,
  intended_validation_role,
  manual_reason_before_GEO_audit
), by = gse_id]

audit[, is_independent := !already_used_in_00_to_09C]
audit[, is_human := has_any_pattern(searchable_text, c("homo sapiens", "human"))]
audit[, is_single_cell_or_single_nucleus := has_any_pattern(
  searchable_text,
  c("single.cell", "single cell", "single.nuc", "single nucleus", "single nuclei", "scrna", "snrna", "10x", "chromium")
)]
audit[, is_pd_related := has_any_pattern(
  searchable_text,
  c("parkinson", "\\bpd\\b", "lrrk2", "pink1", "snca", "sporadic pd")
)]
audit[, is_da_midbrain_or_graft_related := has_any_pattern(
  searchable_text,
  c("dopamin", "\\bda\\b", "midbrain", "substantia nigra", "snpc", "ventral midbrain", "ipsc", "stem", "differentiation", "graft", "transplant")
)]
audit[, has_processed_or_supplementary := !is.na(supplementary_file) & supplementary_file != "" & !grepl("^NA$", supplementary_file)]
audit[, sample_count_numeric := suppressWarnings(as.integer(sample_count))]
audit[, has_enough_samples := !is.na(sample_count_numeric) & sample_count_numeric >= MIN_SAMPLE_COUNT_RECOMMENDED]

if (nrow(gsm_meta) > 0) {
  gsm_summary_by_gse <- gsm_meta[, .(
    recorded_gsm_n = .N,
    any_sample_sra = any(sample_text_has_sra, na.rm = TRUE),
    any_sample_10x = any(sample_text_has_10x, na.rm = TRUE),
    any_sample_single_cell = any(sample_text_has_single_cell, na.rm = TRUE),
    any_sample_dopamine = any(sample_text_has_dopamine, na.rm = TRUE),
    any_sample_pd = any(sample_text_has_pd, na.rm = TRUE)
  ), by = gse_id]
} else {
  gsm_summary_by_gse <- data.table(
    gse_id = character(),
    recorded_gsm_n = integer(),
    any_sample_sra = logical(),
    any_sample_10x = logical(),
    any_sample_single_cell = logical(),
    any_sample_dopamine = logical(),
    any_sample_pd = logical()
  )
}

audit <- merge(audit, gsm_summary_by_gse, by = "gse_id", all.x = TRUE)

logical_cols <- c(
  "any_sample_sra",
  "any_sample_10x",
  "any_sample_single_cell",
  "any_sample_dopamine",
  "any_sample_pd"
)

for (cc in logical_cols) {
  audit[is.na(get(cc)), (cc) := FALSE]
}

audit[, evidence_single_cell := is_single_cell_or_single_nucleus | any_sample_single_cell | any_sample_10x]
audit[, evidence_da_pd_context := is_pd_related | any_sample_pd | is_da_midbrain_or_graft_related | any_sample_dopamine]
audit[, evidence_accessibility := has_processed_or_supplementary | any_sample_sra]

audit[, has_ipsc_or_stem_differentiation_context := has_any_pattern(
  searchable_text,
  c("ipsc", "induced pluripotent", "stem", "progenitor", "differentiation", "organoid")
)]

audit[, has_graft_or_transplant_context := has_any_pattern(
  searchable_text,
  c("graft", "transplant", "transplantation", "engraft")
)]

audit[, has_human_snpc_or_midbrain_tissue_context := has_any_pattern(
  searchable_text,
  c("substantia nigra", "snpc", "midbrain", "ventral midbrain", "postmortem", "post.mortem", "brain tissue")
)]

audit[, has_processed_matrix_evidence := has_any_pattern(
  collapse_text(supplementary_file, summary, overall_design),
  c("matrix", "mtx", "\\.h5", "hdf5", "filtered_feature_bc_matrix", "barcodes", "features", "genes.tsv", "counts", "expression", "\\.csv", "\\.tsv", "\\.txt", "\\.rds", "\\.rda", "loom")
)]

audit[, has_raw_or_sra_evidence := any_sample_sra | has_any_pattern(
  collapse_text(supplementary_file, summary, overall_design),
  c("sra", "srx", "srr", "fastq", "raw", "bam")
)]

audit[, data_accessibility_tier := fifelse(
  has_processed_matrix_evidence,
  "processed_matrix_evidence_detected",
  fifelse(
    has_raw_or_sra_evidence,
    "raw_or_SRA_evidence_detected",
    fifelse(
      has_processed_or_supplementary,
      "supplementary_file_present_manual_check",
      "accessibility_evidence_not_detected"
    )
  )
)]

audit[, biological_context_tier := fifelse(
  (has_ipsc_or_stem_differentiation_context | has_graft_or_transplant_context) &
    (is_da_midbrain_or_graft_related | any_sample_dopamine),
  "primary_like_iPSC_DA_or_graft_related",
  fifelse(
    has_human_snpc_or_midbrain_tissue_context & (is_pd_related | any_sample_pd),
    "human_PD_midbrain_context",
    fifelse(
      is_pd_related | any_sample_pd | is_da_midbrain_or_graft_related | any_sample_dopamine,
      "secondary_PD_or_DA_context",
      "weak_context"
    )
  )
)]

audit[, has_dopaminergic_explicit_context := has_any_pattern(
  searchable_text,
  c("dopaminergic", "dopamine", "\\bda neuron", "\\bda neurons", "\\bda\\b", "midbrain dopamin", "ventral midbrain")
)]

audit[, has_iPSC_DA_differentiation_context := has_any_pattern(
  searchable_text,
  c("ipsc", "induced pluripotent", "pluripotent", "differentiation", "differentiated")
) & has_dopaminergic_explicit_context]

audit[, has_graft_like_or_transplant_explicit_context := has_any_pattern(
  searchable_text,
  c("graft", "transplant", "transplantation", "engraft")
) & has_dopaminergic_explicit_context]

audit[, has_neural_stem_cell_only_context := has_any_pattern(
  searchable_text,
  c("neural stem cell", "neural stem cells", "\\bnsc\\b")
) & !has_dopaminergic_explicit_context & !has_graft_like_or_transplant_explicit_context]

audit[, has_postmortem_or_tissue_context := has_any_pattern(
  searchable_text,
  c("postmortem", "post.mortem", "human substantia nigra", "substantia nigra", "snpc", "brain tissue", "midbrain tissue")
)]

audit[, strict_primary_biology_match := (
  has_iPSC_DA_differentiation_context |
    has_graft_like_or_transplant_explicit_context
)]

audit[, strict_context_biology_match := (
  has_postmortem_or_tissue_context &
    (is_pd_related | any_sample_pd | has_dopaminergic_explicit_context)
)]

audit[, strict_backup_biology_match := (
  has_neural_stem_cell_only_context |
    (
      (is_pd_related | any_sample_pd) &
        evidence_single_cell &
        !strict_primary_biology_match &
        !strict_context_biology_match
    )
)]

audit[, primary_match_for_09E := is_independent &
        is_human &
        evidence_single_cell &
        evidence_accessibility &
        strict_primary_biology_match]

audit[, context_match_for_09E := is_independent &
        is_human &
        evidence_single_cell &
        evidence_accessibility &
        strict_context_biology_match]

audit[, backup_match_for_09E := is_independent &
        is_human &
        evidence_single_cell &
        evidence_accessibility &
        strict_backup_biology_match]

audit[, score_independent := ifelse(is_independent, 2, -10)]
audit[, score_human := ifelse(is_human, 2, 0)]
audit[, score_single_cell := ifelse(evidence_single_cell, 3, 0)]
audit[, score_pd_context := ifelse(is_pd_related | any_sample_pd, 2, 0)]
audit[, score_da_midbrain_graft_context := ifelse(is_da_midbrain_or_graft_related | any_sample_dopamine, 3, 0)]
audit[, score_accessibility := ifelse(evidence_accessibility, 2, 0)]
audit[, score_sample_count := ifelse(has_enough_samples, 1, 0)]

audit[, eligibility_score := score_independent +
        score_human +
        score_single_cell +
        score_pd_context +
        score_da_midbrain_graft_context +
        score_accessibility +
        score_sample_count]

audit[, hard_exclusion_reason := fifelse(
  already_used_in_00_to_09C,
  "already_used_in_00_to_09C_discovery_pipeline",
  fifelse(
    !grepl("^success", geo_query_status),
    "GEO_metadata_query_failed",
    fifelse(
      !is_human,
      "not_human_or_human_not_detected",
      fifelse(
        !evidence_single_cell,
        "single_cell_or_single_nucleus_evidence_not_detected",
        "none"
      )
    )
  )
)]

audit[, external_validation_tier := fifelse(
  hard_exclusion_reason != "none",
  "exclude",
  fifelse(
    curated_role %chin% c(
      "tier1_primary_frozen_validation_candidate",
      "tier1_context_validation_candidate",
      "tier2_backup_or_mechanistic_context_candidate"
    ),
    curated_role,
    fifelse(
      primary_match_for_09E,
      "tier1_primary_frozen_validation_candidate",
      fifelse(
        context_match_for_09E,
        "tier1_context_validation_candidate",
        fifelse(
          backup_match_for_09E,
          "tier2_backup_or_mechanistic_context_candidate",
          "tier3_manual_review_only"
        )
      )
    )
  )
)]

audit[, recommended_validation_role_after_audit := fifelse(
  external_validation_tier == "exclude",
  "do_not_use_for_09E",
  fifelse(
    curated_recommended_role != "manual_review_only",
    curated_recommended_role,
    fifelse(
      external_validation_tier == "tier1_primary_frozen_validation_candidate",
      "primary_frozen_scoring_validation_candidate_for_09E",
      fifelse(
        external_validation_tier == "tier1_context_validation_candidate",
        "disease_context_external_validation_candidate_for_09E",
        fifelse(
          external_validation_tier == "tier2_backup_or_mechanistic_context_candidate",
          "backup_or_mechanistic_context_candidate_not_primary",
          "manual_review_only"
        )
      )
    )
  )
)]

audit[, readiness_for_09E := fifelse(
  external_validation_tier == "tier1_primary_frozen_validation_candidate" &
    has_processed_matrix_evidence,
  "ready_for_09E_primary_processed_matrix_likely_available",
  fifelse(
    external_validation_tier == "tier1_primary_frozen_validation_candidate" &
      evidence_accessibility,
    "manual_download_check_before_09E_primary",
    fifelse(
      external_validation_tier == "tier1_context_validation_candidate" &
        has_processed_matrix_evidence,
      "ready_for_09E_context_processed_matrix_likely_available",
      fifelse(
        external_validation_tier == "tier1_context_validation_candidate" &
          evidence_accessibility,
        "manual_download_check_before_09E_context",
        fifelse(
          external_validation_tier == "tier2_backup_or_mechanistic_context_candidate" &
            evidence_accessibility,
          "backup_only_manual_check_before_use",
          "not_for_09E"
        )
      )
    )
  )
)]

audit[, validation_claim_boundary := fifelse(
  external_validation_tier == "tier1_primary_frozen_validation_candidate",
  "Primary frozen-framework external validation candidate in a biologically closer iPSC-derived/DA/graft-like context. Still transcriptomic only; does not prove projection, integration, efficacy or safety.",
  fifelse(
    external_validation_tier == "tier1_context_validation_candidate",
    "Disease-context validation candidate in human midbrain/SNpc/postmortem PD context. It can test whether score patterns align with disease-relevant DA/midbrain biology, but it is not a direct graft validation.",
    fifelse(
      external_validation_tier == "tier2_backup_or_mechanistic_context_candidate",
      "Backup/mechanistic context candidate. Use only if Tier1 primary/context datasets fail or as secondary supplementary analysis.",
      "Manual review only; do not use for 09E without written justification."
    )
  )
)]

audit[, final_selection_basis := fifelse(
  hard_exclusion_reason != "none",
  paste0("hard_exclusion: ", hard_exclusion_reason),
  fifelse(
    curated_role != "not_curated_manual_review",
    paste0("pre_09E_curated_role: ", curated_role),
    "automated_metadata_rule"
  )
)]

audit[, selection_priority_key := fifelse(
  external_validation_tier == "tier1_primary_frozen_validation_candidate", 1L,
  fifelse(
    external_validation_tier == "tier1_context_validation_candidate", 2L,
    fifelse(
      external_validation_tier == "tier2_backup_or_mechanistic_context_candidate", 3L,
      fifelse(external_validation_tier == "tier3_manual_review_only", 4L, 9L)
    )
  )
)]

setorder(audit, selection_priority_key, curated_priority_rank, -eligibility_score, gse_id)

eligibility_cols <- c(
  "gse_id",
  "external_validation_tier",
  "recommended_validation_role_after_audit",
  "eligibility_score",
  "hard_exclusion_reason",
  "is_independent",
  "is_human",
  "evidence_single_cell",
  "is_pd_related",
  "is_da_midbrain_or_graft_related",
  "evidence_accessibility",
  "sample_count_numeric",
  "biological_context_tier",
  "strict_primary_biology_match",
  "strict_context_biology_match",
  "strict_backup_biology_match",
  "has_iPSC_DA_differentiation_context",
  "has_graft_like_or_transplant_explicit_context",
  "has_postmortem_or_tissue_context",
  "has_neural_stem_cell_only_context",
  "data_accessibility_tier",
  "has_processed_matrix_evidence",
  "has_raw_or_sra_evidence",
  "readiness_for_09E",
  "validation_claim_boundary",
  "final_selection_basis",
  "curated_role",
  "curated_recommended_role",
  "curated_priority_rank",
  "curated_reason",
  "title",
  "experiment_type",
  "organism",
  "status",
  "pubmed_id",
  "supplementary_file",
  "manual_reason_before_GEO_audit",
  "geo_query_status",
  "geo_error_message"
)

eligibility_cols <- eligibility_cols[eligibility_cols %in% names(audit)]

eligibility_audit <- audit[, eligibility_cols, with = FALSE]

atomic_write_csv(as.data.frame(eligibility_audit), eligibility_csv)

stamp("生成 external validation decision report。")

tier1_primary <- eligibility_audit[external_validation_tier == "tier1_primary_frozen_validation_candidate"]
tier1_context <- eligibility_audit[external_validation_tier == "tier1_context_validation_candidate"]
tier2 <- eligibility_audit[external_validation_tier == "tier2_backup_or_mechanistic_context_candidate"]

primary_candidate <- if (nrow(tier1_primary) > 0) {
  tier1_primary[order(curated_priority_rank, -eligibility_score)][1]
} else if (nrow(tier1_context) > 0) {
  tier1_context[order(curated_priority_rank, -eligibility_score)][1]
} else if (nrow(tier2) > 0) {
  tier2[order(curated_priority_rank, -eligibility_score)][1]
} else {
  data.table()
}

backup_candidates <- rbindlist(list(
  if (nrow(tier1_primary) > 1) tier1_primary[order(curated_priority_rank, -eligibility_score)][-1] else data.table(),
  tier1_context[order(curated_priority_rank, -eligibility_score)],
  tier2[order(curated_priority_rank, -eligibility_score)]
), fill = TRUE)

decision_lines <- c(
  "09D External validation dataset eligibility decision report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Discovery datasets frozen/excluded from external validation:",
  paste(DISCOVERY_GSE_USED, collapse = ", "),
  "",
  "Decision principle:",
  "09D only selects/audits external validation candidates.",
  "No 04A marker panel, 05B threshold, 09B feature set or 09C model will be changed based on 09D results.",
  "External validation in 09E must use frozen scoring/model application.
V5 strictly separates biologically closer iPSC-derived/DA/graft-like primary validation, broader human PD midbrain disease-context validation, and backup/mechanistic context datasets.",
  ""
)

if (nrow(primary_candidate) > 0) {
  decision_lines <- c(
    decision_lines,
    "Primary recommended candidate:",
    paste0("GSE: ", primary_candidate$gse_id),
    paste0("Tier: ", primary_candidate$external_validation_tier),
    paste0("Eligibility score: ", primary_candidate$eligibility_score),
    paste0("Recommended role: ", primary_candidate$recommended_validation_role_after_audit),
    paste0("Readiness for 09E: ", primary_candidate$readiness_for_09E),
    paste0("Selection basis: ", primary_candidate$final_selection_basis),
    paste0("Curated reason: ", primary_candidate$curated_reason),
    paste0("Title: ", primary_candidate$title),
    ""
  )
} else {
  decision_lines <- c(
    decision_lines,
    "Primary recommended candidate:",
    "None identified by the current rule. Manual review or additional candidate search is required.",
    ""
  )
}

if (nrow(backup_candidates) > 0) {
  decision_lines <- c(decision_lines, "Backup/context candidates:")
  for (ii in seq_len(min(nrow(backup_candidates), 10))) {
    decision_lines <- c(
      decision_lines,
      paste0(
        ii, ". ", backup_candidates$gse_id[ii],
        " | tier=", backup_candidates$external_validation_tier[ii],
        " | score=", backup_candidates$eligibility_score[ii],
        " | role=", backup_candidates$recommended_validation_role_after_audit[ii]
      )
    )
  }
  decision_lines <- c(decision_lines, "")
}

decision_lines <- c(
  decision_lines,
  "Claim boundary:",
  "A 09E external validation result can support external transcriptomic reproducibility of the prioritization framework.",
  "It cannot prove anatomical projection, graft integration, therapeutic efficacy, tumorigenicity, or clinical safety.",
  "",
  "Next step:",
  "09E should download/process the selected external dataset and apply the frozen 05A/05B/09C framework without threshold tuning or model retraining."
)

writeLines(decision_lines, decision_txt)

method_lines <- c(
  "09D external validation dataset audit method and claim-boundary note",
  "",
  "Method-ready wording:",
  paste0(
    "After completing the discovery and primary marker-rule-derived machine-learning pipeline, we performed an external dataset eligibility audit. ",
    "Datasets already used in the 00-09C discovery framework were excluded from external validation. ",
    "Candidate datasets were evaluated using GEO metadata for independence, human origin, single-cell or single-nucleus evidence, Parkinson/dopaminergic/midbrain relevance, sample count and raw/processed data accessibility. ",
    "The selected external validation dataset will be used only for frozen framework application in the next step, without modifying marker panels, thresholds or trained models."
  ),
  "",
  "Claim boundary:",
  "09D does not analyze expression matrices and does not validate the biological conclusions by itself.",
  "09D only audits dataset eligibility and freezes the decision path for 09E.",
  "External validation supports transcriptomic reproducibility, not functional graft efficacy or clinical safety."
)

writeLines(method_lines, method_note_txt)

plan_lines <- c(
  "09D to 09E frozen external validation plan",
  "",
  "09E must follow these rules:",
  "1. Use the primary 09D selected dataset unless manual failure is documented.",
  "2. Do not change 04A marker panel based on external results.",
  "3. Do not retune 05B thresholds based on external results.",
  "4. Do not retrain 09C model on the external dataset.",
  "5. Apply frozen scoring and/or reduced-feature model only.",
  "6. Report failures transparently if gene overlap or metadata is insufficient.",
  "",
  "Minimum 09E outputs:",
  "09E_external_data_import_audit.csv",
  "09E_external_gene_overlap_audit.csv",
  "09E_external_score_table.csv",
  "09E_external_prediction_table.csv",
  "09E_external_validation_summary.csv",
  "09E_external_validation_claim_boundary_note.txt"
)

writeLines(plan_lines, next_step_plan_txt)
writeLines(capture.output(sessionInfo()), session_info_txt)

stamp("生成 09D PDF figures。")

plot_dt <- copy(eligibility_audit)
plot_dt[, label := paste0(gse_id, " | ", short_label(title, 58))]
plot_dt[, tier_short := tier_short_label_09D(external_validation_tier)]
plot_dt <- plot_dt[order(eligibility_score)]

safe_pdf(fig_score_pdf, width = 14.2, height = max(7.6, 0.50 * nrow(plot_dt) + 3.0))

par(mar = c(5.4, 19.2, 6.0, 5.4), xpd = FALSE)

xmax_score <- max(plot_dt$eligibility_score, na.rm = TRUE)
xmin_score <- min(0, min(plot_dt$eligibility_score, na.rm = TRUE))

bp <- barplot(
  plot_dt$eligibility_score,
  names.arg = plot_dt$label,
  horiz = TRUE,
  las = 1,
  xlab = "Eligibility score",
  main = "09D external validation candidate eligibility score",
  col = ifelse(plot_dt$external_validation_tier == "tier1_primary_frozen_validation_candidate", "grey25",
               ifelse(plot_dt$external_validation_tier == "tier1_context_validation_candidate", "grey45",
                      ifelse(plot_dt$external_validation_tier == "tier2_backup_or_mechanistic_context_candidate", "grey60", "grey80"))),
  border = "grey25",
  cex.names = 0.64,
  cex.axis = 0.90,
  cex.lab = 1.05,
  xlim = c(xmin_score, xmax_score + 3.4)
)

abline(v = MIN_RECOMMENDED_ELIGIBILITY_SCORE, lty = 2, col = "red")

mtext(
  paste0("Recommended score threshold = ", MIN_RECOMMENDED_ELIGIBILITY_SCORE),
  side = 3,
  line = 0.55,
  adj = 0.98,
  cex = 0.78,
  col = "red"
)

text(
  x = xmax_score + 0.42,
  y = bp,
  labels = plot_dt$tier_short,
  cex = 0.70,
  adj = 0
)

finish_pdf(fig_score_pdf)

role_summary <- eligibility_audit[
  ,
  .N,
  by = .(external_validation_tier, recommended_validation_role_after_audit)
][order(external_validation_tier, -N)]

role_summary[, role_label := role_short_label_09D(
  external_validation_tier,
  recommended_validation_role_after_audit
)]

safe_pdf(fig_role_pdf, width = 11.8, height = max(6.8, 0.62 * nrow(role_summary) + 2.4))

par(mar = c(5.2, 13.8, 4.2, 2.2), xpd = FALSE)

bp2 <- barplot(
  role_summary$N,
  names.arg = role_summary$role_label,
  horiz = TRUE,
  las = 1,
  xlab = "Number of candidate datasets",
  main = "09D external validation role summary",
  col = "grey60",
  border = "grey25",
  cex.names = 0.80,
  cex.axis = 0.90,
  cex.lab = 1.05,
  xlim = c(0, max(role_summary$N, na.rm = TRUE) + 1.0)
)

text(
  x = role_summary$N + 0.08,
  y = bp2,
  labels = role_summary$N,
  cex = 0.78,
  adj = 0
)

finish_pdf(fig_role_pdf)

report_lines <- c(
  "09D external validation dataset audit report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Candidate seed table:",
  capture.output(print(candidate_dt)),
  "",
  "Eligibility audit:",
  capture.output(print(eligibility_audit)),
  "",
  "Frozen manifest:",
  capture.output(print(frozen_files)),
  "",
  "Output directories:",
  out_tables_dir,
  out_figures_dir
)

writeLines(report_lines, report_txt)

required_output_files <- c(
  candidate_seed_csv,
  curated_role_table_csv,
  geo_metadata_csv,
  gsm_summary_csv,
  eligibility_csv,
  frozen_manifest_csv,
  decision_txt,
  method_note_txt,
  next_step_plan_txt,
  session_info_txt,
  report_txt,
  fig_score_pdf,
  fig_role_pdf
)

output_check <- data.table(
  file = required_output_files,
  exists = file.exists(required_output_files),
  size_bytes = ifelse(
    file.exists(required_output_files),
    file.info(required_output_files)$size,
    NA_real_
  )
)

atomic_write_csv(as.data.frame(output_check), output_check_csv)

bad_outputs <- output_check[!exists | is.na(size_bytes) | size_bytes <= 0]

if (nrow(bad_outputs) > 0) {
  print(bad_outputs)
  stop("09D 输出验证失败。")
}

cat("\n============================================================\n")
cat("09D external validation dataset eligibility audit FINAL V8 PUBLICATION POLISH 运行结束\n")
cat("============================================================\n\n")

cat("候选 GSE 数量：", nrow(candidate_dt), "\n")
cat("成功查询 GEO metadata：", sum(grepl("^success", geo_meta$geo_query_status), na.rm = TRUE), "\n")
cat("Tier1 primary frozen-validation candidates：", nrow(tier1_primary), "\n")
cat("Tier1 context-validation candidates：", nrow(tier1_context), "\n")
cat("Tier2 backup/mechanistic-context candidates：", nrow(tier2), "\n\n")

if (nrow(primary_candidate) > 0) {
  cat("Primary recommended external candidate：", primary_candidate$gse_id, "\n")
  cat("Eligibility score：", primary_candidate$eligibility_score, "\n")
  cat("Role：", primary_candidate$recommended_validation_role_after_audit, "\n\n")
} else {
  cat("Primary recommended external candidate：NONE，需要人工补充候选。\n\n")
}

cat("输出目录：\n")
cat(out_tables_dir, "\n")
cat(out_figures_dir, "\n\n")

cat("关键输出：\n")
cat(eligibility_csv, "\n")
cat(frozen_manifest_csv, "\n")
cat(decision_txt, "\n")
cat(next_step_plan_txt, "\n")
cat(method_note_txt, "\n\n")

cat("PDF 图：\n")
cat(fig_score_pdf, "\n")
cat(fig_role_pdf, "\n\n")

cat("✅ 09D external validation dataset eligibility audit FINAL V8 PUBLICATION POLISH 完成。\n")
