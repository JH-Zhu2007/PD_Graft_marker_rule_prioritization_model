
PROJECT_DIR <- "D:/PD_Graft_Project"

AUTO_RESCUE <- TRUE

STOP_IF_NO_CANDIDATE <- TRUE

REQUIRE_SINGLE_PAGE_MAIN_SOURCE <- FALSE

MIN_PDF_SIZE_BYTES <- 1000

LOW_CONFIDENCE_SCORE <- 45

HIGH_CONFIDENCE_SCORE <- 70

REQUIRE_MODULE_PREFIX_FOR_AUTO_RESCUE <- TRUE

REQUIRE_PANEL_SPECIFIC_POSITIVE_PATTERN <- TRUE

PRESERVE_FILE_DATE <- TRUE

required_pkgs <- c("data.table", "digest", "pdftools")

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0L) {
  stop(
    paste0(
      "缺少 R 包：",
      paste(missing_pkgs, collapse = ", "),
      "\n请先运行：install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "),
      "))"
    )
  )
}

suppressPackageStartupMessages({
  library(data.table)
})

FIGURE_ROOT <- file.path(PROJECT_DIR, "04_figures")
TABLE_ROOT <- file.path(PROJECT_DIR, "03_tables")
REPORT_ROOT <- file.path(PROJECT_DIR, "06_reports")
MANUSCRIPT_ROOT <- file.path(PROJECT_DIR, "09_manuscript")

DIR_10A <- file.path(TABLE_ROOT, "10A_final_manuscript_figure_panel_V1")
DIR_10B <- file.path(REPORT_ROOT, "10B_manuscript_FINAL_STANDALONE_V7")

INPUT_10A_MAIN_PLAN <- file.path(DIR_10A, "10A_main_figure_plan.csv")
INPUT_10A_SUPP_PLAN <- file.path(DIR_10A, "10A_supplementary_figure_plan.csv")
INPUT_10A_STORYLINE <- file.path(DIR_10A, "10A_storyline_table.csv")
INPUT_10A_KEY_NUMBERS <- file.path(DIR_10A, "10A_key_numbers_for_abstract_and_results.csv")
INPUT_10A_FINAL_VERSION_MANIFEST <- file.path(DIR_10A, "10A_final_version_manifest.csv")
INPUT_10B_MANUSCRIPT <- file.path(DIR_10B, "10B_full_manuscript_FINAL_STANDALONE_V7.md")

OUT_PACKAGE_DIR <- file.path(
  MANUSCRIPT_ROOT,
  "10C_final_V16_F2E_S7A_HALLMARK_BARPLOT_F5B_CLUSTER_SIZE"
)

OUT_MAIN_DIR <- file.path(OUT_PACKAGE_DIR, "main_figure_sources")
OUT_SUPP_DIR <- file.path(OUT_PACKAGE_DIR, "supplementary_figure_sources")

OUT_TABLE_DIR <- file.path(
  TABLE_ROOT,
  "10C_final_V16_F2E_S7A_HALLMARK_BARPLOT_F5B_CLUSTER_SIZE"
)

OUT_REPORT_DIR <- file.path(
  REPORT_ROOT,
  "10C_final_V16_F2E_S7A_HALLMARK_BARPLOT_F5B_CLUSTER_SIZE"
)

dir.create(OUT_MAIN_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_SUPP_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_REPORT_DIR, recursive = TRUE, showWarnings = FALSE)

OUT_INPUT_AUDIT <- file.path(OUT_TABLE_DIR, "10C_V16_input_audit.csv")
OUT_VERSION_AUDIT <- file.path(OUT_TABLE_DIR, "10C_V16_final_version_lock_audit.csv")
OUT_PDF_INVENTORY <- file.path(OUT_TABLE_DIR, "10C_V16_all_pdf_inventory_with_integrity.csv")
OUT_STRICT_CANDIDATES <- file.path(OUT_TABLE_DIR, "10C_V16_strict_candidate_audit.csv")
OUT_LOOSE_CANDIDATES <- file.path(OUT_TABLE_DIR, "10C_V16_loose_candidate_ranking_audit.csv")
OUT_MAIN_MANIFEST <- file.path(OUT_TABLE_DIR, "10C_V16_main_figure_source_manifest.csv")
OUT_SUPP_MANIFEST <- file.path(OUT_TABLE_DIR, "10C_V16_supplementary_figure_source_manifest.csv")
OUT_PANEL_MAPPING <- file.path(OUT_TABLE_DIR, "10C_V16_manuscript_panel_mapping.csv")
OUT_UNRESOLVED <- file.path(OUT_TABLE_DIR, "10C_V16_unresolved_sources.csv")
OUT_DUPLICATE_AUDIT <- file.path(OUT_TABLE_DIR, "10C_V16_duplicate_source_usage_audit.csv")
OUT_COPY_AUDIT <- file.path(OUT_TABLE_DIR, "10C_V16_copy_and_hash_integrity_audit.csv")
OUT_SELECTION_SUMMARY <- file.path(OUT_TABLE_DIR, "10C_V16_selection_confidence_summary.csv")
OUT_LEGENDS <- file.path(OUT_REPORT_DIR, "10C_V16_final_figure_legends_DRAFT_FROM_LOCKED_SOURCES.txt")
OUT_ASSEMBLY_BRIEF <- file.path(OUT_REPORT_DIR, "10C_V16_10D_assembly_brief.txt")
OUT_CLAIM_BOUNDARY <- file.path(OUT_REPORT_DIR, "10C_V16_figure_claim_boundary_note.txt")
OUT_REPORT <- file.path(OUT_REPORT_DIR, "10C_V16_final_figure_source_lock_report.txt")
OUT_SESSION <- file.path(OUT_REPORT_DIR, "10C_V16_sessionInfo.txt")
OUT_VERIFICATION <- file.path(OUT_TABLE_DIR, "10C_V16_output_verification.csv")

stamp <- function(...) {
  cat("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ..., "\n", sep = "")
}

normalize_path <- function(x) {
  normalizePath(x, winslash = "/", mustWork = FALSE)
}

escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (is.null(df) || ncol(df) == 0L) {
    df <- data.frame(status = "empty", stringsAsFactors = FALSE)
  }

  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  data.table::fwrite(df, tmp, bom = TRUE)

  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }

  if (!file.rename(tmp, path)) {
    stop("CSV 写入失败：", path)
  }

  if (!file.exists(path) || !is.finite(file.info(path)$size) || file.info(path)$size <= 0) {
    stop("CSV 输出无效：", path)
  }

  invisible(path)
}

atomic_write_text <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  tmp <- paste0(path, ".tmp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  writeLines(enc2utf8(as.character(lines)), con = tmp, useBytes = TRUE)

  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }

  if (!file.rename(tmp, path)) {
    stop("文本写入失败：", path)
  }

  if (!file.exists(path) || !is.finite(file.info(path)$size) || file.info(path)$size <= 0) {
    stop("文本输出无效：", path)
  }

  invisible(path)
}

safe_fread <- function(path) {
  if (!file.exists(path)) {
    return(data.frame())
  }

  data.table::fread(
    path,
    data.table = FALSE,
    showProgress = FALSE,
    encoding = "UTF-8"
  )
}

safe_read_text <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }

  readLines(path, warn = FALSE, encoding = "UTF-8")
}

sha256_file <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }

  tryCatch(
    digest::digest(file = path, algo = "sha256", serialize = FALSE),
    error = function(e) NA_character_
  )
}

pdf_header_ok <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }

  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)

  raw_head <- readBin(con, what = "raw", n = 5)
  identical(rawToChar(raw_head), "%PDF-")
}

pdf_integrity_record <- function(path) {
  size_bytes <- if (file.exists(path)) {
    file.info(path)$size
  } else {
    NA_real_
  }

  header_ok <- if (file.exists(path)) {
    pdf_header_ok(path)
  } else {
    FALSE
  }

  info <- tryCatch(
    pdftools::pdf_info(path),
    error = function(e) NULL
  )

  data.frame(
    path = normalize_path(path),
    exists = file.exists(path),
    size_bytes = size_bytes,
    pdf_header_ok = header_ok,
    pdf_readable = !is.null(info),
    page_count = if (is.null(info)) NA_integer_ else as.integer(info$pages),
    encrypted = if (is.null(info)) NA else isTRUE(info$encrypted),
    pdf_version = if (is.null(info)) NA_character_ else as.character(info$version),
    sha256 = sha256_file(path),
    stringsAsFactors = FALSE
  )
}

relative_to_figure_root <- function(path) {
  root <- paste0(normalize_path(FIGURE_ROOT), "/")
  sub(paste0("^", escape_regex(root)), "", normalize_path(path), perl = TRUE)
}

sanitize_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

copy_with_hash_check <- function(source, destination) {
  if (!file.exists(source)) {
    return(
      data.frame(
        source_path = normalize_path(source),
        destination_path = normalize_path(destination),
        copied = FALSE,
        source_sha256 = NA_character_,
        destination_sha256 = NA_character_,
        hash_match = FALSE,
        source_size_bytes = NA_real_,
        destination_size_bytes = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  }

  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(destination)) {
    unlink(destination, force = TRUE)
  }

  copied <- file.copy(
    from = source,
    to = destination,
    overwrite = TRUE,
    copy.mode = TRUE,
    copy.date = PRESERVE_FILE_DATE
  )

  src_hash <- sha256_file(source)
  dst_hash <- sha256_file(destination)

  data.frame(
    source_path = normalize_path(source),
    destination_path = normalize_path(destination),
    copied = isTRUE(copied) && file.exists(destination),
    source_sha256 = src_hash,
    destination_sha256 = dst_hash,
    hash_match = isTRUE(!is.na(src_hash) && !is.na(dst_hash) && identical(src_hash, dst_hash)),
    source_size_bytes = file.info(source)$size,
    destination_size_bytes = if (file.exists(destination)) file.info(destination)$size else NA_real_,
    stringsAsFactors = FALSE
  )
}

contains_ci <- function(x, pattern) {
  grepl(pattern, x, ignore.case = TRUE, perl = TRUE)
}

split_patterns <- function(x) {
  if (is.na(x) || !nzchar(x)) {
    return(character())
  }

  v <- unlist(strsplit(x, "\\|"))
  v <- trimws(v)
  v[nzchar(v)]
}

count_hits <- function(text, patterns) {
  patterns <- split_patterns(patterns)

  if (length(patterns) == 0L) {
    return(integer(length(text)))
  }

  hits <- lapply(
    patterns,
    function(p) {
      grepl(p, text, ignore.case = TRUE, perl = TRUE)
    }
  )

  rowSums(do.call(cbind, hits), na.rm = TRUE)
}

bool_hit <- function(text, pattern) {
  if (is.na(pattern) || !nzchar(pattern)) {
    return(rep(FALSE, length(text)))
  }

  grepl(pattern, text, ignore.case = TRUE, perl = TRUE)
}

module_prefix_hit <- function(relative_path, source_module) {
  if (is.na(source_module) || !nzchar(source_module)) {
    return(rep(FALSE, length(relative_path)))
  }

  pattern <- paste0("(^|/)", source_module, "([^0-9A-Za-z]|_|$)")
  grepl(pattern, relative_path, ignore.case = TRUE, perl = TRUE)
}

panel_required_positive_regex <- function(item_id, panel_title, source_module) {

  map <- c(
    F1B = "(GSE178265|GSE132758|GSE204796).*(cluster|UMAP)|(cluster|UMAP).*(GSE178265|GSE132758|GSE204796)",
    F1E = "priority.*index|index.*priority",

    F2A = "signature.*heatmap|heatmap.*signature|candidate.*state",
    F2B = "volcano|DEG",
    F2C = "GO|gene.*ontology|ontology.*gene",
    F2D = "KEGG",
    F2E = "Hallmark|GSEA",

    F3A = "leakage|circularity|feature.*audit|audit.*feature",
    F3B = "internal|cross.*validation|CV|ROC|AUC|performance",
    F3C = "LODO|leave.*one|leave_one|leave-one",
    F3D = "feature.*importance|importance.*feature",
    F3E = "threshold|stability|setting",

    F4A = "negative|control|null|real.*vs.*negative",
    F4B = "empirical|pvalue|p_value|significance",
    F4C = "priority.*index|index.*priority",
    F4D = "signature.*heatmap|heatmap.*signature|cluster.*signature",
    F4E = "scatter|random.*forest|RF|priority",

    F5A = "marker.*targeted|targeted.*import|import.*summary|marker.*import",
    F5B = "marker.*overlap|overlap.*marker|frozen.*marker|gene.*overlap|overlap.*gene",
    F5C = "context.*signature|signature.*heatmap|heatmap.*signature|context.*heatmap",
    F5D = "predictor.*probab|probab.*predictor|frozen.*probab",
    F5E = "priority.*index|index.*priority|context.*priority",

    S1A = "cluster|UMAP|seurat",
    S1B = "annotation|conservative|UMAP",
    S2A = "DA.*score|score.*DA",
    S2B = "projection|competence",
    S3A = "safety|risk",
    S3B = "A9|A10",
    S4A = "marker.*genes|genes.*marker",
    S4B = "signature|interpretation|heatmap",
    S5A = "volcano|DEG|testing",
    S6A = "GO|gene.*ontology|ontology.*gene",
    S6B = "KEGG",
    S7A = "Hallmark|GSEA",
    S8A = "class.*balance|feature.*audit|leakage",
    S8B = "ROC|AUC|performance|dataset.*performance",
    S9A = "threshold|grid|stability|setting",
    S9B = "negative|control|null|distribution|delta.*AUC",
    S10A = "external|overlap|diagnostic|cluster|GSE183248",
    S10B = "GSE243639|marker|cluster|overlap|diagnostic"
  )

  if (item_id %in% names(map)) {
    return(unname(map[[item_id]]))
  }

  NA_character_
}

BUNDLED_IMPORT_AUDIT <- data.frame(
  source = character(),
  destination = character(),
  copied = logical(),
  stringsAsFactors = FALSE
)

stamp("V13 supplement de-dup local module paths：不使用 ZIP；直接读取本地 04_figures 模块文件夹；MIN_PDF_SIZE_BYTES=1000。")

module_folder_hit <- function(relative_path, module_id) {
  if (is.na(module_id) || !nzchar(module_id)) {
    return(rep(FALSE, length(relative_path)))
  }

  grepl(
    paste0("(^|/)", module_id, "([^0-9A-Za-z]|_|$)"),
    relative_path,
    ignore.case = TRUE,
    perl = TRUE
  )
}

v13_explicit_source_patterns <- data.frame(
  item_id = c(
    "F1E",
    "F3B",
    "F3C",
    "S8B",
    "S2B",
    "S10B"
  ),
  expected_module = c(
    "09A",
    "09C",
    "09C",
    "09C",
    "08A",
    "09I"
  ),
  required_relative_folder_regex = c(
    "09A_scRNA_cell_state_proportion_final_V6_pdf",
    "09C_primary_reduced_feature_weak_label_ML_V4_FULL_PUBLICATION_LAYOUT_pdf",
    "09C_primary_reduced_feature_weak_label_ML_V4_FULL_PUBLICATION_LAYOUT_pdf",
    "09C_primary_reduced_feature_weak_label_ML_V4_FULL_PUBLICATION_LAYOUT_pdf",
    "08A|V19",
    "09I|V9|disease_context|GSE243639"
  ),
  preferred_basename_regex = c(
    "^09A_dataset_priority_index_barplot\\.pdf$",
    "^09C_internal_CV_AUC_summary_PUBLICATION_LAYOUT\\.pdf$",
    "^09C_LODO_AUC_summary_PUBLICATION_LAYOUT\\.pdf$",
    "^09C_predicted_probability_distribution_PUBLICATION_LAYOUT\\.pdf$",

    "^GSE204796.*V19_UMAP_score_projection_competence_composite_score(\\(1\\))?\\.pdf$",

    "^09I.*(cluster.*size|size.*barplot|marker.*coverage|coverage.*summary|ML.*alignment|alignment.*summary|prediction.*summary|diagnostic).*\\.pdf$"
  ),
  fallback_basename_regex = c(
    "09A.*priority.*index.*\\.pdf$|09A.*favorable.*minus.*safety.*\\.pdf$",
    "09C.*internal.*CV.*AUC.*\\.pdf$|09C.*cross.*validation.*AUC.*\\.pdf$",
    "09C.*LODO.*AUC.*\\.pdf$|09C.*leave.*one.*dataset.*\\.pdf$",
    "09C.*predicted.*probability.*\\.pdf$|09C.*probability.*distribution.*\\.pdf$|09C.*LODO.*AUC.*\\.pdf$|09C.*internal.*CV.*AUC.*\\.pdf$",
    "GSE204796.*projection_competence_composite_score.*\\.pdf$|GSE204796.*projection.*competence.*score.*\\.pdf$",
    "09I.*(context.*diagnostic|diagnostic|cluster.*size|marker.*coverage|ML.*alignment|prediction.*summary).*\\.pdf$|09I.*marker.*targeted.*import.*summary.*\\.pdf$"
  ),
  stringsAsFactors = FALSE
)

v14_s7a_hallmark_barplot_rule <- data.frame(
  item_id = "S7A",
  expected_module = "08E",
  required_relative_folder_regex = "08E|V4|Hallmark|GSEA",
  preferred_basename_regex = "(Hallmark.*GSEA.*barplot|GSEA.*Hallmark.*barplot|barplot.*Hallmark.*GSEA|Hallmark.*bar.*GSEA|GSEA.*barplot).*\\.pdf$",
  fallback_basename_regex = "(Hallmark.*GSEA.*barplot|GSEA.*Hallmark.*barplot|barplot.*Hallmark.*GSEA|Hallmark.*bar.*GSEA|GSEA.*barplot).*\\.pdf$",
  stringsAsFactors = FALSE
)

v13_explicit_source_patterns <- rbind(
  v13_explicit_source_patterns,
  v14_s7a_hallmark_barplot_rule
)

find_preferred_candidates <- function(plan_row) {
  rule <- v13_explicit_source_patterns[
    v13_explicit_source_patterns$item_id == plan_row$item_id,
    ,
    drop = FALSE
  ]

  if (nrow(rule) == 0L) {
    return(data.frame())
  }

  folder_hit <- bool_hit(
    inventory_ok$relative_path,
    rule$required_relative_folder_regex[[1]]
  )

  preferred_name_hit <- bool_hit(
    inventory_ok$basename,
    rule$preferred_basename_regex[[1]]
  )

  fallback_name_hit <- bool_hit(
    inventory_ok$basename,
    rule$fallback_basename_regex[[1]]
  )

  module_hit <- module_folder_hit(
    inventory_ok$relative_path,
    rule$expected_module[[1]]
  ) | bool_hit(
    inventory_ok$basename,
    paste0("^", rule$expected_module[[1]])
  )

  cand <- inventory_ok[
    (folder_hit | module_hit) &
      (preferred_name_hit | fallback_name_hit),
    ,
    drop = FALSE
  ]

  if (nrow(cand) == 0L) {
    return(data.frame())
  }

  cand$preferred_priority <- ifelse(
    bool_hit(cand$basename, rule$preferred_basename_regex[[1]]),
    1L,
    2L
  )

  cand$folder_priority <- ifelse(
    bool_hit(cand$relative_path, rule$required_relative_folder_regex[[1]]),
    1L,
    2L
  )

  if (identical(plan_row$item_id, "S2B")) {
    cand <- cand[
      !bool_hit(cand$basename, "DA_projection_competence|DA.*projection"),
      ,
      drop = FALSE
    ]

    if (nrow(cand) == 0L) {
      return(data.frame())
    }

    cand$preferred_priority <- ifelse(
      bool_hit(cand$basename, "V19_UMAP_score_projection_competence_composite_score"),
      1L,
      2L
    )
  }

  if (identical(plan_row$item_id, "S10B")) {
    cand <- cand[
      module_folder_hit(cand$relative_path, "09I") |
        bool_hit(cand$basename, "^09I"),
      ,
      drop = FALSE
    ]

    if (nrow(cand) == 0L) {
      return(data.frame())
    }

    cand$preferred_priority <- ifelse(
      bool_hit(cand$basename, "cluster.*size|size.*barplot|marker.*coverage|coverage.*summary|ML.*alignment|alignment.*summary|prediction.*summary|diagnostic"),
      1L,
      ifelse(
        bool_hit(cand$basename, "marker.*targeted.*import.*summary"),
        2L,
        3L
      )
    )
  }

  if (identical(plan_row$item_id, "S7A")) {
    cand <- cand[
      bool_hit(cand$basename, "barplot|bar_plot|bar") &
        !bool_hit(cand$basename, "dotplot|dot_plot|spotplot|spot_plot|bubble"),
      ,
      drop = FALSE
    ]

    if (nrow(cand) == 0L) {
      return(data.frame())
    }

    cand$preferred_priority <- ifelse(
      bool_hit(cand$basename, "barplot|bar_plot|bar"),
      1L,
      9L
    )
  }

  cand <- cand[
    order(
      cand$preferred_priority,
      cand$folder_priority,
      cand$page_count,
      -cand$size_bytes,
      cand$relative_path
    ),
    ,
    drop = FALSE
  ]

  cand
}

v13_target_file_diagnostic <- function(pdf_inventory) {
  target_relatives <- data.frame(
    item_id = c("F1E", "F3B", "F3C", "S8B", "S2B", "S10B"),
    expected_target_hint = c(
      "09A_scRNA_cell_state_proportion_final_V6_pdf/09A_dataset_priority_index_barplot.pdf",
      "09C_primary_reduced_feature_weak_label_ML_V4_FULL_PUBLICATION_LAYOUT_pdf/09C_internal_CV_AUC_summary_PUBLICATION_LAYOUT.pdf",
      "09C_primary_reduced_feature_weak_label_ML_V4_FULL_PUBLICATION_LAYOUT_pdf/09C_LODO_AUC_summary_PUBLICATION_LAYOUT.pdf",
      "09C_primary_reduced_feature_weak_label_ML_V4_FULL_PUBLICATION_LAYOUT_pdf/09C_predicted_probability_distribution_PUBLICATION_LAYOUT.pdf",
      "08A V19 GSE204796 UMAP_score_projection_competence_composite_score.pdf",
      "09I V9 diagnostic preferred; fallback 09I marker_targeted_import_summary allowed"
    ),
    basename_regex = c(
      "^09A_dataset_priority_index_barplot\\.pdf$",
      "^09C_internal_CV_AUC_summary_PUBLICATION_LAYOUT\\.pdf$",
      "^09C_LODO_AUC_summary_PUBLICATION_LAYOUT\\.pdf$",
      "^09C_predicted_probability_distribution_PUBLICATION_LAYOUT\\.pdf$",
      "^GSE204796.*V19_UMAP_score_projection_competence_composite_score(\\(1\\))?\\.pdf$",
      "^09I.*(cluster.*size|size.*barplot|marker.*coverage|coverage.*summary|ML.*alignment|alignment.*summary|prediction.*summary|diagnostic|marker.*targeted.*import.*summary).*\\.pdf$"
    ),
    stringsAsFactors = FALSE
  )

  rows <- list()

  for (i in seq_len(nrow(target_relatives))) {
    item <- target_relatives$item_id[[i]]
    rx <- target_relatives$basename_regex[[i]]

    hit <- pdf_inventory[
      bool_hit(pdf_inventory$basename, rx),
      ,
      drop = FALSE
    ]

    if (nrow(hit) == 0L) {
      rows[[length(rows) + 1L]] <- data.frame(
        item_id = item,
        expected_target_hint = target_relatives$expected_target_hint[[i]],
        found = FALSE,
        matched_basename = NA_character_,
        matched_relative_path = NA_character_,
        integrity_pass = NA,
        size_bytes = NA_real_,
        page_count = NA_integer_,
        sha256 = NA_character_,
        note = "Expected/optimized target PDF was not found by basename regex.",
        stringsAsFactors = FALSE
      )
    } else {
      for (j in seq_len(nrow(hit))) {
        rows[[length(rows) + 1L]] <- data.frame(
          item_id = item,
          expected_target_hint = target_relatives$expected_target_hint[[i]],
          found = TRUE,
          matched_basename = hit$basename[[j]],
          matched_relative_path = hit$relative_path[[j]],
          integrity_pass = hit$integrity_pass[[j]],
          size_bytes = hit$size_bytes[[j]],
          page_count = hit$page_count[[j]],
          sha256 = hit$sha256[[j]],
          note = "Expected/optimized target PDF was found in 04_figures scan.",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  data.table::rbindlist(rows, fill = TRUE)
}

cat("\n============================================================\n")
cat("10C V16 F5B context cluster size lock：Final figure source lock and provenance audit\n")
cat("============================================================\n\n")

stamp("审计 10A / 10B frozen inputs。")

input_paths <- c(
  INPUT_10A_MAIN_PLAN,
  INPUT_10A_SUPP_PLAN,
  INPUT_10A_STORYLINE,
  INPUT_10A_KEY_NUMBERS,
  INPUT_10A_FINAL_VERSION_MANIFEST,
  INPUT_10B_MANUSCRIPT,
  FIGURE_ROOT
)

input_audit <- data.frame(
  input = input_paths,
  exists = file.exists(input_paths) | dir.exists(input_paths),
  type = ifelse(dir.exists(input_paths), "directory", "file"),
  size_bytes = ifelse(file.exists(input_paths), file.info(input_paths)$size, NA_real_),
  sha256 = ifelse(file.exists(input_paths), vapply(input_paths, sha256_file, character(1)), NA_character_),
  stringsAsFactors = FALSE
)

atomic_write_csv(input_audit, OUT_INPUT_AUDIT)

missing_inputs <- input_audit[!input_audit$exists, , drop = FALSE]

if (nrow(missing_inputs) > 0L) {
  print(missing_inputs)
  stop("10C V16 缺少 frozen input，不能继续。")
}

manuscript_text <- safe_read_text(INPUT_10B_MANUSCRIPT)

required_10b_strings <- c(
  "159,277 cells",
  "0.64 and 0.63",
  "0.58 and 0.53",
  "279/327",
  "13 of 16",
  "4,495 cells",
  "83,484 cells",
  "do not establish graft efficacy",
  "anatomical projection"
)

manuscript_string_checks <- vapply(
  required_10b_strings,
  function(x) any(grepl(x, manuscript_text, fixed = TRUE)),
  logical(1)
)

if (any(!manuscript_string_checks)) {
  failed <- data.frame(
    required_string = required_10b_strings,
    pass = manuscript_string_checks,
    stringsAsFactors = FALSE
  )
  print(failed[!failed$pass, , drop = FALSE])
  stop("10B V7 manuscript 未通过关键数字 / claim-boundary 审计。")
}

stamp("审计 10A final-version manifest。")

version_manifest <- safe_fread(INPUT_10A_FINAL_VERSION_MANIFEST)

expected_versions <- data.frame(
  module_id = c(
    "08A", "08B", "08C", "08D1", "08D2", "08E",
    "09A", "09B", "09C", "09F", "09G", "09H", "09I", "09J"
  ),
  required_version_token = c(
    "V19", "FINAL V3", "JOURNAL", "FINAL VERIFIED V2", "KEGG FINAL", "FINAL V4",
    "V6", "V4", "V4", "V3", "V1", "V1", "V9", "V2"
  ),
  stringsAsFactors = FALSE
)

if (!all(c("module_id", "final_version") %in% names(version_manifest))) {
  stop("10A_final_version_manifest.csv 缺少 module_id/final_version 列。")
}

version_audit <- merge(
  expected_versions,
  version_manifest[
    ,
    intersect(c("module_id", "final_version", "manuscript_role"), names(version_manifest)),
    drop = FALSE
  ],
  by = "module_id",
  all.x = TRUE
)

version_audit$version_present <- !is.na(version_audit$final_version)

version_audit$version_token_match <- mapply(
  function(actual, token) {
    !is.na(actual) && grepl(token, actual, ignore.case = TRUE, fixed = TRUE)
  },
  version_audit$final_version,
  version_audit$required_version_token
)

atomic_write_csv(version_audit, OUT_VERSION_AUDIT)

if (any(!version_audit$version_present | !version_audit$version_token_match)) {
  print(version_audit[!version_audit$version_present | !version_audit$version_token_match, , drop = FALSE])
  stop("Final-version manifest 与 10A frozen versions 不一致。")
}

stamp("扫描 04_figures 下全部 PDF。")

all_pdf_paths <- list.files(
  FIGURE_ROOT,
  pattern = "\\.pdf$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

all_pdf_paths <- normalize_path(all_pdf_paths)

if (length(all_pdf_paths) == 0L) {
  stop("04_figures 中没有找到 PDF。")
}

inventory_list <- lapply(
  seq_along(all_pdf_paths),
  function(i) {
    if (i %% 25L == 0L) {
      stamp("PDF integrity：", i, "/", length(all_pdf_paths))
    }

    rec <- pdf_integrity_record(all_pdf_paths[[i]])
    rec$relative_path <- relative_to_figure_root(all_pdf_paths[[i]])
    rec$basename <- basename(all_pdf_paths[[i]])
    rec$modified_time <- as.character(file.info(all_pdf_paths[[i]])$mtime)
    rec
  }
)

pdf_inventory <- data.table::rbindlist(inventory_list, fill = TRUE)

pdf_inventory$integrity_pass <- with(
  pdf_inventory,
  exists &
    pdf_header_ok &
    pdf_readable &
    !is.na(page_count) &
    page_count >= 1L &
    !is.na(encrypted) &
    encrypted == FALSE &
    is.finite(size_bytes) &
    size_bytes >= MIN_PDF_SIZE_BYTES &
    !is.na(sha256) &
    nzchar(sha256)
)

atomic_write_csv(
  as.data.frame(pdf_inventory),
  OUT_PDF_INVENTORY
)

atomic_write_csv(
  as.data.frame(v13_target_file_diagnostic(pdf_inventory)),
  file.path(
    OUT_TABLE_DIR,
    "10C_V16_target_pdf_diagnostic_F1E_F3B_F3C_S8B_S2B_S10B.csv"
  )
)

bad_pdf <- pdf_inventory[pdf_inventory$integrity_pass != TRUE]

if (nrow(bad_pdf) > 0L) {
  stamp("警告：04_figures 中存在 ", nrow(bad_pdf), " 个未通过完整性检查的 PDF；这些文件不会被选为最终 source。")
}

inventory_ok <- pdf_inventory[pdf_inventory$integrity_pass == TRUE]

if (nrow(inventory_ok) == 0L) {
  stop("没有任何 integrity-passing PDF，不能继续。")
}

main_plan <- data.frame(
  item_type = "main",
  item_id = paste0("F", rep(1:5, each = 5), LETTERS[1:5]),
  figure_id = rep(paste0("Figure ", 1:5), each = 5),
  panel = rep(LETTERS[1:5], 5),
  panel_title = c(
    "Study workflow and frozen analysis framework",
    "Representative discovery-dataset cluster-level UMAP",
    "DA/projection-associated molecular score",
    "Safety-risk-associated transcriptional score",
    "Dataset-level priority index",

    "Candidate-state signature heatmap",
    "Ideal-like versus lower-priority DEG volcano",
    "Gene Ontology enrichment",
    "KEGG enrichment",
    "Hallmark GSEA",

    "Feature leakage and circularity audit",
    "Internal cross-validation performance",
    "Leave-one-dataset-out performance",
    "Normalized feature importance",
    "Threshold-sensitivity stability",

    "Negative-control model performance",
    "Negative-control empirical significance",
    "GSE183248 external priority index",
    "GSE183248 frozen-signature heatmap",
    "GSE183248 random-forest priority scatter",

    "GSE243639 marker-targeted import summary",
    "GSE243639 frozen marker overlap",
    "GSE243639 context signature heatmap",
    "GSE243639 frozen predictor probabilities",
    "GSE243639 context priority index"
  ),
  source_module = c(
    "10A", "08A", "08A", "08A", "09A",
    "08B", "08C", "08D1", "08D2", "08E",
    "09B", "09C", "09C", "09C", "09G",
    "09H", "09H", "09F", "09F", "09F",
    "09I", "09I", "09I", "09I", "09I"
  ),
  dir_regex = c(
    "(^|/)10A_final_manuscript_figure_panel_V1[^/]*/",
    "(^|/)08A[^/]*V19[^/]*/",
    "(^|/)08A[^/]*V19[^/]*/",
    "(^|/)08A[^/]*V19[^/]*/",
    "(^|/)09A[^/]*V6[^/]*/",

    "(^|/)08B[^/]*(FINAL|V3)[^/]*/",
    "(^|/)08C[^/]*JOURNAL[^/]*/",
    "(^|/)08D1[^/]*(FINAL|VERIFIED|V2)[^/]*/",
    "(^|/)08D2[^/]*KEGG[^/]*/",
    "(^|/)08E[^/]*(FINAL|V4)[^/]*/",

    "(^|/)09B[^/]*V4[^/]*/",
    "(^|/)09C[^/]*V4[^/]*/",
    "(^|/)09C[^/]*V4[^/]*/",
    "(^|/)09C[^/]*V4[^/]*/",
    "(^|/)09G[^/]*V1[^/]*/",

    "(^|/)09H[^/]*V1[^/]*/",
    "(^|/)09H[^/]*V1[^/]*/",
    "(^|/)09F[^/]*V3[^/]*/",
    "(^|/)09F[^/]*V3[^/]*/",
    "(^|/)09F[^/]*V3[^/]*/",

    "(^|/)09I[^/]*V9[^/]*/",
    "(^|/)09I[^/]*V9[^/]*/",
    "(^|/)09I[^/]*V9[^/]*/",
    "(^|/)09I[^/]*V9[^/]*/",
    "(^|/)09I[^/]*V9[^/]*/"
  ),
  file_regex = c(
    "^10A_final_story_flow_diagram\\.pdf$",
    "GSE178265.*(cluster|seurat).*UMAP.*\\.pdf$|GSE178265.*UMAP.*cluster.*\\.pdf$",
    "GSE178265.*(DA.*projection.*composite|DA_projection|projection.*composite).*\\.pdf$",
    "GSE178265.*(safety.*risk|risk.*score).*\\.pdf$",
    "(dataset.*priority.*index|priority.*index.*dataset).*\\.pdf$",

    "(candidate.*state.*signature.*heatmap|signature.*heatmap).*\\.pdf$",
    "(ideal.*lower.*volcano|DEG.*volcano|volcano.*ideal).*\\.pdf$",
    "(GO.*(combined|aligned|summary|bubble|dotplot)|gene.*ontology).*\\.pdf$",
    "(KEGG.*UP.*DOWN.*DOTPLOT|KEGG.*dotplot).*\\.pdf$",
    "(Hallmark.*GSEA.*(dotplot|summary)|GSEA.*Hallmark).*\\.pdf$",

    "^09B_feature_leakage_category_summary\\.pdf$|leakage.*category.*summary.*\\.pdf$",
    "(internal.*cross.*validation|internal.*CV).*\\.pdf$",
    "(leave.*one.*dataset.*out|LODO).*\\.pdf$",
    "(normalized.*feature.*importance|feature.*importance).*\\.pdf$",
    "(threshold.*stability|classification.*stability|dataset.*priority.*setting).*\\.pdf$",

    "(negative.*control.*performance|real.*null.*performance).*\\.pdf$",
    "(empirical.*test|empirical.*p|negative.*control.*significance).*\\.pdf$",
    "(external.*priority.*index|GSE183248.*priority.*index).*\\.pdf$",
    "^09F_V3_external_cluster_signature_heatmap_PUBLICATION\\.pdf$|external.*signature.*heatmap.*\\.pdf$",
    "(external.*RF.*priority.*scatter|random.*forest.*priority.*scatter).*\\.pdf$",

    "^09I_V9_marker_targeted_import_summary\\.pdf$|marker.*targeted.*import.*summary.*\\.pdf$",
    "(GSE243639.*marker.*overlap|marker.*overlap).*\\.pdf$",
    "(GSE243639.*signature.*heatmap|context.*signature.*heatmap).*\\.pdf$",
    "(GSE243639.*predictor.*probabilit|frozen.*predictor.*probabilit).*\\.pdf$",
    "(GSE243639.*priority.*index|context.*priority.*index).*\\.pdf$"
  ),
  exclude_regex = c(
    "claim_boundary|manifest|next_step|evidence_summary",
    "annotation|class|A9|A10|safety",
    "safety|risk|A9|A10|cluster|annotation|class",
    "cluster|annotation|class|A9|A10|projection",
    "setting|threshold",

    "marker_genes|volcano|GO|KEGG|GSEA",
    "GO|KEGG|GSEA|heatmap",
    "KEGG|GSEA|method|report",
    "GO|GSEA|method|report",
    "GO|KEGG|method|report",

    "candidate|balance|training",
    "LODO|feature.*importance|ROC.*LODO",
    "internal|feature.*importance",
    "internal|LODO|performance",
    "negative|empirical",

    "empirical|pvalue|p_value",
    "performance|AUC",
    "heatmap|scatter|overlap",
    "priority|scatter|overlap",
    "heatmap|priority|overlap",

    "overlap|heatmap|probabilit|priority",
    "import|heatmap|probabilit|priority",
    "import|overlap|probabilit|priority",
    "import|overlap|heatmap|priority",
    "import|overlap|heatmap|probabilit"
  ),
  loose_module_patterns = c(
    "10A|final|story|flow|diagram",
    "08A|V19|GSE178265|cluster|seurat|umap",
    "08A|V19|DA|projection|molecular|score",
    "08A|V19|safety|risk|score",
    "09A|V6|dataset|priority|index",

    "08B|V3|FINAL|candidate|state|signature|heatmap",
    "08C|JOURNAL|ideal|lower|DEG|volcano",
    "08D1|GO|gene|ontology|enrichment",
    "08D2|KEGG|enrichment|dotplot",
    "08E|V4|Hallmark|GSEA",

    "09B|V4|leakage|circularity|feature",
    "09C|V4|internal|cross|validation|CV|AUC|ROC",
    "09C|V4|leave|one|dataset|LODO|AUC|ROC",
    "09C|V4|feature|importance",
    "09G|V1|threshold|stability|setting|priority",

    "09H|V1|negative|control|performance|AUC|ROC",
    "09H|V1|empirical|test|pvalue|significance",
    "09F|V3|GSE183248|external|priority|index",
    "09F|V3|GSE183248|signature|heatmap",
    "09F|V3|GSE183248|RF|random|forest|scatter|priority",

    "09I|V9|GSE243639|marker|targeted|import|summary",
    "09I|V9|GSE243639|marker|overlap|frozen",
    "09I|V9|GSE243639|context|signature|heatmap|cluster",
    "09I|V9|GSE243639|predictor|probability|probabilities|frozen",
    "09I|V9|GSE243639|context|priority|index"
  ),
  loose_keyword_patterns = c(
    "story|flow|diagram|workflow",
    "GSE178265|cluster|seurat|umap",
    "DA|projection|composite|molecular|score",
    "safety|risk|tumor|score",
    "dataset|priority|index",

    "candidate|state|signature|heatmap",
    "ideal|lower|priority|DEG|volcano",
    "GO|gene|ontology|enrichment|dotplot|bubble",
    "KEGG|enrichment|dotplot",
    "Hallmark|GSEA|dotplot|summary",

    "leakage|circularity|feature|audit|category",
    "internal|cross|validation|CV|ROC|AUC|performance",
    "leave|one|dataset|out|LODO|ROC|AUC|performance",
    "feature|importance|normalized",
    "threshold|stability|setting|priority",

    "negative|control|null|performance|ROC|AUC",
    "empirical|test|pvalue|p_value|significance",
    "GSE183248|external|priority|index",
    "GSE183248|external|signature|heatmap|cluster",
    "GSE183248|external|RF|random|forest|scatter|priority",

    "GSE243639|marker|targeted|import|summary",
    "GSE243639|marker|overlap|frozen",
    "GSE243639|context|signature|heatmap|cluster",
    "GSE243639|frozen|predictor|probability|probabilities",
    "GSE243639|context|priority|index"
  ),
  loose_negative_patterns = c(
    "claim_boundary|manifest|next_step",
    "annotation|class|safety|risk|A9|A10|projection",
    "safety|risk|A9|A10|cluster|annotation",
    "projection|DA|cluster|annotation",
    "threshold|setting|stability",

    "volcano|GO|KEGG|GSEA|marker",
    "GO|KEGG|GSEA|heatmap",
    "KEGG|GSEA|method|report",
    "GO|GSEA|method|report",
    "GO|KEGG|method|report",

    "candidate|balance|training|ROC",
    "LODO|leave|feature|importance",
    "internal|cross|feature|importance",
    "internal|cross|LODO|performance",
    "negative|empirical|null",

    "empirical|pvalue|p_value|delta",
    "performance|AUC",
    "heatmap|scatter|overlap",
    "priority|scatter|overlap",
    "heatmap|priority|overlap",

    "overlap|heatmap|probability|priority",
    "import|heatmap|probability|priority",
    "import|overlap|probability|priority",
    "import|overlap|heatmap|priority",
    "import|overlap|heatmap|probability"
  ),
  required = TRUE,
  allow_duplicate_source = FALSE,
  claim_boundary = c(
    "Workflow summarises computational design; it is not biological evidence.",
    "Representative UMAP is a visualization and does not establish lineage or function.",
    "Projection-associated molecular competence does not prove anatomical projection.",
    "Safety-risk-associated transcriptional state does not prove tumorigenicity or clinical safety.",
    "Priority index is a descriptive transcriptomic metric, not a clinical quality score.",

    "Heatmap values support molecular interpretation only.",
    "DEG association does not establish causal cell fate.",
    "Enrichment terms are pathway associations, not functional validation.",
    "Disease-labelled KEGG terms may reflect shared mitochondrial modules.",
    "GSEA supports coordinated transcriptional programs, not graft efficacy.",

    "Leakage audit reduces circularity but cannot remove marker-rule-derived limitations.",
    "Internal CV is not external validation.",
    "LODO performance is dataset-dependent and modest.",
    "Feature importance is model-associated, not causal.",
    "Threshold stability does not prove biological truth.",

    "Negative controls assess non-random signal, not clinical validity.",
    "Empirical p-values assess separation from null models only.",
    "External classification is transcriptomic application, not validated graft safety.",
    "Heatmap uses frozen signatures and visual scaling.",
    "RF probabilities are prioritization outputs, not calibrated clinical risks.",

    "Marker-targeted import is not a full-transcriptome reconstruction.",
    "Marker overlap assesses technical applicability.",
    "Context clustering is signature-space clustering.",
    "Frozen probabilities remain marker-rule-derived model outputs.",
    "Disease-context priority index is descriptive and non-clinical."
  ),
  stringsAsFactors = FALSE
)

supp_plan <- data.frame(
  item_type = "supplementary",
  item_id = c(
    "S1A", "S1B",
    "S2A", "S2B",
    "S3A", "S3B",
    "S4A", "S4B",
    "S5A",
    "S6A", "S6B",
    "S7A",
    "S8A", "S8B",
    "S9A", "S9B",
    "S10A", "S10B"
  ),
  figure_id = c(
    rep("Supplementary Figure 1", 2),
    rep("Supplementary Figure 2", 2),
    rep("Supplementary Figure 3", 2),
    rep("Supplementary Figure 4", 2),
    "Supplementary Figure 5",
    rep("Supplementary Figure 6", 2),
    "Supplementary Figure 7",
    rep("Supplementary Figure 8", 2),
    rep("Supplementary Figure 9", 2),
    rep("Supplementary Figure 10", 2)
  ),
  panel = c(
    "A", "B",
    "A", "B",
    "A", "B",
    "A", "B",
    "A",
    "A", "B",
    "A",
    "A", "B",
    "A", "B",
    "A", "B"
  ),
  panel_title = c(
    "Complete UMAP cluster atlas",
    "Conservative annotation UMAP atlas",

    "DA-like score atlas",
    "Projection-competence score atlas",

    "Safety-risk score atlas",
    "A9-minus-A10 molecular bias atlas",

    "Candidate-state top marker genes",
    "Candidate-state signature interpretation",

    "Full DEG volcano and testing summary",

    "GO detailed enrichment",
    "KEGG detailed enrichment",

    "Hallmark GSEA detailed results",

    "ML class balance and feature audit",
    "ML ROC and dataset-wise performance",

    "Threshold-grid sensitivity",
    "Negative-control repeat distributions",

    "GSE183248 external diagnostic details",
    "GSE243639 disease-context diagnostic details"
  ),
  source_module = c(
    "08A", "08A",
    "08A", "08A",
    "08A", "08A",
    "08B", "08B",
    "08C",
    "08D1", "08D2",
    "08E",
    "09B", "09C",
    "09G", "09H",
    "09F", "09I"
  ),
  dir_regex = c(
    rep("(^|/)08A[^/]*V19[^/]*/", 6),
    rep("(^|/)08B[^/]*(FINAL|V3)[^/]*/", 2),
    "(^|/)08C[^/]*JOURNAL[^/]*/",
    "(^|/)08D1[^/]*(FINAL|VERIFIED|V2)[^/]*/",
    "(^|/)08D2[^/]*KEGG[^/]*/",
    "(^|/)08E[^/]*(FINAL|V4)[^/]*/",
    "(^|/)09B[^/]*V4[^/]*/",
    "(^|/)09C[^/]*V4[^/]*/",
    "(^|/)09G[^/]*V1[^/]*/",
    "(^|/)09H[^/]*V1[^/]*/",
    "(^|/)09F[^/]*V3[^/]*/",
    "(^|/)09I[^/]*V9[^/]*/"
  ),
  file_regex = c(
    "(cluster|seurat).*UMAP.*\\.pdf$",
    "(conservative.*annotation|04D.*annotation).*UMAP.*\\.pdf$",

    "(DA.*like.*score).*UMAP.*\\.pdf$",
    "(projection.*competence.*score).*UMAP.*\\.pdf$",

    "(safety.*risk.*score).*UMAP.*\\.pdf$",
    "(A9.*A10.*score|A9_minus_A10).*UMAP.*\\.pdf$",

    "(top.*marker.*genes|marker.*genes.*candidate).*\\.pdf$",
    "(candidate.*state.*signature|signature.*interpretation).*\\.pdf$",

    "(DEG.*volcano|volcano.*ideal).*\\.pdf$",

    "(GO.*gene.*term|GO.*aligned|GO.*bubble).*\\.pdf$",
    "(KEGG.*UP.*DOWN.*DOTPLOT|KEGG.*dotplot).*\\.pdf$",

    "(Hallmark.*GSEA.*barplot|Hallmark.*GSEA.*dotplot|GSEA.*Hallmark).*\\.pdf$",

    "(class.*balance|feature.*leakage|feature.*category).*\\.pdf$",
    "(ROC|AUC|performance).*\\.pdf$",

    "(threshold.*grid|stability.*setting|priority.*setting).*\\.pdf$",
    "(negative.*control.*distribution|null.*distribution|delta.*AUC).*\\.pdf$",

    "(external.*overlap|external.*diagnostic|external.*cluster).*\\.pdf$",
    "(marker.*targeted.*import|GSE243639.*overlap|GSE243639.*cluster).*\\.pdf$"
  ),
  exclude_regex = c(
    "annotation|class|score|A9|A10",
    "cluster|class|score|A9|A10",

    "projection|composite|safety|A9|A10",
    "DA.*like|composite|safety|A9|A10",

    "DA.*like|projection|A9|A10",
    "DA.*like|projection|safety",

    "heatmap|volcano|GO|KEGG|GSEA",
    "marker.*genes|volcano|GO|KEGG|GSEA",

    "GO|KEGG|GSEA|heatmap",

    "KEGG|GSEA",
    "GO|GSEA",

    "GO|KEGG",

    "ROC|performance",
    "feature.*leakage|class.*balance",

    "negative|empirical",
    "threshold|setting",

    "priority.*index",
    "priority.*index"
  ),
  loose_module_patterns = c(
    "08A|V19|cluster|seurat|umap",
    "08A|V19|conservative|annotation|umap",

    "08A|V19|DA|like|score|umap",
    "08A|V19|projection|competence|score|umap",

    "08A|V19|safety|risk|score|umap",
    "08A|V19|A9|A10|score|bias|umap",

    "08B|V3|FINAL|marker|genes|candidate",
    "08B|V3|FINAL|candidate|state|signature|interpretation",

    "08C|JOURNAL|DEG|volcano|testing",

    "08D1|GO|detailed|enrichment",
    "08D2|KEGG|detailed|enrichment",

    "08E|V4|Hallmark|GSEA",

    "09B|V4|class|balance|feature|audit|leakage",
    "09C|V4|ROC|AUC|performance|dataset",

    "09G|V1|threshold|grid|stability",
    "09H|V1|negative|control|null|distribution",

    "09F|V3|GSE183248|external|diagnostic|overlap|cluster",
    "09I|V9|GSE243639|disease|context|diagnostic|marker|cluster|overlap"
  ),
  loose_keyword_patterns = c(
    "cluster|seurat|umap",
    "conservative|annotation|umap",

    "DA|like|score|umap",
    "projection|competence|score|umap",

    "safety|risk|score|umap",
    "A9|A10|score|bias|umap",

    "top|marker|genes|candidate",
    "candidate|state|signature|interpretation|heatmap",

    "DEG|volcano|testing|summary",

    "GO|detailed|enrichment|gene|ontology",
    "KEGG|detailed|enrichment|dotplot",

    "Hallmark|GSEA|detailed|barplot",

    "class|balance|feature|audit|leakage",
    "ROC|AUC|performance|dataset",

    "threshold|grid|stability|setting",
    "negative|control|null|distribution|delta|AUC",

    "GSE183248|external|diagnostic|overlap|cluster",
    "GSE243639|disease|context|diagnostic|marker|cluster|overlap"
  ),
  loose_negative_patterns = c(
    "annotation|class|score|A9|A10",
    "cluster|class|score|A9|A10",

    "projection|composite|safety|A9|A10",
    "DA.*like|composite|safety|A9|A10",

    "DA.*like|projection|A9|A10",
    "DA.*like|projection|safety",

    "heatmap|volcano|GO|KEGG|GSEA",
    "marker.*genes|volcano|GO|KEGG|GSEA",

    "GO|KEGG|GSEA|heatmap",

    "KEGG|GSEA",
    "GO|GSEA",

    "GO|KEGG",

    "ROC|performance",
    "feature.*leakage|class.*balance",

    "negative|empirical",
    "threshold|setting",

    "priority.*index",
    "priority.*index"
  ),
  required = TRUE,
  allow_duplicate_source = FALSE,
  claim_boundary = c(
    rep("Supplementary UMAPs are visualization outputs only.", 6),
    rep("Marker/signature panels provide molecular support but not functional validation.", 2),
    "DEG results are associations under the prespecified contrast.",
    "GO enrichment is associative.",
    "KEGG enrichment is associative.",
    "GSEA reports coordinated transcriptional programs.",
    "Feature auditing reduces but does not eliminate marker-rule-derived circularity.",
    "Performance remains dataset-dependent.",
    "Threshold sensitivity assesses robustness of rule-derived labels.",
    "Negative controls assess departure from null structure.",
    "External application is not clinical validation.",
    "Disease-context application is marker-targeted and non-clinical."
  ),
  stringsAsFactors = FALSE
)

all_plan <- rbind(main_plan, supp_plan)

stamp("开始 strict matching；失败后自动 loose rescue。")

find_strict_candidates <- function(plan_row) {
  dir_hit <- grepl(
    plan_row$dir_regex,
    inventory_ok$relative_path,
    ignore.case = TRUE,
    perl = TRUE
  )

  file_hit <- grepl(
    plan_row$file_regex,
    inventory_ok$basename,
    ignore.case = TRUE,
    perl = TRUE
  )

  exclude_hit <- if (is.na(plan_row$exclude_regex) || !nzchar(plan_row$exclude_regex)) {
    rep(FALSE, nrow(inventory_ok))
  } else {
    grepl(
      plan_row$exclude_regex,
      inventory_ok$basename,
      ignore.case = TRUE,
      perl = TRUE
    )
  }

  candidates <- inventory_ok[dir_hit & file_hit & !exclude_hit, , drop = FALSE]
  candidates <- candidates[order(candidates$relative_path, candidates$basename), , drop = FALSE]
  candidates
}

strict_candidate_rows <- list()

for (i in seq_len(nrow(all_plan))) {
  row <- all_plan[i, , drop = FALSE]
  cand <- find_strict_candidates(row)

  if (nrow(cand) == 0L) {
    strict_candidate_rows[[length(strict_candidate_rows) + 1L]] <- data.frame(
      item_type = row$item_type,
      item_id = row$item_id,
      figure_id = row$figure_id,
      panel = row$panel,
      panel_title = row$panel_title,
      source_module = row$source_module,
      strict_candidate_rank = NA_integer_,
      strict_candidate_path = NA_character_,
      strict_candidate_relative_path = NA_character_,
      strict_candidate_basename = NA_character_,
      strict_candidate_page_count = NA_integer_,
      strict_candidate_size_bytes = NA_real_,
      strict_candidate_sha256 = NA_character_,
      stringsAsFactors = FALSE
    )
  } else {
    for (j in seq_len(nrow(cand))) {
      strict_candidate_rows[[length(strict_candidate_rows) + 1L]] <- data.frame(
        item_type = row$item_type,
        item_id = row$item_id,
        figure_id = row$figure_id,
        panel = row$panel,
        panel_title = row$panel_title,
        source_module = row$source_module,
        strict_candidate_rank = j,
        strict_candidate_path = cand$path[[j]],
        strict_candidate_relative_path = cand$relative_path[[j]],
        strict_candidate_basename = cand$basename[[j]],
        strict_candidate_page_count = cand$page_count[[j]],
        strict_candidate_size_bytes = cand$size_bytes[[j]],
        strict_candidate_sha256 = cand$sha256[[j]],
        stringsAsFactors = FALSE
      )
    }
  }
}

strict_candidate_audit <- data.table::rbindlist(strict_candidate_rows, fill = TRUE)
atomic_write_csv(as.data.frame(strict_candidate_audit), OUT_STRICT_CANDIDATES)

score_loose_candidates <- function(plan_row) {
  combined_text <- paste(inventory_ok$relative_path, inventory_ok$basename, sep = " / ")

  source_module_hit <- bool_hit(combined_text, plan_row$source_module)
  module_pattern_hit <- bool_hit(combined_text, plan_row$loose_module_patterns)

  keyword_count <- count_hits(combined_text, plan_row$loose_keyword_patterns)
  basename_keyword_count <- count_hits(inventory_ok$basename, plan_row$loose_keyword_patterns)

  negative_count <- count_hits(combined_text, plan_row$loose_negative_patterns)
  basename_negative_count <- count_hits(inventory_ok$basename, plan_row$loose_negative_patterns)

  version_bonus <- ifelse(
    bool_hit(combined_text, "V19|FINAL|V3|V4|V6|V9|V1|JOURNAL|VERIFIED|KEGG"),
    6,
    0
  )

  page_bonus <- ifelse(
    inventory_ok$page_count == 1L,
    8,
    ifelse(inventory_ok$page_count <= 3L, 3, 0)
  )

  score <- 0
  score <- score + ifelse(source_module_hit, 35, 0)
  score <- score + ifelse(module_pattern_hit, 24, 0)
  score <- score + keyword_count * 5
  score <- score + basename_keyword_count * 11
  score <- score + version_bonus
  score <- score + page_bonus
  score <- score - negative_count * 2
  score <- score - basename_negative_count * 5

  module_locked <- if (REQUIRE_MODULE_PREFIX_FOR_AUTO_RESCUE) {

    module_prefix_hit(inventory_ok$relative_path, plan_row$source_module) |
      bool_hit(inventory_ok$basename, paste0("^", plan_row$source_module, "([^0-9A-Za-z]|_|$)")) |
      (
        plan_row$source_module == "08A" &
          bool_hit(inventory_ok$basename, "V19") &
          bool_hit(inventory_ok$basename, "UMAP")
      ) |
      (
        plan_row$source_module == "10A" &
          bool_hit(inventory_ok$basename, "^10A")
      )
  } else {
    rep(TRUE, nrow(inventory_ok))
  }

  required_positive_regex <- panel_required_positive_regex(
    item_id = plan_row$item_id,
    panel_title = plan_row$panel_title,
    source_module = plan_row$source_module
  )

  required_positive_hit <- if (
    REQUIRE_PANEL_SPECIFIC_POSITIVE_PATTERN &&
      !is.na(required_positive_regex) &&
      nzchar(required_positive_regex)
  ) {
    bool_hit(combined_text, required_positive_regex)
  } else {
    rep(TRUE, nrow(inventory_ok))
  }

  keep <- (
    score >= 25 |
      source_module_hit |
      basename_keyword_count >= 2
  ) &
    module_locked &
    required_positive_hit

  cand <- inventory_ok[keep, , drop = FALSE]

  if (nrow(cand) == 0L) {
    return(data.frame())
  }

  cand$rescue_score <- score[keep]
  cand$source_module_hit <- source_module_hit[keep]
  cand$module_pattern_hit <- module_pattern_hit[keep]
  cand$module_locked <- module_locked[keep]
  cand$required_positive_regex <- required_positive_regex
  cand$required_positive_hit <- required_positive_hit[keep]
  cand$keyword_count <- keyword_count[keep]
  cand$basename_keyword_count <- basename_keyword_count[keep]
  cand$negative_count <- negative_count[keep]
  cand$basename_negative_count <- basename_negative_count[keep]
  cand$version_bonus <- version_bonus[keep]
  cand$page_bonus <- page_bonus[keep]

  cand <- cand[
    order(
      -cand$rescue_score,
      -cand$basename_keyword_count,
      -cand$keyword_count,
      cand$page_count,
      -cand$size_bytes,
      cand$relative_path
    ),
    ,
    drop = FALSE
  ]

  if (nrow(cand) > 30L) {
    cand <- cand[seq_len(30L), , drop = FALSE]
  }

  cand
}

loose_candidate_rows <- list()

for (i in seq_len(nrow(all_plan))) {
  row <- all_plan[i, , drop = FALSE]
  cand <- score_loose_candidates(row)

  if (nrow(cand) == 0L) {
    loose_candidate_rows[[length(loose_candidate_rows) + 1L]] <- data.frame(
      item_type = row$item_type,
      item_id = row$item_id,
      figure_id = row$figure_id,
      panel = row$panel,
      panel_title = row$panel_title,
      source_module = row$source_module,
      rescue_rank = NA_integer_,
      rescue_score = NA_real_,
      candidate_path = NA_character_,
      candidate_relative_path = NA_character_,
      candidate_basename = NA_character_,
      page_count = NA_integer_,
      size_bytes = NA_real_,
      sha256 = NA_character_,
      source_module_hit = FALSE,
      module_pattern_hit = FALSE,
      module_locked = FALSE,
      required_positive_regex = NA_character_,
      required_positive_hit = FALSE,
      keyword_count = 0L,
      basename_keyword_count = 0L,
      negative_count = 0L,
      basename_negative_count = 0L,
      version_bonus = 0,
      page_bonus = 0,
      stringsAsFactors = FALSE
    )
  } else {
    for (j in seq_len(nrow(cand))) {
      loose_candidate_rows[[length(loose_candidate_rows) + 1L]] <- data.frame(
        item_type = row$item_type,
        item_id = row$item_id,
        figure_id = row$figure_id,
        panel = row$panel,
        panel_title = row$panel_title,
        source_module = row$source_module,
        rescue_rank = j,
        rescue_score = cand$rescue_score[[j]],
        candidate_path = cand$path[[j]],
        candidate_relative_path = cand$relative_path[[j]],
        candidate_basename = cand$basename[[j]],
        page_count = cand$page_count[[j]],
        size_bytes = cand$size_bytes[[j]],
        sha256 = cand$sha256[[j]],
        source_module_hit = cand$source_module_hit[[j]],
        module_pattern_hit = cand$module_pattern_hit[[j]],
        module_locked = cand$module_locked[[j]],
        required_positive_regex = cand$required_positive_regex[[j]],
        required_positive_hit = cand$required_positive_hit[[j]],
        keyword_count = cand$keyword_count[[j]],
        basename_keyword_count = cand$basename_keyword_count[[j]],
        negative_count = cand$negative_count[[j]],
        basename_negative_count = cand$basename_negative_count[[j]],
        version_bonus = cand$version_bonus[[j]],
        page_bonus = cand$page_bonus[[j]],
        stringsAsFactors = FALSE
      )
    }
  }
}

loose_candidate_audit <- data.table::rbindlist(
  loose_candidate_rows,
  fill = TRUE
)

atomic_write_csv(
  as.data.frame(loose_candidate_audit),
  OUT_LOOSE_CANDIDATES
)

safe_pdf_text_one <- function(path) {
  if (!file.exists(path)) {
    return("")
  }

  out <- tryCatch(
    paste(pdftools::pdf_text(path), collapse = " "),
    error = function(e) ""
  )

  out <- gsub("[\r\n\t]+", " ", out)
  out
}

is_hallmark_dot_or_spot_like <- function(path, basename_value, relative_value) {
  txt <- paste(
    basename_value,
    relative_value,
    safe_pdf_text_one(path)
  )

  bool_hit(
    txt,
    paste(
      c(
        "dotplot",
        "dot_plot",
        "spotplot",
        "spot_plot",
        "bubble",
        "Set size",
        "Gene set size"
      ),
      collapse = "|"
    )
  )
}

is_hallmark_barplot_like <- function(path, basename_value, relative_value) {
  txt <- paste(
    basename_value,
    relative_value,
    safe_pdf_text_one(path)
  )

  has_hallmark <- bool_hit(
    txt,
    "Hallmark|GSEA|gene set|NES|Normalized enrichment score"
  )

  has_bar_name <- bool_hit(
    paste(basename_value, relative_value),
    "barplot|bar_plot|bar-plot|barplot|bar_plot"
  )

  dot_like <- is_hallmark_dot_or_spot_like(
    path,
    basename_value,
    relative_value
  )

  has_hallmark &&
    !dot_like &&
    (
      has_bar_name ||
        bool_hit(txt, "Normalized enrichment score|NES")
    )
}

find_hallmark_barplot_candidates <- function() {
  combined_text <- paste(
    inventory_ok$relative_path,
    inventory_ok$basename,
    sep = " / "
  )

  module_or_name_hit <- bool_hit(
    combined_text,
    "08E|Hallmark|GSEA|MSigDB"
  )

  cand <- inventory_ok[
    module_or_name_hit,
    ,
    drop = FALSE
  ]

  if (nrow(cand) == 0L) {
    return(data.frame())
  }

  cand$hallmark_barplot_like <- vapply(
    seq_len(nrow(cand)),
    function(i) {
      is_hallmark_barplot_like(
        cand$path[[i]],
        cand$basename[[i]],
        cand$relative_path[[i]]
      )
    },
    logical(1)
  )

  cand$hallmark_dot_or_spot_like <- vapply(
    seq_len(nrow(cand)),
    function(i) {
      is_hallmark_dot_or_spot_like(
        cand$path[[i]],
        cand$basename[[i]],
        cand$relative_path[[i]]
      )
    },
    logical(1)
  )

  cand$barplot_name_priority <- ifelse(
    bool_hit(
      paste(cand$basename, cand$relative_path),
      "barplot|bar_plot|bar-plot"
    ),
    1L,
    2L
  )

  cand$module_priority <- ifelse(
    module_folder_hit(cand$relative_path, "08E") |
      bool_hit(cand$relative_path, "08E"),
    1L,
    2L
  )

  audit <- cand[
    ,
    c(
      "path",
      "relative_path",
      "basename",
      "size_bytes",
      "page_count",
      "sha256",
      "hallmark_barplot_like",
      "hallmark_dot_or_spot_like",
      "barplot_name_priority",
      "module_priority"
    ),
    drop = FALSE
  ]

  atomic_write_csv(
    as.data.frame(audit),
    file.path(
      OUT_TABLE_DIR,
      "10C_V16_hallmark_barplot_candidate_content_audit.csv"
    )
  )

  cand <- cand[
    cand$hallmark_barplot_like == TRUE &
      cand$hallmark_dot_or_spot_like != TRUE,
    ,
    drop = FALSE
  ]

  if (nrow(cand) == 0L) {
    return(data.frame())
  }

  cand <- cand[
    order(
      cand$barplot_name_priority,
      cand$module_priority,
      cand$page_count,
      -cand$size_bytes,
      cand$relative_path
    ),
    ,
    drop = FALSE
  ]

  cand
}

hallmark_barplot_candidates_V15 <- find_hallmark_barplot_candidates()

find_09i_context_cluster_size_candidates <- function() {
  combined_text <- paste(
    inventory_ok$relative_path,
    inventory_ok$basename,
    sep = " / "
  )

  cand <- inventory_ok[
    (
      module_folder_hit(inventory_ok$relative_path, "09I") |
        bool_hit(combined_text, "09I|V9|GSE243639|disease_context")
    ) &
      bool_hit(
        combined_text,
        "context.*cluster.*size|cluster.*size.*barplot|context_cluster_size|cluster_size_barplot"
      ) &
      !bool_hit(
        combined_text,
        "marker.*overlap|gene.*overlap|import.*summary|probabilit|priority|heatmap"
      ),
    ,
    drop = FALSE
  ]

  if (nrow(cand) == 0L) {
    return(data.frame())
  }

  cand$priority_score <- 0
  cand$priority_score <- cand$priority_score + ifelse(bool_hit(cand$basename, "09I"), 10, 0)
  cand$priority_score <- cand$priority_score + ifelse(bool_hit(cand$basename, "V9"), 5, 0)
  cand$priority_score <- cand$priority_score + ifelse(bool_hit(cand$basename, "context.*cluster.*size|cluster.*size"), 20, 0)
  cand$priority_score <- cand$priority_score + ifelse(bool_hit(cand$basename, "barplot"), 10, 0)

  cand <- cand[
    order(
      -cand$priority_score,
      cand$page_count,
      -cand$size_bytes,
      cand$relative_path
    ),
    ,
    drop = FALSE
  ]

  cand
}

find_09i_marker_overlap_candidates <- function() {
  combined_text <- paste(
    inventory_ok$relative_path,
    inventory_ok$basename,
    sep = " / "
  )

  cand <- inventory_ok[
    (
      module_folder_hit(inventory_ok$relative_path, "09I") |
        bool_hit(combined_text, "09I|V9|GSE243639|disease_context")
    ) &
      bool_hit(
        combined_text,
        "marker.*overlap|gene.*overlap|overlap.*fraction|frozen.*marker.*overlap|gene_overlap"
      ) &
      !bool_hit(
        combined_text,
        "cluster.*size|import.*summary|probabilit|priority"
      ),
    ,
    drop = FALSE
  ]

  if (nrow(cand) == 0L) {
    return(data.frame())
  }

  cand$priority_score <- 0
  cand$priority_score <- cand$priority_score + ifelse(bool_hit(cand$basename, "09I"), 10, 0)
  cand$priority_score <- cand$priority_score + ifelse(bool_hit(cand$basename, "V9"), 5, 0)
  cand$priority_score <- cand$priority_score + ifelse(bool_hit(cand$basename, "marker.*overlap|gene.*overlap|overlap"), 20, 0)

  cand <- cand[
    order(
      -cand$priority_score,
      cand$page_count,
      -cand$size_bytes,
      cand$relative_path
    ),
    ,
    drop = FALSE
  ]

  cand
}

f5b_context_cluster_size_candidates_V16 <- find_09i_context_cluster_size_candidates()
s10b_marker_overlap_candidates_V16 <- find_09i_marker_overlap_candidates()

v16_figure5_source_replacement_audit <- rbind(
  data.frame(
    target_item = "F5B",
    intended_source = "09I context cluster size barplot",
    candidate_rank = if (nrow(f5b_context_cluster_size_candidates_V16) > 0L) seq_len(nrow(f5b_context_cluster_size_candidates_V16)) else NA_integer_,
    candidate_path = if (nrow(f5b_context_cluster_size_candidates_V16) > 0L) f5b_context_cluster_size_candidates_V16$path else NA_character_,
    candidate_relative_path = if (nrow(f5b_context_cluster_size_candidates_V16) > 0L) f5b_context_cluster_size_candidates_V16$relative_path else NA_character_,
    candidate_basename = if (nrow(f5b_context_cluster_size_candidates_V16) > 0L) f5b_context_cluster_size_candidates_V16$basename else NA_character_,
    stringsAsFactors = FALSE
  ),
  data.frame(
    target_item = "S10B",
    intended_source = "09I marker overlap diagnostic",
    candidate_rank = if (nrow(s10b_marker_overlap_candidates_V16) > 0L) seq_len(nrow(s10b_marker_overlap_candidates_V16)) else NA_integer_,
    candidate_path = if (nrow(s10b_marker_overlap_candidates_V16) > 0L) s10b_marker_overlap_candidates_V16$path else NA_character_,
    candidate_relative_path = if (nrow(s10b_marker_overlap_candidates_V16) > 0L) s10b_marker_overlap_candidates_V16$relative_path else NA_character_,
    candidate_basename = if (nrow(s10b_marker_overlap_candidates_V16) > 0L) s10b_marker_overlap_candidates_V16$basename else NA_character_,
    stringsAsFactors = FALSE
  )
)

atomic_write_csv(
  as.data.frame(v16_figure5_source_replacement_audit),
  file.path(
    OUT_TABLE_DIR,
    "10C_V16_F5B_S10B_source_replacement_candidate_audit.csv"
  )
)

stamp("选择最终 source：strict unique 优先；否则 one-click auto-rescue top1。")

selection_rows <- list()

for (i in seq_len(nrow(all_plan))) {
  row <- all_plan[i, , drop = FALSE]
  item_id <- row$item_id[[1]]

  preferred_cand <- find_preferred_candidates(row)
  strict_cand <- find_strict_candidates(row)
  loose_cand <- score_loose_candidates(row)

  selected_path <- NA_character_
  selection_method <- NA_character_
  selection_status <- NA_character_
  selection_note <- NA_character_
  selection_confidence <- NA_character_
  selection_score <- NA_real_
  strict_candidate_count <- nrow(strict_cand)
  loose_candidate_count <- nrow(loose_cand)

  if (identical(item_id, "F5B")) {
    if (nrow(f5b_context_cluster_size_candidates_V16) >= 1L) {
      selected_path <- f5b_context_cluster_size_candidates_V16$path[[1]]
      selection_method <- "preferred_v16_f5b_context_cluster_size"
      selection_status <- "resolved"
      selection_note <- paste0(
        "V16 forced F5B to 09I/GSE243639 context cluster size barplot, replacing the previous frozen marker overlap main-panel source."
      )
      selection_confidence <- "high_preferred_v16_f5b_cluster_size"
      selection_score <- 180
    } else {
      selected_path <- NA_character_
      selection_method <- "unresolved_v16_f5b_context_cluster_size_required"
      selection_status <- "unresolved"
      selection_note <- paste0(
        "V16 requires F5B to use 09I/GSE243639 context cluster size barplot, but no matching candidate was found. ",
        "Check 10C_V16_F5B_S10B_source_replacement_candidate_audit.csv and the 09I V9 figure folder."
      )
      selection_confidence <- "none"
      selection_score <- NA_real_
    }
  } else if (identical(item_id, "S10B")) {
    if (nrow(s10b_marker_overlap_candidates_V16) >= 1L) {
      selected_path <- s10b_marker_overlap_candidates_V16$path[[1]]
      selection_method <- "preferred_v16_s10b_marker_overlap"
      selection_status <- "resolved"
      selection_note <- paste0(
        "V16 reassigned S10B to the 09I/GSE243639 marker overlap diagnostic because context cluster size is now used as main-panel F5B."
      )
      selection_confidence <- "high_preferred_v16_s10b_marker_overlap"
      selection_score <- 170
    } else {
      selected_path <- NA_character_
      selection_method <- "unresolved_v16_s10b_marker_overlap_required"
      selection_status <- "unresolved"
      selection_note <- paste0(
        "V16 prefers S10B to use 09I/GSE243639 marker overlap diagnostic, but no matching candidate was found. ",
        "Check 10C_V16_F5B_S10B_source_replacement_candidate_audit.csv and the 09I V9 figure folder."
      )
      selection_confidence <- "none"
      selection_score <- NA_real_
    }
  } else if (item_id %in% c("F2E", "S7A")) {
    if (nrow(hallmark_barplot_candidates_V15) >= 1L) {
      selected_path <- hallmark_barplot_candidates_V15$path[[1]]
      selection_method <- "preferred_content_checked_hallmark_barplot"
      selection_status <- "resolved"
      selection_note <- paste0(
        "V15 forced ",
        item_id,
        " to a content-checked Hallmark GSEA barplot. ",
        "Dotplot/spotplot/bubble-like PDFs were rejected using filename plus PDF-text checks."
      )
      selection_confidence <- "high_preferred_content_checked_barplot"
      selection_score <- 150
    } else {
      selected_path <- NA_character_
      selection_method <- "unresolved_hallmark_barplot_required"
      selection_status <- "unresolved"
      selection_note <- paste0(
        "V15 requires ",
        item_id,
        " to use a true Hallmark GSEA barplot, but no content-checked barplot candidate was found. ",
        "Open 10C_V16_hallmark_barplot_candidate_content_audit.csv and check whether 08E generated a barplot PDF."
      )
      selection_confidence <- "none"
      selection_score <- NA_real_
    }
  } else if (nrow(preferred_cand) >= 1L) {
    selected_path <- preferred_cand$path[[1]]
    selection_method <- "preferred_explicit_local_module_path"
    selection_status <- "resolved"
    selection_note <- paste0(
      "V10 explicit-local-module-paths preferred rule selected a module-folder source for ",
      item_id,
      ". It searches real PDF files inside 04_figures module directories, not ZIP bundles."
    )
    selection_confidence <- "high_preferred_exact"
    selection_score <- 120
  } else if (nrow(strict_cand) == 1L) {
    selected_path <- strict_cand$path[[1]]
    selection_method <- "strict_unique"
    selection_status <- "resolved"
    selection_note <- "Exactly one strict frozen-directory candidate was found."
    selection_confidence <- "high"
    selection_score <- 100
  } else if (AUTO_RESCUE && nrow(loose_cand) >= 1L) {
    selected_path <- loose_cand$path[[1]]
    selection_method <- ifelse(nrow(strict_cand) > 1L, "auto_rescue_from_strict_ambiguous", "auto_rescue_from_strict_missing")
    selection_status <- "resolved"
    selection_score <- loose_cand$rescue_score[[1]]

    second_score <- if (nrow(loose_cand) >= 2L) loose_cand$rescue_score[[2]] else NA_real_
    score_gap <- if (!is.na(second_score)) selection_score - second_score else NA_real_

    selection_confidence <- if (selection_score >= HIGH_CONFIDENCE_SCORE && (is.na(score_gap) || score_gap >= 10)) {
      "high_auto"
    } else if (selection_score >= LOW_CONFIDENCE_SCORE) {
      "medium_auto_review_recommended"
    } else {
      "low_auto_review_required"
    }

    selection_note <- paste0(
      "Module-safe one-click auto-rescue selected top-ranked candidate. ",
      "Score=", selection_score,
      "; loose_candidate_count=", loose_candidate_count,
      "; second_score=", ifelse(is.na(second_score), "NA", second_score),
      "; score_gap=", ifelse(is.na(score_gap), "NA", score_gap),
      ". Review 10C_V16_loose_candidate_ranking_audit.csv before final 10D assembly."
    )
  } else {
    selection_method <- "unresolved"
    selection_status <- "unresolved"
    selection_note <- "No strict or loose candidate found."
    selection_confidence <- "none"
  }

  if (identical(item_id, "F5B") && identical(selection_status, "resolved")) {
    f5b_txt <- paste(basename(selected_path), relative_to_figure_root(selected_path))
    f5b_ok <- bool_hit(f5b_txt, "context.*cluster.*size|cluster.*size.*barplot|context_cluster_size|cluster_size_barplot")
    f5b_bad <- bool_hit(f5b_txt, "marker.*overlap|gene.*overlap|import.*summary|probabilit|priority|heatmap")

    if (!f5b_ok || f5b_bad) {
      selected_path <- NA_character_
      selection_method <- "unresolved_v16_f5b_context_cluster_size_required"
      selection_status <- "unresolved"
      selection_confidence <- "none"
      selection_score <- NA_real_
      selection_note <- "V16 final veto rejected F5B because it was not a context cluster size barplot or looked like marker overlap/import/probability/priority/heatmap."
    }
  }

  if (identical(item_id, "S10B") && identical(selection_status, "resolved")) {
    s10b_txt <- paste(basename(selected_path), relative_to_figure_root(selected_path))
    s10b_ok <- bool_hit(s10b_txt, "marker.*overlap|gene.*overlap|overlap.*fraction|frozen.*marker.*overlap|gene_overlap")
    s10b_bad <- bool_hit(s10b_txt, "cluster.*size|import.*summary|probabilit|priority")

    if (!s10b_ok || s10b_bad) {
      selected_path <- NA_character_
      selection_method <- "unresolved_v16_s10b_marker_overlap_required"
      selection_status <- "unresolved"
      selection_confidence <- "none"
      selection_score <- NA_real_
      selection_note <- "V16 final veto rejected S10B because it was not a marker overlap diagnostic or looked like cluster size/import/probability/priority."
    }
  }

  if (item_id %in% c("F2E", "S7A") && identical(selection_status, "resolved")) {
    is_ok_barplot <- is_hallmark_barplot_like(
      selected_path,
      basename(selected_path),
      relative_to_figure_root(selected_path)
    )

    is_forbidden_dot <- is_hallmark_dot_or_spot_like(
      selected_path,
      basename(selected_path),
      relative_to_figure_root(selected_path)
    )

    if (!is_ok_barplot || is_forbidden_dot) {
      selected_path <- NA_character_
      selection_method <- "unresolved_hallmark_barplot_required"
      selection_status <- "unresolved"
      selection_confidence <- "none"
      selection_score <- NA_real_
      selection_note <- paste0(
        "V15 requires ",
        item_id,
        " to use a true Hallmark GSEA barplot. ",
        "The selected candidate was rejected after PDF text/content checks because it appeared dotplot/spotplot/bubble-like or not barplot-like."
      )
    }
  }

  selected_info <- if (!is.na(selected_path) && file.exists(selected_path)) {
    pdf_integrity_record(selected_path)
  } else {
    data.frame(
      path = NA_character_,
      exists = FALSE,
      size_bytes = NA_real_,
      pdf_header_ok = FALSE,
      pdf_readable = FALSE,
      page_count = NA_integer_,
      encrypted = NA,
      pdf_version = NA_character_,
      sha256 = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  page_warning <- ""

  if (selection_status == "resolved" && row$item_type == "main" && selected_info$page_count != 1L) {
    page_warning <- paste0("Main panel source has ", selected_info$page_count, " pages; review before 10D.")
    selection_note <- paste(selection_note, page_warning)
  }

  selection_rows[[length(selection_rows) + 1L]] <- data.frame(
    item_type = row$item_type,
    item_id = row$item_id,
    figure_id = row$figure_id,
    panel = row$panel,
    panel_title = row$panel_title,
    source_module = row$source_module,
    required = row$required,
    strict_candidate_count = strict_candidate_count,
    loose_candidate_count = loose_candidate_count,
    selection_method = selection_method,
    selection_status = selection_status,
    selection_confidence = selection_confidence,
    selection_score = selection_score,
    selection_note = selection_note,
    page_warning = page_warning,
    source_path = selected_path,
    source_relative_path = if (!is.na(selected_path)) relative_to_figure_root(selected_path) else NA_character_,
    source_basename = if (!is.na(selected_path)) basename(selected_path) else NA_character_,
    source_size_bytes = selected_info$size_bytes,
    source_page_count = selected_info$page_count,
    source_sha256 = selected_info$sha256,
    allow_duplicate_source = row$allow_duplicate_source,
    claim_boundary = row$claim_boundary,
    stringsAsFactors = FALSE
  )
}

selection_manifest <- data.table::rbindlist(selection_rows, fill = TRUE)

unresolved <- selection_manifest[
  selection_manifest$required == TRUE & selection_manifest$selection_status != "resolved",
  ,
  drop = FALSE
]

atomic_write_csv(
  if (nrow(unresolved) > 0L) as.data.frame(unresolved) else data.frame(status = "all_required_sources_resolved", stringsAsFactors = FALSE),
  OUT_UNRESOLVED
)

local_module_pdf_diagnostic <- function() {
  target_modules <- c("09A", "09C", "09I")
  rows <- list()

  for (m in target_modules) {
    hit <- module_folder_hit(
      pdf_inventory$relative_path,
      m
    ) | bool_hit(
      pdf_inventory$basename,
      paste0("^", m)
    )

    df <- pdf_inventory[
      hit,
      ,
      drop = FALSE
    ]

    if (nrow(df) == 0L) {
      rows[[length(rows) + 1L]] <- data.frame(
        module = m,
        relative_path = NA_character_,
        basename = NA_character_,
        integrity_pass = NA,
        page_count = NA_integer_,
        size_bytes = NA_real_,
        note = "No PDF found for this module under 04_figures.",
        stringsAsFactors = FALSE
      )
    } else {
      for (i in seq_len(nrow(df))) {
        rows[[length(rows) + 1L]] <- data.frame(
          module = m,
          relative_path = df$relative_path[[i]],
          basename = df$basename[[i]],
          integrity_pass = df$integrity_pass[[i]],
          page_count = df$page_count[[i]],
          size_bytes = df$size_bytes[[i]],
          note = "Detected local module PDF.",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  data.table::rbindlist(
    rows,
    fill = TRUE
  )
}

OUT_LOCAL_MODULE_DIAGNOSTIC <- file.path(
  OUT_TABLE_DIR,
  "10C_V16_local_module_pdf_diagnostic_09A_09C_09I.csv"
)

atomic_write_csv(
  as.data.frame(local_module_pdf_diagnostic()),
  OUT_LOCAL_MODULE_DIAGNOSTIC
)

if (STOP_IF_NO_CANDIDATE && nrow(unresolved) > 0L) {
  atomic_write_csv(
    as.data.frame(selection_manifest[selection_manifest$item_type == "main", , drop = FALSE]),
    OUT_MAIN_MANIFEST
  )
  atomic_write_csv(
    as.data.frame(selection_manifest[selection_manifest$item_type == "supplementary", , drop = FALSE]),
    OUT_SUPP_MANIFEST
  )

  print(unresolved[, c("item_id", "panel_title", "selection_status", "selection_note"), drop = FALSE])
  stop("仍有 panel 找不到任何可用 PDF。请查看 10C_V16_loose_candidate_ranking_audit.csv。")
}

resolved_manifest <- selection_manifest[selection_manifest$selection_status == "resolved", , drop = FALSE]

source_usage <- resolved_manifest[
  ,
  .(
    use_count = .N,
    used_by = paste(item_id, collapse = "; "),
    all_duplicates_allowed = all(allow_duplicate_source)
  ),
  by = source_path
]

duplicate_audit <- source_usage[source_usage$use_count > 1L, , drop = FALSE]

if (nrow(duplicate_audit) == 0L) {
  duplicate_audit <- data.frame(
    source_path = character(),
    use_count = integer(),
    used_by = character(),
    all_duplicates_allowed = logical(),
    stringsAsFactors = FALSE
  )
}

atomic_write_csv(as.data.frame(duplicate_audit), OUT_DUPLICATE_AUDIT)

illegal_duplicates <- duplicate_audit[duplicate_audit$all_duplicates_allowed != TRUE, , drop = FALSE]

if (nrow(illegal_duplicates) > 0L) {
  stamp("警告：存在重复使用同一 PDF 的情况，已写入 duplicate audit。")
}

stamp("复制已锁定图源并进行 SHA-256 复核。")

copy_audit_rows <- list()
copied_paths <- character(nrow(selection_manifest))

for (i in seq_len(nrow(selection_manifest))) {
  row <- selection_manifest[i, , drop = FALSE]

  if (row$selection_status != "resolved") {
    copied_paths[[i]] <- NA_character_
    next
  }

  target_dir <- if (row$item_type == "main") OUT_MAIN_DIR else OUT_SUPP_DIR

  target_filename <- paste0(
    row$item_id,
    "__",
    sanitize_filename(row$panel_title),
    "__SOURCE.pdf"
  )

  destination <- file.path(target_dir, target_filename)

  audit_row <- copy_with_hash_check(
    source = row$source_path,
    destination = destination
  )

  audit_row$item_type <- row$item_type
  audit_row$item_id <- row$item_id
  audit_row$figure_id <- row$figure_id
  audit_row$panel <- row$panel
  audit_row$panel_title <- row$panel_title

  copy_audit_rows[[length(copy_audit_rows) + 1L]] <- audit_row

  copied_paths[[i]] <- normalize_path(destination)
}

copy_audit <- data.table::rbindlist(copy_audit_rows, fill = TRUE)
selection_manifest$copied_path <- copied_paths

atomic_write_csv(as.data.frame(copy_audit), OUT_COPY_AUDIT)

bad_copy <- copy_audit[
  copy_audit$copied != TRUE |
    copy_audit$hash_match != TRUE |
    copy_audit$source_size_bytes != copy_audit$destination_size_bytes,
  ,
  drop = FALSE
]

if (nrow(bad_copy) > 0L) {
  print(bad_copy)
  stop("图源复制或 SHA-256 校验失败。")
}

main_manifest <- selection_manifest[selection_manifest$item_type == "main", , drop = FALSE]
supp_manifest <- selection_manifest[selection_manifest$item_type == "supplementary", , drop = FALSE]

atomic_write_csv(as.data.frame(main_manifest), OUT_MAIN_MANIFEST)
atomic_write_csv(as.data.frame(supp_manifest), OUT_SUPP_MANIFEST)

panel_mapping <- selection_manifest[
  ,
  .(
    manuscript_location = paste0(figure_id, panel),
    item_type,
    item_id,
    figure_id,
    panel,
    panel_title,
    source_module,
    source_path,
    copied_path,
    source_sha256,
    source_page_count,
    selection_method,
    selection_confidence,
    selection_score,
    selection_status,
    claim_boundary
  )
]

atomic_write_csv(as.data.frame(panel_mapping), OUT_PANEL_MAPPING)

selection_summary <- selection_manifest[
  ,
  .(
    item_type,
    item_id,
    figure_id,
    panel,
    panel_title,
    source_module,
    source_basename,
    source_page_count,
    selection_method,
    selection_confidence,
    selection_score,
    strict_candidate_count,
    loose_candidate_count,
    page_warning,
    selection_note
  )
]

atomic_write_csv(as.data.frame(selection_summary), OUT_SELECTION_SUMMARY)

stamp("生成 Figure legend draft。")

legend_titles <- c(
  "Figure 1. Study design and frozen transcriptomic prioritization framework.",
  "Figure 2. Molecular programs distinguishing ideal-like and lower-priority cell states.",
  "Figure 3. Leakage-aware marker-rule-derived modelling and threshold robustness.",
  "Figure 4. Negative controls and primary external application to GSE183248.",
  "Figure 5. Marker-targeted disease-context application to GSE243639."
)

legend_lines <- c(
  "10C V16 final figure legends — draft from locked source files",
  "",
  "These legends describe the frozen computational outputs selected in 10C V3.",
  "Exact typography, panel order and final dimensions will be fixed in 10D.",
  "All terms such as ideal-like, safety-risk-associated, A9/A10-like and projection-associated refer to transcriptomic or molecular states; they do not establish clinical safety, anatomical projection, graft efficacy or functional host integration.",
  ""
)

for (fig_num in 1:5) {
  figure_name <- paste0("Figure ", fig_num)
  fig_rows <- main_manifest[main_manifest$figure_id == figure_name, , drop = FALSE]

  legend_lines <- c(legend_lines, legend_titles[[fig_num]])

  if (fig_num == 1L) {
    legend_lines <- c(
      legend_lines,
      "The frozen framework retained 54 single-cell objects after quality control and generated valid frozen scores for 52 objects comprising 159,277 cells."
    )
  }

  if (fig_num == 3L) {
    legend_lines <- c(
      legend_lines,
      "Leave-one-dataset-out AUCs were 0.64 and 0.63 for the ideal-like logistic and random-forest models and 0.58 and 0.53 for the safety-risk models. Across five principal threshold settings, 279 of 327 groups (85.3%) retained the same dominant class in at least four settings."
    )
  }

  if (fig_num == 4L) {
    legend_lines <- c(
      legend_lines,
      "All real-versus-null comparisons showed positive delta-AUC values, with 13 of 16 empirical tests reaching p <= 0.05. External application to GSE183248 recovered 4,495 cells and eight clusters, all classified as safety-risk-like under the frozen framework."
    )
  }

  if (fig_num == 5L) {
    legend_lines <- c(
      legend_lines,
      "Marker-targeted analysis of GSE243639 included 83,484 cells and recovered eight signature-space clusters: six ideal-like, one safety-risk-like and one mixed/uncertain."
    )
  }

  for (i in seq_len(nrow(fig_rows))) {
    legend_lines <- c(
      legend_lines,
      paste0(
        "(",
        fig_rows$panel[[i]],
        ") ",
        fig_rows$panel_title[[i]],
        ". Source: ",
        basename(fig_rows$copied_path[[i]]),
        ". ",
        fig_rows$claim_boundary[[i]]
      )
    )
  }

  legend_lines <- c(legend_lines, "")
}

legend_lines <- c(
  legend_lines,
  "Supplementary figure source map",
  ""
)

for (supp_figure in unique(supp_manifest$figure_id)) {
  rows <- supp_manifest[supp_manifest$figure_id == supp_figure, , drop = FALSE]

  legend_lines <- c(legend_lines, paste0(supp_figure, "."))

  for (i in seq_len(nrow(rows))) {
    legend_lines <- c(
      legend_lines,
      paste0(
        "(",
        rows$panel[[i]],
        ") ",
        rows$panel_title[[i]],
        ". Source: ",
        basename(rows$copied_path[[i]]),
        ". ",
        rows$claim_boundary[[i]]
      )
    )
  }

  legend_lines <- c(legend_lines, "")
}

atomic_write_text(legend_lines, OUT_LEGENDS)

assembly_brief <- c(
  "10D final multi-panel assembly brief",
  "",
  "Input:",
  normalize_path(OUT_PACKAGE_DIR),
  "",
  "Do not rerun biological analysis in 10D.",
  "10D may only resize, crop whitespace, align panels, add panel letters and standardize typography.",
  "",
  "Main-figure structure:",
  "Figure 1: study design, representative atlas and dataset prioritization.",
  "Figure 2: molecular interpretation using signatures, DEG and enrichment.",
  "Figure 3: leakage-aware ML, LODO generalization and threshold robustness.",
  "Figure 4: negative controls plus primary external GSE183248 application.",
  "Figure 5: marker-targeted disease-context GSE243639 application.",
  "",
  "Strong-journal layout rules:",
  "1. Use a single sans-serif font family consistently.",
  "2. Final printed text should generally remain at least 7–8 pt.",
  "3. Panel letters A–E must be bold and positioned identically.",
  "4. Do not alter data points, axis values, legends or statistical annotations.",
  "5. Preserve vector content where available; do not rasterize text unnecessarily.",
  "6. Avoid red–green-only contrasts; preserve color-blind interpretability.",
  "7. Keep line widths, point sizes and axis-title hierarchy consistent.",
  "8. Export vector PDF and journal-ready 600-dpi TIFF only after final dimensions are known.",
  "9. Record crop, resize or format conversion in a 10D transformation audit.",
  "10. Final panel order must match 10C_V16_manuscript_panel_mapping.csv.",
  "",
  "Auto-rescue note:",
  "10C V16 used module-safe one-click auto-rescue when strict filename matching failed.",
  "Before final 10D assembly, review medium_auto_review_recommended and low_auto_review_required rows in 10C_V16_selection_confidence_summary.csv.",
  "",
  "Hard claim boundary:",
  "No figure title or legend may state proven graft efficacy, clinical safety, anatomical projection, tumorigenicity prediction or functional host integration."
)

atomic_write_text(assembly_brief, OUT_ASSEMBLY_BRIEF)

claim_boundary_lines <- c(
  "10C V16 figure claim-boundary note",
  "",
  "Allowed framing:",
  "- transcriptomic prioritization",
  "- ideal-like molecular program",
  "- safety-risk-associated transcriptional state",
  "- A9/A10-like molecular similarity or bias",
  "- projection-associated molecular competence",
  "- marker-rule-derived prediction or prioritization probability",
  "- external transcriptomic application",
  "- marker-targeted disease-context application",
  "",
  "Prohibited or unsupported framing:",
  "- proven graft efficacy",
  "- clinically safe or unsafe graft",
  "- real anatomical projection",
  "- demonstrated host integration",
  "- proven tumorigenicity or absence of tumorigenicity",
  "- validated clinical risk probability",
  "- causal cell fate",
  "",
  "Interpretive notes:",
  "- UMAPs are visualizations.",
  "- Enrichment is associative.",
  "- Feature importance is not causality.",
  "- Internal CV is not external validation.",
  "- LODO AUCs are modest and dataset-dependent.",
  "- GSE183248 and GSE243639 applications do not constitute prospective validation."
)

atomic_write_text(claim_boundary_lines, OUT_CLAIM_BOUNDARY)

low_conf_rows <- selection_manifest[
  grepl("low", selection_manifest$selection_confidence, ignore.case = TRUE) |
    grepl("medium", selection_manifest$selection_confidence, ignore.case = TRUE),
  ,
  drop = FALSE
]

report_lines <- c(
  "10C V16 module-safe one-click final figure source lock report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  paste0("PDFs scanned: ", nrow(pdf_inventory)),
  paste0("Integrity-passing PDFs: ", sum(pdf_inventory$integrity_pass == TRUE, na.rm = TRUE)),
  paste0("Integrity-failing PDFs excluded: ", nrow(bad_pdf)),
  paste0("Main panels planned: ", nrow(main_manifest)),
  paste0("Main panels resolved: ", sum(main_manifest$selection_status == "resolved")),
  paste0("Supplementary panel sources planned: ", nrow(supp_manifest)),
  paste0("Supplementary panel sources resolved: ", sum(supp_manifest$selection_status == "resolved")),
  "",
  paste0("Preferred explicit local-path selections: ", sum(selection_manifest$selection_method == "preferred_explicit_local_module_path", na.rm = TRUE)),
  paste0("Strict unique selections: ", sum(selection_manifest$selection_method == "strict_unique", na.rm = TRUE)),
  paste0("Module-safe auto-rescue selections: ", sum(grepl("auto_rescue", selection_manifest$selection_method), na.rm = TRUE)),
  paste0("Medium/low confidence rows requiring later visual review: ", nrow(low_conf_rows)),
  paste0("Unresolved required sources: ", nrow(unresolved)),
  paste0("Duplicate source uses: ", nrow(duplicate_audit)),
  paste0("Copy/hash failures: ", nrow(bad_copy)),
  "",
  "Biological analysis rerun: NO",
  "PDF content modified: NO",
  "New biological claims generated: NO",
  "",
  "Locked source package:",
  normalize_path(OUT_PACKAGE_DIR),
  "",
  "Main review file:",
  OUT_SELECTION_SUMMARY,
  "",
  "V16 Figure 5 source-lock update:",
  "- F5B is forced to 09I/GSE243639 context cluster size barplot.",
  "- S10B is reassigned to 09I/GSE243639 marker overlap diagnostic to avoid duplicating the new F5B.",
  "",
  "V15 source-lock update:",
  "- F2E and Supplementary Figure 7 / S7A are forced to content-checked Hallmark GSEA barplot; dotplot/spotplot/bubble-like sources are forbidden.",
  "",
  "V13 de-duplication updates:",
  "- S2B is forced to pure projection competence score when available.",
  "- S10B prefers a 09I diagnostic/coverage/alignment/prediction summary when available; otherwise retains 09I marker-targeted import summary.",
  "",
  "Next step:",
  "10D_FINAL_MULTIPANEL_FIGURE_ASSEMBLY_AND_EXPORT"
)

atomic_write_text(report_lines, OUT_REPORT)

atomic_write_text(capture.output(sessionInfo()), OUT_SESSION)

required_outputs <- c(
  OUT_INPUT_AUDIT,
  OUT_VERSION_AUDIT,
  OUT_PDF_INVENTORY,
  OUT_STRICT_CANDIDATES,
  OUT_LOOSE_CANDIDATES,
  OUT_MAIN_MANIFEST,
  OUT_SUPP_MANIFEST,
  OUT_PANEL_MAPPING,
  OUT_UNRESOLVED,
  OUT_DUPLICATE_AUDIT,
  OUT_COPY_AUDIT,
  OUT_SELECTION_SUMMARY,
  OUT_LEGENDS,
  OUT_ASSEMBLY_BRIEF,
  OUT_CLAIM_BOUNDARY,
  OUT_REPORT,
  OUT_SESSION
)

verification <- data.frame(
  file = required_outputs,
  exists = file.exists(required_outputs),
  size_bytes = ifelse(file.exists(required_outputs), file.info(required_outputs)$size, NA_real_),
  sha256 = vapply(required_outputs, sha256_file, character(1)),
  stringsAsFactors = FALSE
)

atomic_write_csv(verification, OUT_VERIFICATION)

bad_outputs <- verification[
  !verification$exists |
    is.na(verification$size_bytes) |
    verification$size_bytes <= 0 |
    is.na(verification$sha256) |
    !nzchar(verification$sha256),
  ,
  drop = FALSE
]

if (nrow(bad_outputs) > 0L) {
  print(bad_outputs)
  stop("10C V16 输出验证失败。")
}

cat("\n============================================================\n")
cat("10C V16 EXPLICIT LOCAL MODULE PATHS FINAL-FIXED 运行结束\n")
cat("============================================================\n\n")

cat("PDFs scanned：", nrow(pdf_inventory), "\n", sep = "")
cat("Integrity-passing PDFs：", sum(pdf_inventory$integrity_pass == TRUE, na.rm = TRUE), "\n", sep = "")
cat("Integrity-failing PDFs excluded：", nrow(bad_pdf), "\n", sep = "")
cat("Main panels resolved：", sum(main_manifest$selection_status == "resolved"), " / ", nrow(main_manifest), "\n", sep = "")
cat("Supplementary sources resolved：", sum(supp_manifest$selection_status == "resolved"), " / ", nrow(supp_manifest), "\n", sep = "")
cat("Preferred explicit local-path selections：", sum(selection_manifest$selection_method == "preferred_explicit_local_module_path", na.rm = TRUE), "\n", sep = "")
cat("Strict unique selections：", sum(selection_manifest$selection_method == "strict_unique", na.rm = TRUE), "\n", sep = "")
cat("Module-safe auto-rescue selections：", sum(grepl("auto_rescue", selection_manifest$selection_method), na.rm = TRUE), "\n", sep = "")
cat("Medium/low confidence rows：", nrow(low_conf_rows), "\n", sep = "")
cat("Unresolved required sources：", nrow(unresolved), "\n", sep = "")
cat("Copy/hash failures：", nrow(bad_copy), "\n\n", sep = "")

cat("核心输出：\n")
cat(OUT_PACKAGE_DIR, "\n")
cat(OUT_MAIN_MANIFEST, "\n")
cat(OUT_SUPP_MANIFEST, "\n")
cat(OUT_PANEL_MAPPING, "\n")
cat(OUT_SELECTION_SUMMARY, "\n")
cat(OUT_LEGENDS, "\n")
cat(OUT_ASSEMBLY_BRIEF, "\n\n")

if (nrow(low_conf_rows) > 0L) {
  cat("⚠ 有 medium/low confidence 自动选择；10D 拼图前建议打开 selection summary 快速看一眼。\n")
  cat(OUT_SELECTION_SUMMARY, "\n\n")
}

cat("✅ 10C V16 F5B context cluster size lock 一键完整自动版完成。\n")
cat("下一步进入 10D：最终多 panel 拼图、统一版式与期刊格式导出。\n")
