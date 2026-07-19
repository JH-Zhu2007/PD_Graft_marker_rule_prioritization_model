# ============================================================
# PD_Graft_Project — 10E FINAL CONSISTENCY AUDIT V2_FAST
# ============================================================
# Purpose:
#   Final consistency audit after 10D V17 storyline layout.
#
# This script DOES NOT:
#   - rerun analysis
#   - redraw source panels
#   - regenerate figures
#   - modify 10C/10D outputs
#   - change biological claims
#
# This script DOES:
#   - verify final figure files from 10D V17
#   - verify main-figure storyline structure
#   - verify no main figure contains >3 panels
#   - verify volcano is standalone
#   - verify GO/KEGG/Hallmark are grouped
#   - inventory available 10B manuscript/legend text files
#   - audit manuscript figure references when text files are available
#   - audit claim-boundary risk phrases
#   - write action items for 10F legend/manuscript update
#
# Required previous step:
#   10D_FINAL_MULTIPANEL_FIGURE_ASSEMBLY_AND_EXPORT_V17_NO_5_PANEL_VOLCANO_SINGLE.R
#
# Output:
#   D:/PD_Graft_Project/03_tables/10E_final_consistency_audit_V2_FAST
#   D:/PD_Graft_Project/09_manuscript/10E_final_consistency_audit_V2_FAST
# ============================================================

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

# ============================================================
# 0. User paths
# ============================================================

PROJECT_ROOT <- "D:/PD_Graft_Project"

# V2_FAST:
# Set to FALSE by default to avoid slow recursive manuscript text scanning.
# Figure-package structure audit still runs fully.
ENABLE_OPTIONAL_MANUSCRIPT_TEXT_SCAN <- FALSE

# If you manually set ENABLE_OPTIONAL_MANUSCRIPT_TEXT_SCAN <- TRUE,
# this cap prevents very large legacy files from being scanned.
MAX_OPTIONAL_TEXT_FILE_SIZE_BYTES <- 2 * 1024 * 1024

INPUT_10C_TAG <- "10C_final_V16_F2E_S7A_HALLMARK_BARPLOT_F5B_CLUSTER_SIZE"
INPUT_10D_TAG <- "10D_final_multipanel_figure_assembly_V17"

OUT_TAG <- "10E_final_consistency_audit_V2_FAST"

TEN_C_ROOT <- file.path(
  PROJECT_ROOT,
  "09_manuscript",
  INPUT_10C_TAG
)

TEN_D_ROOT <- file.path(
  PROJECT_ROOT,
  "09_manuscript",
  INPUT_10D_TAG
)

TEN_D_TABLE_DIR <- file.path(
  PROJECT_ROOT,
  "03_tables",
  INPUT_10D_TAG
)

TEN_D_MAIN_DIR <- file.path(
  TEN_D_ROOT,
  "main_figures"
)

TEN_D_SUPP_DIR <- file.path(
  TEN_D_ROOT,
  "supplementary_figures"
)

OUT_TABLE_DIR <- file.path(
  PROJECT_ROOT,
  "03_tables",
  OUT_TAG
)

OUT_REPORT_DIR <- file.path(
  PROJECT_ROOT,
  "09_manuscript",
  OUT_TAG
)

dir.create(
  OUT_TABLE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  OUT_REPORT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 1. Output files
# ============================================================

OUT_EXPECTED_MAIN_PLAN <- file.path(
  OUT_TABLE_DIR,
  "10E_V2_FAST_expected_main_figure_storyline_plan.csv"
)

OUT_INPUT_EXISTENCE <- file.path(
  OUT_TABLE_DIR,
  "10E_V2_FAST_input_existence_audit.csv"
)

OUT_FIGURE_OUTPUT_AUDIT <- file.path(
  OUT_TABLE_DIR,
  "10E_V2_FAST_final_figure_output_audit.csv"
)

OUT_PANEL_LAYOUT_AUDIT <- file.path(
  OUT_TABLE_DIR,
  "10E_V2_FAST_panel_layout_audit.csv"
)

OUT_STORYLINE_RULE_AUDIT <- file.path(
  OUT_TABLE_DIR,
  "10E_V2_FAST_storyline_rule_audit.csv"
)

OUT_TEXT_INVENTORY <- file.path(
  OUT_TABLE_DIR,
  "10E_V2_FAST_manuscript_text_inventory.csv"
)

OUT_FIGURE_REFERENCE_AUDIT <- file.path(
  OUT_TABLE_DIR,
  "10E_V2_FAST_figure_reference_audit.csv"
)

OUT_CLAIM_BOUNDARY_AUDIT <- file.path(
  OUT_TABLE_DIR,
  "10E_V2_FAST_claim_boundary_keyword_audit.csv"
)

OUT_LOCKED_NUMBER_AUDIT <- file.path(
  OUT_TABLE_DIR,
  "10E_V2_FAST_locked_number_presence_audit.csv"
)

OUT_ACTION_ITEMS <- file.path(
  OUT_TABLE_DIR,
  "10E_V2_FAST_action_items_for_10F.csv"
)

OUT_REPORT <- file.path(
  OUT_REPORT_DIR,
  "10E_V2_FAST_final_consistency_audit_report.txt"
)

OUT_SESSION <- file.path(
  OUT_REPORT_DIR,
  "10E_V2_FAST_sessionInfo.txt"
)

# ============================================================
# 2. Utility functions
# ============================================================

timestamp <- function() {
  format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )
}

stamp <- function(...) {
  cat(
    "[",
    timestamp(),
    "] ",
    paste0(..., collapse = ""),
    "\n",
    sep = ""
  )
}

normalize_path <- function(x) {
  ifelse(
    is.na(x) | !nzchar(x),
    NA_character_,
    normalizePath(
      x,
      winslash = "/",
      mustWork = FALSE
    )
  )
}

sha256_file <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }

  if (requireNamespace("digest", quietly = TRUE)) {
    return(
      digest::digest(
        file = path,
        algo = "sha256"
      )
    )
  }

  NA_character_
}

bind_rows_base <- function(lst) {
  lst <- lst[
    vapply(
      lst,
      function(x) {
        is.data.frame(x) && nrow(x) > 0L
      },
      logical(1)
    )
  ]

  if (length(lst) == 0L) {
    return(data.frame())
  }

  all_names <- unique(
    unlist(
      lapply(
        lst,
        names
      )
    )
  )

  out <- lapply(
    lst,
    function(d) {
      missing_cols <- setdiff(
        all_names,
        names(d)
      )

      for (m in missing_cols) {
        d[[m]] <- NA
      }

      d[
        ,
        all_names,
        drop = FALSE
      ]
    }
  )

  do.call(
    rbind,
    out
  )
}

atomic_write_csv <- function(df, path) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  tmp <- paste0(
    path,
    ".tmp_",
    Sys.getpid()
  )

  write.csv(
    df,
    tmp,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  if (file.exists(path)) {
    unlink(path)
  }

  file.rename(
    tmp,
    path
  )
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) {
    return(data.frame())
  }

  if (requireNamespace("data.table", quietly = TRUE)) {
    return(
      as.data.frame(
        data.table::fread(
          path,
          data.table = FALSE,
          showProgress = FALSE
        )
      )
    )
  }

  read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

safe_read_text <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }

  txt <- tryCatch(
    {
      readLines(
        path,
        warn = FALSE,
        encoding = "UTF-8"
      )
    },
    error = function(e) {
      tryCatch(
        readLines(
          path,
          warn = FALSE
        ),
        error = function(e2) {
          character()
        }
      )
    }
  )

  txt
}

count_pattern <- function(text, pattern, ignore.case = TRUE) {
  if (length(text) == 0L) {
    return(0L)
  }

  x <- paste(
    text,
    collapse = "\n"
  )

  m <- gregexpr(
    pattern,
    x,
    perl = TRUE,
    ignore.case = ignore.case
  )

  if (identical(m[[1]], -1L)) {
    return(0L)
  }

  length(m[[1]])
}

extract_pattern_matches <- function(text, pattern, ignore.case = TRUE) {
  if (length(text) == 0L) {
    return(character())
  }

  x <- paste(
    text,
    collapse = "\n"
  )

  m <- gregexpr(
    pattern,
    x,
    perl = TRUE,
    ignore.case = ignore.case
  )

  if (identical(m[[1]], -1L)) {
    return(character())
  }

  unique(
    regmatches(
      x,
      m
    )[[1]]
  )
}

get_pdf_page_count <- function(path) {
  if (!file.exists(path)) {
    return(NA_integer_)
  }

  if (!requireNamespace("pdftools", quietly = TRUE)) {
    return(NA_integer_)
  }

  info <- tryCatch(
    pdftools::pdf_info(path),
    error = function(e) {
      NULL
    }
  )

  if (is.null(info)) {
    return(NA_integer_)
  }

  as.integer(info$pages)
}

status_pass_fail <- function(condition) {
  if (isTRUE(condition)) {
    "PASS"
  } else {
    "FAIL"
  }
}

# ============================================================
# 3. Expected V17 main-figure storyline plan
# ============================================================

expected_main_plan <- data.frame(
  figure_type = "main",
  figure_id = c(
    rep("Figure 1", 3),
    rep("Figure 2", 2),
    "Figure 3",
    rep("Figure 4", 3),
    rep("Figure 5", 3),
    rep("Figure 6", 2),
    rep("Figure 7", 2),
    rep("Figure 8", 3),
    rep("Figure 9", 2),
    rep("Figure 10", 3)
  ),
  panel = c(
    "A", "B", "C",
    "A", "B",
    "A",
    "A", "B", "C",
    "A", "B", "C",
    "A", "B",
    "A", "B",
    "A", "B", "C",
    "A", "B",
    "A", "B", "C"
  ),
  item_id = c(
    "F1B", "F1C", "F1D",
    "F1E", "F2A",
    "F2B",
    "F2C", "F2D", "F2E",
    "F3A", "F3B", "F3C",
    "F3D", "F3E",
    "F4A", "F4B",
    "F4C", "F4D", "F4E",
    "F5A", "F5B",
    "F5C", "F5D", "F5E"
  ),
  expected_panel_title = c(
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
    "GSE243639 disease-context cluster sizes",
    "GSE243639 context signature heatmap",
    "GSE243639 frozen predictor probabilities",
    "GSE243639 context priority index"
  ),
  storyline_block = c(
    rep("Discovery atlas and transcriptional scoring atlas", 3),
    rep("Dataset prioritization and candidate-state program", 2),
    "Differential expression volcano",
    rep("Functional enrichment evidence", 3),
    rep("Machine-learning model audit and generalization", 3),
    rep("Machine-learning feature interpretation and stability", 2),
    rep("Negative-control robustness", 2),
    rep("GSE183248 external validation", 3),
    rep("GSE243639 import and disease-context cluster landscape", 2),
    rep("GSE243639 molecular validation and priority scoring", 3)
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(
  expected_main_plan,
  OUT_EXPECTED_MAIN_PLAN
)

# ============================================================
# 4. Input existence audit
# ============================================================

stamp("Checking 10C/10D input directories and audit tables...")

expected_inputs <- data.frame(
  input_name = c(
    "10C root",
    "10D root",
    "10D main figure directory",
    "10D supplementary figure directory",
    "10D table directory",
    "10D source panel audit",
    "10D assembly audit",
    "10D figure index",
    "10D layout policy audit",
    "10D output verification"
  ),
  path = c(
    TEN_C_ROOT,
    TEN_D_ROOT,
    TEN_D_MAIN_DIR,
    TEN_D_SUPP_DIR,
    TEN_D_TABLE_DIR,
    file.path(TEN_D_TABLE_DIR, "10D_V17_source_panel_render_audit.csv"),
    file.path(TEN_D_TABLE_DIR, "10D_V17_multiplanel_assembly_audit.csv"),
    file.path(TEN_D_TABLE_DIR, "10D_V17_final_figure_file_index.csv"),
    file.path(TEN_D_TABLE_DIR, "10D_V17_layout_policy_exclusions_and_title_fixes.csv"),
    file.path(TEN_D_TABLE_DIR, "10D_V17_output_verification.csv")
  ),
  stringsAsFactors = FALSE
)

expected_inputs$exists <- file.exists(expected_inputs$path)
expected_inputs$path <- normalize_path(expected_inputs$path)
expected_inputs$status <- ifelse(
  expected_inputs$exists,
  "PASS",
  "FAIL"
)

atomic_write_csv(
  expected_inputs,
  OUT_INPUT_EXISTENCE
)

if (any(expected_inputs$status == "FAIL")) {
  print(expected_inputs[expected_inputs$status == "FAIL", , drop = FALSE])
  stop(
    "10E cannot continue because required 10D V17 outputs are missing. ",
    "Run 10D V17 first, then rerun this 10E script."
  )
}

# ============================================================
# 5. Load 10D V17 audit tables
# ============================================================

source_panel_audit <- safe_read_csv(
  file.path(TEN_D_TABLE_DIR, "10D_V17_source_panel_render_audit.csv")
)

assembly_audit <- safe_read_csv(
  file.path(TEN_D_TABLE_DIR, "10D_V17_multiplanel_assembly_audit.csv")
)

figure_index <- safe_read_csv(
  file.path(TEN_D_TABLE_DIR, "10D_V17_final_figure_file_index.csv")
)

layout_policy_audit <- safe_read_csv(
  file.path(TEN_D_TABLE_DIR, "10D_V17_layout_policy_exclusions_and_title_fixes.csv")
)

# ============================================================
# 6. Final figure output audit
# ============================================================

stamp("Auditing final figure PDF outputs...")

expected_main_files <- data.frame(
  figure_type = "main",
  figure_id = paste0("Figure ", 1:10),
  expected_pdf_path = file.path(
    TEN_D_MAIN_DIR,
    paste0(
      "Figure_",
      1:10,
      "_10D_V17_final_assembly.pdf"
    )
  ),
  stringsAsFactors = FALSE
)

expected_supp_files <- data.frame(
  figure_type = "supplementary",
  figure_id = paste0("Supplementary Figure ", 1:10),
  expected_pdf_path = file.path(
    TEN_D_SUPP_DIR,
    paste0(
      "Supplementary_Figure_",
      1:10,
      "_10D_V17_final_assembly.pdf"
    )
  ),
  stringsAsFactors = FALSE
)

expected_figure_files <- rbind(
  expected_main_files,
  expected_supp_files
)

figure_output_audit <- expected_figure_files
figure_output_audit$expected_pdf_path <- normalize_path(
  figure_output_audit$expected_pdf_path
)
figure_output_audit$pdf_exists <- file.exists(
  figure_output_audit$expected_pdf_path
)
figure_output_audit$pdf_size_bytes <- ifelse(
  figure_output_audit$pdf_exists,
  file.info(figure_output_audit$expected_pdf_path)$size,
  NA_real_
)
figure_output_audit$pdf_page_count <- vapply(
  figure_output_audit$expected_pdf_path,
  get_pdf_page_count,
  integer(1)
)
figure_output_audit$pdf_sha256 <- vapply(
  figure_output_audit$expected_pdf_path,
  sha256_file,
  character(1)
)
figure_output_audit$status <- ifelse(
  figure_output_audit$pdf_exists &
    !is.na(figure_output_audit$pdf_size_bytes) &
    figure_output_audit$pdf_size_bytes > 1000,
  "PASS",
  "FAIL"
)

atomic_write_csv(
  figure_output_audit,
  OUT_FIGURE_OUTPUT_AUDIT
)

# ============================================================
# 7. Panel layout audit
# ============================================================

stamp("Auditing panel layout and V17 storyline rules...")

main_source <- source_panel_audit[
  source_panel_audit$item_type == "main",
  ,
  drop = FALSE
]

panel_count_rows <- list()

for (fig in paste0("Figure ", 1:10)) {
  rows <- main_source[
    main_source$figure_id == fig,
    ,
    drop = FALSE
  ]

  expected_rows <- expected_main_plan[
    expected_main_plan$figure_id == fig,
    ,
    drop = FALSE
  ]

  panel_count_rows[[length(panel_count_rows) + 1L]] <- data.frame(
    figure_type = "main",
    figure_id = fig,
    expected_panel_count = nrow(expected_rows),
    observed_panel_count = nrow(rows),
    observed_item_ids = paste(rows$item_id, collapse = ";"),
    observed_panels = paste(rows$panel, collapse = ";"),
    observed_panel_titles = paste(rows$panel_title, collapse = " | "),
    max_three_panel_rule = status_pass_fail(nrow(rows) <= 3L),
    expected_count_rule = status_pass_fail(nrow(rows) == nrow(expected_rows)),
    stringsAsFactors = FALSE
  )
}

supp_source <- source_panel_audit[
  source_panel_audit$item_type == "supplementary",
  ,
  drop = FALSE
]

supp_fig_ids <- unique(supp_source$figure_id)

for (fig in supp_fig_ids) {
  rows <- supp_source[
    supp_source$figure_id == fig,
    ,
    drop = FALSE
  ]

  panel_count_rows[[length(panel_count_rows) + 1L]] <- data.frame(
    figure_type = "supplementary",
    figure_id = fig,
    expected_panel_count = NA_integer_,
    observed_panel_count = nrow(rows),
    observed_item_ids = paste(rows$item_id, collapse = ";"),
    observed_panels = paste(rows$panel, collapse = ";"),
    observed_panel_titles = paste(rows$panel_title, collapse = " | "),
    max_three_panel_rule = NA_character_,
    expected_count_rule = NA_character_,
    stringsAsFactors = FALSE
  )
}

panel_layout_audit <- bind_rows_base(panel_count_rows)

atomic_write_csv(
  panel_layout_audit,
  OUT_PANEL_LAYOUT_AUDIT
)

# ============================================================
# 8. Storyline rule audit
# ============================================================

rule_rows <- list()

add_rule <- function(rule_id, description, condition, evidence) {
  rule_rows[[length(rule_rows) + 1L]] <<- data.frame(
    rule_id = rule_id,
    description = description,
    status = status_pass_fail(condition),
    evidence = evidence,
    stringsAsFactors = FALSE
  )
}

# Rule 1: exactly 10 main figures.
main_pdf_pass <- figure_output_audit[
  figure_output_audit$figure_type == "main" &
    figure_output_audit$status == "PASS",
  ,
  drop = FALSE
]

add_rule(
  "R01_main_figure_count",
  "V17 should produce exactly 10 main-figure PDFs.",
  nrow(main_pdf_pass) == 10L,
  paste0("Detected passing main PDFs: ", nrow(main_pdf_pass))
)

# Rule 2: exactly 10 supplementary figures.
supp_pdf_pass <- figure_output_audit[
  figure_output_audit$figure_type == "supplementary" &
    figure_output_audit$status == "PASS",
  ,
  drop = FALSE
]

add_rule(
  "R02_supplementary_figure_count",
  "V17 should produce exactly 10 supplementary-figure PDFs.",
  nrow(supp_pdf_pass) == 10L,
  paste0("Detected passing supplementary PDFs: ", nrow(supp_pdf_pass))
)

# Rule 3: no main figure >3 panels.
main_panel_counts <- panel_layout_audit[
  panel_layout_audit$figure_type == "main",
  ,
  drop = FALSE
]

add_rule(
  "R03_no_main_figure_gt_3_panels",
  "No main figure should contain more than 3 panels.",
  all(main_panel_counts$observed_panel_count <= 3L),
  paste0(
    paste(
      main_panel_counts$figure_id,
      main_panel_counts$observed_panel_count,
      sep = "="
    ),
    collapse = "; "
  )
)

# Rule 4: volcano standalone.
fig3 <- main_source[
  main_source$figure_id == "Figure 3",
  ,
  drop = FALSE
]

add_rule(
  "R04_volcano_standalone",
  "The volcano plot should be standalone as Figure 3.",
  nrow(fig3) == 1L && identical(fig3$item_id[[1]], "F2B"),
  paste0(
    "Figure 3 item_ids: ",
    paste(fig3$item_id, collapse = ";")
  )
)

# Rule 5: GO/KEGG/Hallmark together.
fig4 <- main_source[
  main_source$figure_id == "Figure 4",
  ,
  drop = FALSE
]

add_rule(
  "R05_go_kegg_hallmark_grouped",
  "GO, KEGG and Hallmark should be grouped as Figure 4.",
  setequal(fig4$item_id, c("F2C", "F2D", "F2E")) &&
    nrow(fig4) == 3L,
  paste0(
    "Figure 4 item_ids: ",
    paste(fig4$item_id, collapse = ";")
  )
)

# Rule 6: workflow excluded.
add_rule(
  "R06_workflow_excluded",
  "Original workflow/framework panel F1A should be excluded from final main figures.",
  !("F1A" %in% main_source$item_id),
  paste0(
    "F1A in main_source = ",
    as.character("F1A" %in% main_source$item_id)
  )
)

# Rule 7: GSE243639 cluster-size title fixed.
f5b_row <- main_source[
  main_source$item_id == "F5B",
  ,
  drop = FALSE
]

add_rule(
  "R07_gse243639_cluster_size_title",
  "F5B should be titled as disease-context cluster sizes after 10C V16 source replacement.",
  nrow(f5b_row) == 1L &&
    grepl(
      "cluster size|cluster sizes",
      f5b_row$panel_title[[1]],
      ignore.case = TRUE
    ),
  if (nrow(f5b_row) == 1L) {
    paste0(
      f5b_row$figure_id[[1]],
      f5b_row$panel[[1]],
      ": ",
      f5b_row$panel_title[[1]]
    )
  } else {
    "F5B row not uniquely detected."
  }
)

# Rule 8: all expected item_id/panel mapping correct.
merged_plan <- merge(
  expected_main_plan[
    ,
    c("figure_id", "panel", "item_id", "expected_panel_title"),
    drop = FALSE
  ],
  main_source[
    ,
    c("figure_id", "panel", "item_id", "panel_title"),
    drop = FALSE
  ],
  by = c("figure_id", "panel", "item_id"),
  all.x = TRUE
)

add_rule(
  "R08_expected_item_panel_mapping",
  "All expected V17 item_id/panel mappings should be present.",
  all(!is.na(merged_plan$panel_title)),
  paste0(
    "Missing mappings: ",
    sum(is.na(merged_plan$panel_title))
  )
)

storyline_rule_audit <- bind_rows_base(rule_rows)

atomic_write_csv(
  storyline_rule_audit,
  OUT_STORYLINE_RULE_AUDIT
)

# ============================================================
# 9. Manuscript / legend text inventory — V2_FAST
# ============================================================

stamp("Inventorying manuscript/legend text files in V2_FAST mode...")

# V1 recursively scanned broad directories and could hang on very large text-like files.
# V2_FAST defaults to SKIPPING optional full-text scan.
# This does NOT affect figure-package consistency audit.
# Later 10F will handle specific manuscript/legend files directly.

candidate_10b <- character()
combined_text_lines <- character()
combined_file_labels <- character()

if (isTRUE(ENABLE_OPTIONAL_MANUSCRIPT_TEXT_SCAN)) {
  search_dirs <- c(
    file.path(PROJECT_ROOT, "09_manuscript"),
    file.path(PROJECT_ROOT, "03_tables")
  )

  search_dirs <- search_dirs[
    dir.exists(search_dirs)
  ]

  all_files <- unique(
    unlist(
      lapply(
        search_dirs,
        function(d) {
          list.files(
            d,
            recursive = TRUE,
            full.names = TRUE,
            all.files = FALSE
          )
        }
      )
    )
  )

  all_files <- all_files[
    file.exists(all_files)
  ]

  file_ext <- tolower(
    tools::file_ext(all_files)
  )

  candidate_text_like <- all_files[
    file_ext %in% c(
      "txt",
      "md"
    )
  ]

  candidate_10b <- candidate_text_like[
    grepl(
      "10B|manuscript|legend|caption|abstract|results|method|claim|storyline",
      candidate_text_like,
      ignore.case = TRUE
    )
  ]

  candidate_10b_info <- file.info(candidate_10b)
  candidate_10b <- candidate_10b[
    !is.na(candidate_10b_info$size) &
      candidate_10b_info$size <= MAX_OPTIONAL_TEXT_FILE_SIZE_BYTES
  ]

  if (length(candidate_10b) > 0L) {
    for (f in candidate_10b) {
      lines <- safe_read_text(f)

      if (length(lines) > 0L) {
        combined_text_lines <- c(
          combined_text_lines,
          paste0("\n\n===== FILE: ", normalize_path(f), " =====\n"),
          lines
        )

        combined_file_labels <- c(
          combined_file_labels,
          normalize_path(f)
        )
      }
    }
  }
}

text_inventory <- data.frame(
  file_path = if (length(candidate_10b) > 0L) normalize_path(candidate_10b) else NA_character_,
  file_name = if (length(candidate_10b) > 0L) basename(candidate_10b) else NA_character_,
  extension = if (length(candidate_10b) > 0L) tolower(tools::file_ext(candidate_10b)) else NA_character_,
  size_bytes = if (length(candidate_10b) > 0L && file.exists(candidate_10b)) {
    file.info(candidate_10b)$size
  } else {
    NA_real_
  },
  read_attempted = isTRUE(ENABLE_OPTIONAL_MANUSCRIPT_TEXT_SCAN),
  scan_mode = ifelse(
    isTRUE(ENABLE_OPTIONAL_MANUSCRIPT_TEXT_SCAN),
    "OPTIONAL_TARGETED_TEXT_SCAN",
    "SKIPPED_FAST_MODE"
  ),
  note = ifelse(
    isTRUE(ENABLE_OPTIONAL_MANUSCRIPT_TEXT_SCAN),
    "Optional targeted text scan enabled.",
    "Optional manuscript text scan skipped in V2_FAST to avoid slow recursive regex scanning. 10F will handle specific manuscript/legend files."
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(
  text_inventory,
  OUT_TEXT_INVENTORY
)


# ============================================================
# 10. Figure-reference audit — V2_FAST
# ============================================================

stamp("Auditing figure references in available manuscript text...")

if (!isTRUE(ENABLE_OPTIONAL_MANUSCRIPT_TEXT_SCAN)) {
  figure_reference_audit <- data.frame(
    reference_class = c(
      "main_Figure",
      "main_Fig",
      "supplementary_Figure",
      "supplementary_Fig_S"
    ),
    pattern = c(
      "\\bFigure\\s+[0-9]+\\b",
      "\\bFig\\.\\s*[0-9]+\\b",
      "\\bSupplementary\\s+Figure\\s+[0-9]+\\b",
      "\\bFig\\.\\s*S[0-9]+\\b"
    ),
    n_unique_matches = NA_integer_,
    unique_matches = NA_character_,
    text_files_scanned = 0L,
    status = "SKIPPED_FAST_MODE",
    note = "Skipped in 10E V2_FAST. 10F will scan the specific locked manuscript/legend file.",
    stringsAsFactors = FALSE
  )
} else {
  figure_ref_patterns <- data.frame(
    reference_class = c(
      "main_Figure",
      "main_Fig",
      "supplementary_Figure",
      "supplementary_Fig_S"
    ),
    pattern = c(
      "\\bFigure\\s+[0-9]+\\b",
      "\\bFig\\.\\s*[0-9]+\\b",
      "\\bSupplementary\\s+Figure\\s+[0-9]+\\b",
      "\\bFig\\.\\s*S[0-9]+\\b"
    ),
    stringsAsFactors = FALSE
  )

  fig_ref_rows <- list()

  for (i in seq_len(nrow(figure_ref_patterns))) {
    refs <- extract_pattern_matches(
      combined_text_lines,
      figure_ref_patterns$pattern[[i]],
      ignore.case = TRUE
    )

    fig_ref_rows[[length(fig_ref_rows) + 1L]] <- data.frame(
      reference_class = figure_ref_patterns$reference_class[[i]],
      pattern = figure_ref_patterns$pattern[[i]],
      n_unique_matches = length(refs),
      unique_matches = paste(refs, collapse = "; "),
      text_files_scanned = length(unique(combined_file_labels)),
      status = ifelse(
        length(combined_text_lines) == 0L,
        "NO_TEXT_FILES_FOUND",
        "SCANNED"
      ),
      note = "Optional targeted text scan enabled.",
      stringsAsFactors = FALSE
    )
  }

  figure_reference_audit <- bind_rows_base(fig_ref_rows)
}

atomic_write_csv(
  figure_reference_audit,
  OUT_FIGURE_REFERENCE_AUDIT
)


# ============================================================
# 11. Claim-boundary keyword audit — V2_FAST
# ============================================================

stamp("Auditing claim-boundary risk phrases...")

claim_keywords <- data.frame(
  boundary_class = c(
    rep("hard_block", 18),
    rep("warning", 12)
  ),
  keyword_pattern = c(
    "therapeutic\\s+efficacy",
    "clinical\\s+efficacy",
    "clinical\\s+safety",
    "predicts?\\s+clinical",
    "predicts?\\s+safety",
    "safe\\s+for\\s+transplant",
    "tumou?rigenicity",
    "tumou?r\\s+risk\\s+prediction",
    "functional\\s+integration",
    "host\\s+integration",
    "anatomical\\s+projection",
    "true\\s+projection",
    "retrograde\\s+projection",
    "clone[- ]aware",
    "lineage\\s+tracing",
    "causal\\s+evidence",
    "treats?\\s+Parkinson",
    "cures?\\s+Parkinson",
    "clinical",
    "safety\\s+prediction",
    "graft\\s+safety",
    "projection\\s+evidence",
    "validated\\s+therapy",
    "patient\\s+outcome",
    "disease\\s+modifying",
    "biomarker\\s+for\\s+patients",
    "治疗效果",
    "临床疗效",
    "安全性预测",
    "真实投射"
  ),
  suggested_handling = c(
    rep(
      "Remove or rewrite. Strong-journal claim boundary allows transcriptomic prioritization only.",
      18
    ),
    rep(
      "Review context manually. May be acceptable only if framed as transcriptomic/risk-associated prioritization, not clinical or functional validation.",
      12
    )
  ),
  stringsAsFactors = FALSE
)

if (!isTRUE(ENABLE_OPTIONAL_MANUSCRIPT_TEXT_SCAN)) {
  claim_boundary_audit <- claim_keywords
  claim_boundary_audit$n_matches <- NA_integer_
  claim_boundary_audit$status <- "SKIPPED_FAST_MODE"
  claim_boundary_audit$note <- "Skipped in 10E V2_FAST. 10F/10H will scan the specific locked manuscript file."
} else {
  claim_rows <- list()

  for (i in seq_len(nrow(claim_keywords))) {
    n <- count_pattern(
      combined_text_lines,
      claim_keywords$keyword_pattern[[i]],
      ignore.case = TRUE
    )

    claim_rows[[length(claim_rows) + 1L]] <- data.frame(
      boundary_class = claim_keywords$boundary_class[[i]],
      keyword_pattern = claim_keywords$keyword_pattern[[i]],
      n_matches = n,
      status = ifelse(
        length(combined_text_lines) == 0L,
        "NO_TEXT_FILES_FOUND",
        ifelse(
          n == 0L,
          "PASS",
          ifelse(
            claim_keywords$boundary_class[[i]] == "hard_block",
            "REWRITE_REQUIRED",
            "MANUAL_REVIEW"
          )
        )
      ),
      suggested_handling = claim_keywords$suggested_handling[[i]],
      note = "Optional targeted text scan enabled.",
      stringsAsFactors = FALSE
    )
  }

  claim_boundary_audit <- bind_rows_base(claim_rows)
}

atomic_write_csv(
  claim_boundary_audit,
  OUT_CLAIM_BOUNDARY_AUDIT
)


# ============================================================
# 12. Locked-number presence audit — V2_FAST
# ============================================================

stamp("Auditing presence of locked key numbers in available manuscript text...")

locked_numbers <- data.frame(
  number_label = c(
    "09G stable groups percentage",
    "09G stable groups fraction",
    "09H empirical tests",
    "GSE183248 cell count",
    "GSE183248 cluster count",
    "GSE183248 ideal-like count",
    "GSE183248 safety-risk-like count",
    "GSE243639 cell count"
  ),
  expected_text_pattern = c(
    "85\\.3%",
    "279\\s*/\\s*327",
    "13\\s*/\\s*16",
    "4,?495",
    "8\\s+clusters?",
    "ideal[- ]like\\s*=\\s*0|ideal[- ]like\\s+0",
    "safety[- ]risk[- ]like\\s*=\\s*8|safety[- ]risk[- ]like\\s+8",
    "83,?484"
  ),
  required_in_final_manuscript = c(
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE
  ),
  stringsAsFactors = FALSE
)

if (!isTRUE(ENABLE_OPTIONAL_MANUSCRIPT_TEXT_SCAN)) {
  locked_number_audit <- locked_numbers
  locked_number_audit$n_matches <- NA_integer_
  locked_number_audit$status <- "SKIPPED_FAST_MODE"
  locked_number_audit$note <- "Skipped in 10E V2_FAST. 10F/10H will scan the specific locked manuscript file."
} else {
  number_rows <- list()

  for (i in seq_len(nrow(locked_numbers))) {
    n <- count_pattern(
      combined_text_lines,
      locked_numbers$expected_text_pattern[[i]],
      ignore.case = TRUE
    )

    number_rows[[length(number_rows) + 1L]] <- data.frame(
      number_label = locked_numbers$number_label[[i]],
      expected_text_pattern = locked_numbers$expected_text_pattern[[i]],
      n_matches = n,
      required_in_final_manuscript = locked_numbers$required_in_final_manuscript[[i]],
      status = ifelse(
        length(combined_text_lines) == 0L,
        "NO_TEXT_FILES_FOUND",
        ifelse(
          n > 0L,
          "FOUND",
          "NOT_FOUND_CHECK_10F"
        )
      ),
      note = "Optional targeted text scan enabled.",
      stringsAsFactors = FALSE
    )
  }

  locked_number_audit <- bind_rows_base(number_rows)
}

atomic_write_csv(
  locked_number_audit,
  OUT_LOCKED_NUMBER_AUDIT
)


# ============================================================
# 13. 10F action items
# ============================================================

stamp("Writing 10F action items...")

action_rows <- list()

add_action <- function(priority, action_type, action_item, reason, blocking) {
  action_rows[[length(action_rows) + 1L]] <<- data.frame(
    priority = priority,
    action_type = action_type,
    action_item = action_item,
    reason = reason,
    blocking_for_submission = blocking,
    stringsAsFactors = FALSE
  )
}

if (any(figure_output_audit$status == "FAIL")) {
  add_action(
    "P0",
    "figure_output",
    "Re-run 10D V17 until all expected main and supplementary PDFs pass existence/size checks.",
    "10E detected missing or too-small final figure PDFs.",
    TRUE
  )
}

if (any(storyline_rule_audit$status == "FAIL")) {
  add_action(
    "P0",
    "storyline_layout",
    "Fix failed V17 storyline layout rules before proceeding to legend writing.",
    "The figure package does not yet match the locked V17 storyline design.",
    TRUE
  )
}

if (!isTRUE(ENABLE_OPTIONAL_MANUSCRIPT_TEXT_SCAN)) {
  add_action(
    "P1",
    "manuscript_text",
    "Proceed to 10F using the specific locked manuscript/legend text files instead of broad recursive scanning.",
    "10E V2_FAST intentionally skipped optional full-text scan to avoid slow regex over legacy files.",
    FALSE
  )
} else if (length(combined_text_lines) == 0L) {
  add_action(
    "P1",
    "manuscript_text",
    "Place the locked 10B manuscript/legend text files under 09_manuscript or 03_tables, then rerun optional text scan if needed.",
    "10E could verify figures but could not scan manuscript text because no readable 10B/legend/manuscript text files were found.",
    FALSE
  )
} else {
  add_action(
    "P1",
    "manuscript_references",
    "Update all manuscript in-text figure references to the V17 10-main-figure structure.",
    "V17 changed main figures from the previous compact structure to 10 storyline figures.",
    TRUE
  )
}

if (
  isTRUE(ENABLE_OPTIONAL_MANUSCRIPT_TEXT_SCAN) &&
    any(
      claim_boundary_audit$status %in% c(
        "REWRITE_REQUIRED",
        "MANUAL_REVIEW"
      )
    )
) {
  add_action(
    "P0",
    "claim_boundary",
    "Rewrite or manually review all claim-boundary keyword hits before 10F/10G.",
    "Strong-journal framing must remain transcriptomic prioritization only, not clinical efficacy, true projection, safety prediction, or functional integration.",
    TRUE
  )
}

add_action(
  "P1",
  "figure_legends",
  "Write new V17 figure legends for Figure 1–10 and align panel letters with 10E_V2_FAST_expected_main_figure_storyline_plan.csv.",
  "The final main-figure numbering and panel grouping changed in 10D V17.",
  TRUE
)

add_action(
  "P1",
  "supplementary_legends",
  "Keep Supplementary Figure 1–10 legends synchronized with the unchanged 10D V17 supplementary outputs.",
  "Main figures changed, but supplementary figure files are still exported as Supplementary Figure 1–10.",
  TRUE
)

add_action(
  "P2",
  "reviewer_defense",
  "Prepare reviewer-defense wording for why the volcano plot is standalone and enrichment is grouped as GO/KEGG/Hallmark.",
  "This layout improves readability and follows evidence-type grouping.",
  FALSE
)

action_items <- bind_rows_base(action_rows)

atomic_write_csv(
  action_items,
  OUT_ACTION_ITEMS
)

# ============================================================
# 14. Final report
# ============================================================

stamp("Writing final 10E report...")

pass_rules <- sum(storyline_rule_audit$status == "PASS")
fail_rules <- sum(storyline_rule_audit$status == "FAIL")

pass_figures <- sum(figure_output_audit$status == "PASS")
fail_figures <- sum(figure_output_audit$status == "FAIL")

claim_rewrite_required <- sum(
  claim_boundary_audit$status == "REWRITE_REQUIRED",
  na.rm = TRUE
)

claim_manual_review <- sum(
  claim_boundary_audit$status == "MANUAL_REVIEW",
  na.rm = TRUE
)

report_lines <- c(
  "============================================================",
  "PD_Graft_Project — 10E FINAL CONSISTENCY AUDIT V2_FAST",
  "============================================================",
  paste0("Run time: ", timestamp()),
  paste0("Project root: ", normalize_path(PROJECT_ROOT)),
  paste0("Input 10C: ", INPUT_10C_TAG),
  paste0("Input 10D: ", INPUT_10D_TAG),
  "",
  "Purpose:",
  "10E audits the final V17 figure package and prepares 10F legend/manuscript update.",
  "V2_FAST default: optional broad manuscript text scanning is skipped to avoid slow recursive regex scanning.",
  "No analysis rerun. No figure regeneration. No source relocking.",
  "",
  "Figure output summary:",
  paste0("- Passing figure PDFs: ", pass_figures, " / ", nrow(figure_output_audit)),
  paste0("- Failing figure PDFs: ", fail_figures, " / ", nrow(figure_output_audit)),
  "",
  "Storyline rule summary:",
  paste0("- Passing rules: ", pass_rules, " / ", nrow(storyline_rule_audit)),
  paste0("- Failing rules: ", fail_rules, " / ", nrow(storyline_rule_audit)),
  "",
  "Claim-boundary scan summary:",
  paste0("- Hard rewrite hits: ", claim_rewrite_required),
  paste0("- Manual-review hits: ", claim_manual_review),
  paste0("- Text files scanned: ", length(unique(combined_file_labels))),
  "",
  "Locked V17 main figure structure:",
  "Figure 1: Discovery atlas and transcriptional scoring atlas.",
  "Figure 2: Dataset prioritization and candidate-state program.",
  "Figure 3: Differential expression volcano standalone.",
  "Figure 4: GO / KEGG / Hallmark functional enrichment.",
  "Figure 5: Machine-learning model audit and generalization.",
  "Figure 6: Machine-learning feature interpretation and stability.",
  "Figure 7: Negative-control robustness.",
  "Figure 8: GSE183248 external validation.",
  "Figure 9: GSE243639 import and disease-context cluster landscape.",
  "Figure 10: GSE243639 molecular validation and priority scoring.",
  "",
  "Core audit tables:",
  paste0("- ", normalize_path(OUT_EXPECTED_MAIN_PLAN)),
  paste0("- ", normalize_path(OUT_INPUT_EXISTENCE)),
  paste0("- ", normalize_path(OUT_FIGURE_OUTPUT_AUDIT)),
  paste0("- ", normalize_path(OUT_PANEL_LAYOUT_AUDIT)),
  paste0("- ", normalize_path(OUT_STORYLINE_RULE_AUDIT)),
  paste0("- ", normalize_path(OUT_TEXT_INVENTORY)),
  paste0("- ", normalize_path(OUT_FIGURE_REFERENCE_AUDIT)),
  paste0("- ", normalize_path(OUT_CLAIM_BOUNDARY_AUDIT)),
  paste0("- ", normalize_path(OUT_LOCKED_NUMBER_AUDIT)),
  paste0("- ", normalize_path(OUT_ACTION_ITEMS)),
  "",
  "Interpretation:",
  if (fail_figures == 0L && fail_rules == 0L) {
    "10E V2_FAST figure-package audit passed. Proceed to 10F: legends and manuscript-reference rewrite."
  } else {
    "10E found blocking issues. Fix failed outputs/rules before 10F."
  },
  "",
  "Next step:",
  "10F should generate the updated V17 figure legends and manuscript figure-reference replacement map.",
  "",
  "============================================================"
)

writeLines(
  report_lines,
  OUT_REPORT,
  useBytes = TRUE
)

sink(OUT_SESSION)
print(sessionInfo())
sink()

# ============================================================
# 15. Console summary
# ============================================================

cat("\n============================================================\n")
cat("10E FINAL CONSISTENCY AUDIT V2_FAST 完成\n")
cat("============================================================\n\n")

cat("Input 10D:\n")
cat(normalize_path(TEN_D_ROOT), "\n\n")

cat("Output tables:\n")
cat(normalize_path(OUT_TABLE_DIR), "\n\n")

cat("Output report:\n")
cat(normalize_path(OUT_REPORT), "\n\n")

cat("Figure PDFs passing: ", pass_figures, " / ", nrow(figure_output_audit), "\n", sep = "")
cat("Storyline rules passing: ", pass_rules, " / ", nrow(storyline_rule_audit), "\n", sep = "")
cat("Claim hard rewrite hits: ", claim_rewrite_required, "\n", sep = "")
cat("Claim manual-review hits: ", claim_manual_review, "\n\n", sep = "")

if (fail_figures == 0L && fail_rules == 0L) {
  cat("✅ 10E V2_FAST 图包结构审计通过。下一步进入 10F：figure legends + 正文引用更新。\n")
} else {
  cat("⚠️ 10E V2_FAST 发现阻断问题。先查看 10E_V2_FAST_action_items_for_10F.csv。\n")
}
