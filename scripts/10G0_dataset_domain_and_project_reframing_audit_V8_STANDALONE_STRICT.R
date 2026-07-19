
options(stringsAsFactors = FALSE)

PROJECT_ROOT <- "D:/PD_Graft_Project"
TAG <- "10G0_dataset_domain_and_project_reframing_audit_V8_STANDALONE_STRICT"
OUT_TABLE_DIR <- file.path(PROJECT_ROOT, "03_tables", TAG)
OUT_TEXT_DIR  <- file.path(PROJECT_ROOT, "09_manuscript", TAG)

SCAN_FOLDERS <- c(
  file.path(PROJECT_ROOT, "03_tables"),
  file.path(PROJECT_ROOT, "04_figures"),
  file.path(PROJECT_ROOT, "05_models"),
  file.path(PROJECT_ROOT, "09_manuscript")
)

CONTENT_MAX_FILE_SIZE_MB <- 2
CONTENT_MAX_FILES        <- 600
CONTENT_MAX_LINES        <- 2500

msg <- function(...) cat(..., "\n")

make_dir <- function(x) {
  if (!dir.exists(x)) dir.create(x, recursive = TRUE, showWarnings = FALSE)
}

safe_write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
  msg("[10G0 V8] Wrote:", path)
}

safe_write_lines <- function(x, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(x, con = con, useBytes = TRUE)
  msg("[10G0 V8] Wrote:", path)
}

collapse_unique <- function(x, sep = "; ") {
  x <- unique(x[!is.na(x) & nzchar(x)])
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = sep)
}

extract_gse_strict <- function(x) {
  if (length(x) == 0) return(character(0))
  x <- toupper(as.character(x))
  hits <- unlist(regmatches(x, gregexpr("GSE[0-9]{5,7}", x, perl = TRUE)), use.names = FALSE)
  unique(hits[!is.na(hits) & nzchar(hits)])
}

safe_read_lines <- function(path, n = CONTENT_MAX_LINES) {
  out <- tryCatch(
    readLines(path, warn = FALSE, n = n, encoding = "UTF-8"),
    error = function(e) {
      tryCatch(readLines(path, warn = FALSE, n = n), error = function(e2) character(0))
    }
  )
  out
}

context_snippet <- function(lines, accession, width = 180) {
  if (length(lines) == 0) return(NA_character_)
  idx <- grep(accession, toupper(lines), fixed = TRUE)
  if (length(idx) == 0) return(NA_character_)
  picked <- lines[idx[seq_len(min(3, length(idx)))]]
  picked <- gsub("[\r\n\t]+", " ", picked)
  picked <- gsub(" +", " ", picked)
  picked <- substr(picked, 1, width)
  paste(picked, collapse = " || ")
}

contains_any <- function(x, patterns) {
  x <- paste(x[!is.na(x)], collapse = " ")
  if (!nzchar(x)) return(FALSE)
  any(grepl(paste(patterns, collapse = "|"), x, ignore.case = TRUE, perl = TRUE))
}

known_roles <- data.frame(
  accession = c(
    "GSE132758", "GSE178265", "GSE200610", "GSE204796", "GSE233885",
    "GSE204795", "GSE183248", "GSE243639", "GSE157783", "GSE184950"
  ),
  expected_role = c(
    "core_model_development_dopaminergic_graft_or_lineage_context",
    "core_model_development_dopaminergic_target_reference",
    "core_model_development_dopaminergic_graft_or_lineage_context",
    "core_model_development_dopaminergic_differentiation_or_graft_context",
    "core_model_development_projection_linked_dopaminergic_reference",
    "bulk_support_not_scRNA_model_training",
    "independent_external_frozen_framework_validation_candidate",
    "marker_targeted_context_validation_not_primary_training",
    "manual_review_background_or_reference_only",
    "manual_review_background_or_reference_only"
  ),
  expected_training_in_09C = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
  expected_external_validation = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
  expected_context_validation = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
  expected_bulk_support = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
  recommended_claim_role = c(
    "Use only as dopaminergic cell-state / graft-lineage model-development context after manual domain confirmation.",
    "Use only as dopaminergic target/reference context; do not call it PD disease modelling.",
    "Use only as dopaminergic graft/lineage-related context after manual domain confirmation.",
    "Use only as dopaminergic differentiation/graft-related context after manual domain confirmation.",
    "Use only as projection-linked dopaminergic molecular reference; not anatomical-projection claim.",
    "Use only as bulk transcriptomic support; do not describe as scRNA model training.",
    "Use as frozen external validation/application only if dataset-domain check confirms dopaminergic relevance.",
    "Use as marker-targeted disease/context validation only; not full-transcriptome solid external test set.",
    "Keep as background/reference/manual-review only unless direct model role is proven.",
    "Keep as background/reference/manual-review only unless direct model role is proven."
  ),
  stringsAsFactors = FALSE
)

make_dir(OUT_TABLE_DIR)
make_dir(OUT_TEXT_DIR)

msg("[10G0 V8] Starting standalone STRICT dataset-domain and project-reframing audit...")
msg("[10G0 V8] Project root     :", PROJECT_ROOT)
msg("[10G0 V8] Output table dir:", OUT_TABLE_DIR)
msg("[10G0 V8] Output text dir :", OUT_TEXT_DIR)

existing_scan_folders <- SCAN_FOLDERS[dir.exists(SCAN_FOLDERS)]
msg("[10G0 V8] Existing scan folders:", length(existing_scan_folders))
for (p in existing_scan_folders) msg("[10G0 V8] Listing:", p)

if (length(existing_scan_folders) == 0) stop("No scan folders found. Check PROJECT_ROOT.")

all_files <- unlist(lapply(existing_scan_folders, function(p) {
  list.files(p, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
}), use.names = FALSE)
all_files <- unique(all_files[file.exists(all_files)])

file_info <- data.frame(
  file_path = all_files,
  file_name = basename(all_files),
  file_ext  = tolower(tools::file_ext(all_files)),
  size_bytes = suppressWarnings(file.info(all_files)$size),
  stringsAsFactors = FALSE
)
file_info$size_mb <- round(file_info$size_bytes / 1024^2, 3)
file_info$scan_folder <- NA_character_
for (p in existing_scan_folders) {
  hit <- startsWith(normalizePath(file_info$file_path, winslash = "/", mustWork = FALSE),
                    normalizePath(p, winslash = "/", mustWork = FALSE))
  file_info$scan_folder[hit] <- p
}

msg("[10G0 V8] Files detected:", nrow(file_info))
safe_write_csv(file_info, file.path(OUT_TABLE_DIR, "10G0_V8_STANDALONE_STRICT_all_detected_project_files.csv"))

msg("[10G0 V8] Scanning filenames/paths using strict GSE parser...")
path_rows <- list()
for (i in seq_len(nrow(file_info))) {
  path_text <- paste(file_info$file_path[i], file_info$file_name[i], sep = " ")
  accs <- extract_gse_strict(path_text)
  if (length(accs) > 0) {
    for (a in accs) {
      path_rows[[length(path_rows) + 1]] <- data.frame(
        accession = a,
        evidence_type = "path_or_filename",
        file_path = file_info$file_path[i],
        file_name = file_info$file_name[i],
        snippet = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
}
path_evidence <- if (length(path_rows)) do.call(rbind, path_rows) else data.frame(
  accession=character(), evidence_type=character(), file_path=character(), file_name=character(), snippet=character(), stringsAsFactors=FALSE)
safe_write_csv(path_evidence, file.path(OUT_TABLE_DIR, "10G0_V8_STANDALONE_STRICT_path_accession_evidence.csv"))
msg("[10G0 V8] Path evidence rows:", nrow(path_evidence))

text_ext <- c("csv", "tsv", "txt", "md", "r", "rmd", "log", "yml", "yaml")
key_name_pattern <- paste(c(
  "GSE", "dataset", "manifest", "audit", "report", "story", "storyline",
  "source", "figure", "legend", "reference", "model", "input", "training", "validation",
  "10A", "10B", "10C", "10D", "10E", "10F", "09B", "09C", "09E", "09F", "09I"
), collapse = "|")

key_candidates <- file_info[
  file_info$file_ext %in% text_ext &
    !is.na(file_info$size_bytes) &
    file_info$size_bytes <= CONTENT_MAX_FILE_SIZE_MB * 1024^2 &
    grepl(key_name_pattern, file_info$file_name, ignore.case = TRUE),
]

key_candidates <- key_candidates[order(key_candidates$size_bytes),]
if (nrow(key_candidates) > CONTENT_MAX_FILES) key_candidates <- key_candidates[seq_len(CONTENT_MAX_FILES),]

msg("[10G0 V8] Limited content scan key files:", nrow(key_candidates))

content_rows <- list()
for (i in seq_len(nrow(key_candidates))) {
  fp <- key_candidates$file_path[i]
  lines <- safe_read_lines(fp, n = CONTENT_MAX_LINES)
  if (length(lines) == 0) next
  accs <- extract_gse_strict(lines)
  if (length(accs) > 0) {
    for (a in unique(accs)) {
      content_rows[[length(content_rows) + 1]] <- data.frame(
        accession = a,
        evidence_type = "limited_content_scan",
        file_path = fp,
        file_name = basename(fp),
        snippet = context_snippet(lines, a),
        stringsAsFactors = FALSE
      )
    }
  }
}
content_evidence <- if (length(content_rows)) do.call(rbind, content_rows) else data.frame(
  accession=character(), evidence_type=character(), file_path=character(), file_name=character(), snippet=character(), stringsAsFactors=FALSE)
safe_write_csv(content_evidence, file.path(OUT_TABLE_DIR, "10G0_V8_STANDALONE_STRICT_limited_content_accession_evidence.csv"))
msg("[10G0 V8] Limited content evidence rows:", nrow(content_evidence))

all_evidence <- rbind(path_evidence, content_evidence)
all_evidence$accession <- toupper(all_evidence$accession)
all_evidence <- all_evidence[!duplicated(paste(all_evidence$accession, all_evidence$evidence_type, all_evidence$file_path, all_evidence$snippet)),]

all_evidence <- all_evidence[grepl("^GSE[0-9]{5,7}$", all_evidence$accession),]

safe_write_csv(all_evidence, file.path(OUT_TABLE_DIR, "10G0_V8_STANDALONE_STRICT_all_accession_evidence.csv"))

detected_accessions <- sort(unique(all_evidence$accession))
detected_df <- data.frame(accession = detected_accessions, stringsAsFactors = FALSE)
safe_write_csv(detected_df, file.path(OUT_TABLE_DIR, "10G0_V8_STANDALONE_STRICT_detected_accessions.csv"))
msg("[10G0 V8] Strict valid GSE accessions detected from fresh scan:", length(detected_accessions))

all_accessions <- sort(unique(c(known_roles$accession, detected_accessions)))
msg("[10G0 V8] Total accessions in audit including known role table:", length(all_accessions))

evidence_summary <- data.frame(accession = all_accessions, stringsAsFactors = FALSE)
evidence_summary$path_evidence_n <- vapply(evidence_summary$accession, function(a) sum(path_evidence$accession == a), integer(1))
evidence_summary$content_evidence_n <- vapply(evidence_summary$accession, function(a) sum(content_evidence$accession == a), integer(1))
evidence_summary$total_evidence_n <- evidence_summary$path_evidence_n + evidence_summary$content_evidence_n
evidence_summary$detected_by_fresh_scan <- evidence_summary$accession %in% detected_accessions

evidence_summary$evidence_files <- vapply(evidence_summary$accession, function(a) {
  collapse_unique(all_evidence$file_name[all_evidence$accession == a])
}, character(1))
evidence_summary$evidence_paths_compact <- vapply(evidence_summary$accession, function(a) {
  collapse_unique(head(all_evidence$file_path[all_evidence$accession == a], 8))
}, character(1))
evidence_summary$evidence_snippet_compact <- vapply(evidence_summary$accession, function(a) {
  collapse_unique(head(all_evidence$snippet[all_evidence$accession == a & !is.na(all_evidence$snippet)], 5))
}, character(1))
safe_write_csv(evidence_summary, file.path(OUT_TABLE_DIR, "10G0_V8_STANDALONE_STRICT_accession_evidence_summary.csv"))

audit <- merge(evidence_summary, known_roles, by = "accession", all.x = TRUE, sort = FALSE)

audit$in_known_role_table <- !is.na(audit$expected_role)
audit$expected_role[is.na(audit$expected_role)] <- "additional_detected_accession_manual_review_required"
audit$expected_training_in_09C[is.na(audit$expected_training_in_09C)] <- FALSE
audit$expected_external_validation[is.na(audit$expected_external_validation)] <- FALSE
audit$expected_context_validation[is.na(audit$expected_context_validation)] <- FALSE
audit$expected_bulk_support[is.na(audit$expected_bulk_support)] <- FALSE
audit$recommended_claim_role[is.na(audit$recommended_claim_role)] <- "Manual review required before assigning any model or validation role. Do not include in main training claims until confirmed."

combined_text_by_accession <- function(a) {
  paste(
    all_evidence$file_path[all_evidence$accession == a],
    all_evidence$snippet[all_evidence$accession == a],
    collapse = " "
  )
}

dopaminergic_patterns <- c(
  "dopaminergic", "dopamine", "\\bDA\\b", "mDA", "midbrain", "neuron", "neuronal",
  "graft", "transplant", "differentiation", "A9", "A10", "TH", "DDC", "SLC6A3",
  "DAT", "NR4A2", "NURR1", "LMX1A", "FOXA2"
)
pd_patterns <- c("Parkinson", "\\bPD\\b", "parkinsonian")
blood_patterns <- c("blood", "PBMC", "peripheral", "immune", "lymphocyte", "monocyte", "neutrophil", "T cell", "B cell", "plasma")

for (i in seq_len(nrow(audit))) {
  txt <- combined_text_by_accession(audit$accession[i])
  audit$dopaminergic_or_neuronal_keyword[i] <- contains_any(txt, dopaminergic_patterns)
  audit$PD_keyword[i] <- contains_any(txt, pd_patterns)
  audit$blood_or_peripheral_keyword[i] <- contains_any(txt, blood_patterns)
}

audit$primary_scRNA_model_training_allowed <- audit$expected_training_in_09C
audit$solid_external_test_candidate <- audit$expected_external_validation
audit$context_validation_only <- audit$expected_context_validation
audit$not_scRNA_training_reason <- NA_character_
audit$not_scRNA_training_reason[audit$expected_bulk_support] <- "Bulk support only; not scRNA model training."
audit$not_scRNA_training_reason[audit$expected_context_validation] <- "Context/marker-targeted validation only; not primary model training."
audit$not_scRNA_training_reason[audit$expected_external_validation] <- "External frozen validation/application only; not used for model training."
audit$not_scRNA_training_reason[!audit$expected_training_in_09C & is.na(audit$not_scRNA_training_reason)] <- "Not assigned to 09C primary training by locked role table; manual review/background only."

audit$manual_review_required <- TRUE
audit$manual_review_reason <- "Confirm dataset biological source, assay type, and exact model role before manuscript/GitHub release."
audit$manual_review_reason[!audit$in_known_role_table] <- "Additional detected accession not in known role table; verify whether it is real project data or incidental reference."
audit$manual_review_reason[audit$blood_or_peripheral_keyword & audit$primary_scRNA_model_training_allowed] <- "Potential blood/peripheral keyword near a training candidate; manually confirm this is not unrelated tissue in model training."
audit$manual_review_reason[audit$expected_external_validation] <- "Confirm this independent validation dataset is dopaminergic-relevant before describing it as external validation."
audit$manual_review_reason[audit$expected_context_validation] <- "Keep as marker-targeted context validation; do not describe as full solid external test set."

audit$project_safe_wording <- ifelse(
  audit$expected_training_in_09C,
  "Model-development dataset for dopaminergic neuron/graft-related transcriptomic prioritization; not a PD disease model dataset.",
  ifelse(
    audit$expected_external_validation,
    "Frozen external validation/application dataset; not used for model training and not a clinical outcome test set.",
    ifelse(
      audit$expected_context_validation,
      "Marker-targeted context validation dataset; not primary training and not full-transcriptome external test set.",
      ifelse(
        audit$expected_bulk_support,
        "Bulk support dataset; analyzed separately and not used as scRNA model training.",
        "Background/manual-review dataset; do not use for main model claims unless role is confirmed."
      )
    )
  )
)

audit <- audit[, c(
  "accession", "detected_by_fresh_scan", "in_known_role_table", "expected_role",
  "expected_training_in_09C", "expected_external_validation", "expected_context_validation", "expected_bulk_support",
  "primary_scRNA_model_training_allowed", "solid_external_test_candidate", "context_validation_only",
  "dopaminergic_or_neuronal_keyword", "PD_keyword", "blood_or_peripheral_keyword",
  "path_evidence_n", "content_evidence_n", "total_evidence_n",
  "recommended_claim_role", "project_safe_wording", "not_scRNA_training_reason",
  "manual_review_required", "manual_review_reason",
  "evidence_files", "evidence_paths_compact", "evidence_snippet_compact"
)]

safe_write_csv(audit, file.path(OUT_TABLE_DIR, "10G0_V8_STANDALONE_STRICT_dataset_domain_role_audit.csv"))

model_input_audit <- audit[, c(
  "accession", "expected_role", "expected_training_in_09C", "primary_scRNA_model_training_allowed",
  "expected_external_validation", "expected_context_validation", "expected_bulk_support",
  "solid_external_test_candidate", "context_validation_only", "project_safe_wording", "not_scRNA_training_reason",
  "manual_review_required", "manual_review_reason"
)]
safe_write_csv(model_input_audit, file.path(OUT_TABLE_DIR, "10G0_V8_STANDALONE_STRICT_model_input_dataset_audit.csv"))

additional_review <- audit[!audit$in_known_role_table | audit$blood_or_peripheral_keyword | audit$manual_review_required, ]
safe_write_csv(additional_review, file.path(OUT_TABLE_DIR, "10G0_V8_STANDALONE_STRICT_manual_review_action_items.csv"))

rewording <- data.frame(
  unsafe_or_old_term = c(
    "Parkinson's disease model",
    "PD disease model",
    "PD graft safety prediction",
    "PD therapeutic prediction",
    "clinical efficacy prediction",
    "clinical safety prediction",
    "good cells / bad cells",
    "true projection",
    "functional host integration",
    "tumorigenicity prediction"
  ),
  safe_replacement = c(
    "dopaminergic neuron/graft-related cell-state prioritization framework",
    "dopaminergic neuron/graft-related transcriptomic prioritization framework",
    "safety-risk-associated transcriptional program prioritization",
    "PD-relevant dopaminergic cell-replacement application context",
    "transcriptomic prioritization; not clinical efficacy prediction",
    "risk-associated transcriptional program assessment; not clinical safety prediction",
    "high-priority vs lower-priority transcriptional cell states",
    "projection-associated molecular competence; not anatomical-projection claim",
    "graft-relevant molecular context; not functional host integration proof",
    "proliferation/risk-associated transcriptional signal; not tumorigenicity prediction"
  ),
  stringsAsFactors = FALSE
)
safe_write_csv(rewording, file.path(OUT_TABLE_DIR, "10G0_V8_STANDALONE_STRICT_PD_to_dopaminergic_rewording_table.csv"))

project_reframing_note <- c(
  "10G0 V8 STANDALONE STRICT project reframing note",
  "=================================================",
  "",
  "Recommended project identity:",
  "A hypothesis-guided transcriptomic prioritization framework for dopaminergic neuron / dopaminergic graft-related cell-state evaluation.",
  "",
  "Chinese:",
  "多巴胺能神经元 / 多巴胺能移植相关细胞状态的假设驱动型转录组优先级评估框架。",
  "",
  "Key correction:",
  "The project should not be framed as a direct Parkinson's disease disease-mechanism model or clinical-use model.",
  "PD should be described only as downstream application context because dopaminergic neuron replacement is relevant to Parkinsonian cell-replacement strategies.",
  "",
  "Safe one-sentence description:",
  "This project prioritizes dopaminergic neuron/graft-related cell states using predefined transcriptomic signatures, marker-rule-derived machine learning, cross-dataset validation, and strict claim-boundary control.",
  "",
  "Forbidden overclaims:",
  "- clinical efficacy prediction",
  "- clinical safety prediction",
  "- true graft success prediction",
  "- true anatomical projection",
  "- functional host integration",
  "- tumorigenicity prediction",
  "- real good/bad cell labels"
)
safe_write_lines(project_reframing_note, file.path(OUT_TEXT_DIR, "10G0_V8_STANDALONE_STRICT_project_reframing_note.txt"))

solid_testset_explanation <- c(
  "10G0 V8 STANDALONE STRICT solid-testset explanation",
  "=====================================================",
  "",
  "Teacher-safe explanation:",
  "The core model should be presented as a cross-dataset dopaminergic neuron/graft-related transcriptomic prioritization model, not as a PD clinical-use model.",
  "The primary model-development candidates are the locked dopaminergic/graft/lineage/projection-linked datasets listed in the audit table.",
  "External validation/application must remain frozen and should not be mixed into training.",
  "GSE243639 should be described as marker-targeted context validation, not as a full-transcriptome solid external test set.",
  "",
  "Important limitation:",
  "A solid test set in the clinical sense would require independent outcome-labelled graft/functional data. This project does not claim that level of validation.",
  "",
  "Allowed claim:",
  "The model supports transcriptomic prioritization robustness across selected dopaminergic-relevant datasets and validation contexts.",
  "",
  "Not allowed claim:",
  "The model predicts true clinical safety, therapeutic efficacy, or graft success in PD patients."
)
safe_write_lines(solid_testset_explanation, file.path(OUT_TEXT_DIR, "10G0_V8_STANDALONE_STRICT_solid_testset_explanation.txt"))

safe_answer_bank <- c(
  "10G0 V8 STANDALONE STRICT teacher safe answer bank",
  "===================================================",
  "",
  "Q1: Is this a PD disease project?",
  "A: Not strictly. The core project is dopaminergic neuron/graft-related cell-state transcriptomic prioritization. PD is the application background because dopaminergic neuron replacement is PD-relevant.",
  "",
  "Q2: Did we mix blood/PBMC/unrelated tissues into the core model?",
  "A: The locked primary 09C model role table assigns training to dopaminergic neuron/graft/lineage/projection-linked datasets. Non-training datasets are separated as bulk support, frozen external validation, marker-targeted context validation, or manual-review background. Any blood/peripheral keyword flags in the audit are manual-review warnings, not proof of model contamination.",
  "",
  "Q3: What does the model predict?",
  "A: It estimates transcriptomic priority / ideal-like probability / risk-associated transcriptional tendency at the cell-state level. It does not predict clinical outcome or real graft success.",
  "",
  "Q4: Is GSE243639 a solid external test set?",
  "A: No. It should be described as marker-targeted context validation. It is not a full-transcriptome primary graft external test set.",
  "",
  "Q5: What is the strongest validation claim?",
  "A: Internal CV, LODO-style cross-dataset validation, negative controls, and frozen external/context applications support the robustness of transcriptomic prioritization, within the defined claim boundary."
)
safe_write_lines(safe_answer_bank, file.path(OUT_TEXT_DIR, "10G0_V8_STANDALONE_STRICT_teacher_safe_answer_bank.txt"))

execution_report <- c(
  "10G0 V8 STANDALONE STRICT execution report",
  "==========================================",
  paste("Project root:", PROJECT_ROOT),
  paste("Files detected:", nrow(file_info)),
  paste("Key files content-scanned:", nrow(key_candidates)),
  paste("Strict detected GSE accessions:", length(detected_accessions)),
  paste("Detected accessions:", paste(detected_accessions, collapse = ", ")),
  paste("Total audit accessions including known role table:", nrow(audit)),
  paste("False short accessions GSE184/GSE243 present:", any(audit$accession %in% c("GSE184", "GSE243"))),
  paste("Training-assigned accessions:", paste(audit$accession[audit$expected_training_in_09C], collapse = ", ")),
  paste("External-validation-assigned accessions:", paste(audit$accession[audit$expected_external_validation], collapse = ", ")),
  paste("Context-validation-assigned accessions:", paste(audit$accession[audit$expected_context_validation], collapse = ", ")),
  "",
  "Next recommended step:",
  "Open 10G0_V8_STANDALONE_STRICT_dataset_domain_role_audit.csv and confirm whether any additional_detected_accession_manual_review_required rows are real project datasets. If clean, proceed to 09K0 pseudotime input readiness audit."
)
safe_write_lines(execution_report, file.path(OUT_TEXT_DIR, "10G0_V8_STANDALONE_STRICT_execution_report.txt"))

msg("")
msg("[10G0 V8] Completed standalone STRICT audit.")
msg("[10G0 V8] Files detected:", nrow(file_info))
msg("[10G0 V8] Key files content-scanned:", nrow(key_candidates))
msg("[10G0 V8] Strict valid GSE accessions detected from fresh scan:", length(detected_accessions))
msg("[10G0 V8] Detected:", paste(detected_accessions, collapse = ", "))
msg("[10G0 V8] Total audit accessions including known role table:", nrow(audit))
msg("[10G0 V8] GSE184/GSE243 false positives present:", any(audit$accession %in% c("GSE184", "GSE243")))
msg("[10G0 V8] Output tables:", OUT_TABLE_DIR)
msg("[10G0 V8] Output text  :", OUT_TEXT_DIR)
msg("[10G0 V8] Main output  : 10G0_V8_STANDALONE_STRICT_dataset_domain_role_audit.csv")
msg("[10G0 V8] Next         : confirm audit rows, then run 09K0 pseudotime input readiness audit.")
