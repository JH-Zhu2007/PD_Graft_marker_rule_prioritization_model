
options(stringsAsFactors = FALSE)

cat("\n[10H] Starting dataset-role and model-scope freeze...\n")

PROJECT_ROOT <- "D:/PD_Graft_Project"
if (!dir.exists(PROJECT_ROOT)) {
  stop("[10H] PROJECT_ROOT does not exist: ", PROJECT_ROOT)
}

OUT_TABLE_DIR <- file.path(PROJECT_ROOT, "03_tables", "10H_dataset_role_and_model_scope_freeze_V1")
OUT_TEXT_DIR  <- file.path(PROJECT_ROOT, "09_manuscript", "10H_dataset_role_and_model_scope_freeze_V1")
dir.create(OUT_TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TEXT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("[10H] Project root     : ", PROJECT_ROOT, "\n", sep = "")
cat("[10H] Output table dir: ", OUT_TABLE_DIR, "\n", sep = "")
cat("[10H] Output text dir : ", OUT_TEXT_DIR, "\n", sep = "")

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  out <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      message("[10H] Failed to read CSV: ", path, " | ", conditionMessage(e))
      NULL
    }
  )
  out
}

write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
  cat("[10H] Wrote: ", path, "\n", sep = "")
}

write_txt <- function(lines, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con, useBytes = TRUE)
  cat("[10H] Wrote: ", path, "\n", sep = "")
}

first_existing_col <- function(df, candidates) {
  if (is.null(df)) return(NA_character_)
  hit <- candidates[candidates %in% colnames(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

safe_unique <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  unique(x)
}

TEN_G_DIR <- file.path(PROJECT_ROOT, "03_tables", "10G0_dataset_domain_and_project_reframing_audit_V8_STANDALONE_STRICT")
TEN_G_AUDIT <- file.path(TEN_G_DIR, "10G0_V8_STANDALONE_STRICT_dataset_domain_role_audit.csv")
TEN_G_DETECTED <- file.path(TEN_G_DIR, "10G0_V8_STANDALONE_STRICT_detected_accessions.csv")
TEN_G_EVID_SUM <- file.path(TEN_G_DIR, "10G0_V8_STANDALONE_STRICT_accession_evidence_summary.csv")

cat("[10H] Looking for 10G V8 audit: ", TEN_G_AUDIT, "\n", sep = "")

v8_audit <- safe_read_csv(TEN_G_AUDIT)
v8_detected <- safe_read_csv(TEN_G_DETECTED)
v8_evid_sum <- safe_read_csv(TEN_G_EVID_SUM)

expected_accessions <- c(
  "GSE128040",
  "GSE132758",
  "GSE148434",
  "GSE157783",
  "GSE178265",
  "GSE183248",
  "GSE184950",
  "GSE200610",
  "GSE204795",
  "GSE204796",
  "GSE233885",
  "GSE243639"
)

accession_candidates <- character(0)
for (df in list(v8_audit, v8_detected, v8_evid_sum)) {
  col <- first_existing_col(df, c("accession", "GSE", "gse", "detected_accession", "series", "geo_accession"))
  if (!is.na(col)) accession_candidates <- c(accession_candidates, df[[col]])
}

for (df in list(v8_audit, v8_detected, v8_evid_sum)) {
  if (!is.null(df) && nrow(df) > 0) {
    char_cols <- colnames(df)[vapply(df, is.character, logical(1))]
    for (cc in char_cols) {
      hits <- regmatches(df[[cc]], gregexpr("GSE[0-9]{5,7}", df[[cc]], perl = TRUE))
      accession_candidates <- c(accession_candidates, unlist(hits, use.names = FALSE))
    }
  }
}

accessions <- sort(unique(c(expected_accessions, accession_candidates)))
accessions <- accessions[grepl("^GSE[0-9]{5,7}$", accessions)]

accessions <- setdiff(accessions, c("GSE184", "GSE243"))

cat("[10H] Accessions entering role lock: ", length(accessions), "\n", sep = "")
cat("[10H] Accessions: ", paste(accessions, collapse = ", "), "\n", sep = "")

role_rules <- data.frame(
  accession = c(
    "GSE178265",
    "GSE132758",
    "GSE200610",
    "GSE204796",
    "GSE233885",
    "GSE183248",
    "GSE204795",
    "GSE243639",
    "GSE157783",
    "GSE184950",
    "GSE128040",
    "GSE148434"
  ),
  locked_project_role = c(
    "core_model_development_reference",
    "core_model_development_reference",
    "core_model_development_reference",
    "core_model_development_reference",
    "core_model_development_reference",
    "independent_external_validation_not_training",
    "bulk_support_not_scRNA_model_training",
    "marker_targeted_context_validation_not_training",
    "background_or_manual_review_not_core_training",
    "background_or_manual_review_not_core_training",
    "historical_or_extra_detected_accession_manual_review_not_core_training",
    "historical_or_extra_detected_accession_manual_review_not_core_training"
  ),
  include_in_09C_core_training = c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  include_in_LODO_scope_if_used_in_09C = c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  include_in_external_validation = c(
    FALSE, FALSE, FALSE, FALSE, FALSE,
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  include_in_marker_targeted_context_validation = c(
    FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE
  ),
  include_in_bulk_support = c(
    FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  allowed_for_pseudotime_candidate_screen = c(
    TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  manual_review_required_before_claim = c(
    FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE
  ),
  domain_label_for_manuscript = c(
    "dopaminergic neuron / DA reference or graft-related model-development context",
    "dopaminergic neuron / graft-related model-development context",
    "dopaminergic lineage / graft-related model-development context",
    "dopaminergic differentiation / graft-related model-development context",
    "projection-linked dopaminergic molecular reference context",
    "independent dopaminergic-related external validation context",
    "bulk RNA-seq support analysed separately from the scRNA model",
    "marker-targeted context validation; not a full external test set",
    "manual-review/background reference; not locked as core model training",
    "manual-review/background reference; not locked as core model training",
    "extra accession detected in project files; not locked as model input",
    "extra accession detected in project files; not locked as model input"
  ),
  safe_claim = c(
    "May be described as part of the core dopaminergic cell-state model-development/reference scope, subject to source metadata confirmation.",
    "May be described as part of the core dopaminergic cell-state model-development/reference scope, subject to source metadata confirmation.",
    "May be described as part of the core dopaminergic cell-state model-development/reference scope, subject to source metadata confirmation.",
    "May be described as part of the core dopaminergic cell-state model-development/reference scope, subject to source metadata confirmation.",
    "May be described as projection-linked dopaminergic molecular reference context, not anatomical-projection claim.",
    "May be described as frozen external application/validation; it must not be described as training data.",
    "May be described only as separately analysed bulk support; it must not be described as scRNA model training data.",
    "May be described as marker-targeted context validation; it is not a full-transcriptome solid external test set.",
    "Manual review/background only unless metadata proves direct dopaminergic graft relevance.",
    "Manual review/background only unless metadata proves direct dopaminergic graft relevance.",
    "Historical/extra detected accession; exclude from claims unless manually verified.",
    "Historical/extra detected accession; exclude from claims unless manually verified."
  ),
  disallowed_claim = c(
    rep("Do not call this a PD disease or clinical-outcome dataset. Do not claim therapeutic efficacy, clinical safety, or true graft success.", 5),
    "Do not call this training data. Do not claim clinical efficacy or safety validation.",
    "Do not call this scRNA model training data or an external scRNA test set.",
    "Do not call this primary graft validation, full external test set, clinical validation, or safety/efficacy validation.",
    "Do not include in core training or external validation claims without manual metadata confirmation.",
    "Do not include in core training or external validation claims without manual metadata confirmation.",
    "Do not include in model claims unless manually verified.",
    "Do not include in model claims unless manually verified."
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

extra_accessions <- setdiff(accessions, role_rules$accession)
if (length(extra_accessions) > 0) {
  extra_rows <- data.frame(
    accession = extra_accessions,
    locked_project_role = "newly_detected_manual_review_not_core_training",
    include_in_09C_core_training = FALSE,
    include_in_LODO_scope_if_used_in_09C = FALSE,
    include_in_external_validation = FALSE,
    include_in_marker_targeted_context_validation = FALSE,
    include_in_bulk_support = FALSE,
    allowed_for_pseudotime_candidate_screen = FALSE,
    manual_review_required_before_claim = TRUE,
    domain_label_for_manuscript = "newly detected accession; not locked as model input",
    safe_claim = "Exclude from model and manuscript claims unless manually verified.",
    disallowed_claim = "Do not include in core model, external validation, or biological claims without manual metadata confirmation.",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  role_rules <- rbind(role_rules, extra_rows)
}

role_lock <- role_rules[role_rules$accession %in% accessions, , drop = FALSE]
role_lock <- role_lock[order(role_lock$accession), , drop = FALSE]

evidence_count_df <- data.frame(accession = role_lock$accession, evidence_rows_from_10G = NA_integer_, stringsAsFactors = FALSE)

if (!is.null(v8_evid_sum) && nrow(v8_evid_sum) > 0) {
  acc_col <- first_existing_col(v8_evid_sum, c("accession", "GSE", "gse", "detected_accession", "series", "geo_accession"))
  count_col <- first_existing_col(v8_evid_sum, c("n_evidence_rows", "evidence_rows", "n", "count", "n_rows"))
  if (!is.na(acc_col)) {
    tmp <- v8_evid_sum
    tmp$accession_tmp <- as.character(tmp[[acc_col]])
    if (!is.na(count_col)) {
      tmp$count_tmp <- suppressWarnings(as.integer(tmp[[count_col]]))
    } else {
      tmp$count_tmp <- 1L
    }
    tmp2 <- aggregate(count_tmp ~ accession_tmp, tmp, sum, na.rm = TRUE)
    colnames(tmp2) <- c("accession", "evidence_rows_from_10G")
    evidence_count_df <- merge(evidence_count_df["accession"], tmp2, by = "accession", all.x = TRUE)
  }
}

role_lock <- merge(role_lock, evidence_count_df, by = "accession", all.x = TRUE, sort = FALSE)
role_lock <- role_lock[order(role_lock$accession), , drop = FALSE]

core_training <- role_lock[role_lock$include_in_09C_core_training, , drop = FALSE]
not_core_training <- role_lock[!role_lock$include_in_09C_core_training, , drop = FALSE]
manual_review <- role_lock[role_lock$manual_review_required_before_claim, , drop = FALSE]
pseudotime_candidates <- role_lock[role_lock$allowed_for_pseudotime_candidate_screen, , drop = FALSE]

role_summary <- data.frame(
  category = c(
    "total_locked_accessions",
    "core_09C_training_or_model_development_reference",
    "external_validation_not_training",
    "marker_targeted_context_validation_not_training",
    "bulk_support_not_scRNA_training",
    "manual_review_or_background_not_core_training",
    "pseudotime_candidate_screen_accessions"
  ),
  n = c(
    nrow(role_lock),
    sum(role_lock$include_in_09C_core_training),
    sum(role_lock$include_in_external_validation),
    sum(role_lock$include_in_marker_targeted_context_validation),
    sum(role_lock$include_in_bulk_support),
    sum(role_lock$manual_review_required_before_claim),
    sum(role_lock$allowed_for_pseudotime_candidate_screen)
  ),
  accessions = c(
    paste(role_lock$accession, collapse = ", "),
    paste(core_training$accession, collapse = ", "),
    paste(role_lock$accession[role_lock$include_in_external_validation], collapse = ", "),
    paste(role_lock$accession[role_lock$include_in_marker_targeted_context_validation], collapse = ", "),
    paste(role_lock$accession[role_lock$include_in_bulk_support], collapse = ", "),
    paste(manual_review$accession, collapse = ", "),
    paste(pseudotime_candidates$accession, collapse = ", ")
  ),
  stringsAsFactors = FALSE
)

model_scope_decision <- data.frame(
  decision_item = c(
    "project_primary_scope",
    "PD_scope_boundary",
    "core_model_training_scope",
    "external_validation_scope",
    "context_validation_scope",
    "bulk_support_scope",
    "manual_review_rule",
    "pseudotime_next_step_scope",
    "forbidden_overclaim"
  ),
  locked_decision = c(
    "Dopaminergic neuron / dopaminergic graft-related cell-state transcriptomic prioritization framework.",
    "Parkinson's disease should be presented only as application background for dopaminergic neuron replacement, not as the primary disease-data claim.",
    paste0("Expected 09C core/model-development accessions: ", paste(core_training$accession, collapse = ", "), "."),
    paste0("External validation is locked as non-training application/validation: ", paste(role_lock$accession[role_lock$include_in_external_validation], collapse = ", "), "."),
    paste0("Marker-targeted context validation is locked as non-training and not a full external test set: ", paste(role_lock$accession[role_lock$include_in_marker_targeted_context_validation], collapse = ", "), "."),
    paste0("Bulk support is analysed separately and is not scRNA model training: ", paste(role_lock$accession[role_lock$include_in_bulk_support], collapse = ", "), "."),
    "Any accession marked manual-review/background is excluded from core model and validation claims unless source metadata is manually confirmed.",
    paste0("10I pseudotime readiness audit may screen only allowed candidate accessions/objects: ", paste(pseudotime_candidates$accession, collapse = ", "), "."),
    "Do not claim PD disease modelling, clinical prediction, therapeutic efficacy, true safety prediction, anatomical projection, or functional graft integration."
  ),
  stringsAsFactors = FALSE
)

write_csv(role_lock, file.path(OUT_TABLE_DIR, "10H_V1_dataset_role_LOCK_TABLE.csv"))
write_csv(model_scope_decision, file.path(OUT_TABLE_DIR, "10H_V1_model_scope_decision_table.csv"))
write_csv(role_summary, file.path(OUT_TABLE_DIR, "10H_V1_dataset_role_summary.csv"))
write_csv(core_training, file.path(OUT_TABLE_DIR, "10H_V1_expected_09C_core_training_accessions.csv"))
write_csv(not_core_training, file.path(OUT_TABLE_DIR, "10H_V1_not_09C_core_training_accessions.csv"))
write_csv(manual_review, file.path(OUT_TABLE_DIR, "10H_V1_manual_review_or_background_items.csv"))
write_csv(pseudotime_candidates, file.path(OUT_TABLE_DIR, "10H_V1_pseudotime_candidate_scope.csv"))

teacher_safe_answer <- c(
  "10H Dataset Role and Model Scope Freeze - Teacher-Safe Answer",
  "================================================================",
  "",
  "One-sentence project scope:",
  "This project is now framed as a dopaminergic neuron / dopaminergic graft-related cell-state transcriptomic prioritization framework, with Parkinson's disease used only as a downstream application context for dopaminergic neuron replacement.",
  "",
  "Did we mix unrelated tissues into the core model?",
  "Based on the frozen role table, the expected 09C core model-development scope is restricted to dopaminergic neuron / graft / differentiation / lineage / projection-linked reference contexts. Blood, PBMC, or unrelated peripheral tissue datasets are not locked as core 09C training data.",
  "",
  "Core 09C model-development/reference accessions:",
  paste("-", core_training$accession, core_training$domain_label_for_manuscript),
  "",
  "Not core training:",
  paste("-", not_core_training$accession, not_core_training$locked_project_role),
  "",
  "Important boundaries:",
  "- GSE183248 is treated as non-training external application/validation, not model training.",
  "- GSE243639 is treated as marker-targeted context validation, not a full external test set and not primary graft validation.",
  "- GSE204795 is bulk support analysed separately, not scRNA model training.",
  "- GSE157783, GSE184950, GSE128040 and GSE148434 require manual review/background handling and are not locked as core model inputs.",
  "",
  "Safe wording:",
  "We integrated multiple dopaminergic neuron/graft-related transcriptomic contexts to prioritize cell states. The model output should be interpreted as transcriptomic prioritization, not clinical prediction, therapeutic efficacy, or true safety validation.",
  "",
  "Next step:",
  "Proceed to 10I_pseudotime_input_readiness_audit only after keeping this model-scope lock as the reference table."
)
write_txt(teacher_safe_answer, file.path(OUT_TEXT_DIR, "10H_V1_teacher_safe_answer_one_page.txt"))

scope_freeze_note <- c(
  "10H Model Scope Freeze Note",
  "===========================",
  "",
  "Locked primary scope:",
  model_scope_decision$locked_decision[model_scope_decision$decision_item == "project_primary_scope"],
  "",
  "PD boundary:",
  model_scope_decision$locked_decision[model_scope_decision$decision_item == "PD_scope_boundary"],
  "",
  "Core model accessions:",
  paste(core_training$accession, collapse = ", "),
  "",
  "Non-core accessions and roles:",
  paste0(not_core_training$accession, " | ", not_core_training$locked_project_role),
  "",
  "Pseudotime planning rule:",
  "10I should only inspect suitable scRNA/Seurat objects from the locked dopaminergic-related candidate scope. It should not use bulk-only, marker-targeted context-only, or manual-review/background accessions as pseudotime sources unless manually approved.",
  "",
  "Overclaim rule:",
  model_scope_decision$locked_decision[model_scope_decision$decision_item == "forbidden_overclaim"]
)
write_txt(scope_freeze_note, file.path(OUT_TEXT_DIR, "10H_V1_model_scope_freeze_note.txt"))

execution_report <- c(
  "10H Execution Report",
  "====================",
  paste0("Run time: ", as.character(Sys.time())),
  paste0("Project root: ", PROJECT_ROOT),
  paste0("V8 audit found: ", ifelse(file.exists(TEN_G_AUDIT), "YES", "NO")),
  paste0("Accessions entering role lock: ", length(accessions)),
  paste0("Accessions: ", paste(accessions, collapse = ", ")),
  paste0("Core 09C/model-development accessions: ", paste(core_training$accession, collapse = ", ")),
  paste0("External validation accessions: ", paste(role_lock$accession[role_lock$include_in_external_validation], collapse = ", ")),
  paste0("Marker-targeted context validation accessions: ", paste(role_lock$accession[role_lock$include_in_marker_targeted_context_validation], collapse = ", ")),
  paste0("Bulk support accessions: ", paste(role_lock$accession[role_lock$include_in_bulk_support], collapse = ", ")),
  paste0("Manual-review/background accessions: ", paste(manual_review$accession, collapse = ", ")),
  "",
  "Completed without retraining models, rerunning analysis, or modifying figure packages.",
  "Next recommended module: 10I_pseudotime_input_readiness_audit."
)
write_txt(execution_report, file.path(OUT_TEXT_DIR, "10H_V1_execution_report.txt"))

cat("\n[10H] Completed dataset-role and model-scope freeze.\n")
cat("[10H] Locked accessions total: ", nrow(role_lock), "\n", sep = "")
cat("[10H] Core 09C/model-development accessions: ", paste(core_training$accession, collapse = ", "), "\n", sep = "")
cat("[10H] Not core training accessions: ", paste(not_core_training$accession, collapse = ", "), "\n", sep = "")
cat("[10H] Manual-review/background accessions: ", paste(manual_review$accession, collapse = ", "), "\n", sep = "")
cat("[10H] Pseudotime candidate scope: ", paste(pseudotime_candidates$accession, collapse = ", "), "\n", sep = "")
cat("[10H] Output tables: ", OUT_TABLE_DIR, "\n", sep = "")
cat("[10H] Output text  : ", OUT_TEXT_DIR, "\n", sep = "")
cat("[10H] Main output  : 10H_V1_dataset_role_LOCK_TABLE.csv\n")
cat("[10H] Next         : 10I_pseudotime_input_readiness_audit\n\n")
