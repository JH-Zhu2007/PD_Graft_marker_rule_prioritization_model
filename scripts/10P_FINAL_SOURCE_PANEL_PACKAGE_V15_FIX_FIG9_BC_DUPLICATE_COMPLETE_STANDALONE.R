
cat("\n[10P V15] Starting final source-panel package with Figure 9B/9C duplicate fix...\n")

PROJECT_ROOT <- "D:/PD_Graft_Project"
VERSION_TAG <- "10P_FINAL_V15_FIX_FIG9_BC_DUPLICATE"

dir_create_10p <- function(path_value) {
  if (!dir.exists(path_value)) dir.create(path_value, recursive = TRUE, showWarnings = FALSE)
}

write_csv_10p <- function(df_value, file_value) {
  utils::write.csv(df_value, file_value, row.names = FALSE, fileEncoding = "UTF-8")
  cat("[10P V15] Wrote:", file_value, "\n")
}

normalize_slash_10p <- function(x_value) {
  gsub("\\\\", "/", x_value, fixed = FALSE)
}

safe_lower_10p <- function(x_value) {
  x_value <- as.character(x_value)
  x_value[is.na(x_value)] <- ""
  tolower(x_value)
}

safe_md5_10p <- function(file_vec) {
  out <- rep(NA_character_, length(file_vec))
  ok <- file.exists(file_vec)
  if (any(ok)) {
    out[ok] <- as.character(tools::md5sum(file_vec[ok]))
  }
  out
}

find_existing_dir_10p <- function(candidate_dirs) {
  candidate_dirs <- candidate_dirs[!is.na(candidate_dirs)]
  hit <- candidate_dirs[dir.exists(candidate_dirs)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

copy_pdf_set_10p <- function(from_dir, to_dir) {
  dir_create_10p(to_dir)
  if (is.na(from_dir) || !dir.exists(from_dir)) return(data.frame())
  pdfs <- list.files(from_dir, pattern = "\\.pdf$", full.names = TRUE, recursive = FALSE)
  if (length(pdfs) == 0) return(data.frame())
  target <- file.path(to_dir, basename(pdfs))
  ok <- file.copy(pdfs, target, overwrite = TRUE)
  data.frame(
    source_pdf = normalize_slash_10p(pdfs),
    output_pdf = normalize_slash_10p(target),
    copied = ok,
    stringsAsFactors = FALSE
  )
}

is_fig9_file_10p <- function(file_path) {
  nm <- safe_lower_10p(basename(file_path))
  grepl("figure_9", nm, fixed = TRUE) ||
    grepl("figure_09", nm, fixed = TRUE) ||
    grepl("panel_9", nm, fixed = TRUE)
}

is_panel_9a_10p <- function(file_path) {
  nm <- safe_lower_10p(basename(file_path))
  grepl("panel_9a", nm, fixed = TRUE) || grepl("_9a__", nm, fixed = TRUE)
}

is_panel_9b_10p <- function(file_path) {
  nm <- safe_lower_10p(basename(file_path))
  grepl("panel_9b", nm, fixed = TRUE) || grepl("_9b__", nm, fixed = TRUE)
}

is_panel_9c_10p <- function(file_path) {
  nm <- safe_lower_10p(basename(file_path))
  grepl("panel_9c", nm, fixed = TRUE) || grepl("_9c__", nm, fixed = TRUE)
}

input_main_candidates <- c(
  file.path(PROJECT_ROOT, "04_figures", "10P_FINAL_V14_NO_ASSEMBLY_SOURCE_PANEL_PACKAGE_pdf", "main_final_individual_panel_pdfs"),
  file.path(PROJECT_ROOT, "04_figures", "10P_V13_FINAL_MAIN_PANEL_SELECTION_AFTER_USER_UPLOADS_pdf", "main_final_individual_panel_pdfs"),
  file.path(PROJECT_ROOT, "04_figures", "10P_advanced_individual_source_panel_pdf_export_V11_FIX_FIG9_DUPLICATE_pdf", "main_individual_panel_pdfs"),
  file.path(PROJECT_ROOT, "04_figures", "10P_advanced_individual_source_panel_pdf_export_V10_FUNCTION_SAFE_STRICT_REVIEW_pdf", "main_individual_panel_pdfs")
)

input_supp_candidates <- c(
  file.path(PROJECT_ROOT, "04_figures", "10P_FINAL_V14_NO_ASSEMBLY_SOURCE_PANEL_PACKAGE_pdf", "supplementary_final_individual_panel_pdfs"),
  file.path(PROJECT_ROOT, "04_figures", "10P_V13_FINAL_MAIN_PANEL_SELECTION_AFTER_USER_UPLOADS_pdf", "supplementary_final_individual_panel_pdfs"),
  file.path(PROJECT_ROOT, "04_figures", "10P_advanced_individual_source_panel_pdf_export_V11_FIX_FIG9_DUPLICATE_pdf", "supplementary_individual_panel_pdfs"),
  file.path(PROJECT_ROOT, "04_figures", "10P_advanced_individual_source_panel_pdf_export_V10_FUNCTION_SAFE_STRICT_REVIEW_pdf", "supplementary_individual_panel_pdfs")
)

input_main_dir <- find_existing_dir_10p(input_main_candidates)
input_supp_dir <- find_existing_dir_10p(input_supp_candidates)

out_table_dir <- file.path(PROJECT_ROOT, "03_tables", VERSION_TAG)
out_text_dir  <- file.path(PROJECT_ROOT, "09_manuscript", VERSION_TAG)
out_pdf_dir   <- file.path(PROJECT_ROOT, "04_figures", paste0(VERSION_TAG, "_pdf"))
out_main_dir  <- file.path(out_pdf_dir, "main_final_individual_panel_pdfs")
out_supp_dir  <- file.path(out_pdf_dir, "supplementary_final_individual_panel_pdfs")
out_all_dir   <- file.path(out_pdf_dir, "all_final_individual_panel_pdfs")
out_dup_dir   <- file.path(out_pdf_dir, "removed_duplicate_or_diagnostic_panels")

for (dd in c(out_table_dir, out_text_dir, out_pdf_dir, out_main_dir, out_supp_dir, out_all_dir, out_dup_dir)) dir_create_10p(dd)

cat("[10P V15] Project root:", PROJECT_ROOT, "\n")
cat("[10P V15] Input main dir:", input_main_dir, "\n")
cat("[10P V15] Input supp dir:", input_supp_dir, "\n")
cat("[10P V15] Output PDFs  :", out_pdf_dir, "\n")

if (is.na(input_main_dir) || !dir.exists(input_main_dir)) {
  stop("[10P V15] Cannot find input main panel PDF directory. Run 10P V14 first, then rerun V15.")
}

main_copy_log <- copy_pdf_set_10p(input_main_dir, out_main_dir)
supp_copy_log <- copy_pdf_set_10p(input_supp_dir, out_supp_dir)

write_csv_10p(main_copy_log, file.path(out_table_dir, "10P_V15_main_copy_log.csv"))
write_csv_10p(supp_copy_log, file.path(out_table_dir, "10P_V15_supplementary_copy_log.csv"))

main_pdfs <- list.files(out_main_dir, pattern = "\\.pdf$", full.names = TRUE, recursive = FALSE)
fig9_pdfs <- main_pdfs[vapply(main_pdfs, is_fig9_file_10p, logical(1))]

fig9_manifest <- data.frame(
  output_pdf = normalize_slash_10p(fig9_pdfs),
  file_name = basename(fig9_pdfs),
  panel_guess = ifelse(vapply(fig9_pdfs, is_panel_9a_10p, logical(1)), "9A",
                       ifelse(vapply(fig9_pdfs, is_panel_9b_10p, logical(1)), "9B",
                              ifelse(vapply(fig9_pdfs, is_panel_9c_10p, logical(1)), "9C", "UNKNOWN"))),
  md5 = safe_md5_10p(fig9_pdfs),
  stringsAsFactors = FALSE
)

write_csv_10p(fig9_manifest, file.path(out_table_dir, "10P_V15_figure9_pre_fix_manifest.csv"))

removed_rows <- data.frame()
fix_notes <- c()

row_9b <- which(fig9_manifest$panel_guess == "9B")
row_9c <- which(fig9_manifest$panel_guess == "9C")

if (length(row_9b) > 0 && length(row_9c) > 0) {
  md5_9b <- fig9_manifest$md5[row_9b[1]]
  md5_9c <- fig9_manifest$md5[row_9c[1]]
  if (!is.na(md5_9b) && !is.na(md5_9c) && identical(md5_9b, md5_9c)) {
    file_9c <- fig9_manifest$output_pdf[row_9c[1]]
    archive_9c <- file.path(out_dup_dir, paste0("REMOVED_DUPLICATE_OF_9B__", basename(file_9c)))
    file.copy(file_9c, archive_9c, overwrite = TRUE)
    unlink(file_9c)
    removed_rows <- rbind(removed_rows, data.frame(
      removed_panel = "Figure 9C",
      removed_pdf = normalize_slash_10p(file_9c),
      archived_to = normalize_slash_10p(archive_9c),
      reason = "Figure 9C had identical md5/content to Figure 9B; duplicate main panel removed.",
      stringsAsFactors = FALSE
    ))
    fix_notes <- c(fix_notes, "Figure 9C removed from MAIN because it was an md5-identical duplicate of Figure 9B.")
  }
}

main_pdfs_after_explicit <- list.files(out_main_dir, pattern = "\\.pdf$", full.names = TRUE, recursive = FALSE)
fig9_after <- main_pdfs_after_explicit[vapply(main_pdfs_after_explicit, is_fig9_file_10p, logical(1))]
if (length(fig9_after) > 1) {
  md5_after <- safe_md5_10p(fig9_after)
  seen <- character(0)
  for (ii in seq_along(fig9_after)) {
    current_md5 <- md5_after[ii]
    if (is.na(current_md5) || current_md5 == "") next
    if (current_md5 %in% seen) {
      src_dup <- fig9_after[ii]
      archive_dup <- file.path(out_dup_dir, paste0("REMOVED_GENERIC_FIG9_DUPLICATE__", basename(src_dup)))
      file.copy(src_dup, archive_dup, overwrite = TRUE)
      unlink(src_dup)
      removed_rows <- rbind(removed_rows, data.frame(
        removed_panel = basename(src_dup),
        removed_pdf = normalize_slash_10p(src_dup),
        archived_to = normalize_slash_10p(archive_dup),
        reason = "Generic Figure 9 duplicate md5/content removed from MAIN.",
        stringsAsFactors = FALSE
      ))
      fix_notes <- c(fix_notes, paste0("Generic Figure 9 duplicate removed: ", basename(src_dup)))
    } else {
      seen <- c(seen, current_md5)
    }
  }
}

if (nrow(removed_rows) == 0) {
  removed_rows <- data.frame(
    removed_panel = character(0),
    removed_pdf = character(0),
    archived_to = character(0),
    reason = character(0),
    stringsAsFactors = FALSE
  )
  fix_notes <- c(fix_notes, "No Figure 9B/9C md5-identical duplicate was detected in the copied V15 main package.")
}
write_csv_10p(removed_rows, file.path(out_table_dir, "10P_V15_removed_duplicate_or_diagnostic_panels.csv"))

final_main <- list.files(out_main_dir, pattern = "\\.pdf$", full.names = TRUE, recursive = FALSE)
final_supp <- list.files(out_supp_dir, pattern = "\\.pdf$", full.names = TRUE, recursive = FALSE)

if (length(final_main) > 0) file.copy(final_main, file.path(out_all_dir, paste0("MAIN__", basename(final_main))), overwrite = TRUE)
if (length(final_supp) > 0) file.copy(final_supp, file.path(out_all_dir, paste0("SUPP__", basename(final_supp))), overwrite = TRUE)

final_main <- list.files(out_main_dir, pattern = "\\.pdf$", full.names = TRUE, recursive = FALSE)
final_supp <- list.files(out_supp_dir, pattern = "\\.pdf$", full.names = TRUE, recursive = FALSE)
final_all <- c(final_main, final_supp)

final_main_manifest <- data.frame(
  role = "main",
  pdf_path = normalize_slash_10p(final_main),
  file_name = basename(final_main),
  md5 = safe_md5_10p(final_main),
  stringsAsFactors = FALSE
)
final_supp_manifest <- data.frame(
  role = "supplementary",
  pdf_path = normalize_slash_10p(final_supp),
  file_name = basename(final_supp),
  md5 = safe_md5_10p(final_supp),
  stringsAsFactors = FALSE
)
final_manifest <- rbind(final_main_manifest, final_supp_manifest)
write_csv_10p(final_main_manifest, file.path(out_table_dir, "10P_V15_final_main_individual_panel_manifest.csv"))
write_csv_10p(final_supp_manifest, file.path(out_table_dir, "10P_V15_final_supplementary_individual_panel_manifest.csv"))
write_csv_10p(final_manifest, file.path(out_table_dir, "10P_V15_final_all_individual_panel_manifest.csv"))

main_dup_md5 <- final_main_manifest$md5[duplicated(final_main_manifest$md5) & !is.na(final_main_manifest$md5)]
main_duplicate_rows <- final_main_manifest[final_main_manifest$md5 %in% main_dup_md5, , drop = FALSE]
write_csv_10p(main_duplicate_rows, file.path(out_table_dir, "10P_V15_remaining_main_duplicate_md5_rows.csv"))

final_fig9 <- final_main_manifest[vapply(final_main_manifest$pdf_path, is_fig9_file_10p, logical(1)), , drop = FALSE]
fig9_dup_md5 <- final_fig9$md5[duplicated(final_fig9$md5) & !is.na(final_fig9$md5)]
fig9_duplicate_rows <- final_fig9[final_fig9$md5 %in% fig9_dup_md5, , drop = FALSE]
write_csv_10p(final_fig9, file.path(out_table_dir, "10P_V15_final_figure9_main_manifest.csv"))
write_csv_10p(fig9_duplicate_rows, file.path(out_table_dir, "10P_V15_remaining_figure9_duplicate_md5_rows.csv"))

manual_required <- data.frame()
if (nrow(fig9_duplicate_rows) > 0) {
  manual_required <- rbind(manual_required, data.frame(
    issue = "Figure 9 still has duplicate md5/content in MAIN after V15 cleanup.",
    action_required = "Manually inspect 10P_V15_final_figure9_main_manifest.csv and remove/replace the duplicated panel.",
    stringsAsFactors = FALSE
  ))
}
if (nrow(main_duplicate_rows) > 0) {
  manual_required <- rbind(manual_required, data.frame(
    issue = "Some MAIN panels still share identical md5/content after V15 cleanup.",
    action_required = "Inspect 10P_V15_remaining_main_duplicate_md5_rows.csv before 10Q.",
    stringsAsFactors = FALSE
  ))
}
if (nrow(manual_required) == 0) {
  manual_required <- data.frame(issue = character(0), action_required = character(0), stringsAsFactors = FALSE)
}
write_csv_10p(manual_required, file.path(out_table_dir, "10P_V15_manual_confirmation_required.csv"))

fig9_unique_main_count <- length(unique(final_fig9$md5[!is.na(final_fig9$md5)]))
fig9_main_count <- nrow(final_fig9)
remaining_fig9_dup <- nrow(fig9_duplicate_rows)
remaining_main_dup <- nrow(main_duplicate_rows)

if (remaining_fig9_dup == 0 && remaining_main_dup == 0) {
  decision <- "READY_FOR_10Q_SOURCE_PANEL_VISUAL_AUDIT"
} else {
  decision <- "NOT_READY_REVIEW_REMAINING_DUPLICATES"
}

execution_summary <- data.frame(
  version = VERSION_TAG,
  input_main_dir = normalize_slash_10p(input_main_dir),
  input_supp_dir = normalize_slash_10p(input_supp_dir),
  output_pdf_dir = normalize_slash_10p(out_pdf_dir),
  main_pdf_count = length(final_main),
  supplementary_pdf_count = length(final_supp),
  figure9_main_panel_count = fig9_main_count,
  figure9_unique_md5_count = fig9_unique_main_count,
  removed_duplicate_count = nrow(removed_rows),
  remaining_figure9_duplicate_rows = remaining_fig9_dup,
  remaining_main_duplicate_rows = remaining_main_dup,
  decision = decision,
  stringsAsFactors = FALSE
)
write_csv_10p(execution_summary, file.path(out_table_dir, "10P_V15_execution_summary.csv"))

report_path <- file.path(out_text_dir, "10P_V15_execution_report.txt")
con <- file(report_path, open = "wt", encoding = "UTF-8")
writeLines(c(
  "10P FINAL V15 - FIGURE 9B/9C DUPLICATE FIX",
  "================================================",
  paste0("Version: ", VERSION_TAG),
  paste0("Input main dir: ", normalize_slash_10p(input_main_dir)),
  paste0("Input supp dir: ", normalize_slash_10p(input_supp_dir)),
  paste0("Output PDF dir: ", normalize_slash_10p(out_pdf_dir)),
  "",
  "What this script does:",
  "  - Copies the final no-assembly individual panel PDFs from 10P V14/V13/V11 fallback.",
  "  - Does not assemble multi-panel figures.",
  "  - Does not add A/B/C labels.",
  "  - Does not output PNG/TIFF/JPG.",
  "  - Removes Figure 9C from MAIN if it is md5-identical to Figure 9B.",
  "  - Archives removed duplicate panels in removed_duplicate_or_diagnostic_panels.",
  "",
  "Final Figure 9 policy:",
  "  - Do not keep duplicate panels in main figures.",
  "  - If only two non-duplicate informative Figure 9 panels remain, Figure 9 becomes a 2-panel main figure.",
  "  - Duplicate/coverage-diagnostic panels should be supplementary or removed from the main package.",
  "",
  paste0("Figure 9 main panel count after V15: ", fig9_main_count),
  paste0("Figure 9 unique md5 count after V15: ", fig9_unique_main_count),
  paste0("Removed duplicate count: ", nrow(removed_rows)),
  paste0("Remaining Figure 9 duplicate rows: ", remaining_fig9_dup),
  paste0("Remaining main duplicate rows: ", remaining_main_dup),
  paste0("Decision: ", decision),
  "",
  "Fix notes:",
  paste0("  - ", fix_notes),
  "",
  "Next:",
  "  If 10P_V15_manual_confirmation_required.csv is empty, proceed to 10Q visual audit.",
  "  10Q must still visually inspect source suitability, titles, legends, and claim boundaries."
), con)
close(con)
cat("[10P V15] Wrote report:", report_path, "\n")

cat("\n[10P V15] Completed final Figure 9 duplicate fix package.\n")
cat("[10P V15] Main PDFs:", length(final_main), "\n")
cat("[10P V15] Supplementary PDFs:", length(final_supp), "\n")
cat("[10P V15] Figure 9 main panels:", fig9_main_count, "\n")
cat("[10P V15] Figure 9 unique md5:", fig9_unique_main_count, "\n")
cat("[10P V15] Removed duplicate count:", nrow(removed_rows), "\n")
cat("[10P V15] Remaining Figure 9 duplicate rows:", remaining_fig9_dup, "\n")
cat("[10P V15] Decision:", decision, "\n")
cat("[10P V15] Output PDFs:", out_pdf_dir, "\n")
cat("[10P V15] Check:", file.path(out_table_dir, "10P_V15_manual_confirmation_required.csv"), "\n")
