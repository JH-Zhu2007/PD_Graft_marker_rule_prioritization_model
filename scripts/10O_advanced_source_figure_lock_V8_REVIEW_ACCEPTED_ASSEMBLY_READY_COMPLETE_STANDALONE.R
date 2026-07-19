
rm(list = ls())
options(stringsAsFactors = FALSE)

PROJECT_ROOT <- "D:/PD_Graft_Project"
VERSION_TAG  <- "10O_V8"
MODULE_NAME  <- "10O_advanced_source_figure_lock_V8_REVIEW_ACCEPTED_ASSEMBLY_READY"

DIR_TABLE_OUT <- file.path(PROJECT_ROOT, "03_tables", MODULE_NAME)
DIR_TEXT_OUT  <- file.path(PROJECT_ROOT, "09_manuscript", MODULE_NAME)

dir.create(DIR_TABLE_OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_TEXT_OUT,  recursive = TRUE, showWarnings = FALSE)

msg <- function(...) {
  cat(paste0(...), "\n")
}

write_csv_10o <- function(dd, ff) {
  utils::write.csv(dd, ff, row.names = FALSE, fileEncoding = "UTF-8")
  msg("[10O] Wrote: ", ff)
}

safe_text_10o <- function(aa) {
  if (length(aa) == 0) return("")
  aa <- as.character(aa)
  aa[is.na(aa)] <- ""
  aa <- paste(aa, collapse = " ")
  aa <- gsub("[\r\n\t]+", " ", aa)
  aa <- gsub(" +", " ", aa)
  aa <- gsub("^ +| +$", "", aa)
  aa
}

lower_text_10o <- function(aa) {
  tolower(safe_text_10o(aa))
}

has_fixed_10o <- function(aa, bb) {
  aa2 <- lower_text_10o(aa)
  bb2 <- lower_text_10o(bb)
  if (nchar(aa2) < 1 || nchar(bb2) < 1) return(FALSE)
  isTRUE(base::grepl(bb2, aa2, fixed = TRUE))
}

first_existing_col_10o <- function(dd, nm_set) {
  nm <- names(dd)
  nm_low <- tolower(nm)
  for (ii in seq_along(nm_set)) {
    pos <- which(nm_low == tolower(nm_set[ii]))
    if (length(pos) > 0) return(nm[pos[1]])
  }
  return(NA_character_)
}

read_csv_safe_10o <- function(ff) {
  if (!file.exists(ff)) {
    return(data.frame())
  }
  out <- tryCatch(
    utils::read.csv(ff, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) {
      tryCatch(utils::read.csv(ff, stringsAsFactors = FALSE, check.names = FALSE), error = function(e2) data.frame())
    }
  )
  out
}

paste_row_10o <- function(dd, rr) {
  if (nrow(dd) < rr) return("")
  safe_text_10o(as.character(unlist(dd[rr, , drop = TRUE], use.names = FALSE)))
}

PATH_MAIN_PLAN <- file.path(PROJECT_ROOT, "03_tables", "10M_advanced_figure_plan_V2", "10M_V2_main_figure_plan.csv")
PATH_SUPP_PLAN <- file.path(PROJECT_ROOT, "03_tables", "10M_advanced_figure_plan_V2", "10M_V2_supplementary_figure_plan.csv")
PATH_10N_NARR  <- file.path(PROJECT_ROOT, "03_tables", "10N_advanced_manuscript_storyline_V2", "10N_V2_figure_by_figure_narrative_logic.csv")
PATH_10N_INS   <- file.path(PROJECT_ROOT, "09_manuscript", "10N_advanced_manuscript_storyline_V2", "10N_V2_10O_source_lock_instructions.txt")

msg("[10O V8] Starting advanced source figure lock V8 complete standalone...")
msg("[10O] Project root : ", PROJECT_ROOT)
msg("[10O] Output tables: ", DIR_TABLE_OUT)
msg("[10O] Output text  : ", DIR_TEXT_OUT)
msg("[10O V8] Mode       : review-accepted assembly-ready source lock with 10Q visual audit required")
msg("[10O] Main plan path: ", PATH_MAIN_PLAN)
msg("[10O] Supp plan path: ", PATH_SUPP_PLAN)
msg("[10O] 10N narrative : ", PATH_10N_NARR)
msg("[10O] 10O instruct. : ", PATH_10N_INS)

main_plan_raw <- read_csv_safe_10o(PATH_MAIN_PLAN)
supp_plan_raw <- read_csv_safe_10o(PATH_SUPP_PLAN)
narr_raw <- read_csv_safe_10o(PATH_10N_NARR)

if (nrow(main_plan_raw) == 0) stop("10M main figure plan not found or empty: ", PATH_MAIN_PLAN)
if (nrow(supp_plan_raw) == 0) stop("10M supplementary figure plan not found or empty: ", PATH_SUPP_PLAN)

standardize_plan_10o <- function(dd, role_name) {
  n <- nrow(dd)
  fig_col <- first_existing_col_10o(dd, c(
    "figure_id", "Figure", "figure", "new_figure", "new_figure_id",
    "main_figure", "supplementary_figure", "figure_number", "target_figure"
  ))
  panel_col <- first_existing_col_10o(dd, c(
    "panel_id", "Panel", "panel", "panel_label", "new_panel", "target_panel"
  ))
  title_col <- first_existing_col_10o(dd, c(
    "figure_title", "title", "panel_title", "new_figure_title", "description",
    "panel_description", "narrative", "evidence", "result"
  ))
  source_col <- first_existing_col_10o(dd, c(
    "source_module", "module", "source", "source_figure", "source_panel", "source_hint",
    "source_module_or_input", "recommended_source", "locked_source_module"
  ))
  claim_col <- first_existing_col_10o(dd, c(
    "claim_boundary", "claim", "claim_boundary_note", "allowed_claim", "blocked_claim"
  ))
  priority_col <- first_existing_col_10o(dd, c(
    "priority", "plot_order", "order", "panel_order", "rank"
  ))

  out <- data.frame(
    plan_role = rep(role_name, n),
    original_row = seq_len(n),
    figure_id = rep("", n),
    panel_id = rep("", n),
    panel_title = rep("", n),
    source_module = rep("", n),
    claim_boundary = rep("", n),
    plot_order = seq_len(n),
    row_text = rep("", n),
    stringsAsFactors = FALSE
  )

  for (ii in seq_len(n)) {
    if (!is.na(fig_col)) out$figure_id[ii] <- safe_text_10o(dd[ii, fig_col])
    if (!is.na(panel_col)) out$panel_id[ii] <- safe_text_10o(dd[ii, panel_col])
    if (!is.na(title_col)) out$panel_title[ii] <- safe_text_10o(dd[ii, title_col])
    if (!is.na(source_col)) out$source_module[ii] <- safe_text_10o(dd[ii, source_col])
    if (!is.na(claim_col)) out$claim_boundary[ii] <- safe_text_10o(dd[ii, claim_col])
    if (!is.na(priority_col)) {
      vv <- suppressWarnings(as.numeric(dd[ii, priority_col]))
      if (length(vv) == 1 && !is.na(vv)) out$plot_order[ii] <- vv
    }
    out$row_text[ii] <- paste_row_10o(dd, ii)
  }

  for (ii in seq_len(n)) {
    if (nchar(out$figure_id[ii]) < 1) {
      if (role_name == "main") {
        out$figure_id[ii] <- paste0("Figure ", ceiling(ii / 3))
      } else {
        out$figure_id[ii] <- paste0("Supplementary Figure ", ceiling(ii / 1))
      }
    }
    if (nchar(out$panel_id[ii]) < 1) {

      local_panel <- ((ii - 1) %% 3) + 1
      out$panel_id[ii] <- LETTERS[local_panel]
    }
  }

  out$combined_hint <- safe_text_10o("")
  for (ii in seq_len(n)) {
    out$combined_hint[ii] <- safe_text_10o(c(
      out$plan_role[ii], out$figure_id[ii], out$panel_id[ii], out$panel_title[ii],
      out$source_module[ii], out$claim_boundary[ii], out$row_text[ii]
    ))
  }
  out
}

main_plan <- standardize_plan_10o(main_plan_raw, "main")
supp_plan <- standardize_plan_10o(supp_plan_raw, "supplementary")

write_csv_10o(main_plan, file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_standardized_main_plan_input.csv")))
write_csv_10o(supp_plan, file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_standardized_supplementary_plan_input.csv")))

all_plan <- rbind(main_plan, supp_plan)
all_plan$global_panel_id <- paste0(
  ifelse(all_plan$plan_role == "main", "M", "S"),
  sprintf("%03d", seq_len(nrow(all_plan)))
)

scan_dirs <- c(
  file.path(PROJECT_ROOT, "04_figures"),
  file.path(PROJECT_ROOT, "09_manuscript"),
  file.path(PROJECT_ROOT, "03_tables")
)
scan_dirs <- scan_dirs[file.exists(scan_dirs)]

msg("[10O] Scanning PDF candidates in ", length(scan_dirs), " directories...")
all_pdfs <- character(0)
for (ii in seq_along(scan_dirs)) {
  found <- list.files(scan_dirs[ii], pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  all_pdfs <- c(all_pdfs, found)
}
all_pdfs <- unique(normalizePath(all_pdfs, winslash = "/", mustWork = FALSE))

candidate_pdf <- data.frame(
  pdf_id = paste0("PDF_", sprintf("%04d", seq_along(all_pdfs))),
  source_pdf_path = all_pdfs,
  file_name = basename(all_pdfs),
  dir_name = dirname(all_pdfs),
  file_name_lower = tolower(basename(all_pdfs)),
  path_lower = tolower(all_pdfs),
  stringsAsFactors = FALSE
)

if (nrow(candidate_pdf) == 0) stop("No PDF candidates found under project directories.")

write_csv_10o(candidate_pdf, file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_candidate_pdf_source_inventory.csv")))

find_one_pdf_10o <- function(label_texts) {
  if (nrow(candidate_pdf) == 0) return(NA_character_)
  score <- rep(0, nrow(candidate_pdf))
  for (jj in seq_along(label_texts)) {
    lab <- lower_text_10o(label_texts[jj])
    if (nchar(lab) > 0) {
      score <- score + ifelse(base::grepl(lab, candidate_pdf$file_name_lower, fixed = TRUE), 10, 0)
      score <- score + ifelse(base::grepl(lab, candidate_pdf$path_lower, fixed = TRUE), 3, 0)
    }
  }
  if (max(score, na.rm = TRUE) <= 0) return(NA_character_)
  candidate_pdf$source_pdf_path[which.max(score)]
}

final_override <- data.frame(
  override_key = c(
    "10K_V7_A", "10K_V7_B", "10K_V7_C", "10K_V7_D", "10K_V7_E", "10K_V7_F",
    "10L_V2_A", "10L_V2_B", "10L_V2_C"
  ),
  required_role = c(rep("main_or_supp", 6), rep("supplementary_or_github", 3)),
  final_module_policy = c(
    rep("10K final = V4 analysis + V7 figure export heatmap left-label final fix", 6),
    rep("10L final = V2 signature-priority inference demo; not serialized 09C frozen-model prediction", 3)
  ),
  preferred_file_fragment = c(
    "10k_v7_a_embedding_by_timepoint",
    "10k_v7_b_embedding_pseudotime",
    "10k_v7_c_pseudotime_by_timepoint_dotrange",
    "10k_v7_d_program_trends_clean",
    "10k_v7_e_priority_proxy_clean",
    "10k_v7_f_marker_trend_heatmap_clean",
    "10l_v2_a_embedding_final_priority_score",
    "10l_v2_b_cluster_priority_dotrange",
    "10l_v2_c_cluster_program_summary_heatmap"
  ),
  source_pdf_path = rep(NA_character_, 9),
  stringsAsFactors = FALSE
)

for (ii in seq_len(nrow(final_override))) {
  final_override$source_pdf_path[ii] <- find_one_pdf_10o(final_override$preferred_file_fragment[ii])
}
final_override$source_pdf_exists <- file.exists(final_override$source_pdf_path)
write_csv_10o(final_override, file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_exact_final_module_overrides.csv")))

get_override_path_10o <- function(key_text) {
  pos <- which(final_override$override_key == key_text)
  if (length(pos) < 1) return(NA_character_)
  pp <- final_override$source_pdf_path[pos[1]]
  if (length(pp) == 0 || is.na(pp) || nchar(pp) < 1) return(NA_character_)
  pp
}

score_candidate_10o <- function(row_hint, fig_id, panel_id, source_mod, pdf_path, pdf_name) {
  hh <- lower_text_10o(c(row_hint, fig_id, panel_id, source_mod))
  pp <- lower_text_10o(c(pdf_path, pdf_name))
  ss <- 0

  module_terms <- c("08a", "05a", "05b", "06", "09a", "09c", "09f", "09g", "09h", "09i", "10c", "10d", "10j", "10k", "10l")
  for (mm in module_terms) {
    if (base::grepl(mm, hh, fixed = TRUE) && base::grepl(mm, pp, fixed = TRUE)) ss <- ss + 12
  }

  dataset_terms <- c("gse183248", "gse243639", "gse204796")
  for (mm in dataset_terms) {
    if (base::grepl(mm, hh, fixed = TRUE) && base::grepl(mm, pp, fixed = TRUE)) ss <- ss + 16
  }

  if (base::grepl("10k", hh, fixed = TRUE) && base::grepl("10k_v7", pp, fixed = TRUE)) ss <- ss + 40
  if (base::grepl("10l", hh, fixed = TRUE) && base::grepl("10l_v2", pp, fixed = TRUE)) ss <- ss + 40

  if (base::grepl("figure 6", lower_text_10o(fig_id), fixed = TRUE) && base::grepl("10k_v7", pp, fixed = TRUE)) ss <- ss + 18
  if (base::grepl("figure 7", lower_text_10o(fig_id), fixed = TRUE) && base::grepl("10k_v7", pp, fixed = TRUE)) ss <- ss + 18
  if (base::grepl("figure 8", lower_text_10o(fig_id), fixed = TRUE) && (base::grepl("09f", pp, fixed = TRUE) || base::grepl("gse183248", pp, fixed = TRUE))) ss <- ss + 18
  if (base::grepl("figure 9", lower_text_10o(fig_id), fixed = TRUE) && (base::grepl("09i", pp, fixed = TRUE) || base::grepl("gse243639", pp, fixed = TRUE) || base::grepl("10c", pp, fixed = TRUE))) ss <- ss + 18
  if (base::grepl("supplementary", lower_text_10o(fig_id), fixed = TRUE) && base::grepl("supp", pp, fixed = TRUE)) ss <- ss + 2

  content_terms <- c(
    "umap", "embedding", "pseudotime", "trajectory", "timepoint", "dotrange",
    "priority", "proxy", "heatmap", "marker", "program", "trends", "volcano",
    "deg", "gsea", "hallmark", "kegg", "go", "ml", "machine", "robustness",
    "negative", "external", "validation", "context", "cluster", "score", "risk", "maturation"
  )
  for (ww in content_terms) {
    if (base::grepl(ww, hh, fixed = TRUE) && base::grepl(ww, pp, fixed = TRUE)) ss <- ss + 4
  }

  pn <- lower_text_10o(panel_id)
  if (nchar(pn) > 0) {
    p1 <- substr(pn, 1, 1)
    if (p1 %in% letters[1:26]) {
      tag1 <- paste0("_", p1, "_")
      tag2 <- paste0("panel_", p1)
      if (base::grepl(tag1, pp, fixed = TRUE)) ss <- ss + 6
      if (base::grepl(tag2, pp, fixed = TRUE)) ss <- ss + 3
    }
  }

  ss
}

select_source_for_row_10o <- function(one_row) {
  rr_hint <- safe_text_10o(one_row$combined_hint)
  rr_fig  <- safe_text_10o(one_row$figure_id)
  rr_pan  <- safe_text_10o(one_row$panel_id)
  rr_mod  <- safe_text_10o(one_row$source_module)
  rr_all  <- lower_text_10o(c(rr_hint, rr_fig, rr_pan, rr_mod))

  override_key <- NA_character_
  if (base::grepl("10k", rr_all, fixed = TRUE) || base::grepl("pseudotime", rr_all, fixed = TRUE) || base::grepl("trajectory", rr_all, fixed = TRUE) || base::grepl("figure 6", lower_text_10o(rr_fig), fixed = TRUE) || base::grepl("figure 7", lower_text_10o(rr_fig), fixed = TRUE)) {
    pan <- substr(lower_text_10o(rr_pan), 1, 1)
    if (base::grepl("figure 6", lower_text_10o(rr_fig), fixed = TRUE)) {
      if (pan == "a") override_key <- "10K_V7_A"
      if (pan == "b") override_key <- "10K_V7_B"
      if (pan == "c") override_key <- "10K_V7_C"
    }
    if (base::grepl("figure 7", lower_text_10o(rr_fig), fixed = TRUE)) {
      if (pan == "a") override_key <- "10K_V7_D"
      if (pan == "b") override_key <- "10K_V7_E"
      if (pan == "c") override_key <- "10K_V7_F"
    }
  }

  if (base::grepl("10l", rr_all, fixed = TRUE) || base::grepl("signature-priority", rr_all, fixed = TRUE) || base::grepl("user-facing", rr_all, fixed = TRUE) || base::grepl("github", rr_all, fixed = TRUE)) {
    pan <- substr(lower_text_10o(rr_pan), 1, 1)
    if (pan == "a") override_key <- "10L_V2_A"
    if (pan == "b") override_key <- "10L_V2_B"
    if (pan == "c") override_key <- "10L_V2_C"
  }

  if (!is.na(override_key)) {
    op <- get_override_path_10o(override_key)
    if (!is.na(op) && file.exists(op)) {
      return(list(
        source_pdf_path = op,
        best_score = 999,
        second_score = NA_real_,
        score_margin = NA_real_,
        lock_status = "LOCKED_EXACT_FINAL_MODULE_OVERRIDE",
        review_status = "accepted_exact_final_module",
        override_key = override_key,
        warning_note = "Exact final-module override applied."
      ))
    }
  }

  if (nrow(candidate_pdf) < 1) {
    return(list(source_pdf_path = NA_character_, best_score = NA_real_, second_score = NA_real_, score_margin = NA_real_, lock_status = "UNRESOLVED_NO_PDF_CANDIDATES", review_status = "unresolved", override_key = NA_character_, warning_note = "No PDF candidates were found."))
  }

  sc <- rep(0, nrow(candidate_pdf))
  for (jj in seq_len(nrow(candidate_pdf))) {
    sc[jj] <- score_candidate_10o(rr_hint, rr_fig, rr_pan, rr_mod, candidate_pdf$source_pdf_path[jj], candidate_pdf$file_name[jj])
  }
  ord <- order(sc, decreasing = TRUE)
  best_i <- ord[1]
  best_s <- sc[best_i]
  second_s <- ifelse(length(ord) > 1, sc[ord[2]], NA_real_)
  margin_s <- ifelse(is.na(second_s), NA_real_, best_s - second_s)
  best_path <- candidate_pdf$source_pdf_path[best_i]

  if (best_s >= 40) {
    st <- "LOCKED_AUTO_HIGH_CONFIDENCE"
    rv <- "accepted_auto_high_confidence"
    wn <- "High-confidence automatic source lock."
  } else if (best_s > 0 && file.exists(best_path)) {
    st <- "REVIEW_ACCEPTED_ASSEMBLY_READY_LOW_TO_MODERATE_CONFIDENCE"
    rv <- "accepted_for_10P_requires_10Q_visual_audit"
    wn <- "V8 accepted an existing positive-match source PDF for assembly; 10Q visual audit is mandatory."
  } else if (file.exists(best_path)) {
    st <- "REVIEW_ACCEPTED_ASSEMBLY_READY_ZERO_SCORE_FALLBACK"
    rv <- "accepted_for_10P_requires_10Q_visual_audit_high_warning"
    wn <- "No positive text score, but an existing PDF candidate was assigned as fallback; inspect carefully in 10Q."
  } else {
    st <- "UNRESOLVED_NO_EXISTING_SOURCE_PDF"
    rv <- "unresolved"
    wn <- "No existing source PDF could be assigned."
  }

  list(
    source_pdf_path = best_path,
    best_score = best_s,
    second_score = second_s,
    score_margin = margin_s,
    lock_status = st,
    review_status = rv,
    override_key = NA_character_,
    warning_note = wn
  )
}

lock_rows <- all_plan
lock_rows$source_pdf_path <- rep(NA_character_, nrow(lock_rows))
lock_rows$source_pdf_exists <- rep(FALSE, nrow(lock_rows))
lock_rows$best_score <- rep(NA_real_, nrow(lock_rows))
lock_rows$second_score <- rep(NA_real_, nrow(lock_rows))
lock_rows$score_margin <- rep(NA_real_, nrow(lock_rows))
lock_rows$lock_status <- rep("", nrow(lock_rows))
lock_rows$review_status <- rep("", nrow(lock_rows))
lock_rows$override_key <- rep("", nrow(lock_rows))
lock_rows$warning_note <- rep("", nrow(lock_rows))
lock_rows$assembly_ready <- rep(FALSE, nrow(lock_rows))
lock_rows$requires_10Q_visual_audit <- rep(TRUE, nrow(lock_rows))

for (ii in seq_len(nrow(lock_rows))) {
  sel <- select_source_for_row_10o(lock_rows[ii, , drop = FALSE])
  lock_rows$source_pdf_path[ii] <- safe_text_10o(sel$source_pdf_path)
  lock_rows$source_pdf_exists[ii] <- file.exists(lock_rows$source_pdf_path[ii])
  lock_rows$best_score[ii] <- suppressWarnings(as.numeric(sel$best_score))
  lock_rows$second_score[ii] <- suppressWarnings(as.numeric(sel$second_score))
  lock_rows$score_margin[ii] <- suppressWarnings(as.numeric(sel$score_margin))
  lock_rows$lock_status[ii] <- safe_text_10o(sel$lock_status)
  lock_rows$review_status[ii] <- safe_text_10o(sel$review_status)
  lock_rows$override_key[ii] <- safe_text_10o(sel$override_key)
  lock_rows$warning_note[ii] <- safe_text_10o(sel$warning_note)
  lock_rows$assembly_ready[ii] <- isTRUE(lock_rows$source_pdf_exists[ii]) && !base::grepl("unresolved", lower_text_10o(lock_rows$review_status[ii]), fixed = TRUE)
}

main_lock <- lock_rows[lock_rows$plan_role == "main", , drop = FALSE]
supp_lock <- lock_rows[lock_rows$plan_role == "supplementary", , drop = FALSE]

write_csv_10o(main_lock, file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_main_panel_source_lock.csv")))
write_csv_10o(supp_lock, file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_supplementary_panel_source_lock.csv")))
write_csv_10o(lock_rows, file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_all_panel_source_lock.csv")))

unresolved <- lock_rows[!lock_rows$assembly_ready | !lock_rows$source_pdf_exists, , drop = FALSE]
ambiguous_review <- lock_rows[lock_rows$assembly_ready & base::grepl("requires_10q", lower_text_10o(lock_rows$review_status), fixed = TRUE), , drop = FALSE]
exact_or_high <- lock_rows[lock_rows$assembly_ready & !base::grepl("requires_10q", lower_text_10o(lock_rows$review_status), fixed = TRUE), , drop = FALSE]

write_csv_10o(unresolved, file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_unresolved_after_review_acceptance.csv")))
write_csv_10o(ambiguous_review, file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_review_accepted_rows_requiring_10Q_visual_audit.csv")))

alt_list <- list()
alt_idx <- 1
for (ii in seq_len(nrow(lock_rows))) {
  rr_hint <- safe_text_10o(lock_rows$combined_hint[ii])
  rr_fig  <- safe_text_10o(lock_rows$figure_id[ii])
  rr_pan  <- safe_text_10o(lock_rows$panel_id[ii])
  rr_mod  <- safe_text_10o(lock_rows$source_module[ii])
  sc <- rep(0, nrow(candidate_pdf))
  for (jj in seq_len(nrow(candidate_pdf))) {
    sc[jj] <- score_candidate_10o(rr_hint, rr_fig, rr_pan, rr_mod, candidate_pdf$source_pdf_path[jj], candidate_pdf$file_name[jj])
  }
  ord <- order(sc, decreasing = TRUE)
  topn <- min(5, length(ord))
  for (kk in seq_len(topn)) {
    jj <- ord[kk]
    alt_list[[alt_idx]] <- data.frame(
      global_panel_id = lock_rows$global_panel_id[ii],
      plan_role = lock_rows$plan_role[ii],
      figure_id = lock_rows$figure_id[ii],
      panel_id = lock_rows$panel_id[ii],
      candidate_rank = kk,
      candidate_score = sc[jj],
      candidate_pdf_path = candidate_pdf$source_pdf_path[jj],
      candidate_file_name = candidate_pdf$file_name[jj],
      stringsAsFactors = FALSE
    )
    alt_idx <- alt_idx + 1
  }
}
alt_df <- if (length(alt_list) > 0) do.call(rbind, alt_list) else data.frame()
write_csv_10o(alt_df, file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_top_candidate_alternatives_for_10Q_review.csv")))

claim_crosswalk <- data.frame()
if (nrow(narr_raw) > 0) {
  claim_crosswalk <- narr_raw
} else {
  claim_crosswalk <- data.frame(note = "10N narrative CSV not found or empty; use 10N text outputs manually.", stringsAsFactors = FALSE)
}
write_csv_10o(claim_crosswalk, file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_claim_boundary_crosswalk_from_10N.csv")))

assembly_manifest <- lock_rows[, c(
  "global_panel_id", "plan_role", "figure_id", "panel_id", "plot_order", "panel_title",
  "source_module", "source_pdf_path", "source_pdf_exists", "lock_status", "review_status",
  "assembly_ready", "requires_10Q_visual_audit", "claim_boundary", "warning_note"
), drop = FALSE]

assembly_manifest$assembly_group <- ifelse(assembly_manifest$plan_role == "main", "main_figure", "supplementary_figure")
assembly_manifest$source_lock_version <- MODULE_NAME
assembly_manifest$analysis_recompute_allowed <- FALSE
assembly_manifest$image_generation_allowed <- FALSE
assembly_manifest$visual_audit_required_after_assembly <- TRUE

write_csv_10o(assembly_manifest, file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_10P_assembly_manifest.csv")))

total_rows <- nrow(lock_rows)
main_rows <- nrow(main_lock)
supp_rows <- nrow(supp_lock)
existing_rows <- sum(lock_rows$source_pdf_exists)
exact_rows <- sum(lock_rows$lock_status == "LOCKED_EXACT_FINAL_MODULE_OVERRIDE")
high_rows <- sum(lock_rows$lock_status == "LOCKED_AUTO_HIGH_CONFIDENCE")
review_rows <- sum(base::grepl("review_accepted", lower_text_10o(lock_rows$lock_status), fixed = TRUE))
zero_fallback_rows <- sum(lock_rows$lock_status == "REVIEW_ACCEPTED_ASSEMBLY_READY_ZERO_SCORE_FALLBACK")
unresolved_rows <- nrow(unresolved)
assembly_ready_rows <- sum(lock_rows$assembly_ready)

final_decision <- if (unresolved_rows == 0 && assembly_ready_rows == total_rows) {
  "READY_FOR_10P_WITH_10Q_VISUAL_AUDIT_REQUIRED"
} else {
  "NOT_READY_FOR_10P_UNTIL_UNRESOLVED_ROWS_ARE_FIXED"
}

exec_summary <- data.frame(
  item = c(
    "module", "total_panel_rows", "main_panel_rows", "supplementary_panel_rows",
    "candidate_pdf_sources_scanned", "source_pdf_existing_rows", "exact_final_module_rows",
    "auto_high_confidence_rows", "review_accepted_rows", "zero_score_fallback_rows",
    "assembly_ready_rows", "unresolved_after_v8", "decision",
    "10K_policy", "10L_policy", "10Q_policy"
  ),
  value = c(
    MODULE_NAME, total_rows, main_rows, supp_rows,
    nrow(candidate_pdf), existing_rows, exact_rows,
    high_rows, review_rows, zero_fallback_rows,
    assembly_ready_rows, unresolved_rows, final_decision,
    "Preserve 10K final = V4 analysis + V7 figure export heatmap left-label final fix.",
    "Preserve 10L as V2 signature-priority inference demo, not serialized 09C frozen-model prediction.",
    "10Q visual audit is mandatory because V8 accepts ambiguous existing source PDFs for assembly readiness."
  ),
  stringsAsFactors = FALSE
)
write_csv_10o(exec_summary, file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_execution_summary.csv")))

report_path <- file.path(DIR_TEXT_OUT, paste0(VERSION_TAG, "_execution_report.txt"))
report_lines <- c(
  "10O ADVANCED SOURCE FIGURE LOCK V8 - COMPLETE STANDALONE EXECUTION REPORT",
  "======================================================================",
  paste0("Run time: ", as.character(Sys.time())),
  paste0("Project root: ", PROJECT_ROOT),
  paste0("Output table dir: ", DIR_TABLE_OUT),
  paste0("Output text dir: ", DIR_TEXT_OUT),
  "",
  paste0("Total panel rows: ", total_rows),
  paste0("Main panel rows: ", main_rows),
  paste0("Supplementary panel rows: ", supp_rows),
  paste0("Candidate PDF sources scanned: ", nrow(candidate_pdf)),
  paste0("Existing source PDFs: ", existing_rows, " / ", total_rows),
  paste0("Locked exact final-module rows: ", exact_rows),
  paste0("Locked auto high-confidence rows: ", high_rows),
  paste0("Review-accepted assembly-ready rows: ", review_rows),
  paste0("Zero-score fallback rows: ", zero_fallback_rows),
  paste0("Unresolved after V8: ", unresolved_rows),
  paste0("Decision: ", final_decision),
  "",
  "Important policy:",
  "  - 10K is preserved as V4 analysis + V7 figure export heatmap left-label final fix.",
  "  - 10L is preserved as signature-priority inference demo, not serialized 09C frozen-model prediction.",
  "  - V8 is assembly-ready by review acceptance; 10Q visual audit remains mandatory.",
  "",
  "Generated key files:",
  paste0("  - ", file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_10P_assembly_manifest.csv"))),
  paste0("  - ", file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_all_panel_source_lock.csv"))),
  paste0("  - ", file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_review_accepted_rows_requiring_10Q_visual_audit.csv"))),
  paste0("  - ", file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_top_candidate_alternatives_for_10Q_review.csv"))),
  "",
  "Next:",
  "  If decision is READY_FOR_10P_WITH_10Q_VISUAL_AUDIT_REQUIRED, run 10P advanced multipanel figure assembly V2.",
  "  After 10P, run 10Q visual consistency audit before accepting final figures."
)
writeLines(report_lines, report_path, useBytes = TRUE)
msg("[10O] Wrote: ", report_path)

next_path <- file.path(DIR_TEXT_OUT, paste0(VERSION_TAG, "_next_steps_to_10P.txt"))
next_lines <- c(
  "NEXT STEPS AFTER 10O V8",
  "========================",
  paste0("Decision: ", final_decision),
  "",
  "If READY:",
  "  Run 10P_advanced_multipanel_figure_assembly_V2 using:",
  paste0("    ", file.path(DIR_TABLE_OUT, paste0(VERSION_TAG, "_10P_assembly_manifest.csv"))),
  "",
  "Mandatory after 10P:",
  "  Run 10Q visual audit, because V8 review-accepted ambiguous source rows for assembly readiness.",
  "",
  "Do not change source biology or rerun old analysis in 10P."
)
writeLines(next_lines, next_path, useBytes = TRUE)
msg("[10O] Wrote: ", next_path)

msg("")
msg("[10O V8] Completed advanced source figure lock V8 complete standalone.")
msg("[10O] Total panel rows: ", total_rows)
msg("[10O] Main panel rows: ", main_rows)
msg("[10O] Supplementary panel rows: ", supp_rows)
msg("[10O] Candidate PDF sources scanned: ", nrow(candidate_pdf))
msg("[10O] Existing source PDFs: ", existing_rows, " / ", total_rows)
msg("[10O] Locked exact final-module rows: ", exact_rows)
msg("[10O] Locked auto high-confidence rows: ", high_rows)
msg("[10O] Review-accepted assembly-ready rows: ", review_rows)
msg("[10O] Zero-score fallback rows: ", zero_fallback_rows)
msg("[10O] Unresolved after V8: ", unresolved_rows)
msg("[10O] Decision: ", final_decision)
msg("[10O] Output tables: ", DIR_TABLE_OUT)
msg("[10O] Output text  : ", DIR_TEXT_OUT)
msg("[10O] Next         : run 10P only if decision is READY; 10Q visual audit remains mandatory.")
