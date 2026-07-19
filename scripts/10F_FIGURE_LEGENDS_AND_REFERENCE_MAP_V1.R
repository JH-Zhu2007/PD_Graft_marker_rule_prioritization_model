
options(
  stringsAsFactors = FALSE,
  scipen = 999
)

PROJECT_ROOT <- "D:/PD_Graft_Project"

INPUT_10D_TAG <- "10D_final_multipanel_figure_assembly_V17"
INPUT_10E_TAG <- "10E_final_consistency_audit_V2_FAST"

OUT_TAG <- "10F_figure_legends_and_reference_map_V1"

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

TEN_E_TABLE_DIR <- file.path(
  PROJECT_ROOT,
  "03_tables",
  INPUT_10E_TAG
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

OUT_MANUSCRIPT_DIR <- file.path(
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
  OUT_MANUSCRIPT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

OUT_MAIN_LEGEND_TABLE <- file.path(
  OUT_TABLE_DIR,
  "10F_V1_main_figure_legends_table.csv"
)

OUT_SUPP_LEGEND_TABLE <- file.path(
  OUT_TABLE_DIR,
  "10F_V1_supplementary_figure_legends_table.csv"
)

OUT_MAIN_PANEL_MAP <- file.path(
  OUT_TABLE_DIR,
  "10F_V1_final_main_figure_panel_map.csv"
)

OUT_SUPP_PANEL_MAP <- file.path(
  OUT_TABLE_DIR,
  "10F_V1_final_supplementary_figure_panel_map.csv"
)

OUT_REFERENCE_REPLACEMENT_MAP <- file.path(
  OUT_TABLE_DIR,
  "10F_V1_old_to_new_figure_reference_replacement_map.csv"
)

OUT_10G_ACTION_ITEMS <- file.path(
  OUT_TABLE_DIR,
  "10F_V1_action_items_for_10G.csv"
)

OUT_MAIN_LEGENDS_TXT <- file.path(
  OUT_MANUSCRIPT_DIR,
  "10F_V1_main_figure_legends_V17.txt"
)

OUT_SUPP_LEGENDS_TXT <- file.path(
  OUT_MANUSCRIPT_DIR,
  "10F_V1_supplementary_figure_legends_V17.txt"
)

OUT_COMBINED_LEGENDS_TXT <- file.path(
  OUT_MANUSCRIPT_DIR,
  "10F_V1_combined_figure_legends_V17.txt"
)

OUT_REFERENCE_UPDATE_NOTE <- file.path(
  OUT_MANUSCRIPT_DIR,
  "10F_V1_manuscript_reference_update_note_for_10G.txt"
)

OUT_REPORT <- file.path(
  OUT_MANUSCRIPT_DIR,
  "10F_V1_figure_legends_and_reference_map_report.txt"
)

OUT_SESSION <- file.path(
  OUT_MANUSCRIPT_DIR,
  "10F_V1_sessionInfo.txt"
)

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

write_utf8_lines <- function(lines, path) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  con <- file(
    path,
    open = "w",
    encoding = "UTF-8"
  )

  on.exit(
    close(con),
    add = TRUE
  )

  writeLines(
    lines,
    con = con,
    useBytes = TRUE
  )
}

collapse_panel_list <- function(df) {
  if (nrow(df) == 0L) {
    return("")
  }

  paste0(
    df$panel,
    ", ",
    df$panel_title,
    collapse = "; "
  )
}

stamp("Loading 10D V17 and 10E V2_FAST tables...")

source_panel_audit_path <- file.path(
  TEN_D_TABLE_DIR,
  "10D_V17_source_panel_render_audit.csv"
)

layout_policy_path <- file.path(
  TEN_D_TABLE_DIR,
  "10D_V17_layout_policy_exclusions_and_title_fixes.csv"
)

expected_main_plan_path <- file.path(
  TEN_E_TABLE_DIR,
  "10E_V2_FAST_expected_main_figure_storyline_plan.csv"
)

storyline_rule_path <- file.path(
  TEN_E_TABLE_DIR,
  "10E_V2_FAST_storyline_rule_audit.csv"
)

figure_output_audit_path <- file.path(
  TEN_E_TABLE_DIR,
  "10E_V2_FAST_final_figure_output_audit.csv"
)

required_inputs <- data.frame(
  input_name = c(
    "10D source panel audit",
    "10D layout policy audit",
    "10E expected main figure storyline plan",
    "10E storyline rule audit",
    "10E final figure output audit"
  ),
  path = c(
    source_panel_audit_path,
    layout_policy_path,
    expected_main_plan_path,
    storyline_rule_path,
    figure_output_audit_path
  ),
  stringsAsFactors = FALSE
)

required_inputs$exists <- file.exists(required_inputs$path)

if (any(!required_inputs$exists)) {
  print(required_inputs)
  stop("10F cannot continue because required 10D/10E inputs are missing.")
}

source_panel_audit <- safe_read_csv(source_panel_audit_path)
layout_policy_audit <- safe_read_csv(layout_policy_path)
expected_main_plan <- safe_read_csv(expected_main_plan_path)
storyline_rule_audit <- safe_read_csv(storyline_rule_path)
figure_output_audit <- safe_read_csv(figure_output_audit_path)

if (any(storyline_rule_audit$status != "PASS")) {
  print(storyline_rule_audit)
  stop("10E storyline rules are not all PASS. Do not write final legends until 10E passes.")
}

if (any(figure_output_audit$status != "PASS")) {
  print(figure_output_audit[figure_output_audit$status != "PASS", , drop = FALSE])
  stop("10E figure output audit contains failures. Do not write final legends until all final PDFs exist.")
}

main_panel_map <- source_panel_audit[
  source_panel_audit$item_type == "main",
  ,
  drop = FALSE
]

supp_panel_map <- source_panel_audit[
  source_panel_audit$item_type == "supplementary",
  ,
  drop = FALSE
]

main_panel_map <- main_panel_map[
  order(
    as.integer(gsub("[^0-9]", "", main_panel_map$figure_id)),
    main_panel_map$panel
  ),
  ,
  drop = FALSE
]

supp_panel_map <- supp_panel_map[
  order(
    as.integer(gsub("[^0-9]", "", supp_panel_map$figure_id)),
    supp_panel_map$panel
  ),
  ,
  drop = FALSE
]

atomic_write_csv(
  main_panel_map,
  OUT_MAIN_PANEL_MAP
)

atomic_write_csv(
  supp_panel_map,
  OUT_SUPP_PANEL_MAP
)

stamp("Writing main figure legends...")

main_legends <- data.frame(
  figure_id = paste0("Figure ", 1:10),
  figure_title = c(
    "Discovery atlas and transcriptional scoring atlas.",
    "Dataset prioritization and candidate-state molecular program.",
    "Differential expression between ideal-like and lower-priority states.",
    "Functional enrichment of candidate-state-associated transcriptional programs.",
    "Machine-learning model audit and cross-dataset generalization.",
    "Model feature interpretation and threshold-sensitivity stability.",
    "Negative-control robustness analysis.",
    "External validation in GSE183248.",
    "GSE243639 marker-targeted import and disease-context cluster landscape.",
    "GSE243639 disease-context molecular validation and priority scoring."
  ),
  legend_text = c(
    paste(
      "Figure 1. Discovery atlas and transcriptional scoring atlas.",
      "A, Representative discovery-dataset cluster-level UMAP used to visualize the frozen cell-state organization in the integrated PD graft analysis framework.",
      "B, Cluster-level DA/projection-associated molecular score, summarizing the transcriptomic enrichment of projection-associated molecular competence and A9/A10-like functional identity features.",
      "C, Safety-risk-associated transcriptional score, summarizing the relative enrichment of safety-risk-associated transcriptional programs across the same discovery-state space.",
      "All panels are transcriptomic prioritization outputs and should not be interpreted as direct anatomical projection, host integration, clinical safety, or functional graft efficacy measurements."
    ),
    paste(
      "Figure 2. Dataset prioritization and candidate-state molecular program.",
      "A, Dataset-level priority index summarizing the relative support for candidate ideal-like versus risk-associated states across discovery datasets under the frozen analysis framework.",
      "B, Candidate-state signature heatmap showing the expression pattern of locked candidate-state genes across prioritized cell states or clusters.",
      "Together, these panels connect the atlas-level scoring framework with the candidate-state molecular program used for downstream differential expression and enrichment analyses.",
      "The priority index is intended for transcriptomic prioritization rather than clinical classification or therapeutic prediction."
    ),
    paste(
      "Figure 3. Differential expression between ideal-like and lower-priority states.",
      "A, Volcano plot comparing ideal-like and lower-priority states under the locked candidate-state contrast.",
      "Genes with stronger differential signals support the molecular separation between prioritized candidate states and lower-priority transcriptional states.",
      "This figure is shown as a standalone main figure to improve readability and to separate differential-expression evidence from downstream functional enrichment summaries."
    ),
    paste(
      "Figure 4. Functional enrichment of candidate-state-associated transcriptional programs.",
      "A, Gene Ontology enrichment results for the candidate-state-associated differential expression program.",
      "B, KEGG pathway enrichment results for the same locked contrast.",
      "C, Hallmark GSEA normalized enrichment score summary.",
      "GO, KEGG and Hallmark outputs are grouped together because they represent complementary functional interpretations of the same candidate-state-associated transcriptional program.",
      "Enrichment results are used as molecular interpretation evidence and are not presented as proof of functional graft integration or therapeutic efficacy."
    ),
    paste(
      "Figure 5. Machine-learning model audit and cross-dataset generalization.",
      "A, Feature leakage and circularity audit demonstrating the separation between model features, labels and evaluation targets under the reduced-feature marker-rule-derived design.",
      "B, Internal cross-validation performance of the reduced-feature marker-rule-derived prioritization model.",
      "C, Leave-one-dataset-out performance testing cross-dataset generalization.",
      "The machine-learning model is used as a transcriptomic prioritization framework, not as a clinical classifier or safety-prediction model."
    ),
    paste(
      "Figure 6. Model feature interpretation and threshold-sensitivity stability.",
      "A, Normalized feature-importance summary across the locked reduced-feature model tasks.",
      "B, Threshold-sensitivity stability analysis evaluating whether the prioritization signal remains stable across alternative classification thresholds.",
      "These analyses support interpretability and robustness of the transcriptomic prioritization framework without implying causal or clinical prediction."
    ),
    paste(
      "Figure 7. Negative-control robustness analysis.",
      "A, Negative-control model performance.",
      "B, Empirical significance summary from the negative-control framework.",
      "These analyses test whether the observed prioritization performance exceeds expectation under control or permuted settings and provide robustness support for the frozen model interpretation."
    ),
    paste(
      "Figure 8. External validation in GSE183248.",
      "A, External priority index in GSE183248 under the frozen scoring and prioritization framework.",
      "B, Frozen-signature heatmap showing candidate-state gene-pattern behavior in the external dataset.",
      "C, Random-forest priority scatter summarizing the external prioritization landscape.",
      "GSE183248 is used as external transcriptomic validation of the prioritization framework and not as evidence for clinical efficacy, anatomical projection or graft functional integration."
    ),
    paste(
      "Figure 9. GSE243639 marker-targeted import and disease-context cluster landscape.",
      "A, Marker-targeted import summary for GSE243639.",
      "B, Disease-context cluster-size distribution after marker-targeted mapping.",
      "Because GSE243639 is marker-targeted rather than full-transcriptome discovery data, this analysis is interpreted as disease-context validation of predefined transcriptomic signatures rather than de novo full-transcriptome model retraining."
    ),
    paste(
      "Figure 10. GSE243639 disease-context molecular validation and priority scoring.",
      "A, Disease-context signature heatmap in GSE243639.",
      "B, Frozen predictor probability distribution across disease-context clusters.",
      "C, Context priority index summarizing the relative disease-context support for candidate versus risk-associated transcriptional programs.",
      "These panels extend the locked transcriptomic prioritization framework into a marker-targeted disease-context dataset while preserving the claim boundary of molecular prioritization."
    )
  ),
  claim_boundary_note = c(
    rep(
      "Transcriptomic prioritization only; no clinical efficacy, safety prediction, true anatomical projection, lineage tracing, tumorigenicity prediction or functional host-integration claim.",
      10
    )
  ),
  stringsAsFactors = FALSE
)

main_legends$observed_panel_list <- vapply(
  main_legends$figure_id,
  function(fig) {
    rows <- main_panel_map[
      main_panel_map$figure_id == fig,
      ,
      drop = FALSE
    ]

    collapse_panel_list(rows)
  },
  character(1)
)

atomic_write_csv(
  main_legends,
  OUT_MAIN_LEGEND_TABLE
)

stamp("Writing supplementary figure legend drafts...")

supp_fig_ids <- paste0(
  "Supplementary Figure ",
  1:10
)

supp_legend_rows <- list()

for (fig in supp_fig_ids) {
  rows <- supp_panel_map[
    supp_panel_map$figure_id == fig,
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    supp_legend_rows[[length(supp_legend_rows) + 1L]] <- data.frame(
      figure_id = fig,
      figure_title = paste0(fig, ". Supplementary analysis panel."),
      legend_text = paste0(
        fig,
        ". Supplementary analysis panel. No source-panel rows were detected in the 10D V17 source-panel audit; verify manually."
      ),
      observed_panel_list = "",
      claim_boundary_note = "Verify manually.",
      stringsAsFactors = FALSE
    )

    next
  }

  panel_phrases <- paste0(
    rows$panel,
    ", ",
    rows$panel_title,
    collapse = "; "
  )

  supp_legend_rows[[length(supp_legend_rows) + 1L]] <- data.frame(
    figure_id = fig,
    figure_title = paste0(fig, ". Supplementary analysis supporting the locked transcriptomic prioritization framework."),
    legend_text = paste(
      paste0(fig, ". Supplementary analysis supporting the locked transcriptomic prioritization framework."),
      paste0("Panels: ", panel_phrases, "."),
      "These supplementary panels provide supporting quality-control, robustness, diagnostic or extended validation information for the main figure package.",
      "They are interpreted within the same claim boundary as the main figures: transcriptomic prioritization and molecular-state validation only."
    ),
    observed_panel_list = panel_phrases,
    claim_boundary_note = "Transcriptomic prioritization support only; no clinical efficacy or direct functional graft-integration claim.",
    stringsAsFactors = FALSE
  )
}

supp_legends <- bind_rows_base(supp_legend_rows)

atomic_write_csv(
  supp_legends,
  OUT_SUPP_LEGEND_TABLE
)

stamp("Writing old-to-new figure reference replacement map for 10G...")

old_to_new <- data.frame(
  item_id = c(
    "F1A",
    "F1B", "F1C", "F1D", "F1E",
    "F2A", "F2B", "F2C", "F2D", "F2E",
    "F3A", "F3B", "F3C", "F3D", "F3E",
    "F4A", "F4B", "F4C", "F4D", "F4E",
    "F5A", "F5B", "F5C", "F5D", "F5E"
  ),
  old_reference = c(
    "Figure 1A",
    "Figure 1B", "Figure 1C", "Figure 1D", "Figure 1E",
    "Figure 2A", "Figure 2B", "Figure 2C", "Figure 2D", "Figure 2E",
    "Figure 3A", "Figure 3B", "Figure 3C", "Figure 3D", "Figure 3E",
    "Figure 4A", "Figure 4B", "Figure 4C", "Figure 4D", "Figure 4E",
    "Figure 5A", "Figure 5B", "Figure 5C", "Figure 5D", "Figure 5E"
  ),
  new_reference = c(
    "REMOVED_FROM_MAIN_FIGURES",
    "Figure 1A", "Figure 1B", "Figure 1C", "Figure 2A",
    "Figure 2B", "Figure 3A", "Figure 4A", "Figure 4B", "Figure 4C",
    "Figure 5A", "Figure 5B", "Figure 5C", "Figure 6A", "Figure 6B",
    "Figure 7A", "Figure 7B", "Figure 8A", "Figure 8B", "Figure 8C",
    "Figure 9A", "Figure 9B", "Figure 10A", "Figure 10B", "Figure 10C"
  ),
  new_figure_id = c(
    NA,
    "Figure 1", "Figure 1", "Figure 1", "Figure 2",
    "Figure 2", "Figure 3", "Figure 4", "Figure 4", "Figure 4",
    "Figure 5", "Figure 5", "Figure 5", "Figure 6", "Figure 6",
    "Figure 7", "Figure 7", "Figure 8", "Figure 8", "Figure 8",
    "Figure 9", "Figure 9", "Figure 10", "Figure 10", "Figure 10"
  ),
  update_instruction = c(
    "Remove workflow/framework panel reference or replace with text description of the frozen framework if needed.",
    rep("Replace old in-text panel reference with new V17 reference.", 24)
  ),
  stringsAsFactors = FALSE
)

old_to_new$current_panel_title <- NA_character_

for (i in seq_len(nrow(old_to_new))) {
  if (old_to_new$item_id[[i]] %in% main_panel_map$item_id) {
    old_to_new$current_panel_title[[i]] <- main_panel_map$panel_title[
      match(
        old_to_new$item_id[[i]],
        main_panel_map$item_id
      )
    ]
  }
}

atomic_write_csv(
  old_to_new,
  OUT_REFERENCE_REPLACEMENT_MAP
)

stamp("Writing 10G action items...")

action_items <- data.frame(
  priority = c(
    "P0",
    "P0",
    "P0",
    "P1",
    "P1",
    "P1",
    "P2"
  ),
  action_type = c(
    "manuscript_reference_update",
    "removed_panel",
    "legend_replacement",
    "results_section_structure",
    "supplementary_reference_check",
    "claim_boundary_check",
    "style_polish"
  ),
  action_item = c(
    "Use 10F_V1_old_to_new_figure_reference_replacement_map.csv to update all main-figure references in the manuscript.",
    "Remove or rewrite any text that refers to the deleted workflow/framework panel F1A.",
    "Replace old Figure 1–5 legends with the new Figure 1–10 legends from 10F_V1_main_figure_legends_V17.txt.",
    "Rewrite Results section subheadings so that they follow the V17 figure order.",
    "Check that Supplementary Figure references still point to the correct unchanged supplementary outputs.",
    "Run 10H after 10G to check claim-boundary language and locked key numbers in the updated manuscript.",
    "Polish figure legends for target journal length and style after journal selection."
  ),
  blocking_for_submission = c(
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    FALSE
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(
  action_items,
  OUT_10G_ACTION_ITEMS
)

stamp("Writing legend TXT files...")

main_legend_lines <- c(
  "============================================================",
  "PD_Graft_Project — MAIN FIGURE LEGENDS V17",
  "Generated by 10F V1",
  "============================================================",
  ""
)

for (i in seq_len(nrow(main_legends))) {
  main_legend_lines <- c(
    main_legend_lines,
    main_legends$legend_text[[i]],
    "",
    paste0("Claim-boundary note: ", main_legends$claim_boundary_note[[i]]),
    "",
    "------------------------------------------------------------",
    ""
  )
}

supp_legend_lines <- c(
  "============================================================",
  "PD_Graft_Project — SUPPLEMENTARY FIGURE LEGENDS V17",
  "Generated by 10F V1",
  "============================================================",
  ""
)

for (i in seq_len(nrow(supp_legends))) {
  supp_legend_lines <- c(
    supp_legend_lines,
    supp_legends$legend_text[[i]],
    "",
    paste0("Claim-boundary note: ", supp_legends$claim_boundary_note[[i]]),
    "",
    "------------------------------------------------------------",
    ""
  )
}

combined_legend_lines <- c(
  main_legend_lines,
  "",
  supp_legend_lines
)

write_utf8_lines(
  main_legend_lines,
  OUT_MAIN_LEGENDS_TXT
)

write_utf8_lines(
  supp_legend_lines,
  OUT_SUPP_LEGENDS_TXT
)

write_utf8_lines(
  combined_legend_lines,
  OUT_COMBINED_LEGENDS_TXT
)

reference_update_lines <- c(
  "============================================================",
  "PD_Graft_Project — 10G MANUSCRIPT REFERENCE UPDATE NOTE",
  "Generated by 10F V1",
  "============================================================",
  "",
  "Use the table below as the authoritative old-to-new panel-reference replacement map.",
  "",
  "Important structural changes:",
  "- The old Figure 1A workflow/framework panel was removed from the final main figures.",
  "- The old Figure 2B volcano plot is now standalone Figure 3A.",
  "- GO, KEGG and Hallmark enrichment are now Figure 4A-C.",
  "- The old Figure 3 machine-learning panels are split into Figure 5 and Figure 6.",
  "- The old Figure 4 external validation panels are split into Figure 7 and Figure 8.",
  "- The old Figure 5 GSE243639 panels are split into Figure 9 and Figure 10.",
  "",
  "Authoritative table:",
  normalize_path(OUT_REFERENCE_REPLACEMENT_MAP),
  "",
  "Do not update the manuscript by blind global replacement only.",
  "Review each sentence manually so the biological logic still matches the new figure order.",
  "",
  "Next step:",
  "10G should apply this reference map to the Results text and figure callouts."
)

write_utf8_lines(
  reference_update_lines,
  OUT_REFERENCE_UPDATE_NOTE
)

report_lines <- c(
  "============================================================",
  "PD_Graft_Project — 10F FIGURE LEGENDS AND REFERENCE MAP V1",
  "============================================================",
  paste0("Run time: ", timestamp()),
  paste0("Project root: ", normalize_path(PROJECT_ROOT)),
  paste0("Input 10D: ", INPUT_10D_TAG),
  paste0("Input 10E: ", INPUT_10E_TAG),
  "",
  "Summary:",
  paste0("- Main figure legends written: ", nrow(main_legends)),
  paste0("- Supplementary figure legends written: ", nrow(supp_legends)),
  paste0("- Main panel map rows: ", nrow(main_panel_map)),
  paste0("- Supplementary panel map rows: ", nrow(supp_panel_map)),
  paste0("- Old-to-new reference rows: ", nrow(old_to_new)),
  "",
  "Key V17 manuscript changes:",
  "- Main figure structure is now Figure 1–10.",
  "- No main figure contains more than 3 panels.",
  "- Volcano plot is standalone Figure 3A.",
  "- GO/KEGG/Hallmark enrichment is grouped as Figure 4A-C.",
  "- GSE243639 validation is split into Figure 9 and Figure 10.",
  "",
  "Outputs:",
  paste0("- ", normalize_path(OUT_MAIN_LEGEND_TABLE)),
  paste0("- ", normalize_path(OUT_SUPP_LEGEND_TABLE)),
  paste0("- ", normalize_path(OUT_MAIN_PANEL_MAP)),
  paste0("- ", normalize_path(OUT_SUPP_PANEL_MAP)),
  paste0("- ", normalize_path(OUT_REFERENCE_REPLACEMENT_MAP)),
  paste0("- ", normalize_path(OUT_10G_ACTION_ITEMS)),
  paste0("- ", normalize_path(OUT_MAIN_LEGENDS_TXT)),
  paste0("- ", normalize_path(OUT_SUPP_LEGENDS_TXT)),
  paste0("- ", normalize_path(OUT_COMBINED_LEGENDS_TXT)),
  paste0("- ", normalize_path(OUT_REFERENCE_UPDATE_NOTE)),
  "",
  "Next step:",
  "10G should update the Results text and in-text figure references using the 10F reference map.",
  "",
  "============================================================"
)

write_utf8_lines(
  report_lines,
  OUT_REPORT
)

sink(OUT_SESSION)
print(sessionInfo())
sink()

cat("\n============================================================\n")
cat("10F FIGURE LEGENDS AND REFERENCE MAP V1 完成\n")
cat("============================================================\n\n")

cat("Input 10D:\n")
cat(normalize_path(TEN_D_ROOT), "\n\n")

cat("Output manuscript files:\n")
cat(normalize_path(OUT_MANUSCRIPT_DIR), "\n\n")

cat("Output tables:\n")
cat(normalize_path(OUT_TABLE_DIR), "\n\n")

cat("Main legends written: ", nrow(main_legends), "\n", sep = "")
cat("Supplementary legends written: ", nrow(supp_legends), "\n", sep = "")
cat("Reference replacement map rows: ", nrow(old_to_new), "\n\n", sep = "")

cat("Key files:\n")
cat(normalize_path(OUT_MAIN_LEGENDS_TXT), "\n")
cat(normalize_path(OUT_SUPP_LEGENDS_TXT), "\n")
cat(normalize_path(OUT_REFERENCE_REPLACEMENT_MAP), "\n\n")

cat("✅ 10F 完成。下一步进入 10G：Results text + in-text figure references 更新。\n")
