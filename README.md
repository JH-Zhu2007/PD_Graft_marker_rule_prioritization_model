# DA neuron / graft-related transcriptomic cell-state prioritisation framework

A source-traceable computational transcriptomic framework for prioritising candidate dopaminergic neuron and graft-related cell states by jointly evaluating functional identity, maturation-related evidence and risk-associated transcriptional programmes.

**Public-facing model label:** marker-rule-derived prioritisation model.

## Scientific question

Dopaminergic marker expression alone does not prove that a candidate cell state has the desired combination of dopaminergic identity, projection-associated molecular competence, maturation-related support and a favourable risk-associated transcriptomic profile. This repository provides a reproducible prioritisation framework for ranking candidate transcriptomic cell states and marker signatures for downstream experimental interpretation.

## Visual overview

![PD_Graft public package overview](figures/overview/PD_Graft_12O_annotated_public_repository_overview.png)

## Selected figure highlights

The selected highlights follow the experimental workflow order: **UMAP -> Safety-risk-associated score -> Heatmap -> GO -> KEGG -> Hallmark -> ROC/PR/AUC performance audit**.

Each preview is linked to the corresponding full-resolution PDF in the final figure package.

### 1. UMAP

**What this figure shows:** This panel provides the entry point for the transcriptomic landscape. It places cells in a two-dimensional UMAP space and uses the final annotation-colour display to show how broad candidate cell-state programmes are distributed across the discovery dataset.

It helps readers immediately see the relationship between the major annotated states before moving into score-based, marker-based and pathway-level evidence.

[![1. UMAP](figures/selected_highlights_preview_png/01_umap.png)](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)

[Open full PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)

### 2. Safety-risk-associated score

**What this figure shows:** This panel visualises the safety-risk-associated transcriptional score across the same cell-state landscape. It shows where risk-linked programmes, including proliferative, progenitor-like, immature or stress-associated transcriptional patterns, are concentrated.

Placed directly after the UMAP, it connects spatial cell-state organisation with the risk-aware layer of the prioritisation model.

[![2. Safety-risk-associated score](figures/selected_highlights_preview_png/02_safety_risk_score.png)](figures/12O_final_integrated_package/01_main_single_panel/008_main_10D_V18_main_single_panel_Figure_03_F1D_Safety-risk-associated_trans.pdf)

[Open full PDF](figures/12O_final_integrated_package/01_main_single_panel/008_main_10D_V18_main_single_panel_Figure_03_F1D_Safety-risk-associated_trans.pdf)

### 3. Heatmap

**What this figure shows:** This heatmap summarises the marker-rule-derived signature structure across candidate states. It provides a compact view of how selected identity, maturation and risk-associated gene programmes vary across the prioritised transcriptomic groups.

The heatmap acts as the bridge between single-cell spatial patterns and the gene-level evidence used to define the prioritisation framework.

[![3. Heatmap](figures/selected_highlights_preview_png/03_heatmap.png)](figures/12O_final_integrated_package/01_main_single_panel/010_main_10D_V18_main_single_panel_Figure_05_F2A_Candidate-state_signature_he.pdf)

[Open full PDF](figures/12O_final_integrated_package/01_main_single_panel/010_main_10D_V18_main_single_panel_Figure_05_F2A_Candidate-state_signature_he.pdf)

### 4. Gene Ontology (GO)

**What this figure shows:** The GO enrichment panel translates the marker/signature layer into functional biological terms. It summarises the biological processes and cellular programmes associated with the prioritised candidate states.

This provides a functional interpretation layer after the heatmap, helping readers move from gene lists to organised biological themes.

[![4. Gene Ontology (GO)](figures/selected_highlights_preview_png/04_go.png)](figures/12O_final_integrated_package/01_main_single_panel/012_main_10D_V18_main_single_panel_Figure_07_F2C_Gene_Ontology_enrichment_10D.pdf)

[Open full PDF](figures/12O_final_integrated_package/01_main_single_panel/012_main_10D_V18_main_single_panel_Figure_07_F2C_Gene_Ontology_enrichment_10D.pdf)

### 5. KEGG

**What this figure shows:** The KEGG panel places the candidate-state gene evidence into curated pathway context. It highlights pathway-level structure that complements the GO biological-process view.

Together with GO, this panel helps organise the prioritised transcriptomic signal into interpretable pathway modules.

[![5. KEGG](figures/selected_highlights_preview_png/05_kegg.png)](figures/12O_final_integrated_package/01_main_single_panel/013_main_10D_V18_main_single_panel_Figure_08_F2D_KEGG_enrichment_10D_V18_sing.pdf)

[Open full PDF](figures/12O_final_integrated_package/01_main_single_panel/013_main_10D_V18_main_single_panel_Figure_08_F2D_KEGG_enrichment_10D_V18_sing.pdf)

### 6. Hallmark

**What this figure shows:** The Hallmark panel condenses enrichment evidence into higher-order transcriptional programmes. It provides a broad programme-level summary that complements the more detailed GO and KEGG views.

This figure helps show whether the candidate-state evidence aligns with coherent transcriptional programmes rather than isolated gene-level changes.

[![6. Hallmark](figures/selected_highlights_preview_png/06_hallmark.png)](figures/12O_final_integrated_package/01_main_single_panel/014_main_10D_V18_main_single_panel_Figure_09_F2E_Hallmark_GSEA_10D_V18_single.pdf)

[Open full PDF](figures/12O_final_integrated_package/01_main_single_panel/014_main_10D_V18_main_single_panel_Figure_09_F2E_Hallmark_GSEA_10D_V18_single.pdf)

### 7. ROC / PR / AUC performance audit

**What this figure shows:** The ROC/PR/AUC audit panel summarises the model-performance layer of the workflow. It shows how the weak-label machine-learning component separates the marker-rule-defined comparison tasks and provides an audit trail for the prioritisation model.

Placed last, this figure completes the sequence from cell-state visualisation, to signature evidence, to pathway interpretation, and finally to model-performance auditing.

[![7. ROC / PR / AUC performance audit](figures/selected_highlights_preview_png/07_roc_pr_auc.png)](figures/12O_final_integrated_package/02_ml_audit_required_ROC_PR_AUC/031_ml_auc_11J_ML_audit_ROC_PR_AUC_11J_FINAL_FigB_ROC_PR_performance_audit.pdf)

[Open full PDF](figures/12O_final_integrated_package/02_ml_audit_required_ROC_PR_AUC/031_ml_auc_11J_ML_audit_ROC_PR_AUC_11J_FINAL_FigB_ROC_PR_performance_audit.pdf)

## What this repository contains

- Reproducible R analysis scripts.
- Source manifests and provenance tables.
- Dataset metadata and audit files supporting source traceability.
- Final GitHub-facing integrated figure package.
- Required ROC/PR/AUC machine-learning audit figures.
- Claim-boundary and no-overclaim audit materials.
- English and Chinese public project summaries.

This repository does not redistribute raw GEO data, large intermediate R objects, private local files, or submission-system-only materials.

## Final figure package

The final public figure package is stored in `figures/12O_final_integrated_package`.

Retained figure groups:

- `01_main_single_panel`: 24 PDF files
- `02_ml_audit_required_ROC_PR_AUC`: 4 PDF files
- `03_publication_panel_package`: 145 PDF files
- `04_supplementary_supporting_evidence`: 10 PDF files
- `05_audit_boundary_reproducibility`: 18 PDF files

Total retained public PDF files detected: 201.

`06_optional_context_not_for_strong_claims` is intentionally excluded from the public-facing package.

## Repository structure

```text
docs/          manuscript-facing notes or explanatory documents
figures/       public figure package, overview graphics, annotation guide and manifests
metadata/      dataset metadata and provenance-supporting files
scripts/       reproducible analysis and figure-generation scripts
tables/        public tables and manifest-style outputs
README.md      English public summary
README_zh.md   Chinese public summary
```

## Traceability files

- Public short-filename mapping: `figures/manifests/12P_V4_github_public_figure_filename_mapping.csv`
- Figure annotation table: `figures/manifests/12P_V4_github_public_figure_annotation_table.csv`
- Readable figure guide: `figures/ANNOTATED_FIGURE_GUIDE.md`

## Interpretation boundary

### Supported interpretation

- Source-traceable computational transcriptomic prioritisation framework.
- Candidate transcriptomic cell-state prioritisation.
- Candidate marker-signature and module-score support.
- Marker-rule-derived prioritisation model audit.
- External/contextual evidence support at the transcriptomic level.

### Not claimed

- Clinical-use prediction.
- Patient outcome prediction.
- Therapeutic-response prediction.
- Validated diagnostic, prognostic or therapeutic biomarker discovery.
- Anatomical-projection proof.
- Barcode-lineage proof.
- Genetic causality or disease-mechanism proof.

## Release note

The public release is designed for source traceability, transparent figure navigation and cautious interpretation of a computational prioritisation framework.
