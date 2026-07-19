# DA neuron / graft-related transcriptomic cell-state prioritisation framework

A source-traceable computational transcriptomic framework for prioritising candidate dopaminergic neuron and graft-related cell states by jointly evaluating functional identity, maturation-related evidence and risk-associated transcriptional programmes.

**Public-facing model label:** marker-rule-derived prioritisation model.

## Scientific question

Dopaminergic marker expression alone does not prove that a candidate cell state has the desired combination of dopaminergic identity, projection-associated molecular competence, maturation-related support and a favourable risk-associated transcriptomic profile. This repository provides a reproducible prioritisation framework for ranking candidate transcriptomic cell states and marker signatures for downstream experimental interpretation.

## Visual overview

![PD_Graft public package overview](figures/overview/PD_Graft_12O_annotated_public_repository_overview.png)

## Selected figure highlights

The selected highlights below follow the requested experimental order: **UMAP -> Safety-risk-associated score -> Heatmap -> GO -> KEGG -> Hallmark -> ROC/PR/AUC performance audit**.

### 1. UMAP

Annotation-colour UMAP overview of candidate dopaminergic / graft-related cell states used as the first visual entry point.

![1. UMAP](figures/selected_highlights_preview_png/01_umap.png)

[Open PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)

### 2. Safety-risk-associated score

Score-based view highlighting safety-risk-associated transcriptional programmes across candidate cell states.

![2. Safety-risk-associated score](figures/selected_highlights_preview_png/02_safety_risk_score.png)

[Open PDF](figures/12O_final_integrated_package/01_main_single_panel/008_main_10D_V18_main_single_panel_Figure_03_F1D_Safety-risk-associated_trans.pdf)

### 3. Heatmap

Candidate-state signature heatmap summarising the marker-rule-derived transcriptomic structure.

![3. Heatmap](figures/selected_highlights_preview_png/03_heatmap.png)

[Open PDF](figures/12O_final_integrated_package/01_main_single_panel/010_main_10D_V18_main_single_panel_Figure_05_F2A_Candidate-state_signature_he.pdf)

### 4. Gene Ontology (GO)

GO enrichment figure showing functional themes associated with prioritised candidate states.

![4. Gene Ontology (GO)](figures/selected_highlights_preview_png/04_go.png)

[Open PDF](figures/12O_final_integrated_package/01_main_single_panel/012_main_10D_V18_main_single_panel_Figure_07_F2C_Gene_Ontology_enrichment_10D.pdf)

### 5. KEGG

KEGG enrichment figure linking prioritised states to pathway-level biological context.

![5. KEGG](figures/selected_highlights_preview_png/05_kegg.png)

[Open PDF](figures/12O_final_integrated_package/01_main_single_panel/013_main_10D_V18_main_single_panel_Figure_08_F2D_KEGG_enrichment_10D_V18_sing.pdf)

### 6. Hallmark

Hallmark GSEA figure summarising higher-order programme support for the prioritisation framework.

![6. Hallmark](figures/selected_highlights_preview_png/06_hallmark.png)

[Open PDF](figures/12O_final_integrated_package/01_main_single_panel/014_main_10D_V18_main_single_panel_Figure_09_F2E_Hallmark_GSEA_10D_V18_single.pdf)

### 7. ROC / PR / AUC performance audit

Machine-learning audit figure showing ROC/PR/AUC-related performance checks; included last to follow the requested experimental order.

![7. ROC / PR / AUC performance audit](figures/selected_highlights_preview_png/07_roc_pr_auc_performance_audit.png)

[Open PDF](figures/12O_final_integrated_package/02_ml_audit_required_ROC_PR_AUC/031_ml_auc_11J_ML_audit_ROC_PR_AUC_11J_FINAL_FigB_ROC_PR_performance_audit.pdf)

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
docs/        manuscript-facing notes or explanatory documents
figures/     public figure package, overview graphics, annotation guide and manifests
metadata/    dataset metadata and provenance-supporting files
scripts/     reproducible analysis and figure-generation scripts
tables/      public tables and manifest-style outputs
README.md    English public summary
README_zh.md Chinese public summary
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
