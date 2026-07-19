# DA neuron / graft-related transcriptomic cell-state prioritisation framework

A source-traceable computational framework for prioritising candidate dopaminergic neuron and graft-related transcriptomic cell states by jointly evaluating functional identity, maturation-related evidence and risk-associated transcriptional programmes.

**Public-facing model label:** marker-rule-derived prioritisation model.

## Visual overview

### 12P V14 Figure 006

Public Figure 006 has been replaced with an original-cluster colour UMAP: no numbers or text are printed on the plot; the right-side legend maps each cluster colour to its annotation/name.

[Open Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


### 12P V13 Figure 006

Public Figure 006 has been replaced with a clean annotation-colour UMAP: no cluster numbers are shown on the plot; the right-side legend explains what each colour represents.

[Open Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


### 12P V12 Figure 006

Public Figure 006 has been replaced with a scRNA/cluster-number/name-key style cluster UMAP: the left panel preserves the original numbered cluster map, while the right panel provides a clean cluster-number-to-name key.

[Open Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


### 12P V11 Figure 006

Public Figure 006 has been replaced with a scRNA/Nature-reference style cluster UMAP: the left panel preserves the original numbered cluster map, while the right panel provides a clean cluster-to-majority-annotation key.

[Open Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


### 12P V10 Figure 006

Public Figure 006 has been replaced with a Nature-style true cluster-level annotated UMAP: original clusters are preserved, compact C0/C1/C2 tags are shown on the UMAP, and the full `C<cluster> = majority annotation` mapping is shown in a clean right-side key.

[Open Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


### 12P V9 Figure 006

Public Figure 006 has been replaced with a true cluster-level annotated UMAP: the original cluster structure is preserved and each cluster is labelled as `C<cluster>: <majority annotation>`. This is not the broad 04D/05B class-collapsed map.

[Open Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


### 12P V8 Figure 006

Public Figure 006 has been replaced with the GSE132758 05B safety contrast class / DA-projection-associated class UMAP. No new image was generated; an existing final annotated PDF was copied into the public Figure 006 position.

[Open Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


### 12P V7 annotated Figure 006

The public Figure 006 PDF has been replaced with the annotated GSE132758 04D conservative cell-state UMAP. No new image was generated; the repository file was replaced by copying the existing final annotated PDF.

[Open annotated Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


![PD_Graft annotated final package overview](figures/overview/PD_Graft_12O_annotated_public_repository_overview.png)

## Final annotated figure package

This repository display is based on the **12O final integrated figure package** generated after the 12N no-overclaim audit.

This GitHub-facing version intentionally **excludes** `06_optional_context_not_for_strong_claims` and keeps an annotated figure guide instead. Public figure filenames are shortened for Windows/GitHub compatibility, with full traceability in `figures/manifests/12P_V4_github_public_figure_filename_mapping.csv`.

The annotation table is:

`figures/manifests/12P_V4_github_public_figure_annotation_table.csv`

A readable guide is available at:

`figures/ANNOTATED_FIGURE_GUIDE.md`

### Retained figure groups

- `01_main_single_panel`: 24 PDF files
- `02_ml_audit_required_ROC_PR_AUC`: 4 PDF files
- `03_publication_panel_package`: 145 PDF files
- `04_supplementary_supporting_evidence`: 10 PDF files
- `05_audit_boundary_reproducibility`: 18 PDF files

Excluded optional context-only PDFs from group 06: 11

## Required ML audit

The `02_ml_audit_required_ROC_PR_AUC` folder is intentionally retained. It includes the ROC/PR/AUC-related model-performance audit and feature-importance/marker-overlap checks. These figures support auditability of the marker-rule-derived prioritisation structure; they do **not** establish clinical prediction.

## Scientific question

Dopaminergic marker expression alone does not establish that a candidate cell state combines appropriate functional identity, maturation-related competence and a favourable risk-associated transcriptional profile. This project creates a transparent cross-dataset prioritisation layer to identify candidate states and marker signatures for subsequent experimental testing.

## Interpretation boundary

### Supported interpretation

- source-traceable computational transcriptomic prioritisation framework;
- candidate transcriptomic cell states and marker signatures;
- marker-rule-derived prioritisation model audit;
- pseudotime/module-score support;
- proxy and contextual evidence support.

### Not claimed

- clinical-use prediction;
- validated diagnostic, prognostic or therapeutic biomarkers;
- graft efficacy, patient outcome or clinical safety prediction;
- anatomical-projection proof;
- barcode-confirmed lineage tracing;
- genetic causality or disease-mechanism proof.

## Reproducibility and data availability

Raw GEO data and large intermediate R objects are not redistributed. They should be obtained from the original public repositories. This public package provides scripts, source manifests, metadata, provenance tables, selected result-support materials and the annotated 12O-integrated figure package.

## Language

A Chinese project summary is available in [README_zh.md](README_zh.md).
