# ============================================================
# 10B_MANUSCRIPT_FINAL_STANDALONE_V7_LOCKED_COMPLETE.R
# ============================================================
# 最终独立版：
#   - 不依赖任何旧版 10B manuscript 或旧版 10B 脚本
#   - 完整正文、引用、References、Funding、单一作者贡献全部内置
#   - 修正 GSE243639 归一化为 log1p-CP10K
#   - 修正 09G 稳定性定义：5 个主 diagonal settings，至少 4/5 一致
#   - 明确 125-setting grid 仅用于完整敏感性审计
#   - 清除内部对象名和所有占位符
#   - 不生成图片
#   - Figure legends 推迟到 10C 真正组图后生成
#
# 当前正式状态：
#   10B final = V7_LOCKED_COMPLETE
#
# 输出：
#   D:/PD_Graft_Project/06_reports/10B_manuscript_FINAL_STANDALONE_V7/
#   D:/PD_Graft_Project/03_tables/10B_manuscript_FINAL_STANDALONE_V7/
#
# 成功标志：
#   ✅ 10B FINAL STANDALONE V7 LOCKED COMPLETE 完成。
# ============================================================


# ============================================================
# 0. 路径
# ============================================================

PROJECT_DIR <- "D:/PD_Graft_Project"

OUT_REPORT_DIR <- file.path(
  PROJECT_DIR,
  "06_reports",
  "10B_manuscript_FINAL_STANDALONE_V7"
)

OUT_TABLE_DIR <- file.path(
  PROJECT_DIR,
  "03_tables",
  "10B_manuscript_FINAL_STANDALONE_V7"
)

dir.create(
  OUT_REPORT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  OUT_TABLE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

OUT_MANUSCRIPT <- file.path(
  OUT_REPORT_DIR,
  "10B_full_manuscript_FINAL_STANDALONE_V7.md"
)

OUT_METHODS <- file.path(
  OUT_REPORT_DIR,
  "10B_methods_FINAL_REPRODUCIBLE.txt"
)

OUT_DECLARATIONS <- file.path(
  OUT_REPORT_DIR,
  "10B_declarations_FINAL.txt"
)

OUT_PENDING_ITEMS <- file.path(
  OUT_REPORT_DIR,
  "10B_submission_pending_items.txt"
)

OUT_PARAMETER_TABLE <- file.path(
  OUT_TABLE_DIR,
  "10B_exact_analysis_parameters_FINAL.csv"
)

OUT_CITATION_AUDIT <- file.path(
  OUT_TABLE_DIR,
  "10B_citation_integrity_audit_FINAL.csv"
)

OUT_MANUSCRIPT_AUDIT <- file.path(
  OUT_TABLE_DIR,
  "10B_manuscript_integrity_audit_FINAL.csv"
)

OUT_VERIFICATION <- file.path(
  OUT_TABLE_DIR,
  "10B_output_verification_FINAL.csv"
)

OUT_SESSION <- file.path(
  OUT_REPORT_DIR,
  "10B_sessionInfo.txt"
)


# ============================================================
# 1. 工具函数
# ============================================================

stamp <- function(...) {
  cat(
    "[",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "] ",
    ...,
    "\n",
    sep = ""
  )
}

atomic_write_text <- function(lines, path) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }

  writeLines(
    enc2utf8(as.character(lines)),
    con = path,
    useBytes = TRUE
  )

  if (!file.exists(path)) {
    stop("文本文件未生成：", path)
  }

  size_bytes <- file.info(path)$size

  if (!is.finite(size_bytes) || size_bytes <= 0) {
    stop("文本文件为空或无效：", path)
  }

  invisible(path)
}

atomic_write_csv <- function(df, path) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }

  utils::write.csv(
    df,
    file = path,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  if (!file.exists(path)) {
    stop("CSV 未生成：", path)
  }

  size_bytes <- file.info(path)$size

  if (!is.finite(size_bytes) || size_bytes <= 0) {
    stop("CSV 为空或无效：", path)
  }

  invisible(path)
}

expand_citation <- function(x) {
  parts <- unlist(
    strsplit(
      x,
      ",",
      fixed = TRUE
    )
  )

  out <- integer()

  for (part in parts) {
    part <- trimws(part)

    if (grepl("-", part, fixed = TRUE)) {
      bounds <- as.integer(
        unlist(
          strsplit(
            part,
            "-",
            fixed = TRUE
          )
        )
      )

      out <- c(
        out,
        seq.int(bounds[1], bounds[2])
      )
    } else {
      out <- c(
        out,
        as.integer(part)
      )
    }
  }

  out
}


# ============================================================
# 2. 内置完整 manuscript
# ============================================================

cat("\n============================================================\n")
cat("10B FINAL STANDALONE V7\n")
cat("============================================================\n\n")

stamp("载入内置最终 manuscript。")

manuscript_lines <- c(
  "# Single-cell transcriptomic prioritization of dopaminergic graft-relevant cell states and safety-risk programs in Parkinson's disease",
  "",
  "**Running title:** Transcriptomic prioritization of DA graft-relevant states",
  "",
  "**Keywords:** Parkinson's disease; dopaminergic graft; single-cell RNA sequencing; cell-state prioritization; safety-risk-associated transcriptional state; marker-rule-derived machine learning",
  "",
  "## Abstract",
  "",
  "Background: Cell-state heterogeneity remains a major challenge for transcriptomic evaluation of dopaminergic graft preparations for Parkinson's disease. Existing public single-cell datasets contain differentiated dopaminergic, progenitor, glial, stromal, immune and cycling states, but lack a standardized framework for prioritizing states with favorable dopaminergic identity while flagging transcriptional safety-risk programs.",
  "",
  "Methods: We developed a frozen transcriptomic prioritization framework using six discovery/reference single-cell datasets, with one bulk RNA-seq dataset used for supportive analysis; two additional datasets were reserved for primary external and disease-context applications. After quality control, 54 single-cell objects were retained, and frozen scores were successfully computed for 52 objects comprising 159,277 cells. The framework combined DA-like identity, A9/A10-like molecular signatures, projection-associated molecular competence and safety-risk-associated transcriptional programs. Candidate states were interpreted using differential expression, Gene Ontology, KEGG and Hallmark enrichment. Leakage-audited reduced-feature marker-rule-derived models were evaluated by internal cross-validation, leave-one-dataset-out validation, threshold sensitivity, negative controls and external applications.",
  "",
  "Results: Ideal-like states were characterized by oxidative phosphorylation, electron transport, ATP synthesis and neuronal/axon-associated programs, whereas lower-priority states showed extracellular-matrix, adhesion, inflammatory, cytokine, complement/coagulation, epithelial–mesenchymal-transition, hypoxia and angiogenic programs. Internal model performance was high, but leave-one-dataset-out AUCs were modest (0.64 and 0.63 for ideal-like logistic and random-forest models; 0.58 and 0.53 for safety-risk models), indicating dataset-dependent generalization. Across five principal threshold settings, 85.3% of groups (279/327) retained the same dominant classification in at least four settings, and all real-versus-null comparisons showed positive delta-AUC values; 13 of 16 empirical tests reached p <= 0.05. Primary external application to GSE183248 recovered 8 clusters from 4,495 cells, all classified as safety-risk-like. Marker-targeted disease-context analysis of GSE243639 recovered 8 signature-space clusters from 83,484 cells, including 6 ideal-like, 1 safety-risk-like and 1 mixed/uncertain clusters.",
  "",
  "Conclusions: The framework provides a reproducible transcriptomic approach for prioritizing dopaminergic graft-relevant cell states and separating ideal-like molecular programs from safety-risk-associated or lower-priority states. The results support transcriptomic prioritization and robustness, but do not establish graft efficacy, clinical safety, anatomical projection or functional host integration.",
  "",
  "## Introduction",
  "",
  "",
  "Parkinson's disease is characterized by progressive degeneration of nigrostriatal dopaminergic neurons and the consequent disruption of motor circuitry [1]. Cell-replacement strategies seek to restore dopaminergic function by transplanting developmentally specified dopaminergic progenitors or derived neuronal populations [2,3]. However, the molecular quality of a graft preparation cannot be reduced to the presence of a small number of dopaminergic markers. Differentiation cultures and graft-associated datasets contain heterogeneous neuronal, progenitor, glial, immune, stromal, vascular and cycling states that may differ substantially in maturation, lineage fidelity and potential risk [4,10,12,13].",
  "",
  "This heterogeneity has two major implications. First, cells with dopaminergic marker expression may still differ in A9/A10-like molecular identity, neuronal maturation and the molecular competence associated with axonal growth or projection-related programs. Second, residual proliferative, pluripotency-associated, neuroepithelial, stress-responsive, extracellular-matrix-rich or inflammatory states may represent lower-priority or safety-risk-associated transcriptional programs [4,5,10]. Transcriptomic safety-risk signals do not demonstrate tumorigenicity or clinical harm, but they can identify cell states that warrant deprioritization or further experimental validation.",
  "",
  "Single-cell RNA sequencing provides a cell-state-resolved view of differentiation products, grafts and disease-relevant tissue [6,15,16,23]. Nevertheless, public single-cell datasets are generated across different species, laboratories, developmental stages, protocols and annotation systems [7,23]. Directly merging all studies into a single biological claim can therefore obscure dataset-specific structure. A rigorous integrative framework should distinguish discovery, scoring, model development, robustness testing and external application, while maintaining explicit boundaries between transcriptomic evidence and functional graft validation.",
  "",
  "Here, we developed a frozen transcriptomic prioritization framework for dopaminergic graft-relevant cell states. The framework combines DA-like transcriptional identity, A9/A10-like molecular signatures, projection-associated molecular competence and safety-risk-associated transcriptional programs. We used differential expression and pathway analysis to interpret prioritized states, developed leakage-audited reduced-feature marker-rule-derived models, and tested robustness through threshold perturbation, negative controls, primary external application and disease-context validation. We explicitly treat the resulting scores and predicted probabilities as transcriptomic prioritization outputs, not as evidence of therapeutic efficacy, clinical safety, anatomical projection or functional integration.",
  "",
  "## Results",
  "",
  "",
  "A frozen multi-dataset framework was established for transcriptomic prioritization",
  "",
  "After quality control and filtering, 54 single-cell objects were retained across six discovery/reference single-cell datasets. Frozen signature scoring was successfully completed for 52 objects comprising 159,277 cells; 2 objects were retained in the audit as unscored rather than being silently excluded. The frozen marker panel comprised 18 biological categories and 121 unique marker genes. These categories represented DA-like transcriptional identity, A9/A10-like molecular signatures, neuronal maturation, projection-associated molecular competence, progenitor and pluripotency-associated programs, cell-cycle activity, stress/apoptosis responses, and glial, immune, stromal and vascular lineages.",
  "",
  "Prioritization separated ideal-like from safety-risk-associated and lower-priority states",
  "",
  "The scoring framework identified substantial heterogeneity across datasets and cell groups. States prioritized as ideal-like combined stronger DA-like and projection-associated molecular scores with lower safety-risk-associated signals. By contrast, lower-priority or safety-risk-associated states showed varying contributions from progenitor, cell-cycle, stress, pluripotency-associated, extracellular-matrix, glial or immune programs. A9/A10-like molecular heterogeneity was also observed: the DA-enriched GSE178265 object and GSE233885 showed an A9-like bias, GSE200610 showed an A10-like bias, and GSE132758 and GSE204796 contained mixed molecular patterns. These assignments describe molecular similarity and do not establish anatomical subtype identity.",
  "",
  "Ideal-like and lower-priority states were distinguished by coherent molecular programs",
  "",
  "Differential-expression analysis between ideal-like and lower-priority states included all genes passing the prespecified minimum-expression filter, without an arbitrary top-gene testing cap. Ideal-like states showed higher expression of neuronal and neurite-associated genes, including RTN1, SARAF, BEX1, VAMP2, MAP1B, BEX2, GAP43 and TAGLN3. Lower-priority states showed higher expression of extracellular-matrix, stromal or trophic genes, including DCN, PTGDS, PTN and AGT.",
  "",
  "Gene Ontology enrichment linked ideal-like states to proton transport, electron-transfer activity, respiratory-chain complexes, oxidative phosphorylation, ATP synthesis and axon-associated terms. KEGG analysis similarly identified oxidative phosphorylation and neurodegeneration-associated mitochondrial gene modules. These disease-labelled pathways were interpreted as shared mitochondrial and electron-transport programs rather than evidence that the cells exhibited neurodegenerative pathology. Hallmark enrichment reinforced positive oxidative-phosphorylation, reactive-oxygen-species, protein-secretion and DNA-repair programs in ideal-like states. In contrast, lower-priority states were enriched for extracellular-matrix organization, focal adhesion, cytokine signalling, leukocyte transendothelial migration, complement/coagulation, TGF-beta signalling, epithelial–mesenchymal transition, inflammatory responses, hypoxia and angiogenesis.",
  "",
  "Cell-state composition revealed dataset-level prioritization differences",
  "",
  "Dataset-level composition analysis showed that the DA-enriched GSE178265 object and GSE233885 were dominated by favorable or ideal-like states, whereas GSE200610 and GSE204796 contained larger lower-priority or mixed components. GSE132758 contained a notable safety-risk-associated component. A priority index defined as the ideal-like fraction minus the safety-risk-high fraction ranked the DA-enriched GSE178265 object and GSE233885 near the favorable end of the spectrum, while GSE132758 was negative and GSE204796 was intermediate or mixed. This index was used as a descriptive transcriptomic prioritization metric rather than a clinical quality score.",
  "",
  "Leakage auditing defined reduced non-direct feature sets for marker-rule-derived modelling",
  "",
  "The machine-learning dataset contained 168 groups for the ideal-like task and 172 groups for the safety-risk task. A dedicated leakage and circularity audit identified direct score-derived features that could trivially reconstruct the rule-based marker-rule-derived labels, including the safety-risk composite and DA/projection composite scores. These direct features were excluded from the primary models. Task-specific reduced non-direct marker features were therefore used for primary analysis, while the full exploratory feature set was retained only as a sensitivity analysis.",
  "",
  "Marker-rule-derived models showed high internal performance but modest cross-dataset generalization",
  "",
  "Internal cross-validation yielded high AUC values for the ideal-like random-forest (0.999), safety-risk random-forest (0.991), ideal-like logistic (0.986) and safety-risk logistic (0.93) models. However, leave-one-dataset-out validation was substantially weaker, with AUCs of 0.64 and 0.63 for ideal-like logistic and random-forest models, and 0.58 and 0.53 for safety-risk random-forest and logistic models, respectively. The divergence between internal and leave-one-dataset-out performance indicates dataset-dependent generalization and argues against interpreting the models as validated predictors of graft outcome or clinical safety.",
  "",
  "Threshold sensitivity and negative controls supported non-random prioritization",
  "",
  "Threshold-sensitivity analysis evaluated 327 groups. Across the five principal diagonal threshold settings, 279 of 327 groups (85.3%) retained the same dominant classification in at least four settings, indicating that the main prioritization pattern was not determined by a single arbitrary cutoff. Negative-control analyses compared the real reduced-feature models with models trained using permuted labels or permuted feature matrices. All real-versus-null comparisons showed positive delta-AUC values, and 13 of 16 empirical tests reached p <= 0.05. These results support a non-random transcriptomic feature–label structure, while remaining conditional on the marker-rule-derived framework.",
  "",
  "Primary external application to GSE183248 produced a conservative safety-risk-like profile",
  "",
  "GSE183248 was selected before external application as the primary frozen-validation candidate. Unsupervised recovery identified 8 external clusters across 4,495 cells. The frozen predictor classified all 8 clusters as safety-risk-like and none as ideal-like. This result did not reproduce a favorable state distribution from the discovery datasets. Instead, it demonstrated a conservative context-dependent application of the frozen framework and showed that an independent dataset was not forced into the desired ideal-like category.",
  "",
  "Disease-context analysis of GSE243639 recovered heterogeneous signature-space states",
  "",
  "As an additional disease-context analysis, the frozen framework was applied to the processed GSE243639 count table using a marker-targeted local-import strategy. The import parsed 33,525 expression-table rows to estimate cell-level library sizes and retained all 121 frozen marker genes across 83,484 cells. Eight signature-space context clusters were recovered. The logistic frozen predictor classified 6 clusters as ideal-like, 1 as safety-risk-like and 1 as mixed or uncertain. The processed file generated a terminal incomplete-gzip warning, which was recorded in the audit. Accordingly, this analysis was treated as marker-targeted disease-context support rather than a full-transcriptome raw-data reanalysis.",
  "",
  "Integrated evidence supports transcriptomic prioritization with explicit claim boundaries",
  "",
  "Together, discovery, pathway interpretation, leakage-audited modelling, threshold sensitivity, negative controls and external applications support a frozen transcriptomic framework for ranking DA graft-relevant cell states. The framework distinguishes ideal-like DA/projection-associated/safety-low states from safety-risk-associated or lower-priority states. However, the evidence remains transcriptomic and computational. It does not demonstrate clinical safety, absence of tumorigenicity, therapeutic efficacy, anatomical projection, target innervation or functional host integration.",
  "",
  "## Methods",
  "",
  "",
  "Study design",
  "",
  "The study was designed as an integrative analysis of public transcriptomic datasets relevant to dopaminergic differentiation, graft-associated cell states and Parkinson's disease context. Analysis proceeded through prespecified modules covering data curation, quality control, frozen marker definition, cell-state scoring, biological interpretation, marker-rule-derived model development, robustness testing, primary external application and disease-context validation. Discovery and validation roles were separated before downstream model application to reduce outcome-driven dataset selection.",
  "",
  "Public datasets and analysis roles",
  "",
  "Six single-cell datasets contributed to the discovery and reference framework: GSE178265, GSE132758, GSE157783, GSE204796, GSE200610 and GSE233885. A DA-enriched object derived from GSE178265 served as the core DA identity reference; GSE132758, GSE204796, GSE200610 and GSE233885 contributed graft, differentiation, lineage or projection-linked reference contexts; and GSE157783 provided a human Parkinson's disease midbrain reference. GSE204795 was analysed separately as bulk RNA-seq support. GSE183248 and GSE243639 were reserved for primary external and disease-context applications, respectively. Dataset roles were recorded in a formal manifest before final scoring and model application [4,8-14,24].",
  "",
  "All expression datasets were obtained from the NCBI Gene Expression Omnibus (GEO) [24]. The accession-to-publication mapping was GSE178265 [8], GSE132758 [4], GSE157783 [9], GSE204795 and GSE204796 [10], GSE183248 [11], GSE200610 [12], GSE233885 [13] and GSE243639 [14].",
  "",
  "Single-cell preprocessing and quality control",
  "",
  "Single-cell matrices were imported into Seurat-compatible objects and processed using object-level quality-control thresholds, normalization, variable-feature identification, scaling, principal-component analysis and, where appropriate, neighbourhood graph construction and clustering [15,16,23]. Fifty-four filtered objects were retained. Object-level reduction was completed as full clustering/UMAP or PCA-only according to matrix size and computational feasibility. Downsampled or matrix-only reductions were used only for batch-check assistance and were excluded from final differential-expression, scoring, proportion and model conclusions.",
  "",
  "Frozen marker panel and scoring framework",
  "",
  "A frozen marker panel containing 18 biological categories and 121 unique marker genes was established before final scoring. Marker selection was informed by published studies of dopaminergic subtype identity, differentiation, graft composition, lineage and target-linked molecular diversity [4,5,8,10,12,13,22], and the panel was then frozen before downstream scoring and external application. The categories represented DA-like transcriptional identity, A9-like and A10-like molecular signatures, neuronal maturation, projection-associated molecular competence, progenitor and neuroepithelial programs, pluripotency-associated risk, cell-cycle activity, stress/apoptosis responses, glial, immune, oligodendroglial, extracellular-matrix, fibroblast, vascular/pericyte and other lineage programs. Scores were computed at cell, group and object levels using the same frozen panel across discovery and external analyses.",
  "",
  "Signature-score calculation and composite definitions",
  "",
  "For each cell, a category score was calculated as the arithmetic mean of the expression values of all marker genes from that category that were present in the object's expression matrix. Gene matching was case-insensitive. When only count data were available, library sizes were calculated from the full available matrix, counts were converted to counts per 10,000, and log1p transformation was applied before scoring. Missing marker genes were omitted from the category mean and recorded in a coverage audit; a category with no detected marker genes was assigned a missing value. Cell-level scores were aggregated to group level by arithmetic mean.",
  "",
  "The DA-like composite score was defined as the mean of the DA-core-identity, DA-functional-machinery and neuronal-maturation/synapse scores. The projection-competence composite score was defined as the mean of the projection-associated-molecular-competence and neuronal-maturation/synapse scores. The combined DA–projection-competence score was the mean of the DA-like and projection-competence composite scores. The A9–A10 bias score was calculated as the A9-like score minus the A10-like score. Bias values greater than 0.02 were labelled A9-like, values below −0.02 were labelled A10-like, and values between −0.02 and 0.02 were labelled mixed or unclear. Composite means were calculated over available component scores; missing components were excluded rather than replaced with zero.",
  "",
  "Candidate-state prioritization",
  "",
  "Ideal-like states were defined by concordant DA-like and projection-associated molecular scores together with low safety-risk-associated scores. Safety-risk-associated or lower-priority states were defined by composite contributions from progenitor, cell-cycle, pluripotency, stress and non-target lineage programs. These were rule-based marker-rule-derived labels used for transcriptomic prioritization and model development; they were not clinical outcome labels.",
  "",
  "The safety-risk composite was calculated as a weighted mean of six frozen category scores over the non-missing components: cell-cycle/proliferation (weight 1.20), progenitor/neuroepithelial (1.00), pluripotency/immature risk (1.40), stress/apoptosis response (0.60), extracellular-matrix/fibroblast (0.40), and vascular/pericyte/meningeal (0.30). Safety-risk scores of 0.20 or lower were considered low-signal and scores of 0.35 or higher were considered high-signal. A group was assigned to the ideal-like class when DA-like score was at least 0.08, projection-competence score was at least 0.08, and safety-risk score was no greater than 0.20. Groups with safety-risk score at least 0.35 and DA-like score below 0.05 were classified as high-safety-risk/low-DA; groups with safety-risk score at least 0.35 and DA-like score at least 0.05 were classified as mixed DA or projection with safety risk. A projection-competence-without-DA class required projection score at least 0.05, DA-like score below 0.05, and safety-risk score no greater than 0.20. Remaining groups were classified as lower-priority or mixed. These thresholds were prespecified in the scoring scripts and subsequently evaluated by the threshold-sensitivity analysis.",
  "",
  "Differential expression and pathway enrichment",
  "",
  "Differential expression contrasted ideal-like with lower-priority states. All genes passing a prespecified minimum-expression filter were tested; no arbitrary top-gene cap was used for statistical testing. Multiple testing was controlled using the Benjamini–Hochberg procedure. Gene Ontology and KEGG over-representation analyses were performed against the tested-gene universe using clusterProfiler [17]. Hallmark gene sets were obtained from MSigDB [19] and evaluated by pre-ranked GSEA [18] using fgsea [25]. Pathways labelled as Parkinson's, Alzheimer's or Huntington's disease were interpreted as shared mitochondrial or electron-transport modules rather than disease-state diagnoses.",
  "",
  "Cell-state composition and priority index",
  "",
  "Group-level cell counts were aggregated to object and dataset levels. The ideal-like fraction, safety-risk-high fraction and other priority classes were calculated from classified groups. A descriptive priority index was defined as the ideal-like fraction minus the safety-risk-high fraction. The index was used for relative transcriptomic comparison and not as a clinical quality-control threshold.",
  "",
  "Leakage and circularity audit",
  "",
  "Before primary model training, all candidate features were classified according to leakage risk. Direct composite scores or features used to construct the marker-rule-derived labels were flagged as circular and excluded from the primary reduced-feature datasets. Full exploratory features were retained only for sensitivity analysis. The primary models therefore used task-specific reduced non-direct marker features.",
  "",
  "Marker-rule-derived machine learning",
  "",
  "Logistic-regression and random-forest classifiers [20] were trained separately for ideal-like and safety-risk marker-rule-derived tasks. Performance was assessed by stratified internal cross-validation and leave-one-dataset-out validation, with interpretation informed by known cross-validation selection bias [21]. Internal cross-validation measured within-framework recapitulation, whereas leave-one-dataset-out validation assessed dataset-level generalization. Model outputs were interpreted as exploratory probabilities of marker-rule-derived membership rather than probabilities of therapeutic success or clinical safety.",
  "",
  "The primary machine-learning analysis used random seed 20260714 and five-fold stratified internal cross-validation. All imputation and scaling parameters were estimated from the training portion of each split. Missing numeric values were replaced by the training-set median. Logistic-regression inputs were standardized using the training-set median and standard deviation. To limit overfitting, logistic-regression feature selection was performed within each training split using the absolute standardized difference between positive and negative marker-rule-derived groups, and at most the top 10 non-direct features were retained. Random-forest models used all reduced non-direct features without scaling, 500 trees, and mtry equal to the integer square root of the number of input features. A probability threshold of 0.5 was used for binary summary metrics. Leave-one-dataset-out validation held out each dataset in turn, with all preprocessing and model fitting repeated using only the remaining datasets.",
  "",
  "Threshold-sensitivity analysis",
  "",
  "Ideal-like and safety-risk thresholds were varied across a prespecified diagonal series and a broader three-parameter quantile grid. The five principal diagonal settings used ideal-high quantiles of 0.65, 0.70, 0.75, 0.80 and 0.85; corresponding safety-low quantiles of 0.35, 0.30, 0.25, 0.20 and 0.15; and safety-high quantiles of 0.65, 0.70, 0.75, 0.80 and 0.85. Group-level classification stability was evaluated across these five principal diagonal settings. A group was considered stable when the same dominant class was assigned in at least 80% of the settings, corresponding to at least four of the five settings. The broader audit grid comprised all combinations of ideal-high quantiles 0.65–0.85 in 0.05 increments, safety-low quantiles 0.15–0.35 in 0.05 increments, and safety-high quantiles 0.65–0.85 in 0.05 increments, yielding 125 settings. For each setting, ideal-like status required the ideal score to meet or exceed its quantile threshold and the safety score to be no greater than the safety-low threshold; safety-risk-like status required the safety score to meet or exceed the safety-high threshold. The 125-setting grid was used for full sensitivity auditing of class fractions, dataset priority indices and dataset rankings.",
  "",
  "Negative-control analysis",
  "",
  "Negative controls included models trained with permuted training labels while retaining the original features and models trained with the true labels after independently permuting each feature column. Test labels remained unchanged. The negative-control analysis used random seed 20260715, five-fold stratified internal cross-validation, leave-one-dataset-out validation, and 30 independent repeats for each negative-control type. Random-forest negative-control models used 300 trees and mtry equal to the integer square root of the number of reduced features. Real-model AUCs were compared with the corresponding negative-control distributions. Delta-AUC and empirical exceedance probabilities were calculated to test whether the observed model performance exceeded random label or feature structure.",
  "",
  "Primary external application",
  "",
  "GSE183248 was selected through a prespecified external-dataset eligibility audit before application of the frozen framework. Available processed expression data were imported, normalized, scored using the frozen marker panel and grouped using available metadata or unsupervised recovery. Discovery-trained reduced-feature predictors were applied without retraining, threshold adjustment or marker-panel modification.",
  "",
  "Disease-context marker-targeted validation",
  "",
  "GSE243639 was used as an additional disease-context dataset. The locally downloaded processed count table was scanned in chunks. Cell-level library sizes were accumulated across 33,525 parsed expression-table rows, while only the 121 frozen marker genes were retained for downstream log1p-transformed counts-per-10,000 scoring. Signature-space clusters were recovered from the 18 frozen category scores. A terminal incomplete-gzip warning was recorded, and the analysis was therefore restricted to marker-targeted disease-context interpretation.",
  "",
  "The local count table was read in chunks of 25 expression-table rows. Library sizes were accumulated over all successfully parsed rows, whereas only counts for the 121 frozen marker genes were retained. Marker counts were normalized to counts per 10,000 using the accumulated library size and transformed with log1p. The 18 frozen category scores were then calculated for each cell. Context clustering used random seed 20260715. Non-finite score values were replaced by the median of the corresponding signature score, each score dimension was standardized to zero mean and unit variance, and non-finite standardized values were set to zero. K-means clustering was performed in the 18-dimensional frozen-signature space with eight requested clusters, nstart = 50 and iter.max = 100; the implementation required at least 30 cells per requested cluster. These clusters were exploratory signature-space context clusters and were not interpreted as definitive biological cell types.",
  "",
  "Reproducibility safeguards",
  "",
  "All final analyses used frozen marker definitions, score equations, thresholds and model feature sets that were established before external application. Downsampled or matrix-only reductions were excluded from final differential-expression, score, frequency and machine-learning claims. Random seeds were fixed in the relevant scripts. Model preprocessing was fitted within training folds or training datasets only. External predictors were applied without retraining, threshold adjustment or marker-panel modification. Module-specific input manifests, output-verification tables, failure audits, session information and claim-boundary notes were retained.",
  "",
  "Statistical analysis and reproducibility",
  "",
  "All analyses used scripted R workflows with module-specific output manifests, method notes and claim-boundary files. False-discovery-rate correction was applied where multiple hypothesis testing was performed. No statistical result was interpreted as prospective clinical validation. Final software versions and session information were saved with each major module.",
  "",
  "## Discussion",
  "",
  "",
  "This study developed a frozen transcriptomic framework for prioritizing dopaminergic graft-relevant cell states across heterogeneous public single-cell datasets. The central contribution is not a new clinical predictor or a demonstration of graft efficacy. Instead, the framework provides a reproducible way to combine dopaminergic identity, A9/A10-like molecular programs, projection-associated molecular competence and safety-risk-associated transcriptional states into an auditable cell-state prioritization workflow.",
  "",
  "The biological distinction between ideal-like and lower-priority states was coherent across differential-expression and pathway analyses. Ideal-like states were enriched for oxidative phosphorylation, respiratory-chain activity, ATP synthesis and neuronal or axon-associated programs. These patterns are consistent with greater energetic and neuronal specialization, but should not be equated with functional maturation or successful host integration. Lower-priority states showed extracellular-matrix, focal-adhesion, cytokine, complement/coagulation, inflammatory, epithelial–mesenchymal-transition, hypoxia and angiogenic programs. This combination suggests a broader stromal, wound-response or mixed-lineage state rather than a single safety mechanism.",
  "",
  "A9/A10-like molecular heterogeneity further demonstrated that dopaminergic identity is not uniform. Molecular similarity to A9- or A10-associated signatures can help describe cell-state diversity [8,13,22], but transcriptomic signatures alone cannot establish anatomical origin, target innervation or electrophysiological function. For this reason, the study uses the terms 'A9/A10-like molecular signature' and 'projection-associated molecular competence' rather than claiming true subtype identity or projection.",
  "",
  "The safety component should be interpreted with similar caution. Progenitor, cell-cycle, pluripotency-associated and stress-response programs can identify states that warrant deprioritization or further testing [4,5,10]. However, a safety-risk-associated transcriptional state does not demonstrate tumorigenicity, graft overgrowth or clinical harm. The framework is therefore best viewed as a molecular screening layer that can guide subsequent experimental validation.",
  "",
  "The machine-learning results illustrate both the usefulness and limitations of marker-rule-derived modelling. Internal cross-validation was high because the models learned reproducible transcriptomic patterns related to the frozen prioritization framework. Leave-one-dataset-out performance was substantially lower, indicating dataset shift and limited generalization. The leakage audit was therefore essential: it prevented direct score-derived variables from being presented as predictive features and separated rule recapitulation from reduced-feature modelling. Negative controls showed that the real feature–label structure exceeded random permutations, but this does not convert the models into prospective clinical predictors.",
  "",
  "External application produced an important context-dependent result. The GSE183248 primary external dataset was not forced to match the preferred discovery pattern; all recovered clusters were classified as safety-risk-like. In contrast, GSE243639 disease-context analysis recovered several ideal-like signature-space clusters. This difference supports biological and technical context dependence rather than universal replication. It also argues for reporting external validation as an assessment of framework behaviour under dataset shift, rather than as a binary confirmation of an ideal state.",
  "",
  "Public-data integration inevitably reflects differences in species, protocol, tissue source, developmental stage and annotation [7,23]. The study addressed these issues by separating discovery, reference, primary external and disease-context roles; freezing markers and thresholds before external application; auditing missing or unscored objects; and preserving conservative claim boundaries. Nevertheless, formal prospective validation in standardized differentiation and transplantation systems remains necessary.",
  "",
  "The framework could support future experimental design by prioritizing cell states or differentiation batches for deeper evaluation, selecting markers for flow cytometry or spatial validation, and defining hypotheses for cell–cell communication, regulatory-network perturbation or virtual knockout analyses. Such extensions should be treated as mechanistic hypothesis generation. Definitive validation would require transplantation outcomes, histology, anatomical tracing, electrophysiology and behavioural assessment.",
  "",
  "## Limitations",
  "",
  "",
  "First, the analysis used public datasets generated with heterogeneous protocols, organisms, developmental stages and study designs. Dataset-specific effects remained evident in leave-one-dataset-out validation and external applications.",
  "",
  "Second, ideal-like and safety-risk-associated labels were marker-rule-derived labels derived from a frozen transcriptomic scoring framework rather than experimentally measured graft outcomes. High internal model performance therefore reflects recapitulation of the prioritization structure, not clinical prediction.",
  "",
  "Third, projection-associated molecular competence does not demonstrate anatomical projection, target innervation or synaptic integration. A9/A10-like signatures likewise represent molecular similarity rather than definitive anatomical subtype identity.",
  "",
  "Fourth, safety-risk-associated transcriptional states do not prove tumorigenicity or clinical risk. Experimental proliferation assays, long-term transplantation, histology and functional testing are required.",
  "",
  "Fifth, the primary external dataset GSE183248 produced exclusively safety-risk-like recovered clusters, demonstrating context dependence but limiting claims of broad ideal-like replication.",
  "",
  "Sixth, GSE243639 was analysed using a marker-targeted local-import strategy because the processed count file produced a terminal incomplete-gzip warning. Although 33,525 expression-table rows contributed to library-size estimation and all 121 frozen marker genes were recovered, this analysis was not a full-transcriptome raw-data reanalysis.",
  "",
  "Finally, no wet-lab experiment, transplantation outcome, behavioural phenotype, anatomical tracing or electrophysiological validation was performed.",
  "",
  "## Conclusion",
  "",
  "",
  "We established a frozen, leakage-aware transcriptomic prioritization framework for dopaminergic graft-relevant cell states. The framework distinguishes ideal-like DA/projection-associated/safety-low states from safety-risk-associated or lower-priority states and is supported by coherent pathway differences, threshold robustness, negative controls and context-dependent external applications. Its intended use is molecular prioritization and hypothesis generation. Functional graft performance, clinical safety and therapeutic efficacy remain to be established experimentally.",
  "",
  "## References",
  "",
  "1. Bloem BR, Okun MS, Klein C. Parkinson's disease. Lancet. 2021;397(10291):2284-2303. doi:10.1016/S0140-6736(21)00218-X. PMID:33848468.",
  "2. Kriks S, Shim JW, Piao J, et al. Dopamine neurons derived from human ES cells efficiently engraft in animal models of Parkinson's disease. Nature. 2011;480(7378):547-551. doi:10.1038/nature10648. PMID:22056989.",
  "3. Grealish S, Diguet E, Kirkeby A, et al. Human ESC-derived dopamine neurons show similar preclinical efficacy and potency to fetal neurons when grafted in a rat model of Parkinson's disease. Cell Stem Cell. 2014;15(5):653-665. doi:10.1016/j.stem.2014.09.017. PMID:25517469.",
  "4. Tiklová K, Nolbrant S, Fiorenzano A, et al. Single cell transcriptomics identifies stem cell-derived graft composition in a model of Parkinson's disease. Nat Commun. 2020;11(1):2434. doi:10.1038/s41467-020-16225-5. PMID:32415072.",
  "5. Doi D, Magotani H, Kikuchi T, et al. Pre-clinical study of induced pluripotent stem cell-derived dopaminergic progenitor cells for Parkinson's disease. Nat Commun. 2020;11(1):3369. doi:10.1038/s41467-020-17165-w. PMID:32632153.",
  "6. Luecken MD, Theis FJ. Current best practices in single-cell RNA-seq analysis: a tutorial. Mol Syst Biol. 2019;15(6):e8746. doi:10.15252/msb.20188746. PMID:31217225.",
  "7. Luecken MD, Büttner M, Chaichoompu K, et al. Benchmarking atlas-level data integration in single-cell genomics. Nat Methods. 2022;19(1):41-50. doi:10.1038/s41592-021-01336-8. PMID:34949812.",
  "8. Kamath T, Abdul A, Burris SJ, et al. Single-cell genomic profiling of human dopamine neurons identifies a population that selectively degenerates in Parkinson's disease. Nat Neurosci. 2022;25(5):588-595. doi:10.1038/s41593-022-01061-1. PMID:35513515. GEO:GSE178265.",
  "9. Smajić S, Prada-Medina CA, Landoulsi Z, et al. Single-cell sequencing of human midbrain reveals glial activation and a Parkinson-specific neuronal state. Brain. 2022;145(3):964-978. doi:10.1093/brain/awab446. PMID:34919646. GEO:GSE157783.",
  "10. Xu P, He H, Gao Q, Zhou Y, Wu Z, Zhang X, et al. Human midbrain dopaminergic neuronal differentiation markers predict cell therapy outcomes in a Parkinson's disease model. J Clin Invest. 2022;132(14):e156768. doi:10.1172/JCI156768. PMID:35700056. GEO:GSE204795 and GSE204796.",
  "11. Novak G, Kyriakis D, Grzyb K, et al. Single-cell transcriptomics of human iPSC differentiation dynamics reveal a core molecular network of Parkinson's disease. Commun Biol. 2022;5(1):49. doi:10.1038/s42003-021-02973-7. PMID:35027645. GEO:GSE183248.",
  "12. Storm P, Zhang Y, Nilsson F, Fiorenzano A, Krausse N, Åkerblom M, et al. Lineage tracing of stem cell-derived dopamine grafts in a Parkinson's model reveals shared origin of all graft-derived cells. Sci Adv. 2024;10(42):eadn3057. doi:10.1126/sciadv.adn3057. PMID:39423273. GEO:GSE200610.",
  "13. Fiorenzano A, Storm P, Sozzi E, Bruzelius A, Corsi S, Kajtez J, et al. TARGET-seq: Linking single-cell transcriptomics of human dopaminergic neurons with their target specificity. Proc Natl Acad Sci U S A. 2024;121(47):e2410331121. doi:10.1073/pnas.2410331121. PMID:39541349. GEO:GSE233885.",
  "14. Martirosyan A, Ansari R, Pestana F, Hebestreit K, Gasparyan H, Aleksanyan R, et al. Unravelling cell type-specific responses to Parkinson's disease at single-cell resolution. Mol Neurodegener. 2024;19(1):7. doi:10.1186/s13024-023-00699-0. PMID:38245794. GEO:GSE243639.",
  "15. Stuart T, Butler A, Hoffman P, et al. Comprehensive integration of single-cell data. Cell. 2019;177(7):1888-1902.e21. doi:10.1016/j.cell.2019.05.031. PMID:31178118.",
  "16. Hao Y, Hao S, Andersen-Nissen E, et al. Integrated analysis of multimodal single-cell data. Cell. 2021;184(13):3573-3587.e29. doi:10.1016/j.cell.2021.04.048. PMID:34062119.",
  "17. Wu T, Hu E, Xu S, et al. clusterProfiler 4.0: A universal enrichment tool for interpreting omics data. Innovation (Camb). 2021;2(3):100141. doi:10.1016/j.xinn.2021.100141. PMID:34557778.",
  "18. Subramanian A, Tamayo P, Mootha VK, et al. Gene set enrichment analysis: a knowledge-based approach for interpreting genome-wide expression profiles. Proc Natl Acad Sci U S A. 2005;102(43):15545-15550. doi:10.1073/pnas.0506580102. PMID:16199517.",
  "19. Liberzon A, Birger C, Thorvaldsdóttir H, Ghandi M, Mesirov JP, Tamayo P. The Molecular Signatures Database hallmark gene set collection. Cell Syst. 2015;1(6):417-425. doi:10.1016/j.cels.2015.12.004. PMID:26771021.",
  "20. Breiman L. Random forests. Mach Learn. 2001;45(1):5-32. doi:10.1023/A:1010933404324.",
  "21. Varma S, Simon R. Bias in error estimation when using cross-validation for model selection. BMC Bioinformatics. 2006;7:91. doi:10.1186/1471-2105-7-91. PMID:16504092.",
  "22. Poulin JF, Gaertner Z, Moreno-Ramos OA, Awatramani R. Classification of midbrain dopamine neurons using single-cell gene expression profiling approaches. Trends Neurosci. 2020;43(3):155-169. doi:10.1016/j.tins.2020.01.004. PMID:32101709.",
  "23. Heumos L, Schaar AC, Lance C, et al. Best practices for single-cell analysis across modalities. Nat Rev Genet. 2023;24(8):550-572. doi:10.1038/s41576-023-00586-w. PMID:37002403.",
  "24. Barrett T, Wilhite SE, Ledoux P, et al. NCBI GEO: archive for functional genomics data sets—update. Nucleic Acids Res. 2013;41(Database issue):D991-D995. doi:10.1093/nar/gks1193. PMID:23193258.",
  "",
  "25. Korotkevich G, Sukhov V, Budin N, Shpak B, Artyomov MN, Sergushichev A. Fast gene set enrichment analysis. bioRxiv. 2021:060012. doi:10.1101/060012.",
  "",
  "## Declarations",
  "",
  "### Ethics approval and consent to participate",
  "",
  "Not applicable. This study analysed publicly available, previously generated datasets and did not recruit new participants or generate new identifiable human data. Ethical approval and informed-consent procedures were the responsibility of the original studies.",
  "",
  "### Consent for publication",
  "",
  "Not applicable.",
  "",
  "### Availability of data and materials",
  "",
  "All datasets analysed in this study are publicly available through the NCBI Gene Expression Omnibus under the accession numbers reported in the Methods and reference list.",
  "",
  "### Code availability",
  "",
  "The analysis scripts, final module manifests, package requirements and reproducibility documentation will be deposited in a public code repository before submission. The repository URL will be added to the submitted manuscript.",
  "",
  "### Competing interests",
  "",
  "The author declares no competing interests.",
  "",
  "### Funding",
  "",
  "This research received no specific grant from any funding agency in the public, commercial or not-for-profit sectors.",
  "",
  "### Author contributions",
  "",
  "Conceptualization, Methodology, Software, Validation, Formal analysis, Investigation, Data curation, Visualization, Writing – original draft, Writing – review & editing, and Project administration: Hongze Ma. The author read and approved the final manuscript.",
  "",
  "### Acknowledgements",
  "",
  "The author thanks the investigators who generated and publicly shared the datasets used in this study."
)

manuscript_text <- paste(
  manuscript_lines,
  collapse = "\n"
)


# ============================================================
# 3. Exact parameter table
# ============================================================

stamp("生成最终参数表。")

parameter_table <- data.frame(
  module = c(
    rep("05A", 8),
    rep("05B", 12),
    rep("09C", 9),
    rep("09G", 6),
    rep("09H", 5),
    rep("09I", 9)
  ),
  parameter = c(
    "Scoring seed",
    "Count normalization",
    "Category score",
    "DA-like composite",
    "Projection composite",
    "DA-projection composite",
    "A9-like bias threshold",
    "A10-like bias threshold",

    "Safety component weight: cell cycle/proliferation",
    "Safety component weight: progenitor/neuroepithelial",
    "Safety component weight: pluripotency/immature risk",
    "Safety component weight: stress/apoptosis",
    "Safety component weight: ECM/fibroblast",
    "Safety component weight: vascular/pericyte/meningeal",
    "Safety low maximum",
    "Safety high minimum",
    "Ideal DA minimum",
    "Ideal projection minimum",
    "DA present minimum",
    "Projection present minimum",

    "Model seed",
    "Internal CV folds",
    "Logistic maximum selected features",
    "Logistic imputation",
    "Logistic scaling",
    "Random-forest trees",
    "Random-forest mtry",
    "Binary probability threshold",
    "LODO design",

    "Principal diagonal settings",
    "Stable-class minimum consistency",
    "Diagonal ideal-high quantiles",
    "Diagonal safety-low quantiles",
    "Diagonal safety-high quantiles",
    "Full threshold grid size and purpose",

    "Negative-control seed",
    "Negative-control folds",
    "Negative-control repeats per null type",
    "Negative-control RF trees",
    "Negative-control designs",

    "Disease-context seed",
    "Import chunk size",
    "Retained marker genes",
    "Signature dimensions",
    "Count normalization",
    "Requested k",
    "Minimum cells per requested cluster",
    "K-means nstart",
    "K-means iter.max"
  ),
  value = c(
    "20260714",
    "counts per 10,000 followed by log1p when counts were used",
    "mean expression of detected markers in each category",
    "mean(DA core, DA functional machinery, neuronal maturation/synapse)",
    "mean(projection-associated competence, neuronal maturation/synapse)",
    "mean(DA-like composite, projection composite)",
    "> 0.02",
    "< -0.02",

    "1.20",
    "1.00",
    "1.40",
    "0.60",
    "0.40",
    "0.30",
    "0.20",
    "0.35",
    "0.08",
    "0.08",
    "0.05",
    "0.05",

    "20260714",
    "5",
    "10; selected within each training split",
    "training-set median",
    "training-set median and SD",
    "500",
    "floor(sqrt(number of features))",
    "0.5",
    "one dataset held out at a time",

    "5",
    "same dominant class in at least 4 of 5 settings",
    "0.65,0.70,0.75,0.80,0.85",
    "0.35,0.30,0.25,0.20,0.15",
    "0.65,0.70,0.75,0.80,0.85",
    "125 combinations; used for class-fraction, priority-index and rank sensitivity audit",

    "20260715",
    "5",
    "30",
    "300",
    "permuted training labels; independently permuted feature columns",

    "20260715",
    "25 expression-table rows",
    "121",
    "18 frozen category scores",
    "counts per 10,000 followed by log1p",
    "8",
    "30",
    "50",
    "100"
  ),
  claim_boundary = c(
    rep("Transcriptomic scoring only.", 8),
    rep("Safety-risk-associated transcriptional state; not clinical safety proof.", 12),
    rep("Exploratory marker-rule-derived modelling; not clinical prediction.", 9),
    rep("Robustness analysis; not biological causality.", 6),
    rep("Negative-control robustness; not prospective validation.", 5),
    rep("Disease-context signature-space analysis; not definitive cell types or graft validation.", 9)
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(
  parameter_table,
  OUT_PARAMETER_TABLE
)


# ============================================================
# 4. Citation integrity audit
# ============================================================

stamp("执行 citation integrity audit。")

parts <- strsplit(
  manuscript_text,
  "## References",
  fixed = TRUE
)[[1]]

if (length(parts) < 2) {
  stop("正文缺少 References section。")
}

body_text <- parts[1]
reference_text <- parts[2]

citation_matches <- regmatches(
  body_text,
  gregexpr(
    "\\[(\\d+(?:[-,]\\d+)*)\\]",
    body_text,
    perl = TRUE
  )
)[[1]]

citation_matches <- gsub(
  "^\\[|\\]$",
  "",
  citation_matches
)

used_reference_numbers <- sort(
  unique(
    unlist(
      lapply(
        citation_matches,
        expand_citation
      )
    )
  )
)

reference_matches <- regmatches(
  reference_text,
  gregexpr(
    "(?m)^\\d+\\.\\s",
    reference_text,
    perl = TRUE
  )
)[[1]]

reference_numbers <- as.integer(
  gsub(
    "\\D",
    "",
    reference_matches
  )
)

missing_reference_entries <- setdiff(
  used_reference_numbers,
  reference_numbers
)

unused_reference_entries <- setdiff(
  reference_numbers,
  used_reference_numbers
)

remaining_cit_placeholders <- regmatches(
  manuscript_text,
  gregexpr(
    "\\[CIT-\\d+\\]",
    manuscript_text,
    perl = TRUE
  )
)[[1]]

if (length(missing_reference_entries) > 0) {
  stop(
    "存在正文引用但 References 缺失：",
    paste(
      missing_reference_entries,
      collapse = ", "
    )
  )
}

if (length(remaining_cit_placeholders) > 0) {
  stop(
    "仍存在 CIT 占位符：",
    paste(
      remaining_cit_placeholders,
      collapse = ", "
    )
  )
}

citation_audit <- data.frame(
  metric = c(
    "Used reference numbers",
    "Bibliography reference numbers",
    "Citations without bibliography",
    "Unused bibliography entries",
    "Remaining CIT placeholders",
    "Reference count"
  ),
  value = c(
    paste(
      used_reference_numbers,
      collapse = ","
    ),
    paste(
      reference_numbers,
      collapse = ","
    ),
    ifelse(
      length(missing_reference_entries) == 0,
      "none",
      paste(
        missing_reference_entries,
        collapse = ","
      )
    ),
    ifelse(
      length(unused_reference_entries) == 0,
      "none",
      paste(
        unused_reference_entries,
        collapse = ","
      )
    ),
    ifelse(
      length(remaining_cit_placeholders) == 0,
      "none",
      paste(
        remaining_cit_placeholders,
        collapse = ","
      )
    ),
    as.character(
      length(reference_numbers)
    )
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(
  citation_audit,
  OUT_CITATION_AUDIT
)


# ============================================================
# 5. Final manuscript integrity audit
# ============================================================

stamp("执行最终 manuscript integrity audit。")

checks <- c(
  "Abstract present" = grepl("## Abstract", manuscript_text, fixed = TRUE),
  "Introduction present" = grepl("## Introduction", manuscript_text, fixed = TRUE),
  "Results present" = grepl("## Results", manuscript_text, fixed = TRUE),
  "Methods present" = grepl("## Methods", manuscript_text, fixed = TRUE),
  "Discussion present" = grepl("## Discussion", manuscript_text, fixed = TRUE),
  "Limitations present" = grepl("## Limitations", manuscript_text, fixed = TRUE),
  "Conclusion present" = grepl("## Conclusion", manuscript_text, fixed = TRUE),
  "References present" = grepl("## References", manuscript_text, fixed = TRUE),
  "Declarations present" = grepl("## Declarations", manuscript_text, fixed = TRUE),
  "No internal GSE178265 object name" = !grepl("GSE178265_DA_01B", manuscript_text, fixed = TRUE),
  "No provisional Figure legends" = !grepl("## Figure legends", manuscript_text, fixed = TRUE),
  "No funding placeholder" = !grepl("[TO BE COMPLETED]", manuscript_text, fixed = TRUE),
  "No CIT placeholders" = !grepl("[CIT-", manuscript_text, fixed = TRUE),
  "Single-author contribution complete" = grepl("Hongze Ma", manuscript_text, fixed = TRUE),
  "GSE243639 uses CP10K" = grepl("counts per 10,000 using the accumulated library size", manuscript_text, fixed = TRUE),
  "GSE243639 does not use CPM" = !grepl("counts per million using the accumulated library size", manuscript_text, fixed = TRUE),
  "09G stability uses 4 of 5" = grepl("at least four of the five settings", manuscript_text, fixed = TRUE),
  "09G Results correctly states 279 of 327" = grepl(
    "279 of 327 groups (85.3%) retained the same dominant classification in at least four settings",
    manuscript_text,
    fixed = TRUE
  ),
  "09G 125-grid purpose is explicit" = grepl(
    "used for full sensitivity auditing of class fractions",
    manuscript_text,
    fixed = TRUE
  ),
  "Final LODO values present" = grepl("0.64 and 0.63", manuscript_text, fixed = TRUE) &&
    grepl("0.58 and 0.53", manuscript_text, fixed = TRUE),
  "Score equations present" = grepl("The DA-like composite score was defined", manuscript_text, fixed = TRUE),
  "Safety weights present" = grepl("cell-cycle/proliferation (weight 1.20)", manuscript_text, fixed = TRUE),
  "ML parameters present" = grepl("500 trees", manuscript_text, fixed = TRUE),
  "Negative-control repeats present" = grepl("30 independent repeats", manuscript_text, fixed = TRUE),
  "09I clustering parameters present" = grepl("nstart = 50 and iter.max = 100", manuscript_text, fixed = TRUE)
)

manuscript_audit <- data.frame(
  check = names(checks),
  status = ifelse(
    checks,
    "pass",
    "fail"
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(
  manuscript_audit,
  OUT_MANUSCRIPT_AUDIT
)

if (any(!checks)) {
  print(
    manuscript_audit[
      manuscript_audit$status == "fail",
      ,
      drop = FALSE
    ]
  )

  stop("Final manuscript integrity audit 未通过。")
}


# ============================================================
# 6. 写出 manuscript / methods / declarations
# ============================================================

stamp("写出最终 manuscript。")

atomic_write_text(
  manuscript_lines,
  OUT_MANUSCRIPT
)

method_start <- grep(
  "^## Methods$",
  manuscript_lines
)

discussion_start <- grep(
  "^## Discussion$",
  manuscript_lines
)

if (length(method_start) != 1 || length(discussion_start) != 1) {
  stop("无法唯一定位 Methods / Discussion section。")
}

method_lines <- manuscript_lines[
  method_start:(discussion_start - 1)
]

atomic_write_text(
  method_lines,
  OUT_METHODS
)

declaration_start <- grep(
  "^## Declarations$",
  manuscript_lines
)

if (length(declaration_start) != 1) {
  stop("无法唯一定位 Declarations section。")
}

declaration_lines <- manuscript_lines[
  declaration_start:length(manuscript_lines)
]

atomic_write_text(
  declaration_lines,
  OUT_DECLARATIONS
)

pending_items <- c(
  "10B submission pending items",
  "",
  "1. Run 10C to select and assemble final main and supplementary figures.",
  "2. Generate final Figure legends only after panel letters are fixed.",
  "3. Export the frozen 04A marker panel as a Supplementary Table with gene, category, rationale, source publication, species and alias handling.",
  "4. Select the target journal and convert References to its exact style using Zotero or EndNote.",
  "5. Add the final public code-repository URL before submission.",
  "6. Add institutional affiliation, correspondence address and ORCID to the title page.",
  "7. Confirm whether the target journal requires a declaration on generative-AI-assisted manuscript preparation.",
  "8. Recheck all numeric values against the final 10C figures and panel labels."
)

atomic_write_text(
  pending_items,
  OUT_PENDING_ITEMS
)

atomic_write_text(
  capture.output(sessionInfo()),
  OUT_SESSION
)


# ============================================================
# 7. 输出验证
# ============================================================

required_outputs <- c(
  OUT_MANUSCRIPT,
  OUT_METHODS,
  OUT_DECLARATIONS,
  OUT_PENDING_ITEMS,
  OUT_PARAMETER_TABLE,
  OUT_CITATION_AUDIT,
  OUT_MANUSCRIPT_AUDIT,
  OUT_SESSION
)

verification <- data.frame(
  file = required_outputs,
  exists = file.exists(required_outputs),
  size_bytes = ifelse(
    file.exists(required_outputs),
    file.info(required_outputs)$size,
    NA_real_
  ),
  stringsAsFactors = FALSE
)

atomic_write_csv(
  verification,
  OUT_VERIFICATION
)

bad_outputs <- verification[
  !verification$exists |
    is.na(verification$size_bytes) |
    verification$size_bytes <= 0,
  ,
  drop = FALSE
]

if (nrow(bad_outputs) > 0) {
  print(bad_outputs)
  stop("10B V7 输出验证失败。")
}


# ============================================================
# 8. 完成
# ============================================================

cat("\n============================================================\n")
cat("10B FINAL STANDALONE V7 运行结束\n")
cat("============================================================\n\n")

cat("Old manuscript required：NO\n")
cat("Old 10B script required：NO\n")
cat("Reference count：", length(reference_numbers), "\n")
cat("Missing reference entries：", length(missing_reference_entries), "\n")
cat("Unused reference entries：", length(unused_reference_entries), "\n")
cat("Remaining CIT placeholders：", length(remaining_cit_placeholders), "\n")
cat("GSE243639 normalization：log1p-CP10K\n")
cat("09G stability definition：same dominant class in at least 4/5 principal settings\n")
cat("09G full grid：125 settings for full sensitivity audit\n")
cat("Figure legends included：NO，deferred to 10C\n")
cat("Images generated：NO\n")
cat("Funding placeholder：NO\n")
cat("Author contribution placeholder：NO\n\n")

cat("核心输出：\n")
cat(OUT_MANUSCRIPT, "\n")
cat(OUT_METHODS, "\n")
cat(OUT_DECLARATIONS, "\n")
cat(OUT_PENDING_ITEMS, "\n")
cat(OUT_PARAMETER_TABLE, "\n")
cat(OUT_CITATION_AUDIT, "\n")
cat(OUT_MANUSCRIPT_AUDIT, "\n\n")

cat("✅ 10B FINAL STANDALONE V7 LOCKED COMPLETE 完成。\n")
