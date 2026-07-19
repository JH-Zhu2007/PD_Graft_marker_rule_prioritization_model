# 多巴胺能神经元／移植物相关转录组候选细胞状态优先级框架

本仓库提供一个来源可追溯的计算转录组框架，用于对多巴胺能神经元及移植物相关候选细胞状态进行优先级排序。框架综合考虑功能身份、成熟相关证据和风险相关转录程序，而不是只依赖单一 marker。

**公开模型名称：** marker-rule-derived prioritisation model。

## 科学问题

单纯表达多巴胺能 marker 并不能证明某个候选细胞状态同时具备理想的多巴胺能身份、投射相关分子能力、成熟相关支持，以及较低风险的转录组特征。本项目的目标是建立一个可复现、可审计的转录组优先级框架，用于筛选候选细胞状态和候选 marker signature，供后续实验解释使用。

## 项目总览

![PD_Graft repository overview](figures/overview/PD_Graft_12O_annotated_public_repository_overview.png)

## 精选图片导览

下面嵌入低分辨率预览图，用于快速浏览。点击任意预览图可以打开完整 PDF。

### Public Figure 006

[![Public Figure 006](figures/selected_highlights_preview_png/01_public_figure_006.png)](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)

**类别：** 主图 UMAP

最终 GitHub 展示用 annotation-colour UMAP。

### DA/projection 分子评分

[![DA/projection 分子评分](figures/selected_highlights_preview_png/02_projection_score.png)](figures/12O_final_integrated_package/01_main_single_panel/007_main_10D_V18_main_single_panel_Figure_02_F1C_DA_projection-associated_mol.pdf)

**类别：** 主图证据

展示 projection-associated molecular competence 与 DA 相关评分证据。

### Safety-risk 转录评分

[![Safety-risk 转录评分](figures/selected_highlights_preview_png/03_safety_risk_score.png)](figures/12O_final_integrated_package/01_main_single_panel/008_main_10D_V18_main_single_panel_Figure_03_F1D_Safety-risk-associated_trans.pdf)

**类别：** 主图证据

展示优先级框架中使用的风险相关转录程序。

### 候选状态 signature heatmap

[![候选状态 signature heatmap](figures/selected_highlights_preview_png/04_signature_heatmap.png)](figures/12O_final_integrated_package/01_main_single_panel/010_main_10D_V18_main_single_panel_Figure_05_F2A_Candidate-state_signature_he.pdf)

**类别：** 主图证据

总结候选细胞状态的 marker/signature 支持。

### 通路 / 富集证据

[![通路 / 富集证据](figures/selected_highlights_preview_png/05_pathway_enrichment.png)](figures/12O_final_integrated_package/01_main_single_panel/014_main_10D_V18_main_single_panel_Figure_09_F2E_Hallmark_GSEA_10D_V18_single.pdf)

**类别：** 通路证据

GO/KEGG/Hallmark 富集证据，用于支持转录组解释。

### ROC/PR/AUC 性能审计

[![ROC/PR/AUC 性能审计](figures/selected_highlights_preview_png/06_roc_pr_auc.png)](figures/12O_final_integrated_package/02_ml_audit_required_ROC_PR_AUC/031_ml_auc_11J_ML_audit_ROC_PR_AUC_11J_FINAL_FigB_ROC_PR_performance_audit.pdf)

**类别：** 机器学习审计

必需的机器学习审计图，用于记录 ROC/PR/AUC 检查。

### Feature importance / marker overlap 审计

[![Feature importance / marker overlap 审计](figures/selected_highlights_preview_png/07_feature_importance.png)](figures/12O_final_integrated_package/02_ml_audit_required_ROC_PR_AUC/032_ml_auc_11J_ML_audit_ROC_PR_AUC_11J_FINAL_FigC_feature_importance_marker_o.pdf)

**类别：** 机器学习审计

审计模型 feature 是否与 marker-rule-derived 证据重叠。

### 外部验证支持

[![外部验证支持](figures/selected_highlights_preview_png/08_external_validation.png)](figures/12O_final_integrated_package/01_main_single_panel/022_main_10D_V18_main_single_panel_Figure_17_F4C_GSE183248_external_priority_.pdf)

**类别：** 外部验证

为优先级输出提供外部/上下文转录组支持。

### 疾病背景支持

[![疾病背景支持](figures/selected_highlights_preview_png/09_disease_context.png)](figures/12O_final_integrated_package/01_main_single_panel/029_main_10D_V18_main_single_panel_Figure_24_F5E_GSE243639_context_priority_i.pdf)

**类别：** 疾病背景支持

用于解释候选状态的疾病背景相关转录组证据。

## 本仓库包含什么

- 可复现 R 分析脚本。
- 数据来源 manifest 和 provenance 表。
- 支持来源追溯的数据集 metadata 与 audit 文件。
- 最终 GitHub 展示用 integrated figure package。
- 必需的 ROC/PR/AUC 机器学习审计图。
- claim-boundary / no-overclaim 审计材料。
- 英文和中文公开项目说明。

本仓库不重新分发 raw GEO 数据、大型中间 R 对象、私人本地文件或投稿系统专用材料。

## 最终图片包

最终公开图片包位于：`figures/12O_final_integrated_package`。

保留图片组：

- `01_main_single_panel`：24 个 PDF
- `02_ml_audit_required_ROC_PR_AUC`：4 个 PDF
- `03_publication_panel_package`：145 个 PDF
- `04_supplementary_supporting_evidence`：10 个 PDF
- `05_audit_boundary_reproducibility`：18 个 PDF

当前检测到的公开 PDF 总数：201。

`06_optional_context_not_for_strong_claims` 被有意排除在公开展示包之外。

## Public Figure 006

Public Figure 006 使用最终选定的 annotation-colour UMAP 展示版本：图中不显示 cluster 编号、n、maj 或 majority percentage；右侧 legend 说明每种颜色代表什么 annotation/state。

[打开 Public Figure 006](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)

## 机器学习审计

`02_ml_audit_required_ROC_PR_AUC` 文件夹被有意保留。该部分用于记录 ROC/PR/AUC 相关模型性能检查和 feature/marker-overlap 审计。它支持 marker-rule-derived prioritisation framework 的可审计性，但不代表已经建立临床预测模型。

## 仓库结构

```text
docs/          面向 manuscript 的说明或解释文件
figures/       公开图片包、overview 图、annotation guide 和 manifests
metadata/      数据集 metadata 和 provenance 支持文件
scripts/       可复现分析与作图脚本
tables/        公开表格和 manifest-style 输出
README.md      英文公开说明
README_zh.md   中文公开说明
```

## 可追溯文件

- 公开短文件名映射表：`figures/manifests/12P_V4_github_public_figure_filename_mapping.csv`
- 图片 annotation 表：`figures/manifests/12P_V4_github_public_figure_annotation_table.csv`
- 可读版图片说明：`figures/ANNOTATED_FIGURE_GUIDE.md`

## 解释边界

### 本项目支持的解释

- 来源可追溯的计算转录组优先级框架。
- 候选转录组细胞状态优先级排序。
- 候选 marker signature 和 module-score 支持。
- marker-rule-derived prioritisation model 审计。
- 转录组层面的外部/上下文证据支持。

### 本项目不声称

- 临床使用预测。
- 患者结局预测。
- 治疗反应预测。
- 已验证的诊断、预后或治疗 biomarker 发现。
- 移植物疗效或临床安全性预测。
- 解剖投射证明。
- barcode 确认的谱系追踪。
- 遗传因果或疾病机制证明。

## 可复现性

公开原始数据应从其原始数据库获取。本 GitHub package 主要提供脚本、metadata、manifest、可追溯记录和必要的公开展示结果图，用于理解和审计计算流程。

英文主说明见 [README.md](README.md)。
