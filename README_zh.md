# 多巴胺能神经元／移植物相关转录组候选细胞状态优先级框架

本仓库提供一个来源可追溯的计算转录组框架，通过同时评估功能身份、成熟相关证据与风险相关转录程序，对多巴胺能神经元及移植物相关候选细胞状态进行优先级排序。

**公开模型名称：** marker-rule-derived prioritisation model（标志物规则衍生的优先级模型）。

## 项目总览

### 12P V14 Figure 006

公开 Figure 006 已替换为原始 cluster 上色版 UMAP：图上不再显示编号或文字；右侧 legend 用每种颜色对应一个原始 cluster，并给出该 cluster 的 annotation/name。

[打开 Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


### 12P V13 Figure 006

公开 Figure 006 已替换为干净的 annotation-colour UMAP：图中不显示 cluster 编号，只在右侧 legend 显示每种颜色对应的 annotation/state 名称。

[打开 Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


### 12P V12 Figure 006

公开 Figure 006 已替换为更接近 scRNA/publication 论文风格的 cluster UMAP：左侧保留原始 cluster map 和数字编号，右侧用 cluster-name key 解释每个 cluster 的 annotation name。

[打开 Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


### 12P V11 Figure 006

公开 Figure 006 已替换为更接近 scRNA/Nature 论文风格的 cluster UMAP：左侧保留原始 cluster map 和数字编号，右侧用 annotation key 解释每个 cluster 的 majority annotation。

[打开 Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


### 12P V10 Figure 006

公开 Figure 006 已替换为 Nature-style 的真正 cluster-level annotated UMAP：保留原始 cluster 结构，UMAP 上只放简洁的 C0/C1/C2 标签，完整的 `C<cluster> = majority annotation` 放在右侧 annotation key 中。

[打开 Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


### 12P V9 Figure 006

公开 Figure 006 已替换为真正的 cluster-level annotated UMAP：保留原始 cluster 结构，并给每个 cluster 标注 `C<cluster>: <majority annotation>`。这不是 04D/05B 的少数类别归并图。

[打开 Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


### 12P V8 Figure 006

公开 Figure 006 已替换为 GSE132758 05B safety contrast class / DA-projection-associated class UMAP。这个版本不是数字 cluster 图，也不是 04D 的弱展示版，而是更适合 GitHub 展示的 annotation/class 图。

[打开 Figure 006 PDF](figures/12O_final_integrated_package/01_main_single_panel/006_main_10D_V18_main_single_panel_Figure_01_F1B_Representative_discovery-dat.pdf)


![PD_Graft annotated final package overview](figures/overview/PD_Graft_12O_annotated_public_repository_overview.png)

## 最终带 annotation 的图片包

当前 GitHub 展示基于 **12O final integrated figure package**。这个版本已经按你的要求去掉 `06_optional_context_not_for_strong_claims`，并额外加入 annotation 表和可读说明。

公开短文件名与原始文件名对应关系：

`figures/manifests/12P_V4_github_public_figure_filename_mapping.csv`

annotation 表：

`figures/manifests/12P_V4_github_public_figure_annotation_table.csv`

可读版 annotation guide：

`figures/ANNOTATED_FIGURE_GUIDE.md`

### 保留图片组

- `01_main_single_panel`：24 个 PDF
- `02_ml_audit_required_ROC_PR_AUC`：4 个 PDF
- `03_publication_panel_package`：145 个 PDF
- `04_supplementary_supporting_evidence`：10 个 PDF
- `05_audit_boundary_reproducibility`：18 个 PDF

已排除 06 optional context-only 图片：11 个 PDF

## 必需的 ML 审计图

`02_ml_audit_required_ROC_PR_AUC` 文件夹必须保留。它包含 ROC/PR/AUC 性能审计和 feature-importance/marker-overlap 检查。这些图用于支持模型审计和可解释性，但不能解释为临床预测已经成立。

## 结论边界

本项目支持“候选转录组状态优先级”和“候选 marker signature”层面的解释，但不声称：

- 临床预测或患者结局预测；
- 已验证的诊断、预后或治疗 biomarker；
- 已证明的移植物疗效或临床安全性；
- 真实解剖投射；
- barcode 确认的谱系追踪；
- 遗传因果或疾病机制证明。

英文主说明见 [README.md](README.md)。
