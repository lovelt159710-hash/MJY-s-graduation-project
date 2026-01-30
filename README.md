# MJY Graduation Project (R Empirical Pipeline)

This repo contains an end-to-end empirical pipeline in R:
cleaning → panel → portfolios/factors → spanning → risk-adjusted tests
→ double-sort → Fama–MacBeth → migration → robustness cuts → plots.

## Quick Start
1) Put raw data into the required folder structure (not included in this repo).
2) Run in R:
source("paper_style_annotated_pipeline.R")

Outputs will be created under:
base_path/OUTPUT_PAPER_YYYYmmdd_HHMMSS/

## Folders
- example_output/: selected figures & summary tables (for display)
- docs/: run params + R session info (for reproducibility)
- data/: instructions only (no raw data uploaded)

## Preview
### Cumulative portfolio performance
![BMAdj VW](example_output/Fig_Cum_Port_BMAdj_VW.png)
![BMAdj EW](example_output/Fig_Cum_Port_BMAdj_EW.png)

### Factor series comparison
![Factors VW](example_output/Fig_Cum_Factors_VW.png)
![Factors EW](example_output/Fig_Cum_Factors_EW.png)

---

# 中文说明（README）

## 项目简介
本仓库是本人毕业论文的GitHub 版本，包含：
数据清洗 → 构造月度面板 → 五分组组合/因子（Adj/Raw/Delta）→ Spanning 检验 → 风险调整（Delta~FF3）
→ Size×BM 双排序 → Fama–MacBeth 回归（含交互项边际效应）→ 迁移矩阵（Raw→Adj）
→ 多切点稳健性（2012/2015/2018）→ 高无形行业子样本 → 作图与打包输出。

## 如何运行
1. 将原始数据放入你本机 `base_path` 下对应文件夹（本仓库不上传原始数据）。
2. 打开 R / RStudio，运行：
   - `source("paper_style_annotated_pipeline.R")`
3. 运行结束后，会在 `base_path/OUTPUT_PAPER_YYYYmmdd_HHMMSS/` 生成所有结果文件，并自动打包 zip。

## 输出文件说明（核心）
- `Table1_DescStats.csv`：描述性统计（Table 1）
- `Portfolios_BMAdj.csv` / `Portfolios_BMRaw.csv`：五分组收益（VW/EW）
- `Factor_Series_Adj_Raw_Delta.csv`：Adj/Raw/Delta 因子序列
- `Table_Factor_Mean_NWt.csv`：因子均值与 Newey–West t 值
- `Spanning_*_NW.csv`：Spanning 回归（HML_Adj ~ HML_Raw）
- `DeltaAlpha_*_NW.csv`：Delta 因子对 FF3 的风险调整 alpha
- `Table_DoubleSort_Size_BM.csv`：Size×BM 双排序（Small/Big 内 HML）
- `FM_*`：Fama–MacBeth（OLS/WLS、Size 分组、交互项边际效应）
- `Migration_*`：Raw quintile → Adj quintile 迁移矩阵与 shift 统计
- `Robust_Cut_*`：切点稳健性（Pre/Post × EW/VW）
- `HighIntan_*`：高无形行业子样本结果
- `Fig_*.png`：核心图（累计净值、因子累计表现等）
- `docs/sessionInfo.txt`：R 环境信息（复现用）
- `docs/RUN_PARAMS.csv`：本次运行参数记录

## 复现与合规说明
- 本仓库 **不包含原始数据**（受数据库/版权限制），仅提供代码与可复现的目录结构说明。
- `OUTPUT_PAPER_* / *.zip / *.rds / 原始 Excel` 均已在 `.gitignore` 中忽略，避免误传大文件与原始数据。

