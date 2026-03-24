# MJY Graduation Project (R Empirical Pipeline)

无形资产投资费用化对价值信号扭曲的实证研究 —— 基于中国 A 股市场的永续盘存法重构

## Pipeline 概览

数据清洗 → 永续盘存法重构无形资本 → 月度面板 → 五分组组合 / 因子（Adj / Raw / Delta）
→ Spanning 检验 → 风险调整（FF3 & FF5）→ Size×BM 双排序
→ Fama–MacBeth 回归（含交互项边际效应）→ 迁移矩阵（Raw→Adj）
→ 多切点稳健性 → 高无形行业子样本 → φ 敏感性分析
→ 资本化口径替代 → June ME 分母替代 → NW 带宽比较 → 作图与打包输出

## 如何运行

1. 从 CSMAR 下载所需数据文件，放入同一根目录（本仓库不上传原始数据）
2. 修改脚本第 10 行 `base_path` 为你的数据根目录
3. 在 R / RStudio 中 `source()` 运行
4. 输出自动生成在 `base_path/OUTPUT_PAPER_YYYYmmdd_HHMMSS/` 并打包 ZIP

## 文件结构

```
├── paper_style_full_final_*.R        # 主脚本（实证全流程）
├── outputs/                          # 运行输出
│   ├── Table1_DescStats.csv          #   描述性统计
│   ├── Portfolios_BMAdj/BMRaw.csv    #   五分组收益（VW/EW）
│   ├── Factor_Series_*.csv           #   因子时间序列
│   ├── Table_Factor_Mean_NWt.csv     #   因子均值 & NW t 检验
│   ├── Spanning_*.csv                #   因子跨越回归
│   ├── DeltaAlpha_*.csv              #   Delta 因子风险调整 alpha（FF3 & FF5）
│   ├── FM_*.csv                      #   Fama-MacBeth 回归
│   ├── Table_DoubleSort_Size_BM.csv  #   Size×BM 双排序
│   ├── Migration_*.csv               #   分位数迁移矩阵
│   ├── Robust_Cut_*.csv              #   多切点稳健性
│   ├── HighIntan_*.csv               #   高无形行业子样本
│   ├── Sensitivity_phi_*.csv         #   φ 敏感性分析
│   ├── Supplement_*.csv              #   补充检验（资本化口径 / June ME / NW 带宽）
│   ├── Fig_*.png                     #   累积收益图
│   └── RUN_PARAMS.csv                #   运行参数记录
├── example_output/                   # 早期示例图表
├── docs/                             # 运行环境信息（复现用）
├── data/                             # 数据说明（不含原始数据）
├── .gitignore
└── README.md
```

## Preview

### Cumulative portfolio performance

![BMAdj VW](https://raw.githubusercontent.com/lovelt159710-hash/MJY-s-graduation-project/main/outputs/Fig_Cum_Port_BMAdj_VW.png)

![BMAdj EW](https://raw.githubusercontent.com/lovelt159710-hash/MJY-s-graduation-project/main/outputs/Fig_Cum_Port_BMAdj_EW.png)

### Factor series comparison

![Factors VW](https://raw.githubusercontent.com/lovelt159710-hash/MJY-s-graduation-project/main/outputs/Fig_Cum_Factors_VW.png)

![Factors EW](https://raw.githubusercontent.com/lovelt159710-hash/MJY-s-graduation-project/main/outputs/Fig_Cum_Factors_EW.png)

## 核心方法

- **永续盘存法**（Peters & Taylor, 2017）：对 R&D 和 SGA 中的无形资产投资成分按 15%/20% 折旧率资本化
- **Fama-MacBeth 横截面回归**：Newey-West 修正标准误
- **Fama-French 三因子 / 五因子模型**：因子跨越 & Delta-Alpha 检验
- **稳健性检验**：多切点（2012/2015/2018）、高无形行业子样本、φ 敏感性、资本化口径替代、June ME 分母替代、NW 带宽比较

## 数据说明

- 样本期间：2008–2025（月度，公司-月面板）
- 数据来源：CSMAR（国泰安）
- 原始数据受版权限制，**不包含在本仓库中**

## 核心参考文献

- Peters, R. H., & Taylor, L. A. (2017). Intangible capital and the investment-q relation. *Journal of Financial Economics*, 123(2), 251–272.
- Fama, E. F., & French, K. R. (1993). Common risk factors in the returns on stocks and bonds. *Journal of Financial Economics*, 33(1), 3–56.
- Fama, E. F., & MacBeth, J. D. (1973). Risk, return, and equilibrium: Empirical tests. *Journal of Political Economy*, 81(3), 607–636.
