# ==============================================================================
# 论文实证流程
# 锁定读取Excel（按关键词 must_pattern；否则选最新修改时间）
# 全流程输出：Table1/组合/因子/Mean+NW/Spanning/Delta~FF3/双排序/FM/迁移/多cut/高无形行业/作图/备份/ZIP
# 修复：warnings.txt 自检时序（自检前提前生成）
# 输出：base_path/OUTPUT_PAPER_YYYYmmdd_HHMMSS/ 以及 zip 包
# ==============================================================================


# ------------------------------------------------------------------------------
# 【写法对照】
# 1) 数据来源：CSMAR/数据库导出的月度收益、市值、财报（资产负债表/利润表）、研发支出、FF3 因子等。
# 2) 样本构造：按月形成股票-月份面板；用信息可得性规则避免未来信息（Match_Year）；剔除上市不足 12 个月的股票。
# 3) 核心变量：
#    - BM_Raw：传统账面市值比（Book Equity / Market Equity）
#    - PIM 无形资本：对 RD 与 SGA（部分）采用永续盘存法（Perpetual Inventory Method）累积形成 K_Total
#    - BE_Adj：将无形资本存量加回账面权益，得到“调整后账面权益”
#    - BM_Adj：调整后 BM；Delta_BM = BM_Adj - BM_Raw 衡量“确认无形资产后的价值因子增量”
# 4) 主要实证输出（与论文表图的对应关系，便于写作/复现）：
#    - Table1_DescStats.csv：描述性统计（Table 1）
#    - Portfolios_*.csv + Fig_Cum_*.png：五分组组合与累计净值（Figure / Appendix）
#    - Table_Factor_Mean_NWt.csv：因子均值与 Newey-West t（主结果表）
#    - Spanning_*.csv：spanning 回归（Adj 是否被 Raw “解释”）
#    - DeltaAlpha_*.csv：Delta 因子对 FF3 的 alpha（检验增量是否为独立风险溢价）
#    - FM_*.csv：Fama-MacBeth 横截面回归（解释收益的定价回归）
#    - Migration_*.csv：Raw→Adj 分位迁移（机制/直观展示）
#    - Robust_Cut_*.csv：结构性断点稳健性（2012/2015/2018）
#    - HighIntan_*.csv：高无形行业子样本（异质性/机制稳健）
# 5) 复现文件：RUN_PARAMS.csv / sessionInfo.txt / run_log.txt / warnings.txt
# ------------------------------------------------------------------------------

rm(list = ls()); gc()
options(stringsAsFactors = FALSE)
options(warn = 1)

# ---------- 你只需要改这里 ----------
# 【论文复现提示】
# - base_path：本地数据与输出的根目录；建议不要包含中文空格等特殊字符（路径兼容性更好）
# - min_stocks_per_month：月度横截面样本量门槛；对应论文“样本筛选”部分
# - winsor_prob：缩尾比例；对应论文“异常值处理”
# - nw_lag：Newey-West 滞后阶数；对应论文“标准误处理/自相关稳健”
base_path <- ""   # <-- 改成你的根目录（建议不带尾部 /）
min_stocks_per_month <- 200
winsor_prob <- 0.01
nw_lag <- 3

# 输入文件夹 & 文件名关键词（强烈建议你按自己文件名调整关键词）
# folder_name 必须与你 base_path 下的子文件夹完全一致
pattern_map <- list(
  "Annual Table of Basic Information of Listed Companies" = "LISTEDCOINFO",
  "Monthly stock return rate file" = "TRD_Mnth",
  "Research and Development Investment Status Table" = "LCRDSPENDING|RD",
  "Balance Sheet" = "FS_Combas|Combas",
  "Income Statement" = "FS_Comins|Comins",
  "Three-factor model indicators (monthly)" = "THRFACMONTH|THRFAC"
)

# ---------- 1) 加载包 ----------
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(zoo)
  library(ggplot2)
  library(lmtest)
  library(sandwich)
})

# ---------- 2) 输出目录 & 日志 ----------
run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_dir <- file.path(base_path, paste0("OUTPUT_PAPER_", run_id))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(output_dir, "run_log.txt")
sink(log_file, split = TRUE)
on.exit({
  cat("\n=== SCRIPT EXIT ===\n")
  # warnings 落地（退出时再写一次）
  w_txt <- capture.output(warnings())
  if (length(w_txt) == 0) w_txt <- "No warnings."
  writeLines(w_txt, con = file.path(output_dir, "warnings.txt"))
  sink()
}, add = TRUE)

cat("=== RUN START ===\n")
cat("run_id     =", run_id, "\n")
cat("output_dir =", output_dir, "\n\n")

# ---------- 3) 工具函数 ----------
assert_dir <- function(path, name="") {
  if (!dir.exists(path)) stop("warn  找不到目录：", name, " -> ", path)
}

safe_write_csv <- function(df, path){
  # 防呆：空表直接不写
  if (is.null(df) || nrow(df) == 0) {
    cat("[WARN] 空表未写入：", basename(path), "\n")
    return(FALSE)
  }
  tryCatch({
    write.csv(df, path, row.names = FALSE)
    TRUE
  }, error = function(e){
    cat("[WARN] 写入失败:", basename(path), "|", e$message, "\n")
    FALSE
  })
}

brief <- function(df, name){
  cat("\n---", name, "---\n")
  cat("rows =", nrow(df),
      " | stocks =", dplyr::n_distinct(df$Stkcd),
      " | months =", dplyr::n_distinct(df$Date),
      " | Date =", as.character(min(df$Date, na.rm=TRUE)), "~", as.character(max(df$Date, na.rm=TRUE)), "\n")
}

# winsorize：月内分位数缩尾（winsorization）
# - 目的：减少极端值对分组收益/横截面回归的影响（论文“异常值处理”）
# - 按月缩尾而非全样本缩尾：符合因子研究的横截面设置

winsorize <- function(x, prob = 0.01, min_n = 200) {
  ok <- is.finite(x)
  if (sum(ok) < min_n) return(x)
  q <- quantile(x[ok], c(prob, 1 - prob), na.rm = TRUE, type = 7)
  pmin(pmax(x, q[1]), q[2])
}

# nw_mean_t：对时间序列均值进行 Newey-West 检验（自相关/异方差稳健）
# - 典型用于报告因子均值与 t 值（论文主表常见写法）

nw_mean_t <- function(x, lag_nw = 3) {
  x <- x[is.finite(x)]
  fit <- lm(x ~ 1)
  ct  <- lmtest::coeftest(fit, vcov = sandwich::NeweyWest(fit, lag = lag_nw, prewhite = FALSE))
  data.frame(mean = unname(coef(fit)[1]),
             t    = unname(ct[1,3]),
             p    = unname(ct[1,4]),
             n    = length(x))
}

# nw_coef_table：把回归结果整理为“可直接进论文表”的系数表（含 NW 稳健 se/t/p）
# - 便于把 spanning、alpha 回归、稳健性回归等统一输出成 csv

nw_coef_table <- function(fit, lag_nw = 3) {
  V  <- sandwich::NeweyWest(fit, lag = lag_nw, prewhite = FALSE)
  ct <- lmtest::coeftest(fit, vcov = V)
  m  <- as.matrix(ct)
  data.frame(term = rownames(m),
             estimate = m[,1], se = m[,2], t = m[,3], p = m[,4],
             row.names = NULL, check.names = FALSE)
}

star <- function(p){
  if (is.na(p)) "" else if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
}

# ---------- 4) 读取函数（锁定文件名，否则选最新修改） ----------
# read_clean_raw：输入文件读取策略（论文“数据处理”可描述为：按目录组织、按关键词锁定、取最新版本）
# - col_types="text"：先全部读成字符，后续显式转换，避免股票代码前导 0 丢失/日期误判
# - must_pattern：用正则锁定文件名，防止读错（同一目录多个导出版本时尤其重要）

read_clean_raw <- function(folder_name, must_pattern = NULL) {
  full_folder_path <- file.path(base_path, folder_name)
  assert_dir(full_folder_path, folder_name)

  files <- list.files(full_folder_path, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)
  if (length(files) == 0) stop(paste("warn 没找到文件夹中的 Excel：", folder_name))

  # 按关键词锁定（可多关键词：用 |）
  if (!is.null(must_pattern)) {
    hit <- files[str_detect(tolower(basename(files)), tolower(must_pattern))]
    if (length(hit) == 0) stop("warn 没找到匹配文件：pattern=", must_pattern, " | folder=", folder_name)
    files <- hit
  }

  # 兜底：选最新修改的文件
  files <- files[order(file.info(files)$mtime, decreasing = TRUE)]
  pick <- files[1]

  cat("[READ] folder=", folder_name, " | file=", basename(pick), "\n")
  suppressMessages(readxl::read_xlsx(pick, col_types = "text", na = c("NULL","NA","")))
}

# ---------- 5) 组合/因子/作图 ----------
# 组合构造（portfolio formation）
# - 每月按排序变量（BM_Raw 或 BM_Adj）做 5 分位分组
# - 组内收益：等权(EW)与市值加权(VW)并报（论文常规呈现方式）
# - 风险因子列（RiskPremium/SMB/HML_Std）在这里取 first，是因为同月对所有股票相同

make_portfolios <- function(df, sort_var) {
  df %>%
    group_by(Date) %>%
    mutate(Group = ntile(.data[[sort_var]], 5)) %>%
    group_by(Date, Group) %>%
    summarise(
      Ret_VW = weighted.mean(Ret, w = ME_Float_lag, na.rm = TRUE),
      Ret_EW = mean(Ret, na.rm = TRUE),
      RiskPremium = first(RiskPremium),
      SMB = first(SMB),
      HML_Std = first(HML_Std),
      .groups = "drop"
    )
}

# HML 因子定义（High minus Low）
# - 在 5 分组框架下，HML = 高BM组(G5) - 低BM组(G1)
# - 该定义与经典价值因子一致，便于与文献对比

make_hml <- function(port_df, ret_col) {
  port_df %>%
    select(Date, Group, all_of(ret_col)) %>%
    pivot_wider(names_from = Group, values_from = all_of(ret_col), names_prefix = "G") %>%
    mutate(HML = G5 - G1) %>%
    select(Date, HML)
}

plot_cum_port <- function(port_df, ret_col, file_name, title_txt) {
  plot_data <- port_df %>%
    group_by(Group) %>%
    arrange(Date) %>%
    mutate(Cum = cumprod(1 + .data[[ret_col]])) %>%
    ungroup() %>%
    filter(is.finite(Cum), Cum > 0)

  p <- ggplot(plot_data, aes(x = Date, y = Cum, color = factor(Group))) +
    geom_line(linewidth = 1) +
    scale_y_log10() +
    labs(title = title_txt, subtitle = "第1组=低；第5组=高（对数刻度）",
         x="月份", y="累计净值（log）", color="分组") +
    theme_minimal() + theme(legend.position = "top")

  ggsave(file.path(output_dir, file_name), p, width = 8, height = 5)
  p
}

plot_cum_factor <- function(factor_df, file_name, title_txt) {
  plot_data <- factor_df %>%
    arrange(Date) %>%
    mutate(
      Cum_Adj   = cumprod(1 + HML_Adj),
      Cum_Raw   = cumprod(1 + HML_Raw),
      Cum_Delta = cumprod(1 + HML_Delta)
    ) %>%
    select(Date, Cum_Adj, Cum_Raw, Cum_Delta) %>%
    pivot_longer(-Date, names_to = "Series", values_to = "Cum") %>%
    filter(is.finite(Cum), Cum > 0)

  p <- ggplot(plot_data, aes(x = Date, y = Cum, color = Series)) +
    geom_line(linewidth = 1) +
    scale_y_log10() +
    labs(title = title_txt, subtitle = "Adj / Raw / Delta（对数刻度）",
         x="月份", y="累计净值（log）", color="序列") +
    theme_minimal() + theme(legend.position = "top")

  ggsave(file.path(output_dir, file_name), p, width = 8, height = 5)
  p
}

# ==============================================================================
#  A) 基础信息：上市日期 + 行业（含 2025 覆盖不足修复）
# 【A. 样本基本属性：上市日期与行业】
# - 用途1：上市不足 12 个月的股票会在后续剔除（降低 IPO 异常/数据噪声）
# - 用途2：行业变量用于“高无形行业”子样本划分与控制（异质性/机制）
# - 处理：部分年份行业覆盖不完整时，采用“覆盖接近峰值的最近年份”作为 fallback（保证样本不被过度丢失）
# ==============================================================================
cat("\n[A] 读取基础信息（上市日期+行业）...\n")

df_basic_raw <- read_clean_raw(
  "Annual Table of Basic Information of Listed Companies",
  pattern_map[["Annual Table of Basic Information of Listed Companies"]]
) %>% slice(-(1:2))

df_basic <- df_basic_raw %>%
  mutate(
    Stkcd = sprintf("%06d", as.numeric(Symbol)),
    EndDate = ymd(EndDate),
    Year = year(EndDate),
    IndustryCode = as.character(IndustryCode),
    Listdt = ymd(na_if(str_trim(LISTINGDATE), ""))
  ) %>%
  select(Stkcd, Year, IndustryCode, Listdt) %>%
  distinct(Stkcd, Year, .keep_all = TRUE)

df_listdt <- df_basic %>%
  group_by(Stkcd) %>%
  summarise(
    Listdt = { x <- Listdt; if (all(is.na(x))) as.Date(NA) else min(x, na.rm = TRUE) },
    .groups = "drop"
  )

# 行业覆盖年份修复：找“覆盖量>=峰值90%”的最大年份作为 fallback 年
ind_cov <- df_basic %>% count(Year) %>% arrange(Year)
year_ind_full <- ind_cov %>%
  filter(n >= 0.9 * max(n)) %>%
  summarise(year = max(Year)) %>%
  pull(year)

cat("industry_year_used =", year_ind_full, "\n")
stopifnot(year_ind_full >= 2018)

# ==============================================================================
#  B) 月度收益 & 市值 + MOM12
# 【B. 月度收益与市值：构造因子研究的月频面板基础】
# - Ret：月度收益率；对 |Ret_raw|>2 的观测按“百分数→小数”做缩放（数据库口径差异兼容）
# - ME_lag/ME_Float_lag：使用滞后一期市值做权重（避免当期信息污染；符合资产定价组合构造惯例）
# - MOM12：过去 12 个月动量（不含当月），用于 Fama-MacBeth 回归控制
# - Markettype 过滤：保留常规 A 股/主板等口径（按你数据字典口径）
# ==============================================================================
cat("\n[B] 读取月度收益与市值...\n")

df_ret_raw <- read_clean_raw(
  "Monthly stock return rate file",
  pattern_map[["Monthly stock return rate file"]]
) %>%
  slice(-(1:2)) %>%
  mutate(
    Stkcd = sprintf("%06d", as.numeric(Stkcd)),
    Date  = as.Date(paste0(Trdmnt, "-01")),
    Year  = year(Date),
    Month = month(Date),
    Ret_raw   = as.numeric(Mretwd),
    ME        = as.numeric(Msmvttl),
    ME_Float  = as.numeric(Msmvosd),
    Markettype = as.numeric(Markettype)
  ) %>%
# 样本市场筛选（按数据库 Markettype 编码）
# - 保留常见主板/中小板/创业板等口径；具体含义请对照你数据字典
# - 若你研究范围不同（例如含 B 股/科创板/港股通等），在这里调整编码集合

  filter(Markettype %in% c(1,4,16,32)) %>%
  select(Stkcd, Date, Year, Month, Ret_raw, ME, ME_Float)

df_ret <- df_ret_raw %>%
  arrange(Stkcd, Date) %>%
  mutate(
    Ret = ifelse(is.finite(Ret_raw) & abs(Ret_raw) > 2, Ret_raw / 100, Ret_raw),
    Flag_Scaled = ifelse(is.finite(Ret_raw) & abs(Ret_raw) > 2, 1, 0)
  ) %>%
  group_by(Stkcd) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    ME_lag       = lag(ME, 1),
    ME_Float_lag = lag(ME_Float, 1),
    zret = 1 + Ret,
    MOM12 = zoo::rollapplyr(lag(zret, 1), 12, prod, fill = NA, partial = FALSE) - 1
  ) %>%
  ungroup() %>%
  select(Stkcd, Date, Year, Month, Ret, ME, ME_Float, ME_lag, ME_Float_lag, MOM12, Flag_Scaled)

brief(df_ret, "df_ret")

# ==============================================================================
#  C) 年度：RD、BE、SGA + PIM -> BE_Adj
# 【C. 年度财报与无形资本（PIM）】
# - BE_Raw：账面权益（原始）
# - RD_Exp：研发费用；SGA：销售+管理费用（按文献常取其中一部分视作组织资本投资）
# - PIM（永续盘存法）：K_t = (1-δ)K_{t-1} + I_t，用折旧率 δ 将历史投资累积为“资本存量”
#   本脚本设定：RD δ=0.15；组织资本 δ=0.20；SGA 的 30% 视作组织资本投资（可在论文中说明并做稳健性）
# - BE_Adj = BE_Raw + K_Total：将无形资本“资本化”加回账面权益（构造 Adjusted BM）
# ==============================================================================
cat("\n[C] 读取年度报表（RD/BE/SGA）并计算 PIM...\n")

df_rd <- read_clean_raw(
  "Research and Development Investment Status Table",
  pattern_map[["Research and Development Investment Status Table"]]
) %>%
  slice(-(1:2)) %>%
  rename(Stkcd = Symbol) %>%
  mutate(
    Stkcd  = sprintf("%06d", as.numeric(Stkcd)),
    Accper = ymd(EndDate),
    Year   = year(Accper),
    RD_Exp = as.numeric(RDSpendSum)
  ) %>%
  filter(month(Accper) == 12) %>%
  distinct(Stkcd, Year, .keep_all = TRUE) %>%
  transmute(Stkcd, Year, RD_Exp = ifelse(is.na(RD_Exp), 0, RD_Exp))

df_be <- read_clean_raw(
  "Balance Sheet",
  pattern_map[["Balance Sheet"]]
) %>%
  slice(-(1:2)) %>%
  mutate(
    Stkcd  = sprintf("%06d", as.numeric(Stkcd)),
    Accper = ymd(Accper),
    Year   = year(Accper),
    BE_Raw = as.numeric(A003000000)
  ) %>%
  filter(month(Accper) == 12) %>%
  distinct(Stkcd, Year, .keep_all = TRUE) %>%
  transmute(Stkcd, Year, BE_Raw)

df_profit <- read_clean_raw(
  "Income Statement",
  pattern_map[["Income Statement"]]
) %>%
  slice(-(1:2)) %>%
  mutate(
    Stkcd  = sprintf("%06d", as.numeric(Stkcd)),
    Accper = ymd(Accper),
    Year   = year(Accper),
    Sales_Exp = as.numeric(B001209000),
    Admin_Exp = as.numeric(B001210000)
  ) %>%
  filter(month(Accper) == 12) %>%
  distinct(Stkcd, Year, .keep_all = TRUE) %>%
  mutate(
    Sales_Exp = ifelse(is.na(Sales_Exp), 0, Sales_Exp),
    Admin_Exp = ifelse(is.na(Admin_Exp), 0, Admin_Exp),
    SGA = Sales_Exp + Admin_Exp
  ) %>%
  transmute(Stkcd, Year, SGA)

# calc_pim：永续盘存法（PIM）把“当期投入”转成“资本存量”
# - inv：当期投资/费用（如 RD_Exp 或 SGA 的一部分）
# - delta：折旧率 δ
# - 输出：资本存量序列 K_t
# 论文写法：K_t = (1-δ)K_{t-1} + I_t，K_0 = I_0（或设定为0/首期投资，按文献口径说明）

calc_pim <- function(inv, delta) {
  n <- length(inv)
  stock <- numeric(n)
  stock[1] <- inv[1]
  if (n > 1) for (t in 2:n) stock[t] <- (1 - delta) * stock[t - 1] + inv[t]
  stock
}

df_annual <- df_be %>%
  full_join(df_profit, by = c("Stkcd","Year")) %>%
  full_join(df_rd, by = c("Stkcd","Year")) %>%
  arrange(Stkcd, Year) %>%
  mutate(
    BE_Raw = ifelse(is.na(BE_Raw), 0, BE_Raw),
    SGA    = ifelse(is.na(SGA), 0, SGA),
    RD_Exp = ifelse(is.na(RD_Exp), 0, RD_Exp)
  )

df_pim_res <- df_annual %>%
  group_by(Stkcd) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    K_Know  = calc_pim(RD_Exp, 0.15),
    K_Org   = calc_pim(SGA * 0.30, 0.20),
    K_Total = K_Know + K_Org,
    BE_Adj  = BE_Raw + K_Total
  ) %>%
  ungroup() %>%
  select(Stkcd, Year, BE_Raw, BE_Adj, K_Total)

# ==============================================================================
#  D) FF3 月度因子
# 【D. 风险因子（FF3）】
# - RiskPremium/SMB/HML_Std：作为“已知风险因子”控制与 Delta 因子 alpha 检验的基准
# - MarkettypeID 过滤：确保取到你研究所需市场口径的因子序列
# ==============================================================================
cat("\n[D] 读取 FF3（月度）...\n")

df_ff3 <- read_clean_raw(
  "Three-factor model indicators (monthly)",
  pattern_map[["Three-factor model indicators (monthly)"]]
) %>%
  slice(-(1:2)) %>%
  filter(MarkettypeID == "P9709") %>%
  mutate(
    Date = as.Date(paste0(TradingMonth, "-01")),
    RiskPremium = as.numeric(RiskPremium1),
    SMB = as.numeric(SMB1),
    HML_Std = as.numeric(HML1)
  ) %>%
  select(Date, RiskPremium, SMB, HML_Std)

# ==============================================================================
#  E) 构造最终月度面板 df_final2_f
# 【E. 构造最终月度面板（核心样本）】
# - 信息可得性（避免未来信息）：Match_Year 规则（7月后用上一财年；7月前用上上财年）
# - 上市年龄限制：AgeMonths>=12（降低 IPO/上市初期异常影响）
# - 核心解释变量：
#   BM_Raw = BE_Raw / ME_lag
#   BM_Adj = BE_Adj / ME_lag
#   Delta_BM = BM_Adj - BM_Raw（无形资本调整带来的“增量价值”）
# - 缩尾与样本门槛：按月 winsorize；并要求每月至少 min_stocks_per_month 只股票进入主样本
# ==============================================================================
cat("\n[E] 构造最终月度面板 df_final2_f...\n")

# 信息可得性（Look-ahead bias 控制）
# 月频回测/资产定价常用“每年 7 月更新财务信息”规则：
# - 7 月及以后：使用上一财年（Year-1）的财务变量
# - 7 月之前：使用上上财年（Year-2）的财务变量
# 目的：确保在 t 月形成组合时只使用投资者在当时可获得的会计信息

panel0 <- df_ret %>% mutate(Match_Year = ifelse(Month >= 7, Year - 1L, Year - 2L))
panel1 <- panel0 %>%
  inner_join(df_pim_res %>% mutate(Year = as.integer(Year)),
             by = c("Stkcd"="Stkcd", "Match_Year"="Year"))
panel2 <- panel1 %>% inner_join(df_ff3, by = "Date")
panel3 <- panel2 %>%
  left_join(df_listdt, by = "Stkcd") %>%
  mutate(
    ListYear  = year(Listdt),
    ListMonth = month(Listdt),
    AgeMonths = (Year - ListYear) * 12L + (Month - ListMonth)
  )
# 上市年龄筛选（IPO 异常/数据质量控制）
# - 常见做法：剔除上市未满 12 个月的股票，以减少上市初期波动与信息不完整
# - 同时也降低财务指标匹配在上市初期的缺失问题

panel4 <- panel3 %>% filter(!is.na(AgeMonths), AgeMonths >= 12)

# 行业 join（用 fallback year_ind_full 修复覆盖不足）
# 论文写法建议：将此步骤描述为“为缓解最近年份行业分类缺失导致的样本选择偏误，
# 采用行业信息覆盖较完整的最近年份作为替代分类，并在稳健性中检验不影响主要结论”。
panel5 <- panel4 %>%
  mutate(IndYear = pmin(Year, year_ind_full)) %>%
  left_join(df_basic %>% select(Stkcd, Year, IndustryCode) %>% rename(IndYear = Year),
            by = c("Stkcd","IndYear")) %>%
  filter(!is.na(IndustryCode))

panel6 <- panel5 %>%
  filter(
    is.finite(Ret),
    is.finite(ME_lag), is.finite(ME_Float_lag),
    ME_lag > 0, ME_Float_lag > 0,
    is.finite(BE_Raw), is.finite(BE_Adj),
    BE_Raw > 0, BE_Adj > 0
  ) %>%
  mutate(
# 单位对齐（非常容易在复现时被问）
# - BE_Raw 来自财报，ME_lag 来自市值表；两者在数据库中可能是“元/千元/万元”不同口径
# - 这里乘以 1000 是把市值口径转换到与 BE 同量纲（避免 BM 出现系统性放大/缩小）
# - 建议：在论文数据段落明确说明单位处理；并在 RUN_PARAMS.csv 记录 bm_scale_divisor=1000

    BM_Raw   = BE_Raw / (ME_lag * 1000),
    BM_Adj   = BE_Adj / (ME_lag * 1000),
    K_to_ME  = K_Total / (ME_lag * 1000),
    Delta_BM = BM_Adj - BM_Raw,
    lnME     = log(ME_Float_lag)
  )

# 月内缩尾（winsorization by month）
# - 资产定价研究关注“当月截面”排序与回归；因此按月缩尾更符合研究设计
# - 同时保留观测而非删行，可避免样本选择变化过大

df_final2 <- panel6 %>%
  group_by(Date) %>%
  mutate(
    Ret      = winsorize(Ret,      winsor_prob, min_stocks_per_month),
    BM_Adj   = winsorize(BM_Adj,   winsor_prob, min_stocks_per_month),
    BM_Raw   = winsorize(BM_Raw,   winsor_prob, min_stocks_per_month),
    K_to_ME  = winsorize(K_to_ME,  winsor_prob, min_stocks_per_month),
    Delta_BM = winsorize(Delta_BM, winsor_prob, min_stocks_per_month),
    lnME     = winsorize(lnME,     winsor_prob, min_stocks_per_month),
    MOM12    = winsorize(MOM12,    winsor_prob, min_stocks_per_month)
  ) %>%
  ungroup()

# 月度样本量门槛
# - 若某月可用股票过少，五分组收益与横截面回归会不稳定（甚至无法形成分组）
# - 因此仅保留 n_distinct(Stkcd) >= min_stocks_per_month 的月份进入主样本

df_final2_f <- df_final2 %>%
  group_by(Date) %>%
  filter(n_distinct(Stkcd) >= min_stocks_per_month) %>%
  ungroup()

brief(df_final2_f, "df_final2_f")
stopifnot(n_distinct(df_final2_f$Date) >= 200)
stopifnot(n_distinct(df_final2_f$Stkcd) >= 5000)

# 输出 RUN_PARAMS（复现必备）
params <- data.frame(
  run_id = run_id,
  base_path = base_path,
  min_stocks_per_month = min_stocks_per_month,
  winsor_prob = winsor_prob,
  nw_lag = nw_lag,
  industry_year_used = year_ind_full,
  bm_scale_divisor = 1000
)
safe_write_csv(params, file.path(output_dir, "RUN_PARAMS.csv"))

# ==============================================================================
#  F) Table1 + 月度样本数 + 分组单调性
# 【F. 描述性统计与基本 sanity check】
# - Table1_DescStats.csv：论文 Table 1 的候选（均值、标准差、分位数等）
# - Monthly_n_stocks.csv：样本覆盖（可画图/在数据段落解释样本随时间变化）
# - Check_Group_Monotonicity.csv：BM 分组单调性检查（确保分组排序合理）
# ==============================================================================
cat("\n[F] 输出 Table1 / Monthly_n / 分组单调性...\n")

vars_t1 <- c("Ret","BM_Raw","BM_Adj","K_to_ME","Delta_BM","MOM12","lnME","RiskPremium","SMB","HML_Std")
df_t1 <- df_final2_f %>% select(all_of(vars_t1))

table1 <- df_t1 %>%
  summarise(across(everything(),
                   list(Mean=~mean(.,na.rm=TRUE),
                        SD=~sd(.,na.rm=TRUE),
                        Min=~min(.,na.rm=TRUE),
                        P1=~quantile(.,0.01,na.rm=TRUE),
                        P50=~median(.,na.rm=TRUE),
                        P99=~quantile(.,0.99,na.rm=TRUE),
                        Max=~max(.,na.rm=TRUE)),
                   .names="{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_to=c("Variable","Stat"), names_sep="__") %>%
  pivot_wider(names_from=Stat, values_from=value)

monthly_n <- df_final2_f %>%
  group_by(Date) %>%
  summarise(n_stk = n_distinct(Stkcd), .groups="drop") %>%
  arrange(Date)

group_check <- df_final2_f %>%
  group_by(Date) %>%
  mutate(Group = ntile(BM_Adj, 5)) %>%
  ungroup() %>%
  group_by(Group) %>%
  summarise(
    BM_Adj_mean  = mean(BM_Adj, na.rm=TRUE),
    BM_Raw_mean  = mean(BM_Raw, na.rm=TRUE),
    K_to_ME_mean = mean(K_to_ME, na.rm=TRUE),
    ME_lag_mean  = mean(ME_lag, na.rm=TRUE),
    Ret_mean     = mean(Ret, na.rm=TRUE),
    n = n(),
    .groups="drop"
  )

safe_write_csv(table1,      file.path(output_dir, "Table1_DescStats.csv"))
safe_write_csv(monthly_n,   file.path(output_dir, "Monthly_n_stocks.csv"))
safe_write_csv(group_check, file.path(output_dir, "Check_Group_Monotonicity.csv"))

# ==============================================================================
#  G) 五分组组合 + 因子 Adj/Raw/Delta + Mean/NW + Spanning + Delta~FF3 + 图
# 【G. 组合与因子：Raw vs Adj vs Delta（主结果）】
# - 五分组：每月按 BM 分为 5 组，计算等权(EW)/市值加权(VW)收益
# - HML：G5 - G1，构造价值因子
# - 三条序列：
#   HML_Raw：传统价值因子（原始账面权益）
#   HML_Adj：调整后价值因子（无形资本资本化）
#   HML_Delta = HML_Adj - HML_Raw（“增量价值因子”）
# - spanning：HML_Adj ~ HML_Raw（检验调整后因子是否被传统因子张成）
# - delta alpha：HML_Delta ~ FF3（检验增量是否具有独立 alpha）
# - Fig_Cum_*：累计净值图（可用于论文图或附录图）
# ==============================================================================
cat("\n[G] 输出 5×组合、因子序列、Mean+NW、Spanning、Delta~FF3...\n")

port_adj <- make_portfolios(df_final2_f, "BM_Adj")
port_raw <- make_portfolios(df_final2_f, "BM_Raw")
safe_write_csv(port_adj, file.path(output_dir, "Portfolios_BMAdj.csv"))
safe_write_csv(port_raw, file.path(output_dir, "Portfolios_BMRaw.csv"))

hml_adj_vw <- make_hml(port_adj, "Ret_VW") %>% rename(HML_Adj = HML)
hml_adj_ew <- make_hml(port_adj, "Ret_EW") %>% rename(HML_Adj = HML)
hml_raw_vw <- make_hml(port_raw, "Ret_VW") %>% rename(HML_Raw = HML)
hml_raw_ew <- make_hml(port_raw, "Ret_EW") %>% rename(HML_Raw = HML)

factor_vw <- hml_adj_vw %>%
  left_join(hml_raw_vw, by="Date") %>%
  mutate(HML_Delta = HML_Adj - HML_Raw, WeightType="VW") %>%
  left_join(df_ff3, by="Date")

factor_ew <- hml_adj_ew %>%
  left_join(hml_raw_ew, by="Date") %>%
  mutate(HML_Delta = HML_Adj - HML_Raw, WeightType="EW") %>%
  left_join(df_ff3, by="Date")

factor_all <- bind_rows(factor_vw, factor_ew)
safe_write_csv(factor_all, file.path(output_dir, "Factor_Series_Adj_Raw_Delta.csv"))

mean_table <- bind_rows(
  cbind(Weight="VW", Factor="HML_Adj",   nw_mean_t(factor_vw$HML_Adj,   nw_lag)),
  cbind(Weight="VW", Factor="HML_Raw",   nw_mean_t(factor_vw$HML_Raw,   nw_lag)),
  cbind(Weight="VW", Factor="HML_Delta", nw_mean_t(factor_vw$HML_Delta, nw_lag)),
  cbind(Weight="EW", Factor="HML_Adj",   nw_mean_t(factor_ew$HML_Adj,   nw_lag)),
  cbind(Weight="EW", Factor="HML_Raw",   nw_mean_t(factor_ew$HML_Raw,   nw_lag)),
  cbind(Weight="EW", Factor="HML_Delta", nw_mean_t(factor_ew$HML_Delta, nw_lag))
)
safe_write_csv(mean_table, file.path(output_dir, "Table_Factor_Mean_NWt.csv"))

fit_span_vw <- lm(HML_Adj ~ HML_Raw, data=factor_vw)
fit_span_ew <- lm(HML_Adj ~ HML_Raw, data=factor_ew)
span_vw <- nw_coef_table(fit_span_vw, nw_lag)
span_ew <- nw_coef_table(fit_span_ew, nw_lag)
safe_write_csv(span_vw, file.path(output_dir, "Spanning_VW_NW.csv"))
safe_write_csv(span_ew, file.path(output_dir, "Spanning_EW_NW.csv"))

fit_delta_vw <- lm(HML_Delta ~ RiskPremium + SMB + HML_Std, data=factor_vw)
fit_delta_ew <- lm(HML_Delta ~ RiskPremium + SMB + HML_Std, data=factor_ew)
delta_vw <- nw_coef_table(fit_delta_vw, nw_lag)
delta_ew <- nw_coef_table(fit_delta_ew, nw_lag)
safe_write_csv(delta_vw, file.path(output_dir, "DeltaAlpha_VW_NW.csv"))
safe_write_csv(delta_ew, file.path(output_dir, "DeltaAlpha_EW_NW.csv"))

plot_cum_port(port_adj, "Ret_VW", "Fig_Cum_Port_BMAdj_VW.png", "BM_Adj 五分组累计净值（VW）")
plot_cum_port(port_adj, "Ret_EW", "Fig_Cum_Port_BMAdj_EW.png", "BM_Adj 五分组累计净值（EW）")
plot_cum_factor(factor_vw, "Fig_Cum_Factors_VW.png", "价值因子：Raw vs Adj vs Delta（VW）")
plot_cum_factor(factor_ew, "Fig_Cum_Factors_EW.png", "价值因子：Raw vs Adj vs Delta（EW）")

# ==============================================================================
#  H) 双排序 Size×BM：Small/Big 内 HML（Adj/Raw/Delta）
# 【H. 双排序：Size×BM（经典资产定价展示）】
# - 先按 Size（Small/Big）二分，再在组内按 BM 五分组
# - 在 Small 与 Big 内分别计算 HML（Adj/Raw/Delta 可复用）
# - 输出 Table_DoubleSort_Size_BM.csv：用于展示“效应是否集中在小盘/大盘”
# ==============================================================================
cat("\n[H] 输出 Size×BM 双排序...\n")

make_hml_by_size <- function(df, bm_var, weight = c("EW","VW")) {
  weight <- match.arg(weight)
  df %>%
    group_by(Date) %>%
    mutate(
      SizeGrp = ifelse(ME_Float_lag <= median(ME_Float_lag, na.rm=TRUE), "Small", "Big"),
      BMGrp   = ntile(.data[[bm_var]], 5)
    ) %>%
    group_by(Date, SizeGrp, BMGrp) %>%
    summarise(
      ret = if (weight=="VW") weighted.mean(Ret, w=ME_Float_lag, na.rm=TRUE) else mean(Ret, na.rm=TRUE),
      .groups="drop"
    ) %>%
    pivot_wider(names_from = BMGrp, values_from = ret, names_prefix="G") %>%
    mutate(HML = G5 - G1) %>%
    select(Date, SizeGrp, HML) %>%
    filter(is.finite(HML))
}

hml_adj_size_vw <- make_hml_by_size(df_final2_f, "BM_Adj", "VW") %>% rename(HML_Adj=HML)
hml_raw_size_vw <- make_hml_by_size(df_final2_f, "BM_Raw", "VW") %>% rename(HML_Raw=HML)
hml_adj_size_ew <- make_hml_by_size(df_final2_f, "BM_Adj", "EW") %>% rename(HML_Adj=HML)
hml_raw_size_ew <- make_hml_by_size(df_final2_f, "BM_Raw", "EW") %>% rename(HML_Raw=HML)

fac_size_vw <- hml_adj_size_vw %>% left_join(hml_raw_size_vw, by=c("Date","SizeGrp")) %>%
  mutate(HML_Delta = HML_Adj - HML_Raw, Weight="VW")
fac_size_ew <- hml_adj_size_ew %>% left_join(hml_raw_size_ew, by=c("Date","SizeGrp")) %>%
  mutate(HML_Delta = HML_Adj - HML_Raw, Weight="EW")

fac_size_all <- bind_rows(fac_size_vw, fac_size_ew)

sum_size <- fac_size_all %>%
  group_by(Weight, SizeGrp) %>%
  summarise(
    Adj_mean   = nw_mean_t(HML_Adj,   nw_lag)$mean,
    Adj_t      = nw_mean_t(HML_Adj,   nw_lag)$t,
    Raw_mean   = nw_mean_t(HML_Raw,   nw_lag)$mean,
    Raw_t      = nw_mean_t(HML_Raw,   nw_lag)$t,
    Delta_mean = nw_mean_t(HML_Delta, nw_lag)$mean,
    Delta_t    = nw_mean_t(HML_Delta, nw_lag)$t,
    .groups="drop"
  )
safe_write_csv(sum_size, file.path(output_dir, "Table_DoubleSort_Size_BM.csv"))

# ==============================================================================
#  I) Fama-MacBeth：WLS/OLS + 分大小盘 + 交互项边际效应
# 【I. Fama–MacBeth 横截面回归（定价回归）】
# - 两步法：每月做横截面回归 → 得到系数时间序列 → 对系数均值做 NW t
# - WLS：用 ME_Float_lag 做权重（市值越大权重越高；常见稳健性）
# - OLS：等权回归作为对照
# - Size split：分别在 Small/Big 子样本跑回归（异质性）
# - Interaction：Small dummy 与 Delta_BM 交互，报告 Small 边际效应（机制/异质性更直观）
# ==============================================================================
cat("\n[I] 输出 Fama-MacBeth（WLS/OLS/Size/Interaction）...\n")

nw_t_const <- function(x, lag_nw=3){
  fit <- lm(x ~ 1)
  ct  <- coeftest(fit, vcov = NeweyWest(fit, lag=lag_nw, prewhite=FALSE))
  data.frame(mean=ct[1,1], t=ct[1,3], p=ct[1,4], n=length(x))
}

# WLS
gamma_ts_wls <- df_final2_f %>%
  select(Date, Ret, BM_Raw, Delta_BM, lnME, MOM12, ME_Float_lag) %>%
  filter(is.finite(Ret), is.finite(BM_Raw), is.finite(Delta_BM),
         is.finite(lnME), is.finite(MOM12),
         is.finite(ME_Float_lag), ME_Float_lag > 0) %>%
  group_by(Date) %>%
  do({
    fit <- lm(Ret ~ BM_Raw + Delta_BM + lnME + MOM12, data = ., weights = ME_Float_lag)
    data.frame(g_BMRaw = coef(fit)[["BM_Raw"]],
               g_Delta = coef(fit)[["Delta_BM"]],
               g_lnME  = coef(fit)[["lnME"]],
               g_MOM12 = coef(fit)[["MOM12"]])
  }) %>% ungroup()

fm_wls <- bind_rows(
  cbind(term="BM_Raw",   nw_t_const(gamma_ts_wls$g_BMRaw, nw_lag)),
  cbind(term="Delta_BM", nw_t_const(gamma_ts_wls$g_Delta, nw_lag)),
  cbind(term="lnME",     nw_t_const(gamma_ts_wls$g_lnME,  nw_lag)),
  cbind(term="MOM12",    nw_t_const(gamma_ts_wls$g_MOM12, nw_lag))
) %>% mutate(Weight="WLS", Spec="Full sample")

# OLS
gamma_ts_ols <- df_final2_f %>%
  select(Date, Ret, BM_Raw, Delta_BM, lnME, MOM12) %>%
  filter(is.finite(Ret), is.finite(BM_Raw), is.finite(Delta_BM),
         is.finite(lnME), is.finite(MOM12)) %>%
  group_by(Date) %>%
  do({
    fit <- lm(Ret ~ BM_Raw + Delta_BM + lnME + MOM12, data = .)
    data.frame(g_BMRaw = coef(fit)[["BM_Raw"]],
               g_Delta = coef(fit)[["Delta_BM"]],
               g_lnME  = coef(fit)[["lnME"]],
               g_MOM12 = coef(fit)[["MOM12"]])
  }) %>% ungroup()

fm_ols <- bind_rows(
  cbind(term="BM_Raw",   nw_t_const(gamma_ts_ols$g_BMRaw, nw_lag)),
  cbind(term="Delta_BM", nw_t_const(gamma_ts_ols$g_Delta, nw_lag)),
  cbind(term="lnME",     nw_t_const(gamma_ts_ols$g_lnME,  nw_lag)),
  cbind(term="MOM12",    nw_t_const(gamma_ts_ols$g_MOM12, nw_lag))
) %>% mutate(Weight="OLS", Spec="Full sample")

# Size split（只报 Delta & BM_Raw）
df_sb <- df_final2_f %>%
  group_by(Date) %>%
  mutate(SizeGrp = ifelse(ME_Float_lag <= median(ME_Float_lag, na.rm=TRUE), "Small", "Big")) %>%
  ungroup()

run_fmb_by_group <- function(data, use_wls=TRUE){
  g <- data %>%
    group_by(Date) %>%
    do({
      fit <- if (use_wls) lm(Ret ~ BM_Raw + Delta_BM + lnME + MOM12, data=., weights=ME_Float_lag)
      else               lm(Ret ~ BM_Raw + Delta_BM + lnME + MOM12, data=.)
      data.frame(g_Delta = coef(fit)[["Delta_BM"]],
                 g_BMRaw = coef(fit)[["BM_Raw"]])
    }) %>% ungroup()

  data.frame(
    Delta_mean = nw_t_const(g$g_Delta, nw_lag)$mean,
    Delta_t    = nw_t_const(g$g_Delta, nw_lag)$t,
    BMRaw_mean = nw_t_const(g$g_BMRaw, nw_lag)$mean,
    BMRaw_t    = nw_t_const(g$g_BMRaw, nw_lag)$t
  )
}

out_size <- bind_rows(
  cbind(Weight="WLS", SizeGrp="Small", run_fmb_by_group(df_sb %>% filter(SizeGrp=="Small"), TRUE)),
  cbind(Weight="WLS", SizeGrp="Big",   run_fmb_by_group(df_sb %>% filter(SizeGrp=="Big"),   TRUE)),
  cbind(Weight="OLS", SizeGrp="Small", run_fmb_by_group(df_sb %>% filter(SizeGrp=="Small"), FALSE)),
  cbind(Weight="OLS", SizeGrp="Big",   run_fmb_by_group(df_sb %>% filter(SizeGrp=="Big"),   FALSE))
)

# Interaction：Small dummy + Delta_BM:Small + Small marginal
df_int <- df_final2_f %>%
  group_by(Date) %>%
  mutate(Small = as.integer(ME_Float_lag <= median(ME_Float_lag, na.rm=TRUE))) %>%
  ungroup()

gamma_int <- df_int %>%
  group_by(Date) %>%
  do({
    fit <- lm(Ret ~ BM_Raw + Delta_BM + Small + Delta_BM:Small + lnME + MOM12, data=.)
    b <- coef(fit)
    data.frame(
      b_Delta = b[["Delta_BM"]],
      b_DeltaSmall = b[["Delta_BM:Small"]],
      b_Delta_SmallMarginal = b[["Delta_BM"]] + b[["Delta_BM:Small"]]
    )
  }) %>% ungroup()

marg_table <- bind_rows(
  cbind(term="Delta_BM (Big baseline)",    nw_t_const(gamma_int$b_Delta, nw_lag)),
  cbind(term="Delta_BM:Small (increment)", nw_t_const(gamma_int$b_DeltaSmall, nw_lag)),
  cbind(term="Delta_BM | Small marginal",  nw_t_const(gamma_int$b_Delta_SmallMarginal, nw_lag))
)

marg_table_split <- marg_table %>%
  mutate(
    star = sapply(p, star),
    coef = sprintf("%.4f%s", mean, star),
    tval = sprintf("(%.2f)", t),
    pval = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  ) %>%
  select(term, coef, tval, pval, n)

safe_write_csv(fm_wls,           file.path(output_dir, "FM_WLS_FullSample.csv"))
safe_write_csv(fm_ols,           file.path(output_dir, "FM_OLS_FullSample.csv"))
safe_write_csv(out_size,         file.path(output_dir, "FM_SizeSplit_Delta_BMRaw.csv"))
safe_write_csv(marg_table_split, file.path(output_dir, "Table_Interaction_MarginalEffect_split.csv"))

# ==============================================================================
#  J) 迁移：Raw quintile -> Adj quintile（矩阵 + shift_rate + dQ）
# 【J. 分位迁移：Raw→Adj（机制展示/直观解释）】
# - Q_raw / Q_adj：分别基于 BM_Raw 与 BM_Adj 的月内五分位
# - dQ：调整后分位 - 原始分位（正值=向高BM移动；负值=向低BM移动）
# - 输出：
#   Migration_Matrix_Raw_to_Adj.csv：迁移矩阵（条件概率）
#   Migration_Shift_Summary.csv：迁移率、上移/下移比例
#   dQ_Distribution.csv：dQ 分布（可视化/附录）
# ==============================================================================
cat("\n[J] 输出 迁移矩阵 / shift_rate / dQ...\n")

df_move <- df_final2_f %>%
  group_by(Date) %>%
  mutate(Q_raw = ntile(BM_Raw, 5),
         Q_adj = ntile(BM_Adj, 5),
         dQ = Q_adj - Q_raw) %>%
  ungroup()

move_mat <- df_move %>%
  count(Q_raw, Q_adj) %>%
  group_by(Q_raw) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  select(Q_raw, Q_adj, pct) %>%
  pivot_wider(names_from = Q_adj, values_from = pct, values_fill = 0)

shift_summary <- df_move %>%
  summarise(
    shift_rate = mean(dQ != 0, na.rm=TRUE),
    up_rate    = mean(dQ >  0, na.rm=TRUE),
    down_rate  = mean(dQ <  0, na.rm=TRUE),
    same_rate  = mean(dQ == 0, na.rm=TRUE),
    n = n()
  )

dq_dist <- as.data.frame(table(df_move$dQ))
names(dq_dist) <- c("dQ","Freq")
dq_dist$share <- dq_dist$Freq / sum(dq_dist$Freq)

safe_write_csv(move_mat,      file.path(output_dir, "Migration_Matrix_Raw_to_Adj.csv"))
safe_write_csv(shift_summary, file.path(output_dir, "Migration_Shift_Summary.csv"))
safe_write_csv(dq_dist,       file.path(output_dir, "dQ_Distribution.csv"))

# ==============================================================================
#  K) 多切点稳健性：2012/2015/2018（Pre/Post | EW/VW） —— Date class 修复
# 【K. 结构性断点稳健性（Multiple cuts）】
# - 断点：2012/2015/2018（由你的研究设定：制度/会计准则/市场环境变化等）
# - Pre/Post：断点前后分别构造因子并重复主检验（均值t、spanning alpha、delta alpha）
# - 同时输出迁移统计与 dQ 分布：保证机制结论在不同阶段一致
# ==============================================================================
cat("\n[K] 输出 Robust cut（2012/2015/2018）...\n")

ym_label <- function(d){
  d <- as.Date(d, origin = "1970-01-01")
  format(d, "%Y-%m")
}

make_factor_by_period <- function(df, weight_type=c("EW","VW")) {
  weight_type <- match.arg(weight_type)
  p_adj <- make_portfolios(df, "BM_Adj")
  p_raw <- make_portfolios(df, "BM_Raw")
  ret_col  <- ifelse(weight_type=="EW", "Ret_EW", "Ret_VW")
  hml_adj <- make_hml(p_adj, ret_col) %>% rename(HML_Adj=HML)
  hml_raw <- make_hml(p_raw, ret_col) %>% rename(HML_Raw=HML)
  hml_adj %>% left_join(hml_raw, by="Date") %>%
    mutate(HML_Delta = HML_Adj - HML_Raw) %>%
    left_join(df_ff3, by="Date")
}

summarise_factor_cut <- function(fac, label, cut_label){
  out <- bind_rows(
    cbind(Factor="HML_Adj",   nw_mean_t(fac$HML_Adj,   nw_lag)),
    cbind(Factor="HML_Raw",   nw_mean_t(fac$HML_Raw,   nw_lag)),
    cbind(Factor="HML_Delta", nw_mean_t(fac$HML_Delta, nw_lag))
  )
  out$Sample <- label
  out$Cut <- cut_label
  out
}

run_spanning_alpha_only <- function(fac){
  fit <- lm(HML_Adj ~ HML_Raw, data=fac)
  ct  <- lmtest::coeftest(fit, vcov = sandwich::NeweyWest(fit, lag=nw_lag, prewhite=FALSE))
  data.frame(alpha = ct["(Intercept)",1], t = ct["(Intercept)",3], p = ct["(Intercept)",4])
}

run_delta_alpha_only <- function(fac){
  fit <- lm(HML_Delta ~ RiskPremium + SMB + HML_Std, data=fac)
  ct  <- lmtest::coeftest(fit, vcov = sandwich::NeweyWest(fit, lag=nw_lag, prewhite=FALSE))
  data.frame(alpha = ct["(Intercept)",1], t = ct["(Intercept)",3], p = ct["(Intercept)",4])
}

move_stats_period <- function(df){
  dfm <- df %>% group_by(Date) %>%
    mutate(Q_raw = ntile(BM_Raw, 5), Q_adj = ntile(BM_Adj, 5), dQ = Q_adj - Q_raw) %>%
    ungroup()
  data.frame(
    shift_rate = mean(dfm$dQ != 0, na.rm=TRUE),
    up_rate    = mean(dfm$dQ >  0, na.rm=TRUE),
    down_rate  = mean(dfm$dQ <  0, na.rm=TRUE),
    same_rate  = mean(dfm$dQ == 0, na.rm=TRUE)
  )
}

dq_dist_period <- function(df){
  dfm <- df %>% group_by(Date) %>%
    mutate(Q_raw = ntile(BM_Raw, 5), Q_adj = ntile(BM_Adj, 5), dQ = Q_adj - Q_raw) %>%
    ungroup()
  dd <- as.data.frame(table(dfm$dQ))
  names(dd) <- c("dQ","Freq")
  dd$share <- dd$Freq / sum(dd$Freq)
  dd
}

cut_dates <- as.Date(c("2012-01-01","2015-01-01","2018-01-01"))
all_factor <- list(); all_span <- list(); all_delta <- list(); all_move <- list(); all_dq <- list()

for (i in seq_along(cut_dates)) {
  cd <- cut_dates[i]
  cut_label <- paste0("cut=", ym_label(cd))

  df2 <- df_final2_f %>% mutate(Period = ifelse(Date < cd, "Pre", "Post"))

  for (pd in c("Pre","Post")) {
    subdf <- df2 %>% filter(Period == pd)

    fac_ew <- make_factor_by_period(subdf, "EW")
    fac_vw <- make_factor_by_period(subdf, "VW")

    all_factor[[paste(cut_label,pd,"EW")]] <- summarise_factor_cut(fac_ew, paste0(pd,"|EW"), cut_label)
    all_factor[[paste(cut_label,pd,"VW")]] <- summarise_factor_cut(fac_vw, paste0(pd,"|VW"), cut_label)

    all_span[[paste(cut_label,pd,"EW")]]  <- cbind(Cut=cut_label, Period=pd, Weight="EW", run_spanning_alpha_only(fac_ew))
    all_span[[paste(cut_label,pd,"VW")]]  <- cbind(Cut=cut_label, Period=pd, Weight="VW", run_spanning_alpha_only(fac_vw))

    all_delta[[paste(cut_label,pd,"EW")]] <- cbind(Cut=cut_label, Period=pd, Weight="EW", run_delta_alpha_only(fac_ew))
    all_delta[[paste(cut_label,pd,"VW")]] <- cbind(Cut=cut_label, Period=pd, Weight="VW", run_delta_alpha_only(fac_vw))

    mv <- move_stats_period(subdf)
    all_move[[paste(cut_label,pd)]] <- cbind(Cut=cut_label, Period=pd, mv)

    dd <- dq_dist_period(subdf)
    dd$Cut <- cut_label; dd$Period <- pd
    all_dq[[paste(cut_label,pd)]] <- dd
  }
}

factor_table_all <- bind_rows(all_factor)
span_table_all   <- bind_rows(all_span)
delta_table_all  <- bind_rows(all_delta)
move_table_all   <- bind_rows(all_move)
dq_table_all     <- bind_rows(all_dq)

stopifnot(nrow(factor_table_all) > 0, nrow(span_table_all) > 0, nrow(delta_table_all) > 0)

safe_write_csv(factor_table_all, file.path(output_dir, "Robust_Cut_FactorMean_NWt.csv"))
safe_write_csv(span_table_all,   file.path(output_dir, "Robust_Cut_SpanningAlpha.csv"))
safe_write_csv(delta_table_all,  file.path(output_dir, "Robust_Cut_DeltaAlpha_FF3.csv"))
safe_write_csv(move_table_all,   file.path(output_dir, "Robust_Cut_MigrationSummary.csv"))
safe_write_csv(dq_table_all,     file.path(output_dir, "Robust_Cut_dQ_Distribution.csv"))

# ==============================================================================
#  L) 高无形行业子样本（行业×年 K/ME 中位数 top 1/3）
# 【L. 高无形行业子样本（异质性/机制稳健）】
# - 行业×年：计算行业层面的 K_to_ME 中位数作为“无形强度”
# - 在每年内取 top 1/3 行业定义 HighIntanInd=1
# - 在高无形行业样本内重复因子均值与 alpha 检验（验证效应是否更强/更显著）
# ==============================================================================
cat("\n[L] 输出 高无形行业 Top 1/3 子样本...\n")

ind_intensity <- df_final2_f %>%
  mutate(Yr = year(Date)) %>%
  group_by(IndustryCode, Yr) %>%
  summarise(ind_K_to_ME = median(K_to_ME, na.rm=TRUE), .groups="drop")

df_hi <- df_final2_f %>%
  mutate(Yr = year(Date)) %>%
  left_join(ind_intensity, by=c("IndustryCode","Yr")) %>%
  group_by(Yr) %>%
  mutate(HighIntanInd = as.integer(ind_K_to_ME >= quantile(ind_K_to_ME, 2/3, na.rm=TRUE))) %>%
  ungroup()

df_hi_sample <- df_hi %>% filter(HighIntanInd==1)

fac_hi_vw <- make_factor_by_period(df_hi_sample, "VW")
fac_hi_ew <- make_factor_by_period(df_hi_sample, "EW")

hi_mean <- bind_rows(
  cbind(Weight="VW", Factor="HML_Adj",   nw_mean_t(fac_hi_vw$HML_Adj,   nw_lag)),
  cbind(Weight="VW", Factor="HML_Raw",   nw_mean_t(fac_hi_vw$HML_Raw,   nw_lag)),
  cbind(Weight="VW", Factor="HML_Delta", nw_mean_t(fac_hi_vw$HML_Delta, nw_lag)),
  cbind(Weight="EW", Factor="HML_Adj",   nw_mean_t(fac_hi_ew$HML_Adj,   nw_lag)),
  cbind(Weight="EW", Factor="HML_Raw",   nw_mean_t(fac_hi_ew$HML_Raw,   nw_lag)),
  cbind(Weight="EW", Factor="HML_Delta", nw_mean_t(fac_hi_ew$HML_Delta, nw_lag))
)

hi_delta_vw <- nw_coef_table(lm(HML_Delta ~ RiskPremium + SMB + HML_Std, data=fac_hi_vw), nw_lag)
hi_delta_ew <- nw_coef_table(lm(HML_Delta ~ RiskPremium + SMB + HML_Std, data=fac_hi_ew), nw_lag)

safe_write_csv(hi_mean,     file.path(output_dir, "HighIntan_FactorMean_NWt.csv"))
safe_write_csv(hi_delta_vw, file.path(output_dir, "HighIntan_DeltaAlpha_VW_NW.csv"))
safe_write_csv(hi_delta_ew, file.path(output_dir, "HighIntan_DeltaAlpha_EW_NW.csv"))

# ==============================================================================
#  M) 备份 & sessionInfo & 输出自检 & ZIP
# 【M. 复现与交付：结果备份、自检、打包】
# - workspace_results_paper.rds：保存关键中间结果，便于画额外图/补做稳健性
# - sessionInfo.txt：记录 R 版本与包版本（论文复现要求）
# - warnings.txt + run_log.txt：记录运行过程与潜在问题（方便排查）
# - 输出自检：确保论文所需表/图完整生成；缺文件直接 stop（避免“跑了但没产出”的隐形错误）
# - ZIP：本地交付与归档（GitHub 通常不上传 zip/大体量输出）
# ==============================================================================
cat("\n[M] 输出 RDS/sessionInfo & 输出自检 & ZIP...\n")

saveRDS(list(
  df_final2_f=df_final2_f,
  table1=table1,
  monthly_n=monthly_n,
  group_check=group_check,
  port_adj=port_adj, port_raw=port_raw,
  factor_vw=factor_vw, factor_ew=factor_ew, mean_table=mean_table,
  span_vw=span_vw, span_ew=span_ew,
  delta_vw=delta_vw, delta_ew=delta_ew,
  sum_size=sum_size,
  fm_wls=fm_wls, fm_ols=fm_ols, out_size=out_size,
  marg_table_split=marg_table_split,
  move_mat=move_mat, shift_summary=shift_summary, dq_dist=dq_dist,
  robust_factor=factor_table_all, robust_span=span_table_all, robust_delta=delta_table_all,
  robust_move=move_table_all, robust_dq=dq_table_all,
  df_hi_sample=df_hi_sample
), file=file.path(output_dir, "workspace_results_paper.rds"))

writeLines(capture.output(sessionInfo()), con=file.path(output_dir, "sessionInfo.txt"))

#  修复：自检前提前生成 warnings.txt（否则 on.exit 才写会导致自检缺文件）
w_txt <- capture.output(warnings())
if (length(w_txt) == 0) w_txt <- "No warnings."
writeLines(w_txt, con = file.path(output_dir, "warnings.txt"))

#  输出文件自检（缺任何一个直接停）
expected <- c(
  "RUN_PARAMS.csv",
  "Table1_DescStats.csv",
  "Monthly_n_stocks.csv",
  "Check_Group_Monotonicity.csv",
  "Portfolios_BMAdj.csv",
  "Portfolios_BMRaw.csv",
  "Factor_Series_Adj_Raw_Delta.csv",
  "Table_Factor_Mean_NWt.csv",
  "Spanning_VW_NW.csv",
  "Spanning_EW_NW.csv",
  "DeltaAlpha_VW_NW.csv",
  "DeltaAlpha_EW_NW.csv",
  "Table_DoubleSort_Size_BM.csv",
  "FM_WLS_FullSample.csv",
  "FM_OLS_FullSample.csv",
  "FM_SizeSplit_Delta_BMRaw.csv",
  "Table_Interaction_MarginalEffect_split.csv",
  "Migration_Matrix_Raw_to_Adj.csv",
  "Migration_Shift_Summary.csv",
  "dQ_Distribution.csv",
  "Robust_Cut_FactorMean_NWt.csv",
  "Robust_Cut_SpanningAlpha.csv",
  "Robust_Cut_DeltaAlpha_FF3.csv",
  "Robust_Cut_MigrationSummary.csv",
  "Robust_Cut_dQ_Distribution.csv",
  "HighIntan_FactorMean_NWt.csv",
  "HighIntan_DeltaAlpha_VW_NW.csv",
  "HighIntan_DeltaAlpha_EW_NW.csv",
  "Fig_Cum_Port_BMAdj_VW.png",
  "Fig_Cum_Port_BMAdj_EW.png",
  "Fig_Cum_Factors_VW.png",
  "Fig_Cum_Factors_EW.png",
  "workspace_results_paper.rds",
  "sessionInfo.txt",
  "warnings.txt",
  "run_log.txt"
)

missing_files <- expected[!file.exists(file.path(output_dir, expected))]
if (length(missing_files) > 0) {
  stop("warn 缺输出文件：\n", paste(missing_files, collapse="\n"))
} else {
  cat("[OK] Output self-check passed. All expected files exist.\n")
}

zip_file <- file.path(base_path, paste0("OUTPUT_PAPER_", run_id, ".zip"))
zip_ok <- tryCatch({
  utils::zip(zipfile = zip_file, files = list.files(output_dir, full.names = TRUE))
  TRUE
}, error=function(e){
  cat("[WARN] ZIP 打包失败：", e$message, "\n"); FALSE
})

cat("\nZIP created? ", zip_ok, " -> ", zip_file, "\n")
cat("\n=== RUN END ===\n")

# ==============================================================================
# ==============================================================================
