# ==============================================================================
# 论文实证
# ==============================================================================

rm(list = ls()); gc()
options(stringsAsFactors = FALSE)
options(warn = 1)

# ---------- 你只需要改这里 ----------
base_path <- ""   # <-- 默认沿用你上次跑通的根目录；如有变化再改这里
min_stocks_per_month <- 200
winsor_prob <- 0.01
nw_mode <- "auto"                     # "auto"=自动带宽；"fixed"=固定带宽
nw_lag <- 3                           # 固定带宽，或自动带宽失败时的回退值
nw_lag_cap <- 12                      # 自动带宽的上限，避免过大
bm_denom_mode_main <- "lag_me"        # 主结果仍用滞后一期市值；补充稳健性会自动跑 june_me
use_st_filter <- FALSE                # 是否启用 ST 剔除；默认 FALSE，不重下全库也能直接跑
st_daily_file <- NA_character_        # 可直接写完整路径；不做 ST 时保持 NA 即可
st_drop_rule <- "any_st_day_in_month" # 若启用 ST：当月任一日 ST=Y 即剔除该股票-月份

# 输入文件夹 & 文件名关键词（建议按自己文件名调整关键词）
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
  if (!dir.exists(path)) stop("[ERROR] 找不到目录：", name, " -> ", path)
}

safe_write_csv <- function(df, path){
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

winsorize <- function(x, prob = 0.01, min_n = 200) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  if (sum(ok) < min_n) return(x)
  q <- quantile(x[ok], c(prob, 1 - prob), na.rm = TRUE, type = 7)
  pmin(pmax(x, q[1]), q[2])
}

resolve_nw_lag <- function(fit, lag_nw = nw_lag, mode = nw_mode, lag_cap = nw_lag_cap) {
  n_obs <- suppressWarnings(as.integer(stats::nobs(fit)))
  if (!is.finite(n_obs) || n_obs <= 1) return(0L)

  lag_fixed <- suppressWarnings(as.integer(floor(lag_nw)))
  if (!is.finite(lag_fixed) || lag_fixed < 0) lag_fixed <- 0L

  lag_cap <- suppressWarnings(as.integer(floor(lag_cap)))
  if (!is.finite(lag_cap) || lag_cap < 0) lag_cap <- lag_fixed
  lag_cap <- max(0L, min(lag_cap, n_obs - 1L))

  if (!identical(mode, "auto")) {
    return(max(0L, min(lag_fixed, lag_cap)))
  }

  bw <- tryCatch(
    sandwich::bwNeweyWest(fit, prewhite = FALSE, kernel = "Bartlett"),
    error = function(e) NA_real_
  )
  lag_auto <- suppressWarnings(as.integer(floor(bw)))
  if (!is.finite(lag_auto) || lag_auto < 0) lag_auto <- lag_fixed

  max(0L, min(lag_auto, lag_cap))
}

nw_vcov <- function(fit, lag_nw = nw_lag, mode = nw_mode, lag_cap = nw_lag_cap) {
  lag_use <- resolve_nw_lag(fit, lag_nw = lag_nw, mode = mode, lag_cap = lag_cap)
  list(
    vcov = sandwich::NeweyWest(fit, lag = lag_use, prewhite = FALSE),
    lag_used = lag_use
  )
}

nw_mean_t <- function(x, lag_nw = nw_lag, mode = nw_mode, lag_cap = nw_lag_cap) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 5) {
    return(data.frame(mean = ifelse(length(x) == 0, NA_real_, mean(x)),
                      t = NA_real_, p = NA_real_, n = length(x), lag_used = NA_integer_))
  }
  fit <- lm(x ~ 1)
  nw  <- nw_vcov(fit, lag_nw = lag_nw, mode = mode, lag_cap = lag_cap)
  ct  <- lmtest::coeftest(fit, vcov = nw$vcov)
  data.frame(mean = unname(coef(fit)[1]),
             t    = unname(ct[1,3]),
             p    = unname(ct[1,4]),
             n    = length(x),
             lag_used = nw$lag_used)
}

nw_coef_table <- function(fit, lag_nw = nw_lag, mode = nw_mode, lag_cap = nw_lag_cap) {
  nw <- nw_vcov(fit, lag_nw = lag_nw, mode = mode, lag_cap = lag_cap)
  ct <- lmtest::coeftest(fit, vcov = nw$vcov)
  m  <- as.matrix(ct)
  data.frame(term = rownames(m),
             estimate = m[,1], se = m[,2], t = m[,3], p = m[,4],
             lag_used = nw$lag_used,
             row.names = NULL, check.names = FALSE)
}

pick_first_existing <- function(nms, candidates){
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

find_optional_file <- function(regex_pattern,
                               search_root = base_path,
                               exts = c("xlsx", "xls", "csv")) {
  files <- unlist(lapply(exts, function(ext) {
    list.files(search_root,
               recursive = TRUE,
               full.names = TRUE,
               pattern = paste0("[.]", ext, "$"),
               ignore.case = TRUE)
  }))
  if (length(files) == 0) return(NA_character_)
  hit <- files[stringr::str_detect(tolower(basename(files)), tolower(regex_pattern))]
  if (length(hit) == 0) return(NA_character_)
  hit <- hit[order(file.info(hit)$mtime, decreasing = TRUE)]
  hit[1]
}

# 安全的月份日期解析：支持 YYYY-MM-DD / YYYY-MM / YYYYMM / YYYY.MM
# 不会因为 charToDate 报错而中断脚本
to_month_date <- function(x){
  x <- as.character(x)
  x <- str_trim(x)
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA_character_

  x2 <- str_replace_all(x, "\\.", "-")
  x2 <- ifelse(str_detect(x2, "^\\d{6}$"),
               paste0(substr(x2, 1, 4), "-", substr(x2, 5, 6), "-01"),
               x2)
  x2 <- ifelse(str_detect(x2, "^\\d{4}-\\d{2}$"),
               paste0(x2, "-01"),
               x2)

  out <- rep(as.Date(NA), length(x2))

  idx_std <- !is.na(x2) & str_detect(x2, "^\\d{4}-\\d{2}-\\d{2}$")
  if (any(idx_std)) {
    out[idx_std] <- as.Date(x2[idx_std], format = "%Y-%m-%d")
  }

  idx_remain <- is.na(out) & !is.na(x2)
  if (any(idx_remain)) {
    tmp <- suppressWarnings(lubridate::ymd(x2[idx_remain]))
    out[idx_remain] <- as.Date(tmp)
  }

  out
}

scale_factor_if_needed <- function(x){
  x <- as.numeric(x)
  ok <- is.finite(x)
  if (!any(ok)) return(x)
  med_abs <- median(abs(x[ok]), na.rm = TRUE)
  if (is.finite(med_abs) && med_abs > 2) {
    cat("[INFO] scale_factor_if_needed: median |x| =", round(med_abs, 3), " -> dividing by 100\n")
    x <- x / 100
  }
  x
}

alpha_row <- function(tbl){
  out <- tbl %>% dplyr::filter(term == "(Intercept)") %>% dplyr::select(alpha = estimate, t, p)
  if (nrow(out) == 0) out <- data.frame(alpha = NA_real_, t = NA_real_, p = NA_real_)
  out
}

read_optional_ff5 <- function(){
  all_files <- list.files(
    base_path,
    recursive = TRUE,
    full.names = TRUE,
    pattern = "[.](xlsx|xls|csv)$",
    ignore.case = TRUE
  )

  hit <- all_files[stringr::str_detect(
    tolower(basename(all_files)),
    "fivefac|five-factor|ff5|rmw|cma|fivefacmonth"
  )]
  if (length(hit) == 0) return(NULL)

  hit <- hit[order(file.info(hit)$mtime, decreasing = TRUE)]
  pick <- hit[1]
  cat("[FF5] optional file detected:", pick, "\n")

  raw <- tryCatch({
    if (stringr::str_detect(tolower(pick), "[.]csv$")) {
      read.csv(pick, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      as.data.frame(readxl::read_excel(pick, col_types = "text"), stringsAsFactors = FALSE)
    }
  }, error = function(e){
    cat("[FF5] read failed:", e$message, "\n")
    return(NULL)
  })

  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  raw[] <- lapply(raw, function(x) stringr::str_trim(as.character(x)))
  nms <- names(raw)

  date_col <- pick_first_existing(nms, c("TradingMonth","Date","Month","Trdmnt","年月","月份"))
  mkt_col  <- pick_first_existing(nms, c("RiskPremium1","RiskPremium","MKT_RF","MKT.RF","MktRF","Rmrf","RMRF","MKT"))
  smb_col  <- pick_first_existing(nms, c("SMB1","SMB"))
  hml_col  <- pick_first_existing(nms, c("HML1","HML","HML_Std"))
  rmw_col  <- pick_first_existing(nms, c("RMW1","RMW","Rmw"))
  cma_col  <- pick_first_existing(nms, c("CMA1","CMA","Cma"))

  if (any(is.na(c(date_col, mkt_col, smb_col, hml_col, rmw_col, cma_col)))) {
    cat("[FF5] required columns not found; skip optional FF5 block.\n")
    return(NULL)
  }

  raw <- raw %>%
    dplyr::filter(!is.na(.data[[date_col]]), .data[[date_col]] != "") %>%
    dplyr::mutate(Date_tmp = to_month_date(.data[[date_col]])) %>%
    dplyr::filter(!is.na(Date_tmp))

  cat("[FF5] rows after valid-date filter =", nrow(raw), "\n")

  if ("MarkettypeID" %in% names(raw)) {
    keep_mkt <- raw$MarkettypeID %in% c("P9709", "9709")
    cat("[FF5] rows matching MarkettypeID P9709 =", sum(keep_mkt, na.rm = TRUE), "\n")
    if (sum(keep_mkt, na.rm = TRUE) > 0) raw <- raw[keep_mkt, , drop = FALSE]
  }

  if ("Portfolios" %in% names(raw)) {
    port_num <- suppressWarnings(as.numeric(raw$Portfolios))
    keep_port <- raw$Portfolios %in% c("1", "1.0", "P1") | (!is.na(port_num) & port_num == 1)
    cat("[FF5] rows matching Portfolios 1 =", sum(keep_port, na.rm = TRUE), "\n")
    if (sum(keep_port, na.rm = TRUE) > 0) raw <- raw[keep_port, , drop = FALSE]
  }

  out <- raw %>%
    dplyr::transmute(
      Date = Date_tmp,
      RiskPremium = scale_factor_if_needed(.data[[mkt_col]]),
      SMB = scale_factor_if_needed(.data[[smb_col]]),
      HML_Std = scale_factor_if_needed(.data[[hml_col]]),
      RMW = scale_factor_if_needed(.data[[rmw_col]]),
      CMA = scale_factor_if_needed(.data[[cma_col]])
    ) %>%
    dplyr::filter(!is.na(Date)) %>%
    dplyr::distinct(Date, .keep_all = TRUE) %>%
    dplyr::arrange(Date)

  cat("[FF5] final monthly rows =", nrow(out), "\n")
  attr(out, "source_file") <- pick
  out
}

read_optional_st_daily <- function(explicit_path = st_daily_file) {
  candidate <- explicit_path
  if (is.na(candidate) || !nzchar(candidate) || !file.exists(candidate)) {
    candidate <- find_optional_file("liq_suspension|个股停牌|停牌标识|suspension")
  }
  if (is.na(candidate) || !file.exists(candidate)) {
    cat("[ST] optional daily ST file not found; skip ST filter.
")
    return(NULL)
  }

  cat("[ST] optional daily ST file detected:", candidate, "
")

  raw <- tryCatch({
    if (stringr::str_detect(tolower(candidate), "[.]csv$")) {
      as.data.frame(read.csv(candidate, stringsAsFactors = FALSE, check.names = FALSE),
                    stringsAsFactors = FALSE)
    } else {
      as.data.frame(readxl::read_excel(candidate, col_names = FALSE, col_types = "text"),
                    stringsAsFactors = FALSE)
    }
  }, error = function(e) {
    cat("[ST] read failed:", e$message, "
")
    return(NULL)
  })

  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  raw[] <- lapply(raw, function(x) stringr::str_trim(as.character(x)))

  header_idx <- which(apply(raw, 1, function(r) {
    vals <- tolower(as.character(r))
    any(vals %in% c("stkcd", "证券代码", "symbol")) &&
      any(vals %in% c("suspdate", "停牌日期", "date", "trddt"))
  }))
  header_idx <- if (length(header_idx) == 0) 1 else header_idx[1]

  header <- as.character(unlist(raw[header_idx, ], use.names = FALSE))
  dat <- raw[(header_idx + 1):nrow(raw), , drop = FALSE]
  names(dat) <- header
  dat <- dat[, !is.na(names(dat)) & names(dat) != "", drop = FALSE]

  stk_col  <- pick_first_existing(names(dat), c("Stkcd", "证券代码", "Symbol"))
  date_col <- pick_first_existing(names(dat), c("Suspdate", "停牌日期", "Date", "Trddt"))
  st_col   <- pick_first_existing(names(dat), c("ST", "ST标识", "Stflag", "STFlag"))
  mkt_col  <- pick_first_existing(names(dat), c("MarketType", "市场类型", "Markettype", "MktType"))

  if (any(is.na(c(stk_col, date_col, st_col)))) {
    cat("[ST] required columns not found; skip ST filter.
")
    return(NULL)
  }

  dat <- dat %>%
    filter(!is.na(.data[[stk_col]]), .data[[stk_col]] != "") %>%
    filter(!is.na(.data[[date_col]]), .data[[date_col]] != "") %>%
    filter(!tolower(.data[[stk_col]]) %in% c("没有单位", "na", "null")) %>%
    mutate(
      Stkcd = sprintf("%06d", as.numeric(.data[[stk_col]])),
      Suspdate = suppressWarnings(lubridate::ymd(.data[[date_col]])),
      ST_raw = toupper(stringr::str_trim(as.character(.data[[st_col]]))),
      MarketType_daily = if (!is.na(mkt_col)) as.character(.data[[mkt_col]]) else NA_character_
    ) %>%
    filter(!is.na(Stkcd), !is.na(Suspdate)) %>%
    mutate(
      ST_flag = ST_raw %in% c("Y", "1", "YES", "ST", "*ST", "S", "TRUE"),
      Date = as.Date(lubridate::floor_date(Suspdate, unit = "month"))
    )

  if (nrow(dat) == 0) return(NULL)

  out <- dat %>%
    group_by(Stkcd, Date) %>%
    summarise(
      ST_any_month = as.integer(any(ST_flag, na.rm = TRUE)),
      ST_days = sum(ST_flag, na.rm = TRUE),
      ST_source_obs_days = n(),
      .groups = "drop"
    ) %>%
    arrange(Stkcd, Date)

  attr(out, "source_file") <- candidate
  out
}

apply_st_filter_to_panel <- function(panel_df,
                                     df_st_monthly_input = NULL,
                                     use_st_filter_input = use_st_filter) {
  if (!isTRUE(use_st_filter_input) || is.null(df_st_monthly_input) || nrow(df_st_monthly_input) == 0) {
    panel_df$ST_any_month <- 0L
    panel_df$ST_days <- 0L
    panel_df$ST_source_obs_days <- 0L
    return(panel_df)
  }

  panel_df %>%
    left_join(df_st_monthly_input, by = c("Stkcd", "Date")) %>%
    mutate(
      ST_any_month = ifelse(is.na(ST_any_month), 0L, ST_any_month),
      ST_days = ifelse(is.na(ST_days), 0L, ST_days),
      ST_source_obs_days = ifelse(is.na(ST_source_obs_days), 0L, ST_source_obs_days)
    ) %>%
    filter(ST_any_month == 0L)
}

read_clean_raw <- function(folder_name, must_pattern = NULL) {
  full_folder_path <- file.path(base_path, folder_name)
  assert_dir(full_folder_path, folder_name)

  files <- list.files(full_folder_path, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)
  if (length(files) == 0) stop(paste("[ERROR] 没找到文件夹中的 Excel：", folder_name))

  if (!is.null(must_pattern)) {
    hit <- files[str_detect(tolower(basename(files)), tolower(must_pattern))]
    if (length(hit) == 0) stop("[ERROR] 没找到匹配文件：pattern=", must_pattern, " | folder=", folder_name)
    files <- hit
  }

  files <- files[order(file.info(files)$mtime, decreasing = TRUE)]
  pick <- files[1]

  cat("[READ] folder=", folder_name, " | file=", basename(pick), "\n")
  suppressMessages(readxl::read_xlsx(pick, col_types = "text", na = c("NULL","NA","")))
}

make_portfolios <- function(df, sort_var) {
  df %>%
    group_by(Date) %>%
    mutate(Group = dplyr::ntile(as.numeric(.data[[sort_var]]), 5L)) %>%
    group_by(Date, Group) %>%
    summarise(
      Ret_VW = weighted.mean(Ret, w = ME_Float_lag, na.rm = TRUE),
      Ret_EW = mean(Ret, na.rm = TRUE),
      RiskPremium = dplyr::first(RiskPremium),
      SMB = dplyr::first(SMB),
      HML_Std = dplyr::first(HML_Std),
      .groups = "drop"
    )
}

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
# A) 基础信息：上市日期 + 行业
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

ind_cov <- df_basic %>% count(Year) %>% arrange(Year)
year_ind_full <- ind_cov %>%
  filter(n >= 0.9 * max(n)) %>%
  summarise(year = max(Year)) %>%
  pull(year)

cat("industry_year_used =", year_ind_full, "\n")
stopifnot(year_ind_full >= 2018)

# ============================================================================== 
# B) 月度收益 & 市值 + MOM12
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
  filter(Markettype %in% c(1,4,16,32)) %>%
  select(Stkcd, Date, Year, Month, Ret_raw, ME, ME_Float)

med_abs_ret <- median(abs(df_ret_raw$Ret_raw), na.rm = TRUE)
cat("[DIAG] Ret_raw median |value| =", round(med_abs_ret, 4), "\n")

df_ret <- df_ret_raw %>%
  arrange(Stkcd, Date) %>%
  mutate(Ret = Ret_raw)

if (is.finite(med_abs_ret) && med_abs_ret > 1) {
  cat("[INFO] 判断为百分数口径 -> 全列 /100\n")
  df_ret <- df_ret %>% mutate(Ret = Ret / 100)
} else {
  cat("[INFO] 判断为小数口径 -> 保持原值\n")
}

df_ret <- df_ret %>%
  group_by(Stkcd) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    ME_lag       = lag(ME, 1),
    ME_Float_lag = lag(ME_Float, 1),
    zret = 1 + Ret,
    MOM12 = zoo::rollapplyr(lag(zret, 1), 12, prod, fill = NA, partial = FALSE) - 1
  ) %>%
  ungroup() %>%
  select(Stkcd, Date, Year, Month, Ret, ME, ME_Float, ME_lag, ME_Float_lag, MOM12)

brief(df_ret, "df_ret")

df_june_me_ref <- df_ret %>%
  filter(month(Date) == 6, is.finite(ME), ME > 0) %>%
  transmute(Stkcd, BM_Ref_Year = year(Date), ME_June = ME) %>%
  distinct(Stkcd, BM_Ref_Year, .keep_all = TRUE)

attach_bm_reference <- function(df_input,
                                bm_denom_mode_input = bm_denom_mode_main,
                                df_june_me_input = df_june_me_ref) {
  bm_denom_mode_input <- match.arg(bm_denom_mode_input, c("lag_me", "june_me"))

  out <- df_input %>%
    mutate(BM_Ref_Year = ifelse(Month >= 7, Year, Year - 1L)) %>%
    left_join(df_june_me_input, by = c("Stkcd", "BM_Ref_Year"))

  if (bm_denom_mode_input == "june_me") {
    out <- out %>% mutate(BM_ME_ref = ME_June)
  } else {
    out <- out %>% mutate(BM_ME_ref = ME_lag)
  }

  out %>%
    mutate(BM_ME_ref_source = bm_denom_mode_input)
}

# ============================================================================== 
# C) 年度：RD、BE、SGA + PIM -> BE_Adj
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
    RD_Exp_raw = as.numeric(RDSpendSum)
  ) %>%
  filter(month(Accper) == 12) %>%
  distinct(Stkcd, Year, .keep_all = TRUE) %>%
  transmute(
    Stkcd, Year,
    RD_Exp_raw,
    RD_Exp = ifelse(is.na(RD_Exp_raw), 0, RD_Exp_raw)
  )

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
    Sales_Exp_raw = as.numeric(B001209000),
    Admin_Exp_raw = as.numeric(B001210000)
  ) %>%
  filter(month(Accper) == 12) %>%
  distinct(Stkcd, Year, .keep_all = TRUE) %>%
  mutate(
    Sales_Exp = ifelse(is.na(Sales_Exp_raw), 0, Sales_Exp_raw),
    Admin_Exp = ifelse(is.na(Admin_Exp_raw), 0, Admin_Exp_raw),
    SGA = Sales_Exp + Admin_Exp
  ) %>%
  transmute(Stkcd, Year, Sales_Exp_raw, Admin_Exp_raw, SGA)

calc_pim <- function(inv, delta) {
  inv <- as.numeric(inv)
  inv[!is.finite(inv)] <- 0
  n <- length(inv)
  if (n == 0) return(numeric(0))
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
# C2) 可选：读取日频 ST 数据并转成月度剔除标记
# ============================================================================== 
cat("\n[C2] 读取可选 ST（日频）并转月频...\n")

df_st_monthly <- NULL
if (isTRUE(use_st_filter)) {
  df_st_monthly <- tryCatch(
    read_optional_st_daily(st_daily_file),
    error = function(e) {
      cat("[ST] fatal error:", e$message, "\n")
      NULL
    }
  )
} else {
  cat("[ST] use_st_filter = FALSE -> skip reading ST file.\n")
}

if (!is.null(df_st_monthly) && nrow(df_st_monthly) > 0) {
  st_monthly_summary <- df_st_monthly %>%
    group_by(Date) %>%
    summarise(
      st_flagged_stockmonths = sum(ST_any_month, na.rm = TRUE),
      st_flagged_stocks = n_distinct(Stkcd[ST_any_month == 1L]),
      st_flagged_days = sum(ST_days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Date)
  safe_write_csv(st_monthly_summary, file.path(output_dir, "ST_Monthly_Flag_Summary.csv"))
  cat("[ST] source =", attr(df_st_monthly, "source_file"), " | stock-month rows =", nrow(df_st_monthly), "\n")
} else {
  st_monthly_summary <- NULL
  cat("[ST] no usable ST monthly flags generated.\n")
}
# ============================================================================== 
# D) FF3 月度因子
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
# E) 构造最终月度面板 df_final2_f
# ============================================================================== 
cat("\n[E] 构造最终月度面板 df_final2_f...\n")

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
panel4 <- panel3 %>% filter(!is.na(AgeMonths), AgeMonths >= 12)
panel5_pre_st <- panel4 %>%
  mutate(IndYear = pmin(Year, year_ind_full)) %>%
  left_join(df_basic %>% select(Stkcd, Year, IndustryCode) %>% rename(IndYear = Year),
            by = c("Stkcd","IndYear")) %>%
  filter(!is.na(IndustryCode))

panel5 <- apply_st_filter_to_panel(panel5_pre_st, df_st_monthly, use_st_filter)
panel5_bm <- attach_bm_reference(panel5, bm_denom_mode_main, df_june_me_ref)

if (isTRUE(use_st_filter) && !is.null(df_st_monthly) && nrow(df_st_monthly) > 0) {
  st_filter_impact <- panel5_pre_st %>%
    group_by(Date) %>%
    summarise(n_before_st = n_distinct(Stkcd), .groups = "drop") %>%
    full_join(
      panel5 %>% group_by(Date) %>% summarise(n_after_st = n_distinct(Stkcd), .groups = "drop"),
      by = "Date"
    ) %>%
    full_join(st_monthly_summary, by = "Date") %>%
    mutate(
      n_before_st = ifelse(is.na(n_before_st), 0L, n_before_st),
      n_after_st = ifelse(is.na(n_after_st), 0L, n_after_st),
      n_removed_st = n_before_st - n_after_st
    ) %>%
    arrange(Date)
  safe_write_csv(st_filter_impact, file.path(output_dir, "ST_Filter_Impact_byMonth.csv"))
}

panel6 <- panel5_bm %>%
  filter(
    is.finite(Ret),
    is.finite(ME_lag), is.finite(ME_Float_lag),
    ME_lag > 0, ME_Float_lag > 0,
    is.finite(BM_ME_ref), BM_ME_ref > 0,
    is.finite(BE_Raw), is.finite(BE_Adj),
    BE_Raw > 0, BE_Adj > 0
  ) %>%
  mutate(
    BM_Raw   = BE_Raw / (BM_ME_ref * 1000),
    BM_Adj   = BE_Adj / (BM_ME_ref * 1000),
    K_to_ME  = K_Total / (BM_ME_ref * 1000),
    Delta_BM = BM_Adj - BM_Raw,
    lnME     = log(ME_Float_lag)
  )

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

df_final2_f <- df_final2 %>%
  group_by(Date) %>%
  filter(n_distinct(Stkcd) >= min_stocks_per_month) %>%
  ungroup()

brief(df_final2_f, "df_final2_f")
n_dates  <- n_distinct(df_final2_f$Date)
n_stocks <- n_distinct(df_final2_f$Stkcd)
cat("final panel: months =", n_dates, " stocks =", n_stocks, "\n")
if (n_dates  < 200) stop("[ERROR] 最终面板月份数过少 (", n_dates,  " < 200)，请检查数据覆盖范围")
if (n_stocks < 500) stop("[ERROR] 最终面板股票数过少 (", n_stocks, " < 500)，请检查数据筛选条件")

params <- data.frame(
  run_id = run_id,
  base_path = base_path,
  min_stocks_per_month = min_stocks_per_month,
  winsor_prob = winsor_prob,
  nw_mode = nw_mode,
  nw_lag = nw_lag,
  nw_lag_cap = nw_lag_cap,
  bm_denom_mode_main = bm_denom_mode_main,
  use_st_filter = use_st_filter,
  st_drop_rule = st_drop_rule,
  st_source_file = ifelse(!is.null(df_st_monthly) && nrow(df_st_monthly) > 0,
                          attr(df_st_monthly, "source_file"),
                          NA_character_),
  industry_year_used = year_ind_full,
  bm_scale_divisor = 1000
)
safe_write_csv(params, file.path(output_dir, "RUN_PARAMS.csv"))

# ============================================================================== 
# F) Table1 + 月度样本数 + 分组单调性
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
  mutate(Group = dplyr::ntile(as.numeric(BM_Adj), 5L)) %>%
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
# G) 五分组组合 + 因子 Adj/Raw/Delta + Mean/NW + Spanning + Delta~FF3 + 图
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
# H) 双排序 Size×BM：Small/Big 内 HML（Adj/Raw/Delta）
# ============================================================================== 
cat("\n[H] 输出 Size×BM 双排序...\n")

make_hml_by_size <- function(df, bm_var, weight = c("EW","VW")) {
  weight <- match.arg(weight)
  size_cut <- df %>%
    group_by(Date) %>%
    summarise(SizeMed = stats::median(as.numeric(ME_Float_lag), na.rm=TRUE), .groups="drop")

  df %>%
    left_join(size_cut, by = "Date") %>%
    mutate(
      SizeGrp = ifelse(as.numeric(ME_Float_lag) <= SizeMed, "Small", "Big")
    ) %>%
    group_by(Date, SizeGrp) %>%
    mutate(BMGrp = dplyr::ntile(as.numeric(.data[[bm_var]]), 5L)) %>%
    ungroup() %>%
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
  summarise({
    res_adj   <- nw_mean_t(HML_Adj,   nw_lag)
    res_raw   <- nw_mean_t(HML_Raw,   nw_lag)
    res_delta <- nw_mean_t(HML_Delta, nw_lag)
    data.frame(
      Adj_mean   = res_adj$mean,   Adj_t   = res_adj$t,
      Raw_mean   = res_raw$mean,   Raw_t   = res_raw$t,
      Delta_mean = res_delta$mean, Delta_t = res_delta$t
    )
  }, .groups="drop")
safe_write_csv(sum_size, file.path(output_dir, "Table_DoubleSort_Size_BM.csv"))

# ============================================================================== 
# I) Fama-MacBeth：WLS/OLS + 分大小盘 + 交互项边际效应
# ============================================================================== 
cat("\n[I] 输出 Fama-MacBeth（WLS/OLS/Size/Interaction）...\n")

nw_t_const <- function(x, lag_nw = nw_lag, mode = nw_mode){
  nw_mean_t(x, lag_nw = lag_nw, mode = mode)
}

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

df_sb_cut <- df_final2_f %>%
  group_by(Date) %>%
  summarise(SizeMed = stats::median(as.numeric(ME_Float_lag), na.rm=TRUE), .groups = "drop")

df_sb <- df_final2_f %>%
  left_join(df_sb_cut, by = "Date") %>%
  mutate(SizeGrp = ifelse(as.numeric(ME_Float_lag) <= SizeMed, "Small", "Big")) %>%
  select(-SizeMed)

run_fmb_by_group <- function(data, use_wls=TRUE){
  g <- data %>%
    group_by(Date) %>%
    do({
      fit <- if (use_wls) lm(Ret ~ BM_Raw + Delta_BM + lnME + MOM12, data=., weights=ME_Float_lag)
      else               lm(Ret ~ BM_Raw + Delta_BM + lnME + MOM12, data=.)
      data.frame(g_Delta = coef(fit)[["Delta_BM"]],
                 g_BMRaw = coef(fit)[["BM_Raw"]])
    }) %>% ungroup()

  res_delta <- nw_t_const(g$g_Delta, nw_lag)
  res_bmraw <- nw_t_const(g$g_BMRaw, nw_lag)
  data.frame(
    Delta_mean = res_delta$mean,
    Delta_t    = res_delta$t,
    BMRaw_mean = res_bmraw$mean,
    BMRaw_t    = res_bmraw$t
  )
}

out_size <- bind_rows(
  cbind(Weight="WLS", SizeGrp="Small", run_fmb_by_group(df_sb %>% filter(SizeGrp=="Small"), TRUE)),
  cbind(Weight="WLS", SizeGrp="Big",   run_fmb_by_group(df_sb %>% filter(SizeGrp=="Big"),   TRUE)),
  cbind(Weight="OLS", SizeGrp="Small", run_fmb_by_group(df_sb %>% filter(SizeGrp=="Small"), FALSE)),
  cbind(Weight="OLS", SizeGrp="Big",   run_fmb_by_group(df_sb %>% filter(SizeGrp=="Big"),   FALSE))
)

star <- function(p){
  if (is.na(p)) "" else if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
}

size_cut <- df_final2_f %>%
  group_by(Date) %>%
  summarise(SizeMed = stats::median(as.numeric(ME_Float_lag), na.rm = TRUE), .groups = "drop")

df_int <- df_final2_f %>%
  left_join(size_cut, by = "Date") %>%
  mutate(Small = as.integer(as.numeric(ME_Float_lag) <= SizeMed)) %>%
  select(-SizeMed)

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
# J) 迁移：Raw quintile -> Adj quintile（矩阵 + shift_rate + dQ）
# ============================================================================== 
cat("\n[J] 输出 迁移矩阵 / shift_rate / dQ...\n")

df_move <- df_final2_f %>%
  group_by(Date) %>%
  mutate(
    Q_raw = dplyr::ntile(as.numeric(BM_Raw), 5L),
    Q_adj = dplyr::ntile(as.numeric(BM_Adj), 5L),
    dQ = Q_adj - Q_raw
  ) %>%
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
# K) 多切点稳健性：2012/2015/2018（Pre/Post | EW/VW）
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
  tbl <- nw_coef_table(fit, lag_nw = nw_lag, mode = nw_mode)
  data.frame(alpha = tbl$estimate[tbl$term == "(Intercept)"][1],
             t = tbl$t[tbl$term == "(Intercept)"][1],
             p = tbl$p[tbl$term == "(Intercept)"][1],
             lag_used = tbl$lag_used[tbl$term == "(Intercept)"][1])
}

run_delta_alpha_only <- function(fac){
  fit <- lm(HML_Delta ~ RiskPremium + SMB + HML_Std, data=fac)
  tbl <- nw_coef_table(fit, lag_nw = nw_lag, mode = nw_mode)
  data.frame(alpha = tbl$estimate[tbl$term == "(Intercept)"][1],
             t = tbl$t[tbl$term == "(Intercept)"][1],
             p = tbl$p[tbl$term == "(Intercept)"][1],
             lag_used = tbl$lag_used[tbl$term == "(Intercept)"][1])
}

move_stats_period <- function(df){
  dfm <- df %>% group_by(Date) %>%
    mutate(
      Q_raw = dplyr::ntile(as.numeric(BM_Raw), 5L),
      Q_adj = dplyr::ntile(as.numeric(BM_Adj), 5L),
      dQ = Q_adj - Q_raw
    ) %>%
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
    mutate(
      Q_raw = dplyr::ntile(as.numeric(BM_Raw), 5L),
      Q_adj = dplyr::ntile(as.numeric(BM_Adj), 5L),
      dQ = Q_adj - Q_raw
    ) %>%
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
# L) 高无形行业子样本（行业×年 K/ME 中位数 top 1/3）
# ============================================================================== 
cat("\n[L] 输出 高无形行业 Top 1/3 子样本...\n")

ind_intensity <- df_final2_f %>%
  mutate(Yr = year(Date)) %>%
  group_by(IndustryCode, Yr) %>%
  summarise(ind_K_to_ME = median(K_to_ME, na.rm=TRUE), .groups="drop")

ind_hi_flag <- ind_intensity %>%
  group_by(Yr) %>%
  mutate(HighIntanInd = as.integer(ind_K_to_ME >= quantile(ind_K_to_ME, 2/3, na.rm=TRUE))) %>%
  ungroup()

df_hi <- df_final2_f %>%
  mutate(Yr = year(Date)) %>%
  left_join(ind_hi_flag, by=c("IndustryCode","Yr"))

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
# M) 备份 & sessionInfo & 输出自检 & ZIP
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

w_txt <- capture.output(warnings())
if (length(w_txt) == 0) w_txt <- "No warnings."
writeLines(w_txt, con = file.path(output_dir, "warnings.txt"))

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
  stop("[ERROR] 缺输出文件：\n", paste(missing_files, collapse="\n"))
} else {
  cat("[OK] Output self-check passed. All expected files exist.\n")
}

zip_file <- file.path(base_path, paste0("OUTPUT_PAPER_", run_id, ".zip"))
zip_ok <- tryCatch({
  old_wd <- getwd()
  setwd(output_dir)
  utils::zip(zipfile = zip_file, files = list.files(".", full.names = FALSE))
  setwd(old_wd)
  TRUE
}, error=function(e){
  try(setwd(old_wd), silent = TRUE)
  cat("[WARN] ZIP 打包失败：", e$message, "\n"); FALSE
})

cat("\nZIP created? ", zip_ok, " -> ", zip_file, "\n")
cat("\n=== RUN END ===\n")

# ============================================================================== 
# N) Supplement: optional FF5 + missing-input robustness + phi sensitivity
# ============================================================================== 
cat("\n[N] Supplement: optional FF5 + missing-input robustness + phi sensitivity...\n")

build_pim_res_custom <- function(
  df_be_input = df_be,
  df_profit_input = df_profit,
  df_rd_input = df_rd,
  subset_mode = c("full", "nm_inputs_pos_rd"),
  delta_rd = 0.15,
  delta_org = 0.20,
  phi_org = 0.30,
  org_cap_mode = c("baseline", "rd_only", "admin_only")
){
  subset_mode <- match.arg(subset_mode)
  org_cap_mode <- match.arg(org_cap_mode)

  annual <- df_be_input %>%
    full_join(df_profit_input, by = c("Stkcd","Year")) %>%
    full_join(df_rd_input, by = c("Stkcd","Year")) %>%
    arrange(Stkcd, Year) %>%
    mutate(
      RD_missing = is.na(RD_Exp_raw),
      Sales_missing = is.na(Sales_Exp_raw),
      Admin_missing = is.na(Admin_Exp_raw),
      BE_Raw = ifelse(is.na(BE_Raw), 0, BE_Raw),
      RD_Exp = ifelse(is.na(RD_Exp_raw), 0, RD_Exp_raw),
      Sales_Exp = ifelse(is.na(Sales_Exp_raw), 0, Sales_Exp_raw),
      Admin_Exp = ifelse(is.na(Admin_Exp_raw), 0, Admin_Exp_raw),
      SGA = Sales_Exp + Admin_Exp
    )

  if (subset_mode == "nm_inputs_pos_rd") {
    annual <- annual %>% filter(!RD_missing, !Sales_missing, !Admin_missing, RD_Exp_raw > 0)
  }

  annual %>%
    mutate(
      OrgInput = dplyr::case_when(
        org_cap_mode == "baseline" ~ SGA,
        org_cap_mode == "admin_only" ~ Admin_Exp,
        org_cap_mode == "rd_only" ~ 0,
        TRUE ~ SGA
      )
    ) %>%
    group_by(Stkcd) %>%
    arrange(Year, .by_group = TRUE) %>%
    mutate(
      K_Know  = calc_pim(RD_Exp, delta_rd),
      K_Org   = calc_pim(OrgInput * phi_org, delta_org),
      K_Total = K_Know + K_Org,
      BE_Adj  = BE_Raw + K_Total
    ) %>%
    ungroup() %>%
    select(Stkcd, Year, BE_Raw, BE_Adj, K_Total)
}

build_panel_from_pim <- function(df_pim_res_input,
                                 df_ret_input        = df_ret,
                                 df_ff3_input        = df_ff3,
                                 df_listdt_input     = df_listdt,
                                 df_basic_input      = df_basic,
                                 year_ind_full_input = year_ind_full,
                                 winsor_prob_input   = winsor_prob,
                                 min_stocks_input    = min_stocks_per_month,
                                 df_st_monthly_input = df_st_monthly,
                                 use_st_filter_input = use_st_filter,
                                 bm_denom_mode_input = bm_denom_mode_main,
                                 df_june_me_input    = df_june_me_ref){
  panel0_x <- df_ret_input %>% mutate(Match_Year = ifelse(Month >= 7, Year - 1L, Year - 2L))
  panel1_x <- panel0_x %>%
    inner_join(df_pim_res_input %>% mutate(Year = as.integer(Year)),
               by = c("Stkcd" = "Stkcd", "Match_Year" = "Year"))
  panel2_x <- panel1_x %>% inner_join(df_ff3_input, by = "Date")
  panel3_x <- panel2_x %>%
    left_join(df_listdt_input, by = "Stkcd") %>%
    mutate(
      ListYear  = year(Listdt),
      ListMonth = month(Listdt),
      AgeMonths = (Year - ListYear) * 12L + (Month - ListMonth)
    )

  panel4_x <- panel3_x %>% filter(!is.na(AgeMonths), AgeMonths >= 12)
  panel5_pre_st_x <- panel4_x %>%
    mutate(IndYear = pmin(Year, year_ind_full_input)) %>%
    left_join(df_basic_input %>% select(Stkcd, Year, IndustryCode) %>% rename(IndYear = Year),
              by = c("Stkcd","IndYear")) %>%
    filter(!is.na(IndustryCode))

  panel5_x <- apply_st_filter_to_panel(panel5_pre_st_x, df_st_monthly_input, use_st_filter_input)
  panel5_bm_x <- attach_bm_reference(panel5_x, bm_denom_mode_input, df_june_me_input)

  panel6_x <- panel5_bm_x %>%
    filter(
      is.finite(Ret),
      is.finite(ME_lag), is.finite(ME_Float_lag),
      ME_lag > 0, ME_Float_lag > 0,
      is.finite(BM_ME_ref), BM_ME_ref > 0,
      is.finite(BE_Raw), is.finite(BE_Adj),
      BE_Raw > 0, BE_Adj > 0
    ) %>%
    mutate(
      BM_Raw   = BE_Raw / (BM_ME_ref * 1000),
      BM_Adj   = BE_Adj / (BM_ME_ref * 1000),
      K_to_ME  = K_Total / (BM_ME_ref * 1000),
      Delta_BM = BM_Adj - BM_Raw,
      lnME     = log(ME_Float_lag)
    )

  df_final2_x <- panel6_x %>%
    group_by(Date) %>%
    mutate(
      Ret      = winsorize(Ret,      winsor_prob_input, min_stocks_input),
      BM_Adj   = winsorize(BM_Adj,   winsor_prob_input, min_stocks_input),
      BM_Raw   = winsorize(BM_Raw,   winsor_prob_input, min_stocks_input),
      K_to_ME  = winsorize(K_to_ME,  winsor_prob_input, min_stocks_input),
      Delta_BM = winsorize(Delta_BM, winsor_prob_input, min_stocks_input),
      lnME     = winsorize(lnME,     winsor_prob_input, min_stocks_input),
      MOM12    = winsorize(MOM12,    winsor_prob_input, min_stocks_input)
    ) %>%
    ungroup()

  df_final2_x %>%
    group_by(Date) %>%
    filter(n_distinct(Stkcd) >= min_stocks_input) %>%
    ungroup()
}

build_factor_bundle <- function(panel_df,
                                ff5_df = NULL,
                                df_ff3_input = df_ff3,
                                nw_lag_input = nw_lag,
                                nw_mode_input = nw_mode){
  if (is.null(panel_df) || nrow(panel_df) == 0) return(NULL)

  port_adj_x <- make_portfolios(panel_df, "BM_Adj")
  port_raw_x <- make_portfolios(panel_df, "BM_Raw")

  hml_adj_vw_x <- make_hml(port_adj_x, "Ret_VW") %>% rename(HML_Adj = HML)
  hml_adj_ew_x <- make_hml(port_adj_x, "Ret_EW") %>% rename(HML_Adj = HML)
  hml_raw_vw_x <- make_hml(port_raw_x, "Ret_VW") %>% rename(HML_Raw = HML)
  hml_raw_ew_x <- make_hml(port_raw_x, "Ret_EW") %>% rename(HML_Raw = HML)

  factor_vw_x <- hml_adj_vw_x %>%
    left_join(hml_raw_vw_x, by = "Date") %>%
    mutate(HML_Delta = HML_Adj - HML_Raw, WeightType = "VW") %>%
    left_join(df_ff3_input, by = "Date")

  factor_ew_x <- hml_adj_ew_x %>%
    left_join(hml_raw_ew_x, by = "Date") %>%
    mutate(HML_Delta = HML_Adj - HML_Raw, WeightType = "EW") %>%
    left_join(df_ff3_input, by = "Date")

  out <- list(
    factor_vw = factor_vw_x,
    factor_ew = factor_ew_x,
    mean_table = bind_rows(
      cbind(Weight="VW", Factor="HML_Adj",   nw_mean_t(factor_vw_x$HML_Adj,   lag_nw = nw_lag_input, mode = nw_mode_input)),
      cbind(Weight="VW", Factor="HML_Raw",   nw_mean_t(factor_vw_x$HML_Raw,   lag_nw = nw_lag_input, mode = nw_mode_input)),
      cbind(Weight="VW", Factor="HML_Delta", nw_mean_t(factor_vw_x$HML_Delta, lag_nw = nw_lag_input, mode = nw_mode_input)),
      cbind(Weight="EW", Factor="HML_Adj",   nw_mean_t(factor_ew_x$HML_Adj,   lag_nw = nw_lag_input, mode = nw_mode_input)),
      cbind(Weight="EW", Factor="HML_Raw",   nw_mean_t(factor_ew_x$HML_Raw,   lag_nw = nw_lag_input, mode = nw_mode_input)),
      cbind(Weight="EW", Factor="HML_Delta", nw_mean_t(factor_ew_x$HML_Delta, lag_nw = nw_lag_input, mode = nw_mode_input))
    ),
    delta_ff3_vw = nw_coef_table(lm(HML_Delta ~ RiskPremium + SMB + HML_Std, data = factor_vw_x), lag_nw = nw_lag_input, mode = nw_mode_input),
    delta_ff3_ew = nw_coef_table(lm(HML_Delta ~ RiskPremium + SMB + HML_Std, data = factor_ew_x), lag_nw = nw_lag_input, mode = nw_mode_input)
  )

  if (!is.null(ff5_df) && nrow(ff5_df) > 0) {
    fac_vw_5 <- factor_vw_x %>% select(Date, HML_Delta) %>% inner_join(ff5_df, by = "Date")
    fac_ew_5 <- factor_ew_x %>% select(Date, HML_Delta) %>% inner_join(ff5_df, by = "Date")
    if (nrow(fac_vw_5) >= 24) out$delta_ff5_vw <- nw_coef_table(lm(HML_Delta ~ RiskPremium + SMB + HML_Std + RMW + CMA, data = fac_vw_5), lag_nw = nw_lag_input, mode = nw_mode_input)
    if (nrow(fac_ew_5) >= 24) out$delta_ff5_ew <- nw_coef_table(lm(HML_Delta ~ RiskPremium + SMB + HML_Std + RMW + CMA, data = fac_ew_5), lag_nw = nw_lag_input, mode = nw_mode_input)
  }

  out
}

write_bundle_outputs <- function(bundle, prefix){
  if (is.null(bundle)) return(FALSE)
  safe_write_csv(bundle$mean_table,   file.path(output_dir, paste0(prefix, "_FactorMean_NWt.csv")))
  safe_write_csv(bundle$delta_ff3_vw, file.path(output_dir, paste0(prefix, "_DeltaAlpha_VW_FF3.csv")))
  safe_write_csv(bundle$delta_ff3_ew, file.path(output_dir, paste0(prefix, "_DeltaAlpha_EW_FF3.csv")))
  if (!is.null(bundle$delta_ff5_vw)) safe_write_csv(bundle$delta_ff5_vw, file.path(output_dir, paste0(prefix, "_DeltaAlpha_VW_FF5.csv")))
  if (!is.null(bundle$delta_ff5_ew)) safe_write_csv(bundle$delta_ff5_ew, file.path(output_dir, paste0(prefix, "_DeltaAlpha_EW_FF5.csv")))
  TRUE
}

run_fmb_fullsample_summary <- function(panel_df, nw_lag_input = nw_lag, nw_mode_input = nw_mode){
  gamma_ts_wls_x <- panel_df %>%
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

  gamma_ts_ols_x <- panel_df %>%
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

  bind_rows(
    cbind(term = "BM_Raw",   nw_t_const(gamma_ts_wls_x$g_BMRaw, lag_nw = nw_lag_input, mode = nw_mode_input), Weight = "WLS"),
    cbind(term = "Delta_BM", nw_t_const(gamma_ts_wls_x$g_Delta, lag_nw = nw_lag_input, mode = nw_mode_input), Weight = "WLS"),
    cbind(term = "lnME",     nw_t_const(gamma_ts_wls_x$g_lnME,  lag_nw = nw_lag_input, mode = nw_mode_input), Weight = "WLS"),
    cbind(term = "MOM12",    nw_t_const(gamma_ts_wls_x$g_MOM12, lag_nw = nw_lag_input, mode = nw_mode_input), Weight = "WLS"),
    cbind(term = "BM_Raw",   nw_t_const(gamma_ts_ols_x$g_BMRaw, lag_nw = nw_lag_input, mode = nw_mode_input), Weight = "OLS"),
    cbind(term = "Delta_BM", nw_t_const(gamma_ts_ols_x$g_Delta, lag_nw = nw_lag_input, mode = nw_mode_input), Weight = "OLS"),
    cbind(term = "lnME",     nw_t_const(gamma_ts_ols_x$g_lnME,  lag_nw = nw_lag_input, mode = nw_mode_input), Weight = "OLS"),
    cbind(term = "MOM12",    nw_t_const(gamma_ts_ols_x$g_MOM12, lag_nw = nw_lag_input, mode = nw_mode_input), Weight = "OLS")
  ) %>% relocate(Weight, term)
}

ff5_monthly <- tryCatch(
  read_optional_ff5(),
  error = function(e){
    cat("[FF5] fatal error:", e$message, "\n")
    NULL
  }
)

if (is.null(ff5_monthly)) {
  writeLines(
    c(
      "No optional FF5 file found under base_path, or FF5 parsing failed.",
      "To run FF5 alpha, place an xlsx/xls/csv file somewhere under base_path with month/date and the five columns:",
      "RiskPremium (or MKT_RF), SMB, HML (or HML_Std), RMW, CMA."
    ),
    con = file.path(output_dir, "FF5_NOT_RUN.txt")
  )
} else {
  writeLines(attr(ff5_monthly, "source_file"), con = file.path(output_dir, "FF5_Source_File.txt"))

  main_ff5_vw <- factor_vw %>% select(Date, HML_Delta) %>% inner_join(ff5_monthly, by = "Date")
  main_ff5_ew <- factor_ew %>% select(Date, HML_Delta) %>% inner_join(ff5_monthly, by = "Date")

  cat("[FF5] rows in ff5_monthly =", nrow(ff5_monthly), "\n")
  cat("[FF5] overlap VW =", nrow(main_ff5_vw), "\n")
  cat("[FF5] overlap EW =", nrow(main_ff5_ew), "\n")

  if (nrow(main_ff5_vw) >= 24) {
    delta_vw_ff5 <- nw_coef_table(lm(HML_Delta ~ RiskPremium + SMB + HML_Std + RMW + CMA, data = main_ff5_vw), nw_lag)
    safe_write_csv(delta_vw_ff5, file.path(output_dir, "DeltaAlpha_VW_FF5_NW.csv"))
  }
  if (nrow(main_ff5_ew) >= 24) {
    delta_ew_ff5 <- nw_coef_table(lm(HML_Delta ~ RiskPremium + SMB + HML_Std + RMW + CMA, data = main_ff5_ew), nw_lag)
    safe_write_csv(delta_ew_ff5, file.path(output_dir, "DeltaAlpha_EW_FF5_NW.csv"))
  }
}

pim_nm_inputs <- build_pim_res_custom(subset_mode = "nm_inputs_pos_rd", phi_org = 0.30)
panel_nm_inputs <- build_panel_from_pim(pim_nm_inputs)

nm_info <- data.frame(
  rows = nrow(panel_nm_inputs),
  stocks = dplyr::n_distinct(panel_nm_inputs$Stkcd),
  months = dplyr::n_distinct(panel_nm_inputs$Date),
  min_date = ifelse(nrow(panel_nm_inputs) == 0, NA, as.character(min(panel_nm_inputs$Date))),
  max_date = ifelse(nrow(panel_nm_inputs) == 0, NA, as.character(max(panel_nm_inputs$Date)))
)
safe_write_csv(nm_info, file.path(output_dir, "Robust_NonMissingPosRD_SampleInfo.csv"))

bundle_nm_inputs <- NULL
if (nrow(panel_nm_inputs) > 0 && dplyr::n_distinct(panel_nm_inputs$Date) >= 24) {
  bundle_nm_inputs <- build_factor_bundle(panel_nm_inputs, ff5_monthly)
  write_bundle_outputs(bundle_nm_inputs, "Robust_NonMissingPosRD")
} else {
  writeLines(
    c("Non-missing & positive-RD robustness sample is too small after screening.", capture.output(nm_info)),
    con = file.path(output_dir, "Robust_NonMissingPosRD_NOT_RUN.txt")
  )
}

phi_grid <- c(0.20, 0.30, 0.40)
sens_mean_list <- list()
sens_ff3_list <- list()
sens_ff5_list <- list()

for (phi_now in phi_grid) {
  cat("[Sensitivity] phi_org =", phi_now, "\n")
  pim_phi   <- build_pim_res_custom(subset_mode = "full", phi_org = phi_now)
  panel_phi <- build_panel_from_pim(pim_phi)

  if (nrow(panel_phi) == 0 || dplyr::n_distinct(panel_phi$Date) < 24) next

  bundle_phi <- build_factor_bundle(panel_phi, ff5_monthly)

  tmp_mean <- bundle_phi$mean_table %>%
    filter(Factor == "HML_Delta") %>%
    mutate(phi_org = phi_now, months = dplyr::n_distinct(panel_phi$Date), stocks = dplyr::n_distinct(panel_phi$Stkcd))
  sens_mean_list[[as.character(phi_now)]] <- tmp_mean

  tmp_ff3 <- bind_rows(
    alpha_row(bundle_phi$delta_ff3_vw) %>% mutate(Weight = "VW"),
    alpha_row(bundle_phi$delta_ff3_ew) %>% mutate(Weight = "EW")
  ) %>%
    mutate(phi_org = phi_now,
           months = dplyr::n_distinct(panel_phi$Date),
           stocks = dplyr::n_distinct(panel_phi$Stkcd))
  sens_ff3_list[[as.character(phi_now)]] <- tmp_ff3

  if (!is.null(bundle_phi$delta_ff5_vw) && !is.null(bundle_phi$delta_ff5_ew)) {
    tmp_ff5 <- bind_rows(
      alpha_row(bundle_phi$delta_ff5_vw) %>% mutate(Weight = "VW"),
      alpha_row(bundle_phi$delta_ff5_ew) %>% mutate(Weight = "EW")
    ) %>%
      mutate(phi_org = phi_now,
             months = dplyr::n_distinct(panel_phi$Date),
             stocks = dplyr::n_distinct(panel_phi$Stkcd))
    sens_ff5_list[[as.character(phi_now)]] <- tmp_ff5
  }
}

sens_mean <- bind_rows(sens_mean_list)
sens_ff3  <- bind_rows(sens_ff3_list)
sens_ff5  <- bind_rows(sens_ff5_list)

if (nrow(sens_mean) > 0) safe_write_csv(sens_mean, file.path(output_dir, "Sensitivity_phi_HMLDelta_Mean_NWt.csv"))
if (nrow(sens_ff3)  > 0) safe_write_csv(sens_ff3,  file.path(output_dir, "Sensitivity_phi_DeltaAlpha_FF3.csv"))
if (nrow(sens_ff5)  > 0) safe_write_csv(sens_ff5,  file.path(output_dir, "Sensitivity_phi_DeltaAlpha_FF5.csv"))



# ------------------------------------------------------------------------------
# N2) Supplement: NW 自动带宽 vs 固定3阶（主样本对照）
# ------------------------------------------------------------------------------
bundle_main_auto  <- build_factor_bundle(df_final2_f, ff5_monthly, nw_lag_input = nw_lag, nw_mode_input = "auto")
bundle_main_fixed <- build_factor_bundle(df_final2_f, ff5_monthly, nw_lag_input = 3,      nw_mode_input = "fixed")

nw_compare_mean <- bind_rows(
  bundle_main_fixed$mean_table %>% filter(Factor == "HML_Delta") %>% mutate(NW_Mode = "fixed_3"),
  bundle_main_auto$mean_table  %>% filter(Factor == "HML_Delta") %>% mutate(NW_Mode = "auto")
)
nw_compare_ff3 <- bind_rows(
  alpha_row(bundle_main_fixed$delta_ff3_vw) %>% mutate(Weight = "VW", NW_Mode = "fixed_3"),
  alpha_row(bundle_main_fixed$delta_ff3_ew) %>% mutate(Weight = "EW", NW_Mode = "fixed_3"),
  alpha_row(bundle_main_auto$delta_ff3_vw)  %>% mutate(Weight = "VW", NW_Mode = "auto"),
  alpha_row(bundle_main_auto$delta_ff3_ew)  %>% mutate(Weight = "EW", NW_Mode = "auto")
)
if (!is.null(bundle_main_fixed$delta_ff5_vw) && !is.null(bundle_main_fixed$delta_ff5_ew) &&
    !is.null(bundle_main_auto$delta_ff5_vw)  && !is.null(bundle_main_auto$delta_ff5_ew)) {
  nw_compare_ff5 <- bind_rows(
    alpha_row(bundle_main_fixed$delta_ff5_vw) %>% mutate(Weight = "VW", NW_Mode = "fixed_3"),
    alpha_row(bundle_main_fixed$delta_ff5_ew) %>% mutate(Weight = "EW", NW_Mode = "fixed_3"),
    alpha_row(bundle_main_auto$delta_ff5_vw)  %>% mutate(Weight = "VW", NW_Mode = "auto"),
    alpha_row(bundle_main_auto$delta_ff5_ew)  %>% mutate(Weight = "EW", NW_Mode = "auto")
  )
} else {
  nw_compare_ff5 <- data.frame()
}
safe_write_csv(nw_compare_mean, file.path(output_dir, "Supplement_NWCompare_HMLDelta_Mean.csv"))
safe_write_csv(nw_compare_ff3,  file.path(output_dir, "Supplement_NWCompare_DeltaAlpha_FF3.csv"))
if (nrow(nw_compare_ff5) > 0) safe_write_csv(nw_compare_ff5, file.path(output_dir, "Supplement_NWCompare_DeltaAlpha_FF5.csv"))

# ------------------------------------------------------------------------------
# N3) Supplement: 资本化口径对照（Baseline / RD-only / Admin-only）
# ------------------------------------------------------------------------------
capital_specs <- data.frame(
  capital_spec = c("baseline", "rd_only", "admin_only"),
  spec_label = c("Baseline_RDplusSGA", "RD_only", "RD_plus_AdminOnly"),
  stringsAsFactors = FALSE
)

cap_sample_list <- list()
cap_mean_list <- list()
cap_ff3_list <- list()
cap_ff5_list <- list()

for (i in seq_len(nrow(capital_specs))) {
  spec_now <- capital_specs$capital_spec[i]
  label_now <- capital_specs$spec_label[i]
  cat("[CapitalSpec] ", label_now, "\n", sep = "")

  pim_spec <- build_pim_res_custom(subset_mode = "full", phi_org = 0.30, org_cap_mode = spec_now)
  panel_spec <- build_panel_from_pim(pim_spec, bm_denom_mode_input = "lag_me")

  cap_sample_list[[label_now]] <- data.frame(
    capital_spec = spec_now,
    spec_label = label_now,
    rows = nrow(panel_spec),
    stocks = dplyr::n_distinct(panel_spec$Stkcd),
    months = dplyr::n_distinct(panel_spec$Date),
    min_date = ifelse(nrow(panel_spec) == 0, NA, as.character(min(panel_spec$Date))),
    max_date = ifelse(nrow(panel_spec) == 0, NA, as.character(max(panel_spec$Date)))
  )

  if (nrow(panel_spec) == 0 || dplyr::n_distinct(panel_spec$Date) < 24) next

  bundle_spec <- build_factor_bundle(panel_spec, ff5_monthly, nw_lag_input = nw_lag, nw_mode_input = nw_mode)

  cap_mean_list[[label_now]] <- bundle_spec$mean_table %>%
    filter(Factor == "HML_Delta") %>%
    mutate(capital_spec = spec_now, spec_label = label_now)

  cap_ff3_list[[label_now]] <- bind_rows(
    alpha_row(bundle_spec$delta_ff3_vw) %>% mutate(Weight = "VW"),
    alpha_row(bundle_spec$delta_ff3_ew) %>% mutate(Weight = "EW")
  ) %>% mutate(capital_spec = spec_now, spec_label = label_now)

  if (!is.null(bundle_spec$delta_ff5_vw) && !is.null(bundle_spec$delta_ff5_ew)) {
    cap_ff5_list[[label_now]] <- bind_rows(
      alpha_row(bundle_spec$delta_ff5_vw) %>% mutate(Weight = "VW"),
      alpha_row(bundle_spec$delta_ff5_ew) %>% mutate(Weight = "EW")
    ) %>% mutate(capital_spec = spec_now, spec_label = label_now)
  }
}

cap_sample_info <- bind_rows(cap_sample_list)
cap_mean_table  <- bind_rows(cap_mean_list)
cap_ff3_table   <- bind_rows(cap_ff3_list)
cap_ff5_table   <- bind_rows(cap_ff5_list)

if (nrow(cap_sample_info) > 0) safe_write_csv(cap_sample_info, file.path(output_dir, "Supplement_CapitalSpec_SampleInfo.csv"))
if (nrow(cap_mean_table)  > 0) safe_write_csv(cap_mean_table,  file.path(output_dir, "Supplement_CapitalSpec_HMLDelta_Mean.csv"))
if (nrow(cap_ff3_table)   > 0) safe_write_csv(cap_ff3_table,   file.path(output_dir, "Supplement_CapitalSpec_DeltaAlpha_FF3.csv"))
if (nrow(cap_ff5_table)   > 0) safe_write_csv(cap_ff5_table,   file.path(output_dir, "Supplement_CapitalSpec_DeltaAlpha_FF5.csv"))

# ------------------------------------------------------------------------------
# N4) Supplement: 6月末市值口径 BM 稳健性
# ------------------------------------------------------------------------------
panel_june_me <- build_panel_from_pim(df_pim_res, bm_denom_mode_input = "june_me")
june_me_info <- data.frame(
  bm_denom_mode = "june_me",
  rows = nrow(panel_june_me),
  stocks = dplyr::n_distinct(panel_june_me$Stkcd),
  months = dplyr::n_distinct(panel_june_me$Date),
  min_date = ifelse(nrow(panel_june_me) == 0, NA, as.character(min(panel_june_me$Date))),
  max_date = ifelse(nrow(panel_june_me) == 0, NA, as.character(max(panel_june_me$Date)))
)
safe_write_csv(june_me_info, file.path(output_dir, "Supplement_JuneME_SampleInfo.csv"))

bundle_june_me <- NULL
fmb_june_me <- NULL
if (nrow(panel_june_me) > 0 && dplyr::n_distinct(panel_june_me$Date) >= 24) {
  bundle_june_me <- build_factor_bundle(panel_june_me, ff5_monthly, nw_lag_input = nw_lag, nw_mode_input = nw_mode)
  write_bundle_outputs(bundle_june_me, "Supplement_JuneME")
  fmb_june_me <- run_fmb_fullsample_summary(panel_june_me, nw_lag_input = nw_lag, nw_mode_input = nw_mode)
  safe_write_csv(fmb_june_me, file.path(output_dir, "Supplement_JuneME_FM_FullSample.csv"))
} else {
  writeLines(
    c("June-end ME robustness sample is too small or unavailable.", capture.output(june_me_info)),
    con = file.path(output_dir, "Supplement_JuneME_NOT_RUN.txt")
  )
}

# ------------------------------------------------------------------------------
# N5) Debug: 核对 lag_me vs june_me 是否真的不同
# ------------------------------------------------------------------------------
cat("[Debug] June ME reference diagnostics...
")

june_ref_integrity <- df_june_me_ref %>%
  summarise(
    rows = n(),
    uniq_pairs = n_distinct(paste(Stkcd, BM_Ref_Year)),
    duplicate_pairs = rows - uniq_pairs,
    na_me_june = sum(!is.finite(ME_June) | is.na(ME_June)),
    min_ref_year = min(BM_Ref_Year, na.rm = TRUE),
    max_ref_year = max(BM_Ref_Year, na.rm = TRUE)
  )
safe_write_csv(june_ref_integrity, file.path(output_dir, "DEBUG_JuneME_RefIntegrity.csv"))

panel_lag_me_check  <- build_panel_from_pim(df_pim_res, bm_denom_mode_input = "lag_me")
panel_june_me_check <- build_panel_from_pim(df_pim_res, bm_denom_mode_input = "june_me")

check_ref_compare <- panel_lag_me_check %>%
  select(Stkcd, Date, Month,
         BM_Raw_lag = BM_Raw,
         BM_Adj_lag = BM_Adj,
         Delta_BM_lag = Delta_BM,
         K_to_ME_lag = K_to_ME) %>%
  inner_join(
    panel_june_me_check %>%
      select(Stkcd, Date, Month,
             BM_Raw_june = BM_Raw,
             BM_Adj_june = BM_Adj,
             Delta_BM_june = Delta_BM,
             K_to_ME_june = K_to_ME),
    by = c("Stkcd", "Date", "Month")
  ) %>%
  mutate(
    same_BM_Raw = abs(BM_Raw_lag - BM_Raw_june) < 1e-12,
    same_BM_Adj = abs(BM_Adj_lag - BM_Adj_june) < 1e-12,
    same_Delta_BM = abs(Delta_BM_lag - Delta_BM_june) < 1e-12,
    same_K_to_ME = abs(K_to_ME_lag - K_to_ME_june) < 1e-12,
    diff_BM_Raw = BM_Raw_june - BM_Raw_lag,
    diff_BM_Adj = BM_Adj_june - BM_Adj_lag,
    diff_Delta_BM = Delta_BM_june - Delta_BM_lag,
    diff_K_to_ME = K_to_ME_june - K_to_ME_lag
  )

summary_check <- check_ref_compare %>%
  summarise(
    n = n(),
    same_raw_share = mean(same_BM_Raw, na.rm = TRUE),
    same_adj_share = mean(same_BM_Adj, na.rm = TRUE),
    same_delta_share = mean(same_Delta_BM, na.rm = TRUE),
    same_k_share = mean(same_K_to_ME, na.rm = TRUE),
    mean_abs_diff_raw = mean(abs(diff_BM_Raw), na.rm = TRUE),
    mean_abs_diff_adj = mean(abs(diff_BM_Adj), na.rm = TRUE),
    mean_abs_diff_delta = mean(abs(diff_Delta_BM), na.rm = TRUE),
    mean_abs_diff_k = mean(abs(diff_K_to_ME), na.rm = TRUE)
  )
safe_write_csv(summary_check, file.path(output_dir, "DEBUG_JuneME_PanelCompare_Summary.csv"))

panel_ref_raw <- panel5 %>%
  select(Stkcd, Date, Year, Month, ME_lag, ME_Float_lag) %>%
  attach_bm_reference("lag_me", df_june_me_ref) %>%
  rename(BM_ME_ref_lag = BM_ME_ref,
         BM_ME_ref_source_lag = BM_ME_ref_source,
         ME_June_from_lag_call = ME_June) %>%
  select(Stkcd, Date, Year, Month, ME_lag, BM_ME_ref_lag, BM_ME_ref_source_lag, ME_June_from_lag_call)

panel_ref_june <- panel5 %>%
  select(Stkcd, Date, Year, Month, ME_lag, ME_Float_lag) %>%
  attach_bm_reference("june_me", df_june_me_ref) %>%
  rename(BM_ME_ref_june = BM_ME_ref,
         BM_ME_ref_source_june = BM_ME_ref_source,
         ME_June_from_june_call = ME_June) %>%
  select(Stkcd, Date, Year, Month, BM_ME_ref_june, BM_ME_ref_source_june, ME_June_from_june_call)

ref_compare <- panel_ref_raw %>%
  inner_join(panel_ref_june, by = c("Stkcd","Date","Year","Month")) %>%
  mutate(
    lag_equals_june_ref = abs(ME_lag - BM_ME_ref_june) < 1e-12,
    ref_lag_equals_ref_june = abs(BM_ME_ref_lag - BM_ME_ref_june) < 1e-12,
    june_ref_missing = !is.finite(BM_ME_ref_june) | is.na(BM_ME_ref_june),
    lag_ref_missing = !is.finite(BM_ME_ref_lag) | is.na(BM_ME_ref_lag)
  )

ref_summary <- ref_compare %>%
  summarise(
    n = n(),
    share_lag_equals_june_ref = mean(lag_equals_june_ref, na.rm = TRUE),
    share_ref_equal = mean(ref_lag_equals_ref_june, na.rm = TRUE),
    share_june_ref_missing = mean(june_ref_missing, na.rm = TRUE),
    share_lag_ref_missing = mean(lag_ref_missing, na.rm = TRUE),
    mean_abs_ref_diff = mean(abs(BM_ME_ref_lag - BM_ME_ref_june), na.rm = TRUE)
  )
safe_write_csv(ref_summary, file.path(output_dir, "DEBUG_JuneME_RefCompare_Summary.csv"))

sample_stocks <- ref_compare %>%
  distinct(Stkcd) %>%
  arrange(Stkcd) %>%
  slice_head(n = 3) %>%
  pull(Stkcd)

sample_rows <- ref_compare %>%
  filter(Stkcd %in% sample_stocks,
         Date >= as.Date("2020-07-01"),
         Date <= as.Date("2021-06-01")) %>%
  select(Stkcd, Date, Year, Month,
         ME_lag,
         BM_ME_ref_lag, BM_ME_ref_source_lag,
         BM_ME_ref_june, BM_ME_ref_source_june,
         ME_June_from_lag_call, ME_June_from_june_call,
         lag_equals_june_ref, ref_lag_equals_ref_june) %>%
  arrange(Stkcd, Date)
safe_write_csv(sample_rows, file.path(output_dir, "DEBUG_JuneME_SampleRows.csv"))

sample_diff_rows <- check_ref_compare %>%
  filter(!(same_BM_Raw & same_BM_Adj & same_Delta_BM & same_K_to_ME)) %>%
  arrange(Stkcd, Date) %>%
  slice_head(n = 5000)
safe_write_csv(sample_diff_rows, file.path(output_dir, "DEBUG_JuneME_FirstDiffRows.csv"))

cat("[Debug] June ME panel compare summary:
")
print(summary_check)
cat("[Debug] June ME ref compare summary:
")
print(ref_summary)

saveRDS(
  list(
    ff5_monthly = ff5_monthly,
    panel_nm_inputs = panel_nm_inputs,
    bundle_nm_inputs = bundle_nm_inputs,
    sens_mean = sens_mean,
    sens_ff3 = sens_ff3,
    sens_ff5 = sens_ff5,
    bundle_main_auto = bundle_main_auto,
    bundle_main_fixed = bundle_main_fixed,
    nw_compare_mean = nw_compare_mean,
    nw_compare_ff3 = nw_compare_ff3,
    nw_compare_ff5 = nw_compare_ff5,
    cap_sample_info = cap_sample_info,
    cap_mean_table = cap_mean_table,
    cap_ff3_table = cap_ff3_table,
    cap_ff5_table = cap_ff5_table,
    panel_june_me = panel_june_me,
    bundle_june_me = bundle_june_me,
    fmb_june_me = fmb_june_me,
    june_ref_integrity = june_ref_integrity,
    check_ref_compare = check_ref_compare,
    summary_check = summary_check,
    ref_compare = ref_compare,
    ref_summary = ref_summary,
    sample_rows = sample_rows,
    sample_diff_rows = sample_diff_rows
  ),
  file = file.path(output_dir, "workspace_results_supplement.rds")
)

zip_ok2 <- tryCatch({
  if (file.exists(zip_file)) file.remove(zip_file)
  old_wd2 <- getwd()
  setwd(output_dir)
  utils::zip(zipfile = zip_file, files = list.files(".", full.names = FALSE))
  setwd(old_wd2)
  TRUE
}, error = function(e){
  try(setwd(old_wd2), silent = TRUE)
  cat("[WARN] supplemental ZIP refresh failed:", e$message, "\n")
  FALSE
})

cat("[OK] supplemental ZIP refreshed? ", zip_ok2, " -> ", zip_file, "\n")
cat("\n=== SUPPLEMENT END ===\n")
