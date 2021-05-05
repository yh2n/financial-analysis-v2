# Calculate market(SPY) beta for selected stocks during market up periods.


# Configuration --------------------------------
rm(list = ls())
SCRIPT_PATH <- "./script/"
BASKET_PATH <- "../data/baskets/"
PRICEFILE_PATH <- "../data/raw/"
OUTPUT_PATH <- "./output/"

library(matrixStats)
library(quantmod)
library(RQuantLib)
library(PerformanceAnalytics)
source(paste0(SCRIPT_PATH, "get_prices.R"))
source(paste0(SCRIPT_PATH, "get_target_prices.R"))
source(paste0(SCRIPT_PATH, "get_earnings.R"))
source(paste0(SCRIPT_PATH, "format_number.R"))
source(paste0(SCRIPT_PATH, "move_col_after.R"))
source(paste0(SCRIPT_PATH, "get_market_cap.R"))


# Inputs ---------------------------------------
datasource <- "T"

# basket
tickers <- strsplit("UNP, HON, MS, TMO, DD, BK, QCOM, CRM", split=", ")[[1]]
corr_tks <- "SPY"
all_tks <- unique(c(tickers, corr_tks))

# other parameters
start_date <- "2010-01-01"
end_date <- "2021-04-08"


# Data  ----------------------------------------
pricefile_name <- paste("R_prc_up_beta", start_date, end_date, sep="_")
pricefile_path <- paste0(PRICEFILE_PATH, pricefile_name, ".csv")
if(!file.exists(pricefile_path)) {
  cat("Cannot find existing price file.\n")
  prices <- get_prices(tickers=all_tks, start=start_date, end=end_date,
                           outfiles=pricefile_path)
} else {
  cat("Price files already exist. Reading from existing file...\n")
  prices <- read.zoo(pricefile_path, sep=",", index.column=1, header=TRUE, check.names=FALSE)
  prices <- as.xts(prices)
}

rtns_daily <- prices / lag(prices) - 1
rtns_daily_roll_3M <- prices / lag(prices, 21*3) - 1
rtns_mthly <- lapply(prices, monthlyReturn)
rtns_mthly <- do.call(merge, rtns_mthly)
colnames(rtns_mthly) <- colnames(prices)


# Testing --------------------------------------
calc_beta <- function(lb_start, lb_end, returns) {
  lb_rtns <- returns[paste0(lb_start, "/", lb_end), ]
  lb_rtns_spy_pos <- lb_rtns[lb_rtns[, "SPY"] > 0, ]

  res <- sapply(lb_rtns_spy_pos[, tickers], function(rtn) {
    y <- lb_rtns_spy_pos[, "SPY"]
    x <- rtn
    fit <- lm(y ~ x)
    summary(fit)$coefficients[2, 1]
  })
  toDecimalPlaces(res, 2)
}

backtest <- function(lb_lens, lb_ends, returns) {
  res <- list()
  for (lb_len in lb_lens) {
    for (lb_end in lb_ends) {
      lb_end_idx <- which(index(returns) == lb_end)
      lb_start <- format(index(returns)[lb_end_idx - lb_len + 1], "%Y-%m-%d")
      params <- c(lb_len, lb_end)
      names(params) <- c("lb_len", "lb_end")
      res <- c(
        res,
        list(data.frame(t(c(params, calc_beta(lb_start, lb_end, returns)))))
      )
    }
  }
  do.call(rbind, res)
}


# for daily
lb_lens <- c(21*3, 21*6, 252, 252*2, 252*3, 252*5, 252*10)
lb_ends <- c("2021-04-08")
print("Daily Rtns:")
print(backtest(lb_lens, lb_ends, rtns_daily))
print("Daily Rtns(Rolling 3M):")
print(backtest(lb_lens, lb_ends, rtns_daily_roll_3M))

# for monthly
lb_lens <- c(12, 18, 24, 36, 60)
lb_ends <- c("2021-04-08")
print("Monthly Rtns:")
print(backtest(lb_lens, lb_ends, rtns_mthly))
