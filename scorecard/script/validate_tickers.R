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
source(paste0(SCRIPT_PATH, "get_target_price.R"))
source(paste0(SCRIPT_PATH, "get_earnings.R"))
source(paste0(SCRIPT_PATH, "format_number.R"))
source(paste0(SCRIPT_PATH, "move_col_after.R"))
source(paste0(SCRIPT_PATH, "get_market_cap.R"))


# Inputs ---------------------------------------
datasource <- "T"

# basket
basket <- "constituents_XAR_20200216"

# other parameters
start_date <- "2020-09-01"
end_date <- "2021-02-16"

# tickers 
tkr_list <- read.table(paste0(BASKET_PATH, basket, ".csv"), header=FALSE, sep=",", stringsAsFactors=FALSE)[,1]
all_tks <- unique(tkr_list)


# Data -----------------------------------------
# read price file, if not exist, create one.
pricefile_name <- paste("R_prc_cl", basket, start_date, end_date, datasource, sep="_")
hipricefile_name <- paste("R_prc_hi", basket, start_date, end_date, datasource, sep="_")
lopricefile_name <- paste("R_prc_lo", basket, start_date, end_date, datasource, sep="_")
openfile_name = paste("R_prc_op", basket, start_date, end_date, datasource, sep="_")
pricefile <- paste0(PRICEFILE_PATH, pricefile_name, ".csv")
hipricefile <- paste0(PRICEFILE_PATH, hipricefile_name, ".csv")
lopricefile <- paste0(PRICEFILE_PATH, lopricefile_name, ".csv")
openpricefile <- paste0(PRICEFILE_PATH, openfile_name, ".csv")

if(!file.exists(pricefile)) {
  cat("Cannot find existing price file.\n")
  price_list <- get_prices(tickers=all_tks, start=start_date, end=end_date,
                           types=c("Cl", "Hi", "Lo", "Op"), datasource=datasource,
                           outfiles=c(pricefile, hipricefile, lopricefile, openpricefile))
} else {
  cat("Price files already exist. Reading from existing file...\n")
}
prices <- read.zoo(pricefile, sep=",", index.column=1, header=TRUE, check.names=FALSE)
prices <- as.xts(prices)
cat("Prices read from file ", pricefile, "\n")
hiprices <- read.zoo(hipricefile, sep=",", index.column=1, header=TRUE, check.names=FALSE)
hiprices <- as.xts(hiprices)
cat("High prices read from file ", hipricefile, "\n")
loprices <- read.zoo(lopricefile, sep=",", index.column=1, header=TRUE, check.names=FALSE)
loprices <- as.xts(loprices)
cat("Low prices read from file ", lopricefile, "\n")
openprices <- read.zoo(openpricefile, sep=",", index.column=1, header=TRUE, check.names=FALSE)
openprices <- as.xts(openprices)
cat("Open prices read from file ", openpricefile, "\n")

# download target prices and find last price
tp_use_col <- c("tp_mean_est", "tp_median_est", "tp_high_est", "tp_low_est", "tp_std_dev_est")
tp_list_last <- get_last_target_price(tickers=all_tks, start=as.Date(end_date)-100, end=end_date, use_col=tp_use_col)
tp_mean <- tp_list_last$tp_mean_est
tp_median <- tp_list_last$tp_median_est
tp_high <- tp_list_last$tp_high_est
tp_low <- tp_list_last$tp_low_est
tp_sd <- tp_list_last$tp_std_dev_est
tp_mean_lag <- get_last_target_price(tickers=all_tks, start=as.Date(end_date)-465, end=as.Date(end_date)-365, use_col="tp_mean_est")$tp_mean_est

# download earnings and find last earnings
earnings_list <- get_earnings(tickers=all_tks, start=as.Date(end_date)-365, end=end_date)
earnings <- unlist(lapply(earnings_list, function(tk) {
  if(NROW(tk) == 0) return(NULL) else return(tail(tk[, "eps_act"], 1))
}))

# download market cap and find last market cap
market_cap_list <- get_market_cap(tickers=all_tks, start=as.Date(end_date)-365, end=end_date)
market_cap <- unlist(lapply(market_cap_list, function(tk) {
  if(NROW(tk) == 0) return(NULL) else return(tail(tk[, "Market Capitalization"], 1))
}))
