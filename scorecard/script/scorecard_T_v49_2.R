# This script collects statistics for given list of tickers. 
# Prices are downloaded from selected sources if not found locally.

# Update log  ----------------------------------
# 6DD  is v6 with drawdowns
# 7DD  added % of ups when QQQ is down
# 8DD  corr of returns rather than prices 
# 9DD  added rank by sharpe to output, added Moat, 
#      rounded output to 2 decimals, fixed bugs
# 10DD input "Moat" from basket file
# 11DD added sum of Sharpe to output; sort by sum of Sharpe
# 12DD ranks dropped
# 13DD added 2Y sharpe to sharpe sum; added column "Distance from 52-Week High"
# 14DD rearrange columns
# 15DD added distance(SD) to 50D SMA 
# 16DD added 6M, 1Y stats; changed column names
# 17DD added 1M stats; added new column "Average Annulalized Return" for selected lookbacks
# 18DD using RQuantLib for YTD calculation
# 19DD in "Average Annulalized Return": added 5Y to lookbacks and added "Avg. Monthly Return"
# 20DD index based on ticker rather than interger
# 21DD added return and sharpe for period "Since Jun 2002"
# 22DD added 5Y to Sum of Sharpes
# 23DD added stats for 10Y and 15Y; removed 1M from Sum of Sharpes and Avg of Returns;
#      rearranged output as: 15Y, 10Y, 1Y, 5Y, 2Y, 6M, 3M, 1M, 2W, YTD, Since 2002
# 24DD dropped YTD; keep annualized numbers only for periods longer than 1Y; 
#      rearranged output as: 15Y, 10Y, 5Y, 1Y, 2Y, 6M, 3M, 1M, 2W, Since 2002
# 25DD added 2 place holders for stats period 'since inception'
# 26DD added stats period 'since inception'
# 27DD replaced 2Y with 3Y; changed Sum of Sharpe to Avg Sharpe;
#      use non-NA valued to calc avg of sharpe and append [available years] to value;
#      use 3M, 6M, 1Y, 3Y, 5Y, 10Y, 15Y for Sharpe calc; 
#      sort output by Avg Sharpe
#      rearranged output as: 15Y, 10Y, 5Y, 3Y, 1Y, 6M, 3M, 1M, Since 2002
#      dropped corr tk: AMZN, NFLX; moved QQQcorrelation after SPYcorrelation
#      for period > 1Y, merge ann return and sharpe to one column
# change on 27DD 
#      for '1Y' dropped total return, then merge ann return and sharpe to one column
# 28DD split the sharpe ratios with the returns
# 29DD merge the sharpe with the returns, but do not drop the sharpe in order to color
# 30DD misc output formatting; added Alec Path config
# 31DD fix read data date index bug
# 32DD reroll data reading format to 30DD; 
#      merge sharpe with return for periods < 1Y, but do not drop the sharpe in order to color;
#      use {sharpe}
# 33DD change "since 2002" to "since 1998"
# 34DD drop 3M in Avg Sharpe and Avg Annualized Rtn calc
# 35DD geometric average for periods higher than one year
# 36DD added column "Growth of $100 since 1998, USD"(PV of $100 invested in 1998)
#      fixed bug in annualized return calculation with geometric average
# 37DD merged "Growth" column with "Ann Return" column
# 38DD new format for dollar output(add thousand separator, round to ones place)
# 39DD move stats for "since inception" before "since 1998"; use arithmetic mean for "since inception"
# v40  rename script "scorecard_T_[version]"
# v41  add "Distance from 1Y Mean Target Price";
# v42  use new get_prices.R (v3.6)
# v43  append "Avg Aharpe" to "Avg Rtns" and drop "Avg Sharpe"
# v44  change output filename
# v45  remove "(average monthly return)" from "Average Ann return"
# v46  remove "Moat"; add column "Price Target Envelope"(with new get_target_price())
# v47  redefine "distance"("with respect to target" -> "with respect to current price");
#      break column "price target envelope"
# v48  use new get_prices.R (v4.0)
#      add column "Price Relative to 200D SMA"
# v49  add columns "EPS", "Distance to 1Y Mean -1SD Target Price", "Distance to 1Y Mean +1SD Target Price",
#      "Distance to 1Y Mean Target Price (1Y Ago)";
#      change column "Price Target Envelope (Median)" name to "Distance to 1Y Median Target Price";
#      remove columns "Price Target Envelope (Low)", "Price Target Envelope (High)"
#      rearrange columns;
#      use new get_target_price.R (v5.1)


# Configuration --------------------------------
rm(list = ls())
ALEC_PATH <- FALSE
SCRIPT_PATH <- ifelse(ALEC_PATH, "C:/R/", "./script/")
BASKET_PATH <- ifelse(ALEC_PATH, "C:/kensho/scorecard/", "./data/")
PRICEFILE_PATH <- ifelse(ALEC_PATH, "C:/kensho/basket/", "./data/")
OUTPUT_PATH <- ifelse(ALEC_PATH, "C:/kensho/scorecard/", "./output/")

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
version <- "v49"
datasource <- "Y" #K:Kensho  G:Google

# parameters
 basket <- "scorecard_single_ticker"
# basket <- "scorecard_new_vista_T"
# basket <- "scorecard_ETF"
start_date <- "1980-01-01"
high_to_close_start_date <- "2020-02-19"
end_date <- format(Sys.Date())
DDthreshold <- 20
downSize <- 0.0025
corr_tks <- c("SPY", "QQQ")
corrLength <- 756
n2years <- 504
ytd <- businessDaysBetween("UnitedStates/NYSE", as.Date("2019-01-01"), as.Date(end_date))
jan1998_td <- businessDaysBetween("UnitedStates/NYSE", as.Date("1998-01-01"), as.Date(end_date))
lookbacks <- c(10, 21, 3*21, 6*21, 1*252, 3*252, 5*252, 10*252, 15*252, jan1998_td)
names(lookbacks) <- c("2W", "1M", "3M", "6M", "1Y", "3Y", "5Y", "10Y", "15Y", "Since Jan 1998")
lookbacks <- lookbacks[c("15Y", "10Y", "5Y", "3Y", "1Y", "6M", "3M", "1M", "Since Jan 1998")]
period_gteq_1y <- c("15Y", "10Y", "5Y", "3Y", "1Y", "Since Jan 1998", "Since Inception/1980")
period_less_1y <- names(lookbacks)[!(names(lookbacks) %in% period_gteq_1y)]

# tickers 
tkr_list <- read.table(paste0(BASKET_PATH, basket, ".csv"), header=TRUE, sep=",", stringsAsFactors=FALSE)[,1]
all_tks <- unique(c(tkr_list, corr_tks))

# outfile path
outfile_name <- paste("score_T", basket, start_date, end_date, datasource, version, sep="_")
outfile <- paste0(OUTPUT_PATH, outfile_name, ".csv")


# Data -----------------------------------------
# read price file, if not exist, create one.
pricefile_name <- paste("prc", basket, start_date, end_date, datasource, sep="_")
pricefile <- paste0(PRICEFILE_PATH, pricefile_name, ".csv")
hipricefile_name <- paste("hi", basket, start_date, end_date, datasource, sep="_")
hipricefile <- paste0(PRICEFILE_PATH, hipricefile_name, ".csv")
lopricefile_name <- paste("lo", basket, start_date, end_date, datasource, sep="_")
lopricefile <- paste0(PRICEFILE_PATH, lopricefile_name, ".csv")
unadjclosefile_name <-n<- paste("unadj", basket, start_date, end_date, datasource, sep="_")
unadjclosefile <- paste0(PRICEFILE_PATH, unadjclosefile_name, ".csv")
openfile_name = paste("open", basket, start_date, end_date, datasource, sep="_")
openpricefile <- paste0(PRICEFILE_PATH, openfile_name, ".csv")
if(!file.exists(pricefile)) {
  cat("Cannot find existing price file.\n")
  price_list <- get_prices(tickers=all_tks, start=start_date, types=c("Ad", "Hi", "Lo", "Cl", "Op"), 
                           end=end_date, datasource=datasource, outfiles=c(pricefile, hipricefile, lopricefile, unadjclosefile, openpricefile))
}
prices <- read.zoo(pricefile, sep=",", index.column=1, header=TRUE, check.names=FALSE)
prices <- as.xts(prices)
cat("Prices read from existing file ",pricefile, "\n\n")
hiprices <- read.zoo(hipricefile, sep=",", index.column=1, header=TRUE, check.names=FALSE)
hiprices <- as.xts(hiprices)
cat("High prices read from existing file ",hipricefile, "\n\n")
loprices <- read.zoo(lopricefile, sep=",", index.column=1, header=TRUE, check.names=FALSE)
loprices <- as.xts(loprices)
cat("Low prices read from existing file ",lopricefile, "\n\n")
unadjprices <- read.zoo(unadjclosefile, sep=",", index.column=1, header=TRUE, check.names=FALSE)
unadjprices <- as.xts(unadjprices)
cat("Unadjusted close prices read from existing file ",unadjclosefile, "\n\n")
openprices <- read.zoo(openpricefile, sep=",", index.column=1, header=TRUE, check.names=FALSE)
openprices <- as.xts(openprices)
cat("Open prices read from existing file ",openpricefile, "\n\n")


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
#market_cap <- market_cap * last(prices, "1 day")
# Calc returns
r_daily <- lapply(prices, function(tk) {
  temp <- dailyReturn(tk)
  colnames(temp) <- names(tk)
  temp
})
r_daily <- do.call(merge, r_daily)

# Calc close to high returns
r_hi_daily <- ((hiprices - lag(unadjprices)) / lag(unadjprices)) * 100
# Calc close to open returns
r_open_daily <- ((openprices - lag(unadjprices)) / lag(unadjprices)) * 100
# calc daily spread as a ratio of close price
r_spread_daily <- ((hiprices - loprices)) / unadjprices
r_spread_daily <- r_spread_daily[, !colnames(r_spread_daily) %in% c("SPY", "QQQ")]
# rolling week high prices
rolling_high_weekly <-rollapply(hiprices, width=5, FUN = max)
# rolling week low prices
rolling_low_weekly <-rollapply(loprices, width=5, FUN = min)
# rolling month high prices
rolling_high_monthly <-rollapply(hiprices, width=22, FUN = max)
# rolling month low prices
rolling_low_monthly <-rollapply(loprices, width=22, FUN = min)
rolling_spread_weekly <- (rolling_high_weekly - rolling_low_weekly) / unadjprices
rolling_spread_weekly <- rolling_spread_weekly[, !colnames(rolling_spread_weekly) %in% c("SPY", "QQQ")]
rolling_spread_monthly <- (rolling_high_monthly - rolling_low_monthly) / unadjprices
rolling_spread_monthly <- rolling_spread_monthly[, !colnames(rolling_spread_monthly) %in% c("SPY", "QQQ")]


# Calc Inception to date(life_to_date)
inceptions <- sapply(prices, function(tk){min(which(!is.na(tk)))})
life_to_date <- NROW(prices) - inceptions
max_lookbacks <- sapply(life_to_date, function(ltd){
  sorted_lookbacks <- sort(lookbacks[names(lookbacks)!="Since Jan 1998"], decreasing=F)
  ltd_vs_lookbacks <- ltd > sorted_lookbacks
  largest_idx <- max(which(ltd_vs_lookbacks))
  return(names(sorted_lookbacks[largest_idx]))
})


# Statistics Container ------------------------------
rtn_colnames <- paste0(c(names(lookbacks), "Since Inception/1980"), " Return")
rtn_ann_colnames <- paste0(c(names(lookbacks), "Since Inception/1980"), " Return(Annualized)")
sharpe_colnames <- paste0(c(names(lookbacks), "Since Inception/1980"), " Sharpe")
cor_colnames <- paste0(corr_tks, "correlation")

stats_colnames <- c(
  "Ticker",
  as.vector(rbind(rtn_colnames, rtn_ann_colnames, sharpe_colnames)), 
  cor_colnames,
  "N 20+days UP Periods",
  "Beta Up",
  "N 20+days DOWN Periods",
  "Beta Down",
  "Beta up to 3yr",
  "Up/QQQ down"
)
stats_colClasses <- c(
  "character",
  rep("numeric", length(c(rtn_colnames, rtn_ann_colnames, sharpe_colnames))),
  rep("numeric", length(cor_colnames)),
  "character",
  "character",
  "character",
  "character",
  "character",
  "character"
)
stats_df <- read.csv(
  text="", col.names=stats_colnames, colClasses=stats_colClasses, check.names=FALSE
)


# Backtesting for every tickers ----------------
cor_start <- NROW(prices) - corrLength

for(k in tkr_list) {
  iv <- cor_start # fixed bug changed results
  im <- min(which(!is.na(r_daily[,k]))) # if available length smaller than corrLength, use whatever there are
  if(iv < im) iv <- im
  vol <- as.numeric(sd(r_daily[iv:NROW(prices),k], na.rm=TRUE))
  
  # Calc returns and sharpe ratios
  lookbacks_k <- c(lookbacks, life_to_date[k])
  names(lookbacks_k)[length(lookbacks_k)] <- "Since Inception/1980"
  
  lb_start <- NROW(prices) - lookbacks_k
  if(any(lb_start<=0)) stop("Data length shorter than some of the lookbacks")
  
  R <- coredata(prices)[NROW(prices), k] / coredata(prices)[lb_start, k] - 1
  
  # annualized(arithmetic mean) return and sharpe
  Ra <- R * 252 / lookbacks_k
  Sh <- R / (lookbacks_k*vol)*sqrt(252)
  
  # use geometric mean to annualize for periods longer than 1Y except for "since inception"
  idx <- which(lookbacks_k > 252 & names(lookbacks_k) != "Since Inception/1980")
  Ra[idx] <- (1+R[idx])^(252/lookbacks_k[idx]) - 1
  Sh[idx] <- sapply(lookbacks_k[idx], function(laglen){
    rr <- tail(r_daily[,k], laglen)
    if(anyNA(rr)) {
      return(NA)
    } else {
      return(mean(rr)/sd(rr)*sqrt(252))
    }
  })

  # Combine results
  R_Ra_Sh <- as.vector(rbind(R, Ra, Sh))
  
  # Calc correlations
  correlations <- sapply(corr_tks, function(tk){
    cor(
      x=r_daily[iv:NROW(r_daily), k], 
      y=r_daily[iv:NROW(r_daily), tk], 
      use="pairwise.complete.obs"
    )
  })

  # create a str vector
  str_stats <- character()
  
  # start DD
  ###### can be extracted outside the loop
  is <- min(which(!is.na(r_daily[,k])))
  if(is < min(which(!is.na(r_daily[, "SPY"])))) is <- min(which(!is.na(r_daily[, "SPY"])))
  alldrd <- findDrawdowns(r_daily[is:NROW(prices), "SPY"]) # find DD of SPY
  Nd <- length(alldrd$from)
  if(Nd > 0) {
    upStart <- NULL
    upEnd <- NULL
    downStart <- NULL
    downEnd <- NULL
    for(i in 1:Nd) {
      if(alldrd$peaktotrough[i] >= DDthreshold) {
        downStart <- c(downStart,alldrd$from[i]-1)
        downEnd <- c(downEnd, alldrd$trough[i]-1)
      }
      if(alldrd$recovery[i] >= DDthreshold) {
        upStart <- c(upStart,alldrd$trough[i]-1)
        upEnd <- c(upEnd, alldrd$to[i]-1)
      }
    }
    
    Nup <- length(upStart) # Number of up periods longer than threshold
    if(Nup > 0) {
      betaUp <- rep(0, Nup)
      for(i in 1:Nup) {
        ius <- upStart[i] + is - 1
        iue <- upEnd[i] + is - 1
        y <- r_daily[ius:iue, k]
        x <- r_daily[ius:iue, "SPY"]
        fit <- lm(y ~ x)
        betaUp[i] <- summary(fit)$coefficients[2, 1]
      }
      str_stats <- c(str_stats, Nup, formatC(mean(betaUp), digits=2, format="f"))
    } else {
      str_stats <- c(str_stats, "NA", "NA")
    }
    
    Ndown <- length(downStart)
    if(Ndown > 0) {
      betaDown <- rep(0, Ndown)
      for(i in 1:Ndown) {
        ids <- downStart[i] + is- 1
        ide <- downEnd[i] + is - 1
        y <- r_daily[ids:ide, k]
        x<- r_daily[ids:ide, "SPY"]
        fit <- lm(y ~ x)
        betaDown[i] <- summary(fit)$coefficients[2, 1]
      }
      str_stats <- c(str_stats, Ndown, formatC(mean(betaDown), digits=2, format="f"))
    } else {
      str_stats <- c(str_stats, "NA", "NA")
    }
  } else {
    str_stats <- c(str_stats, "NA", "NA", "NA", "NA")
  }
  
  # Calc Beta for last three years or starting trading date
  if(NROW(prices) - iv > 10) {
    y <- r_daily[iv:NROW(prices), k]
    x <- r_daily[iv:NROW(prices), "SPY"]
    fit <- lm(y ~ x)
    beta <- summary(fit)$coefficients[2, 1]
    # print(paste("beta:", tkr_list[k], beta, sep=" "))
    str_stats <- c(str_stats, formatC(beta, digits=2, format="f"))
  } else {
    str_stats <- c(str_stats, NA)
  }

  # Calc percent of Up times during QQQ down for the last two years
  iv <- NROW(prices) - n2years
  if(iv < im){
    iv <- im
  }
  downCnt <- NROW(r_daily[r_daily[iv:NROW(prices), "QQQ"] <= -downSize])
  upCnt <- NROW(r_daily[(r_daily[iv:NROW(prices), "QQQ"] <= -downSize) 
                      & (r_daily[iv:NROW(prices), k] >= downSize)])
  if(downCnt > 0) {
    str_stats <- c(str_stats, toPercent(upCnt/downCnt))
  } else {
    str_stats <- c(str_stats, "NA")
  }
  
  # save results to dataframe
  names(str_stats) <- NULL
  stats_k_df <- data.frame(
    c(
      list(k),
      as.list(R_Ra_Sh),
      as.list(correlations),
      as.list(str_stats)
    ), 
    stringsAsFactors=F
  )
  colnames(stats_k_df) <- stats_colnames
  stats_df <- rbind(stats_df, stats_k_df)
}

# Add dist to target prices
calc_dist_to_goal <- function(p, goal) {
  if(!is.null(goal)) {
    return(goal[stats_df[, "Ticker"]] / as.numeric(last(p[, stats_df[, "Ticker"]])) - 1)
  } else {
    return(NA)
  }
}
stats_df[, "Distance to 1Y Mean Target Price"] <- toPercent(calc_dist_to_goal(prices, tp_mean), plus_sign=TRUE)
stats_df[, "Distance to 1Y Median Target Price"] <- toPercent(calc_dist_to_goal(prices, tp_median), plus_sign=TRUE)
stats_df[, "Distance to 1Y Mean -1SD Target Price"] <- toPercent(calc_dist_to_goal(prices, tp_mean - tp_sd), plus_sign=TRUE)
stats_df[, "Distance to 1Y Mean +1SD Target Price"] <- toPercent(calc_dist_to_goal(prices, tp_mean + tp_sd), plus_sign=TRUE)
stats_df[, "Distance to 1Y Mean Target Price (1Y Ago Lagged)"] <- toPercent(calc_dist_to_goal(prices, tp_mean_lag), plus_sign=TRUE)

# add in close to high prices
CLOSE_TO_HIGH_AND_OPEN_THRESHOLDS <- c(0.5, 0.75, 1, 1.5, 2)
r_hi_daily <- r_hi_daily[, !colnames(r_hi_daily) %in% c("SPY", "QQQ")]
r_hi_daily <- r_hi_daily[index(r_hi_daily) >= high_to_close_start_date]
for (val in CLOSE_TO_HIGH_AND_OPEN_THRESHOLDS) {
  stats_df[, paste0("% of Days Close T-1 to High T > ", val, " Since ", high_to_close_start_date)] <- toPercent(colSums(r_hi_daily >= val, na.rm=T) / colSums(!is.na(r_hi_daily)))
}
r_open_daily <- r_open_daily[, !colnames(r_open_daily) %in% c("SPY", "QQQ")]
r_open_daily <- r_open_daily[index(r_open_daily) >= high_to_close_start_date]
for (val in CLOSE_TO_HIGH_AND_OPEN_THRESHOLDS) {
  stats_df[, paste0("% of Days Close T-1 to Open T > ", val, " Since ", high_to_close_start_date)] <- toPercent(colSums(r_open_daily >= val, na.rm=T) / colSums(!is.na(r_hi_daily)))
}

stats_df[, paste0("1M Median Daily Return")] <- toPercent(colMedians(last(r_daily[, !colnames(r_daily) %in% c("SPY", "QQQ")], "1 month"), na.rm=T))
stats_df[, paste0("3M Median Daily Return")] <- toPercent(colMedians(last(r_daily[, !colnames(r_daily) %in% c("SPY", "QQQ")], "3 months"), na.rm=T))

stats_df[, paste0("Lifetime Mean Daily Spread")] <- toPercent(colMeans(r_spread_daily, na.rm=T))
stats_df[, paste0("3M Mean Daily Spread")] <- toPercent(colMeans(last(r_spread_daily, "3 months"), na.rm=T))
stats_df[, paste0("3M Mean Weekly Spread")] <- toPercent(colMeans(last(rolling_spread_weekly, "3 months"), na.rm=T))
stats_df[, paste0("6M Mean Monthly Spread")] <- toPercent(colMeans(last(rolling_spread_monthly, "6 months"), na.rm=T))
stats_df[, paste0("Weekly Spread > 10% (Last 3M)")] <-toPercent(colSums(last(rolling_spread_weekly, "3 months") > 0.1, na.rm=T) / colSums(!is.na(last(rolling_spread_weekly, "3 months"))))
stats_df[, paste0("Monthly Spread > 10% (Last 6M)")] <-toPercent(colSums(last(rolling_spread_monthly, "6 months") > 0.1, na.rm=T) / colSums(!is.na(last(rolling_spread_monthly, "6 months"))))
last_data_row = last(prices)
last_data_date = index(last_data_row)[1]
last_data_row = last_data_row[, !colnames(last_data_row) %in% c("SPY", "QQQ")]
stats_df[, paste0("Close Price on ", last_data_date)] = t(last_data_row)
# Add max_lookbacks to df
stats_df[, "max_lookbacks"] <- max_lookbacks[stats_df[, "Ticker"]]

# Add inception_year to df
stats_df[, "inception_year"] <- sapply(inceptions[stats_df[, "Ticker"]], function(incp_idx) {
  return(format(index(prices[incp_idx]), "%Y"))
})

# Calc PV of $100 invested in 1998
stats_df[, "Growth of $100 since 1998, USD"] <- (stats_df[, "Since Jan 1998 Return"] + 1) * 100

# Calc Avg Sharpe ratios
sharpe_to_avg <- paste0(c("6M", "1Y", "3Y", "5Y", "10Y", "15Y"), " Sharpe")
avg_sharpe <- rowMeans(sapply(stats_df[, sharpe_to_avg], FUN=as.numeric), na.rm=T)
stats_df[, "Avg Sharpe"] <- avg_sharpe

# Calc Avg Annualized Returns
rtn_to_avg <- paste0(c("6M", "1Y", "3Y", "5Y", "10Y", "15Y"), " Return(Annualized)")
avg_ann_rtn <- rowMeans(stats_df[, rtn_to_avg], na.rm=T)
stats_df[, "Avg Annualized Returns"] <- avg_ann_rtn

# Add distance to 52-week-high
high_price_52_week <- sapply(tail(prices, 252), function(tk){max(na.omit(tk))})
stats_df[,"Distance to 52-Week High"] <- toPercent(calc_dist_to_goal(prices, high_price_52_week))

# calc distance in SD from SMA
calc_dist_from_sma_in_sd <- function(p, sma_len) {
  sd_from_sma <- sapply(tail(p[, stats_df$Ticker], sma_len), function(tk){
    no_na_tk <- na.omit(tk)
    (last(no_na_tk)-mean(no_na_tk)) / sd(no_na_tk)
  })
  # format
  sapply(sd_from_sma, function(i){
    res <- paste0(toDecimalPlaces(i, 2, plus_sign=TRUE))
#    if(i>=0) return(paste0("'", res)) else return(res) # add ' to prevent treated as formula by google sheet 
  })
}
stats_df[, "Price Relative to 200D SMA"] <- calc_dist_from_sma_in_sd(prices, 200)
stats_df[, "Price Relative to 50D SMA"] <- calc_dist_from_sma_in_sd(prices, 50)

# Add EPS
stats_df[, "EPS"] <- sapply(stats_df[, "Ticker"], function(tk) {
  if(is.null(earnings[tk])) NA else toDecimalPlaces(earnings[tk], 2)
})

# Add Market Cap
stats_df[, "Market Cap"] <- sapply(stats_df[, "Ticker"], function(tk) {
  if(is.null(market_cap[tk])) NA else toDecimalPlaces(market_cap[tk], 2)
})



# Formatting ------------------------------------------------------------
# rank by Avg Sharpe. This should be done before the ranking column get further formatted(become str)
stats_df <- stats_df[order(stats_df[, "Avg Sharpe"], decreasing=T),]

# round to 2 decimal places
to_round <- c("Avg Sharpe", sharpe_colnames, cor_colnames)
stats_df[, to_round] <- sapply(stats_df[, to_round], toDecimalPlaces)

# convert to dollar
stats_df[, "Growth of $100 since 1998, USD"] <- 
  toDollar(stats_df[, "Growth of $100 since 1998, USD"], digits=0, thousand_sep=TRUE)

# convert to percentage
to_percent <- c(rtn_colnames, rtn_ann_colnames, "Avg Annualized Returns")
stats_df[, to_percent] <- sapply(stats_df[, to_percent], toPercent)

# paste Avg Sharpe to Avg Ann Rtns
stats_df[, "Avg Annualized Returns"] <- 
  paste0(stats_df[, "Avg Annualized Returns"], " {", stats_df[, "Avg Sharpe"], "}")

# paste [max_lookbacks] to Avg Sharpe and Avg Ann Returns
stats_df[, "Avg Sharpe"] <- 
  paste0(stats_df[, "Avg Sharpe"], " [", stats_df[, "max_lookbacks"], "]")
stats_df[, "Avg Annualized Returns"] <- 
  paste0(stats_df[, "Avg Annualized Returns"], " [", stats_df[, "max_lookbacks"], "]")

rtn_colnames_gteq_1y <- paste0(period_gteq_1y, " Return")
rtn_colnames_less_1y <- paste0(period_less_1y, " Return")
ann_rtn_colnames_gteq_1y <- paste0(rtn_colnames_gteq_1y, "(Annualized)")
ann_rtn_colnames_less_1y <- paste0(rtn_colnames_less_1y, "(Annualized)")

# for period < 1Y combine rtn columns with annualized rtn columns 
stats_df[, ann_rtn_colnames_less_1y] <- sapply(rtn_colnames_less_1y, function(i) {
  i_ann <- paste0(i, "(Annualized)")
  paste0(stats_df[, i], paste0("(",stats_df[, i_ann],")"))
})

# paste year of inception to inception returns, then frop inception_year column
stats_df[, "Since Inception/1980 Return(Annualized)"] <- paste0(
  stats_df[, "Since Inception/1980 Return(Annualized)"], " [", stats_df[, "inception_year"], "]"
)
stats_df <- stats_df[, colnames(stats_df) != "inception_year"]

# for "1998", merge "Growth" column with annualized rtn, then drop the "Growth" col
stats_df[, "Since Jan 1998 Return(Annualized)"] <- paste0(
  stats_df[, "Since Jan 1998 Return(Annualized)"], ", ", stats_df[, "Growth of $100 since 1998, USD"]
)
stats_df <- stats_df[, colnames(stats_df)!="Growth of $100 since 1998, USD"]

# for periods >= 1Y, append sharpe into ann rtn column
stats_df[, ann_rtn_colnames_gteq_1y] <- sapply(period_gteq_1y, function(p){
  to_merge_ann_rtn <- paste0(p, " Return(Annualized)")
  to_merge_sharpe <- paste0(p, " Sharpe")
  paste0(stats_df[, to_merge_ann_rtn], " {", stats_df[, to_merge_sharpe], "}")
})
colnames(stats_df)[colnames(stats_df) %in% ann_rtn_colnames_gteq_1y] <- paste0(period_gteq_1y, " Annualized Return {Sharpe}")

# for periods < 1Y, append sharpe into ann rtn column
stats_df[, ann_rtn_colnames_less_1y] <- sapply(period_less_1y, function(p){
  to_merge_ann_rtn <- paste0(p, " Return(Annualized)")
  to_merge_sharpe <- paste0(p, " Sharpe")
  paste0(stats_df[, to_merge_ann_rtn], " {", stats_df[, to_merge_sharpe], "}")
})
colnames(stats_df)[colnames(stats_df) %in% ann_rtn_colnames_less_1y] <- paste0(ann_rtn_colnames_less_1y, " {Sharpe}")

# drop undesired columns
stats_df <- stats_df[, colnames(stats_df) != "Avg Sharpe"]
stats_df <- stats_df[, colnames(stats_df) != "max_lookbacks"]
stats_df <- stats_df[, !(names(stats_df) %in% rtn_colnames_gteq_1y)]
stats_df <- stats_df[, !(names(stats_df) %in% rtn_colnames_less_1y)]

# rearrange columns
stats_df <- move_col_after(stats_df, paste0("Close Price on ", last_data_date), "Ticker")
stats_df <- move_col_after(stats_df, 'Market Cap', paste0("Close Price on ", last_data_date))
#stats_df <- move_col_after(stats_df, 'Market Cap Jan 1, 2021', 'Market Cap')
#stats_df <- move_col_after(stats_df, '# of Market Cap Doublings, Last 3Y', 'Market Cap Jan 1, 2021')
#stats_df <- move_col_after(stats_df, 'Avg. Time to Market Cap Doubling, Last 3Y', '# of Market Cap Doublings, Last 3Y')
#stats_df <- move_col_after(stats_df, '3Y Total Return', 'Avg. Time to Market Cap Doubling, Last 3Y')
stats_df <- move_col_after(stats_df, '3Y Total Return', 'Market Cap')
stats_df <- move_col_after(stats_df, '3Y Annualized Return {Sharpe}', '3Y Total Return')
stats_df <- move_col_after(stats_df, '1Y Annualized Return {Sharpe}', '3Y Annualized Return {Sharpe}')
stats_df <- move_col_after(stats_df, "EPS", "1Y Annualized Return {Sharpe}")
stats_df <- move_col_after(stats_df, "SPYcorrelation", "EPS")
stats_df <- move_col_after(stats_df, "QQQcorrelation", "SPYcorrelation")
stats_df <- move_col_after(stats_df, 'Performance vs QQQ, 3M', 'QQQcorrelation')
stats_df <- move_col_after(stats_df, '3M Sharpe', 'Performance vs QQQ, 3M')
stats_df <- move_col_after(stats_df, "3M Median Daily Return", '3M Sharpe')
stats_df <- move_col_after(stats_df, "1M Median Daily Return", "3M Median Daily Return")
stats_df <- move_col_after(stats_df, "Avg Annualized Returns", "1M Median Daily Return")
stats_df <- move_col_after(stats_df, "Distance to 52-Week High", "Avg Annualized Returns")
stats_df <- move_col_after(stats_df, "Distance to 1Y Mean Target Price", "Distance to 52-Week High")
stats_df <- move_col_after(stats_df, "Distance to 1Y Median Target Price", "Distance to 1Y Mean Target Price")
stats_df <- move_col_after(stats_df, "Distance to 1Y Mean -1SD Target Price", "Distance to 1Y Median Target Price")
stats_df <- move_col_after(stats_df, "Distance to 1Y Mean +1SD Target Price", "Distance to 1Y Mean -1SD Target Price")
stats_df <- move_col_after(stats_df, "Distance to 1Y Mean Target Price (1Y Ago Lagged)", "Distance to 1Y Mean +1SD Target Price")
stats_df <- move_col_after(stats_df, "Price Relative to 200D SMA", "Distance to 1Y Mean Target Price (1Y Ago Lagged)")
stats_df <- move_col_after(stats_df, "Price Relative to 50D SMA", "Price Relative to 200D SMA")
stats_df <- move_col_after(stats_df, "Beta up to 3yr", "QQQcorrelation")
stats_df <- move_col_after(stats_df, "Since Jan 1998 Annualized Return {Sharpe}", "Since Inception/1980 Sharpe")
stats_df <- move_col_after(stats_df, "Since Jan 1998 Sharpe", "Since Jan 1998 Annualized Return {Sharpe}")
stats_df <- move_col_after(stats_df, "SPYcorrelation", "EPS")

# Write output to file --------------------------------
write.table(stats_df, outfile, sep=",", row.names=FALSE)


# # Creating a workbook using the XLSX package.
# wb <- xlsx::createWorkbook(type="xlsx")
# 
# # Creating a sheet inside the workbook.
# sheet <- xlsx::createSheet(wb, sheetName="Sheet0")
# 
# # Adding the full dataset into the sheet.
# xlsx::addDataFrame(stats_df, sheet, startRow=1, startCol=1, row.names=FALSE, col.names=TRUE)
# 
# # Creating cell style needed to left-justify text.
# cs <- CellStyle(wb) + Alignment(horizontal="ALIGN_CENTER", vertical="VERTICAL_CENTER")
# 
# # Selecting rows to apply cell style to.
# all.rows <- getRows(sheet, rowIndex=1:(NROW(stats_df)+1))
# 
# # Selecting cells within selected rows to apply cell style to.
# all.cells <- getCells(all.rows)
# 
# # Applying cell style to selected cells.
# invisible(lapply(all.cells, setCellStyle, cs))
# 
# # 
# setColumnWidth(sheet, colIndex=1:NCOL(stats_df), colWidth=60)
# 
# # Saving the workbook.
# xlsx::saveWorkbook(wb, paste0(OUTPUT_PATH, outfile_name, ".xlsx"))
# 
