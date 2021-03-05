# This script collects statistics for given list of tickers. 
# Prices are downloaded from selected sources if not found locally.


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

TESTING_MODE <- FALSE


# Inputs ---------------------------------------
datasource <- "T"

# basket
basket <- "scorecard_single_ticker"
# basket <- "scorecard_new_vista_T"
# basket <- "scorecard_ETF"

# other parameters
start_date <- "1980-01-01"
end_date <- format(Sys.Date())
DDthreshold <- 20
downSize <- 0.0025
corr_tks <- c("SPY", "QQQ")
jan1998_td <- businessDaysBetween("UnitedStates/NYSE", as.Date("1998-01-01"), as.Date(end_date))
lookbacks <- c(10, 21, 3*21, 6*21, 1*252, 3*252, 5*252, 10*252, 15*252, jan1998_td)
names(lookbacks) <- c("2W", "1M", "3M", "6M", "1Y", "3Y", "5Y", "10Y", "15Y", "Since Jan 1998")
lookbacks <- lookbacks[c("15Y", "10Y", "5Y", "3Y", "1Y", "6M", "3M", "1M", "Since Jan 1998")]
lookback_2Y <- 504
period_gteq_1y <- c("15Y", "10Y", "5Y", "3Y", "1Y", "Since Jan 1998", "Since Inception/1980")
period_less_1y <- names(lookbacks)[!(names(lookbacks) %in% period_gteq_1y)]

if(TESTING_MODE) {
  end_date <- "2021-02-25"
  basket <- "scorecard_single_ticker_TESTING_ONLY"
  # basket <- "scorecard_single_ticker"
}

# tickers 
tkr_list <- read.table(paste0(BASKET_PATH, basket, ".csv"), header=FALSE, sep=",", stringsAsFactors=FALSE)[,1]
all_tks <- unique(c(tkr_list, corr_tks))

# outfile path
outfile_name <- paste("score_T", basket, start_date, end_date, datasource, sep="_")
outfile <- paste0(OUTPUT_PATH, outfile_name, ".csv")


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
cat("Prices read from file ",pricefile, "\n")
hiprices <- read.zoo(hipricefile, sep=",", index.column=1, header=TRUE, check.names=FALSE)
hiprices <- as.xts(hiprices)
cat("High prices read from file ",hipricefile, "\n")
loprices <- read.zoo(lopricefile, sep=",", index.column=1, header=TRUE, check.names=FALSE)
loprices <- as.xts(loprices)
cat("Low prices read from file ",lopricefile, "\n")
openprices <- read.zoo(openpricefile, sep=",", index.column=1, header=TRUE, check.names=FALSE)
openprices <- as.xts(openprices)
cat("Open prices read from file ",openpricefile, "\n")

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

# calc rolling high/low prices
cat("\nCalculating rolling Hi/Lo prices...")
# rolling week high prices
rolling_high_weekly <- rollapply(hiprices, width=5, FUN=max, align="right")
# rolling week low prices
rolling_low_weekly <- rollapply(loprices, width=5, FUN=min, align="right")
# rolling month high prices
rolling_high_monthly <- rollapply(hiprices, width=22, FUN=max, align="right")
# rolling month low prices
rolling_low_monthly <- rollapply(loprices, width=22, FUN=min, align="right")
cat("Done.")

# Calc returns
cat("\nCalculating stock returns...")
# cl to cl rtns
r_daily <- (prices - lag(prices)) / lag(prices)
# Calc close to high returns
r_hi_daily <- (hiprices - lag(prices)) / lag(prices)
# Calc close to open returns
r_open_daily <- (openprices - lag(prices)) / lag(prices)
# calc daily spread as a ratio of close price
r_spread_daily <- ((hiprices - loprices)) / prices
r_spread_daily <- r_spread_daily[, !colnames(r_spread_daily) %in% c("SPY", "QQQ")]
# calc rolling spreads
rolling_spread_weekly <- (rolling_high_weekly - rolling_low_weekly) / prices
rolling_spread_weekly <- rolling_spread_weekly[, !colnames(rolling_spread_weekly) %in% c("SPY", "QQQ")]
rolling_spread_monthly <- (rolling_high_monthly - rolling_low_monthly) / prices
rolling_spread_monthly <- rolling_spread_monthly[, !colnames(rolling_spread_monthly) %in% c("SPY", "QQQ")]
cat("Done.")

# Calc Inception to date(life_to_date)
cat("\nCalaulating lengths of stock history...")
inceptions <- sapply(prices, function(tk){min(which(!is.na(tk)))})
life_to_date <- NROW(prices) - inceptions
max_lookbacks <- sapply(life_to_date, function(ltd){
  sorted_lookbacks <- sort(lookbacks[names(lookbacks)!="Since Jan 1998"], decreasing=F)
  ltd_vs_lookbacks <- ltd > sorted_lookbacks
  largest_idx <- max(which(ltd_vs_lookbacks))
  return(names(sorted_lookbacks[largest_idx]))
})
cat("Done.")


# Statistics Container ------------------------------
cat("\nCalculating statistics...")
rtn_colnames <- paste0(c(names(lookbacks), "Since Inception/1980"), " Return")
rtn_ann_colnames <- paste0(c(names(lookbacks), "Since Inception/1980"), " Return(Annualized)")
sharpe_colnames <- paste0(c(names(lookbacks), "Since Inception/1980"), " Sharpe")

stats_colnames <- c(
  "Ticker",
  as.vector(rbind(rtn_colnames, rtn_ann_colnames, sharpe_colnames)), 
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
for(k in tkr_list) {
  lookbacks_k <- c(lookbacks, life_to_date[k])
  names(lookbacks_k)[length(lookbacks_k)] <- "Since Inception/1980"
  lb_start <- NROW(prices) - lookbacks_k
  if(any(lb_start<=0)) {
    stop("Data length shorter than some of the lookbacks")
  }
  
  # Calc lookback total returns and annualized arithmetic returns
  R_tot <- coredata(prices)[NROW(prices), k] / coredata(prices)[lb_start, k] - 1
  Ra <- R_tot * 252 / lookbacks_k
  
  # use annualized geometric rtn for periods > 1Y except for "Since inception/1980"
  idx <- which(lookbacks_k > 252 & names(lookbacks_k) != "Since Inception/1980")
  Ra[idx] <- (1+R_tot[idx])^(252/lookbacks_k[idx]) - 1
  
  # Sharpe Ratio
  Sh <- sapply(lookbacks_k, function(lb) {
    rtns <- tail(r_daily[, k], lb)
    if(anyNA(rtns)) {
      return(NA)
    } else {
      return(mean(rtns) / sd(rtns) * sqrt(252))
    }
  })
  
  # Combine results
  R_Ra_Sh <- as.vector(rbind(R_tot, Ra, Sh))
  
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
  iv <- NROW(prices) - lookbacks["3Y"] + 1
  im <- min(which(!is.na(r_daily[, k]))) # if available length < lookback, use whatever there is
  if(iv < im) iv <- im
  if(NROW(prices) - iv > 10) {
    y <- r_daily[iv:NROW(prices), k]
    x <- r_daily[iv:NROW(prices), "SPY"]
    fit <- lm(y ~ x)
    beta <- summary(fit)$coefficients[2, 1]
    str_stats <- c(str_stats, formatC(beta, digits=2, format="f"))
  } else {
    str_stats <- c(str_stats, NA)
  }
  
  # Calc percent of Up times during QQQ down for the last two years
  iv <- NROW(prices) - lookback_2Y
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
      as.list(str_stats)
    ), 
    stringsAsFactors=F
  )
  colnames(stats_k_df) <- stats_colnames
  stats_df <- rbind(stats_df, stats_k_df)
}

# Calc correlations to corr_tks
for (cor_tk in corr_tks) {
  cor_lb_name <- "3M"
  cor_lb <- lookbacks[cor_lb_name]
  cor_colname <- paste0(cor_tk, " correlation(", cor_lb_name, ")")
  stats_df[, cor_colname] <- sapply(stats_df[, "Ticker"], function(tk) {
    r_cor <- tail(r_daily[, c(cor_tk, tk)], cor_lb)
    cor(x=r_cor[, cor_tk],
        y=r_cor[, tk],
        use="pairwise.complete.obs"
    )
  })
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
stats_df[, "Distance to 1Y Mean +1SD Target Price"] <- toPercent(calc_dist_to_goal(prices, tp_mean + tp_sd), plus_sign=TRUE)
stats_df[, "Distance to 1Y Mean Target Price (1Y Ago Lagged)"] <- toPercent(calc_dist_to_goal(prices, tp_mean_lag), plus_sign=TRUE)
stats_df[, paste0("1M Median Daily Return")] <- toPercent(colMedians(last(r_daily[, !colnames(r_daily) %in% c("SPY", "QQQ")], "1 month"), na.rm=T))
stats_df[, paste0("3M Median Daily Return")] <- toPercent(colMedians(last(r_daily[, !colnames(r_daily) %in% c("SPY", "QQQ")], "3 months"), na.rm=T))
stats_df[, paste0("Lifetime Mean Daily Spread")] <- toPercent(colMeans(r_spread_daily, na.rm=T))
stats_df[, paste0("3M Mean Daily Spread")] <- toPercent(colMeans(last(r_spread_daily, "3 months"), na.rm=T))
stats_df[, paste0("3M Mean Weekly Spread")] <- toPercent(colMeans(last(rolling_spread_weekly, "3 months"), na.rm=T))
stats_df[, paste0("6M Mean Monthly Spread")] <- toPercent(colMeans(last(rolling_spread_monthly, "6 months"), na.rm=T))
stats_df[, paste0("Weekly Spread > 10% (Last 3M)")] <-toPercent(colSums(last(rolling_spread_weekly, "3 months") > 0.1, na.rm=T) / colSums(!is.na(last(rolling_spread_weekly, "3 months"))))
stats_df[, paste0("Monthly Spread > 10% (Last 6M)")] <-toPercent(colSums(last(rolling_spread_monthly, "6 months") > 0.1, na.rm=T) / colSums(!is.na(last(rolling_spread_monthly, "6 months"))))

r_monthly <- (prices - lag(prices, k=22)) / lag(prices, k=22)
r_monthly <- r_monthly[, !colnames(r_monthly) %in% c("SPY", "QQQ")]
r_three_monthly <- (prices - lag(prices, k=66)) / lag(prices, k=66)
r_three_monthly_vs_qqq <- lapply(r_three_monthly, function(tk) {
  temp <- tk - r_three_monthly[, 'QQQ']
  colnames(temp) <- names(tk)
  temp
})
r_three_monthly_vs_qqq <- do.call(merge, r_three_monthly_vs_qqq)
r_three_monthly_vs_qqq <- r_three_monthly_vs_qqq[, !colnames(r_three_monthly_vs_qqq) %in% c("SPY", "QQQ")]
stats_df[, paste0('Outperformance/Underperformance vs QQQ, 3M')] <- toPercent(last(r_three_monthly_vs_qqq, '1 day'))
stats_df[, paste0("% of Time > 10% Within 1M (Last 3Y)")] <- toPercent(colSums(last(r_monthly, "3 years") > 0.1) / colSums(!is.na(last(r_monthly, "3 years"))))
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
stats_df[, "Market Cap (BN)"] <- sapply(stats_df[, "Ticker"], function(tk) {
  if(is.null(market_cap[tk])) NA else toDecimalPlaces(market_cap[tk]/1000000000, 2)
})

cat("Done.")


# Formatting ------------------------------------------------------------
cat("Formatting...")
# rank by 3M Sharpe. This should be done before the ranking column get further formatted(become str)
stats_df <- stats_df[order(stats_df[, "3M Sharpe"], decreasing=T),]

# round to 2 decimal places
to_round <- c("Avg Sharpe",
              sharpe_colnames,
              grep("correlation", names(stats_df), value=TRUE))
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
# keep 3Y Return only and drop other lookback rtn columns
rtn_colnames_gteq_1y_to_del <- rtn_colnames_gteq_1y[rtn_colnames_gteq_1y != "3Y Return"]
stats_df <- stats_df[, colnames(stats_df) != "Avg Sharpe"]
stats_df <- stats_df[, colnames(stats_df) != "max_lookbacks"]
stats_df <- stats_df[, !(names(stats_df) %in% rtn_colnames_gteq_1y_to_del)]
stats_df <- stats_df[, !(names(stats_df) %in% rtn_colnames_less_1y)]

# reorder columns
stats_df <- move_col_after(stats_df, paste0("Close Price on ", last_data_date), "Ticker")
stats_df <- move_col_after(stats_df, "Market Cap (BN)", paste0("Close Price on ", last_data_date))
stats_df <- move_col_after(stats_df, "3Y Return", "Market Cap (BN)")
stats_df <- move_col_after(stats_df, "3Y Annualized Return {Sharpe}", "3Y Return")
stats_df <- move_col_after(stats_df, "1Y Annualized Return {Sharpe}", "3Y Annualized Return {Sharpe}")
stats_df <- move_col_after(stats_df, "EPS", "1Y Annualized Return {Sharpe}")
stats_df <- move_col_after(stats_df, "SPY correlation(3M)", "EPS")
stats_df <- move_col_after(stats_df, "QQQ correlation(3M)", "SPY correlation(3M)")
stats_df <- move_col_after(stats_df, "Outperformance/Underperformance vs QQQ, 3M", "QQQ correlation(3M)")
stats_df <- move_col_after(stats_df, "3M Sharpe", "Outperformance/Underperformance vs QQQ, 3M")
stats_df <- move_col_after(stats_df, "3M Median Daily Return", "3M Sharpe")
stats_df <- move_col_after(stats_df, "1M Median Daily Return", "3M Median Daily Return")
stats_df <- move_col_after(stats_df, "Weekly Spread > 10% (Last 3M)", "1M Median Daily Return")
stats_df <- move_col_after(stats_df, "% of Time > 10% Within 1M (Last 3Y)", "Weekly Spread > 10% (Last 3M)")
stats_df <- move_col_after(stats_df, "Avg Annualized Returns", "Weekly Spread > 10% (Last 3M)")
stats_df <- move_col_after(stats_df, "Distance to 52-Week High", "Avg Annualized Returns")
stats_df <- move_col_after(stats_df, "Distance to 1Y Median Target Price", "Distance to 52-Week High")
stats_df <- move_col_after(stats_df, "Distance to 1Y Mean Target Price", "Distance to 1Y Median Target Price")
stats_df <- move_col_after(stats_df, "Distance to 1Y Mean +1SD Target Price", "Distance to 1Y Mean Target Price")
stats_df <- move_col_after(stats_df, "Distance to 1Y Mean Target Price (1Y Ago Lagged)", "Distance to 1Y Mean +1SD Target Price")
stats_df <- move_col_after(stats_df, "Price Relative to 200D SMA", "Distance to 1Y Mean Target Price (1Y Ago Lagged)")
stats_df <- move_col_after(stats_df, "Price Relative to 50D SMA", "Price Relative to 200D SMA")
stats_df <- move_col_after(stats_df, "Beta up to 3yr", "Price Relative to 50D SMA")
stats_df <- move_col_after(stats_df, "Since Jan 1998 Annualized Return {Sharpe}", "Since Inception/1980 Sharpe")
stats_df <- move_col_after(stats_df, "Since Jan 1998 Sharpe", "Since Jan 1998 Annualized Return {Sharpe}")
stats_df <- move_col_after(stats_df, "Lifetime Mean Daily Spread", "Monthly Spread > 10% (Last 6M)")
stats_df <- move_col_after(stats_df, "3M Mean Daily Spread", "Lifetime Mean Daily Spread")
stats_df <- move_col_after(stats_df, "3M Mean Weekly Spread", "3M Mean Daily Spread")
stats_df <- move_col_after(stats_df, "6M Mean Monthly Spread", "3M Mean Weekly Spread")

cat("Done.\n")


# Write output to file --------------------------------
cat("Writing results to: ",outfile,"...")
write.table(stats_df, outfile, sep=",", row.names=FALSE)
cat("Done.\n")
cat("\nAll done!\n")
