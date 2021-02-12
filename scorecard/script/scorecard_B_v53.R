# This script collects statistics for baskets of given lists of tickers. 
# Prices are downloaded from selected sources if not found locally.

# Update log  ----------------------------------
# 5B - v5 for basket
# 6B corrected for missig prices
# 7B changed output
# 7BW based on 7B but with predefined weights.
# 7BN multiple and external basket+weights inputs; multiple outputs to file; fixed bugs
# 8BN use correlation of returns instead of prices; magic corrLen
# 9N  added rank by sharpe to output, fixed bugs, rounded to 2 decimals
# 10N added sum of Sharpe to output; sort by sum of Sharpe
# 11BN ranks dropped
# 12BN added 2Y sharpe to sharpe sum; added column "Distance from 52-Week High"
# 13BN rearrange columns
# 15BN added distance(SD) to 50D SMA; rearrange output
# 16BN added 6M, 1Y stats; changed column names
# 17BN added 1M stats; added new column "Average Annulalized Return" for selected lookbacks
# 18BN using RQuantLib for YTD calculation
# 19BN change to equal weight
# 20BN in "Average Annulalized Return": added 5Y to lookbacks and added "Avg. Monthly Return"; 
#      use whatever traded tickers in basket(allow partially trading) 
# 21BN read basket name and output to results
# 22BN added return and sharpe for period "Since Jun 2002"
# 23BN added 5Y to Sum of Sharpes
# 24BN added stats for 10Y and 15Y; removed 1M from Sum of Sharpes and Avg of Returns;
#      rearranged output as: 15Y, 10Y, 1Y, 5Y, 2Y, 6M, 3M, 1M, 2W, YTD, Since 2002
# 25BN dropped YTD; keep annualized numbers only for periods longer than 1Y; 
#      rearranged output as: 15Y, 10Y, 5Y, 1Y, 2Y, 6M, 3M, 1M, 2W, Since 2002
# 26BN added 2 place holders for stats period 'since inception'
# 27BN added stats period 'since inception'
# 28BN replaced 2Y with 3Y; changed sum of sharpe to avg of sharpe;
#      use non-NA values to calc avg of sharpe and append [available years] to value;
#      use 3M, 6M, 1Y, 3Y, 5Y, 10Y, 15Y for Sharpe calc
#      sort output by Avg Sharpe
#      rearranged output as: 15Y, 10Y, 5Y, 3Y, 1Y, 6M, 3M, 1M, Since 2002
#      dropped corr tk: AMZN, NFLX; moved QQQcorrelation after SPYcorrelation
#      for period > 1Y, merge ann return and sharpe to one column
# change on 27DD 
#      for '1Y' dropped total return, then merge ann return and sharpe to one column
# 29BN split the sharpe ratios with the returns
# 30BN merge the sharpe with the returns, but do not drop the sharpe in order to color sheet
# 31BN removed 'basket' mentions in output; misc output formatting; added Alec Path config
# 32BN rank by 1Y Sharpe
# 32BNW use baskets input with weights
# 33BNW rank by Avg Sharpe
# 34BNW merge sharpe with return for periods < 1Y, but do not drop the sharpe in order to color;
#       use {sharpe}
# 35BNW change "since 2002" to "since 1998"
# 36BNW drop 3M in Avg Sharpe and Avg Annualized Rtn calc
# 37BNW geometric average for periods higher than one year
# 38BNW added column "Growth of $100 since 1998, USD"(PV of $100 invested in 1998)
#       fixed bug in annualized return calculation with geometric average
# 39BNW merged "Growth" column with "Ann Return" column
# 40BNW use as many early-started tickers as possible to represent whole basket(for all lb)
# 41BNW new format for dollar output(add thousand separator, round to ones place)
# 42BNW move stats for "since inception" before "since 1998"; use arithmetic mean for "since inception"
# v43   rename script "scorecard_B_[version]"; use ReadBaskets() to read baskets.
# v44   add "Distance from 1Y Mean Target Price";
#       fix weighted basket bug;
# v45   use new get_prices.R (v3.6)
# v46   remove basket index column
# v47   append "Avg Aharpe" to "Avg Rtns" and drop "Avg Sharpe"
# v48   fix bug when there is shorting weight in basket; change output filename
# v49   remove "(average monthly return)" from "Average Ann return"
# v50   remove "Moat"; add column "Price Target Envelope"(with new get_target_price())
# v51   redefine "distance"("with respect to target" -> "with respect to current price");
#       break column "price target envelope"
# v52   use new get_prices.R (v4.0)
#       add column "Price Relative to 200D SMA"
# v53   add columns "EPS", "Distance to 1Y Mean -1SD Target Price", "Distance to 1Y Mean +1SD Target Price",
#       "Distance to 1Y Mean Target Price (1Y Ago)";
#       change column "Price Target Envelope (Median)" name to "Distance to 1Y Median Target Price";
#       remove columns "Price Target Envelope (Low)", "Price Target Envelope (High)"
#       rearrange columns
#       use new get_target_price.R (v5.1)


# Configuration --------------------------------
rm(list = ls())
ALEC_PATH <- FALSE
SCRIPT_PATH <- ifelse(ALEC_PATH, "C:/R/", "./script/")
BASKET_PATH <- ifelse(ALEC_PATH, "C:/kensho/scorecard/", "../data/baskets/")
PRICEFILE_PATH <- ifelse(ALEC_PATH, "C:/kensho/basket/", "./data/")
OUTPUT_PATH <- ifelse(ALEC_PATH, "C:/kensho/scorecard/", "./output/")

library(quantmod)
library(RQuantLib)
source(paste0(SCRIPT_PATH, "get_prices.R"))
source(paste0(SCRIPT_PATH, "get_target_price.R"))
source(paste0(SCRIPT_PATH, "get_earnings.R"))
source(paste0(SCRIPT_PATH, "format_number.R"))
source(paste0(SCRIPT_PATH, "move_col_after.R"))
source(paste0(SCRIPT_PATH, "read_baskets.R"))


# Inputs ---------------------------------------
version <- "v53"
datasource <- "Y" #K:Kensho  G:Google

# parameters
baskets_file <- "scorecard_new_vista_B"
weighted <- FALSE
start_date <- "1980-01-01"
end_date <- "2020-06-26"
corr_tks <- c("SPY", "QQQ")
corrLength <- 756
ytd <- businessDaysBetween("UnitedStates/NYSE", as.Date("2019-01-01"), as.Date(end_date))
jan1998_td <- businessDaysBetween("UnitedStates/NYSE", as.Date("1998-01-01"), as.Date(end_date))
lookbacks <- c(10, 21, 3*21, 6*21, 1*252, 3*252, 5*252, 10*252, 15*252, jan1998_td)
names(lookbacks) <- c("2W", "1M", "3M", "6M", "1Y", "3Y", "5Y", "10Y", "15Y", "Since Jan 1998")
lookbacks <- lookbacks[c("15Y", "10Y", "5Y", "3Y", "1Y", "6M", "3M", "1M", "Since Jan 1998")]
period_gteq_1y <- c("15Y", "10Y", "5Y", "3Y", "1Y", "Since Jan 1998", "Since Inception/1980")
period_less_1y <- names(lookbacks)[!(names(lookbacks) %in% period_gteq_1y)]


# Data -----------------------------------------
# read baskets (and weights if applicable)
bskt_n_wts_list <- read_baskets(paste0(BASKET_PATH, baskets_file, ".csv"), weighted=weighted)
basket_list <- bskt_n_wts_list[[1]]
weight_list <- bskt_n_wts_list[[2]]

# all tickers
all_tks <- unique(c(unlist(basket_list), corr_tks))

# download prices
pricefile_name <- paste("prc", baskets_file, start_date, end_date, datasource, sep="_")
pricefile <- paste0(PRICEFILE_PATH, pricefile_name, ".csv")
if(file.exists(pricefile)) {
  prices <- read.zoo(pricefile, sep=",", index=1, header=TRUE, check.names=FALSE)
  prices <- as.xts(prices)
  cat("Prices read from existing file ",pricefile, "\n\n")
} else {
  cat("Cannot find existing price file.\n")
  prices <- get_prices(tickers=all_tks, start=start_date, end=end_date, datasource=datasource, outfiles=pricefile)
}

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

# Calc returns
r_daily <- lapply(prices, function(tk) {
  temp <- dailyReturn(tk)
  colnames(temp) <- names(tk)
  temp
})
r_daily <- do.call(merge, r_daily)


# Calc Inception to date(life_to_date)
basket_inceptions <- sapply(basket_list, function(basket){
  inceptions <- sapply(prices[, basket], function(tk){min(which(!is.na(tk)))})
  return(max(inceptions))
})
life_to_date <- NROW(prices) - basket_inceptions
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
cor_colnames <- paste0(corr_tks,"correlation")

stats_colnames <- c(
  "Weighting", 
  as.vector(rbind(rtn_colnames, rtn_ann_colnames, sharpe_colnames)), 
  cor_colnames
)
stats_colClasses <- c(
  "character",
  rep("numeric", length(c(rtn_colnames, rtn_ann_colnames, sharpe_colnames))),
  rep("numeric", length(cor_colnames))
)
stats_df <- read.csv(
  text="", col.names=stats_colnames, colClasses=stats_colClasses, check.names=FALSE
)


# Backtesting for every basket&weights ----------------
for(i in seq(basket_list)) {
  cap_init <- 100
  tks <- basket_list[[i]]
  wts <- weight_list[[i]]
  
  R <- rep(0, length(lookbacks))
  Ra <- rep(0, length(lookbacks))
  Sh <- rep(0,length(lookbacks))
  
  correl <- rep(0, length(corr_tks))
  
  lookbacks_i <- c(lookbacks, life_to_date[i])
  names(lookbacks_i)[length(lookbacks_i)] <- "Since Inception/1980"
  
  for(j in seq(lookbacks_i)) {
    lb_start <- (NROW(prices) - lookbacks_i[j]) # before lookback period
    if(lb_start <= 0) stop(paste0("Data length shorter than lookback: ", names(lookbacks_i[j]), ": ", lookbacks_i[j]))

    # use trading tks to represent whole basket for the lookback
    traded_tks <- tks[!is.na(prices[lb_start, tks])]
    traded_tks_weights <- wts[traded_tks] / sum(abs(wts[traded_tks]))
    
    if(length(traded_tks) > 0) {
      if(all(traded_tks_weights > 0)) { 
        # handling for non-shorting
        shares <- cap_init * traded_tks_weights / prices[lb_start, traded_tks]
        cap_d <- prices[lb_start:NROW(prices), traded_tks] %*% t(shares)
        pnl_d <- diff(cap_d)
      } else { 
        # handling for shorting exist
        long_tks_w <- traded_tks_weights[traded_tks_weights > 0]
        long_cap_init <- cap_init * sum(long_tks_w)
        long_shares <- long_cap_init * long_tks_w / prices[lb_start, names(long_tks_w)]
        long_cap_d <- prices[lb_start:NROW(prices), names(long_tks_w)] %*% t(long_shares)
        long_pnl_d <- diff(long_cap_d)
        
        short_tks_w <- abs(traded_tks_weights[traded_tks_weights < 0])
        short_cap_init <- cap_init * sum(short_tks_w)
        short_shares <- short_cap_init * short_tks_w / prices[lb_start, names(short_tks_w)]
        short_cap_d <- prices[lb_start:NROW(prices), names(short_tks_w)] %*% t(short_shares)
        short_pnl_d <- -diff(short_cap_d)
        
        cap_d <- long_cap_d + short_cap_d
        pnl_d <- long_pnl_d + short_pnl_d
      }
      pnl_tot <- cap_d[length(cap_d)] - cap_d[1]
      rtn_d <- pnl_d / cap_d[-length(cap_d)]
      rtn_tot <- pnl_tot / cap_init
      vol <- sd(rtn_d)
      
      R[j] <- rtn_tot
      
      # annualized(arithmetic mean) return and sharpe
      Ra[j] <- as.numeric(R[j])*252/lookbacks_i[j]  #ann
      Sh[j] <- as.numeric(R[j])/(as.numeric(vol)*lookbacks_i[j])*sqrt(252)
      
      # use geometric mean to annualize for periods longer than 1Y except for "since inception"
      if(lookbacks_i[j] > 252 & names(lookbacks_i[j]) != "Since Inception/1980") {
        Ra[j] <- (1+R[j])^(252/lookbacks_i[j]) - 1
        Sh[j] <- mean(rtn_d) / vol * sqrt(252)
      }
    } else {
      R[j] <- NA
      Ra[j] <- NA
      Sh[j] <- NA
    }
    if(length(rtn_d) > 0)
      curr_bpr <- rtn_d
  }
  
  # Combine results
  R_Ra_Sh <- as.vector(rbind(R, Ra, Sh))
  
  # Calc correlations
  n_rtn <- length(curr_bpr)
  for(m in 1:length(corr_tks)) {
    tk <- corr_tks[m]
    if(n_rtn > 10) {
      if(n_rtn > corrLength){
        x <- curr_bpr[(n_rtn-corrLength+1):n_rtn]
        y <- r_daily[(NROW(r_daily)-corrLength+1):NROW(r_daily), tk]
      } else {
        x <- curr_bpr
        y <- r_daily[(NROW(r_daily)-n_rtn+1):NROW(r_daily), tk]
      }
      correl[m] <- cor(x=x, y=y, use="pairwise.complete.obs")
    } else {
      correl[m] <- NA
    }
  }
  
  # save results to dataframe
  if(weighted) {
    bskt_weighting_str <- paste(paste(toDecimalPlaces(wts, 2), tks, sep="*"), collapse=" + ")
  } else {
    bskt_weighting_str <- paste(tks, collapse=" + ")
  }
  if(names(basket_list[i])!="Basket") {
    bskt_weighting_str <- paste0(names(basket_list[i]), ": ", bskt_weighting_str)
  }
  
  stats_k_df <- data.frame(
    c(
      list(bskt_weighting_str),
      as.list(R_Ra_Sh),
      as.list(correl)
    ), 
    stringsAsFactors=F)
  colnames(stats_k_df) <- stats_colnames
  stats_df <- rbind(stats_df, stats_k_df)
}

# Add dist to target prices
calc_bskt_dist_to_goal <- function(p, goal) {
  if(!is.null(goal)) {
    dist_to_goal <- sapply(seq(basket_list), function(i) {
      bskt <- basket_list[[i]]
      wts <- weight_list[[i]]
      bskt_goal <- goal[bskt] %*% wts
      bskt_cp <- last(p)[, bskt] %*% wts
      return(bskt_goal / bskt_cp - 1)
    })
    return(dist_to_goal)
  } else {
    return(NA)
  }
}
stats_df[, "Distance to 1Y Mean Target Price"] <- toPercent(calc_bskt_dist_to_goal(prices, tp_mean), plus_sign=TRUE)
stats_df[, "Distance to 1Y Median Target Price"] <- toPercent(calc_bskt_dist_to_goal(prices, tp_median), plus_sign=TRUE)
stats_df[, "Distance to 1Y Mean -1SD Target Price"] <- toPercent(calc_bskt_dist_to_goal(prices, tp_mean - tp_sd), plus_sign=TRUE)
stats_df[, "Distance to 1Y Mean +1SD Target Price"] <- toPercent(calc_bskt_dist_to_goal(prices, tp_mean + tp_sd), plus_sign=TRUE)
stats_df[, "Distance to 1Y Mean Target Price (1Y Ago Lagged)"] <- toPercent(calc_bskt_dist_to_goal(prices, tp_mean_lag), plus_sign=TRUE)

# Add max_lookbacks to df
stats_df[, "max_lookbacks"] <- max_lookbacks

# Add inception_year to df
stats_df[, "inception_year"] <- sapply(basket_inceptions, function(incp_idx) {
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
stats_df[, "Distance to 52-Week High"] <- toPercent(calc_bskt_dist_to_goal(prices, high_price_52_week), plus_sign=TRUE)

# calc distance in SD from SMA
calc_bskt_dist_from_sma_in_sd <- function(p, w_list, sma_len) {
  sd_from_sma <- sapply(w_list, function(wts){
    p_sma <- tail(p[, names(wts)], sma_len) %*% wts
    (last(p_sma)-mean(p_sma)) / sd(p_sma)
  })
  # format
  sapply(sd_from_sma, function(i){
    res <- paste0(toDecimalPlaces(i, 2, plus_sign=TRUE), " SD")
    if(i>=0) return(paste0("'", res)) else return(res) # add ' to prevent treated as formula by google sheet 
  })
}
stats_df[, "Price Relative to 200D SMA"] <- calc_bskt_dist_from_sma_in_sd(prices, weight_list, 200)
stats_df[, "Price Relative to 50D SMA"] <- calc_bskt_dist_from_sma_in_sd(prices, weight_list, 50)

# Add EPS
stats_df[, "EPS"] <- sapply(weight_list, function(wts) {
  if(length(earnings[names(wts)]) == length(wts)) {
    toDecimalPlaces(wts %*% earnings[names(wts)], 2)
  } else {
    NA
  }
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
# stats_df <- stats_df[, !(names(stats_df) %in% paste0(rtn_period_gteq_1y, " Sharpe"))]

# add some empty columns
stats_df[, c(
  "Beta up to 3yr",
  "N 20+days UP Periods",
  "Beta Up",
  "N 20+days DOWN Periods",
  "Beta Down",
  "Up/QQQ down"
)] <- NA

# rearrange columns
stats_df <- move_col_after(stats_df, "Avg Annualized Returns", "Weighting")
stats_df <- move_col_after(stats_df, "EPS", "Avg Annualized Returns")
stats_df <- move_col_after(stats_df, "Distance to 52-Week High", "EPS")
stats_df <- move_col_after(stats_df, "Distance to 1Y Mean Target Price", "Distance to 52-Week High")
stats_df <- move_col_after(stats_df, "Distance to 1Y Median Target Price", "Distance to 1Y Mean Target Price")
stats_df <- move_col_after(stats_df, "Distance to 1Y Mean -1SD Target Price", "Distance to 1Y Median Target Price")
stats_df <- move_col_after(stats_df, "Distance to 1Y Mean +1SD Target Price", "Distance to 1Y Mean -1SD Target Price")
stats_df <- move_col_after(stats_df, "Distance to 1Y Mean Target Price (1Y Ago Lagged)", "Distance to 1Y Mean +1SD Target Price")
stats_df <- move_col_after(stats_df, "Price Relative to 200D SMA", "Distance to 1Y Mean Target Price (1Y Ago Lagged)")
stats_df <- move_col_after(stats_df, "Price Relative to 50D SMA", "Price Relative to 200D SMA")
stats_df <- move_col_after(stats_df, "SPYcorrelation", "Price Relative to 50D SMA")
stats_df <- move_col_after(stats_df, "QQQcorrelation", "SPYcorrelation")
stats_df <- move_col_after(stats_df, "Beta up to 3yr", "QQQcorrelation")
stats_df <- move_col_after(stats_df, "Since Jan 1998 Annualized Return {Sharpe}", "Since Inception/1980 Sharpe")
stats_df <- move_col_after(stats_df, "Since Jan 1998 Sharpe", "Since Jan 1998 Annualized Return {Sharpe}")


# Write output to file --------------------------------
# outfile
outfile_name <- paste("score_B", baskets_file, start_date, end_date, datasource, version, sep="_")
outfile <- paste0(OUTPUT_PATH, outfile_name, ".csv")

# write to file
write.table(stats_df, outfile, sep=",", row.names=F)