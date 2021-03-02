# Calculate performance for trading strategy:
#   Buy X days before/after earnings
#   Sell Y days before/after earnings
# Using data from Quandl - Zacks Earnings Surprises(ZES)


# Configuration --------------------------------
rm(list = ls())
SCRIPT_PATH <- "./script/"
BASKET_PATH <- "../data/baskets/"
PRICEFILE_PATH <- "../data/raw/"
OUTPUT_PATH <- "./output/"

source(paste0(SCRIPT_PATH, 'get_prices.R'))
source(paste0(SCRIPT_PATH, 'get_earnings.R'))
source(paste0(SCRIPT_PATH, 'format_number.R'))

library(RQuantLib)


# Inputs ---------------------------------------
version <- "v5"

basket <- "scorecard_single_ticker"
tickers <- read.table(paste0(BASKET_PATH, basket, ".csv"), header=FALSE, sep=",", stringsAsFactors=FALSE)[,1]


start <- "2018-01-01"
end <- "2020-12-31"
entry_exit_points <- list(c(-10, 0), c(-5, 0), c(-1, 0), c(0, 1), c(0, 5), c(0, 10))
names(entry_exit_points) <- sapply(entry_exit_points, function(i){paste0("(", i[1], ", ", i[2], ")")})


# Data -----------------------------------------
# download prices(extra 2M before and after earning start and end)
prices <- get_all_prices(tickers, as.Date(start) - 60, as.Date(end) + 60)

# download earnings
earnings <- get_earnings(tickers, start, end, adj_pre_mkt=TRUE)
earnings_dates <- lapply(earnings, function(e){e[, "act_rpt_date"]})


# Core ----------------------------------------
strategy <- function(prices, earnings_dates, entry_exit_points, latest_first=TRUE) {
  cols <- c(
    "Earnings Date",
    sapply(names(entry_exit_points), function(i) {
      c(i, paste0("cal_days_to_recover_cl", i), paste0("cal_days_to_recover_hi", i))
    })
  )
  if (length(earnings_dates) == 0) {
    df <- data.frame(matrix(ncol=length(cols), nrow=0))
  } else {
    # cast index type from POSIXct to Date
    index(prices) <- as.Date(index(prices))
    
    # for each entry_exit pair, return a list of returns on earnings dates
    rtns_list <- lapply(entry_exit_points, function(i) {
      # current entry & exit offset
      entry_offset <- i[1]
      exit_offset <- i[2]
      
      # calculate return for each earnings date
      rtns <- sapply(earnings_dates, function(e_i) {
        # find out index of earnings date in price series
        earnings_date_idx <- which(index(prices) == e_i)
        if(length(earnings_date_idx) == 0) {
          return(NA_real_)
        }
        entry_idx <- earnings_date_idx + entry_offset
        exit_idx <- earnings_date_idx + exit_offset
        if(entry_idx < 1 || exit_idx > NROW(prices))  {
          return(NA_real_)
        }
        return(as.numeric(Cl(prices)[exit_idx]) / as.numeric(Cl(prices)[entry_idx]) - 1)
      })
      
      # calculate calendar days to recover
      #   N=-1: never recovered, 
      #   N=0: already in profit, 
      #   N>1: actual days to recover
      calc_d2recover <- function(f) {
        if (length(rtns) == 0 || is.na(rtns)) {
          return(NA_integer_)
        }
        sapply(seq_along(rtns), function(j) {
          if (length(rtns[j]) == 0 || is.na(rtns[j])) {
            return(NA_integer_)
          }
          if (rtns[j] >= 0) {
            return(0)
          }
          entry_idx <- which(index(prices) == earnings_dates[j]) + entry_offset
          exit_idx <- which(index(prices) == earnings_dates[j]) + exit_offset
          if (exit_idx >= NROW(prices)) {
            return(-1)
          }
          p_entry <- as.numeric(Cl(prices)[entry_idx])
          recover_idx <- which(f(prices)[(exit_idx+1):NROW(prices)] > p_entry)
          if(length(recover_idx) == 0) {
            return(-1)
          }
          return(as.numeric(index(prices[exit_idx + min(recover_idx)]) - index(prices[exit_idx])))
        })
      }
      cal_days_to_recover_cl <- calc_d2recover(Cl)
      cal_days_to_recover_hi <- calc_d2recover(Hi)
      
      return(list(rtns, cal_days_to_recover_cl, cal_days_to_recover_hi))
    })
    
    df <- data.frame(earnings_dates, rtns_list, stringsAsFactors=FALSE)
  }
  
  names(df) <- cols
  if(latest_first) df <- df[dim(df)[1]:1, ] # reverse df if latest_first==TRUE
  return(df)
}

# run through all tickers
cat("Calculating... ")
summary_table <- sapply(tickers, function(tk) {
# for (tk in tickers) {
  print(tk)
  # calc returns for tk
  df <- strategy(prices=prices[[tk]], earnings_dates=earnings_dates[[tk]], entry_exit_points=entry_exit_points, latest_first=TRUE)
  
  # calc summary statistics for each entry_exit pair
  stats_list <- lapply(names(entry_exit_points), function(i) {
    n_obsv <- length(na.omit(df[, i]))
    holding_len <- entry_exit_points[[i]][2] - entry_exit_points[[i]][1]
    min_rtn <- min(df[, i], na.rm=TRUE)
    max_rtn <- max(df[, i], na.rm=TRUE)
    mean_rtn <- mean(df[, i], na.rm=TRUE)
    med_rtn <- median(df[, i], na.rm=TRUE)
    med_pos_rtn <- median(df[df[, i] > 0, i], na.rm=TRUE)
    hit_rate <- sum(df[, i] > 0, na.rm=TRUE) / length(na.omit(df[, i]))
    d2rec_cl <- df[, paste0("cal_days_to_recover_cl", i)]
    d2rec_hi <- df[, paste0("cal_days_to_recover_hi", i)]
    d2rec_neg_cl <- d2rec_cl[which(!is.na(d2rec_cl) & d2rec_cl > 0)]
    d2rec_neg_hi <- d2rec_hi[which(!is.na(d2rec_hi) & d2rec_hi > 0)]
    med_cal_days_to_recover_cl <- median(d2rec_neg_cl)
    med_cal_days_to_recover_hi <- median(d2rec_neg_hi)
    max_cal_days_to_recover_cl <- max(d2rec_neg_cl)
    max_cal_days_to_recover_hi <- max(d2rec_neg_hi)
    res <- c(toDecimalPlaces(n_obsv, 0),
             toPercent(min_rtn),
             toPercent(med_rtn),
             toPercent(med_pos_rtn),
             toPercent(mean_rtn),
             toPercent(max_rtn),
             toPercent(hit_rate),
             toDecimalPlaces(med_cal_days_to_recover_cl, 0),
             toDecimalPlaces(med_cal_days_to_recover_hi, 0),
             max_cal_days_to_recover_cl,
             max_cal_days_to_recover_hi
             )
    names(res) <- paste0(c("N Observations",
                           "Min Return",
                           "Median Return",
                           "Median Return of Positive Trades",
                           "Mean Return",
                           "Max Return",
                           "Hit Rate",
                           "Median Calendar Days to Recover(Negative Trades)(Cl)",
                           "Median Calendar Days to Recover(Negative Trades)(Hi)",
                           "Max Calendar Days to Recover(Negative Trades)(Cl)",
                           "Max Calendar Days to Recover(Negative Trades)(Hi)"
                           ), " ", i)
    return(res)
  })
  stats <- as.vector(do.call(rbind, stats_list))
  names(stats) <- as.vector(do.call(rbind, lapply(stats_list, names)))
  
  # format and write trade details to csv for tk
  df[, names(entry_exit_points)] <- toPercent(df[, names(entry_exit_points)])
  
  df_output_path <- paste0(OUTPUT_PATH, paste("rtns", tk, start, end, version, sep="_"), ".csv")
  write.table(df, df_output_path, sep=",", row.names=FALSE)
  
  return(stats)
})

# transpose
summary_table <- t(summary_table)

# write summary table to csv
summary_table_path <- paste0(OUTPUT_PATH, paste("summary", start, end, version, sep="_"), ".csv")
write.table(summary_table, summary_table_path, sep=",", row.names=TRUE, col.names=NA)

cat("Done. \nResults saved to directory: ", OUTPUT_PATH, "\n")
