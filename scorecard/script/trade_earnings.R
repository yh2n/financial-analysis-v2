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

tickers <- c("SHOP", "ROKU", "SPWR")
start <- "2011-01-01"
end <- "2021-02-12"
entry_exit <- list(c(0, 1), c(0, 5))
names(entry_exit) <- sapply(entry_exit, function(i){paste0("(", i[1], ", ", i[2], ")")})


# Data -----------------------------------------
# download prices(extra 1M before and after earning start and end)
prices <- get_all_prices(tickers, as.Date(start) - 30, as.Date(end) + 30)

# download earnings
earnings <- get_earnings(tickers, start, end)
# handling for before-mkt reports: treat them as after-market from 1D before
earnings_dates <- lapply(earnings, function(e) {
  idx <- which(e[, "act_rpt_desc"] == "BEFOR")
  if(length(idx) > 0) {
    e[idx, "act_rpt_date"] <- e[idx, "act_rpt_date"] - 1
    for(i in idx) {
      while(!isBusinessDay("UnitedStates/NYSE", e[i, "act_rpt_date"])) {
        e[i, "act_rpt_date"] <- e[i, "act_rpt_date"] - 1
      }
    }
  }
  return(e[, "act_rpt_date"])
})


# Core ----------------------------------------
strategy <- function(p, e, entry_exit, latest_first=TRUE) {
  # convert index type from POSIXct to Date
  index(p) <- as.Date(index(p))
  
  # for each entry_exit pair, return a list of returns on earnings dates
  rtns_list <- lapply(entry_exit, function(i) {
    # current entry & exit offset
    entry_offset <- i[1]
    exit_offset <- i[2]
    
    # calculate return for each earnings date
    rtns <- sapply(e, function(e_i) {
      # find out index of earnings date in price series
      earnings_date_idx <- which(index(p) == e_i)
      if(length(earnings_date_idx) == 0) {
        return(NA_real_)
      }
      entry_idx <- earnings_date_idx + entry_offset
      exit_idx <- earnings_date_idx + exit_offset
      if(entry_idx < 1 || exit_idx > NROW(p))  {
        return(NA_real_)
      }
      return(as.numeric(Cl(p)[exit_idx]) / as.numeric(Cl(p)[entry_idx]) - 1)
    })
    
    # calculate calendar days to recover
    #   N=-1: never recovered, 
    #   N=0: already in profit, 
    #   N>1: actual days to recover
    calc_d2recover <- function(f) {
      sapply(seq(rtns), function(j) {
        if(is.na(rtns[j])) {
          return(NA_integer_)
        }
        if(rtns[j] >= 0) {
          return(0)
        }
        entry_idx <- which(index(p) == e[j]) + entry_offset
        exit_idx <- which(index(p) == e[j]) + exit_offset
        p_entry <- as.numeric(Cl(p)[entry_idx])
        recover_idx <- which(f(p)[(exit_idx+1):NROW(p)] > p_entry)
        if(length(recover_idx) == 0) {
          return(-1)
        }
        return(as.numeric(index(p[exit_idx + min(recover_idx)]) - index(p[exit_idx])))
      })
    }
    cal_days_to_recover_cl <- calc_d2recover(Cl)
    cal_days_to_recover_hi <- calc_d2recover(Hi)
    
    return(list(rtns, cal_days_to_recover_cl, cal_days_to_recover_hi))
  })
  
  # create result data.frame
  df <- data.frame(e, rtns_list, stringsAsFactors=FALSE)
  colnames(df) <- c("Earnings Date",
                    sapply(names(entry_exit), function(i){
                      c(i, paste0("cal_days_to_recover_cl", i), paste0("cal_days_to_recover_hi", i))})
                    )
  if(latest_first) df <- df[dim(df)[1]:1, ] # reverse df if latest_first==TRUE
  return(df)
}

# run through all tickers
cat("Calculating... ")
summary_table <- sapply(tickers, function(tk) {
  # calc returns for tk
  df <- strategy(p=prices[[tk]], e=earnings_dates[[tk]], entry_exit=entry_exit, latest_first=TRUE)
  
  # calc summary statistics for each entry_exit pair
  stats_list <- lapply(names(entry_exit), function(i) {
    holding_len <- entry_exit[[i]][2] - entry_exit[[i]][1]
    min_rtn <- min(df[, i], na.rm=TRUE)
    max_rtn <- max(df[, i], na.rm=TRUE)
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
    res <- c(toDecimalPlaces(holding_len, 0),
             toPercent(min_rtn),
             toPercent(med_rtn),
             toPercent(med_pos_rtn),
             toPercent(max_rtn),
             toPercent(hit_rate),
             toDecimalPlaces(med_cal_days_to_recover_cl, 0),
             toDecimalPlaces(med_cal_days_to_recover_hi, 0),
             max_cal_days_to_recover_cl,
             max_cal_days_to_recover_hi
             )
    names(res) <- paste0(c("Holding Length",
                           "Min Return",
                           "Median Return",
                           "Median Return of Positive Trades",
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
  df[, names(entry_exit)] <- toPercent(df[, names(entry_exit)])
  
  df_output_path <- paste0(OUTPUT_PATH, paste("rtns", tk, start, end, version, sep="_"), ".csv")
  write.table(df, df_output_path, sep=",", row.names=FALSE)
  
  return(stats)
})

# Change row sequence: Group by entry exit for the following rows
target_rows <- c("Median Calendar Days to Recover(Negative Trades)(Cl)",
                  "Median Calendar Days to Recover(Negative Trades)(Hi)",
                  "Max Calendar Days to Recover(Negative Trades)(Cl)",
                  "Max Calendar Days to Recover(Negative Trades)(Hi)")
old_seq_idx <- as.vector(sapply(target_rows, grep, x=rownames(summary_table), fixed=TRUE))
new_seq <- as.vector(sapply(names(entry_exit), function(i){paste0(target_rows, " ", i)}))
summary_table[old_seq_idx, ] <- summary_table[new_seq, ]
rownames(summary_table)[old_seq_idx] <- new_seq

# write summary table to csv
summary_table_path <- paste0(OUTPUT_PATH, paste("summary", start, end, version, sep="_"), ".csv")
write.table(summary_table, summary_table_path, sep=",", row.names=TRUE, col.names=NA)

cat("Done. \nResults saved to directory: ", OUTPUT_PATH, "\n")