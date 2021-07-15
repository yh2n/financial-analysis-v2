# Download earnings data from Quandl - Zacks Earnings Surprises(ZES)


# Configuration --------------------------------
library(Quandl)
library(RQuantLib)


# Functions -----------------------------------------
get_earnings <- function(tickers, start, end, api_key=NULL, adj_pre_mkt=FALSE) {
  #' Download earnings from Quandl - Zacks/ES
  #' 
  #' @param tickers Iterable of tickers to download.
  #' @param start The start date for filtering earnings period (per_end_date).
  #' @param end The end date for filtering earnings period (per_end_date).
  #' @param api_key Quandl API key. If not provided will use default key.
  #' @param adj_pre_mkt If `TRUE`, pre-market earnings events  will be 
  #'   adjusted to one-trading-day prior after-market. Default to be `FALSE`
  #' @return list of data.frame of earnings
  
  # pass api key
  if (is.null(api_key)) api_key <- "TQUMKUJHYiN1xGQHZxXT"
  Quandl.api_key(api_key)
  
  # download earnings data from quandl
  cat("\nDownloading earnings data from Quandl - Zacks/ES... ")
  df_list <- lapply(tickers, function(tk) {
    df <- Quandl.datatable("ZACKS/ES", per_end_date.gte=start, per_end_date.lte=end, ticker=tk)
    if (NROW(df) == 0) {
      warning(tk, " returned 0 rows of Earnings data with given date range [", start, ", ", end, "].")
    }
    return(df)
  })
  names(df_list) <- tickers
  
  if (adj_pre_mkt) {
    cat("Adjusting pre-market to after-market...")
    df_list <- adjust_pre_market_multiple_tickers(df_list)
  }
  
  cat("Done.\n")
  return(df_list)
}


adjust_pre_market_multiple_tickers <- function(earnings_list) {
  #' Adjust pre-market earnings events to one-trading-day prior after-market
  #'   for multiple tickers
  #' @param earnings_list List of earnings data.frames.
  #' @return List of adjusted earnings data.frames.
  
  lapply(earnings_list, adjust_pre_market_single_ticker)
}


adjust_pre_market_single_ticker <- function(earnings) {
  #' Adjust pre-market earnings events to one-trading-day prior after-market
  #'   for multiple tickers
  #' @param earnings Earnings data.frame.
  #' @return Adjusted earnings data.frame.
  
  idx <- which(earnings[, "act_rpt_desc"] == "BEFOR")
  if(length(idx) > 0) {
    earnings[idx, "act_rpt_date"] <- earnings[idx, "act_rpt_date"] - 1
    for(i in idx) {
      while(!isBusinessDay("UnitedStates/NYSE", earnings[i, "act_rpt_date"])) {
        earnings[i, "act_rpt_date"] <- earnings[i, "act_rpt_date"] - 1
      }
      earnings[i, "act_rpt_desc"] <- "AFTER"
    }
  }
  return(earnings)
}
