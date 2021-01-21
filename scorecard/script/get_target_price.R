# Download target price from Quandl - Zacks Target Price(ZTP)


# Update log  ----------------------------------
# v1    Initial version
# v2    renaming following tidyverse style guide
# v3    add get_last_target_price
# v4    add multiple use_col support for get_target_price
# v5    add get_all_target_price()
# v5.1  change return type of get_target_price to list only


# Configuration --------------------------------
library(Quandl)


# Core -----------------------------------------
get_target_price <- function(tickers, start, end, use_col="tp_mean_est", api_key=NULL){
  # call get_all_target_price()
  df_list <- get_all_target_price(tickers=tickers, start, end, api_key)
  
  # combine by type of tp
  cat("Processing... ")
  tp_list <- lapply(use_col, function(tp_col) {
    xts_list <- lapply(tickers, function(tk){
      df <- df_list[[tk]]
      res <- xts(df[, tp_col], order.by=df[, "obs_date"])
      colnames(res) <- tk
      return(res)
    })
    return(do.call(merge, xts_list))
  })
  names(tp_list) <- use_col
  cat("Done.\n\n")
  
  return(tp_list)
}

get_all_target_price <- function(tickers, start, end, api_key=NULL) {
  # pass api key
  if(is.null(api_key)) api_key <- "TQUMKUJHYiN1xGQHZxXT"
  Quandl.api_key(api_key)
  
  # download tp from quandl
  cat("\nDownloading target prices from Quandl - Zacks/TP... ")
  df_list <- lapply(tickers, function(tk) {
    df <- Quandl.datatable("ZACKS/TP", obs_date.gte=start, obs_date.lte=end, ticker=tk)
    if(NROW(df)==0) warning(paste0(tk, " returned 0 rows of TP data with the given date range."))
    return(df)
  })
  names(df_list) <- tickers
  cat("Done. ")
  return(df_list)
}

get_last_target_price <- function(tickers, start, end, use_col="tp_mean_est", api_key=NULL) {
  tp_list <- get_target_price(tickers=tickers, start=start, end=end, use_col=use_col, api_key=api_key)
  last_tp <- lapply(tp_list, function(tp) {
    if(length(tp) > 0) {
      return(sapply(tp, function(tk) {tail(na.omit(tk), 1)}))
    } else {
      return(NULL)
    }
  })
  return(last_tp)
}