# Download earnings data from Quandl - Zacks Earnings Surprises(ZES)


# Update log  ----------------------------------
# v1   Initial version


# Configuration --------------------------------
library(Quandl)


# Core -----------------------------------------
get_earnings <- function(tickers, start, end, api_key=NULL) {
  # Returns:
  # list of data.frame of earnings
  
  # pass api key
  if(is.null(api_key)) api_key <- "TQUMKUJHYiN1xGQHZxXT"
  Quandl.api_key(api_key)
  
  # download earnings data from quandl
  cat("\nDownloading earnings data from Quandl - Zacks/ES... ")
  df_list <- lapply(tickers, function(tk) {
    df <- Quandl.datatable("ZACKS/ES", per_end_date.gte=start, per_end_date.lte=end, ticker=tk)
    if(NROW(df)==0) warning(paste0(tk, " returned 0 rows of earnings data with the given date range."))
    return(df)
  })
  names(df_list) <- tickers
  cat("Done.\n\n")
  return(df_list)
}

