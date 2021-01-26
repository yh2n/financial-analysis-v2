# Download market cap data from Quandl - Zacks Market Cap


# Update log  ----------------------------------
# v1   Initial version


# Configuration --------------------------------
library(Quandl)


# Core -----------------------------------------
get_market_cap <- function(tickers, start, end, api_key=NULL) {
  # Returns:
  # list of data.frame of market_cap
  
  
  # download earnings data from quandl
  cat("\nDownloading market cap data from Yahoo Finance... ")
  df_list <- lapply(tickers, function(tk) {
    df <- getQuote(tk, what=yahooQF(c("Market Capitalization")))
    if(NROW(df)==0) warning(paste0(tk, " returned 0 rows of market cap data with the given date range."))
    return(df)
  })
  names(df_list) <- tickers
  cat("Done.\n\n")
  return(df_list)
}

