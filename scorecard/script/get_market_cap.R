# Download market cap data from Yahoo Finance


# Update log  ----------------------------------
# v1   Initial version


# Configuration --------------------------------
library(quantmod)


# Core -----------------------------------------
get_market_cap <- function(tickers, start, end, api_key=NULL) {
  # Returns:
  # list of data.frame of market_cap
  
  # download earnings data from yahoo
  cat("\nDownloading market cap data from Yahoo Finance... ")
  df_list <- lapply(tickers, function(tk) {
    # replace "." with "-" according to yahoo symbology
    tk <- gsub("[.]", "-", tk)
    df <- getQuote(tk, what=yahooQF(c("Market Capitalization")))
    if(NROW(df) == 0) {
      warning(tk, " returned 0 rows of market cap data with given date range [", start, ", ", end, "].")
    }
    return(df)
  })
  names(df_list) <- tickers
  cat("Done.\n\n")
  return(df_list)
}

