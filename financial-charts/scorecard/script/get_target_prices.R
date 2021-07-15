# Download target price from source.

library(httr)
library(jsonlite)


FACTSET_CREDENTIALS <- read_json("./factset_secrets.json")


get_target_prices <- function(tickers, start, end, datasource="factset", credentials=NULL) {
  #' Wrapper function for downloading target prices from specified source.
  #'
  #' @param tickers Single or multiple tickers to download.
  #' @param start Start date of estimate period.
  #' @param end End date of estimate period.
  #' @param credentials Credentials for datasource.

  if (datasource != "factset") {
    stop("Currently factset is the only datasource available!")
  }
  
  cat("\nDownloading target prices from :", datasource, "... ")
  df_list <- get_target_prices_from_factset(tickers, start, end)
  cat("Done.\n")
  return(df_list)
}


get_target_prices_from_factset <- function(tickers, start, end,
                                           periodicity="NTMA", credentials=NULL) {
  #' Download target prices from FactSet Estimates API.
  #' 
  #' @param tickers Single or multiple tickers to download.
  #' @param start Start date of estimate period.
  #' @param end End date of estimate period.
  #' @param periodicity The periodicity for the estimates requested.
  #'   ANN - Annual
  #'   QTR - Quarterly
  #'   SEMI - Semi-Annual
  #'   NTMA - Next-Twelve-Months - Time-weighted Annual.
  #'   LTMA - Last-Twelve-Months - Time-weighted Annual.
  #' @param credentials A named list with keys "username" & "apikey" that 
  #'   contains FactSet username and apikey. Use default creds if not provided.
  #'   
  #' @return List of df that contains target prices for tickers, where
  #'   each data.frame contains results for one ticker.
  
  # convert single string to list of string
  if (is.character(tickers) & length(tickers) == 1) {
    tickers <- list(tickers)
  }
  
  # credentials
  if (is.null(credentials)) {
    credentials <- FACTSET_CREDENTIALS
  }
  username <- credentials$username
  apikey <- credentials$apikey
  
  # Create a request object and set the parameters
  rolling_endpoint <- "https://api.factset.com/content/factset-estimates/v2/rolling-consensus"
  rolling_request <- list(ids=tickers,
                          relativeFiscalStart=1,
                          relativeFiscalEnd=1,
                          periodicity=periodicity,
                          metrics=list("PRICE_TGT"),
                          currency="USD",
                          startDate=start,
                          endDate=end,
                          frequency="D")
  # Pull data
  rolling_response  <- POST(rolling_endpoint,
                            authenticate(username, apikey, type="basic"),
                            body=(rolling_request),
                            add_headers(Accept='application/json'),
                            encode='json')
  # Convert the post response -> character dataset -> Json -> df
  rolling_char <- rawToChar(rolling_response$content)
  rolling_json <- fromJSON(rolling_char)[['data']]
  rolling_df <- as.data.frame(rolling_json)
  
  # Clean out negative prices
  rolling_df <- rolling_df[rolling_df$low >= 0, ]
  
  # Split df by ticker
  df_list <- lapply(tickers, function(tk) {
    df_tk <- rolling_df[rolling_df$requestId == tk, ]
    if (NROW(df_tk) == 0) {
      warning(tk, " returned 0 rows of Target Price data with given date range [", start, ", ", end, "].")
    }
    return(df_tk)
  })
  names(df_list) <- tickers
  return(df_list)
}


find_last_target_prices <- function(target_prices, n_lag=0, use_cols="mean") {
  #' Find last observation of target prices by given type(s).
  #' 
  #' @param target_prices List of target prices df downloaded by get_target_prices().
  #' @param n_lag Number of lags in trading days.
  #'   E.g. n_lag=1 means shifting the value at t-1 to t. Default as 0(no lag).
  #' @param  use_cols Columns to use in original target_prices df.
  #' 
  #' @return List of vectors. Each list correspond to a given type(use_cols).
  #'   Each vector contains last obsv(s) of corresponding tp type for
  #'   each ticker.
  last_tp_list <- lapply(use_cols, function(col) {
    last_tp <- sapply(target_prices, function(df) {
      if(NROW(df) > n_lag) {
        tp_xts <- xts(df[[col]], order.by=as.Date(df$estimateDate))
        tp_xts <- lag(tp_xts, n_lag)
        return(as.numeric(tail(na.omit(tp_xts), 1)))
      } else {
        return(NA_real_)
      }
    })
  })
  names(last_tp_list) <- use_cols
  return(last_tp_list)
}
