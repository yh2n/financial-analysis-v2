# Calculate Basket Return Series


# Update log  ----------------------------------
# v1   Initial version
# v2   replace NaN with NA for the case of wts=NULL & use_partial=TRUE
# v3   renaming following tidyverse style guide
# v4   add abs() when normalizing weights to adapt for shorting situation


# Configuration --------------------------------
library(quantmod)


# Core -----------------------------------------
calculate_basket_returns <- function(r, tks, wts=NULL, use_partial=FALSE) {
  # Return basket r series given constituents returns and weights.
  #
  # Args:
  #   r: (xts) the return series.
  #   tks: (str) the constituents tickers.
  #   wts: (numeric) vector of weights of the constituents, or NULL(same as FALSE)
  #   use_partial: (bool) If TRUE, basket will use currently available(started trading) tickers 
  #                to represent the complete basket. Tickers will be added when they start trading.
  #
  # Returns:
  #   a single xts series of the basket r.
  
  if(!use_partial) {
    if(is.null(wts) || !wts) {
      return(xts(rowMeans(r[, tks], na.rm=FALSE), order.by=index(r)))
    } else {
      return(xts(r[, tks] %*% (wts / sum(abs(wts))), order.by=index(r)))
    }
  } else {
    if(is.null(wts) || !wts) {
      res <- xts(rowMeans(r[, tks], na.rm=TRUE), order.by=index(r))
      res[is.nan(res)] <- NA # replace NaN with NA
      return(res)
    } else {
      # calculate basket return on each date
      bskt_rtns <- apply(r[, tks], MARGIN=1, function(i) {
        # use trading tickers only
        traded_tks_idx <- which(!is.na(i))
        if(length(traded_tks_idx)==0) {
          return(NA_real_)
        } else {
          # recalculate weights
          traded_tks_wts <- wts[traded_tks_idx] / sum(abs(wts[traded_tks_idx]))
          return(i[traded_tks_idx] %*% traded_tks_wts)
        }
      })
      return(xts(bskt_rtns, order.by=index(r)))
    }
  }
}

