# Calculate lookback beta to benchmark.

calc_benchmark_beta <- function(returns, lookback, tickers, benchmark,
                                min_obsv=10, pos_only=FALSE, neg_only=FALSE) {
  #' Calculate lookback beta to benchmark.
  #'
  #' @param returns (xts) Returns series for tickers and benchmark.
  #' @param lookback (int) Length of lookback.
  #' @param tickers (str or iterable of str) Tickers that are used to calculate beta.
  #' @param benchmark (str) The benchmark tickers regressing against.
  #' @param min_obsv (int) The minimum number of non-NA observations required to
  #'   calculate beta. Lower than that will return NA instead. Default to be 10.
  #' @param pos_only (bool) If TRUE, will only use periods where benchmark had
  #'   positive returns. Cannot be TRUE at the same time with neg_only.
  #' @param neg_only (bool) If TRUE, will only use periods where benchmark had
  #'   negative returns. Cannot be TRUE at the same time with pos_only.
  r_lb <- tail(returns, lookback)
  if (pos_only && neg_only) {
    stop("Only one of pos_only and neg_only can be TRUE!")
  }
  if (pos_only) {
    r_lb <- r_lb[r_lb[, benchmark] > 0, ]
  } else if (neg_only) {
    r_lb <- r_lb[r_lb[, benchmark] < 0, ]
  }
  betas <- sapply(r_lb[, tickers], function(rtn) {
    y <- r_lb[, benchmark]
    x <- rtn
    if (length(na.omit(rtn)) >= 10) {
      fit <- lm(y ~ x, na.action=na.omit)
      return(summary(fit)$coefficients[2, 1])
    } else {
      return(NA_real_)
    }
  })
  return(betas)
}