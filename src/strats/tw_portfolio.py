import numpy as np


def n_day_periods(trading_days, offset, n):
    """Generate time periods of length n,
    using `trading_days` as the calendar.

    `offset` allows different period sets to be created from
    the same calendar, even if the period duration is the same.

    NOTE: In future, this may be better placed in a more general Portfolio
    utils file but so far has only been used for the TW portfolio.

    Parameters
    ----------
    trading_days : pd.DateTimeIndex
    offset : int
        Number of days after start of `trading_days` from which
        to begin generating the periods.
    n : int
        Duration of each period

    Returns
    -------
    list of slices of pd.Timestamps, each spanning `n` days according
    to `trading_days`
    """
    period_endpoints = range(0, len(trading_days), n)
    periods = zip(period_endpoints, period_endpoints[1:])
    periods = [
        slice(
            trading_days[offset + period[0]],
            trading_days[offset + period[1]])
        for period in periods
        if offset + period[1] < len(trading_days)]
    return periods


def corr_to_benchmarks(returns, bench_returns, window):
    """Calculate the rolling correlation of all stocks in the `returns`
    time series to each of the benchmarks in the `bench_returns`
    series.

    The result is in theory a 3D tensor, with dimensions
        (num_benchmarks) x (num_tickers) x (num_days)
    but is returned as a dictionary here for simplicity.

    Parameters
    ----------
    returns : pd.DataFrame
    bench_returns : pd.DataFrame
    window : int
        Size of the rolling window

    Returns
    -------
    dict
        with keys corresponding to each benchmark, and values the rolling
        correlations of all tickers in `returns` to that benchmark.
    """
    corrs = {}

    for name, series in bench_returns.items():
        corrs[name] = returns.rolling(window) \
            .corr(series.rolling(window))
    return corrs


def quantile_corr_criteria(corrs1, corrs2, quantile):
    """Calculates the quantile-based Third Way criteria, which requires that
    each qualifying stock has a magnitude of correlation to both value and
    growth based indexes less than some threshold.

    Parameters
    ----------
    corrs1 : pd.DataFrame
        Rolling correlations in time for each ticker to one of the
        benchmark indexes.
    corrs2 : pd.DataFrame
    quantile : int

    Returns
    -------
    pd.DataFrame
        of shape (num_trading_days, num_tickers). Each row is a boolean series
        of all tickers with truthy values for those satisfying the criteria
        on that day.
    """
    cutoffs1 = corrs1.abs().quantile(q=quantile, axis=1)
    cutoffs2 = corrs2.abs().quantile(q=quantile, axis=1)

    return corrs1.abs().lt(cutoffs1, axis=0) & \
        corrs2.abs().lt(cutoffs2, axis=0)


def bottom_k_total_corr_criteria(corrs1, corrs2, k):
    """Similar signature to `quantile_corr_criteria`, but instead ensures a
    fixed number of qualifying tickers on each day by computing the maximum
    correlation magnitude of each ticker to either benchmark, and accepting
    only those tickers with the `k` lowest scores.

    Parameters
    ----------
    corrs1 : pd.DataFrame
    corrs2 : pd.DataFrame
    k : int

    Returns
    -------
    pd.DataFrame
    """

    return np.maximum(corrs1.abs(), corrs2.abs()).rank(axis=1) < k
