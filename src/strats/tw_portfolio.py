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
    corrs = {}

    for name, series in bench_returns.items():
        corrs[name] = returns.rolling(window) \
            .corr(series.rolling(window))
    return corrs


def quantile_corr_criteria(corrs1, corrs2, quantile):
    cutoffs1 = corrs1.abs().quantile(q=quantile, axis=1)
    cutoffs2 = corrs2.abs().quantile(q=quantile, axis=1)

    return corrs1.abs().lt(cutoffs1, axis=0) & \
        corrs2.abs().lt(cutoffs2, axis=0)


def bottom_k_total_corr_criteria(corrs1, corrs2, k):
    max_corr = corrs1.columns.to_series().apply(
        lambda tick: np.maximum(corrs1.abs()[tick], corrs2.abs()[tick])).T

    return max_corr.apply(lambda series: series.rank() < k, axis=1)
