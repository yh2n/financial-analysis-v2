def n_day_periods(trading_days, offset, n):
    """Generate time periods of length n,
    using `trading_days` as the calendar.

    `offset` allows different period sets to be created from
    the same calendar, even if the period duration is the same.

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


def monthly_returns(series, is_return_series=True):
    if is_return_series:
        series = series + 1
    grouped = series.groupby(series.index.to_period('M'))
    if is_return_series:
        return grouped.prod() - 1
    return grouped.last() / grouped.first() - 1


def returns_over_periods(daily_returns, periods, is_return_series=True):
    """Calculates the total returns for each period in `periods`.

    Parameters
    ----------
    daily_returns : pd.Series
    periods : list of pd.Timestamp slices

    Returns
    -------
    pd.Series
        The index corresponds to the endpoints of the `periods`; value is the
        total returns within that period.
    """
    daily_returns = daily_returns.loc[:periods[-1].stop]
    period_end = daily_returns.index.to_series().apply(
        lambda date: _period_end_for_date(date, periods))
    return (1 + daily_returns).groupby(period_end).prod() - 1


def returns_over_periods_from_prices(daily_prices, periods):
    """As `returns_over_periods`, but calculates returns from `daily_prices`
    first.
    """
    period_end = daily_prices.index.to_series().apply(
        lambda date: _period_end_for_date(date, periods))

    return returns_over_periods(
        daily_prices.groupby(period_end).pct_change(), periods)


def _period_end_for_date(date, periods):
    for period in periods:
        if date >= period.start and date < period.stop:
            return period.stop
    raise ValueError(f'Date {date} not in any period.')
