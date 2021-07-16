def price_targets_test_df(
        close_prices, estimates,
        days_before_fiscal_end, return_window):
    """
    Create a dataframe to be used in an analysis of price targets vs.
    returns.

    To try to approximate independent estimates, one estimate is chosen
    per fiscal period per ticker. For consistency the estimate
    `days_before_fiscal_end` days before the announcement is used.

    Parameters
    ----------
    close_prices
    estimates:
        Flat DataFrame of estimate data as returned from FactSet
    days_before_fiscal_end : int
        The number of days before earnings announcement to use to pick the
        estimate to include.
    return_window : int
        Window following the estimate in which to calculate returns

    Returns
    -------
    DataFrame with one row per (ticker, fiscalEndDate) pair. Each row
    contains potential exog variables around price and returns, and endog
    variables using the estimates such as high, low, mean, and deviation from
    current price.
    """
    grouped = estimates.groupby(['ticker', 'fiscalEndDate'])
    df = grouped \
        .tail(days_before_fiscal_end) \
        .groupby(['ticker', 'fiscalEndDate']) \
        .first()

    # Convert to MultiIndex of (ticker, [dates])
    prices = close_prices.stack().swaplevel().sort_index()

    prices_by_ticker = prices.groupby(level=0)

    returns = prices_by_ticker.pct_change().rolling(return_window).mean()
    total_returns = prices_by_ticker.apply(_nday_total_return, return_window)

    df = df.reset_index().set_index(['ticker', 'estimateDate'])

    # Exog
    df['price_realised'] = prices.shift(-days_before_fiscal_end)
    df['price_at_estimate'] = prices
    df['from_current'] = df['mean'] / df['price_at_estimate']
    df['hi_sd'] = (df['high'] - df['mean']) / df['standardDeviation']
    df['low_sd'] = (df['low'] - df['mean']) / df['standardDeviation']
    df['sd_pct'] = df['standardDeviation'] / df['mean']

    df['hi_sd'] = df['hi_sd'].fillna(0)
    df['low_sd'] = df['low_sd'].fillna(0)

    # Endog
    df['mean_return'] = returns.shift(-return_window)
    df['total_return'] = total_returns.shift(-return_window)
    df['miss'] = df['price_realised'] / df['mean']

    df = df[~df['mean_return'].isna()]

    df['realised_diff'] = (df['price_realised'] / df['price_at_estimate']) - 1

    df = df[df['estimateCount'] > 5]
    return df


def _nday_total_return(series, n):
    return (series / series.shift(n)) - 1
