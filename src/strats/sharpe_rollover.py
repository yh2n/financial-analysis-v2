import numpy as np
import pandas as pd

from pandas.tseries.holiday import USFederalHolidayCalendar


def sharpe(returns, freq='daily'):
    """Calculate the annualized Sharpe ratio.

    Parameters
    ----------
    returns : pd.Series
    freq : str, optional
        Either 'daily' or 'monthly', to determine
        annualizing factor.
    """
    annualize_factor = None
    if freq == 'daily':
        annualize_factor = 252
    elif freq == 'monthly':
        annualize_factor = 12
    else:
        raise ValueError("freq must be one of 'daily' or 'monthly'")

    if np.std(returns) == 0:
        print('Returns has zero std dev')
        if (~returns.isna()).sum() == 1:
            print('Because there was only one traded day in window')
            return np.nan
        elif (returns[~returns.isna()] == 0).all():
            print('Because all returns were 0')
            return np.nan

    return np.mean(returns) / np.std(returns) * (annualize_factor ** 0.5)


def next_business_day(date):
    """
    Parameters
    ----------
    date : pd.Timestamp, str

    Returns
    -------
    pd.Timestamp
        `date`, if `date` is on a business day. Otherwise
        the following business day.
    """
    return pd.tseries.offsets.CustomBusinessDay(
        n=1, calendar=USFederalHolidayCalendar()).rollforward(date)


def prev_business_day(date):
    """
    Parameters
    ----------
    date : pd.Timestamp, str

    Returns
    -------
    pd.Timestamp
        `date`, if `date` is on a business day. Otherwise
        the previous business day.
    """
    return pd.tseries.offsets.CustomBusinessDay(
        n=1, calendar=USFederalHolidayCalendar()).rollback(date)


def get_trade_dates(start, end, step):
    out = []
    day = start
    while day < end:
        day = next_business_day(day)
        out.append(day)
        day += step
    return out


def returns(close_prices, window_duration, hold_duration, top_k):
    """"Sharpe rollover" strategy:
        - Take equal-weighted positions in the `top_k` tickers
        with the highest Sharpe ratio over the past `window_duration`.
        - After `hold_duration`, exit all positions and enter again as
        above.

    Parameters
    ----------
    close_prices : pd.Series
    window_duration : pd.DateOffset
    hold_duration : pd.DateOffset
    top_k : int

    Returns
    -------
    returns: pd.DataFrame
        Returns from each of the securities sold for each day traded
    all_buy_prices: pd.DataFrame
        Buy prices of each security on each day bought
    all_sell_prices: pd.DataFrame
    """
    buy_prices = None

    trade_dates = get_trade_dates(
        close_prices.index[0] + window_duration,
        close_prices.index[-1],
        hold_duration)

    tickers = close_prices.columns

    buy_signals = pd.DataFrame(
        False, index=trade_dates, columns=tickers)
    sell_signals = pd.DataFrame(
        False, index=trade_dates[1:], columns=tickers)

    returns = pd.DataFrame(np.nan, index=trade_dates[1:], columns=range(top_k))

    for i, rollover in enumerate(trade_dates):
        window_begin = prev_business_day(rollover - window_duration)
        current_sharpes = close_prices.loc[window_begin:rollover] \
            .pct_change().apply(sharpe).sort_values(ascending=False)

        if buy_prices is not None:
            # Calculate actual returns from rolling over positions
            sell_prices = close_prices.loc[rollover, buy_prices.index]
            returns.loc[rollover] = ((sell_prices / buy_prices) - 1).values
            sell_signals.loc[rollover, sell_prices.index] = True

        top_scores = current_sharpes.head(top_k)
        buy_prices = close_prices.loc[rollover, top_scores.index]

        buy_signals.loc[rollover, buy_prices.index] = True

    return returns, buy_signals, sell_signals
