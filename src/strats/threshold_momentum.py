import pandas as pd
import numpy as np


def threshold_momentum_returns(close_prices, hi_prices, threshold):
    """
    Returns a series with returns on the days the strategy would
    have sold.

    Logic:
        - Buy at close_t if return on close from t-1 to t exceeds `threshold`
        - Sell at the high the following day (theoretical)
    """
    close_to_close = close_prices.pct_change()
    close_to_hi = (hi_prices - close_prices.shift()) / close_prices.shift()
    bought_day_before = close_to_close.shift() >= threshold
    return close_to_hi.where(bought_day_before, other=np.nan)


def threshold_momentum_limit_returns(
        close_prices, hi_prices, threshold, limit):
    """
    :returns: a series with the returns on the days the strategy would
    have sold.

    Logic:
        - Buy at close_t if return on close from t-1 to t exceeds `threshold`
        - Sell at either
         - a return of `limit`
         - the close the following day, if the return never reaches `limit`.
    """

    close_to_close = close_prices.pct_change()
    close_to_hi = (hi_prices - close_prices.shift()) / close_prices.shift()

    holding = close_to_close.shift() >= threshold
    sold_at_limit = (close_to_hi >= limit) & holding
    sold_at_close = (close_to_hi < limit) & holding

    returns = close_to_close.copy() * np.nan
    returns[sold_at_limit] = limit
    returns[sold_at_close] = close_to_close

    return returns


def threshold_momentum_holdout_returns(
        close_prices, hi_prices, threshold, limit):
    """
    :returns: a series with the returns on the days the strategy would
    have sold.

    Logic:
        - Buy at close_t if return on close from t-1 to t exceeds `threshold`
        - Sell at either
            - `limit` if return meets or exceeds `limit` the next day
            - first nonnegative return thereafter
    """

    close_to_close = close_prices.pct_change()

    returns = pd.DataFrame(
        np.nan, index=close_to_close.index, columns=close_to_close.columns)
    drawdowns = pd.DataFrame(
        np.nan, index=close_to_close.index, columns=close_to_close.columns)

    for ticker in returns.columns:
        bought_date = None
        bought_price = None

        closes = close_prices[ticker]
        his = hi_prices[ticker]

        for day, yesterday in zip(returns.index[1:], returns.index[:-1]):
            if bought_date is not None:
                close_return = (closes.loc[day] - bought_price) / bought_price
                hi_return = (his.loc[day] - bought_price) / bought_price

                target = limit if bought_date == yesterday else 0

                # If it's day t+1 and target isn't hit during the day
                # but the close return is non-negative, then close out
                if hi_return >= target or close_return >= 0:
                    returns.loc[day, ticker] = target
                    bought_date = None
                    bought_price = None
                else:
                    drawdowns.loc[day, ticker] = close_return

            if (bought_date is None
                    and close_to_close.loc[day, ticker] >= threshold):
                bought_date = day
                bought_price = closes.loc[day]

    return returns, drawdowns
