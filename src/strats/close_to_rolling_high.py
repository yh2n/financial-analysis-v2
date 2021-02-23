import numpy as np
import pandas as pd


def close_to_rolling_high(close_prices, hi_prices):
    """Calculate returns from the close one week ago to the high over
    that period.
        i.e. the value for `t` will be

            (max(hi_{t-5}, ... hi_t) / c_{t-5}) - 1
    """
    rolling_high = hi_prices.rolling(5).max()
    return (rolling_high / close_prices.shift(5)) - 1


def ctrh_returns(close_prices, hi_prices, top_k):
    """Strategy that takes positions based on close_to_rolling_high (ctrh),
    as calculated above.

    Positions are taken in stocks with the highest ctrh. Respective ctrh values
    are used as sell limits. When a stock is sold, a new position is taken
    based on current ctrh values.

    Equal positions are taken in all stocks, so that returns are all divided by
    a factor of `top_k`.


    Parameters
    ----------
    close_prices : pd.DataFrame
    hi_prices : pd.DataFrame
    top_k : int

    Returns
    -------
    returns: pd.Series
        Returns for each trading day. 0 on days with no sells; nan on days
        with no positions.
    holding_returns: pd.DataFrame
        Returns from buy_price to current close for each stock held.
        If no position was held in a stock on that day, nan.
    days_since_buy: pd.DataFrame
        For each stock held, number of days since position was taken.
        If no position held, nan.
    """
    ctrh_returns = close_to_rolling_high(close_prices, hi_prices)

    returns = pd.Series(np.nan, index=close_prices.index)
    holding_returns = []
    holdings = None
    days_since_buy = []

    for day in close_prices.index:
        top_scoring = ctrh_returns.loc[day] \
            .sort_values(ascending=False).head(top_k)

        if holdings is None:
            if not top_scoring.isna().any():
                holdings = pd.DataFrame({
                    'sell_limits': top_scoring,
                    'buy_prices': close_prices.loc[day, top_scoring.index],
                    'days_since_buy': 0
                })
        else:
            holding_returns.append((
                close_prices.loc[day, holdings.index]
                / holdings['buy_prices']) - 1)
            highs = hi_prices.loc[day, holdings.index]
            curr_returns = (highs / holdings['buy_prices']) - 1

            selling = holdings.index[curr_returns >= holdings['sell_limits']]
            still_holding = holdings.index.drop(selling)

            returns.loc[day] = (holdings.loc[selling, 'sell_limits'].mean()
                                if len(selling) > 0 else 0)

            holdings = holdings.drop(selling)
            new_buys = top_scoring.drop(
                still_holding, errors='ignore').head(len(selling))
            holdings.loc[still_holding, 'days_since_buy'] += 1

            new_holdings = pd.DataFrame({
                'sell_limits': new_buys,
                'buy_prices': close_prices.loc[day, new_buys.index],
                'days_since_buy': 0})

            holdings = pd.concat((holdings, new_holdings))
            days_since_buy.append(holdings['days_since_buy'])

    return returns, pd.DataFrame(holding_returns), pd.DataFrame(days_since_buy)
