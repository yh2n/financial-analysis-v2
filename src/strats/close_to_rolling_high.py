import pandas as pd

from strats.portfolio import Portfolio


def close_to_rolling_high(close_prices, hi_prices):
    """Calculate returns from the close one week ago to the high over
    that period.
        i.e. the value for `t` will be

            (max(hi_{t-5}, ... hi_t) / c_{t-5}) - 1
    """
    rolling_high = hi_prices.rolling(5).max()
    return (rolling_high / close_prices.shift(5)) - 1


def ctrh_returns(close_prices, hi_prices, top_k, dump_period=None):
    """Strategy that takes positions based on close_to_rolling_high (ctrh),
    as calculated above.

    Positions are taken in stocks with the highest ctrh. Respective ctrh values
    are used as sell limits. When a stock is sold, a new position is taken
    based on current ctrh values.


    Parameters
    ----------
    close_prices : pd.DataFrame
    hi_prices : pd.DataFrame
    top_k : int
    dump_period : int, optional
        Maximum number of days to hold a position for. Sells on the
        `dump_period`th day no matter what. If None, holds indefinitely.

    Returns
    -------
    returns: Portfolio
    """
    ctrh_returns = close_to_rolling_high(close_prices, hi_prices)

    p = Portfolio()

    for day in close_prices.index:
        top_scoring = ctrh_returns.loc[day] \
            .sort_values(ascending=False).head(top_k)

        if p.tickers_held.empty:
            if not top_scoring.isna().any():
                p.buy(day, top_scoring.index, close_prices.loc[day])
                sell_limits = top_scoring.sort_index()
        else:
            curr_returns = (hi_prices.loc[day, p.tickers_held] \
                            / p.last_buy_price) - 1

            selling = curr_returns.index[curr_returns >= sell_limits]
            sell_prices = (sell_limits + 1) * p.last_buy_price

            if len(selling) > 0:
                p.sell(day, selling, sell_prices[selling])

            if dump_period is not None:
                dumping = p.tickers_held[p.days_held > dump_period]
                if len(dumping) > 0:
                    p.sell(day, dumping, close_prices.loc[day])

            sell_limits = sell_limits[p.tickers_held]

            new_buys = top_scoring.drop(
                p.tickers_held, errors='ignore').head(
                    top_k - len(p.tickers_held))
            p.tick(day, close_prices.loc[day])

            p.buy(day, new_buys.index, close_prices.loc[day])

            sell_limits = pd.concat((sell_limits, new_buys)).sort_index()
    return p
