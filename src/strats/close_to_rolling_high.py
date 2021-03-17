import pandas as pd

from strats.portfolio import Portfolio

MONTH_DAYS = 21


def close_to_rolling_high(close_prices, hi_prices):
    """Calculate returns from the close one week ago to the high over
    that period.
        i.e. the value for `t` will be

            (max(hi_{t-5}, ... hi_t) / c_{t-5}) - 1
    """
    rolling_high = hi_prices.rolling(5).max()
    return (rolling_high / close_prices.shift(5)) - 1


def _top_score_limit_returns(
        scores, close_prices, hi_prices, top_k,
        entry=None, one_week_only=False, dump_period=None,
        rebalance=True):
    """Strategy that takes positions based on top `scores`, and sets
    sell limits to those scores.

    Parameters
    ----------
    scores : pd.DataFrame
    close_prices : pd.DataFrame
    hi_prices : pd.DataFrame
    top_k : int
        Number of positions to hold
    entry : str, pd.Timestamp
        Date to (attempt) to start strategy. If < top_k tickers on
        `entry` have non-nan `scores`, then successive days are tried.
    one_week_only : boolean
        Run strategy just for one week. Forces `dump_period = 5` to exit all
        positions at the end of the week.
    dump_period : int, optional
        Maximum number of days to hold a position for. Sells on the
        `dump_period`th day no matter what. If None, holds indefinitely.
        Set to 5 if `one_week_only == True`.
    rebalance : boolean
        Take new positions after selling according to current top `scores`.

    Returns
    -------
    p : Portfolio
    """
    entry = entry or scores.first_valid_index()

    days_run = 0
    p = Portfolio()

    if one_week_only:
        dump_period = 5

    for day in scores.loc[entry:].index:
        if one_week_only and days_run > dump_period:
            break

        top_scoring = scores.loc[day] \
            .sort_values(ascending=False).head(top_k)

        if days_run == 0:
            if not top_scoring.isna().any():
                p.buy(day, top_scoring.index, close_prices.loc[day])
                sell_limits = top_scoring.sort_index()
                days_run += 1
        elif days_run > 0:
            curr_returns = (hi_prices.loc[day, p.tickers_held] \
                            / p.last_buy_price) - 1

            selling = curr_returns.index[curr_returns >= sell_limits]
            sell_prices = (sell_limits + 1) * p.last_buy_price

            if len(selling) > 0:
                p.sell(day, selling, sell_prices[selling])

            p.tick(day, close_prices.loc[day])

            if dump_period is not None:
                dumping = p.tickers_held[p.days_held >= dump_period]
                if len(dumping) > 0:
                    p.sell(day, dumping, close_prices.loc[day])

            sell_limits = sell_limits[p.tickers_held]

            if rebalance:
                new_buys = top_scoring.drop(
                    p.tickers_held, errors='ignore').head(
                        top_k - len(p.tickers_held))

                p.buy(day, new_buys.index, close_prices.loc[day])

                sell_limits = pd.concat((sell_limits, new_buys)).sort_index()

            days_run += 1
    return p


def ctrh_returns(close_prices, hi_prices, top_k, dump_period=None):
    """Runs `top_score_limit` strategy according to last week's
    close-to-rolling-high.

    See `close_to_rolling_high` for definition of those scores.

    Strategy runs over entire provided period and rebalances as positions
    are sold.
    """
    ctrh_returns = close_to_rolling_high(close_prices, hi_prices)
    return _top_score_limit_returns(
        ctrh_returns, close_prices, hi_prices,
        top_k, rebalance=True, dump_period=dump_period)


def ctrh_conf_returns(close_prices, hi_prices, top_k, dump_period=None):
    """Runs `top_score_limit` strategy according to:
        1. 10th percentile close_to_rolling_high over the past 3 months
        2. where that value is less than 10th percentile ctrh over the
            most recent month.

    The score (and therefore sell_limit) is the 3m value.

    The idea is that
        - Taking a low (10th pctile) value yields high probability of
        a positive sell
        - Requiring that 1m value > 3m value ensures the behaviour is
        sustaining.
    """
    ctrh_returns = close_to_rolling_high(close_prices, hi_prices)

    conf_3m = ctrh_returns.rolling(3 * MONTH_DAYS).quantile(0.1)
    conf_1m = ctrh_returns.rolling(MONTH_DAYS).quantile(0.1)

    scores = conf_3m.where(conf_1m >= conf_3m)

    return _top_score_limit_returns(
        scores, close_prices, hi_prices,
        top_k, rebalance=True, dump_period=dump_period)


def sampled_1wk_ctrh_conf_returns(
        close_prices, hi_prices, top_k, num_samples):
    """Runs the `ctrh_conf_returns` for just one week at a time,
    randomly sampling the entrypoints and selling off all positions
    at the end of the week.

    The output is a list of the total returns for each of the
    `num_samples` sampled runs.
    """
    ctrh_returns = close_to_rolling_high(close_prices, hi_prices)

    conf_3m = ctrh_returns.rolling(3 * MONTH_DAYS).quantile(0.1)
    conf_1m = ctrh_returns.rolling(MONTH_DAYS).quantile(0.1)
    scores = conf_3m.where(conf_1m >= conf_3m)

    tradeable_dates = conf_3m.index[~conf_3m.isna().all(axis=1)].to_series()
    start_dates = tradeable_dates.sample(num_samples).index

    weekly_returns = []

    for date in start_dates:
        portfolio = _top_score_limit_returns(
            scores, close_prices, hi_prices, top_k,
            entry=date, one_week_only=True, rebalance=False)
        weekly_returns.append(portfolio.returns.sum().sum() / top_k)
    return weekly_returns
