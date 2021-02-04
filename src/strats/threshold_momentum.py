import numpy as np


def threshold_momentum_returns(close_prices, hi_prices, threshold):
    """
    Returns a series with returns on the days the strategy would
    have sold.

    Specifically:
        - Buy at close_t if return on close from t-1 to t exceeds `threshold`
        - Sell at the high the following day (theoretical)
    """
    close_to_close = close_prices.pct_change()
    close_to_hi = (hi_prices - close_prices.shift()) / close_prices.shift()
    return close_to_hi[close_to_close.shift() >= threshold]


def threshold_momentum_limit_returns(
        close_prices, hi_prices, threshold, limit):
    """
    :returns: a series with the returns on the days the strategy would
    have sold.

    Specifically,
        - Buy at close_t if return on close from t-1 to t exceeds `threshold`
        - Sell at either
         - a return of `limit`
         - the high the following day, if lower than `limit`.
    """

    close_to_close = close_prices.pct_change()
    close_to_hi = (hi_prices - close_prices.shift()) / close_prices.shift()

    max_returns = close_to_hi[close_to_close.shift() >= threshold]
    return np.minimum(limit, max_returns)


def calculate_stats(returns, threshold):
    thres_data = []

    for tick in returns.columns:
        all_moves = returns[tick].dropna()
        thres_mean = all_moves.mean()
        thres_std = all_moves.std()
        thres_count = all_moves.count()
        thres_sharpe = thres_mean / thres_std
        thres_data.append({
            'ticker': tick, 'mean': thres_mean, 'threshold': threshold,
            'sharpe': thres_sharpe, 'count': thres_count,
            'count_pos': (all_moves > 0).sum(),
            'count_neg': (all_moves < 0).sum(),
            'std': thres_std})
    return thres_data
