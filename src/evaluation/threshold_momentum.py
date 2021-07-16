import pandas as pd


def calculate_stats(returns, threshold, drawdowns=None):
    thres_data = []

    for tick in returns.columns:
        all_moves = returns[tick].dropna()
        thres_mean = all_moves.mean()
        thres_std = all_moves.std()
        thres_count = all_moves.count()
        thres_sharpe = thres_mean / thres_std
        stats = {
            'ticker': tick, 'mean': thres_mean, 'threshold': threshold,
            'sharpe': thres_sharpe, 'count': thres_count,
            'count_pos': (all_moves > 0).sum(),
            'count_neg': (all_moves < 0).sum(),
            'std': thres_std
        }
        if drawdowns is not None:
            # :TODO: add stats about drawdown durations
            stats['mean_dd'] = drawdowns[tick].mean()
            stats['count_dd'] = drawdowns[tick].count()
            stats['max_dd'] = drawdowns[tick].min()

        thres_data.append(stats)
    return thres_data


def calculate_stats_for_thresholds(
        close_prices, hi_prices, strategy_fn, *args):

    all_stats = []
    MOVE_THRESHOLDS = [0.02, 0.03, 0.04, 0.05]

    for threshold in MOVE_THRESHOLDS:
        returns = strategy_fn(close_prices, hi_prices, threshold, *args)
        all_stats.extend(calculate_stats(returns, threshold))

    return pd.DataFrame(all_stats)


def calculate_stats_with_drawdowns_for_thresholds(
        close_prices, hi_prices, strategy_fn, *args):
    """
    :TODO: Really duplicated with `calculate_stats_for_thresholds`
    due to jamming in dropdown calculations for the one strategy
    where it matters. Once we've cleaned up our strategy interfaces
    this should be updated / removed.
    """

    all_stats = []
    MOVE_THRESHOLDS = [0.02, 0.03, 0.04, 0.05]

    for threshold in MOVE_THRESHOLDS:
        returns, drawdowns = strategy_fn(
            close_prices, hi_prices, threshold, *args)
        all_stats.extend(calculate_stats(
            returns, threshold, drawdowns=drawdowns))

    return pd.DataFrame(all_stats)
