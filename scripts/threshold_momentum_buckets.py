import argparse

import pandas as pd
import numpy as np

from datetime import datetime

from src.strats.threshold_momentum import threshold_momentum_returns
from src.utils.load_bucket_prices import load_bucket_prices


def bucket_stats(all_returns, threshold):
    pos_marks = [0, .01, 0.05, .1]
    neg_marks = [-x for x in pos_marks]

    out = []
    for tick in all_returns.columns:
        returns = all_returns[tick].dropna()

        results = {'Ticker': tick, 'Threshold': threshold,
                   'Count': returns.shape[0]}

        for mark in neg_marks[::-1]:
            key = f'Pr(return < {int(mark * 100)}%)'
            results[key] = np.mean(returns < mark)
        for mark in pos_marks:
            key = f'Pr(return >= {int(mark * 100)}%)'
            results[key] = np.mean(returns >= mark)
        out.append(results)
    return out


def bucket_returns_for_thresholds(close_prices, hi_prices):
    all_stats = []
    MOVE_THRESHOLDS = [0.02, 0.03, 0.04, 0.05]

    for threshold in MOVE_THRESHOLDS:
        returns = threshold_momentum_returns(
            close_prices, hi_prices, threshold)
        all_stats.extend(bucket_stats(returns, threshold))

    close_to_hi = (hi_prices - close_prices.shift()) / close_prices.shift()
    all_stats.extend(bucket_stats(close_to_hi, 'All'))

    return pd.DataFrame(all_stats)


if __name__ == '__main__':
    start = '2016-01-01'

    parser = argparse.ArgumentParser()
    parser.add_argument('--today', action='store_const', const=str(
        datetime.today().date()), default='2021-02-11')
    args = parser.parse_args()

    end = args.today

    output_name = f'threshold_momentum_returns_{start}_to_{end}.csv'
    prices = load_bucket_prices(start, end)

    buckets = bucket_returns_for_thresholds(
        prices['adj_close'], prices['adj_high'])
    buckets.round(decimals=4).sort_values(['Ticker', 'Threshold']).to_csv(
        output_name, index=False)
