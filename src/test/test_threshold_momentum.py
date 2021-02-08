import unittest

import numpy as np
import pandas as pd

from pandas.testing import assert_frame_equal

from strats.threshold_momentum import (
    threshold_momentum_returns,
    threshold_momentum_limit_returns,
    threshold_momentum_holdout_returns)


def single_col_df(series):
    return pd.DataFrame({'AAPL': series}, dtype=float)


class TestSellHighThresholdMomentum(unittest.TestCase):
    def test_sell(self):
        # Correct returns for single jump
        close_prices = single_col_df([1, 2, 2])
        hi_prices = single_col_df([1, 2, 4])

        expected = single_col_df([np.nan, np.nan, 1.0])

        returns = threshold_momentum_returns(close_prices, hi_prices, 0.05)

        assert_frame_equal(returns, expected)

    def test_no_sell(self):
        # Returns nan series for no jumps
        close_prices = single_col_df([1, 1, 1])
        hi_prices = single_col_df([1, 10, 20])

        expected = single_col_df([np.nan, np.nan, np.nan])

        returns = threshold_momentum_returns(close_prices, hi_prices, 0.05)

        assert_frame_equal(returns, expected)

    def test_buy_sell(self):
        # Buys and sells on the same day
        close_prices = single_col_df([1, 2, 3, 3])
        hi_prices = single_col_df([1, 2, 4, 6])

        expected = single_col_df([np.nan, np.nan, 1.0, 1.0])

        returns = threshold_momentum_returns(close_prices, hi_prices, 0.05)

        assert_frame_equal(returns, expected)

    def test_holding_at_end(self):
        # Don't calculate any returns for days still holding at end
        close_prices = single_col_df([1, 1, 2])
        hi_prices = single_col_df([1, 2, 2])

        expected = single_col_df([np.nan, np.nan, np.nan])

        returns = threshold_momentum_returns(close_prices, hi_prices, 0.05)

        assert_frame_equal(returns, expected)


class TestCloseoutThresholdMomentum(unittest.TestCase):
    limit = 0.05

    def test_hits(self):
        # Returns only limit when limit exceeded
        close_prices = single_col_df([1, 2, 2])
        hi_prices = single_col_df([1, 2, 4])

        expected = single_col_df([np.nan, np.nan, self.limit])

        returns = threshold_momentum_limit_returns(
            close_prices, hi_prices, 0.05, self.limit)

        assert_frame_equal(returns, expected)

    def test_closes_out(self):
        # Sells at close when limit not hit
        close_prices = single_col_df([1, 2, 1])

        eps = 0.01

        hi_below_limit = close_prices.iloc[1] * (1 + self.limit) - eps
        hi_prices = single_col_df([1, 2, hi_below_limit])

        expected = single_col_df([np.nan, np.nan, -.5])

        returns = threshold_momentum_limit_returns(
            close_prices, hi_prices, 0.05, self.limit)

        assert_frame_equal(returns, expected)

    def test_no_sell(self):
        close_prices = single_col_df([1, 1, 1])

        hi_prices = single_col_df([1, 2, 2])

        expected = single_col_df([np.nan, np.nan, np.nan])

        returns = threshold_momentum_limit_returns(
            close_prices, hi_prices, 0.05, self.limit)

        assert_frame_equal(returns, expected)

    def test_buy_sell(self):
        # Buys and sells on the same day
        close_prices = single_col_df([1, 2, 3, 3])
        hi_prices = single_col_df([1, 2, 4, 6])

        expected = single_col_df([np.nan, np.nan, self.limit, self.limit])

        returns = threshold_momentum_limit_returns(
            close_prices, hi_prices, 0.05, self.limit)

        assert_frame_equal(returns, expected)

    def test_holding_at_end(self):
        # Don't calculate any returns for days still holding at end
        close_prices = single_col_df([1, 1, 2])
        hi_prices = single_col_df([1, 2, 2])

        expected = single_col_df([np.nan, np.nan, np.nan])

        returns = threshold_momentum_limit_returns(
            close_prices, hi_prices, 0.05, self.limit)

        assert_frame_equal(returns, expected)


class TestHoldoutThresholdMomentum(unittest.TestCase):
    limit = 0.05

    def test_waits(self):
        # Recovers 2 days after buying
        close_prices = single_col_df([1, 2, 1, 2])
        hi_prices = single_col_df([1, 2, 1, 2])

        expected_returns = single_col_df([np.nan, np.nan, np.nan, 0])
        expected_drawdowns = single_col_df([np.nan, np.nan, -0.5, np.nan])

        returns, drawdowns = threshold_momentum_holdout_returns(
            close_prices, hi_prices, 0.05, self.limit)

        assert_frame_equal(returns, expected_returns)
        assert_frame_equal(drawdowns, expected_drawdowns)

    def test_hits(self):
        # Hits day after buying
        close_prices = single_col_df([1, 2, 1, 2])
        hi_prices = single_col_df([1, 2, 3, 2])

        expected_returns = single_col_df([np.nan, np.nan, self.limit, np.nan])
        expected_drawdowns = single_col_df([np.nan, np.nan, np.nan, np.nan])

        returns, drawdowns = threshold_momentum_holdout_returns(
            close_prices, hi_prices, 0.05, self.limit)

        assert_frame_equal(returns, expected_returns)
        assert_frame_equal(drawdowns, expected_drawdowns)

    def test_no_sell(self):
        close_prices = single_col_df([1, 1, 1])
        hi_prices = single_col_df([1, 2, 2])

        expected = single_col_df([np.nan, np.nan, np.nan])
        expected_drawdowns = single_col_df([np.nan, np.nan, np.nan])

        returns, drawdowns = threshold_momentum_holdout_returns(
            close_prices, hi_prices, 0.05, self.limit)

        assert_frame_equal(returns, expected)
        assert_frame_equal(drawdowns, expected_drawdowns)

    def test_buy_sell(self):
        close_prices = single_col_df([1, 2, 3, 3])
        hi_prices = single_col_df([1, 2, 4, 6])

        expected = single_col_df([np.nan, np.nan, self.limit, self.limit])
        expected_drawdowns = single_col_df([np.nan, np.nan, np.nan, np.nan])

        returns, drawdowns = threshold_momentum_holdout_returns(
            close_prices, hi_prices, 0.05, self.limit)

        assert_frame_equal(returns, expected)
        assert_frame_equal(drawdowns, expected_drawdowns)

    def test_holding_at_end(self):
        # Don't calculate any returns for days still holding at end
        close_prices = single_col_df([1, 1, 2])
        hi_prices = single_col_df([1, 2, 2])

        expected = single_col_df([np.nan, np.nan, np.nan])
        expected_drawdowns = single_col_df([np.nan, np.nan, np.nan])

        returns, drawdowns = threshold_momentum_holdout_returns(
            close_prices, hi_prices, 0.05, self.limit)

        assert_frame_equal(returns, expected)
        assert_frame_equal(drawdowns, expected_drawdowns)


if __name__ == '__main__':
    unittest.main()
