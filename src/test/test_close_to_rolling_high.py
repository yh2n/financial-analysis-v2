import unittest

import numpy as np
import pandas as pd

from pandas.testing import assert_frame_equal

from strats.close_to_rolling_high import (
    ctrh_returns,
    close_to_rolling_high)


def single_col_df(series):
    return pd.DataFrame({'AAPL': series}, dtype=float)


class TestCloseToRollingHigh(unittest.TestCase):
    def test_sell(self):
        close_prices = single_col_df(
            [1, 1, 1, 1, 1] + [1, 1])
        hi_prices = single_col_df(
            [1, 1, 1.5, 1, 1] + [1, 2])

        ctrh = close_to_rolling_high(close_prices, hi_prices)
        expected_ctrh = single_col_df(
            ([np.nan] * 5) + [0.5, 1.0])

        assert_frame_equal(ctrh, expected_ctrh)

        portfolio = ctrh_returns(close_prices, hi_prices, 1)
        expected = pd.DataFrame({'AAPL': 0.5}, index=[6])

        assert_frame_equal(
            portfolio.returns, expected)

    def test_holds_right_stock(self):
        close_prices = pd.DataFrame({
            'AAPL': [1] * 5 + [1, 2, 2],
            'AAPL_2': [1] * 5 + [1, 1, 1]
        })
        hi_prices = pd.DataFrame({
            'AAPL': [1, 1, 1.5, 1, 1] + [1, 2, 3],
            'AAPL_2': [1] * 5 + [1, 3, 1]
        })

        ctrh = close_to_rolling_high(close_prices, hi_prices)
        expected_ctrh = pd.DataFrame({
            'AAPL': ([np.nan] * 5) + [0.5, 1.0, 2.0],
            'AAPL_2': ([np.nan] * 5) + [0.0, 2.0, 2.0]
        })

        assert_frame_equal(ctrh, expected_ctrh)

        portfolio = ctrh_returns(close_prices, hi_prices, 1)
        expected = pd.DataFrame(
            [{'AAPL': 0.5, 'AAPL_2': np.nan},
             {'AAPL': np.nan, 'AAPL_2': 0.0}], index=[6, 7])
        assert_frame_equal(
            portfolio.returns, expected)

    def test_picks_new_stock(self):
        # Buy stock 1 on day 5 due to 1 -> 1.5 close to rolling high
        # then sell on 1st day of 2nd week (day 6) as returns are 100% > 50%.
        stock1_wk1_close = [1, 1, 1, 1, 1]
        stock1_wk2_close = [1, 2, 1]

        stock1_wk1_hi = [1, 1, 1.5, 1, 1]
        stock1_wk2_hi = [1, 2, 1]

        # On day 6 we can buy again after selling.
        # rolling close-to-high of stock 1 is going to be
        # 50% (day 2 close vs high over days 2-7)
        # while stock 2's is going to be 200%
        # (due to the high on day 5).

        # So stock 2 will be bought on day 6 at a price of 0.5
        # and then sold the following day.
        stock2_wk1_close = [1, 1, 1, 1, 1]
        stock2_wk2_close = [1, 0.5, 1.5]

        stock2_wk1_hi = [1, 1, 1, 1, 1]
        stock2_wk2_hi = [1, 3, 1.5]

        close_prices = pd.DataFrame({
            'AAPL': stock1_wk1_close + stock1_wk2_close,
            'AAPL_2': stock2_wk1_close + stock2_wk2_close
        })
        hi_prices = pd.DataFrame({
            'AAPL': stock1_wk1_hi + stock1_wk2_hi,
            'AAPL_2': stock2_wk1_hi + stock2_wk2_hi,
        })

        ctrh = close_to_rolling_high(close_prices, hi_prices)
        expected_ctrh = pd.DataFrame({
            'AAPL': ([np.nan] * 5) + [0.5, 1.0, 1.0],
            'AAPL_2': ([np.nan] * 5) + [0.0, 2.0, 2.0]
        })

        assert_frame_equal(ctrh, expected_ctrh)

        portfolio = ctrh_returns(close_prices, hi_prices, 1)
        expected = pd.DataFrame(
            [{'AAPL': 0.5, 'AAPL_2': np.nan},
             {'AAPL': np.nan, 'AAPL_2': 2.0}], index=[6, 7])

        assert_frame_equal(
            portfolio.returns, expected)


if __name__ == '__main__':
    unittest.main()
