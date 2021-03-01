import unittest

import numpy as np
import pandas as pd

from pandas.testing import assert_frame_equal, assert_series_equal

from strats.portfolio import Portfolio


class TestPortfolio(unittest.TestCase):
    prices = pd.DataFrame(
        [{'AAL': 2.0, 'ZG': 10.0},
         {'AAL': 1.0, 'ZG': 10.0},
         {'AAL': 1.0, 'ZG': 10.0}],
        index=['2012-01-03', '2012-01-04', '2012-01-05'])

    tickers = ['AAL', 'ZG']

    def setUp(self):
        self.p = Portfolio()

    def test_buy(self):
        day = '2012-01-03'

        self.p.buy(day, ['AAL'], self.prices.loc[day])

        expected_pos = pd.Series({'AAL': 1.0})
        expected_last_buys = pd.Series({'AAL': 2.0})
        expected_buy_signals = pd.DataFrame({'AAL': 1.0}, index=[day])

        assert_series_equal(self.p._positions, expected_pos)
        assert_series_equal(self.p.last_buy_price, expected_last_buys)
        assert_frame_equal(self.p.buy_signals, expected_buy_signals)

    def test_sell(self):
        day = '2012-01-03'

        sell_prices = pd.Series([4.0, 10.0], index=self.tickers)

        self.p.buy(day, ['AAL'], self.prices.loc[day])
        self.p.sell(day, ['AAL'], sell_prices)

        expected_pos = pd.Series(dtype=float)
        expected_last_buys = pd.Series(dtype=float)
        expected_buy_signals = pd.DataFrame({'AAL': 1.0}, index=[day])
        expected_sell_signals = expected_buy_signals
        expected_returns = pd.DataFrame({'AAL': 1.0}, index=[day])

        assert_series_equal(self.p._positions, expected_pos)
        assert_series_equal(self.p.last_buy_price, expected_last_buys)
        assert_frame_equal(self.p.buy_signals, expected_buy_signals)
        assert_frame_equal(self.p.sell_signals, expected_sell_signals)
        assert_frame_equal(self.p.returns, expected_returns)

    def test_sell_next_day(self):
        buy_day = '2012-01-03'
        sell_day = '2012-01-04'

        self.p.buy(buy_day, ['AAL'], self.prices.loc[buy_day])
        self.p.sell(sell_day, ['AAL'], self.prices.loc[sell_day])

        expected_pos = pd.Series(dtype=float)
        expected_last_buys = pd.Series(dtype=float)
        expected_buy_signals = pd.DataFrame({'AAL': 1.0}, index=[buy_day])
        expected_sell_signals = pd.DataFrame({'AAL': 1.0}, index=[sell_day])
        expected_returns = pd.DataFrame({'AAL': -0.5}, index=[sell_day])

        assert_series_equal(self.p._positions, expected_pos)
        assert_series_equal(self.p.last_buy_price, expected_last_buys)
        assert_frame_equal(self.p.buy_signals, expected_buy_signals)
        assert_frame_equal(self.p.sell_signals, expected_sell_signals)
        assert_frame_equal(self.p.returns, expected_returns)

    def test_buy_consecutive(self):
        buy_day = '2012-01-03'
        buy_day2 = '2012-01-04'

        self.p.buy(buy_day, ['AAL'], self.prices.loc[buy_day])
        self.p.buy(buy_day2, ['ZG'], self.prices.loc[buy_day2])

        expected_pos = pd.Series({'AAL': 1.0, 'ZG': 1.0})
        expected_last_buys = pd.Series({'AAL': 2.0, 'ZG': 10.0})
        expected_buy_signals = pd.DataFrame(
            [{'AAL': 1.0, 'ZG': np.nan},
             {'AAL': np.nan, 'ZG': 1.0}], index=[buy_day, buy_day2])

        assert_series_equal(self.p._positions, expected_pos)
        assert_series_equal(self.p.last_buy_price, expected_last_buys)
        assert_frame_equal(self.p.buy_signals, expected_buy_signals)

    def test_buy_multiple(self):
        buy_day = '2012-01-03'

        prices = pd.Series([2.0, 10.0], index=self.tickers)

        self.p.buy(buy_day, ['AAL', 'ZG'], prices)

        expected_pos = pd.Series({'AAL': 1.0, 'ZG': 1.0})
        expected_last_buys = pd.Series({'AAL': 2.0, 'ZG': 10.0})
        expected_buy_signals = pd.DataFrame(
            {'AAL': 1.0, 'ZG': 1.0}, index=[buy_day])

        assert_series_equal(self.p._positions, expected_pos)
        assert_series_equal(self.p.last_buy_price, expected_last_buys)
        assert_frame_equal(self.p.buy_signals, expected_buy_signals)

    def test_sell_multiple(self):
        buy_day = '2012-01-03'
        sell_day = '2012-01-04'

        self.p.buy(buy_day, ['AAL', 'ZG'], self.prices.loc[buy_day])
        self.p.sell(sell_day, ['AAL', 'ZG'], self.prices.loc[sell_day])

        expected_pos = pd.Series(dtype=float)
        expected_last_buys = pd.Series(dtype=float)
        expected_buy_signals = pd.DataFrame(
            {'AAL': 1.0, 'ZG': 1.0}, index=[buy_day])
        expected_sell_signals = pd.DataFrame(
            {'AAL': 1.0, 'ZG': 1.0}, index=[sell_day])
        expected_returns = pd.DataFrame(
            {'AAL': -0.5, 'ZG': 0.0}, index=[sell_day])

        assert_series_equal(self.p._positions, expected_pos)
        assert_series_equal(self.p.last_buy_price, expected_last_buys)
        assert_frame_equal(self.p.buy_signals, expected_buy_signals)
        assert_frame_equal(self.p.sell_signals, expected_sell_signals)
        assert_frame_equal(self.p.returns, expected_returns)

    def test_tick(self):
        day = '2012-01-03'
        day2 = '2012-01-04'

        self.p.buy(day, ['AAL'], self.prices.loc[day])
        self.p.tick(day2, self.prices.loc[day2])

        expected_pos = pd.Series({'AAL': 1.0})
        expected_last_buys = pd.Series({'AAL': 2.0})
        expected_buy_signals = pd.DataFrame({'AAL': 1.0}, index=[day])
        expected_returns = pd.DataFrame({'AAL': 0.0}, index=[day2])
        expected_holding_returns = pd.DataFrame(
            {'AAL': -0.5}, index=[day2])
        expected_days_held = pd.Series({'AAL': 1.0})

        assert_series_equal(self.p._positions, expected_pos)
        assert_series_equal(self.p.last_buy_price, expected_last_buys)
        assert_frame_equal(self.p.buy_signals, expected_buy_signals)
        assert_frame_equal(self.p.returns, expected_returns)
        assert_frame_equal(self.p.holding_returns, expected_holding_returns)
        assert_series_equal(self.p.days_held, expected_days_held)

    def test_tick_nothing_held(self):
        day = '2012-01-03'

        self.p.tick(day, self.prices.loc[day])

        expected_pos = pd.Series(dtype=float)
        expected_last_buys = pd.Series(dtype=float)
        expected_buy_signals = pd.DataFrame(dtype=float)
        expected_returns = pd.DataFrame(dtype=float, index=[day])
        expected_holding_returns = pd.DataFrame(dtype=float, index=[day])
        expected_days_held = pd.Series(dtype=float)

        assert_series_equal(self.p._positions, expected_pos)
        assert_series_equal(self.p.last_buy_price, expected_last_buys)
        assert_frame_equal(self.p.buy_signals, expected_buy_signals)
        assert_frame_equal(self.p.returns, expected_returns)
        assert_frame_equal(self.p.holding_returns, expected_holding_returns)
        assert_series_equal(self.p.days_held, expected_days_held)

    def test_tick_diff_buys(self):
        day = '2012-01-03'
        day2 = '2012-01-04'
        day3 = '2012-01-05'

        self.p.buy(day, ['AAL'], self.prices.loc[day])
        self.p.tick(day2, self.prices.loc[day2])
        self.p.buy(day2, ['ZG'], self.prices.loc[day2])
        self.p.tick(day3, self.prices.loc[day3])

        expected_pos = pd.Series({'AAL': 1.0, 'ZG': 1.0})
        expected_last_buys = pd.Series({'AAL': 2.0, 'ZG': 10.0})
        expected_buy_signals = pd.DataFrame(
            [{'AAL': 1.0, 'ZG': np.nan},
             {'AAL': np.nan, 'ZG': 1.0}], index=[day, day2])
        expected_returns = pd.DataFrame(
            [{'AAL': 0.0, 'ZG': np.nan},
             {'AAL': 0.0, 'ZG': 0.0}], index=[day2, day3])
        expected_holding_returns = pd.DataFrame(
            [{'AAL': -0.5, 'ZG': np.nan},
             {'AAL': -0.5, 'ZG': 0.0}], index=[day2, day3])
        expected_days_held = pd.Series({'AAL': 2.0, 'ZG': 1.0})

        assert_series_equal(self.p._positions, expected_pos)
        assert_series_equal(self.p.last_buy_price, expected_last_buys)
        assert_frame_equal(self.p.buy_signals, expected_buy_signals)
        assert_frame_equal(self.p.returns, expected_returns)
        assert_frame_equal(self.p.holding_returns, expected_holding_returns)
        assert_series_equal(self.p.days_held, expected_days_held)


if __name__ == '__main__':
    unittest.main()
