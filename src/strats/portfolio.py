import pandas as pd

from collections import defaultdict


def union_add(this, that):
    """Adds the values in `that` to `this`,
    filling with 0 wherever the index does not exist in
    either.

    Parameters
    ----------
    this, that : pd.Series
    """
    return this.add(that, fill_value=0)


def validate_tickers(tickers):
    tickers = pd.Index(tickers).sort_values()
    if tickers.has_duplicates:
        raise ValueError('Duplicate tickers provided.')
    return tickers


def validate_prices(tickers, prices):
    if not tickers.isin(prices.index).all():
        raise ValueError('Prices not provided for all tickers.')


def validate_weights(tickers, weights):
    if isinstance(weights, (float, int)):
        return pd.Series(weights, index=tickers)
    elif not tickers.identical(weights.index):
        if weights.index.sort_values().identical(tickers):
            return weights.sort_index()
        else:
            raise ValueError('Weights index does not match tickers.')
    return weights


def dict_to_df(dicto):
    return pd.DataFrame(dicto.values(), index=dicto.keys())


def valid_range(df):
    valid_dates = slice(df.first_valid_index(),
                        df.last_valid_index())
    return df.loc[valid_dates]


class Portfolio:
    def __init__(self, close_prices):
        self._trading_days = close_prices.index.to_series()
        self._close_prices = close_prices

        self._positions = pd.Series(dtype=float)
        self.last_buy_price = pd.Series(dtype=float)

        self._returns = defaultdict(lambda: pd.Series(dtype=float))
        self._buy_signals = defaultdict(lambda: pd.Series(dtype=float))
        self._sell_signals = defaultdict(lambda: pd.Series(dtype=float))
        self._holdings = defaultdict(lambda: pd.Series(dtype=float))
        self._last_buy_prices = defaultdict(lambda: pd.Series(dtype=float))

    def buy(self, day, tickers, prices, weights=1.0):
        if not isinstance(weights, (int, float)) or weights != 1.0:
            raise ValueError('Fractional buying not yet implemented.')

        tickers = validate_tickers(tickers)
        if tickers.empty:
            return

        validate_prices(tickers, prices)
        weights = validate_weights(tickers, weights)

        self._positions = union_add(self._positions, weights)
        self.last_buy_price = union_add(
            self.last_buy_price, prices[tickers] * weights)
        self._buy_signals[day] = union_add(self._buy_signals[day], weights)

        next_day = self._next_trading_day(day)
        if next_day:
            self._holdings[next_day] = self._positions.copy()

        self._last_buy_prices[day] = self.last_buy_price.copy()

    def sell(self, day, tickers, prices, weights=1.0):
        if not isinstance(weights, (int, float)) or weights != 1.0:
            raise ValueError('Fractional selling not yet implemented.')

        tickers = validate_tickers(tickers)
        if tickers.empty:
            return

        validate_prices(tickers, prices)
        weights = validate_weights(tickers, weights)

        self._positions[tickers] -= weights

        returns = (prices[tickers] / self.last_buy_price[tickers]) - 1

        self._returns[day] = union_add(self._returns[day], returns * weights)
        self._sell_signals[day] = union_add(self._sell_signals[day], weights)

        closed_positions = self.tickers_held[self._positions == 0]

        self._positions.drop(closed_positions, inplace=True)
        self.last_buy_price.drop(closed_positions, inplace=True)

        next_day = self._next_trading_day(day)
        if next_day:
            self._holdings[next_day] = self._positions.copy()

    def _next_trading_day(self, day):
        """Next trading day, according to self._trading_days as extracted
        from the price series.

        Used for cases where an action should be associated with the following
        day for some bookkeeping. E.g. `self.holdings` returns what was held
        at the beginning of a day, so should reflect buys from the
        previous day.

        Returns
        -------
        pd.Timestamp, None:
            None if next day is not in the period.
        """
        next_day = self._trading_days.shift(-1)[day]
        return next_day if not pd.isnull(next_day) else None

    def _tdays_between(self, day1, day2):
        return self._trading_days.index.get_loc(day2) - \
            self._trading_days.index.get_loc(day1)

    @property
    def tickers_held(self):
        return self._positions.index.sort_values()

    @property
    def returns(self):
        """Daily (traded) returns.

        Zero on the days where no trades took place but where positions
        were still held.

        Returns
        -------
        pd.DataFrame
            of all trading days, for each ticker ever held over the backtest
        """
        returns = dict_to_df(self._returns)
        relevant_tickers = self.holdings.columns.union(returns.columns)

        returns = returns.reindex(
            index=self._trading_days,
            columns=relevant_tickers)

        # On days where returns weren't generated from sells,
        # default to 0 for tickers still held.
        returns_all_days = returns.where(
            ~returns.isna().all(axis=1), self.holdings * 0)
        return valid_range(returns_all_days)

    @property
    def holdings(self):
        holdings = dict_to_df(self._holdings) \
            .reindex(self._trading_days, method='ffill')
        return valid_range(holdings)

    @property
    def holding_returns(self):
        """At each time step, for positions held at that time,
        the returns that would be realised if they were sold at that
        point.

        Returns
        -------
        pd.DataFrame
            of all trading days, for each ticker ever held over the backtest.
            Entries are NaN for tickers not held on those days.
        """
        buy_prices = dict_to_df(self._last_buy_prices) \
            .reindex(self._trading_days, method='ffill')
        holding_returns = (self.holdings * self._close_prices / buy_prices) - 1
        return valid_range(holding_returns)[buy_prices.columns]

    @property
    def buy_signals(self):
        return dict_to_df(self._buy_signals)

    @property
    def sell_signals(self):
        return dict_to_df(self._sell_signals)

    @property
    def days_held(self):
        return self.tickers_held.to_series() \
            .apply(lambda tick: self._tdays_between(
                self.buy_signals[tick].last_valid_index(),
                self.holdings[tick].last_valid_index()))

    def stats(self):
        returns = self.returns.mean(axis=1)
        mean = returns.mean()
        stddev = returns.std()
        return {
            'mean': mean,
            'stddev': stddev,
            'sharpe': mean / stddev * (252 ** 0.5)
        }
