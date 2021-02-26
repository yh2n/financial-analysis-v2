import pandas as pd

from collections import defaultdict


def union_add(this, that):
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


class Portfolio:
    def __init__(self, start, end, tickers):
        self._positions = pd.Series()
        self.last_buy_price = pd.Series()
        self.days_held = pd.Series()

        self._returns = defaultdict(pd.Series)
        self._holding_returns = defaultdict(pd.Series)
        self.buy_signals = defaultdict(pd.Series)
        self.sell_signals = defaultdict(pd.Series)

    def buy(self, day, tickers, prices, weights=1.0):
        tickers = validate_tickers(tickers)
        validate_prices(tickers, prices)
        weights = validate_weights(tickers, weights)

        self._positions = union_add(self._positions, weights)
        self.last_buy_price = union_add(self.last_buy_price, prices[tickers])
        self.buy_signals[day] = union_add(self.buy_signals[day], weights)

        new_hold = self._positions[tickers] == weights

        self.days_held = union_add(
            self.days_held, pd.Series(0, index=new_hold.index))

    def sell(self, day, tickers, prices, weights=1.0):
        tickers = validate_tickers(tickers)
        validate_prices(tickers, prices)
        weights = validate_weights(tickers, weights)

        self._positions[tickers] -= weights

        returns = (prices[tickers] / self.last_buy_price[tickers]) - 1

        self._returns[day] = union_add(self._returns[day], returns * weights)
        self.sell_signals[day] = union_add(self.sell_signals[day], weights)

        closed_positions = self.tickers_held[self._positions == 0]

        self.days_held.drop(closed_positions, inplace=True)
        self._positions.drop(closed_positions, inplace=True)
        self.last_buy_price.drop(closed_positions, inplace=True)

    def tick(self, day, prices):
        self._returns[day] = union_add(self._returns[day],
                                       pd.Series(0.0, index=self.tickers_held))
        self._holding_returns[day] = (prices[self.tickers_held]
                                      / self.last_buy_price) - 1
        self.days_held += 1

    @property
    def tickers_held(self):
        return self._positions.index.sort_values()

    @property
    def returns(self):
        return pd.DataFrame(self._returns.values(), index=self._returns.keys())

    @property
    def holding_returns(self):
        return pd.DataFrame(
            self._holding_returns.values(), index=self._holding_returns.keys())
