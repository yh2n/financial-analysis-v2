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


class Portfolio:
    def __init__(self):
        self._positions = pd.Series(dtype=float)
        self.last_buy_price = pd.Series(dtype=float)
        self.days_held = pd.Series(dtype=float)

        self._returns = defaultdict(lambda: pd.Series(dtype=float))
        self._holding_returns = defaultdict(lambda: pd.Series(dtype=float))
        self._buy_signals = defaultdict(lambda: pd.Series(dtype=float))
        self._sell_signals = defaultdict(lambda: pd.Series(dtype=float))

    def buy(self, day, tickers, prices, weights=1.0):
        if not isinstance(weights, (int, float)) or weights != 1.0:
            raise ValueError('Fractional buying not yet implemented.')

        tickers = validate_tickers(tickers)
        validate_prices(tickers, prices)
        weights = validate_weights(tickers, weights)

        self._positions = union_add(self._positions, weights)
        self.last_buy_price = union_add(self.last_buy_price, prices[tickers])
        self._buy_signals[day] = union_add(self._buy_signals[day], weights)

        new_hold = self._positions[tickers] == weights

        self.days_held = union_add(
            self.days_held, pd.Series(0, index=new_hold.index))

    def sell(self, day, tickers, prices, weights=1.0):
        if not isinstance(weights, (int, float)) or weights != 1.0:
            raise ValueError('Fractional selling not yet implemented.')

        tickers = validate_tickers(tickers)
        validate_prices(tickers, prices)
        weights = validate_weights(tickers, weights)

        self._positions[tickers] -= weights

        returns = (prices[tickers] / self.last_buy_price[tickers]) - 1

        self._returns[day] = union_add(self._returns[day], returns * weights)
        self._sell_signals[day] = union_add(self._sell_signals[day], weights)

        closed_positions = self.tickers_held[self._positions == 0]

        self.days_held.drop(closed_positions, inplace=True)
        self._positions.drop(closed_positions, inplace=True)
        self.last_buy_price.drop(closed_positions, inplace=True)

    def tick(self, day, prices):
        """
        It's natural to update some quantities day-by-day, and that's
        what this `tick` method is intended to do.

        One issue is this requires that the caller know to call this
        on every loop of a daily strategy (and that such a loop exists).

        This would become less of an issue if we standardised a
        strategy-running engine.

        `self._returns` is updated to set returns for all positions to 0 if
        not currently set, to reflect that unsold positions generate 0 returns.
        `union_add` is used so no assumptions are made about when `sell` may be
        called for this `day`.
        """
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
        return pd.DataFrame(self._holding_returns.values(),
                            index=self._holding_returns.keys())

    @property
    def buy_signals(self):
        return pd.DataFrame(self._buy_signals.values(),
                            index=self._buy_signals.keys())

    @property
    def sell_signals(self):
        return pd.DataFrame(self._sell_signals.values(),
                            index=self._sell_signals.keys())
