import pandas as pd

from statsmodels.regression.linear_model import OLS
from statsmodels.api import add_constant


def read_monthly_ff_file(path):
    data = pd.read_csv(path, index_col=0, parse_dates=True)

    data.index = data.index.to_series().apply(_yearmonth_to_datetime)
    data.index = data.index.to_period('M')
    data.columns = data.columns.str.strip()

    return data / 100


def read_daily_ff_file(path):
    data = pd.read_csv(path, index_col=0, parse_dates=True)
    data.columns = data.columns.str.strip()
    return data / 100


def _yearmonth_to_datetime(yearmonth_int):
    """Takes a 'yearmonth' integer (so January 1970 would
    be `197001`) and returns the `pd.Timestamp` for the first day
    of that month.

    Parameters
    ----------
    yearmonth_int

    Returns
    -------
    pd.Timestamp
    """
    yearmonth = str(yearmonth_int)

    year = int(yearmonth[:4])
    month = int(yearmonth[-2:])

    return pd.Timestamp(
        year=year,
        month=month,
        day=1)


def run_aligned_ols(endog, exog):
    endog, exog = _align_dfs(endog.dropna(), exog)
    return OLS(endog, add_constant(exog)).fit()


def _align_dfs(df1, df2):
    """Subsets both df1 and df2 to have the same index.
    Tolerant of unequal endpoints, but not of otherwise missing
    index values.

    NOTE: This may be useful enough to pull out into a more general
    utils file at some point.

    Returns
    -------
    df1, df2
        Both along the same index, which has endpoints corresponding
        to the tighter of the two.
    """
    start = max(df1.index.min(), df2.index.min())
    end = min(df1.index.max(), df2.index.max())

    df1 = df1.loc[start:end]
    df2 = df2.loc[start:end]

    if not df1.index.equals(df2.index):
        raise ValueError('Indexes do not contiguously overlap.')
    return df1, df2


# The returns aggregators below may end up better suited in a more general
# utils file, but have so far only been used in the FF analyses.
def monthly_returns(series, is_return_series=True):
    if is_return_series:
        series = series + 1
    grouped = series.groupby(series.index.to_period('M'))
    if is_return_series:
        return grouped.prod() - 1
    return grouped.last() / grouped.first() - 1


def returns_over_periods(daily_returns, periods, is_return_series=True):
    """Calculates the total returns for each period in `periods`.

    Parameters
    ----------
    daily_returns : pd.Series
    periods : list of pd.Timestamp slices

    Returns
    -------
    pd.Series
        The index corresponds to the endpoints of the `periods`; value is the
        total returns within that period.
    """
    daily_returns = daily_returns.loc[:periods[-1].stop]
    period_end = daily_returns.index.to_series().apply(
        lambda date: _period_end_for_date(date, periods))
    return (1 + daily_returns).groupby(period_end).prod() - 1


def returns_over_periods_from_prices(daily_prices, periods):
    """As `returns_over_periods`, but calculates returns from `daily_prices`
    first.
    """
    period_end = daily_prices.index.to_series().apply(
        lambda date: _period_end_for_date(date, periods))

    return returns_over_periods(
        daily_prices.groupby(period_end).pct_change(), periods)


def _period_end_for_date(date, periods):
    for period in periods:
        if date >= period.start and date < period.stop:
            return period.stop
    raise ValueError(f'Date {date} not in any period.')
