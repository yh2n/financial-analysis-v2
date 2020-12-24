import pandas_datareader.data as web
import pandas as pd
import numpy as np


__version__ = '0.0.1'


def get_prices(tickers,
               start,
               end,
               types=None,
               data_source='yahoo',
               out_path=None,
               sort_tks=False):
    """Download prices from external source.
    Args:
        tickers (str or list): The tickers to be downloaded.
        start, end (str): The start date and end date of target period.
        types: The price type(s) to download. If not specified will download
               all types.
        data_source: The data source to use for downloading.
                     See pandas_datareader doc.
        out_path: If specified, the results will be saved to specified path.
        sort_tks: If the tickers in result should be sorted.

    Returns:
        pandas.DataFrame

    """
    if isinstance(tickers, str):
        tickers = [tickers]
    if isinstance(types, str):
        types = [types]
    if (sort_tks):
        tickers = sorted(tickers)
    if data_source == 'yahoo':
        df = get_prices_from_yahoo(tickers, start, end, types)
    if out_path is not None:
        df.to_csv(out_path)
    return df


def get_prices_from_yahoo(tickers, start, end, types=None):
    """Download daily prices from Yahoo!."""
    valid_types = ['Adj Close', 'Close', 'High', 'Low', 'Open', 'Volume']
    if types is not None and not all(i in valid_types for i in types):
        raise ValueError(
            f"Wrong 'types' provided. Must be chosen from{valid_types}.")
    # download from yahoo
    df = web.DataReader(name=tickers,
                        data_source='yahoo',
                        start=start,
                        end=end)
    # hardcoded 1 day before inception dates(for fixing yahoo data)
    inception_dates = {
        'DOMO': '2018-06-28',
        'PS': '2018-05-16',
        'SMAR': '2018-04-26',
        'TWLO': '2016-06-22',
        'ZUO': '2018-04-11',
        'MB': '2015-06-21',
        'GDDY': '2015-04-15',
        'HDP': '2014-12-14',
        'SHOP': '2015-05-21',
        'TEAM': '2015-12-15',
        'PD': '2019-04-11'
    }
    # fix inception dates
    for tk in tickers:
        if tk in inception_dates:
            df.loc[:inception_dates[tk], pd.IndexSlice[:, tk]] = np.nan
    # filter types if provided
    if types is not None:
        df = df[types]
    df = df.apply(print_and_fill_gaps)
    # send warnings if no data
    df.apply(lambda i: print("WARNING: ", i.name,
                             "has no data during the selected period!")
             if i.isna().all() else None)
    return df


def print_and_fill_gaps(series):
    if series.isna().all():
        return series
    s = series.copy()
    trading_idx = s.loc[~s.isna()].index
    first_day = min(trading_idx)
    last_day = max(trading_idx)
    s_trading = s[first_day:last_day]
    if s_trading.isna().any():
        print("Gaps found and filled in ", s.name, " :")
        print(s_trading[s_trading.isna()].index.strftime('%Y%m%d').tolist())
        s[first_day:last_day] = s[first_day:last_day].fillna(method='ffill')
    return s
