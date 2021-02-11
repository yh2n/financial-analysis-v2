import os

import pandas_datareader.data as web
import pandas as pd
import numpy as np


__version__ = '0.0.3'


def _tiingo_type_mapper(typestr):
    out = ''
    for c in typestr:
        if c.isupper():
            out += '_'
            c = c.lower()
        out += c
    return out


SOURCES = ['yahoo', 'tiingo']

VALID_TYPES = {
    'yahoo': ['Adj Close', 'Close', 'High', 'Low', 'Open', 'Volume'],
    'tiingo': ['close', 'high', 'low', 'open', 'volume', 'adjClose', 'adjHigh',
               'adjLow', 'adjOpen', 'adjVolume', 'divCash', 'splitFactor']
}

TYPE_MAPPERS = {
    'yahoo': lambda typestr: typestr.replace(' ', '_').lower(),
    'tiingo': _tiingo_type_mapper
}


def get_prices(tickers,
               start,
               end,
               types=None,
               data_source='tiingo',
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
    if data_source not in SOURCES:
        raise ValueError(
            'data_source must be one of {SOURCES}.'
        )

    print(f'Downloading prices from {data_source.capitalize()}...')
    df = get_prices_from_source(tickers, start, end, data_source, types)

    if out_path is not None:
        try:
            df.to_csv(out_path)
            print("Results saved to: ", out_path)
        except (IOError, PermissionError):
            Warning("Failed to output to file!")
    print("Download finished.")
    return df


def get_prices_from_source(tickers, start, end, source, types=None):
    """Download daily prices from Yahoo!."""
    if types is not None and not all(i in VALID_TYPES[source] for i in types):
        raise ValueError(
            f"Wrong 'types' provided for source {source}. Must be chosen from "
            f'{VALID_TYPES[source]}.')

    params = {}
    if source == 'tiingo':
        params['api_key'] = os.getenv('TIINGO_API_KEY')

    df = web.DataReader(name=tickers,
                        data_source=source,
                        start=start,
                        end=end,
                        **params)
    df = df.rename(mapper=TYPE_MAPPERS[source], axis=1)

    if source == 'tiingo':
        df = df.unstack(level=0)

    df.index.name = 'date'
    df.columns.names = ['attributes', 'symbols']

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
    df = df.apply(_print_and_fill_gaps)
    # QC: send warnings if no data
    df.apply(lambda i: print("WARNING: ", i.name,
                             "has no data during the selected period!")
             if i.isna().all() else None)
    return df


def _print_and_fill_gaps(series):
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
