import pandas as pd

from collections import defaultdict


def clean_holdings(holdings):
    holdings['quarter'] = pd.to_datetime(holdings['quarter'])
    holdings.loc[:, 'stock_ticker'] = holdings['stock_ticker'].str.strip()

    # Change . to - to match Tiingo format
    holdings.loc[:, 'stock_ticker'] = holdings['stock_ticker'] \
        .str.replace('.', '-', regex=False)

    holdings = dedup_holdings(holdings)
    holdings = filter_valid_positions(holdings)

    return holdings


def resolve_holdings_and_fund_names(holdings, funds):
    """Fund lists downloaded from the website can sometimes have
    different names to the holdings data, even though the ids are the
    same. For anaylsis it's nicer to have them all together.

    It may also be the case that the names change through time in the
    holdings data, too.

    Parameters
    ----------
    holdings : pd.DataFrame
    funds : pd.DataFrame
        Collection of funds from the WW website filter.
        Expects 'Filer' column to hold the filer name.

    Returns
    -------
    (holdings, funds) with filer names reconciled.
    """
    id_to_name = _filer_id_to_name_mapping(holdings, funds)
    holdings.loc[:, 'filer_name'] = id_to_name[holdings['filer_id']].values
    funds.loc[:, 'Filer'] = id_to_name[funds['filer_id']].values

    return holdings, funds


def exclude_missing_prices(holdings, prices):
    """
    Some tickers are supported on Tiingo, but they mean something different
    to a stock ticker that was in the 13F.

    Attempt to filter them by only accepting tickers on dates after they
    were first available on Tiingo
    """

    ticker_to_start = prices.apply(lambda s: s.first_valid_index())
    valid_tickers = prices.columns.intersection(ticker_to_start.index)

    holdings = holdings.loc[holdings['stock_ticker'].isin(valid_tickers)]

    tiingo_start_dates = ticker_to_start[holdings['stock_ticker']].values
    return holdings.loc[holdings['quarter'] >= tiingo_start_dates]


def dedup_holdings(holdings):
    # Sometimes tickers are doubled up; sometimes the stock names are doubled
    # up. There can be many reasons for this, but we make a best bet by taking
    # the highest MV positions.
    return holdings.sort_values('current_mv') \
        .drop_duplicates(
            subset=['filer_name', 'stock_ticker', 'quarter_id'], keep='last') \
        .drop_duplicates(
            subset=['filer_name', 'stock_name', 'quarter_id'], keep='last')


def filter_valid_positions(holdings):
    holdings = holdings.loc[holdings['security_type'] == 'SH']

    holdings = holdings.loc[holdings['position_change_type'] != 'soldall']

    holdings = holdings.loc[holdings['current_shares'] != 0]

    # Around 300 have current_mv < 0. ~6.6k have current-mv == 0
    # As there may be an innocuous issue with WW's current_mv calc,
    # don't throw away those equal to 0
    holdings = holdings.loc[holdings['current_mv'] >= 0]

    return filter_valid_tickers(holdings)


def filter_valid_tickers(holdings):
    holdings = holdings.loc[~holdings['stock_ticker'].isna()]

    has_numbers = holdings['stock_ticker'].str.contains('[0-9]')
    is_empty = holdings['stock_ticker'].str.len() == 0

    return holdings.loc[(~has_numbers) & (~is_empty)]


def filter_young_funds(holdings):
    """Only keep holdings for funds that have more than 2 years worth of
    data in `holdings`. Not currently used in analyses.
    """
    def quarters_alive(holdings):
        return holdings.groupby('filer_name').apply(
            lambda fund_holdings: len(fund_holdings['quarter_id'].unique()))

    young = quarters_alive(holdings) < 8
    return holdings[~holdings['filer_name'].isin(young[young].index)]


def _filer_id_to_name_mapping(holdings, fund_list):
    name_to_id = defaultdict(set)
    id_to_name = {}

    for data, name_key in [(holdings, 'filer_name'), (fund_list, 'Filer')]:
        unique_id_name_pairs = data[[name_key, 'filer_id']].drop_duplicates()

        for _, pair in unique_id_name_pairs.iterrows():
            name_to_id[pair[name_key]].add(pair['filer_id'])
            id_to_name[pair['filer_id']] = pair[name_key]

    assert all([len(v) == 1 for v in name_to_id.values()])

    return pd.Series(id_to_name)
