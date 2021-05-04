import base64
import hashlib
import hmac
import os
import json
import requests
import time

import pandas as pd

from pathlib import Path
from urllib.parse import urlencode

API_ENDPOINT = 'https://whalewisdom.com/shell/command.json'

MAX_NUM_FILERS = 10


# Only request these columns from the API to reduce space usage
keep_cols = ['filer_id', 'filer_name', 'stock_id', 'stock_name',
             'stock_ticker', 'security_type', 'shares_change',
             'position_change_type', 'current_ranking', 'previous_ranking',
             'current_percent_of_portfolio', 'previous_percent_of_portfolio',
             'current_mv', 'previous_mv', 'current_shares', 'previous_shares',
             'percent_ownership', 'avg_price', 'percent_change',
             'quarter_id_owned', 'quarter', 'quarter_id', 'sector', 'industry']

# Requests require IDs, so these are the IDs of the above columns
keep_col_ids = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                10, 11, 12, 13, 14, 15, 20, 25, 26, 27]


def make_holdings_request(filer_ids, quarter_ids, temp_dir=None):
    """Get holdings data for all `filer_ids` for all `quarter_ids`.

    Internally breaks requests into chunks of `MAX_NUM_FILERS` filer_ids.

    Passing `temp_dir` persists each batch to `temp_dir`, so that not all
    is lost in the case of unexpected errors.

    Parameters
    ----------
    filer_ids : list of int
    quarter_ids : list of int
    temp_dir : None, optional
        Directory to save intermediate results to.
        Helpful in case of an unexpected error during a large fetch

    Returns
    -------
    pd.DataFrame
    """
    filer_ids = sorted(filer_ids)
    all_dfs = []

    for i in range(0, len(filer_ids), MAX_NUM_FILERS):
        filer_chunk = filer_ids[i:i + MAX_NUM_FILERS]
        df = _make_single_holdings_request(
            filer_chunk, quarter_ids, temp_dir=temp_dir)
        all_dfs.append(df)
        print(f'Finished batch {i / MAX_NUM_FILERS}')
    return pd.concat(all_dfs)


def make_quarters_request():
    """Simple request to fetch all quarters metadata.
    """
    args = {'command': 'quarters'}
    res = _make_ww_request(args)
    _check_ww_response(res)

    df = pd.DataFrame(res.json()['quarters'])
    df['filing_period'] = pd.to_datetime(df['filing_period'])
    return df.set_index('filing_period')


def _make_single_holdings_request(
        filer_ids, quarter_ids, temp_dir=None):
    """Wrapped WW holdings request. Returns a DataFrame.

    Accepts up to MAX_NUM_FILERS in filer_ids and any number of quarters.

    Optionally save results a `temp_dir`.

    Parameters
    ----------
    See `make_holdings_request`.

    Returns
    -------
    pd.DataFrame
        Holdings data for each filer in `filer_ids` for each quarter
        in `quarter_ids`. Each row corresponds to a single position for a fund
        for a quarter.
    """
    if len(filer_ids) > MAX_NUM_FILERS:
        raise ValueError(
            f'Cannot include more than {MAX_NUM_FILERS} filers in one request')

    args = {
        'command': 'holdings',
        'filer_ids': filer_ids,
        'quarter_ids': quarter_ids,
        'columns': keep_col_ids
    }
    res = _make_ww_request(args)
    _check_ww_response(res)

    df = _process_holdings_results(res.json()['results'])
    if temp_dir is not None:
        if isinstance(temp_dir, str):
            temp_dir = Path(temp_dir)

        filer_key = f'{min(filer_ids)}-{max(filer_ids)}'
        quarter_key = f'{min(quarter_ids)}-{max(quarter_ids)}'

        fname = f'ww_filers_{filer_key}_quarters_{quarter_key}.csv'

        df.to_csv(temp_dir / fname)
    return df


def _make_ww_request(args):
    """
    Returns
    -------
    requests.Response object
    """
    if isinstance(args, dict):
        args = json.dumps(args)

    params = _ww_arguments(args)

    params_str = urlencode(params, safe='/+=')
    return requests.get(API_ENDPOINT, params=params_str)


def _ww_arguments(arguments):
    """Convert serialized arguments dict to the request parameters
    that WW expects

    Parameters
    ----------
    arguments : str
    """
    shared_key = os.getenv('WW_SHARED_KEY')

    timestamp = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
    sig = _ww_sig(arguments, timestamp)

    return {
        'args': arguments,
        'api_shared_key': shared_key,
        'api_sig': sig,
        'timestamp': timestamp
    }


def _ww_sig(arguments, timestamp):
    """Generate the WW signature string required for each request

    Details: https://whalewisdom.com/shell/api_help

    Parameters
    ----------
    arguments : str
    timestamp : str
    """
    digest = hashlib.sha1
    secret_key = os.getenv('WW_SECRET_KEY')
    raw_args = arguments + '\n' + timestamp

    hmac_hash = hmac.new(
        secret_key.encode(), raw_args.encode(), digest).digest()
    return base64.b64encode(hmac_hash).rstrip().decode()


def _check_ww_response(resp):
    resp.raise_for_status()

    if resp.text == 'Unknown error processing your command':
        raise ValueError('Got unknown WhaleWisdom error with 200 status')
    if 'errors' in resp:
        raise ValueError(f'WhaleWisdom responded with error: {resp["errors"]}')


def _process_holdings_results(results_json):
    """
    Parameters
    ----------
    results_json : list
        value of the 'results' entry from a valid response
        from the WW holdings endpoint.

        Contains a list of `filer_results`, dictionaries containing
        holdings info for a filer in a quarter.

    Returns
    -------
    pd.DataFrame
        All extracted holdings of all filers.
    """
    holdings_each_filing = [
        _flatten_holdings_filing_record(filing_record)
        for filer_results in results_json
        for filing_record in filer_results['records']
    ]
    flattened = [
        position
        for holdings in holdings_each_filing
        for position in holdings
    ]

    df = pd.DataFrame(flattened)

    if not df.empty:
        df['quarter'] = pd.to_datetime(df['quarter'])
    return df


def _flatten_holdings_filing_record(holdings_result):
    """
    Parameters
    ----------
    holdings_result : dict
        All holdings data for one filer for one quarter.
        `holdings` key contains the actual stock holdings for
        the quarter. Other keys represent quarter and fund
        level data.

    Quarter and fund level data is extracted and added to each holdings
    data 'row' so that each final row has all relevant information.

    Returns
    -------
    list of dicts
        Each dict is data for a holding in that quarter.

    """
    holdings = holdings_result['holdings']
    other_items = holdings_result.copy()
    del other_items['holdings']

    for holding in holdings:
        holding.update(other_items)
    return holdings
