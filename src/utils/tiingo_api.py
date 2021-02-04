import logging
import requests
import pandas as pd

from datetime import datetime

DEFAULT_START = '2005-01-01'
TODAY = str(datetime.today().date())

API_KEY = '9a73b39f64bb2c32bbc0a52fb5ff970c2929f241'

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
log = logging.getLogger('tiingo')


def get_prices_single_symbol(symbol, start, end):
    params = {
        'token': API_KEY,
        'startDate': start,
        'endDate': end
    }

    symbol = symbol.lower()

    url = f'https://api.tiingo.com/tiingo/daily/{symbol}/prices'
    response = requests.get(url, params)

    if response.status_code != requests.codes.ok:
        log.error('Response has status code %d' % response.status_code)
        response.raise_for_status()

    data = pd.DataFrame(response.json())

    if data.size > 0:
        data['date'] = pd.to_datetime(data['date']).dt.normalize()
        data = data.set_index('date')

    return data


def get_prices(symbols, start, end, out_path=None, verbose=True):
    sym_data = {}

    if isinstance(symbols, str):
        symbols = [symbols]

    for symbol in symbols:
        try:
            data = get_prices_single_symbol(symbol, start, end)
            log.info('Received %d data points for symbol %s',
                     data.shape[0], symbol)
            sym_data[symbol.upper()] = data
        except Exception as e:
            log.error('Failed to fetch data: %s', str(e))

    df = pd.concat(sym_data).unstack(level=0)
    if out_path is not None:
        try:
            df.to_csv(out_path)
            if verbose:
                log.info('Results saved to: %s', out_path)
        except (IOError, PermissionError) as e:
            log.warning('Failed to output to file: %s', str(e))
    return df
