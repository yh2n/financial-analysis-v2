import pandas as pd

from pathlib import Path
from .get_prices import get_prices

DATA_PATH = Path('data/raw')
BASKET_PATH = Path('data/basket')
BASKET_NAME = 'scorecard_single_ticker'
TICKER_PATH = BASKET_PATH / '{}.csv'.format(BASKET_NAME)


def load_bucket_prices(start, end):
    """
    Returns all available data types from `start` to `end` for the tickers
    specified in `scorecard/data/scorecard_single_ticker.csv'. Data is either
    read from a file in `scorecard/data/` or fetched using `get_prices`.

    Basket can be made a parameter in future.
    """
    price_filename = f'prc_{BASKET_NAME}_{start}_{end}_Y.csv'
    price_filepath = DATA_PATH / price_filename

    tickers = pd.read_csv(TICKER_PATH, header=None,
                          names=['Ticker'], squeeze=True)

    if Path(price_filepath).exists():
        print("Found existing price file. Reading...")
        all_prices = pd.read_csv(price_filepath, header=[0, 1], index_col=0,
                                 parse_dates=True)
        print("Prices read from: ", price_filepath)
    else:
        all_prices = get_prices(
            tickers, start, end,
            out_path=price_filepath)
    return all_prices
