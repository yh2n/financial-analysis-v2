import pandas as pd

from pathlib import Path
from .get_prices import get_prices

DATA_PATH = Path('data/raw')
BASKET_PATH = Path('data/baskets')
BASKET_NAME = 'scorecard_single_ticker'
TICKER_PATH = BASKET_PATH / '{}.csv'.format(BASKET_NAME)


def load_bucket_prices(project_root, start, end, data_source='tiingo',
                       basket=BASKET_NAME, api_key=None):
    filename = f'prc_{basket}_{start}_{end}_{data_source}.csv'
    if isinstance(project_root, str):
        project_root = Path(project_root)

    filepath = project_root / DATA_PATH / filename
    ticker_path = BASKET_PATH / f'{basket}.csv'

    tickers = pd.read_csv(project_root / ticker_path, header=None,
                          names=['Ticker'], squeeze=True)

    if Path(filepath).exists():
        print("Found existing data file. Reading...")
        df = pd.read_csv(filepath, header=[0, 1], index_col=0,
                         parse_dates=True)
        print("Data read from:", filepath)
    else:
        print(f'No existing file found. Fetching data for \
            {len(tickers)} tickers...')
        df = get_prices(tickers, start, end,
                        data_source=data_source, api_key=api_key)
        df.to_csv(filepath)
        print("Results saved to:", filepath)
    return df
