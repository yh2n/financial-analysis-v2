import json
from datetime import datetime
import sys
sys.path.append('../src')

from utils.get_prices import get_prices
from charting.chart_seasonality import chart_monthly_seasonality


if __name__ == '__main__':
    with open('../tiingo_secrets.json') as f:
        api_key = json.load(f)['api_key']
    lookback = 10
    today = datetime.today()
    end = f'{today.year - 1}-12-31'
    start = f'{today.year - lookback}-01-01'
    tk = 'SPY'
    prices = get_prices([tk], start, end, api_key=api_key)['adj_close'][tk]
    output_name = f'seasonality_monthly_{tk}_{start}_{end}.jpg'
    output_path = f'../reports/charts/{output_name}'
    print("Charting...")
    chart_monthly_seasonality(prices, output_path)
    print("Done!")
