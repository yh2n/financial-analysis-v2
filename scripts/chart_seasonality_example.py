import json
from datetime import datetime
from pathlib import Path
import sys
sys.path.append('./src')
from utils.get_prices import get_prices
from charting.chart_seasonality import (chart_monthly_seasonality,
                                        chart_cum_avg_daily_rtns)


SECRETS_PATH = Path('./tiingo_secrets.json')
OUTPUT_PATH = Path('./reports/charts')

if __name__ == '__main__':
    with open(SECRETS_PATH) as f:
        api_key = json.load(f)['api_key']
    lookback = 10
    today = datetime.today()
    end = f'{today.year - 1}-12-31'
    start = f'{today.year - lookback}-01-01'
    tk = 'SPY'
    prices = get_prices([tk], start, end, api_key=api_key)['adj_close'][tk]
    output1_name = f'seasonality_monthly_{tk}_{start}_{end}.jpg'
    output1_path = OUTPUT_PATH / output1_name
    output2_name = f'seasonality_daily_{tk}_{start}_{end}.jpg'
    output2_path = OUTPUT_PATH / output2_name
    print("Charting...")
    chart_monthly_seasonality(prices, output1_path)
    chart_cum_avg_daily_rtns(prices, output2_path)
    print("Done!")
