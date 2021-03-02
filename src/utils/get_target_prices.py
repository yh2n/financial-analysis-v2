import os
import quandl

SOURCES = ['quandl']
QUANDL_API_KEY = os.environ.get('QUANDL_API_KEY', None)

def get_target_prices(source, tickers):
	if source not in SOURCES:	
		raise NotImplementedError('Source {} not supported, only {} supported'.format(source, ', '.join(SOURCES)))
	if source == 'quandl':
		data = quandl.get_table('ZACKS/TP', ticker=tickers, api_key=QUANDL_API_KEY)
		return data
