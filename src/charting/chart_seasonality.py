import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as plticker


def calc_hit_rate(ts, threshold=0):
    """Calculate hit rate for timeseries.
    Args:
        ts (pd.Series): Timeseries.
        threshold (float): The minimum value to be considered 'hit'.

    Returns:
        A single number of hit rate.
    """
    return (ts > threshold).sum() / ts.count()


def calc_monthly_rtns(series, s_type='price'):
    """Calculate the calendar monthly returns.
    Args:
        series (pd.Series): Daily price timeseries or daily return timeseries.
        s_type (str): 'price' or 'return'.

    Returns:
        A pd.Series of monthly returns.
    """
    if s_type == 'price':
        r_daily = series.pct_change().dropna().squeeze()
    elif s_type == 'return':
        r_daily = series
    else:
        raise ValueError("s_type must be 'price' or 'return'")
    r_monthly = r_daily.dropna().resample('M').apply(lambda x: (1+x).prod()-1)
    return r_monthly


def calc_avg_daily_rtns(prices):
    """Calculate the average returns of every calendar day
    based on historical prices.
    Args:
        prices (pd.Series): Daily prices timeseries.

    Returns:
        A pd.Series of average daily returns
    """
    tk = prices.name
    rtns = prices.pct_change()
    avg_d_rtns = rtns.groupby([prices.index.month, prices.index.day]).mean()
    avg_d_rtns.index.rename(['Month', 'Day'], [0, 1], inplace=True)
    avg_d_rtns = avg_d_rtns.reset_index()
    avg_d_rtns.index = pd.to_datetime('2020-'
                                      + avg_d_rtns['Month'].apply(str)
                                      + '-'
                                      + avg_d_rtns['Day'].apply(str),
                                      format='%Y-%m-%d')
    return avg_d_rtns[tk]


def calc_cum_avg_daily_rtns(prices):
    """Calculate cumulative average daily returns."""
    avg_d_rtns = calc_avg_daily_rtns(prices)
    cum_avg_d_rtn = (1 + avg_d_rtns).cumprod() - 1
    return cum_avg_d_rtn


def calc_monthly_seasonality_stats(prices):
    """Calculate mean returns and hit rates for each calendar month.
    Args:
        prices (pd.Series): Daily prices used for calculating seasonality.

    Returns:
        mean_rtn (pd.Series) and hit_rate (pd.Series) for 12 calendar months,
        indexed by the month index from 1 to 12.
    """
    r_monthly = calc_monthly_rtns(prices)
    mean_rtn = r_monthly.groupby(r_monthly.index.month).mean()
    hit_rate = r_monthly.groupby(r_monthly.index.month).apply(calc_hit_rate)
    return mean_rtn, hit_rate


def chart_monthly_seasonality(prices, output_path=None):
    """Chart monthly seasonal mean returns and hit rates.
    Args:
        prices (pd.Series): Daily prices used for calculating seasonality.
        output_path (str): If specified, chart will be saved to the
                           specified path.

    """
    tk = prices.name.upper()
    mean_rtn, hit_rate = calc_monthly_seasonality_stats(prices)
    mean_rtn.index = pd.to_datetime(mean_rtn.index, format='%m').strftime('%b')
    hit_rate.index = pd.to_datetime(hit_rate.index, format='%m').strftime('%b')
    inception_yr = prices.index[1].strftime('%Y')

    # Create a new figure, plot barchart of monthly mean returns
    fig, ax = plt.subplots(figsize=(20, 9))
    x = np.arange(len(mean_rtn))
    y = mean_rtn * 100
    ax.bar(x[y > 0], y[y > 0], color='midnightblue',
           label='Average Return')
    ax.bar(x[y <= 0], y[y <= 0], color='darkred')
    ax.yaxis.set_major_locator(plticker.MultipleLocator(base=0.5))
    ax.set_xlabel('Month')
    ax.set_ylabel('Mean Monthly Return %')
    ax.set_title(f'{tk} Seasonality: 1 Month Return (Since {inception_yr})')

    # Plot line chart of hit rates using existing x-axis
    ax1 = ax.twinx()
    ax1.set_ylim([0, 100])
    ax1.set_ylabel('% of Time Positive')
    ax1.plot(hit_rate*100, color='deepskyblue', label='% Up')
    ax1.grid(False)

    # Add legend
    fig.legend(loc="upper left", bbox_to_anchor=(0, 1),
               bbox_transform=ax.transAxes)

    if output_path is not None:
        fig.savefig(output_path)


def chart_cum_avg_daily_rtns(prices, output_path=None):
    """Chart cumulative avg daily returns.
    Args:
        prices (pd.Series): Daily prices used for calculating seasonality.
        output_path (str): If specified, chart will be saved to the
                           specified path.

    """
    tk = prices.name
    prices = prices.dropna()
    cum_avg_d_rtns = calc_cum_avg_daily_rtns(prices)
    # avg_d_rtns = calc_avg_daily_rtns(prices)
    # avg_m_rtns = calc_monthly_rtns(avg_d_rtns, 'return')

    fig, ax = plt.subplots(figsize=(20, 9))
    df = 100 + cum_avg_d_rtns*100
    df.index = df.index.strftime('%b-%d')
    ax.plot(df, color='red')
    ax.xaxis.set_major_locator(plticker.MultipleLocator(base=15))
    ax.yaxis.set_major_locator(plticker.MultipleLocator(base=5))
    ax.yaxis.tick_right()
    today_idx = df.index.get_loc(pd.Timestamp.today().strftime('%b-%d'))
    ax.axvline(today_idx, color='green')
    init_date = prices.index[0].strftime('%Y-%m-%d')
    last_date = prices.index[-1].strftime('%Y-%m-%d')
    ax.set_title(f'{tk} Seasonality: Cumulative Avg Daily Return'
                 f' ({init_date} to {last_date})')

    if output_path is not None:
        fig.savefig(output_path)


def calc_monthly_seasonality_signals(prices, signal_metric, n_exclude=None,
                                     threshold=None):
    """Generate trading signals according to monthly seasonality stats.
    Signals: -1 means short, 0 means flat, 1 means long.
    Args:
        prices: Daily prices used to calculate everything.
        signal_metric: The type of metric used to determine signal.
            Should be either 'return' or 'hit_rate'.
        n_exclude (int): Number of worst months to exclude(not trade).
            If not None, threshold will be ignored.
        threshold (float): The min value required on the metric to enter
            long position. If n_exclude is not None, threshold will be ignored.

    Returns:
        signals (pd.Series): Trade signals, indexed by month index(1 to 12).
    """
    if n_exclude is None and threshold is None:
        raise ValueError("Either n_exclude or threshold cannot be None!")
    if signal_metric not in ['return', 'hit_rate']:
        raise ValueError(
            "signal_metric can only be either 'return' or 'hit_rate'!")

    mean_rtn_lb, hit_rate_lb = calc_monthly_seasonality_stats(prices)
    vals = mean_rtn_lb if signal_metric == 'return' else hit_rate_lb
    signals = vals.copy()
    if n_exclude is not None:
        idx_ex = signals.sort_values().head(n_exclude).index
        signals[idx_ex] = 0
        signals[~signals.index.isin(idx_ex)] = 1
    else:
        signals[signals < threshold] = 0
        signals[signals >= threshold] = 1
    signals.loc[:] = 1
    signals.loc[[9, 10]] = 0
    return signals


def trade_monthly_seasonality(prices, lb_start, lb_end, hd_start, hd_end,
                              signal_metric, n_exclude, threshold):
    """Calculate monthly returns from tradding monthly seasonality.
    Args:
        prices: Daily prices.
        lb_start, lb_end (str): Start/end date of lookback period
            in format '%Y-%m-%d'.
        hd_start, hd_end (str): Start/end date of holding period
            in format '%Y-%m-%d'.
        signal_metric (str): The type of metric used to determine signal.
            Should be either 'return' or 'hit_rate'.
        n_exclude (int): Number of worst months to exclude(not trade).
            If not None, threshold will be ignored.
        threshold (float): The min value required on the metric to enter
            long position. If n_exclude is not None, threshold will be ignored.

    Returns:
        pd.Series of monthly returns from trading monthly seasonality signals.
    """
    prc_lb = prices.loc[lb_start:lb_end]
    rtn_hd_m = calc_monthly_rtns(prices).loc[hd_start:hd_end]
    signals = calc_monthly_seasonality_signals(
        prc_lb, signal_metric, n_exclude, threshold)
    signals_this_yr = pd.Series({
        i: np.nan if i.month not in signals.index else signals[i.month]
        for i in rtn_hd_m.index
    })
    return signals_this_yr * rtn_hd_m


def backtest(prices,
             lookback=10,
             holding=1,
             signal_metric='return',
             n_exclude=2,
             threshold=None):
    """Wrapper function for backtesting trade_monthly_seasonality().
    Returns the monthly returns timeseries from trading seasonality.
    """
    trades = []
    lb_start = prices.index[0].year
    while True:
        lb_end = lb_start + lookback - 1
        hd_start = lb_end + 1
        hd_end = hd_start + holding - 1
        if hd_end > prices.index[-1].year:
            break
        trades.append(
            trade_monthly_seasonality(prices, str(lb_start), str(lb_end),
                                      str(hd_start), str(hd_end),
                                      signal_metric, n_exclude, threshold))
        lb_start += 1
    trades = pd.concat(trades)
    return trades
