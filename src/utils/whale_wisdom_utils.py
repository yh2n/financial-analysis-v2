import numpy as np

import pandas as pd


def get_fehf_holdings(holdings, fehf_ids, use_names=False):
    """Get just the holdings data corresponding to the FEHFs, as
    determined by `fehf_ids`.

    `fehf_ids` could also be a collection of names, in which case
    `use_names` should be set to True

    Parameters
    ----------
    holdings : pd.DataFrame
    fehf_ids : list of int or list of str
        IDs or strings to identify the FEHFs
    use_names : bool, default False
        Treat `fehf_ids` as a collection of names

    Returns
    -------
    pd.DataFrame
        Holdings of the FEHF funds
    """
    key = 'filer_name' if use_names else 'filer_id'
    fehf_holdings = holdings.loc[holdings[key].isin(fehf_ids)]
    missing_ids = set(fehf_ids) - set(fehf_holdings[key])
    missing_names = holdings.loc[
        holdings[key].isin(missing_ids), 'filer_name'].unique()
    print(f'The following names are FEHF but not in holdings: {missing_names}')
    return fehf_holdings


def median_holdings_each_quarter(holdings):
    return holdings \
        .groupby(['filer_name', 'quarter']) \
        .count()['filer_id'] \
        .groupby('quarter') \
        .median()


def median_size_each_quarter(holdings):
    return holdings \
        .groupby(['filer_name', 'quarter']) \
        ['current_percent_of_portfolio'].median() \
        .groupby('quarter') \
        .median()


def pct_in_top_k(holdings, k):
    def _fn(filer_quarter):
        topk = filer_quarter[filer_quarter['current_ranking'] < k]
        return topk['current_percent_of_portfolio'] \
            .sort_values() \
            .sum()
    return holdings \
        .groupby(['filer_name', 'quarter']) \
        .apply(_fn)


def turnover(holdings):
    """Calculate ticker-based turnover metric for each fund for each quarter.

    Only ticker-based. So regardless of the position size, it wont be
    counted as turned-over if it is still in the holdings. Values from this
    will likely be higher than whatever is ideal.

    Parameters
    ----------
    holdings : pd.DataFrame

    Returns
    -------
    pd.Series
        index is a pd.MultiIndex of (filer_name, quarter). Values are turnover,
        which is in [0, 1].
    """
    def _fn(all_for_fund):
        by_quarter = all_for_fund.groupby('quarter')['stock_ticker']
        out = pd.Series(dtype=float)

        last_q_holdings = None
        current_q_holdings = None

        for date, quarter_holdings in by_quarter:
            current_q_holdings = set(quarter_holdings)
            if last_q_holdings is not None:
                out[date] = len(last_q_holdings - current_q_holdings)
                out[date] /= len(last_q_holdings)
            else:
                out[date] = np.nan

            last_q_holdings = current_q_holdings
        return out

    return holdings.groupby('filer_name').apply(_fn)
