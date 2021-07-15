def bootstrap(returns, statistic_fn, size, num_samples):
    """
    :returns: Series of returns to sample from
    :statistic_fn: Function expecting a single series argument
        and returning a scalar value
    :size: Size of each bootstrap sample
    :num_samples: Number of bootstrap sample to return

    Returns a list of statistics calculated on each bootstrap sample
    """
    returns = returns[~returns.isna()]
    return [
        statistic_fn(returns.sample(size, replace=True))
        for _ in range(num_samples)
    ]
