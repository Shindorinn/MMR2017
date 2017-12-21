import pandas as pd
import numpy as np

'''
Code from R:
function (x, na.rm = FALSE, type = 3)
{
  if (any(ina <- is.na(x))) {
    if (na.rm)
      x <- x[!ina]
    else return(NA)
  }
  if (!(type %in% (1:3)))
    stop("Invalid 'type' argument.")
  n <- length(x)
  x <- x - mean(x)
  y <- sqrt(n) * sum(x^3)/(sum(x^2)^(3/2))
  if (type == 2) {
    if (n < 3)
      stop("Need at least 3 complete observations.")
    y <- y * sqrt(n * (n - 1))/(n - 2)
  }
  else if (type == 3)
    y <- y * ((1 - 1/n))^(3/2)
  y
}
'''


def mean():
    pass


def sd():
    pass


def skewness(x, remove_NAN=False, skew_type=3):
    if any(pd.isnull(x)):
        if remove_NAN:
            x.dropna(inplace=True)
        else:
            return None
    if skew_type not in [1, 2, 3]:
        raise ValueError('Invalid type argument.')
    n = x.shape[0]
    vals = x.values - np.mean(x.values)
    y = np.sqrt(n) * np.sum(vals**3)/(np.sum(x**2)**(3.0/2.0))
    if skew_type == 2:
        if n <3:
            raise ValueError('Need at least 3 complete observations')
        y *= np.sqrt(n*(n-1))/(n-2)
    elif skew_type == 3:
        y *= (1.0 - 1.0/float(n))**(3.0/2.0)
    return y
