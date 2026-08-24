# Shiller US Stock Market Data

**Source:** [shillerdata.com](http://www.econ.yale.edu/~shiller/data.htm)

**File:** `ie_data.xls`

---

## Details

US Stock market data used in my book, Irrational Exuberance [Princeton
University Press 2000, Broadway Books 2001, 2nd ed., 2005, 3rd ed. 2015] as
updated are available for download below. This data set consists of monthly
stock price, dividends, and earnings data and interest rates and the consumer
price index (to allow conversion to real values), starting January 1871. The
price, dividend, and earnings series are from the same sources as described in
Chapter 26 of my book (Market Volatility [Cambridge, MA: MIT Press, 1989]),
although now I provide monthly data, rather than annual data. Monthly dividend
and earnings data are computed from the S&P four-quarter totals for the quarters
since 1926, with linear interpolation to monthly figures. Dividend and earnings
data before 1926 are from Cowles and Associates (Common Stock Indexes, 2nd ed.
[Bloomington, Ind.: Principia Press, 1939]), interpolated from annual data.
Stock price data are monthly averages of daily closing prices. The CPI-U
(Consumer Price Index-All Urban Consumers) published by the U.S. Bureau of Labor
Statistics begins in 1913; for years before 1913 I spliced to the CPI Warren and
Pearson's price index, by multiplying it by the ratio of the indexes in January
1913. See George F. Warren and Frank A. Pearson, Gold and Prices (New York: John
Wiley and Sons, 1935). Data are from their Table 1, pp. 11–14.

---

## File Layout (verified 2026-08-17)

Sheet **`Data`**. Rows 1–7 are title and multi-line column headers; the data
begins on **row 8**.

| Column | Row-7 label | Contents |
|--------|-------------|----------|
| 1 | `Date` | Year and month encoded as `YYYY.MM` (see trap below) |
| 2 | `P` | S&P Composite price, **nominal**, monthly average of daily closes |
| 3 | `D` | Dividend |
| 4 | `E` | Earnings |
| 5 | `CPI` | Consumer Price Index |
| 6 | `Date Fraction` | Date as a decimal year |
| 7 | `Rate GS10` | Long interest rate |
| 8 | `Real Price` | Column 2 deflated by CPI |
| 9 | `Real Dividend` | |
| 10 | `Real Total Return Price` | Price **plus reinvested dividends**, real |
| 11 | `Real Earnings` | |

Coverage: **1871.01 – 2026.08**, 1,868 monthly observations (1,867 returns).

Other sheets in the workbook: `Disclaimer`, `Index Plot`, `PE (CAPE) Plot`,
`Excess CAPE Yield (ECY)`.

### Trap: the date column

`Date` is stored as a number, not text, so October reads as `1871.1`, and
floating-point noise appears in recent rows (`2026.0599999999999`). Recover the
month by rounding, never by string-slicing:

```r
yr <- floor(date_num)
mo <- round((date_num - yr) * 100)
```

Reading the column as text and re-parsing collapses October to month 1.

### Trailing rows

The sheet carries a few trailing rows of notes below the data. Filter on
non-missing numeric date **and** price rather than reading to the last row.

---

## Which price column?

The three price columns answer different questions and give different numbers:

| Series | Column | Mean monthly return | SD |
|--------|--------|--------------------|-----|
| Nominal price | 2 | 0.48% | 4.04% |
| Real price | 8 | 0.31% | 4.07% |
| Real total return | 10 | — | — |

A **price** index excludes dividends, so it understates what an investor
actually earned by roughly the dividend yield. A **total return** index includes
them. State which one a figure uses whenever the level of returns matters;
for the shape of the distribution the choice barely moves anything.

---

## Usage in This Project

Read by `scripts/R/part1_figures.R` for the Part 1 empirical return
distribution. See that script for the exact columns and filters applied.
