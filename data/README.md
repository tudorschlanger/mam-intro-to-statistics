# Input Data

This directory holds all data sources from outside the project. Document each dataset here.

## Data Inventory

| File | Source | Date accessed | Format | Variables | Notes |
|------|--------|---------------|--------|-----------|-------|
| `raw/berkshire_market_monthly.csv` | Copied from the MGMT 924 course bundle (`docs/Class material MGMT 924/Class 4-6.../buffett.csv`); ultimate provenance not documented by the course | 2026-08-21 | csv | 6 | 563 monthly rows, Nov 1976 – Dec 2023. `eom`, `buffett_exc` (Berkshire excess return), `mkt_exc`, `smb`, `hml`, `rf`. The factor columns look like the Fama–French monthly set but the course does not say so — confirm before citing. Used by `scripts/R/part3_figures.R`. |
| `raw/stocks_bonds.csv` | Copied from the MGMT 924 course bundle (`docs/Class material MGMT 924/Class 2-3.../stocks_bonds.csv`) | 2026-08-24 | csv | 4 | 1,177 monthly rows, Dec 1925 – Dec 2023; both return series non-missing from May 1941 (992 months), `us_stock_market` alone from Jan 1926 (1,176 months). `eom`, `us_stock_market`, `treasury10yr`, `tbil`. Used by **all three** figure scripts: Part 1 (theta, the share of up months), Part 2 (covariance/correlation), Part 3 (equity premium, Sept-1931 anchor). |
| `raw/shiller/ie_data.xls` | Robert Shiller, <https://shillerdata.com> — see `raw/shiller/README.md` | 2026-08 | xls | sheet `Data` | Monthly 1871.01 – 2026.08 (n = 1,868). Part 1 reads column 8 (Real Price); Part 2 reads columns 5 (CPI) and 10 (real total return). Date is numeric `YYYY.MM`, so October reads as `.1` — recover the month by rounding, never by slicing the string. |

## Removed 2026-08-24

Deleted as unused; each was a byte-identical duplicate or a strict subset of data that
is still here, so nothing is lost:

| File | Why it was safe to delete |
|------|---------------------------|
| `raw/market_returns_daily.csv` (76 MB) | Byte-identical copy remains at `docs/Class material MGMT 924/Class 4-6.../market_returns_daily.csv`, where `class4_6.R` still uses it. Was Part 1's source for theta = 0.55 (share of up **days**); Part 1 now derives theta = 0.63 from `stocks_bonds.csv` (share of up **months**). |
| `raw/market_stocks_gov_bonds_monthly.csv` | Byte-identical to `raw/stocks_bonds.csv` (md5 `594953ea…`) — a pure duplicate under a second name. |
| `raw/market_index_monthly_shiller.csv` | Shiller total-return extract, 1871.01–2024.08 (n = 1,844). `raw/shiller/ie_data.xls` covers 1871.01–2026.08 (n = 1,868), a strict superset. |

Note: nothing in `docs/Class material MGMT 924/` was touched. Every file there is read by
that bundle's own scripts (`class1.R`, `class1.py`, `class2-3.R`, `class4_6.R`,
`Class 7-9 … Code.R`), so it is live reference material, not dead weight.

## Guidelines

- Never modify raw data files — all transformations happen in `scripts/`
- Document provenance: where the data came from, when it was accessed, any license terms
- If data is restricted, note the access conditions and do not commit sensitive files
- Use `.gitignore` for large binary files — consider git-lfs or store externally
