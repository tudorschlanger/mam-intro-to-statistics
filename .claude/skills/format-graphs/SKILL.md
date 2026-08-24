---
name: format-graphs
description: >
  Apply the project's consistent matplotlib figure style (scripts/plot_style.py).
  Use this skill whenever you are writing or modifying Python code that produces a
  figure, plot, chart, or graph (matplotlib/Polars `.plot`), or whenever the user
  asks to format, fix, restyle, or generate any figure. The parallel skill for
  LaTeX tables is format-tables. When in doubt, load this skill.
---

# Figure Style Guide

`scripts/plot_style.py` is the **single source of truth** for figure styling
(fonts, palette, spines, grid, DPI). Every plotting script must conform to it.
Apply these rules to every figure you write or modify, without exception.

---

## 1. The non-negotiable rule

```python
from scripts.plot_style import OKABE_ITO, apply_style, align_grid, percent_axis
apply_style()           # call ONCE, at module top, before any plotting
```

- Call `apply_style()` exactly once per script, at the top.
- **Never** override its font rcParams afterward. Do NOT write
  `matplotlib.rcParams.update({"font.size": ...})` or pass `fontsize=` to
  `set_xlabel`/`set_ylabel`/`set_title` to shrink/grow the global sizes. The
  canonical sizes are font 18, axes labels 18, title 20, legend 16. If a label
  does not fit, **shorten the label or resize the figure** — do not shrink the font.
- Per-element `fontsize=` is allowed only for *secondary annotations* (bar value
  labels, in-bar text) where a smaller size is intentional — never to defeat the
  global axis/label sizes.

## 2. Colors — Okabe-Ito only

Use the `OKABE_ITO` dict; never raw hex or matplotlib defaults (`C0`, `tab:blue`).

```python
OKABE_ITO = {"black", "orange", "sky_blue", "green",
             "yellow", "blue", "vermillion", "purple"}
```

- Two-series DB vs DC: `OKABE_ITO["blue"]` (DB) and `OKABE_ITO["vermillion"]`
  or `OKABE_ITO["orange"]` (DC). Pick one mapping and keep it consistent across a
  figure set.
- Multi-category stacked/área plots: cycle blue → orange → green → sky_blue →
  vermillion → purple (the order used by the look-through layer plots).

## 3. Grid — always `align_grid(ax)`

```python
align_grid(ax)          # dotted grey horizontal lines aligned to y-ticks, behind data
```

- Call `align_grid(ax)` on every axes (it is what enforces the house grid).
- **Gotcha:** `align_grid` sets a `MaxNLocator` on the y-axis. If you need fixed
  ticks, call `ax.set_yticks(...)` / `set_ylim` / `percent_axis(ax)` **after**
  `align_grid`, not before (see memory `align-grid-yticks`).

## 4. Spines & axes

- Top and right spines are removed by `apply_style()` — do not re-enable them.
- **Percentages — always the full 0–100% scale.** Call `percent_axis(ax)` *after*
  `align_grid(ax)`: it puts a tick every 10 percentage points (`0%, 10%, …, 100%`),
  **always shows and labels 100%**, and sets `ylim=[0, 1]`. Never zoom a share axis
  to its data range — that exaggerates variation and breaks comparability across a
  figure set. (Pass `top=`/`step=` only to override; `top>1` if a series exceeds 100%.)
- Thousands: `mticker.FuncFormatter(lambda v, _: f"{int(v):,}")`.
- Start counts at zero where meaningful: `ax.set_ylim(bottom=0)` (percent axes are
  pinned to `[0, 1]` by `percent_axis`).

## 5. Labels & titles

- Axis labels short enough to fit at 18pt (≈ ≤ 25 characters for a vertical
  y-label on a height-5 figure). Put the full definition in the figure's **LaTeX
  caption**, not the axis label.
- For figures embedded in the paper with a `\caption{}`, do **not** add an
  `ax.set_title()` (the caption is the title). Titles are for standalone /
  diagnostic plots only.
- **No redundant axis label for a time axis.** When the x-axis is a time dimension
  (years), do **not** set an x-axis label (`ax.set_xlabel("Year")`) — the year tick
  labels already make it self-evident. Keep the year ticks; drop the word "Year".
  (Same logic for any axis whose unit is obvious from its own tick labels.)

## 6. Figure size & saving

- Typical sizes in this repo: single panel `(8, 5)` or `(9, 5)`; wide time series
  `(8, 5)`; two-panel `(12, 5)`; many-category bar `(18, 8)`. Match a neighbour.
- Save vector PDF to a `scripts.paths` location (never hardcode):
  ```python
  fig.savefig(FIG_OVERLEAF / "subdir" / "name.pdf", bbox_inches="tight", dpi=300)
  plt.close(fig)
  ```
- `bbox_inches="tight"` on every save so nothing is clipped; `dpi=300`.
- `matplotlib.use("Agg")` at the top for headless/cluster runs.

## 7. Legends

- `apply_style()` already gives legends a white background, no edge, size 16.
- In-plot legend: `ax.legend(loc="best")`. Outside-plot (for busy charts):
  `ax.legend(bbox_to_anchor=(1.02, 1), loc="upper left", borderaxespad=0, framealpha=1.0)`.

## 8. Verify

After generating, **read the output PDF** and check: no clipped labels, legend
legible and inside/outside as intended, Okabe-Ito colors, dotted y-grid aligned to
ticks, top/right spines absent. On the cluster a `findfont: ... 'Palatino' not
found` warning is benign (DejaVu Serif fallback) — not a styling failure.

---

## Reference implementation

`scripts/compustat/4d_coverage_by_year.py` (the data-coverage line charts) is a
clean, minimal example: `apply_style()` with no overrides, `OKABE_ITO` colors,
`align_grid` + `percent_axis` (full 0–100% scale), short axis labels, no redundant
"Year" x-label, `bbox_inches="tight"`.
