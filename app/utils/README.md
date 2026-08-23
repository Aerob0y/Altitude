# Application utilities

Utilities are grouped by responsibility rather than by a common `utils_`
filename prefix:

- `core/` loads package dependencies used throughout the application.
- `data/` contains data loading, caching, filtering, and reference metadata.
- `plotting/` contains the shared Plotly theme and chart-building helpers.
- `shiny/` contains reusable Shiny UI and server helpers.

Source `load_utils.r` instead of sourcing individual files. The loader keeps
cross-file dependencies explicit and provides one entry point for application
startup.

## Reference-driven plotting

`reference_plot()` is a parallel alternative to `x_plotly()`; the existing
function and modules are unchanged. It reads all chart choices from a small R
list or JSON file, normalises wide and long data to the same trace structure,
and then applies a single Plotly layout. See
`app/config/plot_reference.example.json` for both data shapes.

```r
# Choose one named chart from a JSON reference file.
p <- reference_plot(
  data = balances,
  reference = "app/config/plot_reference.example.json",
  chart = "wide_example"
)

# A list is convenient while interactively designing a chart.
p <- reference_plot(
  long_data,
  list(
    title = "Employment",
    format = "long",
    x = "Date",
    value = "Value",
    series = "Industry",
    style = "area",
    stack = TRUE
  )
)
```

For **wide data**, list each plotted column in `traces`. Each trace can set
`name`, `style` (`line`, `dashed`, `bar`, or `area`), `colour`, `axis` (`y` or
`y2`), and `stack`. For **long data**, set `value` and `series`; optionally use
`include` to select and order categories. Top-level `stack` switches bars
between grouped and stacked and gives line/area traces a shared stack group.

The **Investment (reference test)** navigation tab is a live comparison. It
loads the same HM3 dataset and exposes the same two investment-type selectors
as the existing **Investment** tab, but reads its trace definitions from
`app/config/hb3_test.json` and renders them with `reference_plot()`.
