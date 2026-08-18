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
