# Dashboard modules

Each dashboard tab follows the same small Shiny module contract so that its loading behaviour, controls, notes, and visual treatment remain predictable.

## UI

1. Create a namespaced ID with `NS(id)`.
2. Read selector choices from the appropriate series guide or cached dataset.
3. Collect controls in a `tagList()`.
4. Return `ui_single(..., module = "dataset_key")`. The module key selects the title, source, and plain-language note from `module_notes` in `app/ui/ui_elements.r`; it also applies the shared module layout styled in `app/www/overview.css`.

## Server

1. Accept `id`, `selected_tab`, and `activate_on`.
2. Define `enabled <- reactive(identical(selected_tab(), activate_on))` inside `moduleServer()`.
3. Load data lazily with `eventReactive(enabled(), ...)` and guard work with `req(enabled())`.
4. Build the chart in a reactive expression and render it with `renderPlotly()`.
5. Keep selection limits and fallback values close to the inputs they protect.

When adding a module, add its display note to `module_notes`, pass the same key to `ui_single()`, and register both its UI and server in the application.
