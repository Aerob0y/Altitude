fp <- FALSE
# Altitude/
# │
# ├─ interactive                    # top level checker
# │
# ├─ app/                           # the app lives there
# │  ├─ utils                       # utilities for data ----
# │  │  ├─ <Code>                   ----
source("app/utils/core/load_utils.r") # shared dependencies and helpers
functions_by_file("app/utils/load_utils.r", fp = fp)
functions_by_file("app/utils/plotting/standards.r", fp = fp)
functions_by_file("app/utils/shiny/download.r", fp = fp)
functions_by_file("app/utils/data/data.r", fp = fp)
functions_by_file("app/utils/plotting/plotly.r", fp = fp) # not done
functions_by_file("app/utils/plotting/standard_plot.r", fp = fp) # not done
functions_by_file("app/utils/core/launch_module.r", fp = fp) # not done

# │  │  ├─ </Code>                  ----
# │  │  ├─ core                     # utilities for data 
# │  │  │   ├─ load_utils.r         # utilities for loading data
# │  │  │   ├─ dependencies.r       # required libraries
# │  │  │   └─ launch_module.r      # utility to launch a module in isolation
# │  │  ├─ data
# │  │  │   └─ data                 # utilities for data
# │  │  ├─ plotting
# │  │  │   ├─ plotly.r             # original plotly wrapper
# │  │  │   └─ standard_plot.r      # new standard plot wrapper
# │  │  └─ shiny
# │  │      └─ download.r           # download button utilities
# │  ├─ ui                          # utilities for data ----
# │  │  ├─ <Code>                   ----
source("app/ui/ui_elements.r") # common UI elements
functions_by_file("app/ui/ui_elements.r", fp = fp)
source("app/ui/ui_slides.r") # slides and overview
functions_by_file("app/ui/ui_slides.r", fp = fp)
# │  │  ├─ </Code>                  ----
# │  │  ├─ ui_elements.r        # common UI elements
# │  │  └─ ui_slides.r          # tabbed UI elements
# │
# ├─ app.r
# ├─ runapp.r
# ├─ server.r
# │  ├─ <Code>                   ----
source("app/server.r")
# │  └─ </Code>                  ----
# ├─ ui.r
# │  │  <Code>                   ----
source("app/ui.r")
# │  └─ </Code>                  ----
# └─ module_launcher.r

print(available_modules)

launch_module("hm14", suffix = "_update")
