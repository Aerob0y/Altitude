# Altitude
Altitude/
│
├─ interactive                    # top level checker
│
│
├─ app/                           # the app lives there
│  ├─ data                        # all data lives here
│  │  └─ *                        
│  ├─ modules                     # each graph lives here in a module to be served
│  │  └─ *                        
│  ├─ rsconnect/                  # connection files
│  │  └─ files                    
│  ├─ ui/                         # utilities for ui
│  ├─ utils                       # utilities for data
│  ├─ app.r                       # run_app() that builds UI & server from modules
│  ├─ server.r                    # primary server
│  └─ ui.r                        # primary ui
│
├─ standalone/                    # ready to go stand alone code
│
├─ update/
│  ├─ update.r                    # run the update sequence, load utils, 
│  ├─ update_utilities            # utilities for update
│  │  ├─ update_libraries.r       # required librarires
│  │  ├─ update_etag.r            #      
│  │  └─ export_plotly_spec.r/    #
│  └─ update_modules              # each updates a dataset
│     ├─ update_adp.r             # gets the accomidation data
│     ├─ update_bond              # gets bond data
│     ├─ update_border            # gets border data
│     ├─ update_ect               # gets ect data
│     ├─ update_fuel              # gets fuel data
│     └─ update_rbnz              # gets rbnz data
│               
├─ .lintr                         # you already have this — keep it
├─ .Rbuildignore                  # ignore inst/app/www dev bits, etc.
├─ .gitignore
└─ README.md


python -m http.server 8000
http://localhost:8000/embed_filter.html
