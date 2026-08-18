mod_download_ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "dl-toolbar",
    tags$div(
      class = "dl-format",
      selectInput(ns("format"), NULL, c("png","jpeg","svg"),
                  selected = "png", width = "70px", selectize = FALSE)
    ),
    tags$div(
      class = "dl-width",
      numericInput(ns("width"), NULL, 1920, min = 400, step = 1, width = "80px")
    ),
    tags$div(
      class = "dl-height",
      numericInput(ns("height"), NULL, 1080, min = 400, step = 1, width = "80px")
    ),
    tags$div(
      class = "dl-clean",
      checkboxInput(ns("clean"), "Clean", FALSE)
    )
  )
}

mod_download_server <- function(id, defaults = list(format = "png", width = 1920, height = 1080, scale = 2, clean = FALSE)) {
  moduleServer(id, function(input, output, session) {

    # set defaults once
    observeEvent(TRUE, {
      updateSelectInput(session, "format", selected = defaults$format)
      updateNumericInput(session, "width",  value = defaults$width)
      updateNumericInput(session, "height", value = defaults$height)
      updateCheckboxInput(session, "clean", value = defaults$clean)
    }, once = TRUE)

    download <- reactive(list(
      format = input$format %||% defaults$format,
      width  = input$width  %||% defaults$width,
      height = input$height %||% defaults$height,
      scale  = defaults$scale
    ))

    clean_export <- reactive(isTRUE(input$clean))

    list(download = download, clean_export = clean_export)
  })
}

download_button <- function(p) {
  p |> plotly::config(
    #modeBarButtonsToRemove = list("toImage"),
    modeBarButtonsToAdd = list(
      list(
        name  = "Download…",
        title = "Choose file type & resolution",
        icon  = htmlwidgets::JS("Plotly.Icons.camera"),
        click = htmlwidgets::JS(
"function(gd){
  var fmt = (prompt('File type: png / jpeg / svg', 'png') || 'png')
              .toLowerCase().trim();
  if (fmt === 'jpg') fmt = 'jpeg';
  if (['png','jpeg','svg'].indexOf(fmt) === -1) fmt = 'png';

  var w = parseInt(prompt('Width (px)', '2000') || '2000', 10);
  var h = parseInt(prompt('Height (px)', '1200') || '1200', 10);
  var s = parseFloat(prompt('Scale (e.g., 1, 2, 3)', '2') || '2');

  if (!isFinite(w) || w < 100) w = 2000;
  if (!isFinite(h) || h < 100) h = 1200;
  if (!isFinite(s) || s <= 0) s = 2;

  var opts = {format: fmt, width: w, height: h, scale: s};

  var orig = {
    rs: gd.layout.xaxis && gd.layout.xaxis.rangeselector ? gd.layout.xaxis.rangeselector.visible : true,
    rsl: gd.layout.xaxis && gd.layout.xaxis.rangeslider ? gd.layout.xaxis.rangeslider.visible : false
  };

  Plotly.relayout(gd, {
    'xaxis.rangeselector.visible': false,
    'xaxis.rangeslider.visible': false
  })
  .then(function(){ return Plotly.downloadImage(gd, opts); })
  .then(function(){
    return Plotly.relayout(gd, {
      'xaxis.rangeselector.visible': orig.rs,
      'xaxis.rangeslider.visible': orig.rsl
    });
  });
}"
        )
      )
    )
  )
}


download_settings_ui <- function(ns) {
  tags$div(
    class = "dl-wrap",
    actionLink(
      ns("dl_toggle"),
      label = "Download Settings",
      icon = icon("camera"),
      class = "dl-gear"
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] %% 2 == 1", ns("dl_toggle")),
      tags$div(
        class = "dl-panel",
        mod_download_ui(ns("dl"))
      )
    )
  )
}