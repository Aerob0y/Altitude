



rbnz_series <- function(col = NULL, u = TRUE, filter_graph, filter_group = NULL, filter_series = NULL, split = NULL, dim = NULL, adj = NULL, names = NULL) {
  if (!is.null(col) && !(col %in% names(.cache$series))) {
    stop(sprintf("Unknown column '%s'. Try one of: %s",
                 col, paste(names(.cache$series), collapse = ", ")))
  }
  t <- .cache$series %>%
    filter(Graph == filter_graph) %>%
    filter(if (!is.null(filter_group)) Grouping == filter_group else TRUE) %>%
    filter(if (!is.null(filter_series)) Series.Id == filter_series else TRUE) %>%
    filter(if (!is.null(split)) Split == split else TRUE) %>%
    filter(if (!is.null(dim)) Dim %in% dim else TRUE) %>%
    filter(if (!is.null(adj)) Adj == adj else TRUE) %>%
    filter(if (!is.null(names)) Names == names else TRUE) %>%
    select(all_of(col))
  if (u) {t <- t  %>% unique()}
  t <- t %>%
    unlist() %>%
    as.vector()
  t
}


rbnz_plotly <- function(g, group = NULL, group_fallback = NULL,  split = NULL, split_fallback = NULL, adj = NULL, dim = NULL, dim_fallback = NULL, title = NULL, subtitle = NULL, names = NULL, names_fallback = NULL) {
  d <- .load_data(g)
  s <- .cache$series
  fallback <- FALSE

  # Apply filters
  if (!is.null(g))        {s <- s %>% filter(Graph %in% g)}
  if (!is.null(group)) {s <- s %>% filter(Grouping %in% group)}
  if (!is.null(split))    {s <- s %>% filter(Split %in% split)}
  if (!is.null(dim))      {s <- s %>% filter(Dim %in% dim)}
  if (!is.null(names))      {s <- s %>% filter(Names %in% names)}
  if (!is.null(adj))      {s <- s %>% filter(Adj %in% adj)}
  if (nrow(s) == 0) {
    s <- .cache$series
    fallback <- TRUE

    if (!is.null(group_fallback)) {group <- group_fallback}
    if (!is.null(split_fallback))    {split <- split_fallback}
    if (!is.null(dim_fallback))      {dim <- dim_fallback}
    if (!is.null(names_fallback))      {names <- names_fallback}
    if (!is.null(g))        {s <- s %>% filter(Graph %in% g)}
    if (!is.null(group)) {s <- s %>% filter(Grouping %in% group)}
    if (!is.null(split))    {s <- s %>% filter(Split %in% split)}
    if (!is.null(dim))      {s <- s %>% filter(Dim %in% dim)}
    if (!is.null(names))      {s <- s %>% filter(Names %in% names)}
    if (!is.null(adj))      {s <- s %>% filter(Adj %in% adj)}
    
    #if (!is.null(g))        {s <- s %>% filter(Graph %in% g)}
    #if (!is.null(group_fallback)) {s <- s %>% filter(Grouping %in% group_fallback)}
    #if (!is.null(split_fallback))    {s <- s %>% filter(Split %in% split_fallback)}
    #if (!is.null(dim_fallback))      {s <- s %>% filter(Dim %in% dim_fallback)}
    #if (!is.null(names_fallback))      {s <- s %>% filter(Names %in% names_fallback)}
  }
  unique_dims <- s %>% select(Dim) %>% unique()
  generic_plotly(
    data = d,
    series = s,
    k = "Date",
    t1 = title,
    t2 = subtitle,
    dim = unique_dims$Dim,
    fallback = fallback
  )
}

