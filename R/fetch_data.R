#' Create options for data import
#' 
#' @description Provide a list of options to the general plotting functions such as 
#' \code{xplot_scatter} in order to create appropriate data input for ggplot2.
#' 
#' @param .problem The problem to be used, by default returns the last one.
#' @param .subprob The subproblem to be used, by default returns the last one.
#' @param .method The estimation method to be used, by default returns the last one.
#' @param .source Define the location of the data in the xpdb. Should be either 'data' 
#' to use the output tables or the name of an output file attached to the xpdb.
#' @param simtab Only used when 'data' is defined as the source and `.problem` is default. Should the data be coming 
#' from an estimation or a simulation table.
#' @param filter A function used to filter the data e.g. filter = function(x) x[x$TIME > 20, ] where x is the data.
#' @param tidy Logical, whether the data should be transformed to tidy data.
#' @param index_col Only used when 'tidy' is defined a \code{TRUE} and \code{value_col} is \code{NULL}. 
#' Column names to use as index when tidying the data.
#' @param value_col Only used when 'tidy' is defined a \code{TRUE} and \code{index_col} is \code{NULL}. 
#' Column names to be stacked when tidying the data.
#' @param post_processing A function used to modify the data after it has been tidied up e.g. post_processing = function(x) 
#' dplyr::mutate(.data = x, variable = as.factor(.$variable)) where x is the tidy data.
#'
#' @seealso \code{\link{xplot_distrib}} \code{\link{xplot_qq}} \code{\link{xplot_scatter}} 
#' 
#' @examples
#' data_opt(.problem = 1, .source = 'data', simtab = TRUE)
#' 
#' @export
data_opt <- function(.problem        = NULL, 
                     .subprob        = NULL, 
                     .method         = NULL,
                     .source         = 'data', 
                     simtab          = FALSE,
                     filter          = NULL,
                     tidy            = FALSE,
                     index_col       = NULL,
                     value_col       = NULL,
                     post_processing = NULL) {
  list(problem = .problem, subprob = .subprob, method = .method, 
       source = .source, simtab = simtab, filter = filter, tidy = tidy, 
       index_col = index_col, value_col = value_col,
       post_processing = post_processing)
}


#' Create functions to drop non observation records
#' 
#' @description Create shortcut functions on the fly to remove records 
#' not associated with an observation.
#' 
#' @param xpdb An xpose database object.
#' @param .problem The $problem number to be used.
#' @param quiet Should messages be displayed to the console.
#' 
#' @return A function
#' 
#' @keywords internal
#' @export
only_obs <- function(xpdb, .problem, quiet) {
  mdv_var <- xp_var(xpdb, .problem, type = c('evid', 'mdv'), silent = TRUE)$col[1]
  fun <- function(x) {}
  if (!is.null(mdv_var)) {
    string <- c('Filtering data by ', mdv_var, ' == 0')
    body(fun) <- bquote({
      msg(.(string), .(quiet))
      x[as.vector(x[, .(mdv_var)] == 0), ]
    })
  } else {
    string <- c('No `evid` or `mdv` variable available to filter the data.')
    body(fun) <- bquote({
      msg(.(string), .(quiet))
      x
    }) 
  }
  fun
}


#' Create functions for data deduplication
#' 
#' @description Create shortcut functions on the fly to remove duplicated records in data.
#' 
#' @param xpdb An xpose database object.
#' @param .problem The $problem number to be used.
#' @param facets The plot faceting variable. The `facets` variables along with the `id` column 
#' type will be as grouping factors during data deduplication process.
#' @param quiet Should messages be displayed to the console.
#' 
#' @return A function
#' 
#' @keywords internal
#' @export
only_distinct <- function(xpdb, .problem, facets, quiet) {
  if (is.formula(facets)) facets <- all.vars(facets)
  vars <- c(xp_var(xpdb, .problem, type = c('id'))$col[1], facets)
  
  
  fun <- function(x) {}
  body(fun) <- bquote({
    
    var_stg <- .(vars)
    
    # Silently remove "variable" when not in the data
    if ('variable' %in% var_stg && !'variable' %in% colnames(x)) {
      var_stg <- var_stg[-which(var_stg == 'variable')]
    }
    
    msg_stg <- c('Removing duplicated rows based on: ', 
                 stringr::str_c(var_stg, collapse = ', '))
    msg(msg_stg, .(quiet))
    
    dplyr::distinct(.data = x, !!!rlang::syms(var_stg), .keep_all = TRUE)
  })
  
  fun
  
}


#' Reorder factors by numerical order
#' 
#' @description Will for example convert `ETA(1)` to 1 create factors then generate labels
#' by wrapping the digits with prefix and suffix.
#' 
#' @param prefix A prefix to be added in front of the factor digits.
#' @param suffix A suffix to be added after the factor digits.
#' 
#' @return A modified tibble
#' 
#' @keywords internal
#' @export
reorder_factors <- function(prefix, suffix = NULL) {
  if (!is.na(prefix)) {
    # Sort and reformat factors
    function(x) {
      x %>% 
        dplyr::mutate(variable = as.numeric(gsub('\\D', '', .$variable))) %>% 
        dplyr::mutate(variable = factor(.$variable, levels = sort(unique(.$variable)),
                                        labels = stringr::str_c(prefix, sort(unique(.$variable)), suffix)))
    }
  } else {
    # Only sort factors
    function(x) {
      levels <- x %>%
        dplyr::distinct(!!rlang::sym('variable')) %>%
        dplyr::mutate(variable_order = substring(.$variable, 1, 2)) %>%
        dplyr::mutate(variable_order = dplyr::case_when(.$variable_order == 'TH' ~ 1,
                                                        .$variable_order == 'OM' ~ 2,
                                                        .$variable_order == 'SI' ~ 3,
                                                        TRUE ~ 0)) %>%
        dplyr::arrange_at(.vars = 'variable_order')
      
      dplyr::mutate(.data = x, variable = factor(x$variable, levels = levels$variable))
    }
  }
}


#' Fetch data
#' 
#' @description Main internal function to get the data from different source and 
#' prepare it for plotting. Arguments are usually provided by `data_opt()`.
#' 
#' @inheritParams data_opt
#' @param xpdb An xpose database object.
#' @param quiet Should messages be displayed to the console.
#' 
#' @return A tibble
#' 
#' @keywords internal
#' @export
fetch_data <- function(xpdb, 
                       .problem  = NULL, 
                       .subprob  = NULL,
                       .method   = NULL,
                       .source   = 'data', 
                       simtab    = FALSE,
                       filter    = NULL,
                       tidy      = FALSE, 
                       index_col = NULL,
                       value_col = NULL,
                       post_processing = NULL,
                       quiet     = FALSE) {
  
  if (.source == 'data') {
    if (is.null(.problem)) .problem <- last_data_problem(xpdb, simtab)
    if (is.na(.problem)) {
      stop(c('No data associated with $prob no.', .problem, ' could be found.'), call. = FALSE)
    }
    data <- get_data(xpdb, .problem = .problem)
    msg(c('Using data from $prob no.', .problem), quiet)
  } else {
    if (!any(xpdb$files$extension == .source)) {
      stop(c('File extension `.', .source, '` not found in model output files.'), call. = FALSE) 
    }
    if (is.null(.problem)) .problem <- last_file_problem(xpdb, .source)
    if (is.null(.subprob)) .subprob <- last_file_subprob(xpdb, .source, .problem)
    if (is.null(.method)) .method  <- last_file_method(xpdb, .source, .problem, .subprob)
    data <- get_file(xpdb, file = NULL, ext = .source, .problem = .problem, 
                     .subprob = .subprob, .method = .method, quiet = TRUE)
    msg(c('Using ', xpdb$files$name[xpdb$files$extension == .source][1] , ' $prob no.', .problem, 
          ', subprob no.', .subprob, ', method ', .method, '.'), quiet)
  }
  
  if (is.function(filter)) data <- filter(data)
  
  if (tidy) {
    if (!is.null(value_col)) { 
      index_col <- colnames(data)[!colnames(data) %in% value_col]
    }
    
    dplyr::if_else(length(index_col) > 5, 
                   stringr::str_c(stringr::str_c(index_col[1:5], collapse = ', '), 
                                  '... and', length(index_col) - 5 , 'more variables', sep = ' '),
                   stringr::str_c(index_col , collapse = ', ')) %>%
      {msg(c('Tidying data by ', .), quiet)}
    
    data <- tidyr::gather(data = data, key = 'variable', value = 'value',
                          !!!rlang::syms(colnames(data)[!colnames(data) %in% index_col]))
  }
  
  if (is.function(post_processing)) data <- post_processing(data)
  
  # Add metadata to output
  attributes(data) <- c(attributes(data), 
                        list(problem = .problem, simtab = simtab,
                             subprob = .subprob, method = .method, 
                             source = .source))
  data
}
