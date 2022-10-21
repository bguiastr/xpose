#' Parameter value or gradient vs. iterations
#'
#' @description Change of parameter value or gradient vs. iterations.
#'
#' @inheritParams dv_vs_pred
#' @param .subprob The sub-problem number to be used. By default returns 
#' the last sub-problem associated with the selected problem.
#' @param .method The estimation method to be used, by default returns the last one for each file
#' 
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Faceting
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' prm_vs_iteration(xpdb_ex_pk)
#' 
#' grd_vs_iteration(xpdb_ex_pk)
#' 
#' @name minimization_plots
#' @export
prm_vs_iteration <- function(xpdb,
                             mapping  = NULL,
                             group    = 'variable',
                             type     = 'l',
                             title    = 'Parameter @y vs. @x | @run',
                             subtitle = 'Method: @method, minimization time: @runtime\nTermination message: @term',
                             caption  = '@dir',
                             tag      = NULL,
                             log      = NULL,
                             guide    = FALSE,
                             facets,
                             .problem,
                             .subprob,
                             .method,
                             quiet,
                             ...) {
  # Check input
  check_xpdb(xpdb, check = 'files')
  if (missing(.problem)) .problem <- last_file_problem(xpdb, 'ext')
  if (missing(.subprob)) .subprob <- last_file_subprob(xpdb, 'ext', .problem)
  if (missing(.method)) .method   <- last_file_method(xpdb, ext = 'ext', 
                                                      .problem = .problem, .subprob = .subprob)
  check_problem(.problem, .subprob, .method)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (missing(facets)) facets <- 'variable'
  x_var <- 'ITERATION'
  msg(c('Parameters non-varying across ', x_var, ' not shown.'), quiet)
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = data_opt(.problem = .problem, .subprob = .subprob,
                               .method = .method, .source = 'ext',
                               filter = function(x) {
                                 x <- x %>% 
                                   dplyr::filter(.[, x_var] >= 0) %>% 
                                   dplyr::select_if(.predicate = function(x) dplyr::n_distinct(x) > 1)
                                 if (ncol(x[, colnames(x) != x_var]) == 0) {
                                   stop('No parameters varying across ', x_var, ' were found.', call. = FALSE)
                                 }
                                 x
                               }, tidy = TRUE, index_col = x_var,
                               post_processing = reorder_factors(prefix = NA)),
                mapping = aes_c(aes(x = .data[[x_var]], y = .data[["value"]]), mapping),
                type = type, guide = guide, facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = as.character(match.call()[[1]]),
                scales = 'free_y', ...)
}

#' @rdname minimization_plots
#' @export
grd_vs_iteration <- function(xpdb,
                             mapping  = NULL,
                             group    = 'variable',
                             type     = 'l',
                             title    = 'Gradient @y vs. @x | @run',
                             subtitle = 'Method: @method, minimization time: @runtime\nTermination message: @term',
                             caption  = '@dir',
                             tag      = NULL,
                             log      = NULL,
                             guide    = FALSE,
                             facets,
                             .problem,
                             .subprob,
                             .method,
                             quiet,
                             ...) {
  # Check input
  check_xpdb(xpdb, check = 'files')
  if (missing(.problem)) .problem <- last_file_problem(xpdb, 'grd')
  if (missing(.subprob)) .subprob <- last_file_subprob(xpdb, 'grd', .problem)
  if (missing(.method)) .method   <- last_file_method(xpdb, ext = 'grd', 
                                                      .problem = .problem, .subprob = .subprob)
  check_problem(.problem, .subprob, .method)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (missing(facets)) facets <- 'variable'
  x_var <- 'ITERATION'
  msg(c('Parameters non-varying across ', x_var, ' not shown.'), quiet)
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = data_opt(.problem = .problem, .subprob = .subprob, 
                               .method = .method, .source = 'grd',
                               filter = function(x) {
                                 x <- x %>% 
                                   dplyr::filter(.[, x_var] >= 0) %>% 
                                   dplyr::select_if(.predicate = function(x) dplyr::n_distinct(x) > 1)
                                 if (ncol(x[, colnames(x) != x_var]) == 0) {
                                   stop('No parameters varying across ', x_var, ' were found.', call. = FALSE)
                                 }
                                 x
                               }, tidy = TRUE, index_col = x_var, 
                               post_processing = reorder_factors(prefix = 'GRD(', suffix = ')')),
                mapping = aes_c(aes(x = .data[[x_var]], y = .data[["value"]]), mapping),
                type = type, guide = guide, facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = as.character(match.call()[[1]]),
                scales = 'free_y', ...)
}
