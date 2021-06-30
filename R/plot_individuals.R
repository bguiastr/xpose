#' Observations, individual predictions and population predictions plotted against 
#' the independent variable for every individual
#'
#' @description Observations (DV), individual predictions (IPRED) and population predictions 
#' (PRED) plotted against the independent variable for every individual
#' 
#' @inheritParams dv_vs_pred
#' @param color Changes the **lines, points and text** color. Should be a vector of 3 values 
#' (i.e. DV, IPRED, PRED). This \code{color} argument is a special case in xpose as it applies 
#' to three different layers (\code{geom_line}, \code{geom_point} and \code{geom_text}). This 
#' special case is due to the fact that in ggplot2 it is not possible to have two different 
#' color scales for different layers.
#' @param point_alpha Points alpha, should be a vector of 3 values (i.e. DV, IPRED, PRED).
#' @param line_linetype Lines linetype, should be a vector of 3 values (i.e. DV, IPRED, PRED).
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Faceting
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' \dontrun{
#' # Basic example
#' ind_plots(xpdb_ex_pk, page = 1,
#'           ncol = 2, nrow = 2)
#' }
#' @export
ind_plots <- function(xpdb,
                      mapping       = NULL,
                      group         = 'variable',
                      type          = 'lp',
                      title         = 'Individual plots | @run',
                      subtitle      = 'Ofv: @ofv, Eps shrink: @epsshk',
                      caption       = '@dir | Page @page of @lastpage',
                      tag           = NULL,
                      log           = NULL,
                      facets,
                      .problem,
                      quiet,
                      color         = c('grey60', 'deepskyblue4', 'deepskyblue3'),
                      point_alpha   = c(0.8, 0, 0),
                      line_linetype = c('blank', 'solid', '55'),
                      ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets,
                                               variable = xp_var(xpdb, .problem, type = 'id')$col)
  
  extra_args <- list(...)
  if (!any(names(extra_args) == 'nrow')) extra_args$nrow <- 3
  if (!any(names(extra_args) == 'ncol')) extra_args$ncol <- 3
  if (any(names(extra_args) %in% c('line_color', 'point_color', 'text_color'))) {
    warning('In ind_plots the argument `color` should be used instead of `line/point/text_color`.', call. = FALSE)
  }
  
  variable_names <- xp_var(xpdb, .problem, type = c('dv', 'pred', 'ipred'))$col
  
  do.call('xplot_scatter', 
          c(extra_args, 
            list(xpdb = xpdb, group = group, quiet = quiet,
                 opt = data_opt(.problem = .problem, tidy = TRUE,
                                filter = only_obs(xpdb, .problem, quiet),
                                value_col = variable_names,
                                post_processing = function(x) {
                                  dplyr::mutate(.data = x, variable = factor(x$variable, levels = variable_names))
                                }),
                 mapping = aes_c(aes_string(x = xp_var(xpdb, .problem, type = 'idv')$col, 
                                            y = 'value', line_color = 'variable', text_color = 'variable',
                                            line_linetype = 'variable', point_color = 'variable', 
                                            point_alpha = 'variable'), mapping),
                 type = type, facets = facets,
                 xscale = check_scales('x', log),
                 yscale = check_scales('y', log), 
                 title = title, subtitle = subtitle, caption = caption, 
                 tag = tag, plot_name = as.character(match.call()[[1]])))) +
    scale_alpha_manual(values = point_alpha) +
    scale_color_manual(values = color) +
    scale_linetype_manual(values = line_linetype)
}
