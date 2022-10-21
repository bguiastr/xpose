#' Observations plotted against model predictions
#'
#' @description Plot of observations (DV) vs population predictions (PRED), 
#' individual predictions (IPRED) or conditional population predictions (CPRED).
#'
#' @param xpdb An xpose database object.
#' @param mapping List of aesthetics mappings to be used for the xpose plot 
#' (e.g. \code{point_color}).
#' @param group Grouping variable to be used for lines.
#' @param type String setting the type of plot to be used. Can be points 'p',
#' line 'l', smooth 's' and text 't' or any combination of the four.
#' @param title Plot title. Use \code{NULL} to remove.
#' @param subtitle Plot subtitle. Use \code{NULL} to remove.
#' @param caption Page caption. Use \code{NULL} to remove.
#' @param tag Plot identification tag. Use \code{NULL} to remove.
#' @param log String assigning logarithmic scale to axes, can be either '', 
#' 'x', y' or 'xy'.
#' @param guide Enable guide display (e.g. unity line).
#' @param facets Either a character string to use \code{\link[ggforce]{facet_wrap_paginate}}
#' or a formula to use \code{\link[ggforce]{facet_grid_paginate}}.
#' @param .problem The $problem number to be used. By default returns 
#' the last estimation problem.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param ... Any additional aesthetics to be passed on \code{xplot_scatter}.
#' 
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Faceting
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' dv_vs_pred(xpdb_ex_pk)
#' 
#' dv_vs_ipred(xpdb_ex_pk)
#' 
#' @name dv_vs_pred
#' @export
dv_vs_ipred <- function(xpdb,
                        mapping  = NULL,
                        group    = 'ID',
                        type     = 'pls',
                        title    = '@y vs. @x | @run',
                        subtitle = 'Ofv: @ofv, Eps shrink: @epsshk',
                        caption  = '@dir',
                        tag      = NULL,
                        log      = NULL,
                        guide    = TRUE,
                        facets,
                        .problem,
                        quiet,
                        ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet))   quiet <- xpdb$options$quiet
  if (missing(facets))  facets <- xpdb$xp_theme$facets
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = data_opt(.problem = .problem, filter = only_obs(xpdb, .problem, quiet)),
                mapping = aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'ipred')$col]], 
                                    y = .data[[xp_var(xpdb, .problem, type = 'dv')$col]]), mapping),
                type = type, guide = guide, facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption, 
                tag = tag, plot_name = as.character(match.call()[[1]]),
                guide_slope = 1, ...)
}

#' @rdname dv_vs_pred
#' @export
dv_vs_pred <- function(xpdb,
                       mapping  = NULL,
                       group    = 'ID',
                       type     = 'pls',
                       title    = '@y vs. @x | @run',
                       subtitle = 'Ofv: @ofv',
                       caption  = '@dir',
                       tag      = NULL,
                       log      = NULL,
                       guide    = TRUE,
                       facets,
                       .problem,
                       quiet,
                       ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (missing(facets))  facets <- xpdb$xp_theme$facets
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = data_opt(.problem = .problem, filter = only_obs(xpdb, .problem, quiet)),
                mapping = aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'pred')$col]], 
                                    y = .data[[xp_var(xpdb, .problem, type = 'dv')$col]]), mapping),
                type = type, guide = guide, facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = as.character(match.call()[[1]]),
                guide_slope = 1, ...)
}
