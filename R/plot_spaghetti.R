#' Observations and model predictions plotted against the independent variable
#'
#' @description Plot of observations (DV), individual model predictions (IPRED) 
#' and/or population predictions (PRED) plotted against the independent variable (IDV).
#'
#' @inheritParams dv_vs_pred
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Faceting
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' dv_vs_idv(xpdb_ex_pk)
#' 
#' ipred_vs_idv(xpdb_ex_pk)
#' 
#' pred_vs_idv(xpdb_ex_pk)
#' 
#' dv_preds_vs_idv(xpdb_ex_pk)
#' 
#' @name pred_vs_idv
#' @export
dv_vs_idv <- function(xpdb,
                      mapping  = NULL,
                      group    = 'ID',
                      type     = 'pls',
                      title    = '@y vs. @x | @run',
                      subtitle = 'Ofv: @ofv',
                      caption  = '@dir',
                      tag      = NULL,
                      log      = NULL,
                      facets,
                      .problem,
                      quiet,
                      ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (missing(facets)) facets <- xpdb$xp_theme$facets
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = data_opt(.problem = .problem, 
                               filter = only_obs(xpdb, .problem, quiet)),
                mapping = aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'idv')$col]], 
                                    y = .data[[xp_var(xpdb, .problem, type = 'dv')$col]]), mapping),
                type = type, facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = as.character(match.call()[[1]]), ...)
}


#' @name pred_vs_idv
#' @export
ipred_vs_idv <- function(xpdb,
                         mapping  = NULL,
                         group    = 'ID',
                         type     = 'pls',
                         facets,
                         title    = '@y vs. @x | @run',
                         subtitle = 'Ofv: @ofv, Eps shrink: @epsshk',
                         caption  = '@dir',
                         tag      = NULL,
                         log      = NULL,
                         .problem,
                         quiet,
                         ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (missing(facets)) facets <- xpdb$xp_theme$facets
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = data_opt(.problem = .problem, 
                               filter = only_obs(xpdb, .problem, quiet)),
                mapping = aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'idv')$col]], 
                                    y = .data[[xp_var(xpdb, .problem, type = 'ipred')$col]]), mapping),
                type = type, facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = as.character(match.call()[[1]]), ...)
}


#' @name pred_vs_idv
#' @export
pred_vs_idv <- function(xpdb,
                        mapping  = NULL,
                        group    = 'ID',
                        type     = 'pls',
                        facets,
                        title    = '@y vs. @x | @run',
                        subtitle = 'Ofv: @ofv',
                        caption  = '@dir',
                        tag      = NULL,
                        log      = NULL,
                        .problem,
                        quiet,
                        ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (missing(facets)) facets <- xpdb$xp_theme$facets
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = data_opt(.problem = .problem, 
                               filter = only_obs(xpdb, .problem, quiet)),
                mapping = aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'idv')$col]], 
                                    y = .data[[xp_var(xpdb, .problem, type = 'pred')$col]]), mapping),
                type = type, facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = as.character(match.call()[[1]]), ...)
}


#' Observations, individual model predictions and model prediction 
#' plotted against the independent variable
#'
#' @rdname pred_vs_idv
#' @export
dv_preds_vs_idv <- function(xpdb,
                            mapping  = NULL,
                            group    = 'ID',
                            type     = 'pls',
                            facets,
                            title    = 'Observations, Individual and Population Predictions vs. @x | @run',
                            subtitle = 'Ofv: @ofv, Eps shrink: @epsshk',
                            caption  = '@dir',
                            tag      = NULL,
                            log      = NULL,
                            .problem,
                            quiet,
                            ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets, 
                                               variable = 'variable')
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = data_opt(.problem = .problem, tidy = TRUE, 
                               filter = only_obs(xpdb, .problem, quiet),
                               value_col = xp_var(xpdb, .problem, 
                                                  type = c('dv', 'pred', 'ipred'))$col),
                mapping = aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'idv')$col]], 
                                    y = .data[["value"]]), mapping), 
                type = type, guide = FALSE, facets = facets,
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = as.character(match.call()[[1]]), ...)
}

