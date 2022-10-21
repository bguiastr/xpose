#' QQ plots of ETA and residuals
#'
#' @description QQ plots of the ETA and model residuals.
#' 
#' @inheritParams dv_vs_pred
#' @param type String setting the type of plot. Can only be points 'p'.
#' @param guide Should the guide (e.g. reference line) be displayed.
#' @param drop_fixed Should columns that only have a single unique value 
#' (i.e. fixed) be dropped.
#' 
#' @inheritSection xplot_qq Layers mapping
#' @inheritSection xplot_scatter Faceting
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_distrib}}
#' @examples
#' # QQ plot of parameters
#' prm_qq(xpdb_ex_pk)
#' 
#' # QQ plot of eta
#' eta_qq(xpdb_ex_pk)
#' 
#' # QQ plot of residuals
#' res_qq(xpdb_ex_pk, res = c('IWRES', 'CWRES'))
#' 
#' # QQ plot of continuous covariates
#' cov_qq(xpdb_ex_pk)
#' 
#' @name qq_plot
#' @export
prm_qq <- function(xpdb,
                   mapping  = NULL,
                   drop_fixed = TRUE,
                   type     = 'p',
                   title    = 'QQ plot of parameters | @run',
                   subtitle = 'Based on @nind individuals',
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
  if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets, 
                                               variable = 'variable')
  
  prm_col <- xp_var(xpdb, .problem, type = 'param')$col
  if (drop_fixed) {
    prm_col <- drop_fixed_cols(xpdb, .problem, cols = prm_col, quiet = quiet)
  }
  if (is.null(prm_col)) {
    stop('No parameter column found in the xpdb data index.', call. = FALSE)
  }
  
  xplot_qq(xpdb = xpdb, quiet = quiet,
           opt = data_opt(.problem = .problem, 
                          filter = only_distinct(xpdb, .problem, facets, quiet), 
                          tidy = TRUE, value_col = prm_col),
           mapping = aes_c(aes(sample = .data[["value"]]), mapping), 
           type = type, guide = guide, facets = facets,
           xscale = check_scales('x', log), 
           yscale = check_scales('y', log), 
           title = title, subtitle = subtitle, caption = caption,
           tag = tag, plot_name = as.character(match.call()[[1]]), 
           xscale_name = 'Quantiles of normal', 
           yscale_name = 'Quantiles of parameter', ...)
}

#' @rdname qq_plot
#' @export
eta_qq <- function(xpdb,
                   mapping  = NULL,
                   drop_fixed = TRUE,
                   type     = 'p',
                   title    = 'QQ plot of etas | @run',
                   subtitle = 'Based on @nind individuals, Eta shrink: @etashk',
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
  if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets, 
                                               variable = 'variable')
  
  eta_col <- xp_var(xpdb, .problem, type = 'eta')$col
  if (drop_fixed) {
    eta_col <- drop_fixed_cols(xpdb, .problem, cols = eta_col, quiet = quiet)
  }
  if (is.null(eta_col)) {
    stop('No eta column found in the xpdb data index.', call. = FALSE)
  }
  
  if (software(xpdb) == 'nonmem') {
    post_processing_eta <-  reorder_factors(prefix = 'ETA(', suffix = ')')
  } else {
    post_processing_eta <- NULL
  }
  xplot_qq(xpdb = xpdb, quiet = quiet,
           opt = data_opt(.problem = .problem, 
                          filter = only_distinct(xpdb, .problem, facets, quiet), 
                          tidy = TRUE, value_col = eta_col,
                          post_processing = post_processing_eta),
           mapping = aes_c(aes(sample = .data[["value"]]), mapping), 
           type = type, guide = guide, facets = facets,
           xscale = check_scales('x', log), 
           yscale = check_scales('y', log), 
           title = title, subtitle = subtitle, caption = caption,
           tag = tag, plot_name = as.character(match.call()[[1]]), 
           xscale_name = 'Quantiles of normal', 
           yscale_name = 'Quantiles of eta', ...)
}

#' @param res Only used for \code{res_qq}. Defines the type of residual to be used. Default is "CWRES".
#' @rdname qq_plot
#' @export
res_qq <- function(xpdb,
                   mapping  = NULL,
                   res      = 'CWRES',
                   type     = 'p',
                   title    = 'QQ plot of @sample | @run',
                   subtitle = 'Based on @nobs observations',
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
  
  if (is.null(xp_var(xpdb, .problem, col = res))) {
    stop('No ', stringr::str_c(res, collapse = ', '), 
         ' column found in the xpdb data index.', call. = FALSE)
  }
  
  if (length(res) > 1) {
    if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets, 
                                                 variable = 'variable')
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet),
                    tidy = TRUE, value_col = res)
    vars <- aes_c(aes(sample = .data[["value"]]), mapping)
  } else {
    if (missing(facets)) facets <- xpdb$xp_theme$facets
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet))
    vars <- aes_c(aes(sample = .data[[toupper(res)]]), mapping)
  }
  
  xplot_qq(xpdb = xpdb, quiet = quiet,
           opt = opt, mapping = vars,
           type = type, guide = guide, facets = facets,
           xscale = check_scales('x', log), 
           yscale = check_scales('y', log), 
           title = title, subtitle = subtitle, caption = caption,
           tag = tag, plot_name = as.character(match.call()[[1]]), ...) +
    labs(x = 'Quantiles of normal', 
         y = 'Quantiles of @sample')
}

#' @name qq_plot
#' @export
cov_qq <- function(xpdb,
                   mapping  = NULL,
                   drop_fixed = TRUE,
                   type     = 'p',
                   title    = 'QQ plot of continuous covariates | @run',
                   subtitle = 'Based on @nind individuals',
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
  if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets, 
                                               variable = 'variable')
  
  cov_col <- xp_var(xpdb, .problem, type = 'contcov')$col
  if (drop_fixed) {
    cov_col <- drop_fixed_cols(xpdb, .problem, cols = cov_col, quiet = quiet)
  }
  if (is.null(cov_col)) {
    stop('No continuous covariate column found in the xpdb data index.', call. = FALSE)
  }
  
  xplot_qq(xpdb = xpdb, quiet = quiet,
           opt = data_opt(.problem = .problem, 
                          filter = only_distinct(xpdb, .problem, facets, quiet), 
                          tidy = TRUE, value_col = cov_col),
           mapping = aes_c(aes(sample = .data[["value"]]), mapping), 
           type = type, guide = guide, facets = facets,
           xscale = check_scales('x', log), 
           yscale = check_scales('y', log), 
           title = title, subtitle = subtitle, caption = caption,
           tag = tag, plot_name = as.character(match.call()[[1]]), 
           xscale_name = 'Quantiles of normal', 
           yscale_name = 'Quantiles of covariate', ...)
}
