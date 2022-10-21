#' Default xpose distribution plot function
#'
#' @description Manually generate distribution plots from an xpdb object.
#'
#' @inheritParams xplot_scatter
#' @param type String setting the type of plot to be used. Can be histogram 'h',
#' density 'd', rug 'r' or any combination of the three.
#' @param guide Should the guide (e.g. reference distribution) be displayed.

#' @section Layers mapping:
#' Plots can be customized by mapping arguments to specific layers. The naming convention is 
#' layer_option where layer is one of the names defined in the list below and option is 
#' any option supported by this layer e.g. histogram_fill = 'blue', rug_sides = 'b', etc.
#' \itemize{
#'   \item histogram: options to \code{geom_histogram}
#'   \item density: options to \code{geom_density}
#'   \item rug: options to \code{geom_rug}
#'   \item xscale: options to \code{scale_x_continuous} or \code{scale_x_log10}
#'   \item yscale: options to \code{scale_y_continuous} or \code{scale_y_log10}
#' }
#' @inheritSection xplot_scatter Faceting
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}} \code{\link{xplot_qq}}
#' 
#' @examples
#' # A simple histogram
#' xplot_distrib(xpdb_ex_pk, aes(x = WT), type = 'hr')
#' 
#' # A simple density plot
#' xplot_distrib(xpdb_ex_pk, aes(x = CWRES), type = 'dr')
#' 
#' @export
xplot_distrib <- function(xpdb,
                          mapping   = NULL,
                          type      = 'hr',
                          guide     = FALSE,
                          xscale    = 'continuous',
                          yscale    = 'continuous',
                          title     = NULL,
                          subtitle  = NULL,
                          caption   = NULL,
                          tag       = NULL,
                          plot_name = 'density_plot',
                          gg_theme,
                          xp_theme,
                          opt,
                          quiet,
                          ...) {
  # Check input
  check_xpdb(xpdb, check = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  # Fetch data
  if (missing(opt)) opt <- data_opt()
  data <- fetch_data(xpdb, quiet = quiet, .problem = opt$problem, .subprob = opt$subprob, 
                     .method = opt$method, .source = opt$source, simtab = opt$simtab, 
                     filter = opt$filter, tidy = opt$tidy, index_col = opt$index_col, 
                     value_col = opt$value_col, post_processing = opt$post_processing)
  if (is.null(data) || nrow(data) == 0) {
    stop('No data available for plotting. Please check the variable mapping and filering options.', 
         call. = FALSE)
  }
  
  # Check type
  check_plot_type(type, allowed = c('d', 'h', 'r'))
  
  # Assign xp_theme
  if (!missing(xp_theme)) xpdb <- update_themes(xpdb = xpdb, xp_theme = xp_theme)
  
  # Assign gg_theme
  if (missing(gg_theme)) {
    gg_theme <- xpdb$gg_theme
  } else {
    gg_theme <- update_themes(xpdb = xpdb, gg_theme = gg_theme)$gg_theme 
  }
  if (is.function(gg_theme)) {
    gg_theme <- do.call(gg_theme, args = list())
  }
  
  # Create ggplot base
  xp <- ggplot(data = data, aes_filter(mapping, keep_only = c('x', 'y'))) + gg_theme 
  
  # Add histogram
  if (stringr::str_detect(type, stringr::fixed('h', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = mapping %>% 
                          aes_rename('y', 'histogram_y') %>% 
                          aes_c(fun_aes = aes(histogram_y = after_stat(!!rlang::sym("density")))),
                        xp_theme = xpdb$xp_theme,
                        name     = 'histogram',
                        ggfun    = 'geom_histogram',
                        ...)
  }
  
  # Add density
  if (stringr::str_detect(type, stringr::fixed('d', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = mapping,
                        xp_theme = xpdb$xp_theme,
                        name     = 'density',
                        ggfun    = 'geom_density',
                        ...)
  }
  
  # Add rug
  if (stringr::str_detect(type, stringr::fixed('r', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = aes_rename(mapping, 'x', 'rug_x'),
                        xp_theme = xpdb$xp_theme,
                        name     = 'rug',
                        ggfun    = 'geom_rug',
                        rug_inherit.aes = FALSE, 
                        rug_data = data,
                        ...)
  }
  
  # Add reference distibution
  if (guide) {
    msg('Reference distribution not yet available.', TRUE)
    # xp <- xp + xp_geoms(xp_theme = xpdb$xp_theme,
    #                     name     = 'guide',
    #                     ggfun    = 'geom_line',
    #                     ...)
  }
  
  # Define scales
  xp <- xp + 
    xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'xscale',
             ggfun    = paste0('scale_x_', xscale),
             ...) +
    xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'yscale',
             ggfun    = paste0('scale_y_', yscale),
             ...)
  
  # Define panels
  if (!is.null(list(...)[['facets']])) {
    xp <- xp + xpose_panels(xp_theme = xpdb$xp_theme, 
                            extra_args = list(...))
  }
  
  # Add labels
  xp <- xp + labs(title = title, subtitle = subtitle, caption = caption)
  
  if (utils::packageVersion('ggplot2') >= '3.0.0') {
    xp <- xp + labs(tag = tag)
  }
  
  # Add metadata to plots
  xp$xpose <- list(fun      = plot_name,
                   summary  = xpdb$summary,
                   problem  = attr(data, 'problem'),
                   subprob  = attr(data, 'subprob'),
                   method   = attr(data, 'method'),
                   quiet    = quiet,
                   xp_theme = xpdb$xp_theme[stringr::str_c(c('title', 'subtitle', 
                                                             'caption', 'tag'), '_suffix')])
  
  # Ouptut the plot
  as.xpose.plot(xp)
}
