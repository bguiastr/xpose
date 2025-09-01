#' Default xpose QQ plot function
#'
#' @description Manually generate QQ plots from an xpdb object.
#'
#' @inheritParams xplot_scatter
#' @param type String setting the type of plot to be used. Can only be points 'p'.
#' @param guide Should the guide (e.g. reference line) be displayed.
#' 
#' @section Layers mapping:
#' Plots can be customized by mapping arguments to specific layers. The naming convention is 
#' layer_option where layer is one of the names defined in the list below and option is 
#' any option supported by this layer e.g. point_color = 'blue', etc.
#' \itemize{
#'   \item point: options to \code{geom_point}
#'   \item guide: options to \code{geom_abline}
#'   \item xscale: options to \code{scale_x_continuous} or \code{scale_x_log10}
#'   \item yscale: options to \code{scale_y_continuous} or \code{scale_y_log10}
#' }
#' @inheritSection xplot_scatter Faceting
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}} \code{\link{xplot_distrib}}
#' 
#' @examples
#' xplot_qq(xpdb_ex_pk, aes(sample = CWRES), guide = TRUE)
#' 
#' @export
xplot_qq <- function(xpdb,
                     mapping   = NULL,
                     type      = 'p',
                     guide     = FALSE,
                     xscale    = 'continuous',
                     yscale    = 'continuous',
                     title     = NULL,
                     subtitle  = NULL,
                     caption   = NULL,
                     tag       = NULL,
                     plot_name = 'qq_plot',
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
  check_plot_type(type, allowed = 'p')
  
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
  xp <- ggplot(data = data, mapping) + gg_theme 
  
  # Add points (note: could not get geom_text to work with stat_qq)
  if (stringr::str_detect(type, stringr::fixed('p', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping    = mapping,
                        xp_theme   = xpdb$xp_theme,
                        name       = 'point',
                        ggfun      = 'geom_point',
                        point_stat = 'qq',
                        ...)
  }
  
  # Add reference line
  if (guide) {
    xp <- xp + xp_geoms(xp_theme = xpdb$xp_theme,
                        name     = 'guide',
                        ggfun    = 'geom_qq_line',
                        ...)
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
  xp <- xp + labs(title = title, subtitle = subtitle, caption = caption, tag = tag)
  

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
