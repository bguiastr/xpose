#' A set of xpose themes
#'
#' @description xpose themes are used to consistently apply a set of preference for the plot geoms 
#' (e.g. color scales, point size, etc.) whereas ggplot2 theme focus on the plot background, axes, 
#' titles etc.
#' 
#' \itemize{
#' \item \code{theme_xp_default}: The default xp_theme in xpose
#' \item \code{theme_xp_xpose4}: An xp_theme that makes xpose look like xpose4.
#' }
#'
#' @examples
#' # With the xp_theme theme_xp_default() (default)
#' dv_vs_ipred(xpdb_ex_pk, facets = 'SEX')
#' 
#' # With the xp_theme theme_xp_xpose4()
#' xpdb_ex_pk %>%
#'   update_themes(xp_theme = theme_xp_xpose4()) %>%
#'   dv_vs_ipred(facets = 'SEX')
#'   
#' @name xp_themes
#' @export
theme_xp_default <- function() {
  tmp_theme <- list(
    # General
    rounding        = 1,
    title_suffix    = '',
    subtitle_suffix = '',
    caption_suffix  = '',
    tag_suffix      = '',
    
    # Panels
    facets          = NULL,
    ncol            = NULL,
    nrow            = NULL,
    page            = NULL,
    scales          = 'free',
    shrink          = TRUE,
    labeller        = labeller(.default = label_both, .multi_line = FALSE),
    as.table        = TRUE,
    switch          = NULL,
    drop            = TRUE,
    dir             = 'h',
    strip.position  = 'top',
    margins         = FALSE,
    space           = 'fixed',
    byrow           = TRUE,
    
    # Guide
    guide_alpha     = 1,
    guide_color     = 'grey70',
    guide_linetype  = 'solid',
    guide_size      = 0.8,
    
    # Line
    line_alpha      =  0.7,
    line_color      = 'grey20',
    line_linetype   = 'solid',
    line_linewidth  =  0.5,
    
    # Point
    point_alpha     = 0.7,
    point_color     = 'grey20',
    point_fill      = NA,
    point_shape     = 19,
    point_size      = 2.5,
    point_stroke    = 0,
    
    # Smooth
    smooth_alpha    = 0.4,
    smooth_color    = 'deepskyblue2',
    smooth_fill     = 'deepskyblue2',
    smooth_linetype = 1,
    smooth_method   = 'loess',
    smooth_se       = FALSE,
    smooth_size     = 1,
    smooth_weight   = 1,
    
    # Text
    text_alpha      = 0.7,
    text_angle      = 0,
    text_color      = 'grey33',
    text_family     = '',
    text_fontface   = 'plain',
    text_lineheight = 1.2,
    text_size       = 3.1,
    text_hjust      = 0.5, # Change not recommended if type = 't' is used
    text_vjust      = 0.5, # Change not recommended if type = 't' is used
    
    # Density
    density_alpha    = 0.6,
    density_color    = NA,
    density_fill     = 'grey35',
    density_weight   = 1,
    density_size     = 0.5,
    density_linetype = 1,
    
    # Histogram
    histogram_alpha  = 0.6,
    histogram_color  = NA,
    histogram_fill   = 'grey35',
    histogram_size   = 0.5,
    histogram_bins   = 10,
    
    # Rug
    rug_alpha        = 0.8,
    rug_color        = 'grey35',
    rug_linetype     = 1,
    rug_size         = 0.3,
    
    # Area
    area_alpha       = 0.6,
    area_color       = NA,
    area_fill        = 'grey35',
    area_size        = 0.5,
    area_linetype    = 1
  )
  
  ## ggplot2 v3.4.0 compatibility fix
  if (utils::packageVersion("ggplot2") < "3.4.0") {
    names(tmp_theme)[names(tmp_theme) == "line_linewidth"] <- "line_size"
  }
  
  as.xpose.theme(tmp_theme)
}

#' @rdname xp_themes
#' @export
theme_xp_xpose4 <- function() {
  tmp_theme <- list(
    
    # General
    rounding        = 1,
    title_suffix    = '',
    subtitle_suffix = '',
    caption_suffix  = '',
    tag_suffix      = '',
    
    # Panels
    facets          = NULL,
    ncol            = NULL,
    nrow            = NULL,
    page            = NULL,
    scales          = 'free',
    shrink          = TRUE,
    labeller        = labeller(.default = label_both, .multi_line = FALSE),
    as.table        = TRUE,
    switch          = NULL,
    drop            = TRUE,
    dir             = 'h',
    strip.position  = 'top',
    margins         = FALSE,
    space           = 'fixed',
    byrow           = TRUE,
    
    # Guide
    guide_alpha     = NA,
    guide_color     = 'black',
    guide_linetype  = 'solid',
    guide_size      = 0.5,
    
    # Line
    line_alpha      =  NA,
    line_color      = 'blue',
    line_linetype   = 'solid',
    line_linewidth  =  0.5,
    
    # Point
    point_alpha     = NA,
    point_color     = 'blue',
    point_fill      = NA,
    point_shape     = 1,
    point_size      = 2,
    point_stroke    = 0.5,
    
    # Smooth
    smooth_alpha    = NA,
    smooth_color    = 'red',
    smooth_fill     = 'red',
    smooth_linetype = 1,
    smooth_method   = 'loess',
    smooth_se       = FALSE,
    smooth_size     = 1,
    smooth_weight   = 1,
    
    # Text
    text_alpha      = NA,
    text_angle      = 0,
    text_color      = 'black',
    text_family     = '',
    text_fontface   = 'plain',
    text_lineheight = 1.2,
    text_size       = 3.1,
    text_hjust      = 0.5, # Change not recommended if type = 't' is used
    text_vjust      = 0.5, # Change not recommended if type = 't' is used
    
    # Density
    density_alpha    = NA,
    density_color    = 'black',
    density_fill     = NA,
    density_weight   = 1,
    density_size     = 0.5,
    density_linetype = 2,
    
    # Histogram
    histogram_alpha  = NA,
    histogram_color  = 'black',
    histogram_fill   = 'cyan',
    histogram_size   = 0.5,
    histogram_bins   = 10,
    
    # Rug
    rug_alpha        = NA,
    rug_color        = 'black',
    rug_linetype     = 1,
    rug_size         = 0.3,
    
    # Area
    area_alpha       = NA,
    area_color       = NA,
    area_fill        = 'grey35',
    area_size        = 0.5,
    area_linetype    = 1
  )
  
  ## ggplot2 v3.4.0 compatibility fix
  if (utils::packageVersion("ggplot2") < "3.4.0") {
    names(tmp_theme)[names(tmp_theme) == "line_linewidth"] <- "line_size"
  }
  
  as.xpose.theme(tmp_theme)
}
