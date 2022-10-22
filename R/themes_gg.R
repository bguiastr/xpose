#' An additional set of themes for ggplot2
#'
#' @description An additional set of complete ggplot2 themes intended to make ggplot2 more readable 
#' when used in presentation or publications. These themes also bring the \code{legend_position} option
#' without having to call the ggplot2 theme() function to modify a complete theme.
#' 
#' \itemize{
#' \item \code{theme_bw2}: Black and white theme inspired by a theme from Gunnar Yngman.
#' \item \code{theme_readable}: Light grey theme, with dimmed background and grid lines 
#' intended to bring the focus on the data.
#' }
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param legend_position The position of legends defined as 'none', 'left', 
#' 'right', 'bottom', 'top', or a two-element numeric vector.
#'
#' @examples
#' # With the gg_theme theme_readable() (default)
#' dv_vs_ipred(xpdb_ex_pk, facets = 'SEX')
#' 
#' # With the gg_theme theme_bw2()
#' xpdb_ex_pk %>% 
#'  update_themes(gg_theme = theme_bw2()) %>% 
#'  dv_vs_ipred(facets = 'SEX')
#'  
#' @name gg_themes
#' @export
theme_bw2 <- function(base_size = 11, base_family = '', legend_position = 'right') {
  
  ## ggplot2 v3.4.0 compatibility fix
  ## Fix to avoid triggering R CMD check errors
  element_line <- function(linewidth = NULL, ...) {
    if (utils::packageVersion("ggplot2") > "3.3.6") {
      asNamespace("ggplot2")$element_line(linewidth = linewidth, ...)
    } else {
      asNamespace("ggplot2")$element_line(size = linewidth, ...)
    }
  }
  
  element_rect <- function(linewidth = NULL, ...) {
    if (utils::packageVersion("ggplot2") > "3.3.6") {
      asNamespace("ggplot2")$element_rect(linewidth = linewidth, ...)
    } else {
      asNamespace("ggplot2")$element_rect(size = linewidth, ...)
    }
  }
  
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(plot.title = element_text(face = 'bold', size = rel(1.2), hjust = 0, 
                                    vjust = 1, margin = margin(b = base_size/2 * 1.2)),
          strip.text = element_text(color = 'white', size = rel(0.8)),
          strip.background = element_rect(color = 'black', fill = 'black'),
          legend.position = legend_position,
          legend.key = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = 'grey90', linewidth = 0.25),
          plot.margin = grid::unit(c(0.01 ,0.01, 0.01, 0.01), 'npc'),
          panel.background = element_rect(color = 'black', fill = 'white', linewidth = 0.1),
          complete = TRUE)
}

#' @rdname gg_themes
#' @export
theme_readable <- function(base_size = 11, base_family = '', legend_position = 'right') {
  
  ## ggplot2 v3.4.0 compatibility fix
  ## Fix to avoid triggering R CMD check errors
  element_line <- function(linewidth = NULL, ...) {
    if (utils::packageVersion("ggplot2") > "3.3.6") {
      asNamespace("ggplot2")$element_line(linewidth = linewidth, ...)
    } else {
      asNamespace("ggplot2")$element_line(size = linewidth, ...)
    }
  }
  
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(plot.title = element_text(face = 'bold', size = rel(1.2), hjust = 0, 
                                    vjust = 1, margin = margin(b = base_size/2 * 1.2)),
          strip.background = element_rect(color = NA, fill = 'grey88'),
          axis.text = element_text(size = rel(0.8), colour = 'black'),
          legend.position = legend_position,
          legend.key = element_blank(),
          panel.background = element_rect(color = NA, fill = 'grey95'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = 'grey88', linewidth = 0.25),
          plot.margin = grid::unit(c(0.01 ,0.01, 0.01, 0.01), 'npc'),
          complete = TRUE)
}
