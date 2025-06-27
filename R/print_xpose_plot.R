#' Draw an xpose_plot object
#' 
#' @description This function explicitly draw an xpose_plot and interprets keywords 
#' contained in labels.
#' 
#' @param x An \code{xpose_plot} object.
#' @param page The page number to be drawn. Can be specified as vector or range 
#' of integer values.
#' @param ... Options to be passed on to the ggplot2 print method.
#' 
#' @method print xpose_plot
#' @examples
#' my_plot <- dv_vs_ipred(xpdb_ex_pk) +
#'             labs(title = 'A label with keywords: @nind individuals & @nobs observations')

#' # Using the print function
#' print(my_plot)
#' 
#' # Or simply by writing the plot object name
#' my_plot
#' 
#' @export
print.xpose_plot <- function(x, page, ...) {
  
  # Parse template titles
  if (is.xpose.plot(x)) {
    x_labs <- suppressMessages(get_labs(plot = x))
    
    # Add prefix to title subtitle, caption and tags
    x <- x + labs(
      title    = append_suffix(x$xpose, x_labs$title, 'title'),
      subtitle = append_suffix(x$xpose, x_labs$subtitle, 'subtitle'),
      caption  = append_suffix(x$xpose, x_labs$caption, 'caption'),
      tag      = append_suffix(x$xpose, x_labs$tag, 'tag')
    )
    
    # Get the mapping variables keywords and values
    var_map <- x$mapping %>% 
      as.character() %>%
      stringr::str_remove(pattern = "^~") %>% 
      
      ## Improve parsing since we now have to use the .data[["var"]] format in aes()
      ifelse(stringr::str_detect(., "\\.data\\[\\[\"\\w+\"]]"), 
             yes = stringr::str_remove_all(., "(\\.data\\[\\[\")|(\"]])"),
             no  = .) %>% 
      purrr::set_names(names(x$mapping))
    
    # Process the keywords
    x <- x + do.call(
      what = ggplot2::labs,
      args = suppressMessages(ggplot2::get_labs(plot = x)) %>% 
        purrr::compact() %>% 
        purrr::map_if(
          .p = stringr::str_detect(., '@'),
          .f = parse_title, 
          xpdb = x$xpose,
          problem = x$xpose$problem, quiet = x$xpose$quiet,
          ignore_key = c('page', 'lastpage'),
          extra_key = c('plotfun', 'timeplot', names(var_map)), 
          extra_value = c(x$xpose$fun, 
                          format(Sys.time(), "%a %b %d %X %Z %Y"), 
                          var_map)
        )
    )
  }
  
  # Print multiple pages
  if (class(x$facet)[1] %in% c('FacetWrapPaginate', 'FacetGridPaginate')) {
    
    # Get total number of pages
    page_tot <- plot_layout(x)$n_pages
    
    # Get and check the page number to be drawn
    if (!missing(page)) {
      page_2_draw <- page
    } else if (!is.null(x$facet$params$page)) {
      page_2_draw <- x$facet$params$page
    } else {
      page_2_draw <- 1:page_tot
    }
    
    if (any(page_2_draw > page_tot)) {
      page_2_draw <- page_2_draw[page_2_draw <= page_tot]
      if (length(page_2_draw) == 0) {
        stop('All `page` element exceeded the total (', page_tot, ') number of pages.', call. = FALSE)  
      }
      warning('`page` contained elements exceeding the total (', page_tot, ') number of pages. These were ignored.',
              call. = FALSE)
    }
    
    # Prevent issue with facet_repair when page = NULL
    x$facet$params$page <- page_2_draw
    
    # Begin multiple page plotting
    n_page_2_draw <- length(page_2_draw)
    
    if (interactive() && !x$xpose$quiet) {
      message('Rendering ', n_page_2_draw, ' selected page(s) out of ', page_tot, '.')
    }
    
    if (n_page_2_draw == 1) {
      x %>% 
        paginate(page_2_draw, page_tot) %>% 
        plot(...)
    } else {
      if (interactive() && !x$xpose$quiet) {
        pb <- utils::txtProgressBar(min = 0, max = n_page_2_draw, 
                                    style = 3)   # Create progress bar
      }
      for (p in seq_along(page_2_draw)) {
        x$facet$params$page <- page_2_draw[p]
        x %>% 
          paginate(page_2_draw[p], page_tot) %>% 
          plot(...)
        if (interactive() && !x$xpose$quiet) {
          utils::setTxtProgressBar(pb, value = p) # Update progress bar
        }
      }
      if (interactive() && !x$xpose$quiet) close(pb)
      
      # Prevent ggforce from droping multiple pages value
      x$facet$params$page <- page_2_draw
    }
  } else {
    if (!missing(page)) warning('Faceting not set. Ignoring `page` argument.', call. = FALSE)
    
    # Warn for big plots
    panel_tot <- plot_layout(x)$n_panels
    
    if (panel_tot > 20) {
      msg(c('The faceting resulted in ', panel_tot, 
            ' panels. The plot may take a while to render.'), 
          quiet = x$xpose$quiet)
    }
    
    # Print without multiple pages
    x %>% 
      paginate(page_2_draw = 1, page_tot = 1) %>% 
      plot(...)
  }
}


# Add page number to pages
paginate <- function(plot, page_2_draw, page_tot) {
  plot + do.call(
    what = ggplot2::labs, 
    args = suppressMessages(ggplot2::get_labs(plot)) %>% 
      purrr::compact() %>% 
      purrr::map_if(
        .p = ~!is.null(.) && stringr::str_detect(., '@(page|lastpage)'),
        .f = parse_title, xpdb = plot$xpose,
        problem = plot$xpose$problem, quiet = plot$xpose$quiet,
        extra_key = c('page', 'lastpage'), 
        extra_value = c(as.character(page_2_draw), page_tot)
      )
  )
}


# Calculate the total number of pages
plot_layout <- function(plot) {
  plot_str <- suppressMessages(ggplot2::ggplot_build(plot))
  
  panels   <- plot_str$layout$layout
  n_panels <- ifelse(!is.null(panels), nrow(panels), 0L)
  
  pages    <- plot_str$layout$layout$page
  n_pages  <- ifelse(!is.null(pages), max(pages), 0L)
  
  list(n_panels = n_panels, n_pages = n_pages)  
}
