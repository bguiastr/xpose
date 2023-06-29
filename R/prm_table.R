#' Display a parameter estimates to the console
#' 
#' @description Display parameter estimates from an xpdb object to the console.
#' 
#' @inheritParams get_prm
#' @seealso \code{\link{get_prm}},
#' @examples
#' \dontrun{
#' # Store the parameter table
#' prm <- get_prm(xpdb_ex_pk, .problem = 1)
#' 
#' # Display parameters to the console
#' prm_table(xpdb_ex_pk, .problem = 1)
#' }
#' @export
prm_table <- function(xpdb,
                      .problem  = NULL, 
                      .subprob  = NULL, 
                      .method   = NULL,
                      digits    = 4,
                      transform = TRUE,
                      show_all  = FALSE) {
  
  x <- get_prm(xpdb = xpdb, .problem = .problem, .subprob = .subprob, 
               .method = .method, digits = digits, transform = transform, 
               show_all = show_all, quiet = TRUE)
  
  if (transform) {
    cat('\nReporting transformed parameters:\nFor the OMEGA and SIGMA matrices, values are reported as standard deviations for the diagonal elements and as correlations for the off-diagonal elements. The relative standard errors (RSE) for OMEGA and SIGMA are reported on the approximate standard deviation scale (SE/variance estimate)/2. Use `transform = FALSE` to report untransformed parameters.\n')
  } else {
    cat('\nReporting untransformed parameters:\nFor the OMEGA and SIGMA matrices, values are reported as variances for the diagonal elements and as covariances for the off-diagonal elements.\n')
  }
  
  # Convert single prm_df to list
  if (dplyr::is.tbl(x)) x <- list(x)
  
  # Generate output to console
  purrr::map(.x = x, function(prm, transform) {
    prm_attr        <- attributes(prm)
    uncertainty_lab <- ifelse(transform, 'RSE', 'SE')
    if (!transform) prm$rse <- prm$se
    
    header <- dplyr::tibble(name = 'Parameter', label = 'Label', 
                            value = 'Value', rse = uncertainty_lab, fixed = '   ')
    
    cat('\nEstimates for $prob no.', prm_attr$problem, 
        ', subprob no.', prm_attr$subprob, ', method ', prm_attr$method, '\n', sep = '')
    
    prm %>% 
      dplyr::mutate_all(.funs = 'as.character') %>% 
      dplyr::mutate(fixed = ifelse(.$fixed, 'fix', '   ')) %>% 
      {dplyr::bind_rows(header, .)} %>% 
      dplyr::mutate(name  = stringr::str_pad(.$name, max(nchar(.$name)), 'right'),
                    label = stringr::str_pad(.$label, max(nchar(.$label)), 'right'),
                    value = stringr::str_pad(.$value, max(nchar(.$value)), 'right'),
                    rse   = ifelse(is.na(.$rse), ' - ', .$rse)) %>%
      dplyr::mutate(string = stringr::str_c('', .$name, .$label, .$value, .$fixed, .$rse, sep = ' ')) %>% 
      {purrr::flatten_chr(.[,'string'])} %>% 
      cat(sep = '\n')
  },
  transform = transform) %>% 
    invisible()
}
