# Tests start here --------------------------------------------------------
test_that('data_opt function works properly', {
  expect_equal(data_opt(.problem = 1, .subprob = 2, .method = 'foce', .source = 'data', 
                        simtab = FALSE, filter = function(x){ x }, tidy = FALSE, index_col = 'ID',
                        value_col = 'IPRED', post_processing = function(x) { x }),
               list(problem = 1, subprob = 2, method = 'foce', source = 'data', simtab = FALSE, 
                    filter = function(x){ x }, tidy = FALSE, index_col = 'ID',
                    value_col = 'IPRED', post_processing = function(x) { x }))
})

test_that('only_obs function works properly', {
  expect_true(is.function(only_obs(xpdb_ex_pk, .problem = 1, quiet = TRUE)))
  expect_equal(only_obs(xpdb_ex_pk, .problem = 1, quiet = TRUE)(x = xpdb_ex_pk$data$data[[1]]),
               xpdb_ex_pk$data$data[[1]][xpdb_ex_pk$data$data[[1]]$EVID == 0, ])
})

test_that('only_distinct function works properly', {
  expect_true(is.function(only_distinct(xpdb_ex_pk, .problem = 1, facets = 'DOSE', quiet = TRUE)))
  expect_equal(only_distinct(xpdb_ex_pk, .problem = 1, facets = 'DOSE', quiet = TRUE)(x = xpdb_ex_pk$data$data[[1]]),
               dplyr::distinct(.data = xpdb_ex_pk$data$data[[1]], !!!rlang::syms(c('ID', 'DOSE')), .keep_all = TRUE))
})

test_that('reorder_etas_factors function works properly', {
  expect_true(is.function(reorder_factors(prefix = 'ETA(', suffix = ')')))
})

test_that('fetch_data can get simple data', {
  imported_data <- fetch_data(xpdb_ex_pk, .problem = 1, .source = 'data', 
                              simtab = FALSE, filter = only_obs(xpdb_ex_pk, 1, TRUE))
  
  # Check attributes
  expect_equal(attr(imported_data, 'problem'), 1)
  expect_equal(attr(imported_data, 'simtab'), FALSE)
  expect_equal(attr(imported_data, 'source'), 'data')

  # Check output
  expect_equivalent(imported_data, xpdb_ex_pk$data$data[[1]][xpdb_ex_pk$data$data[[1]]$EVID == 0, ])
})

test_that('fetch_data can tidy data', {
  imported_tidy_data <- fetch_data(xpdb_ex_pk, .problem = 1, .source = 'data', 
                                   simtab = FALSE, filter = function(x) {
                                     x[as.vector(x[, 'EVID'] == 0), c('ID', 'TIME', 'IPRED', 'PRED', 'DV')]},
                                   tidy = TRUE, value_col = c('IPRED','PRED', 'DV'))
  
  # Check output
  expect_equivalent(imported_tidy_data, 
               xpdb_ex_pk$data$data[[1]][xpdb_ex_pk$data$data[[1]]$EVID == 0, 
                                         c('ID', 'TIME', 'IPRED', 'PRED', 'DV')] %>% 
                 tidyr::gather(value = 'value', key = 'variable', 
                              !!!rlang::syms(c('IPRED', 'PRED', 'DV')))
  )
})

test_that('fetch_data can get file data', {
  fetched_data <- fetch_data(xpdb_ex_pk, .problem = 1, .source = 'ext', quiet = TRUE)
  
  expect_equivalent(fetched_data, xpdb_ex_pk$files[xpdb_ex_pk$files$name == 'run001.ext',]$data[[1]])
})
