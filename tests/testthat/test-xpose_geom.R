context('Check xpose_geoms')

# Tests start here --------------------------------------------------------

test_that('parse_arg works properly', {
  expect_null(parse_arg(x = NULL, name = 'point'))
  expect_equal(parse_arg(x = c(point_color = 'blue', line_color = 'red'), name = 'point'), 
               c(colour = 'blue'))
})

test_that('update_args works properly', {
  expect_equal(update_args(thm_arg = filter_xp_theme(xpdb_ex_pk$xp_theme, 'point_'), 
                           name = 'point', point_color = 'red'),
               list(alpha = 0.7, colour = 'red', fill = NA, shape = 19, size = 2.5, stroke = 0))
})

test_that('xp_map works properly', {
  test <- xp_map(arg = list(color = 'red'), mapping = aes(x = .data[["IPRED"]]), ggfun = 'geom_point')
  test <- as.list(test)    ## Convert environment to list
  test$constructor <- NULL ## Constructor cannot be match due to the different construct approaches
  
  ref <- geom_point(mapping = aes(x = .data[["IPRED"]]) , color = 'red')
  ref <- as.list(ref)      ## Convert environment to list
  ref$constructor <- NULL  ## Constructor cannot be match due to the different construct approaches
  
  expect_equal(test, ref)
})

test_that('xp_geoms works properly', {
  test <- xp_geoms(mapping = aes(point_color = .data[["IPRED"]]), xpdb_ex_pk$xp_theme, name = 'point', 
                   ggfun = 'geom_point', point_size = 3)
  test <- as.list(test)    ## Convert environment to list
  test$constructor <- NULL ## Constructor cannot be match due to the different construct approaches
  
  ref <- geom_point(mapping = aes(color = .data[["IPRED"]]) , alpha = 0.7, fill = NA, 
                    shape = 19, size = 3, stroke = 0)
  ref <- as.list(ref)      ## Convert environment to list
  ref$constructor <- NULL  ## Constructor cannot be match due to the different construct approaches
  
  expect_equal(test, ref)
})
