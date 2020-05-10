#' Generate xpdb example for the xpose package
#' 
#' @description Generate the xpdb_ex_pk.rda object when change in internal xpose 
#' structure is made.
#' @usage Set working directory to the xpose project directory. And run the whole script.
#' 
library(xpose)

# Generate xpdb
xpdb_ex_pk <- xpose_data(runno = "001",
                         dir   = here::here("inst/extdata/"), 
                         quiet = FALSE)

# Overwrite directory paths to make it look clean in examples
xpdb_ex_pk$summary$value[xpdb_ex_pk$summary$label == "dir"] <- "data"
xpdb_ex_pk$options$dir <- "data"
attr(xpdb_ex_pk$code, "dir") <- "data"

# Ensure the xpose data class is preserved
xpdb_ex_pk <- xpose::as.xpdb(xpdb_ex_pk)

# Export rda to data folder
save(xpdb_ex_pk, file = here::here("data/xpdb_ex_pk.rda"), 
     compress = 'xz', version = 2)
