# .onAttach <- function(...) {
# 
#   if (!interactive()) return()
#   
#   text <- c('Note: the next xpose release (v0.5) will bring some major changes.\nIf you rely on xpose for your work, please keep an eye on our github:\nhttps://github.com/UUPharmacometrics/xpose')
#   
#   packageStartupMessage(sample(text, size = 1))
# }

# Remove CRAN note on no visible binding for global variable
utils::globalVariables('.')
