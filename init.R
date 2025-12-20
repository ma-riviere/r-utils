# Infix operators
'%notin%' <- Negate('%in%')
'%||%' <- function(x, y) if (is.null(x)) y else x
'%|e|%' <- function(x, y) if (is.null(x) || length(x) == 0 || !nzchar(x)) y else x

# renv
source("r-utils/renv/init.R")
