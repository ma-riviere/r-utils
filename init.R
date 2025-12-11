'%notin%' <- Negate('%in%')
'%||%' <- function(x, y) if (is.null(x)) y else x

source("r-utils/renv_helpers.R")

options(repos = c(PPM = "https://packagemanager.posit.co/cran/latest", CRAN = "https://cloud.r-project.org"))

Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = FALSE)
Sys.setenv(RENV_CONFIG_SYNCHRONIZED_CHECK = FALSE)
