source("r-utils/renv/helpers.R")

options(
    repos = c(
        PPM = "https://packagemanager.posit.co/cran/latest",
        CRAN = "https://cloud.r-project.org"
    )
)

Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = FALSE)
Sys.setenv(RENV_CONFIG_SYNCHRONIZED_CHECK = FALSE)