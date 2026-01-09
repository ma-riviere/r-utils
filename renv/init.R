source("r-utils/renv/helpers.R")

options(
    repos = c(
        PPM = "https://packagemanager.posit.co/cran/latest",
        CRAN = "https://cloud.r-project.org"
    )
)

# See:
## - https://rstudio.github.io/renv/reference/config.html
## - https://rstudio.github.io/renv/reference/snapshot.html
Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = FALSE)
Sys.setenv(RENV_CONFIG_SNAPSHOT_INFERENCE = FALSE)
Sys.setenv(RENV_CONFIG_SNAPSHOT_VALIDATE = FALSE)
Sys.setenv(RENV_CONFIG_SYNCHRONIZED_CHECK = FALSE)

# GitHub
if (!nzchar(Sys.getenv("GITHUB_PAT"))) {
    warning("[RENV] GITHUB PAT not found - package loading might fail due to Github API's download cap.")
}