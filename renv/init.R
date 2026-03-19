source("r-utils/renv/helpers.R")

# Expose global R library to renv (for dev tools like httpgd)
if (startsWith(Sys.getenv("RENV_PROFILE"), "dev-")) {
    set_external_libraries()
}

# See:
## - https://rstudio.github.io/renv/reference/config.html
## - https://rstudio.github.io/renv/reference/snapshot.html
options(
    repos = c(
        PPM = "https://packagemanager.posit.co/cran/latest",
        CRAN = "https://cloud.r-project.org"
    ),
    renv.config.pak.enabled = TRUE,
    renv.config.sandbox.enabled = FALSE,
    renv.config.snapshot.inference = FALSE,
    renv.config.snapshot.validate = FALSE,
    renv.config.synchronized.check = FALSE
)

# GitHub
if (!nzchar(Sys.getenv("GITHUB_PAT"))) {
    warning("[RENV] GITHUB PAT not found - package loading might fail due to Github API's download cap.")
}
