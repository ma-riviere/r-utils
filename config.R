options(
    n_cores = max(1, parallel::detectCores(logical = TRUE) - 1),
    verbose = FALSE
)

Sys.setenv(MAKEFLAGS = paste0("-j", getOption("n_cores")))
