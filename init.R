# Infix operators
'%notin%' <- Negate('%in%')
`%ni%` <- `%notin%` # Alias
'%||%' <- function(x, y) if (is.null(x)) y else x
'%|e|%' <- function(x, y) {
    if (is.null(x) || length(x) == 0 || !nzchar(x)) y else x
}
"%s+%" <- function(lhs, rhs) paste0(lhs, rhs)

# R Version
if (Sys.getenv("RENV_PROFILE") == "") {
    Sys.setenv(RENV_PROFILE = paste0("dev-", version$major, ".", sub("\\..*", "", version$minor)))
}

# Load individual files
utils_files <- list.files(
    "r-utils",
    pattern = "*\\.R",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
)
utils_files <- utils_files[basename(utils_files) != "init.R"]
void_ <- lapply(utils_files, source)

# renv
source("r-utils/renv/init.R")
