# Infix operators
'%notin%' <- Negate('%in%')
`%ni%` <- `%notin%` # Alias
'%||%' <- function(x, y) if (is.null(x)) y else x
'%|e|%' <- function(x, y) {
    if (is.null(x) || length(x) == 0 || !nzchar(x)) y else x
}
"%s+%" <- function(lhs, rhs) paste0(lhs, rhs)

walk <- function(.x, .f, ...) {
    .f <- match.fun(.f)
    for (element in .x) {
        .f(element, ...)
    }
    return(invisible(.x))
}

# R Version
if (!nzchar(Sys.getenv("RENV_PROFILE"))) {
    Sys.setenv(RENV_PROFILE = paste0("dev-", version$major, ".", sub("\\..*", "", version$minor)))
}

# Load individual files
source_frames <- Filter(\(frame) !is.null(frame$ofile), sys.frames())
if (length(source_frames) == 0L) {
    stop("r-utils/init.R must be loaded with source().")
}
r_utils_path <- dirname(normalizePath(source_frames[[length(source_frames)]]$ofile, mustWork = TRUE))
rm(source_frames)
utils_files <- list.files(r_utils_path, pattern = "*\\.R", full.names = TRUE, recursive = FALSE, ignore.case = TRUE)
utils_files <- utils_files[basename(utils_files) != "init.R"]
walk(utils_files, source, local = environment())

# renv
source(file.path(r_utils_path, "renv", "init.R"), local = environment())

rm(r_utils_path, utils_files)
