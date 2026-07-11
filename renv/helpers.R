get_r_version <- function() {
    return(paste0(version$major, ".", sub("\\..*", "", version$minor)))
}

# Set RENV_CONFIG_EXTERNAL_LIBRARIES to the global R library matching the current R version (major.minor) in /opt/R/
set_external_libraries <- function(base_path = "/opt/R") {
    r_minor <- get_r_version()
    opt_dirs <- basename(list.files(base_path, full.names = FALSE))
    match <- grep(paste0("^", r_minor), opt_dirs, value = TRUE)
    if (length(match) != 1) {
        return(invisible(NULL))
    }
    global_lib <- file.path(base_path, match, "lib/R/library")
    if (!dir.exists(global_lib)) {
        return(invisible(NULL))
    }
    Sys.setenv(RENV_CONFIG_EXTERNAL_LIBRARIES = global_lib)
    return(invisible(global_lib))
}

is_installed <- function(pkg) {
    suppressMessages({
        require(pkg, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)
    })
}

read_package_file <- function(packages_file) {
    packages <- readLines(packages_file)
    packages <- trimws(packages)
    packages <- packages[nzchar(packages)]
    packages <- gsub(packages, pattern = ",", replacement = "")
    return(packages)
}

append_r_version_to_profile <- function(profile) {
    r_version <- get_r_version()
    return(paste0(profile, "-", r_version))
}

get_pkg_name <- function(remotes_string) {
    # renv:::renv_remotes_parse is internal but has no exported equivalent (renv 1.2.3)
    res <- renv:::renv_remotes_parse(remotes_string)
    return(res$package %||% res$repo)
}

#' Create or update renv/profiles/<prefix>-<R major.minor>/renv.lock from a package manifest.
#'
#' The manifest path is decoupled from the profile prefix (e.g. packages.txt + "docker").
#' Must run in a fresh R process with RENV_PROFILE=<prefix>-<R major.minor> set upfront:
#' renv cannot reliably rewire a session loaded under another profile (installs would
#' silently target the old profile's library). Batch generation is one process per profile:
#'   RENV_PROFILE=dev-4.6 Rscript -e 'source("r-utils/renv/helpers.R"); generate_profile("dev", repos = ...)'
generate_profile <- function(prefix, packages_file = paste0("packages-", prefix, ".txt"), repos) {
    if (missing(repos)) {
        stop(
            "Pass repos explicitly (an immutable dated PPM snapshot); ",
            "defaulting to session repos would silently record rolling URLs."
        )
    }
    profile_with_version <- append_r_version_to_profile(prefix)
    if (!identical(Sys.getenv("RENV_PROFILE"), profile_with_version)) {
        stop(
            "RENV_PROFILE is '",
            Sys.getenv("RENV_PROFILE"),
            "' but must be '",
            profile_with_version,
            "', set in the environment before starting R."
        )
    }
    generate_active_lockfile(packages_file, repos, profile = profile_with_version)
    return(invisible(profile_with_version))
}

#' Create or update the project's ROOT renv.lock (no profiles) from a package manifest.
#'
#' For projects with a single root lockfile restored directly in Docker (e.g. plumber2-base
#' services). Must run in a fresh R process with RENV_PROFILE unset (or "default").
generate_lockfile <- function(packages_file = "packages.txt", repos) {
    if (missing(repos)) {
        stop(
            "Pass repos explicitly (an immutable dated PPM snapshot); ",
            "defaulting to session repos would silently record rolling URLs."
        )
    }
    profile <- Sys.getenv("RENV_PROFILE")
    if (nzchar(profile) && profile != "default") {
        stop(
            "RENV_PROFILE is '",
            profile,
            "': generate_lockfile() targets the root renv.lock. ",
            "Unset it before starting R, or use generate_profile()."
        )
    }
    return(invisible(generate_active_lockfile(packages_file, repos, profile = "default")))
}

# Shared core: installs the manifest packages into the ALREADY-ACTIVE renv environment and
# snapshots its lockfile. Never selects, activates, or resets a profile; the target is fixed
# by RENV_PROFILE at process startup and only verified here.
generate_active_lockfile <- function(packages_file, repos, profile = "default") {
    if (!file.exists(packages_file)) {
        stop("Package file not found: ", packages_file)
    }
    repos <- validate_pinned_repos(repos)

    # Load renv infrastructure; the autoloader honors RENV_PROFILE set at startup and
    # bootstraps renv into the profile library if the profile does not exist yet
    if (file.exists("renv/activate.R")) {
        source("renv/activate.R")
    } else {
        renv::init(bare = TRUE, restart = FALSE, load = TRUE)
    }

    # Confirm the active library matches the target before wiping anything
    library_path <- renv::paths$library()
    library_matches_target <- if (identical(profile, "default")) {
        !grepl("/profiles/", library_path, fixed = TRUE)
    } else {
        grepl(paste0("/profiles/", profile, "/"), library_path, fixed = TRUE)
    }
    if (!library_matches_target) {
        stop("Active renv library (", library_path, ") does not match target '", profile, "'.")
    }

    options(repos = repos)

    # Wipe the target library so resolution starts from scratch instead of snapshotting
    # stale versions already present in the library (renv's install preflight needs the
    # empty directory to exist)
    unlink(library_path, recursive = TRUE)
    dir.create(library_path, recursive = TRUE, showWarnings = FALSE)

    # pak must match the running R's ABI (e.g. R_getVar appeared in R 4.5): bootstrap
    # the matching static build into the fresh library so it shadows any incompatible
    # pak elsewhere on .libPaths
    if (isTRUE(getOption("renv.config.pak.enabled"))) {
        # utils:: explicitly: renv's install.packages shim would delegate to
        # renv::install -> pak, loading renv's private pak copy (possibly built
        # for another R minor) before the correct one is installed
        utils::install.packages(
            "pak",
            lib = library_path,
            repos = sprintf(
                "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
                .Platform$pkgType,
                R.Version()$os,
                R.Version()$arch
            )
        )
    }

    packages <- read_package_file(packages_file)
    renv::install(packages, prompt = FALSE, repos = repos)
    renv::snapshot(packages = vapply(packages, get_pkg_name, character(1)), prompt = FALSE, force = TRUE)

    # renv.config.snapshot.validate is disabled session-wide (renv/init.R): validate the
    # generated lockfile explicitly when jsonvalidate is available
    lockfile_path <- renv::paths$lockfile()
    if (requireNamespace("jsonvalidate", quietly = TRUE)) {
        renv::lockfile_validate(lockfile = lockfile_path, error = TRUE)
    }

    return(invisible(lockfile_path))
}

validate_pinned_repos <- function(repos) {
    rolling <- grepl("/latest/?$", repos) | grepl("cloud.r-project.org", repos, fixed = TRUE)
    if (any(rolling)) {
        stop(
            "Rolling repository URL(s) would make the lockfile non-reproducible: ",
            paste(repos[rolling], collapse = ", "),
            ". Use an immutable dated PPM snapshot (https://packagemanager.posit.co/cran/<YYYY-MM-DD>)."
        )
    }
    # pak silently ADDS a rolling CRAN mirror when no configured repo is named "CRAN",
    # which resolves versions that do not exist in a pinned snapshot
    if (!"CRAN" %in% names(repos)) {
        if (length(repos) == 1) {
            names(repos) <- "CRAN"
        } else {
            stop("Name the repository standing in for CRAN 'CRAN' (pak adds a rolling CRAN mirror otherwise).")
        }
    }
    return(repos)
}
