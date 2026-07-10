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

read_packages <- function(profile) {
    return(read_package_file(paste0("packages-", profile, ".txt")))
}

append_r_version_to_profile <- function(profile) {
    r_version <- get_r_version()
    return(paste0(profile, "-", r_version))
}

get_pkg_name <- function(remotes_string) {
    res <- renv:::renv_remotes_parse(remotes_string)
    return(res$package %||% res$repo)
}

set_renv_profile <- function(profile = "dev") {
    existing_profiles <- list.files(path = "renv/profiles")
    profile_with_version <- append_r_version_to_profile(profile)

    profile_to_use <- "default"
    if (profile_with_version %in% existing_profiles) {
        profile_to_use <- profile_with_version
    }
    Sys.setenv(RENV_PROFILE = profile_to_use)
    invisible(profile_to_use)
}

safe_restore <- function() {
    lockfile_path <- renv::paths$lockfile()

    if (file.exists(lockfile_path)) {
        renv::restore(prompt = FALSE)
    }
    invisible(lockfile_path)
}

get_user_profiles <- function() {
    profiles <- list.files(pattern = "packages-.*.txt")
    profiles <- sub("packages-", "", profiles)
    profiles <- sub(".txt", "", profiles)
    profiles <- unique(profiles)
    return(profiles)
}

install_profile_packages <- function(profile) {
    profile_with_version <- append_r_version_to_profile(profile)
    renv::activate(profile = profile_with_version)

    packages <- read_packages(profile)

    renv::install(packages, prompt = FALSE, rebuild = TRUE, repos = getOption("repos"))
    renv::snapshot(packages = sapply(packages, get_pkg_name), prompt = FALSE, force = TRUE)
}

#' Create or update renv/profiles/<prefix>-<R major.minor>/renv.lock from a package manifest.
#'
#' Unlike install_profiles(), the manifest path is decoupled from the profile prefix
#' (e.g. packages.txt + "docker"), repositories are explicit (pass an immutable dated
#' PPM snapshot for docker profiles), and the session is left on the generated profile.
generate_profile <- function(prefix, packages_file = paste0("packages-", prefix, ".txt"), repos = getOption("repos")) {
    if (!file.exists(packages_file)) {
        stop("Package file not found: ", packages_file)
    }
    if (file.exists("renv/activate.R")) {
        source("renv/activate.R")
    } else {
        renv::init(bare = TRUE, restart = FALSE, load = TRUE)
    }

    profile_with_version <- append_r_version_to_profile(prefix)
    renv::activate(profile = profile_with_version)
    options(repos = repos)

    # Wipe the profile library so resolution starts from scratch instead of
    # snapshotting stale versions already present in the library
    unlink(renv::paths$library(), recursive = TRUE)

    packages <- read_package_file(packages_file)
    renv::install(packages, prompt = FALSE, rebuild = TRUE, repos = repos)
    renv::snapshot(packages = vapply(packages, get_pkg_name, character(1)), prompt = FALSE, force = TRUE)

    # Profiles own the lockfiles; drop root artifacts renv creates as side effects
    unlink("renv.lock")
    unlink("renv/profile")

    return(invisible(profile_with_version))
}

install_profiles <- function(profiles = NULL) {
    # Profiles to install
    profiles_to_install <- get_user_profiles()
    if (!is.null(profiles)) {
        profiles_to_install <- intersect(profiles_to_install, profiles)
    }
    if (is.null(profiles_to_install) || length(profiles_to_install) == 0) {
        return("No matching profiles")
    }

    # Cleaning existing library (to force reinstall from scratch)
    unlink("renv/library", recursive = TRUE)
    if (file.exists("renv/activate.R")) {
        source("renv/activate.R")
    } else {
        renv::init(bare = TRUE, restart = FALSE, load = TRUE)
    }
    renv::upgrade(prompt = FALSE)

    # Create profiles
    for (profile in profiles_to_install) {
        cat("\nInstalling profile:", profile, "\n")
        install_profile_packages(profile)
    }

    # Reset to default (or dev if it is defined)
    dev_profile <- set_renv_profile("dev")
    renv::activate(profile = dev_profile)
    safe_restore()

    # Cleaning root renv.lock (since we use profiles)
    unlink("renv.lock")
    unlink("renv/profile")

    return(invisible(profiles_to_install))
}
