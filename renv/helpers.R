get_r_version <- function() {
    return(paste0(version$major, ".", sub("\\..*", "", version$minor)))
}

is_installed <- function(pkg) {
    suppressMessages({require(pkg, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)})
}

read_packages <- function(profile) {
    packages_file <- paste0("packages-", profile, ".txt")
    packages <- readLines(packages_file)
    packages <- trimws(packages)
    packages <- packages[packages != ""]
    packages <- gsub(packages, pattern = ",", replacement = "")
    return(packages)
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
    source("renv/activate.R")
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
