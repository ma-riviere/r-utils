get_r_version <- function() {
    return(paste0(version$major, ".", version$minor))
}

get_os_info <- function() {
    os_type <- tolower(Sys.info()[[1]])
    os_release_lines <- readLines("/etc/os-release")

    os_info <- list()
    for (line in os_release_lines) {
        if (grepl("=", line)) {
            parts <- strsplit(line, "=", fixed = TRUE)[[1]]
            key <- tolower(trimws(parts[1]))
            if (key %notin% c("id", "version_id", "version_codename")) {
                next
            }
            value <- trimws(parts[2])
            value <- gsub("\"", "", value, fixed = TRUE) # Remove all double quotes
            os_info[[key]] <- tolower(value)
        }
    }
    return(list(
        os_type = os_type,
        os_name = os_info$id,
        os_version = os_info$version_id,
        os_codename = os_info$version_codename
    ))
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
