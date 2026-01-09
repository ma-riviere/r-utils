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

get_wsl_info <- function() {
    os_type <- tolower(Sys.info()[[1]])
    os_info <- system("echo $WSL_DISTRO_NAME", intern = TRUE)
    os_info <- strsplit(os_info, "-", fixed = TRUE)[[1]]

    return(list(
        os_type = tolower(Sys.info()[[1]]),
        os_name = tolower(os_info[1]),
        os_version = tolower(os_info[2])
    ))
}
