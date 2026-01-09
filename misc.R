get_current_file_name <- function(ext = TRUE) {
    basename(get_current_file_path(ext = ext))
}

get_current_file_path <- function(ext = TRUE) {
    file <- this.path::this.path()
    if (ext) file else fs::path_ext_remove(file)
}

update_submodules <- function() {
    system("git submodule foreach git pull origin main", intern = TRUE)
}
