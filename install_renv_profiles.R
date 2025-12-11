# Cleaning existing library (to force reinstall from scratch)
unlink("renv/library", recursive = TRUE)
source("renv/activate.R")
renv::upgrade(prompt = FALSE)

# Create profiles
profiles <- get_user_profiles()
for (profile in profiles) {
    cat("\nInstalling profile:", profile, "\n")
    install_profile_packages(profile)
}

# Cleaning root renv.lock (since we use profiles)
unlink("renv.lock")
unlink("renv/profile")

# Reset to default (or dev if it is defined)
dev_profile <- set_renv_profile("dev")
renv::activate(profile = dev_profile)
safe_restore()
