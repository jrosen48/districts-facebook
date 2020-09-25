library(targets)

source(here::here("R", "functions.R"))

# Set target-specific options such as packages.
tar_option_set(packages = c("readr", "ggplot2", "janitor", "dplyr")
               )

# Define targets
targets <- list(
  tar_target(file, list.files(here::here("data"), 
                              full.names = TRUE,
                              pattern = "\\.csv$"), 
             format = "file"),
  tar_target(raw_data, read_csv(file)),
  tar_target(d, prep_data(raw_data)),
  tar_target(hist_perform, create_hist_perform(d), format = "file")
)

# End with a call to tar_pipeline() to wrangle the targets together.
# This target script must return a pipeline object.
tar_pipeline(targets)
