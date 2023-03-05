## Prepare metashape configs for each drone dataset based on an existing base YML

library(here)
library(tidyverse)

source("/ofo-share/utils/automate-metashape/R/create_derived_configs.R")


# Dir to save derived YMLs (using the repo)
save_dir = here("metashape-configs/derived/120m-01")
if(!dir.exists(save_dir)) dir.create(save_dir)

# Path to base YML
base_yml_path = here("metashape-configs/base/base-config_120m-01.yml")



#### Prepare replacements data frame. For each replacement, need the folder of images

image_folders = list.dirs("/ofo-share/early-regen_dronedata-partial/imagery-raw-120m/priority-1", recursive = FALSE)

# Base folder name (to use as photogrammetry run name)
basefolders = image_folders |> str_split("/") |> map(-1) |> unlist()

# Paths to the image sets, to replace the base config value with
photo_paths = image_folders

# Filename to use for saving the config (also used as the metashape run name per the base config option [run_name: "from_config_filename"]
config_names = basefolders |> str_replace_all(fixed("_"), "-") # underscores to dashes to make it easier to machine-read the metashape product file names later (beause other filname components are separated by underscores)

# Create replacement data frame. First col ("name") will be the filename (sans extension) of the derived YML. The remaining cols are the YML keys to replace the values for.
# Each row will translate to a different derived YML.

replacements = data.frame(
  name = config_names,
  photo_path = photo_paths
)

## Create the derived files
create_derived_configs(base_yml_path, save_dir, replacements, automate_metashape_path = "/ofo-share/utils/automate-metashape", shell_script_filename = "priority-1")
