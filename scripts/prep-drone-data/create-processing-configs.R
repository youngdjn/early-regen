

#### Inputs

# Dir to save derived YMLs
save_dir = "~/Documents/temp/yml_out/"

# Path to base YML
base_yml_path = here("config/example.yml")

# Replacement data frame. First col ("name") will be the filename (sans extension) of the derived YML. The remaining cols are the YML keys to replace the values for.
# Each row will translate to a different derived YML.
# Each row of the DF must contain a name (for the resulting yml file) and at elast one YML key to replace the values of.

replacements = data.frame(
  name = c("run1", "run2", "run3"),
  photo_path = c("testpath1", "testpath2", "testpath2"),
  run_name = c("testrun1", "testrun2", "testrun3")
)