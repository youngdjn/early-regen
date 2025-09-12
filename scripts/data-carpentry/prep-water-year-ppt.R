# Purpose: Take 2 years of monthly PRISM ppt data and calculate the water year ppt for 2021-Oct to
# 2022-Sept.

library(tidyverse)
library(terra)
library(here)
# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)

# Load ppt data
ppt_monthly_folder = file.path(datadir, "prism", "ppt-wy2022-4km")


# Compose filenames
yearmo_2021 = paste0("2021", c("10", "11", "12"))
yearmo_2022 = paste0("2022", c("01", "02", "03", "04", "05", "06", "07", "08", "09"))

yearmo = c(yearmo_2021, yearmo_2022)

filenames = paste0("PRISM_ppt_stable_4kmM3_", yearmo, "_bil.bil")
filepaths = file.path(ppt_monthly_folder, filenames)

ppt_monthly = rast(filepaths)

ppt_2022 = sum(ppt_monthly)

# Save
writeRaster(ppt_2022, file.path(ppt_monthly_folder, "summed_wy_2022", "ppt-2021-2022-water-year-4km.tif"), overwrite = TRUE)


## Repeat for tmean
tmean_monthly_folder = file.path(datadir, "prism", "tmean-wy2022-4km")

# Compose filenames
filenames = paste0("PRISM_tmean_stable_4kmM3_", yearmo, "_bil.bil")
filepaths = file.path(tmean_monthly_folder, filenames)  

tmean_monthly = rast(filepaths)

tmean_2022 = mean(tmean_monthly)

# Save
writeRaster(tmean_2022, file.path(tmean_monthly_folder, "summed_wy_2022", "tmean-2021-2022-water-year-4km.tif"), overwrite = TRUE)
