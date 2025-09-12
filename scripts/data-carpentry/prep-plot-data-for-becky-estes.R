# Purpose: save a shapefile of plot data for Becky Estes with basic field-observed attributes

library(tidyverse)
library(here)
library(sf)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

d = st_read(file.path(data_dir, "field-data", "processed", "early-regen-2022.gpkg"))

d_foc = d |>
    filter(fire == "Caldor") |>
    select(plot_id, plot_type, dist_green = dist_grn_ALL, starts_with("seedl_dens_")) |>
    # convert seedlings per sq m to seedlings per acre, and m to feet
    mutate(across(starts_with("seedl_dens_"), ~ . * 4046.86),
           dist_green = dist_green * 3.28084)

st_write(d_foc, file.path(data_dir, "field-data", "processed", "ucdavis_young_caldor_regenplots.gpkg"))
st_write(d_foc, file.path(data_dir, "field-data", "processed", "ucdavis_young_caldor_regenplots", "ucdavis_young_caldor_regenplots.shp"))
