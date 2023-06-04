# Purpose: Prepare manually entered plot data for analysis

##!! TODO: make sure that scorched needle volume by species is being computed correctly

library(tidyverse)
library(here)
library(sf)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))

d = read_csv(datadir("field-data/processed/plot-data-prepped.csv")) |>
  mutate(across(contains("_dens_"), ~round(., digits = 2)))


d_verbose = d |>
  select(fire, plot_id, plot_type, dist_grn_ALL, starts_with("radius_"), starts_with("vol_"), starts_with("seedl_dens_"), starts_with("dist_grn_"), fire_intens, day_of_burning, shrub_cover, sight_line, lat, lon) |>
  arrange(fire, plot_id)

# Write this for Derek reference in field
write_csv(d_verbose, file.path(data_dir, "resurvey-prep/previous-plot-summary_verbose.csv"))

d_minimal = d |>
  select(fire, plot_id, plot_type, starts_with("radius_")) |>
  arrange(fire, plot_id)

# Write this into a table to print for crew
write_csv(d_minimal, file.path(data_dir, "resurvey-prep/previous-plot-summary_for-crew.csv"))

# Also make it a kml for Avenza: all and separate for seed wall, core, delayed
d_geo = d |>
  select(plot_id, plot_type, starts_with("radius_"), lat, lon) |>
  mutate(Name = plot_id) |>
  select(Name, everything()) |>
  arrange(plot_id)
d_geo = st_as_sf(d_geo, coords = c("lon", "lat"), crs = 4326)

d_geo_sw = d_geo |> filter(plot_type == "seedwall")
d_geo_core = d_geo |> filter(plot_type == "core")
d_geo_delayed = d_geo |> filter(plot_type == "delayed")

st_write(d_geo, file.path(data_dir, "resurvey-prep/previous-plots_all.kml"), delete_dsn = TRUE)
st_write(d_geo_sw, file.path(data_dir, "resurvey-prep/previous-plots_seedwall.kml"), delete_dsn = TRUE)
st_write(d_geo_core, file.path(data_dir, "resurvey-prep/previous-plots_core.kml"), delete_dsn = TRUE)
st_write(d_geo_delayed, file.path(data_dir, "resurvey-prep/previous-plots_delayed.kml"), delete_dsn = TRUE)

## Find exemplary plots far from seed source with high seedlings

d_exemp = d_verbose |>
  filter(sight_line > 100 | is.na(sight_line),
         dist_grn_ALL > 100 | is.na(dist_grn_ALL),
         seedl_dens_ALL > 0.1) |>
  mutate(Name = plot_id) |>
  select(Name, everything())

# Write to KML for Avenza in the field
d_geo = st_as_sf(d_exemp, coords = c("lon", "lat"), crs = 4326)
st_write(d_geo, file.path(data_dir, "resurvey-prep/previous-plots_exemplary.kml"), delete_dsn = TRUE)
