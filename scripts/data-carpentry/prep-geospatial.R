# Purpose: Prepare shapefiles of plot points for different plot subsets

library(tidyverse)
library(here)
library(sf)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


#### Load plot data ####

d = read_csv(datadir("field-data/processed/plot_seedl_cone_grnseedsource.csv"))

d = st_as_sf(d,coords=c("lon","lat"), crs=4326)

## write all plots

st_write(d, datadir("field-data/processed/spatial/all_plots.gpkg"))

### core pine plots

core_nogrn_pine = d %>%
  filter(toupper(plot_type) == "CORE") %>%
  filter(dist_grn_PINES > 100 | is.na(dist_grn_PINES))

st_write(core_nogrn_pine, datadir("field-data/processed/spatial/core_nogrn_pine.gpkg"))



