# Takes the output of prep-plot-data.R and cleans it by removing unnecessary columns in order to publish it

library(tidyverse)
library(here)

# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)

# Load data
d = read_csv(file.path(datadir,"field-data/processed/plot-data-prepped.csv"))

d_pub = d |>
  select(fire, plot_id, plot_type, survey_date = date, lat, lon, shrub_cover, shrub_ht, herb_cover, herb_ht, litter_cover, litter_depth, moss_cover, branches_cover, vol_grn_50m:vol_blk_50m, vol_grn_10m:vol_blk_10m, sight_line, mean_tree_dbh, mean_seedwall_height,
         seedwall_dist, prefire_prop_YLWPINES:prefire_prop_CADE, seedwall_density_cat, seedwall_density, dom_yellpine_cone_sp, prefire_prop_PINES:prefire_prop_ALL, seedl_dens_ABCO:seedl_dens_ABIES, cone_dens_PILA:cone_dens_PINES, under_cones_new_PILA:under_cones_new_PINES,
         dist_grn_PILA:dist_grn_ABIES, ends_with("_green_vol"), ends_with("_untocrhed_vol"), ends_with("_green_vol_abs"), ends_with("_untocrhed_vol_abs"), ba, capable_growing_area, fire_intens, day_of_burning, sri, ppt, tmean) |>
  select(-starts_with("PIPJ")) |>
  # round the lat/long to 3 decimals (about 100 m)
  mutate(across(c(lat,lon), ~round(.,digits = 3)))
  
write_csv(d_pub, file.path(datadir,"field-data/processed/plot-data-prepped-pub.csv"))
