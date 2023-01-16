library(tidyverse)
library(here)
library(sf)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


# load processed plot data

plots = st_read(datadir("field-data/processed/spatial/all_plots.gpkg"))

plots = plots %>%
  filter(toupper(fire) == "CREEK") %>%
  mutate(core_nogn_w_seedl = toupper(plot_type) == "CORE" & vol_grn_50m <= 5 & seedl_dens_ALL > 0 & (dist_grn_ALL > 75 | is.na(dist_grn_ALL)),
         gn = toupper(plot_type) == "CORE" & vol_grn_50m > 5,
         sw_w_seedl = toupper(plot_type) == "SEEDWALL" & seedl_dens_ALL > .007,
         sw_no_seedl = toupper(plot_type) == "SEEDWALL" & seedl_dens_ALL == 0,
         sw_int_seedl = toupper(plot_type) == "SEEDWALL" & seedl_dens_ALL < 0.007 & seedl_dens_ALL > 0,
         core_nogn_no_seedl = toupper(plot_type) == "CORE" & vol_grn_50m <= 5 & seedl_dens_ALL == 0 & (dist_grn_ALL > 75 | is.na(dist_grn_ALL))) %>%
  mutate(composite = case_when(core_nogn_w_seedl ~ "core_nogn_w_seedl",
                               core_nogn_no_seedl ~ "core_nogn_no_seedl",
                               gn ~ "gn",
                               sw_w_seedl ~ "sw_w_seedl",
                               sw_no_seedl ~ "sw_no_seedl",
                               sw_int_seedl ~ "sw_int_seedl",
                               TRUE ~ "none"))

st_write(plots, datadir("field-data/processed/spatial/temp_revisit_selection.gpkg"), delete_dsn=TRUE)


## pull in the priority plots (just to get their IDs)

pri_plots = st_read(datadir("field-data/processed/spatial/temp_revisit_selection/creek-revisit-priority-plots.gpkg"))
pri_plot_ids = pri_plots$plot_id
pri_plot_ids = pri_plot_ids[pri_plot_ids != "C42"]
pri_plot_ids = c(pri_plot_ids,"C22","C41")

## the rest of the seed wall plots are low-pri

plots = plots %>%
  mutate(priority = ifelse(toupper(plot_type) == "SEEDWALL",2, NA),
          priority = ifelse(plot_id %in% pri_plot_ids, 1, priority)) %>%
  filter(!is.na(priority)) %>%
  mutate(Name = plot_id)

# remove excess core plots with no seedlings
plots = plots %>%
  filter(! plot_id %in% c("C45","C15","C032B","C22","C16","C062-332","C062-259"))


## write the plot data to a spreadsheet for checking in the field

plots_nonsp = plots
st_geometry(plots_nonsp) = NULL

plots_nonsp = plots_nonsp %>%
  select(Name, composite, priority, everything())

write_csv(plots_nonsp, datadir("field-data/processed/spatial/temp_revisit_selection/selected/creek-plot-data.csv"))

# write different components of the selected plots to kml

# core far from green
st_write(plots %>% filter(composite %in% c("core_nogn_w_seedl","core_nogn_no_seedl")), datadir("field-data/processed/spatial/temp_revisit_selection/selected/creek-core.kml"), delete_dsn = TRUE)

# dm
st_write(plots %>% filter(composite %in% c("gn")), datadir("field-data/processed/spatial/temp_revisit_selection/selected/creek-dm.kml"), delete_dsn = TRUE)

# sw priority
st_write(plots %>% filter(toupper(plot_type) == "SEEDWALL" & priority == 1), datadir("field-data/processed/spatial/temp_revisit_selection/selected/creek-sw-highpriority.kml"), delete_dsn = TRUE)

# sw low priority
st_write(plots %>% filter(toupper(plot_type) == "SEEDWALL" & priority == 2), datadir("field-data/processed/spatial/temp_revisit_selection/selected/creek-sw-lowpriority.kml"), delete_dsn = TRUE)






## load creek drone assignment to get the table of attributes to export

drone = st_read(datadir("field-data/processed/spatial/temp_revisit_selection/creek-drone-assigment-2022.gpkg"))

drone_centroids = st_centroid(drone)

st_write(drone_centroids,datadir("field-data/processed/spatial/temp_revisit_selection/creek-drone-assigment-2022-centroids.gpkg"))

st_geometry(drone) = NULL


write_csv(drone,datadir("field-data/processed/spatial/temp_revisit_selection/creek-drone-assignment-2022-data.csv"))

