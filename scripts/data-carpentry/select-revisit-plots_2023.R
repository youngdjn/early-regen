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
         gn = toupper(plot_type) == "CORE" & (vol_grn_50m > 2 | (dist_grn_ALL < 50 & !is.na(dist_grn_ALL))),
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

st_write(plots, datadir("field-data/processed/spatial/temp_revisit_selection_2023.gpkg"), delete_dsn=TRUE)


## pull in the priority plots (just to get their IDs)

pri_plots = st_read(datadir("field-data/processed/spatial/temp_revisit_selection/creek-revisit2023-priority-plots.gpkg"))
pri_plot_ids = pri_plots$plot_id
pri_plot_ids = pri_plot_ids[pri_plot_ids != "C42"]
pri_plot_ids = c(pri_plot_ids,"C22","C41")

## the remaining priorities depend on plot type

plots = plots %>%
  filter(plot_id %in% pri_plot_ids) |>
  mutate(priority = 5,
         priority = ifelse(core_nogn_no_seedl, 4, priority),
         priority = ifelse(core_nogn_w_seedl, 3, priority),
         priority = ifelse(sw_w_seedl | sw_no_seedl | sw_int_seedl, 2, priority),
         priority = ifelse(gn,1, priority)) |>
  select(plot_id, plot_type, gn, starts_with("sw_"), starts_with("core_"), vol_grn_50m, dist_grn_ALL, priority) |>
  filter(!is.na(priority)) %>%
  mutate(Name = plot_id)

# remove excess core plots with no seedlings
plots = plots %>%
  filter(! plot_id %in% c("C45","C15","C032B","C22","C16","C062-332","C062-259"))


## Get the radius used in year 1: for each plot, for each species, get the records that had at least 1 seedl, and of those take the max diam. If that doesn't exist for a species, use 8.
d_sl = read_csv(datadir("field-data/processed/for-creek-2023-revisit/seedl_data_2021_2022.csv"))
d_sl_2021 = d_sl |>
  filter(year == "2021", fire == "Creek") |>
  filter((seedlings_0yr > 0) & !is.na(seedlings_0yr)) |>
  group_by(plot_id, species) |>
  summarize(radius_2yr = max(radius)) |>
  ungroup()




## Get the radius used in year 2 for 0 yr seedl: for each plot, for each species, get the records that had >= 1 seedl, and of those, take the max diam. If that doesn't exist for a species, use the normal rules but stop at 8
d_sl_2022 = d_sl |>
  filter(year == "2022", fire == "Creek") |>
  filter((seedlings_0yr > 0) & !is.na(seedlings_0yr)) |>
  group_by(plot_id, species) |>
  summarize(radius_1yr = max(radius)) |>
  ungroup()

## Combine
d_sl_radii = full_join(d_sl_2021, d_sl_2022) |>
  filter(species %in% c("PILA", "PIPJ", "ABCO", "CADE")) |>
  #fill out missing vals with 8
  # complete(#plot_id, species,
  #          fill = list(radius_2yr = 8,
  #                      radius_1yr = 8)) |>
  arrange(plot_id, species) |>
  select(plot_id, species, radius_1yr, radius_2yr) #|>
  # remove rows that are all 8s
  # mutate(all8 = (radius_1yr == 8 & radius_2yr == 8)) |>
  # filter(!all8) |>
  # select(-all8)

write_csv(d_sl_radii, datadir("field-data/processed/for-creek-2023-revisit/seedling_radii_creek_2023.csv"))






# 
# 
# 
# 
# ## write the plot data to a spreadsheet for checking in the field
# 
# plots_nonsp = plots
# st_geometry(plots_nonsp) = NULL
# 
# plots_nonsp = plots_nonsp %>%
#   select(Name, composite, priority, everything())
# 
# write_csv(plots_nonsp, datadir("field-data/processed/spatial/temp_revisit_selection/selected/creek-plot-data.csv"))

# write different components of the selected plots to kml (each priority separately)

plots_write = plots |>
  mutate(Name = paste0(plot_id, " (P", priority, ")")) |>
  select(Name)

st_write(plots_write |> filter(grepl(fixed("(P1)"), Name)), datadir("field-data/processed/for-creek-2023-revisit/creek_plots_priority1.kml"), delete_dsn = TRUE)
st_write(plots_write |> filter(grepl(fixed("(P2)"), Name)), datadir("field-data/processed/for-creek-2023-revisit/creek_plots_priority2.kml"), delete_dsn = TRUE)
st_write(plots_write |> filter(grepl(fixed("(P3)"), Name)), datadir("field-data/processed/for-creek-2023-revisit/creek_plots_priority3.kml"), delete_dsn = TRUE)
st_write(plots_write |> filter(grepl(fixed("(P4)"), Name)), datadir("field-data/processed/for-creek-2023-revisit/creek_plots_priority4.kml"), delete_dsn = TRUE)
st_write(plots_write |> filter(grepl(fixed("(P5)"), Name)), datadir("field-data/processed/for-creek-2023-revisit/creek_plots_priority5.kml"), delete_dsn = TRUE)
st_write(plots_write, datadir("field-data/processed/for-creek-2023-revisit/creek_plots_ALL.kml"), delete_dsn = TRUE)


## load creek drone assignment to get the table of attributes to export

drone = st_read(datadir("field-data/processed/spatial/temp_revisit_selection/creek-drone-assigment-2022.gpkg"))

drone_centroids = st_centroid(drone)

st_write(drone_centroids,datadir("field-data/processed/spatial/temp_revisit_selection/creek-drone-assigment-2022-centroids.gpkg"))

st_geometry(drone) = NULL


write_csv(drone,datadir("field-data/processed/spatial/temp_revisit_selection/creek-drone-assignment-2022-data.csv"))

