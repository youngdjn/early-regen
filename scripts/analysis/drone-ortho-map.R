library(sf)
library(terra)
library(here)
library(tidyverse)

# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))

# load full res ortho
r = rast(file.path(data_dir, "orthomosaic/Caldor-C013_cropped_agg.tif"))

# load seed wall plots
sw = st_read(file.path(data_dir, "intermediate-inspection/sw_allsp_v2.gpkg")) |>
  filter(grepl("S012", plot_id)) |>
  filter(! plot_id %in% c("S012-531", "S012-532", "S012-533"))
core = st_read(file.path(data_dir, "intermediate-inspection/core_allsp_v2.gpkg")) |>
  filter(grepl("C013", plot_id))

core_torch = core |>
  filter(fire_intens2 > 75)
core_scorch = core |>
  filter(fire_intens2 < 75)

all = bind_rows(sw, core)

# 
# library(tidyverse)
# library(here)
# library(sf)
# library(terra)
# library(rnaturalearth)
library(tidyterra)
# library(lubridate)
# library(scales)
 library(ggspatial)

map = ggplot() +
  geom_spatraster_rgb(data = r, max_col_value = 120) +
  geom_sf(data = all, color = "white", size = 2) +  
  geom_sf(data = sw, color = "#A2D435", size = 1.5) +
  geom_sf(data = core_scorch, color = "#9D5B0B", size = 1.5) +
  geom_sf(data = core_torch, color = "black", size = 1.5) +
  theme_void() +
  annotation_scale(pad_x = unit(.8,"cm"),
                   pad_y = unit(.6,"cm"), location = "tl",
                   width_hint = 0.1,
                   height = unit(.01,"cm"))
  
map

png(file.path(data_dir, "figures/maps/drone-ortho.png"), res = 300, width = 2000, height = 1200)
map
dev.off()
