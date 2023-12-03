library(sf)
library(terra)
library(here)
library(tidyverse)
library(tidyterra)
library(ggspatial)


# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)

# load full res ortho
r = rast(file.path(datadir, "orthomosaic/Caldor-C013_cropped_agg.tif"))

# load seed wall plots
sw = st_read(file.path(datadir, "intermediate-inspection/sw_allsp_v2.gpkg")) |>
  filter(grepl("S012", plot_id)) |>
  filter(! plot_id %in% c("S012-531", "S012-532", "S012-533"))
core = st_read(file.path(datadir, "intermediate-inspection/core_allsp_v2.gpkg")) |>
  filter(grepl("C013", plot_id))

core_torch = core |>
  filter(fire_intens > 85)
core_scorch = core |>
  filter(fire_intens < 85)

all_except_supp = bind_rows(sw, core_torch, core_scorch)

supp1 = data.frame(lon = -120.2875, lat = 38.64678)
supp1 = st_as_sf(supp1, coords = c("lon", "lat"), crs = st_crs(core_torch)) |> rename(geom = "geometry")
supp1$fire = "test"
core_torch = bind_rows(core_torch,supp1)

supp2 = data.frame(lon = c(-120.2891, -120.287), lat = c(38.6462, 38.6472))
supp2 = st_as_sf(supp2, coords = c("lon", "lat"), crs = st_crs(core_torch)) |> rename(geom = "geometry")
supp2$fire = "test"
core_scorch = bind_rows(core_scorch,supp2)



supp = bind_rows(supp1, supp2)


## add some more hypothetical core plots




map = ggplot() +
  geom_spatraster_rgb(data = r, max_col_value = 120) +
  geom_sf(data = all_except_supp, color = "white", size = 3) +  
  geom_sf(data = supp, color = "white", size = 3) +  
  geom_sf(data = sw, color = "#A3E435", size = 1.5) +
  geom_sf(data = core_scorch, color = "#9D5B0B", size = 1.5) +
  geom_sf(data = core_torch, color = "black", size = 1.5) +
  theme_void() +
  annotation_scale(pad_x = unit(.8,"cm"),
                   pad_y = unit(1,"cm"), location = "tl",
                   width_hint = 0.1,
                   height = unit(.01,"cm"))
  
map

png(file.path(datadir, "figures/maps/drone-ortho.png"), res = 3*300, width = 3*2000, height = 3*1200)
map
dev.off()
