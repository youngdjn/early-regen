# Make maps of the two study fires (shading: day of burning) with study plots on them

library(tidyverse)
library(here)
library(sf)
library(terra)
library(rnaturalearth)
library(tidyterra)
library(lubridate)
library(scales)
library(ggspatial)

# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)

## Load fire perims
perim_caldor = st_read(file.path(datadir, "fire-perims/caldor/ca3858612053820210815_20201011_20211016_burn_bndy.shp")) |> mutate(name = "Caldor")
perim_dixie = st_read(file.path(datadir, "fire-perims/dixie/ca3987612137920210714_20201012_20211015_burn_bndy.shp")) |> mutate(name = "Dixie")
fires = bind_rows(perim_caldor, perim_dixie)
fires = st_simplify(fires, preserveTopology = FALSE, dTolerance = 100)

states = ne_states(country = "united states of america", returnclass = c("sf"))

### Load field plots and thin to the ones used for analysis. NOTE: this repeats code in plot_raw_data() so potentially move that to a separate function to re-use
source("scripts/analysis/year1-dixie-caldor_functions.R")
# Load data
d = read_csv(file.path(datadir,"field-data/processed/plot-data-prepped_v2.csv"))
d = d |>
  rename(fire = "Fire")
d_sp = prep_d_sp("ALL")
d_sp_sw = d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60)
d_sp_nogrn = d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed"))
allplots = bind_rows(d_sp_nogrn, d_sp_sw) |>
  mutate(plot_type = recode(plot_type, "delayed" = "core")) |> # this may select some delayed mortality plots that behave as core plots because they're > 100 m from green.
  mutate(plot_type = recode(plot_type, "core" = "Interior", "seedwall" = "Edge"))
## Get coords for these plots
plots_spatial = st_read(file.path(datadir, "field-data/processed/early-regen-2022_v2.gpkg")) |> select(plot_id)
plots = left_join(allplots, plots_spatial, by = "plot_id") |> st_sf()


## Get day of burning rasters
dob_caldor = rast(file.path(datadir, "day-of-burning-rasters/2021-CAENF-024030_dob.tif"))
dob_dixie = rast(file.path(datadir, "day-of-burning-rasters/2021-CAPNF-001273_dob.tif"))
dob_dixie = mask(dob_dixie, perim_dixie)


# ## Convert day of burning to date
# dob_to_date = function(dob) {
#   ymd("2021-01-01") + dob - 1
# }
# 
# date_to_dob = function(date) {
#   datedate = ymd(date)
#   dob_pre = days(datedate - ymd("2021-01-01")) + days(1)
#   dob = round(period_to_seconds(dob_pre) / 86400)
#   return(dob)
# }
# 
# trans_dob = trans_new("dob",
#                       dob_to_date,
#                       date_to_dob,
#                       format = label_date("%b %Y"))


ca = ggplot() +
  geom_sf(data = states, fill = NA, linewidth = 01) +
  geom_sf(data = fires, fill = "red", color = NA) +
  theme_bw(30) +
  coord_sf(xlim=c(-125,-112),ylim=c(32,45)) +
  scale_x_continuous(breaks = c(-124, -116)) +
  scale_y_continuous(breaks = c(32, 36, 40, 44)) +
  theme(panel.grid = element_blank())
ca

png(file.path(datadir, "figures/maps/ca.png"), res = 350, width = 1500, height = 1500)
ca
dev.off()

dob_caldor[dob_caldor > 258] = 258 # there is an area we didn't sample that burned much later than the rest, even later than Dixie.

caldor = ggplot() +
  geom_spatraster(data = dob_caldor) +
  geom_sf(data = fires |> filter(name == "Caldor"), fill = "NA", color = "red", linewidth = 0.5) +
  geom_sf(data = plots |> filter(fire == "Caldor"), aes(color = plot_type), color = "white", size = 3.5) +
  geom_sf(data = plots |> filter(fire == "Caldor"), aes(color = plot_type), size = 2) +
  scale_fill_viridis_c(na.value = NA, breaks = c(182, 196, 213, 227, 244, 258), labels = c("01-Jul", "15-Jul", "01-Aug","15-Aug", "01-Sep", "15-Sep +"), name = "Day of burning", limits = c(182,258)) +
  scale_color_manual(values = c("Interior" = "#9D5B0B","Edge" = "#A2D435"), name = "Plot type") +
  coord_sf(crs = 4326, ylim = c(38.4, 39.0), xlim = c(-120.65, -119.92)) +
  scale_y_continuous(breaks = c(38.4, 38.6, 38.8)) +
  scale_x_continuous(breaks = c(-120.6, -120.3, -120.0)) +
  theme_bw(15) +
  annotation_scale(pad_x = unit(0.7,"cm"),
                   pad_y = unit(1,"cm"), location = "tl", text_cex = 1, bar_cols = c("black", "black"),
                   height = unit(.01,"cm"))
caldor

png(file.path(datadir, "figures/maps/caldor.png"), res = 300, width = 1800, height = 1000)
caldor
dev.off()

dixie = ggplot() +
  geom_spatraster(data = dob_dixie) +
  geom_sf(data = fires |> filter(name == "Dixie"), fill = "NA", color = "red", linewidth = 0.5) +
  geom_sf(data = plots |> filter(fire == "Dixie"), aes(color = plot_type), color = "white", size = 3.5) +
  geom_sf(data = plots |> filter(fire == "Dixie"), aes(color = plot_type), size = 2) +
  scale_fill_viridis_c(na.value = NA, breaks = c(182, 196, 213, 227, 244, 258), labels = c("01-Jul", "15-Jul", "01-Aug","15-Aug", "01-Sep", "15-Sep +"), name = "Day of burning", limits = c(182,258)) +
  scale_color_manual(values = c("Interior" = "#9D5B0B","Edge" = "#A2D435"), name = "Plot type") +
  #coord_sf(crs = 3310, ylim = c(210000, 305000), xlim = c(-140000,-55000)) +
  coord_sf(crs = 4326, ylim = c(39.87, 40.78), xlim = c(-121.6, -120.6)) +
  scale_x_continuous(breaks = c(-121.6, -121.2, -120.8)) +
  scale_y_continuous(breaks = c(40.0, 40.4, 40.8)) +
  theme_bw(15) +
  annotation_scale(pad_x = unit(0.7,"cm"),
                   pad_y = unit(1,"cm"), location = "tr", text_cex = 1, bar_cols = c("black", "black"),
                   height = unit(.01,"cm"))
dixie

png(file.path(datadir, "figures/maps/dixie.png"), res = 300, width = 1600, height = 1000)
dixie
dev.off()
