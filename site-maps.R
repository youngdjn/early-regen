library(tidyverse)
library(here)
library(sf)
library(terra)
library(rnaturalearth)
library(tidyterra)
library(lubridate)
library(scales)

# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)

## Load fire perims
perim_caldor = st_read(file.path(datadir, "fire-perims/caldor/ca3858612053820210815_20201011_20211016_burn_bndy.shp")) |> mutate(name = "Caldor")
perim_dixie = st_read(file.path(datadir, "fire-perims/dixie/ca3987612137920210714_20201012_20211015_burn_bndy.shp")) |> mutate(name = "Dixie")
fires = bind_rows(caldor, dixie)
fires = st_simplify(fires, preserveTopology = FALSE, dTolerance = 100)

states = ne_states(country = "united states of america", returnclass = c("sf"))

### Load field plots and thin to the ones used for analysis. NOTE: this repeats code in plot_raw_data() so potentially move that to a separate function to re-use
source("scripts/analysis/year1-dixie-caldor_functions.R")
# Load data
d = read_csv(file.path(datadir,"field-data/processed/plot-data-prepped.csv"))
d = d |>
  filter(fire %in% c("Caldor", "Dixie")) |>
  # remove a plot that imagery revealed to be near some marginally green trees
  filter(!(plot_id %in% "C22-029")) |>
  mutate(fire_intens =  100 - pmax(litter_cover,vol_brn_50m),
         fire_intens2 = 100 - ((litter_cover+vol_brn_50m)/2),
         fire_intens10 = 100 - (litter_cover + vol_brn_10m)/2) |>
  # calc capable growing area
  mutate(capable_growing_area = 1 - nongrowing_cover/100) |>
  mutate(across(starts_with("seedl_dens_"), ~./capable_growing_area)) # seedling density within the cabaple growing area
d_sp = prep_d_sp("ALL")
d_sp_sw = d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60)
d_sp_nogrn = d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed")) |>  # prep for figure: classify fire intens
  mutate(fire_intens2_cat = ifelse(fire_intens2 < median(fire_intens2), "Scorched", "Torched")) |>
  mutate(fire_intens_cat_foc = fire_intens2_cat)
allplots = bind_rows(d_sp_nogrn, d_sp_sw) |>
  mutate(plot_type = recode(plot_type, "delayed" = "core")) # this may select some delayed mortality plots that behave as core plots because they're > 100 m from green.

## Get coords for these plots
plots_spatial = st_read(file.path(datadir, "field-data/processed/early-regen-2022.gpkg")) |> select(plot_id)
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
  geom_sf(data = states, fill = NA, linewidth = 0.3) +
  geom_sf(data = fires, fill = "red", color = NA) +
  theme_bw(20) +
  coord_sf(xlim=c(-125,-112),ylim=c(32,45)) +
  scale_x_continuous(breaks = c(-124, -120, -116, -112)) +
  scale_y_continuous(breaks = c(32, 36, 40, 44)) +
  theme(panel.grid = element_blank())
ca


caldor = ggplot() +
  geom_spatraster(data = dob_caldor) +
  geom_sf(data = fires |> filter(name == "Caldor"), fill = "NA", color = "red", linewidth = 0.5) +
  geom_sf(data = plots |> filter(fire == "Caldor"), aes(color = plot_type), color = "white", size = 2.5) +
  geom_sf(data = plots |> filter(fire == "Caldor"), aes(color = plot_type), size = 2) +
  scale_fill_viridis_c(na.value = NA, breaks = c(227, 244, 258), labels = c("15-Aug", "01-Sep", "15-Sep"), name = "Day of burning") +
  scale_color_manual(values = c("core" = "#9D5B0B","seedwall" = "#A2D435"))
caldor

dixie = ggplot() +
  geom_spatraster(data = dob_dixie) +
  geom_sf(data = fires |> filter(name == "Dixie"), fill = "NA", color = "red", linewidth = 0.5) +
  geom_sf(data = plots |> filter(fire == "Dixie"), aes(color = plot_type), color = "white", size = 2.5) +
  geom_sf(data = plots |> filter(fire == "Dixie"), aes(color = plot_type), size = 2) +
  scale_fill_viridis_c(na.value = NA, breaks = c(182, 196, 213, 227, 244, 258), labels = c("01-Jul", "15-Jul", "01-Aug","15-Aug", "01-Sep", "15-Sep"), name = "Day of burning") +
  scale_color_manual(values = c("core" = "#9D5B0B","seedwall" = "#A2D435")) +
  #coord_sf(crs = 3310, ylim = c(210000, 305000), xlim = c(-140000,-55000)) +
  coord_sf(crs = 4326, ylim = c(39.9, 40.75), xlim = c(-121.6, -120.6)) +
  scale_x_continuous(breaks = c(-121.6, -121.2, -120.8)) +
  theme_bw(10) +
  annotation_scale(pad_x = unit(1,"cm"),
                   pad_y = unit(1,"cm"), location = "tr")
dixie







p = ggplot() +
  geom_sf(data=ca,fill=NA) +
  geom_sf(data=eco,aes(fill=ecoregion_coarse),color=NA) +
  theme_bw(20) +
  scale_fill_manual(values=colors,name="Ecoregion") +
  coord_sf(xlim=c(-125,-117.5),ylim=c(36,42))

png("data/figures/ecoregion_map.png",width=800,height=600)
p
dev.off()


#### Map the focal region within the western US

states = st_read("data/carto/cb_2018_us_state_5m/cb_2018_us_state_5m.shp") %>% st_transform(3310)
states = st_buffer(states,0)

# crop states to western US

bbox = st_bbox(c(xmin=-500000, xmax = 1000000,
                 ymin = -800000, ymax = 1000000), crs=3310)

bbox %>% st_as_sfc(bbox)


bbox_inset = st_bbox(c(xmin=-125, xmax=-117.5,
                       ymin=36, ymax=42), crs=4326)


states_crop = st_intersection(states, bbox %>% st_as_sfc())

p_inset = ggplot() +
  geom_sf(data=states %>% st_transform(3310),fill=NA, size=0.2) +
  geom_sf(data=bbox_inset %>% st_as_sfc %>% st_transform(3310),color="red", fill=NA, size=1.2) +
  theme_bw(15) +
  coord_sf(xlim= c(-500000,1100000),
           ylim = c(-800000,1100000)) +
  theme(plot.background = element_rect(colour = "black", fill="white", size=1))
p_inset

png("data/figures/vicinity_map.png",width=800,height=600)
p_inset
dev.off()


library(patchwork)

fig = p + inset_element(p_inset, .03,.04,.5,.5)
fig

png("data/figures/map_fig_v2.png",width=7*800,height=7*600, res=3*200)
fig
dev.off()


