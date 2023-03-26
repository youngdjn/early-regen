library(tidyverse)
library(mgcv)
library(here)
library(scales)
library(ggpubr)
library(lubridate)

source("scripts/analysis/year1-dixie-caldor_functions.R")

# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)


# Load data
d = read_csv(file.path(datadir,"field-data/processed/plot-data-prepped.csv"))

# Keep Caldor and Dixie 2022 only
d = d |>
  filter(fire %in% c("Caldor", "Dixie")) |>
  # remove plots that imagery revealed to be near marginally green trees
  filter(!(plot_id %in% c("C22-029", "C041-500", "D042-207")))

# Check distrib of seedling density
hist(d$seedl_dens_ALL, breaks=200)

# Calc fire intensity (scorch vs torch) indices
d = d |>
  mutate(fire_intens =  100 - pmax(litter_cover,vol_brn_50m),
         fire_intens2 = 100 - ((litter_cover+vol_brn_50m)/2),
         fire_intens10 = 100 - (litter_cover + vol_brn_10m)/2) |>
  # these should go into data prep script:
  mutate(ba = ba_factor * ba_tally) |>
  mutate(capable_growing_area = 1 - nongrowing_cover/100) |>
  mutate(across(starts_with("seedl_dens_"), ~./capable_growing_area)) # seedling density within the cabaple growing area

  


### Environmental context across all plots
## Precip by date burned

#### Constants

## Define the date windows over which to summarize raw data (by day of year)
windows = data.frame(fire = c("Caldor","Caldor", "Dixie", "Dixie", "Dixie"),
                     start = c(229, 239, 203, 225, 252),
                     end =   c(231, 242, 205, 230, 252))


#### Analysis

# when plotting model fits, trim to range of data minus what percentile of data points at the extremes
percentile_exclude = 0.025

## For ALL SPECIES
# Prep data
d_sp = prep_d_sp("ALL")

# plot seed source distance to seed wall vs climate
d_sw = prep_d_sw_mod(d_sp, max_sw_dist = 60)
p = ggplot(d_sw, aes(x = ppt, y = dist_sw, color = day_of_burning, shape = fire)) +
  geom_point(size = 3) +
  theme_bw(15) +
  scale_shape(name = "Fire") +
  scale_color_viridis_c(na.value = NA, breaks = c(182, 196, 213, 227, 244, 258), labels = c("01-Jul", "15-Jul", "01-Aug","15-Aug", "01-Sep", "15-Sep"), name = "Day of burning") +
  labs(x = "Mean annual precipitation (mm)", y = "Distance to edge (m)")
p

png(file.path(datadir, "figures/supp_dist_ppt.png"), res = 200, width = 1500, height = 1100)
p
dev.off()


# It shows there are few seed wall plots close to seed sources at low precip.
# So possibly the low-precip seed wall plots underestimate regen density.
# So where are the low-precip seed wall plots (day of burning and fire)?
# They are most of the Dixie plots, at all three day of burning levels.

# Plot seed wall plot seed source distance by day of burning and fire
p = ggplot(d_sw, aes(x = day_of_burning, y = dist_sw, color = fire, shape = fire)) +
  geom_point(size = 3) +
  theme_bw(15) +
  scale_shape(name = "Fire") +
  scale_color_viridis_d(name = "Fire", begin = 0.2, end = 0.8) +
  scale_x_continuous(breaks = c(182, 196, 213, 227, 244, 258), labels = c("01-Jul", "15-Jul", "01-Aug","15-Aug", "01-Sep", "15-Sep"), name = "Day of burning") +
  labs(y = "Distance to edge (m)")

png(file.path(datadir, "figures/supp_dist_dob.png"), res = 200, width = 1500, height = 1100)
p
dev.off()  

# Some of the earliest and latest Dixie seed walls have  a further distance from seed source,
# so this may exaggerate the regeneration density of core plots compared against them


# Plot raw data
plot_raw_data(d_sp, axis_label = bquote(Seedlings~m^-2), plot_title = NULL, filename = "all")


#### get proportions by species

##TODO: exclude the early burn plots? compute by median?

# for core area plots
d_spcomp =  d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed")) |>
  summarize(ABIES = sum(seedl_dens_ABIES),
            CADE = sum(seedl_dens_CADE),
            PILA = sum(seedl_dens_PILA),
            PSME = sum(seedl_dens_PSME),
            PICO = sum(seedl_dens_PICO),
            PIPJ = sum(seedl_dens_YLWPINES))

d_spcomp$tot = rowSums(d_spcomp)

d_spcomp_core = d_spcomp |>
  mutate(across(everything(), ~. / tot)) |>
  mutate(across(everything(), ~round(.*100, 1))) |>
  mutate(type = "core")

# and seed wall plots
d_spcomp =  d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60) |>
  summarize(ABIES = sum(seedl_dens_ABIES),
            CADE = sum(seedl_dens_CADE),
            PILA = sum(seedl_dens_PILA),
            PSME = sum(seedl_dens_PSME),
            PICO = sum(seedl_dens_PICO),
            PIPJ = sum(seedl_dens_YLWPINES))

d_spcomp$tot = rowSums(d_spcomp)

d_spcomp_sw = d_spcomp |>
  mutate(across(everything(), ~. / tot)) |>
  mutate(across(everything(), ~round(.*100, 1))) |>
  mutate(type = "seedwall")

# combine and write
spcomp = bind_rows(d_spcomp_core, d_spcomp_sw)
write_csv(spcomp, file.path(datadir, "tables/regen_species_comp.csv"))


## write core and seed wall plots used for analysis to shapefile
library(sf)
d_spatial = left_join(d_sp, d |> select(plot_id, lat, lon))
d_spatial = st_as_sf(d_spatial, coords = c("lon","lat"), crs = 4326)

core_write = d_spatial |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed"))

sw_write =  d_spatial |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60)

st_write(core_write, file.path(datadir, "intermediate-inspection/core_allsp_v2.gpkg"), delete_dsn = TRUE)
st_write(sw_write, file.path(datadir, "intermediate-inspection/sw_allsp_v2.gpkg"), delete_dsn = TRUE)


#### Fit GAMs
d_sp = prep_d_sp("ALL")
d_mod_all_core = prep_d_core_mod(d_sp) |>
  mutate(seedl_dens_sp = round(seedl_dens_sp * 314),
         cone_dens_sp = cone_dens_sp * 314) |>
  mutate(species = "All conifers", type = "Interior")

m = gam(seedl_dens_sp ~ s(fire_intens2, k = 3) + s(ppt, k = 3) , data = d_mod_all_core, family = poisson, method = "REML")
m_nointens = gam(seedl_dens_sp ~ s(ppt, k = 3), data = d_mod_all_core, family = poisson, method = "REML")
all_core_intens_dev_exp = (100 *(m$null.deviance - m$deviance) / m$null.deviance) |> round(1)
all_core_nointens_dev_exp = (100 *(m_nointens$null.deviance - m_nointens$deviance) / m_nointens$null.deviance) |> round(1)
summary(m)
sum(influence(m))
#plot(m)
# Prep scenario plotting data
predictors = c("fire_intens2", "ppt")
scenario_preds_all = get_scenario_preds(m, d_mod_all_core, predictors, sp = "All conifers", percentile_exclude = percentile_exclude) |> mutate(type = "Interior")



## For PINES
# Prep data
d_sp = prep_d_sp("PINES")
d_mod_pines_core = prep_d_core_mod(d_sp) |>
  mutate(seedl_dens_sp = round(seedl_dens_sp * 314),
         cone_dens_sp = cone_dens_sp * 314) |>
  mutate(species = "Pines", type = "Interior")
# Plot raw data
# Fit GAM
m = gam(seedl_dens_sp ~ s(fire_intens2, k = 3) + s(ppt, k = 3) + s(prefire_prop_sp, k = 3), data = d_mod_pines_core, family = poisson, method = "REML")
m_nointens = gam(seedl_dens_sp ~ s(ppt, k = 3) + s(prefire_prop_sp, k = 3), data = d_mod_pines_core, family = poisson, method = "REML")
pines_core_intens_dev_exp = (100 *(m$null.deviance - m$deviance) / m$null.deviance) |> round(1)
pines_core_nointens_dev_exp = (100 *(m_nointens$null.deviance - m_nointens$deviance) / m_nointens$null.deviance) |> round(1)
summary(m)
sum(influence(m))
#plot(m)
# Prep scenario plotting data
predictors = c("fire_intens2", "ppt", "cone_dens_sp", "prefire_prop_sp")
scenario_preds_pines = get_scenario_preds(m, d_mod_pines_core, predictors, sp = "Pines", percentile_exclude = percentile_exclude) |> mutate(type = "Interior")



## For ALL SPECIES seedwall GAM fits 
d_sp = prep_d_sp("ALL")
d_mod_all_sw = prep_d_sw_mod(d_sp, max_sw_dist = 30) |>
  mutate(seedl_dens_sp = round(seedl_dens_sp * 314),
         cone_dens_sp = cone_dens_sp * 314) |>
  mutate(species = "All conifers", type = "Edge")
m = gam(seedl_dens_sp ~ + s(ppt, k = 3) + s(fire_intens2, k = 3), data = d_mod_all_sw, family = poisson)
m_nointens = gam(seedl_dens_sp ~ + s(ppt, k = 3), data = d_mod_all_sw, family = poisson)
all_sw_intens_dev_exp = (100 *(m$null.deviance - m$deviance) / m$null.deviance) |> round(1)
all_sw_nointens_dev_exp = (100 *(m_nointens$null.deviance - m_nointens$deviance) / m_nointens$null.deviance) |> round(1)
summary(m)
sum(influence(m))
# Prep scenario plotting data
predictors = c("fire_intens2", "ppt")
scenario_preds_all_sw = get_scenario_preds(m, d_mod_all_sw, predictors, sp = "All conifers", percentile_exclude = percentile_exclude) |> mutate(type = "Edge")


## For PINES seedwall GAM fits 
d_sp = prep_d_sp("PINES")
d_mod_pines_sw = prep_d_sw_mod(d_sp, max_sw_dist = 30) |>
  mutate(seedl_dens_sp = round(seedl_dens_sp * 314),
         cone_dens_sp = cone_dens_sp * 314) |>
  mutate(species = "Pines", type = "Edge")
m = gam(seedl_dens_sp ~ s(ppt, k = 3) + s(fire_intens2, k = 3), data = d_mod_pines_sw, family = poisson)
m_nointens = gam(seedl_dens_sp ~ s(ppt, k = 3), data = d_mod_pines_sw, family = poisson)
pines_sw_intens_dev_exp = (100 *(m$null.deviance - m$deviance) / m$null.deviance) |> round(1)
pines_sw_nointens_dev_exp = (100 *(m_nointens$null.deviance - m_nointens$deviance) / m_nointens$null.deviance) |> round(1)
summary(m)
sum(influence(m))
# Prep scenario plotting data
predictors = c("fire_intens2", "ppt", "cone_dens_sp", "prefire_prop_sp", "grn_vol_sp")
scenario_preds_pines_sw = get_scenario_preds(m, d_mod_pines_sw, predictors, sp = "Pines", percentile_exclude = percentile_exclude) |> mutate(type = "Edge")



# Combine the scenario plots for each species group together so we can plot the fits of multiple species in one panel
scenario_preds = bind_rows(scenario_preds_all, scenario_preds_pines, scenario_preds_all_sw, scenario_preds_pines_sw)
d_mods = bind_rows(d_mod_all_core,
                  d_mod_pines_core,
                  d_mod_all_sw,
                  d_mod_pines_sw)


# Make scenario plots
ymin = 0.05
ymax = 25

p1 = make_scenario_ggplot(scenario_preds, d_mods, "fire_intens2", "Torching extent (%)", ymin = ymin, ymax = ymax)
p2 = make_scenario_ggplot(scenario_preds, d_mods, "ppt", "Mean annual precipitation (mm)", ymin = ymin, ymax = ymax)

p = ggarrange(p1, p2 + rremove("ylab") + rremove("y.text"), common.legend = TRUE, widths = c(1.2,1))

png(file.path(datadir, "figures/main_model_fits.png"), res = 350, width = 2000, height = 1100)
p
dev.off()


#### Make a table of deviance explained 

dev = data.frame(species = c("All conifers - core", "Pines - core", "All conifers - SW", "Pines - SW"),
                 dev_expl_intens = c(all_core_intens_dev_exp, pines_core_intens_dev_exp, all_sw_intens_dev_exp, pines_sw_intens_dev_exp),
                 dev_expl_nointens = c(all_core_nointens_dev_exp, pines_core_nointens_dev_exp, all_sw_nointens_dev_exp, pines_sw_nointens_dev_exp)) |>
  mutate(difference = dev_expl_intens - dev_expl_nointens)
dev



#### Make a density~torching (color: ppt) figure with the data points, and fit lines for two ppt scenarios
d_sp = prep_d_sp("ALL")
d_mod_all_core = prep_d_core_mod(d_sp) |>
  mutate(seedl_dens_sp = round(seedl_dens_sp * 314),
         cone_dens_sp = cone_dens_sp * 314) |>
  mutate(species = "All conifers", type = "Interior")

m = gam(seedl_dens_sp ~ s(fire_intens2, k = 3) + s(ppt, k = 3) , data = d_mod_all_core, family = poisson, method = "REML")
ppt_split = 1200
scenario_preds = get_scenario_preds(m, d_mod_all_core, "fire_intens2", sp = "All conifers", percentile_exclude = percentile_exclude, interacting_predictor = "ppt", interacting_splits = ppt_split) |> mutate(type = "Interior")
p1 = make_scenario_w_ppt_ggplot(scenario_preds, d_mod_all_core, "fire_intens2", "Torching extent (%)", ymin = NULL, ymax = NULL, interacting_splits = ppt_split, show_data = TRUE)

png(file.path(datadir, "figures/fits_w_data.png"), res = 500, width = 2200, height = 2000)
p1
dev.off()



### Plot relationship between cone density and seedling density by species, and by scorched vs torched

d_ylwpines = prep_d_sp("YLWPINES") |> prep_d_core_mod() |> mutate(species = "Yellow pine")
#d_psme = prep_d_sp("PSME") |> prep_d_core_mod() |> mutate(species = "Douglas-fir")
#d_pila = prep_d_sp("PILA") |> prep_d_core_mod() |> mutate(species = "Sugar pine")

d_sps = bind_rows(d_ylwpines)

# We can use d_sps for modeling. Next we will modify it to cause some data loss by turning 0s to nonzero so we can plot on log scale



# get median cone density by sp
medians = d_sps |>
  group_by(species) |>
  summarize(median_dens = median(cone_dens_sp))

# bind this to the data so we can compute whether each obs was above or below the median
d_sps = left_join(d_sps,medians)


d_fig = d_sps |>
  # Put counts back on per sq m scale
  mutate(seedl_dens_sp = ifelse(seedl_dens_sp < 0.5/314, 0.5/314, seedl_dens_sp)) |>
  mutate(cone_dens_sp = ifelse(cone_dens_sp < 0.5/314, 0.5/314, cone_dens_sp)) |>
  mutate(cone_dens_sp_cat = ifelse(cone_dens_sp >= median_dens, "High", "Low")) |>
  mutate(cone_dens_sp_cat = factor(cone_dens_sp_cat, levels = c("Low","High"))) |>
  mutate(under_cones_new_sp = ifelse(under_cones_new_sp == "low", "Low", "High")) |>
  mutate(under_cones_new_sp = factor(under_cones_new_sp, levels = c("Low","High")))

color_breaks = c(0.001, 0.01, 0.1, 1, 10, 100)

p1 = ggplot(d_fig, aes(x = cone_dens_sp_cat, y = seedl_dens_sp, color = cone_dens_sp)) +
  geom_jitter(height = 0, width = 0.15) +
  scale_color_viridis_c(trans = "log", name = bquote(Cones~m^-2), breaks = color_breaks, labels = color_breaks, limits = c(0.001,5), oob = squish) +
  coord_trans(y = "log") +
  geom_boxplot(data = d_fig, coef = 0, outlier.shape = NA, fill = NA, width = 0.4) +
  scale_y_continuous(breaks = c(.001,.01,.1,1,10,100, 1000), minor_breaks = c(0.0005,0.005, 0.05, 0.5, 5.0, 50, 500), labels = label_comma()) +
  labs(x = "Plot cone density", y = bquote(Yellow~pine~seedlings~m^-2)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())
p1

p2 = ggplot(d_fig, aes(x = under_cones_new_sp, y = seedl_dens_sp)) +
  geom_jitter(height = 0, width = 0.15, color = "gray60") +
  coord_trans(y = "log") +
  geom_boxplot(data = d_fig, coef = 0, outlier.shape = NA, fill = NA, width = 0.4) +
  scale_y_continuous(breaks = c(.001,.01,.1,1,10,100, 1000), minor_breaks = c(0.0005,0.005, 0.05, 0.5, 5.0, 50, 500), labels = label_comma()) +
  labs(x = "Single-tree cone density", y = bquote(Yellow~pine~seedlings~m^-2)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

p = ggarrange(p2, p1 + rremove("ylab") + rremove("y.text"), widths = c(1,1.25))

png(file.path(datadir, "figures/cone_dens_boxplots.png"), res = 500, width = 2600, height = 1600)
p
dev.off()






# 
# 
# 
# 
# 
# 
# m1 = gam(seedl_dens_sp ~ cone_dens_sp_log + s(fire_intens2, k = 3), data = d_mod_core, family = poisson())
# 
# summary(m1)
# 
# d_fig = d_mod_core |>
#   # Put counts back on per sq m scale
#   mutate(seedl_dens_sp = ifelse(seedl_dens_sp < 0.5, 0.5, seedl_dens_sp) / 314) |>
#   mutate(cone_dens_sp = ifelse(cone_dens_sp < 0.5/314, 0.5/314, cone_dens_sp))
# 
# 
# 
# ggplot(d_fig, aes(x = cone_dens_sp, y = seedl_dens_sp, color = fire_intens2 > 78.75)) +
#   geom_point() +
#   scale_y_continuous(breaks = c(.001,.01,.1,1,10,100, 1000), minor_breaks = c(0.0005,0.005, 0.05, 0.5, 5.0, 50, 500), labels = label_comma()) +
#   scale_x_continuous(breaks = c(.001,.01,.1,1,10,100, 1000), minor_breaks = c(0.0005,0.005, 0.05, 0.5, 5.0, 50, 500), labels = label_comma()) +
#   coord_trans(y = "log", x = "log")
# 
# ggplot(d_fig, aes(x = under_cones_new_sp, y = seedl_dens_sp, color = fire_intens2 > 78)) +
#   geom_boxplot() +
#   geom_point() +
#   coord_trans(y = "log", x = "log")
# 
# ggplot(d_fig, aes(x = cone_dens_sp, y = fire_intens2)) +
#   geom_point() +
#   coord_trans(x = "log")
# 
# ggplot(d_fig, aes(x = under_cones_new_sp, y = fire_intens2)) +
#   geom_point() +
#   geom_boxplot() +
#   coord_trans(x = "log")
# 
# 
# 
# ## for 3 species we need the continuous seedl~cones plot
# 
# 
# intens_split = 78.75
# predictors = c("cone_dens_sp_log")
# scenario_preds = get_scenario_preds(m1, d_mod_core, predictors, sp = "YLWPINES", percentile_exclude = percentile_exclude, interacting_predictor = "fire_intens2", interacting_splits = intens_split) |> mutate(type = "Core")
# 
# 
# 
# p1 = make_scenario_w_intens_ggplot(scenario_preds, d_mod_core, "cone_dens_sp", "Cones / sq m", ymin = NULL, ymax = NULL, interacting_splits = intens_split, show_data = TRUE)
# p1
# 
# 
# 
# ## test with ggplot
# 
# d_test = d_mod_core |>
#   mutate(seedl_dens_sp = ifelse(seedl_dens_sp  == 0, 1, seedl_dens_sp))
# 
# ggplot(d_test, aes(x = cone_dens_sp_log, y = seedl_dens_sp)) +
#   geom_point() +
#   coord_trans(y = "log") +
#   geom_smooth(method = mgcv::gam, family = poisson, method.args=list(family="poisson"))
# 
# 
# 
# ## For pines we need the boxplot

