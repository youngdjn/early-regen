library(tidyverse)
library(mgcv)
library(here)
library(scales)

source("scripts/analysis/year1-dixie-caldor_functions.R")

# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)


# Load data
d = read_csv(file.path(datadir,"field-data/processed/plot-data-prepped.csv"))

# Keep Caldor and Dixie 2022 only
d = d |>
  filter(fire %in% c("Caldor", "Dixie"))

# Check distrib of seedling density
hist(d$seedl_dens_ALL, breaks=200)

# Calc fire intensity (scorch vs torch) indices
d = d |>
  mutate(fire_intens =  100 - pmax(litter_cover,vol_brn_50m),
         fire_intens2 = 200 - (litter_cover + vol_brn_50m)) |>
  # these should go into data prep script:
  mutate(ba = ba_factor * ba_tally) |>
  mutate(capable_growing_area = 100 - nongrowing_cover)


### Environmental context across all plots
## Precip by date burned

ggplot(d, aes(x=day_of_burning, y = ppt, color = fire)) +
  geom_point()


#### Constants

## Define the date windows over which to summarize raw data (by day of year)
windows = data.frame(fire = c("Caldor","Caldor", "Dixie", "Dixie", "Dixie"),
                     start = c(229, 239, 203, 225, 252),
                     end =   c(231, 242, 205, 230, 252))


#### Analysis

## Prep for ALL SPECIES
# Prep data
d_sp = prep_d_sp("ALL")
d_mod = prep_d_core_mod(d_sp) |>
  mutate(seedl_dens_sp = round(seedl_dens_sp * 314),
         cone_dens_sp = cone_dens_sp * 314)
# Plot raw data
plot_raw_data(d_sp)
# Fit GAM
m = gam(seedl_dens_sp ~ s(fire_intens2, k = 3) + s(ppt, k = 3) + s(capable_growing_area, k = 3), data = d_mod, family = poisson, method = "REML")
all_intens_dev_exp = (100 *(m$null.deviance - m$deviance) / m$null.deviance) |> round(1)
m_nointens = gam(seedl_dens_sp ~ s(ppt, k = 3) + s(capable_growing_area, k = 3), data = d_mod, family = poisson, method = "REML")
all_nointens_dev_exp = (100 *(m_nointens$null.deviance - m_nointens$deviance) / m_nointens$null.deviance) |> round(1)
summary(m)
sum(influence(m))
#plot(m)
# Prep scenario plotting data
predictors = c("fire_intens2", "ppt", "capable_growing_area")
scenario_preds_all = get_scenario_preds(m, d_mod, predictors, sp = "All conifers")

## Prep for PINES
# Prep data
d_sp = prep_d_sp("PINES")
d_mod = prep_d_core_mod(d_sp) |>
  mutate(seedl_dens_sp = round(seedl_dens_sp * 314),
         cone_dens_sp = cone_dens_sp * 314)
# Plot raw data
plot_raw_data(d_sp)
# Fit GAM
m = gam(seedl_dens_sp ~ s(fire_intens2, k = 3) + s(ppt, k = 3) + s(capable_growing_area, k = 3) + s(cone_dens_sp, k = 3), data = d_mod, family = poisson, method = "REML")
pines_intens_dev_exp = (100 *(m$null.deviance - m$deviance) / m$null.deviance) |> round(1)
m_nointens = gam(seedl_dens_sp ~ s(ppt, k = 3) + s(capable_growing_area, k = 3) + s(cone_dens_sp, k = 3), data = d_mod, family = poisson, method = "REML")
pines_nointens_dev_exp = (100 *(m_nointens$null.deviance - m_nointens$deviance) / m_nointens$null.deviance) |> round(1)
summary(m)
sum(influence(m))
#plot(m)
# Prep scenario plotting data
predictors = c("fire_intens2", "ppt", "capable_growing_area", "cone_dens_sp")
scenario_preds_pines = get_scenario_preds(m, d_mod, predictors, sp = "Pines")

## Prep for ABIES
# Prep data
d_sp = prep_d_sp("ABIES")
d_mod = prep_d_core_mod(d_sp) |>
  mutate(seedl_dens_sp = round(seedl_dens_sp * 314),
         cone_dens_sp = cone_dens_sp * 314)
# Plot raw data
plot_raw_data(d_sp)
# Fit GAM
m = gam(seedl_dens_sp ~ s(fire_intens2, k = 3) + s(ppt, k = 3) + s(capable_growing_area, k = 3), data = d_mod, family = poisson, method = "REML")
firs_intens_dev_exp = (100 *(m$null.deviance - m$deviance) / m$null.deviance) |> round(1)
m_nointens = gam(seedl_dens_sp ~ s(ppt, k = 3) + s(capable_growing_area, k = 3), data = d_mod, family = poisson, method = "REML")
firs_nointens_dev_exp = (100 *(m_nointens$null.deviance - m_nointens$deviance) / m_nointens$null.deviance) |> round(1)
summary(m)
sum(influence(m))
#plot(m)
# Prep scenario plotting data
predictors = c("fire_intens2", "ppt", "capable_growing_area")
scenario_preds_firs = get_scenario_preds(m, d_mod, predictors, sp = "Firs")



# Combine the scenario plots for each species group together so we can plot the fits of multiple species in one panel
scenario_preds = bind_rows(scenario_preds_all, scenario_preds_pines, scenario_preds_firs)




# Make scenario plots
p1 = make_scenario_ggplot(scenario_preds, "fire_intens2", ymin = 0.04, ymax = 8)
p2 = make_scenario_ggplot(scenario_preds, "ppt", ymin = 0.04, ymax = 8)
p3 = make_scenario_ggplot(scenario_preds, "capable_growing_area", ymin = 0.04, ymax = 8)
p4 = make_scenario_ggplot(scenario_preds, "cone_dens_sp", ymin = 0.04, ymax = 8)

library(ggpubr)

ggarrange(p1, 
          ggarrange(p2, p3 + rremove("ylab") + rremove("y.text"), p4 + rremove("ylab") + rremove("y.text"), nrow=1, ncol = 3, legend = "none", widths = c(1.2,1,1)),
          nrow = 2, ncol = 1, heights = c(3,1),
          common.legend = TRUE)


ggarrange(p1,p2,p3,p4, common.legend = TRUE)

## TODO: truncate lines to range of data



#### Make a table of deviance explained 

dev = data.frame(species = c("All conifers", "Pines", "Firs"),
                 dev_expl_intens = c(all_intens_dev_exp, pines_intens_dev_exp, firs_intens_dev_exp),
                 dev_expl_nointens = c(all_nointens_dev_exp, pines_nointens_dev_exp, firs_nointens_dev_exp)) |>
  mutate(difference = dev_expl_intens - dev_expl_nointens)


# All conifers with intensity and without, same for other sp















#### Does litter affect seedling dens in seed wall plots?
d_sp = prep_d_sp("ALL")
d_mod = prep_d_sw_mod(d_sp, max_sw_dist = 30)
hist(d_mod$seedl_dens_sp, breaks = 200)
m = gam(round(seedl_dens_sp*314) ~ + s(ppt, k = 3) + s(dist_grn_sp, k = 3) , data = d_mod, family = poisson)
summary(m)
plot(m)

library(mgcViz)
m = getViz(m)
print(plot(m, allTerms = TRUE), pages = 1)


hist(d_mod$dist_grn_sp)

##### MAKE SURE SEED WALL PLOTS ARE CLOSE TO GREEN. RECALL WHICH DATA FIELD THAT WOULD BE FROM THE DATASHEET AND DF