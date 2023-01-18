library(tidyverse)
library(here)


# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))



## By species (YELLOWPINES, PINES, ABCO, PSME, CADE)
# Take all plots with no green within 50m
# Plot seedling density vs burn date, color points by nearby cone density

## Compare with seed wall seedling density

d = read_csv(datadir("field-data/processed/plot_seedl_cone_grnseedsource_comp.csv"))

# Thin to Caldor and Dixie only
unique(d$fire)

d = d |>
  filter(fire %in% c("Caldor", "Dixie"))

## PINES
d_nogrn_sp = d |>
  filter(PINES_green_vol_abs == 0,
         (prefire_prop_pipj + prefire_prop_pila + prefire_prop_pico + prefire_prop_pimo) > 20,
         plot_type %in% c("core","delayed"))

## alternate way to get plots far from seed sources
d_nogrn_sp = d |>
  filter(PINES_green_vol_abs == 0,
         ((is.na(dist_grn_PINES) | dist_grn_PINES > 100) & as.numeric(sight_line) > 75),
         (prefire_prop_pipj + prefire_prop_pila + prefire_prop_pico + prefire_prop_pimo) > 20,
         plot_type %in% c("core","delayed"))


d_sw_sp = d|>
  filter(plot_type == "seedwall",
         PINES_green_vol > 20)

ggplot(d_nogrn_sp, aes(x = day_of_burning, y = ifelse(seedl_dens_PINES > 2, 2, seedl_dens_PINES))) +
  geom_jitter(data = d_sw_sp, color="green", size=3, width=2) +
  geom_jitter(size=3, width=2) +
  facet_grid(~fire) +
  #scale_color_viridis_c(limits=c(0,.5)) +
  theme_bw(15) +
  labs(x = "Day of Burning", y = "Seedlings / sq m")

## ABCO
d_nogrn_sp = d |>
  filter(ABCO_green_vol_abs == 0,
         prefire_prop_abco > 10,
         plot_type %in% c("core","delayed"))

d_nogrn_sp = d |>
  filter(ABCO_green_vol_abs == 0,
         ((is.na(dist_grn_ABCO) | dist_grn_ABCO > 100) & as.numeric(sight_line) > 75),
         prefire_prop_abco > 10,
         plot_type %in% c("core","delayed"))

d_sw_sp = d|>
  filter(plot_type == "seedwall",
         ABCO_green_vol > 10)

ggplot(d_nogrn_sp, aes(x = day_of_burning, y = ifelse(seedl_dens_ABCO > 20, 20, seedl_dens_ABCO))) +
  geom_jitter(data = d_sw_sp, color="green", size=3, width=2) +
  geom_jitter(size=3, width=2) +
  facet_grid(~fire) +
  #scale_color_viridis_c(limits=c(0,.5)) +
  theme_bw(15) +
  labs(x = "Day of Burning", y = "Seedlings / sq m")

## PSME
d_nogrn_sp = d |>
  filter(PSME_green_vol_abs == 0,
         prefire_prop_psme > 10,
         plot_type %in% c("core","delayed"))

d_sw_sp = d|>
  filter(plot_type == "seedwall",
         PSME_green_vol > 5)

ggplot(d_nogrn_sp, aes(x = day_of_burning, y = ifelse(seedl_dens_ABCO > 5, 5, seedl_dens_ABCO))) +
  geom_jitter(data = d_sw_sp, color="green", size=3, width=2) +
  geom_jitter(size=3, width=2) +
  facet_grid(~fire) +
  #scale_color_viridis_c(limits=c(0,.5)) +
  theme_bw(15) +
  labs(x = "Day of Burning", y = "Seedlings / sq m")

## CADE
d_nogrn_sp = d |>
  filter(CADE_green_vol_abs == 0,
         prefire_prop_cade > 10,
         plot_type %in% c("core","delayed"))

d_sw_sp = d|>
  filter(plot_type == "seedwall",
         CADE_green_vol > 10)

ggplot(d_nogrn_sp, aes(x = day_of_burning, y = ifelse(seedl_dens_ABCO > 20, 20, seedl_dens_ABCO))) +
  geom_jitter(data = d_sw_sp, color="green", size=3, width=2) +
  geom_jitter(size=3, width=2) +
  facet_grid(~fire) +
  #scale_color_viridis_c(limits=c(0,.5)) +
  theme_bw(15) +
  labs(x = "Day of Burning", y = "Seedlings / sq m")


### In core area: seedling density relative to cone density
## PINES
d_nogrn_sp = d |>
  filter(PINES_green_vol_abs == 0,
         ((is.na(dist_grn_PINES) | dist_grn_PINES > 100) & as.numeric(sight_line) > 75),
         (prefire_prop_pipj + prefire_prop_pila + prefire_prop_pico + prefire_prop_pimo) > 20,
         plot_type %in% c("core","delayed"))

ggplot(d_nogrn_sp, aes(x=sqrt(cone_dens_PINES), y = sqrt(seedl_dens_PINES))) +
  geom_point(size = 2) +
  theme_bw(15) +
  labs(x = "Cones / sq m", y = "Seedlings / sq m")

ggplot(d_nogrn_sp, aes(x=sqrt(under_cones_new_PINES), y = sqrt(seedl_dens_PINES))) +
  geom_point() +
  theme_bw(3)

## YLWPINES
d_nogrn_sp = d |>
  filter(YLWPINES_green_vol_abs == 0,
         (prefire_prop_pipj) > 20,
         plot_type %in% c("core","delayed"))

ggplot(d_nogrn_sp, aes(x=sqrt(cone_dens_YLWPINES), y = sqrt(seedl_dens_YLWPINES))) +
  geom_point()

ggplot(d_nogrn_sp, aes(x=sqrt(under_cones_new_YLWPINES), y = sqrt(seedl_dens_YLWPINES))) +
  geom_point()

m = lm(seedl_dens_YLWPINES~cone_dens_YLWPINES, d = d_nogrn_sp)
summary(m)

######## In core area: seedling density relative to overstory torch
## PINES
d_nogrn_sp = d |>
  filter(PINES_green_vol_abs == 0,
         ((is.na(dist_grn_PINES) | dist_grn_PINES > 100) & as.numeric(sight_line) > 75), # this makes it a little worse here
         (prefire_prop_pipj + prefire_prop_pila + prefire_prop_pico + prefire_prop_pimo) > 20,
         plot_type %in% c("core","delayed"))

ggplot(d_nogrn_sp, aes(x = PINES_untorched_vol_abs*100, y = seedl_dens_PINES)) +
  geom_point(size = 2) +
  theme_bw(16) +
  labs(x = "Scorched pine canopy (%)", y = "Pine seedlings / sq m")

ggplot(d_nogrn_sp, aes(x = vol_brn_50m, y = seedl_dens_PINES)) +
  geom_point()

m = lm(seedl_dens_PINES ~ vol_brn_50m, d = d_nogrn_sp)
summary(m)


## YLWPINES
d_nogrn_sp = d |>
  filter(YLWPINES_green_vol_abs == 0,
         ((is.na(dist_grn_YLWPINES) | dist_grn_YLWPINES > 100) & as.numeric(sight_line) > 75), # this makes it a little worse here
         (prefire_prop_pipj) > 20,
         plot_type %in% c("core","delayed"))

ggplot(d_nogrn_sp, aes(x = YLWPINES_untorched_vol_abs, y = seedl_dens_YLWPINES)) +
  geom_point()
ggplot(d_nogrn_sp, aes(x = vol_brn_50m, y = seedl_dens_YLWPINES)) +
  geom_point()

m = lm(seedl_dens_YLWPINES~vol_brn_50m, d = d_nogrn_sp)
summary(m)

## ABCO
d_nogrn_sp = d |>
  filter(ABCO_green_vol_abs == 0,
         ((is.na(dist_grn_ABCO) | dist_grn_ABCO > 100) & as.numeric(sight_line) > 75),
         plot_type %in% c("core","delayed"))

ggplot(d_nogrn_sp, aes(x = ABCO_untorched_vol_abs, y = seedl_dens_ABCO)) +
  geom_point()

ggplot(d_nogrn_sp, aes(x = vol_brn_50m, y = seedl_dens_ABCO)) +
  geom_point()

m = lm(seedl_dens_ABCO~ABCO_untorched_vol_abs, d = d_nogrn_sp)
summary(m)



### GAM with multiple predictors

library(mgcv)


### PINES

d_nogrn_sp = d |>
  filter(PINES_green_vol_abs == 0,
         ((is.na(dist_grn_PINES) | dist_grn_PINES > 100) & as.numeric(sight_line) > 75), # this makes it a little worse here
         (prefire_prop_pipj + prefire_prop_pila + prefire_prop_pico + prefire_prop_pimo) > 20,
         plot_type %in% c("core","delayed"))

m = gam(seedl_dens_PINES ~ day_of_burning + PINES_untorched_vol_abs + cone_dens_PINES, data = d_nogrn_sp)
summary(m)

m = gam(seedl_dens_PINES ~ s(day_of_burning, k=3) + s(vol_brn_50m, k=3) + s(cone_dens_PINES, k=3), data = d_nogrn_sp)
summary(m)

m = gam(seedl_dens_PINES ~ te(day_of_burning, PINES_untorched_vol_abs, k=3) + s(cone_dens_PINES, k=3), data = d_nogrn_sp)
summary(m)

plot(m, pers=TRUE, too.far=1)




### YLWPINES

d_nogrn_sp = d |>
  filter(YLWPINES_green_vol_abs == 0,
         ((is.na(dist_grn_YLWPINES) | dist_grn_YLWPINES > 100) & as.numeric(sight_line) > 75), # this makes it a little worse here
         (prefire_prop_pipj) > 20,
         plot_type %in% c("core","delayed"))

m = gam(seedl_dens_YLWPINES ~ day_of_burning + YLWPINES_untorched_vol_abs + cone_dens_YLWPINES, data = d_nogrn_sp)
summary(m)

m = gam(seedl_dens_YLWPINES ~ s(day_of_burning, k=3) + s(vol_brn_50m, k=3) + s(cone_dens_YLWPINES, k=3), data = d_nogrn_sp)
summary(m)

m = gam(seedl_dens_YLWPINES ~ te(day_of_burning, YLWPINES_untorched_vol_abs, k=3) + s(cone_dens_YLWPINES, k=3), data = d_nogrn_sp)
summary(m)

# poisson
m = gam(round(seedl_dens_YLWPINES) ~ te(day_of_burning, YLWPINES_untorched_vol_abs, k=3) + s(cone_dens_YLWPINES, k=3), data = d_nogrn_sp, family=poisson())
summary(m)

plot(m, pers=TRUE, too.far=1)



## Make prediction plots for the DOB-untorched vol model

untorched_low = data.frame(YLWPINES_untorched_vol_abs = 0.1,
                           day_of_burning = 200:255,
                           cone_dens_YLWPINES = 0.4,
                           scorch = "low")
untorched_med = data.frame(YLWPINES_untorched_vol_abs = 0.4,
                           day_of_burning = 200:255,
                           cone_dens_YLWPINES = 0.4,
                           scorch = "mod")
untorched_high = data.frame(YLWPINES_untorched_vol_abs = 0.7,
                           day_of_burning = 200:255,
                           cone_dens_YLWPINES = 0.4,
                           scorch = "high")

newdat = bind_rows(untorched_low,
                   untorched_med,
                   untorched_high)

newdat$scorch = factor(newdat$scorch, levels = c("high", "mod","low"))

preds = predict(m,newdat, type = "response")

newdat$fit = preds

ggplot(newdat, aes(x = day_of_burning, y = fit, color = scorch)) +
  geom_line(size=2) +
  theme_bw(15) +
  scale_color_viridis_d(end=0.8, option = "magma", direction = -1) +
  labs(x = "Day of burning", y = "Seedlings / sq m")






### ABCO

d_nogrn_sp = d |>
  filter(ABCO_green_vol_abs == 0,
         ((is.na(dist_grn_YLWPINES) | dist_grn_YLWPINES > 100) & as.numeric(sight_line) > 75), # this makes it a little worse here
         (prefire_prop_abco) > 10,
         plot_type %in% c("core","delayed"))

m = gam(seedl_dens_ABCO ~ day_of_burning + ABCO_untorched_vol_abs, data = d_nogrn_sp)
summary(m)

m = gam(seedl_dens_ABCO ~ s(day_of_burning, k=3) + s(ABCO_untorched_vol_abs, k=3), data = d_nogrn_sp)
summary(m)

m = gam(round(seedl_dens_ABCO) ~ te(day_of_burning, ABCO_untorched_vol_abs, k = c(3,3)), data = d_nogrn_sp, family = poisson())
summary(m)

plot(m, pers=TRUE, too.far=0.25)





## Make prediction plots for the DOB-untorched vol model

untorched_low = data.frame(ABCO_untorched_vol_abs = 0.1,
                           day_of_burning = 200:255,
                           cone_dens_ABCO = 0.4,
                           scorch = "low")
untorched_med = data.frame(ABCO_untorched_vol_abs = 0.4,
                           day_of_burning = 200:255,
                           cone_dens_ABCO = 0.4,
                           scorch = "mod")
untorched_high = data.frame(ABCO_untorched_vol_abs = 0.7,
                            day_of_burning = 200:255,
                            cone_dens_ABCO = 0.4,
                            scorch = "high")

newdat = bind_rows(untorched_low,
                   untorched_med,
                   untorched_high)

newdat$scorch = factor(newdat$scorch, levels = c("high", "mod","low"))

preds = predict(m,newdat, type = "response")

newdat$fit = preds

ggplot(newdat, aes(x = day_of_burning, y = fit, color = scorch)) +
  geom_line(size=2) +
  theme_bw(15) +
  scale_color_viridis_d(end=0.8, option = "magma", direction = -1) +
  labs(x = "Day of burning", y = "Seedlings / sq m")
