# TODO
# FOR STAT MODELS
# yes/no for replacement density: 70 seedl / acre is   70 / 4046.86 = 0.0173 seedl / sq m. Maybe use 5 times that: 0.0865 seedl/sq m (and make sure insensitive to that assumption)
# cap at density of 0.5 seedl / sq m (2023 seedl / acre)
# 10 m radius plot is 314 sq m

# Special look at the > 10 seedl dens core area plots (not seedwall) to see what defines them.

# FOR SUMMARY STATS
# break core area into scorched and torched

# Look at stat correlations specifically within the plots of day 220+ or 220-245


library(tidyverse)
library(here)


# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))



## By species (YELLOWPINES, PINES, ABCO, PSME, CADE)
# Take all plots with no green within 100m
# Plot seedling density vs burn date, color points by nearby cone density

## Compare with seed wall seedling density

d = read_csv(datadir("field-data/processed/plot-data-prepped.csv"))

# Thin to Caldor and Dixie 2022 only
unique(d$fire)

d = d |>
  filter(fire %in% c("Caldor", "Dixie"))

hist(d$seedl_dens_ALL, breaks=200)

# # Exclude Dixie plots with precip > 1375
# d = d |>
#   filter(fire == "Caldor" | ppt < 1375)

## Assign fire intensity indices
d = d |>
  mutate(fire_intens =  100 - pmax(litter_cover,vol_brn_50m),
         fire_intens2 = 100 - (litter_cover + vol_brn_50m))





#### Explore data ranges ####


## What is mean seed wall seedling density?

d_sw = d |>
  filter(plot_type == "seedwall")

summary(d_sw$seedl_dens_ALL)


## Precip by date burned

ggplot(d, aes(x=day_of_burning, y = ppt, color = fire)) +
  geom_point()












## ALL CONIFERS

d_nogrn_sp = d |>
  filter(ALL_green_vol_abs == 0,
         ((is.na(dist_grn_ALL) | dist_grn_ALL > 100) & as.numeric(sight_line) > 100),
         plot_type %in% c("core","delayed"))


# What's the distrib of the fire intensity vals?
hist(d_nogrn_sp$fire_intens2)
hist(d_nogrn_sp$fire_intens)



# mean seedl dens
summary(d_nogrn_sp$seedl_dens_ALL)
hist(d_nogrn_sp$seedl_dens_ALL, breaks =seq(0,50,by=1))
lowdens = d_nogrn_sp$seedl_dens_ALL[d_nogrn_sp$seedl_dens_ALL < 1]
hist(lowdens, breaks = 100)
middens = d_nogrn_sp$seedl_dens_ALL[d_nogrn_sp$seedl_dens_ALL < 10]
hist(middens, breaks=100)




d_sw_sp = d|>
  filter(plot_type == "seedwall")

d_nogrn_fig = d_nogrn_sp |>
  mutate(seedl_dens_ALL = ifelse(seedl_dens_ALL < 0.001592357, 0.001592357, seedl_dens_ALL)) |>
  mutate(fire_intens2_trunc = ifelse(fire_intens2 < -25, -25, fire_intens2)) |>
  mutate(fire_intens2_cat = ifelse(fire_intens2 < median(fire_intens2), "scorched", "torched"))

### Get means for specific DOB windows
## Define the windows
windows = data.frame(fire = c("Caldor","Caldor", "Dixie", "Dixie", "Dixie"),
                     start = c(229, 239, 203, 225, 252),
                     end =   c(231, 242, 205, 230, 252),
                     seedwall_median = NA,
                     core_blk_median = NA,
                     core_brn_median = NA)

for(i in 1:nrow(windows)) {
  
  window = windows[i,]
  
  core_blk_foc = d_nogrn_fig |>
    filter(fire == window$fire) |>
    filter(between(day_of_burning, window$start, window$end)) |>
    filter(fire_intens2_cat == "torched")
  core_blk_median = median(core_blk_foc$seedl_dens_ALL)
  
  core_brn_foc = d_nogrn_fig |>
    filter(fire == window$fire) |>
    filter(between(day_of_burning, window$start, window$end)) |>
    filter(fire_intens2_cat == "scorched")
  core_brn_median = median(core_brn_foc$seedl_dens_ALL)
  
  sw_foc = d_sw_sp |>
    filter(between(day_of_burning, window$start, window$end))
  sw_median = median(sw_foc$seedl_dens_ALL)
  
  windows[i,"seedwall_median"] = sw_median
  windows[i,"core_blk_median"] = core_blk_median
  windows[i,"core_brn_median"] = core_brn_median
  
}


ggplot(d_nogrn_fig, aes(x = day_of_burning, y = seedl_dens_ALL)) +
  geom_hline(yintercept = 0.0173, linetype = "dashed", color="gray70") +
  geom_jitter(data = d_sw_sp, color="#A2D435", size=3, width=2, aes(shape="")) +
  geom_jitter(size=3, width=2, aes(color = fire_intens2_cat)) +
  labs(shape = "Seed wall") +
  scale_color_manual(values = c(torched = "black", scorched = "#9D5B0B"), name = "Core area") +
  facet_grid(~fire) +
  theme_bw(15) +
  labs(x = "Day of Burning", y = "Seedlings / sq m") +
  scale_y_continuous(breaks = c(.01,.1,1,10,100)) +
  coord_trans(y = "log") +
  geom_segment(data = windows,aes(x = start-2, xend = end+2, y = seedwall_median, yend = seedwall_median), linewidth = 1.5, color = "white") +
  geom_segment(data = windows,aes(x = start-2, xend = end+2, y = core_blk_median, yend = core_blk_median), linewidth = 1.5, color = "white") +
  geom_segment(data = windows,aes(x = start-2, xend = end+2, y = core_brn_median, yend = core_brn_median), linewidth = 1.5, color = "white") +
  geom_segment(data = windows,aes(x = start-2, xend = end+2, y = seedwall_median, yend = seedwall_median), linewidth = 1, color = "#A2D435") +
  geom_segment(data = windows,aes(x = start-2, xend = end+2, y = core_blk_median, yend = core_blk_median), linewidth = 1, color = "black") +
  geom_segment(data = windows,aes(x = start-2, xend = end+2, y = core_brn_median, yend = core_brn_median), linewidth = 1, color = "#9D5B0B")




### Model seedling density in the core plots from day > 220

d_mod = d_nogrn_sp |>
  filter(day_of_burning > 220) |>
  mutate(fire_intens =  pmax(litter_cover,vol_brn_50m),
         fire_intens2 = litter_cover + vol_brn_50m) |>
  mutate(seedl_dens_ALL = ifelse(seedl_dens_ALL < 0.001592357, 0.001592357, seedl_dens_ALL)) |>
  mutate(seedl_dens_ALL = ifelse(seedl_dens_ALL > 10, 10, seedl_dens_ALL)) |>
  mutate(ba = ba_factor * ba_tally)

ggplot(d_mod, aes(x = vol_brn_50m, y = seedl_dens_ALL)) +
  geom_point() +
  coord_trans(y = "log")


m = gam(round(seedl_dens_ALL*314) ~ s(fire_intens2, k = 3) + s(ppt, k = 3)  + s(nongrowing_cover, k = 3), data = d_mod, family = poisson)
summary(m)
plot(m)













## PINES

d_nogrn_sp = d |>
  filter(PINES_green_vol_abs == 0,
         ((is.na(dist_grn_PINES) | dist_grn_PINES > 100) & as.numeric(sight_line) > 100),
         (prefire_prop_pipj + prefire_prop_pila + prefire_prop_pico + prefire_prop_pimo) > 20,
         plot_type %in% c("core","delayed"))

# mean seedl dens
summary(d_nogrn_sp$seedl_dens_PINES)



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


summary(d_nogrn_sp$seedl_dens_ABCO)
hist(d_nogrn_sp$seedl_dens_ABCO, breaks = 50)

ggplot(d_nogrn_sp, aes(x = day_of_burning, y = ifelse(seedl_dens_ABCO > 20, 20, seedl_dens_ABCO))) +
  geom_jitter(data = d_sw_sp, color="green", size=3, width=2) +
  geom_jitter(size=3, width=2) +
  facet_grid(~fire) +
  #scale_color_viridis_c(limits=c(0,.5)) +
  theme_bw(15) +
  labs(x = "Day of Burning", y = "Seedlings / sq m") +
  lims(y = c(0,0.5))

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

ggplot(d_nogrn_sp, aes(x=under_cones_new_PINES, y = sqrt(seedl_dens_PINES), group = under_cones_new_PINES)) +
  geom_boxplot(width = 0.1) +
  theme_bw(12)

## YLWPINES
d_nogrn_sp = d |>
  filter(YLWPINES_green_vol_abs == 0,
         (prefire_prop_pipj) > 20,
         plot_type %in% c("core","delayed")) |>
  mutate(under_cones_YLWPINES = recode(under_cones_new_YLWPINES, "0" = "None", "1" = "Low", "2" = "High")) |>
  mutate(under_cones_YLWPINES = factor(under_cones_YLWPINES, levels=c("None","Low","High"))) |>
  filter(!(is.na(under_cones_YLWPINES)))

ggplot(d_nogrn_sp, aes(x=sqrt(cone_dens_YLWPINES), y = sqrt(seedl_dens_YLWPINES))) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  theme_bw(15) +
  labs(x = "Cones / sq m", y = "Seedlings / sq m")

ggplot(d_nogrn_sp, aes(x=under_cones_YLWPINES, y = sqrt(seedl_dens_YLWPINES))) +
  geom_point() +
  geom_point(size=2) +
  geom_boxplot() +
  theme_bw(15) +
  labs(x = "Relative pine cone density in vicinity", y = "Seedlings / sq m")


m = lm(seedl_dens_YLWPINES~cone_dens_YLWPINES, d = d_nogrn_sp)
summary(m)


######## In core area: seedling density relative to overstory torch

## ALL conifers
d_nogrn_sp = d |>
  filter(ALL_green_vol_abs == 0,
         ((is.na(dist_grn_ALL) | dist_grn_ALL > 100) & as.numeric(sight_line) > 75), # this makes it a little worse here
         plot_type %in% c("core","delayed"))

ggplot(d_nogrn_sp |> mutate(seedl_dens_ALL = ifelse(seedl_dens_ALL > 5, 5, seedl_dens_ALL)), aes(x = vol_brn_50m, y = seedl_dens_ALL)) +
  geom_point(size = 2) +
  theme_bw(16) +
  labs(x = "Scorched canopy (%)", y = "Conifer seedlings / sq m")

ggplot(d_nogrn_sp, aes(x = vol_brn_50m, y = seedl_dens_PINES)) +
  geom_point()

m = lm(seedl_dens_PINES ~ vol_brn_50m, d = d_nogrn_sp)
summary(m)



## PINES
d_nogrn_sp = d |>
  filter(PINES_green_vol_abs == 0,
         ((is.na(dist_grn_PINES) | dist_grn_PINES > 100) & as.numeric(sight_line) > 100), # this makes it a little worse here
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
  geom_point(size = 2) +
  theme_bw(16) +
  labs(x = "Scorched fir canopy (%)", y = "White fir seedlings / sq m")

ggplot(d_nogrn_sp, aes(x = vol_brn_50m, y = seedl_dens_ABCO)) +
  geom_point()

m = lm(seedl_dens_ABCO~ABCO_untorched_vol_abs, d = d_nogrn_sp)
summary(m)



### GAM with multiple predictors

library(mgcv)


### PINES

d_nogrn_sp = d |>
  filter(PINES_green_vol_abs == 0,
         fire == "Dixie",
         ppt < 1375,
         ((is.na(dist_grn_PINES) | dist_grn_PINES > 100) & as.numeric(sight_line) > 100), # this makes it a little worse here
         (prefire_prop_pipj + prefire_prop_pila + prefire_prop_pico + prefire_prop_pimo) > 20,
         plot_type %in% c("core","delayed"))

m = gam(seedl_dens_PINES ~ day_of_burning + PINES_untorched_vol_abs + cone_dens_PINES, data = d_nogrn_sp)
summary(m)

m = gam(seedl_dens_PINES ~ s(day_of_burning, k=3) + s(vol_brn_50m, k=3) + s(cone_dens_PINES, k=3), data = d_nogrn_sp)
summary(m)

m = gam(round(seedl_dens_PINES) ~ te(day_of_burning, PINES_untorched_vol_abs, k=3), data = d_nogrn_sp, family = poisson())
summary(m)

plot(m, pers=TRUE, too.far=1)





## Make prediction plots for the DOB-untorched vol model

untorched_low = data.frame(vol_brn_50m = 0.1 * 100,
                           tmean = mean(d_nogrn_sp$tmean),
                           ppt = mean(d_nogrn_sp$ppt),
                           cone_dens_PINES = mean(d_nogrn_sp$cone_dens_PINES),
                           day_of_burning = seq(200,255, length.out = 200),
                           scorch = "low")
untorched_med = data.frame(vol_brn_50m = 0.35 * 100,
                           tmean = mean(d_nogrn_sp$tmean),
                           ppt = mean(d_nogrn_sp$ppt),
                           cone_dens_PINES = mean(d_nogrn_sp$cone_dens_PINES),
                           day_of_burning = seq(200,255, length.out = 200),
                           scorch = "mod")
untorched_high = data.frame(vol_brn_50m = 0.6 * 100,
                            tmean = mean(d_nogrn_sp$tmean),
                            ppt = mean(d_nogrn_sp$ppt),
                            cone_dens_PINES = mean(d_nogrn_sp$cone_dens_PINES),
                            day_of_burning = seq(200,255, length.out = 200),
                            scorch = "high")

newdat = bind_rows(untorched_low,
                   untorched_med,
                   untorched_high)

newdat$scorch = factor(newdat$scorch, levels = c("high", "mod","low"))

preds = predict(m,newdat, type = "response")

newdat$fit = preds

## Get partial residuals: 

# Take observed data frame and set everything at the mean except scorch and DOB
# Use it to get median predictions
# Get the residuals of the original model
# Partial residuals = preds + resids

obs_partial = d_nogrn_sp |>
  select(vol_brn_50m,
         tmean,
         ppt,
         cone_dens_PINES,
         day_of_burning) |>
  mutate(tmean = mean(tmean),
         ppt = mean(ppt))

partial_preds = predict(m, newdata = obs_partial)
resids = residuals(m)

d_partial = data.frame(obs_partial,
                       partial_preds,
                       resids) |>
  mutate(partial_resids = exp(partial_preds + resids))


## Plot

ggplot(newdat, aes(x = day_of_burning, y = fit, color = vol_brn_50m)) +
  geom_line(linewidth=2, aes(group = scorch)) +
  # geom_point(data = d_partial, aes(x = day_of_burning, y = partial_resids, color = vol_brn_50m)) +
  scale_color_viridis_c(end = 0.8, option = "magma") +
  theme_bw(15) +
  labs(x = "Day of burning", y = "Seedlings / sq m")









### YLWPINES

d_nogrn_sp = d |>
  filter(YLWPINES_green_vol_abs == 0,
         ((is.na(dist_grn_YLWPINES) | dist_grn_YLWPINES > 75) & as.numeric(sight_line) > 75), # this makes it a little worse here
         (prefire_prop_pipj) > 20,
         plot_type %in% c("core","delayed"))


inspect = d_nogrn_sp |>
  filter(fire == "Dixie",
         day_of_burning > 240) |>
  select(plot_id,
         seedl_dens_YLWPINES,
         dist_grn_YLWPINES,
         dist_grn_ABCO)



d_nogrn_sp[d_nogrn_sp$seedl_dens_YLWPINES > 1, "seedl_dens_YLWPINES"] = 1

inspect = d_nogrn_sp |>
  filter(fire == "Dixie") |>
  select(day_of_burning,
         vol_brn_50m,
         seedl_dens_YLWPINES)

ggplot(data=d_nogrn_sp |> filter(fire == "Dixie"), aes(x = day_of_burning, y = vol_brn_50m, color = seedl_dens_YLWPINES)) +
  geom_jitter(width=1, height = 1)


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
         ((is.na(dist_grn_ABCO) | dist_grn_ABCO > 100) & as.numeric(sight_line) > 100), # this makes it a little worse here
         (prefire_prop_abco) > 10,
         plot_type %in% c("core","delayed"))

d_nogrn_sp[d_nogrn_sp$seedl_dens_ABCO > 1, "seedl_dens_ABCO"] = 1

m = gam(seedl_dens_ABCO ~ day_of_burning + ABCO_untorched_vol_abs, data = d_nogrn_sp)
summary(m)

m = gam(seedl_dens_ABCO ~ s(day_of_burning, k=3) + s(ABCO_untorched_vol_abs, k=3), data = d_nogrn_sp)
summary(m)

m = gam(round(seedl_dens_ABCO) ~ te(day_of_burning, ABCO_untorched_vol_abs, k = c(3,3)) + s(ppt, k = 3), data = d_nogrn_sp, family = poisson())
summary(m)

plot(m, pers=TRUE, too.far=0.25, trans = exp)

ggplot(data=d_nogrn_sp |> filter(fire == "Dixie"), aes(x = day_of_burning, y = vol_brn_50m, color = seedl_dens_ABCO)) +
  geom_jitter(width=1, height = 1)


## Make prediction plots for the DOB-untorched vol model

untorched_low = data.frame(ABCO_untorched_vol_abs = 0.05,
                           tmean = mean(d_nogrn_sp$tmean),
                           ppt = mean(d_nogrn_sp$ppt),
                           day_of_burning = 200:255,
                           scorch = "low")
untorched_med = data.frame(ABCO_untorched_vol_abs = 0.2,
                           tmean = mean(d_nogrn_sp$tmean),
                           ppt = mean(d_nogrn_sp$ppt),
                           day_of_burning = 200:255,
                           scorch = "mod")
untorched_high = data.frame(ABCO_untorched_vol_abs = 0.5,
                            tmean = mean(d_nogrn_sp$tmean),
                            ppt = mean(d_nogrn_sp$ppt),
                            day_of_burning = 200:255,
                            scorch = "high")

newdat = bind_rows(untorched_low,
                   untorched_med,
                   untorched_high)

newdat$scorch = factor(newdat$scorch, levels = c("high", "mod","low"))

preds = predict(m,newdat, type = "response")

newdat$fit = preds

## Get partial residuals: 

# Take observed data frame and set everything at the mean except scorch and DOB
# Use it to get median predictions
# Get the residuals of the original model
# Partial residuals = preds + resids

obs_partial = d_nogrn_sp |>
  select(ABCO_untorched_vol_abs,
         tmean,
         ppt,
         day_of_burning) |>
  mutate(tmean = mean(tmean),
         ppt = mean(ppt))

partial_preds = predict(m, newdata = obs_partial)
resids = residuals(m)

d_partial = data.frame(obs_partial,
                       partial_preds,
                       resids) |>
  mutate(partial_resids = exp(partial_preds + resids))




#### Get the range of the observations in each of the scorch categories

r1 = d_nogrn_sp |>
  filter(between(ABCO_untorched_vol_abs, 0.0, 0.1)) |>
  summarize(npts = n(),
            min = min(day_of_burning),
            max = max(day_of_burning),
            q10 = quantile(day_of_burning,.05),
            q90 = quantile(day_of_burning,.95))
r2 = d_nogrn_sp |>
  filter(between(ABCO_untorched_vol_abs, 0.1, 0.3)) |>
  summarize(npts = n(),
            min = min(day_of_burning),
            max = max(day_of_burning),
            q10 = quantile(day_of_burning,.05),
            q90 = quantile(day_of_burning,.95))
r3 = d_nogrn_sp |>
  filter(between(ABCO_untorched_vol_abs, 0.3, .8)) |>
  summarize(npts = n(),
            min = min(day_of_burning),
            max = max(day_of_burning),
            q10 = quantile(day_of_burning,.05),
            q90 = quantile(day_of_burning,.95))

rbind(r1, r2, r3)

## Truncate preds based on the ranges

newdat = newdat |>
  filter((between(ABCO_untorched_vol_abs, 0.0, 0.1) & between(day_of_burning, 203, 241)) |
           (between(ABCO_untorched_vol_abs, 0.1, 0.2) & between(day_of_burning, 203, 241)) |
           (between(ABCO_untorched_vol_abs, 0.2, 0.8) & between(day_of_burning, 225, 242)))



## Plot

ggplot(newdat, aes(x = day_of_burning, y = fit, color = ABCO_untorched_vol_abs)) +
  geom_line(linewidth=2, aes(group = scorch)) +
  #geom_point(data = d_partial, aes(x = day_of_burning, y = partial_resids, color = ABCO_untorched_vol_abs)) +
  scale_color_viridis_c(end = 0.8, option = "magma") +
  theme_bw(15) +
  labs(x = "Day of burning", y = "Seedlings / sq m")


### Counterfactual with scorched canopy varying


















### ALL conifers

d_nogrn_sp = d |>
  filter(ALL_green_vol_abs == 0,
         ((is.na(dist_grn_ALL) | dist_grn_ALL > 100) & as.numeric(sight_line) > 100), # this makes it a little worse here
         plot_type %in% c("core","delayed"))

m = gam(seedl_dens_ALL ~ day_of_burning + ALL_untorched_vol_abs, data = d_nogrn_sp)
summary(m)

m = gam(seedl_dens_ALL ~ s(day_of_burning, k=3) + s(ALL_untorched_vol_abs, k=3), data = d_nogrn_sp)
summary(m)

m = gam(round(seedl_dens_ALL) ~ te(day_of_burning, ALL_untorched_vol_abs, k = c(3,3)), data = d_nogrn_sp, family = poisson())
summary(m)

plot(m, pers=TRUE, too.far=0.25)





## Make prediction plots for the DOB-untorched vol model

untorched_low = data.frame(ALL_untorched_vol_abs = 0.1,
                           day_of_burning = 200:255,
                           cone_dens_ALL = 0.4,
                           scorch = "low")
untorched_med = data.frame(ALL_untorched_vol_abs = 0.4,
                           day_of_burning = 200:255,
                           cone_dens_ALL = 0.4,
                           scorch = "mod")
untorched_high = data.frame(ALL_untorched_vol_abs = 0.7,
                            day_of_burning = 200:255,
                            cone_dens_ALL = 0.4,
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



## TO DO: GAMs for each fire separately. Also try excluding/including the high-precip Dixie plots.
# Do for pines and abies
# Set outliers to highest non-outlier value
# Consider transforming vars? Probably centering and standardizing at least.
# Partial resids
