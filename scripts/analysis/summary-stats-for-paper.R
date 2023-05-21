#### Summary statistics to report in paper

library(tidyverse)
library(here)

# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)

source("scripts/analysis/year1-dixie-caldor_functions.R")

# Load data
d = read_csv(file.path(datadir,"field-data/processed/plot-data-prepped.csv"))

# Prep the plots used for the "all species" analyses
d_sp = prep_d_sp("ALL")

### Overall seedling densities, and count of scorched/torched plots
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed")) |>
  mutate(intens_cat = ifelse(fire_intens > 85, "torched", "scorched")) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            n_torched = sum(intens_cat == "torched"),
            n_scorched = sum(intens_cat == "scorched"))


## Specifically early-burn plots
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed"),
         day_of_burning <= 210) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            n = n())


## Seed wall plots burning BEfore 1 Aug
d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60,
         day_of_burning < 210) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            nplots = n(),
            med_dist_grn = median(dist_grn_sp))




### Species proportions in low vs high torch plots
# for low intensity plots
d_spcomp =  d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         day_of_burning > 210,
         between(fire_intens, 0, 50),
         plot_type %in% c("core", "delayed")) |>
  summarize(across(c(seedl_dens_ABIES,seedl_dens_CADE,seedl_dens_PILA,seedl_dens_PSME,seedl_dens_PICO,seedl_dens_YLWPINES), median),
            nplots = n())
tot = d_spcomp |>
  select(starts_with("seedl_dens_")) |>
  mutate(tot = rowSums(across(everything()))) |>
  pull(tot)

d_spcomp_lowintens = d_spcomp |>
  mutate(across(starts_with("seedl_dens_"), ~. / tot)) |>
  mutate(across(starts_with("seedl_dens_"), ~round(.*100, 1))) |>
  mutate(intensity = "low")
d_spcomp_lowintens

# for high intensity plots
d_spcomp =  d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         day_of_burning > 210,
         between(fire_intens, 80, 100),
         plot_type %in% c("core", "delayed")) |>
  summarize(across(c(seedl_dens_ABIES,seedl_dens_CADE,seedl_dens_PILA,seedl_dens_PSME,seedl_dens_PICO,seedl_dens_YLWPINES), median),
            nplots = n())
tot = d_spcomp |>
  select(starts_with("seedl_dens_")) |>
  mutate(tot = rowSums(across(everything()))) |>
  pull(tot)

d_spcomp_highintens = d_spcomp |>
  mutate(across(starts_with("seedl_dens_"), ~. / tot)) |>
  mutate(across(starts_with("seedl_dens_"), ~round(.*100, 1))) |>
  mutate(intensity = "high")
d_spcomp_highintens

d_spcomp_intens = bind_rows(d_spcomp_lowintens, d_spcomp_highintens)
d_spcomp_intens







### proportions by species of seedlings
# exclude the early burn plots, compute by median

# for core area plots
d_spcomp =  d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         day_of_burning > 210,
         plot_type %in% c("core", "delayed")) |>
  summarize(across(c(seedl_dens_ABIES,seedl_dens_CADE,seedl_dens_PILA,seedl_dens_PSME,seedl_dens_PICO,seedl_dens_YLWPINES), median))
d_spcomp$tot = rowSums(d_spcomp)

d_spcomp_core = d_spcomp |>
  mutate(across(everything(), ~. / tot)) |>
  mutate(across(everything(), ~round(.*100, 1))) |>
  mutate(type = "core")
d_spcomp_core


# and seed wall plots
d_spcomp =  d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60) |>
  summarize(across(c(seedl_dens_ABIES,seedl_dens_CADE,seedl_dens_PILA,seedl_dens_PSME,seedl_dens_PICO,seedl_dens_YLWPINES), median))

d_spcomp$tot = rowSums(d_spcomp)

d_spcomp_sw = d_spcomp |>
  mutate(across(everything(), ~. / tot)) |>
  mutate(across(everything(), ~round(.*100, 1))) |>
  mutate(type = "seedwall")
d_spcomp_sw

# combine and write
spcomp = bind_rows(d_spcomp_core, d_spcomp_sw)
spcomp
write_csv(spcomp, file.path(datadir, "tables/regen_species_comp.csv"))



### Get mean pre-fire overstory species comp across all core plots burning in Aug or later
d_ovrspcomp = d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         day_of_burning > 210,
         plot_type %in% c("core", "delayed")) |>
  summarize(across(starts_with("prefire_prop_"), mean)) |>
  arrange(decreasing = TRUE)
d_ovrspcomp



### Get max seedling density by species
d_max_seedl_dens =  d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed")) |>
  summarize(across(starts_with("seedl_dens_"), max))
d_max_seedl_dens









#######%
#######
#######
########
#######

##### Older summary stats calcs


#### Summary statistics to report in paper
d_sp = prep_d_sp("ALL")

### Overall seedling densities, and count of scorched/torched plots
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed")) |>
  mutate(intens_cat = ifelse(fire_intens > 85, "torched", "scorched")) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            n_torched = sum(intens_cat == "torched"),
            n_scorched = sum(intens_cat == "scorched"))


##### For edge-vs-interior comparison table

## Scorched core are plots burning after 1 Aug
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed"),
         day_of_burning > 210,
         fire_intens < median_scorching_extent) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            n = n())

## Early-burn, scorched interior plots (before 1 Aug), but with relaxed distance to green
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 60) & sight_line > 60),
         plot_type %in% c("core", "delayed"),
         day_of_burning <= 210,
         fire_intens < median_scorching_extent) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            n = n())

## Scorched core area plots burning mid-Aug
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed"),
         day_of_burning > 222,
         day_of_burning <= 232,
         fire_intens < median_scorching_extent) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            nplots = n())

# 
# # total number of relaxed distance plots regardless of fire intens
# d_sp |>
#   filter(grn_vol_abs_sp == 0,
#          ((is.na(dist_grn_sp) | dist_grn_sp > 60) & sight_line > 60),
#          plot_type %in% c("core", "delayed"),
#          day_of_burning <= 210) |>
#   summarize(dens_mean = mean(seedl_dens_sp),
#             dens_median = median(seedl_dens_sp),
#             n = n())

# 
# ## Seed wall plots
# a = d_sp |>
#   filter(plot_type == "seedwall") |>
#              filter(dist_sw <= 60) |>
#   summarize(dens_mean = mean(seedl_dens_sp),
#             dens_median = median(seedl_dens_sp),
#             med_dist_grn = median(dist_grn_sp),
#             n = n())
# a

## Seed wall plots burning BEfore 1 Aug
d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60,
         day_of_burning < 210) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            nplots = n(),
            med_dist_grn = median(dist_grn_sp))


## Seed wall plots burning after 1 Aug
d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60,
         day_of_burning >= 210) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            nplots = n(),
            med_dist_grn = median(dist_grn_sp))

## Seed wall plots burning in mid-Aug
d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60,
         day_of_burning >= 222,
         day_of_burning <= 232) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            nplots = n(),
            med_dist_grn = median(dist_grn_sp))






## Core area plots burning mid-Aug
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed"),
         day_of_burning > 222,
         day_of_burning <= 232) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            nplots = n())


## Proportion of core area plots burning mid-Aug
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed"),
         day_of_burning > 222,
         day_of_burning <= 232) |>
  nrow()

## Total number of core area plots
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed")) |>
  nrow()

# Total number of scorched and torched core area plots
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed")) |>
  summarize(torched = sum(fire_intens > median_scorching_extent),
            scorched = sum(fire_intens <= median_scorching_extent))

# Total number of scorched and torched seed wall plots
d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60) |>
  summarize(torched = sum(fire_intens > median_scorching_extent),
            scorched = sum(fire_intens <= median_scorching_extent),
            tot = n())

### Compare early-burned and late-burned *scorched* core plot seedling densities
# Early burned
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 60) & sight_line > 60),
         plot_type %in% c("core", "delayed"),
         fire_intens <= median_scorching_extent,
         day_of_burning < 210) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            nplots = n())

# Late burned
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed"),
         fire_intens <= median_scorching_extent,
         day_of_burning > 210) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            nplots = n())



### Compare early-burned and late-burned *torched* core plot seedling densities (using the less strict 60 m )
# Early burned
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 60) & sight_line > 60),
         plot_type %in% c("core", "delayed"),
         fire_intens > median_scorching_extent,
         day_of_burning < 210) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            nplots = n())

# Late burned
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed"),
         fire_intens > median_scorching_extent,
         day_of_burning > 210) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            nplots = n())





### Median seedling density (and plot count) in early-burned plots
# Core plots
d_earlydens =  d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         day_of_burning < 210,
         plot_type %in% c("core", "delayed")) |>
  summarize(seedl_dens_sp = median(seedl_dens_sp),
            nplots = n())
d_earlydens

# Seed wall plots
d_earlydens =  d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60,
         day_of_burning < 210) |>
  summarize(seedl_dens_sp = median(seedl_dens_sp))
d_earlydens

# Core plots (liberal definition)
d_earlydens =  d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 60) & sight_line > 60),
         day_of_burning < 210,
         plot_type %in% c("core", "delayed")) |>
  summarize(seedl_dens_sp = median(seedl_dens_sp),
            nplots = n())
d_earlydens




### Get max seedling density by species
d_max_seedl_dens =  d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed")) |>
  summarize(across(starts_with("seedl_dens_"), max))
d_max_seedl_dens




#####
#####
#####
#####
#####
#####
#####



####% Confirm plots meet survey criteria
d_sp = prep_d_sp("ALL")

### seed wall plots
d_sp_sw = d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60)

# percent green remaining within 10 m
summary(d_sp_sw$vol_grn_10m)
(table(d_sp_sw$vol_grn_10m))

# percent pine canopy volume in seed wall
table(d_sp_sw$PINES_green_vol)
inspect = d_sp_sw |> select(plot_id, PINES_green_vol, capable_growing_area)

# pine proportion
summary(d_sp_sw$vol_grn_10m)
(table(d_sp_sw$vol_grn_10m))

### core area plots
d_sp_core = d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
         plot_type %in% c("core", "delayed"))

# percent pre-fire pine
inspect = d_sp_core |> select(plot_id, PINES_green_vol, prefire_prop_PINES, capable_growing_area)

# within each area, what percentage of plots had >= 10% pine?
a = inspect |>
  mutate(area = str_split(plot_id, fixed("-")) |> map(1)) |>
  mutate(meets_minimum_pines = prefire_prop_PINES >= 10) |>
  group_by(area) |>
  summarize(prop_meets_minimum_pines = mean(meets_minimum_pines),
            tot_plots = n(),
            exception_plots = sum(!meets_minimum_pines))
a

