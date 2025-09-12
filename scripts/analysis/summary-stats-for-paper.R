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
  mutate(dist_grn_conservative = pmin(dist_grn_sp, sight_line, na.rm = TRUE)) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            dist_median = median(dist_grn_conservative, na.rm = TRUE),
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

