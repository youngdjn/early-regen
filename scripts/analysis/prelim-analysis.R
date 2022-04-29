# Purpose: Preliminary plot data analysis to inform 2022 revisits

library(tidyverse)
library(here)
library(sf)
library(readxl)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


#### Load compiled plot data ####

d = read_csv(datadir("field-data/processed/plot_seedl_cone_grnseedsource.csv"))


inspect = d %>%
  filter(toupper(plot_type) == "SEEDWALL")

inspect = d %>%
  select(fire, plot_id, seedl_dens_PINES, seedl_dens_YLWPINES)



# for pines, firs, cade: get plots > 100 m from green seed source and get the proportion that had seedlings

summarize_across_plots = function(data, species, seed_dist) {
  
  dist_grn_var = paste0("dist_grn_", species)
  seedl_dens_var = paste0("seedl_dens_", species)
  cone_dens_var = paste0("cone_dens_", species)
  
  # if we're asking for cones for a species that doesn't have cones, create a fake col with only NAs
  if(!(cone_dens_var %in% names(data))) {
    data[,cone_dens_var] = NA
  }
  
  if(seed_dist == "gt100") {
    focal_dist = data[,dist_grn_var] > 100 | is.na(data[,dist_grn_var]) # Confirm what NA means
  } else if(seed_dist == "lt50") {
    focal_dist = data[,dist_grn_var] < 50
  } else if(seed_dist == "all") {
    focal_dist = TRUE
  }
  
  browser()
  
  summarized = data %>%
    filter(focal_dist) %>%
    mutate(seedl_present = .data[[seedl_dens_var]] > 0,
           seedl_dens_var = .data[[seedl_dens_var]],
           cone_dens_var = .data[[cone_dens_var]]) %>%
    summarize(prop_recruitment = sum(seedl_present)/n(),
              n_plots = n(),
              mean_seedl_dens = mean(seedl_dens_var),
              mean_cone_dens = mean(cone_dens_var),
              mean_seedl_cone_ratio = mean_seedl_dens/mean_cone_dens,
              mean_seedl_dens_present = mean(seedl_dens_var[seedl_dens_var > 0]),
              mean_cone_dens_present = mean(cone_dens_var[seedl_dens_var > 0]), # mean cone density where the *seedlings* are present
              # proportion of plots with seedlings present, by fire
              prop_recruitment_august = sum(seedl_present & (fire == "August")) / sum(fire == "August"),
              prop_recruitment_creek = sum(seedl_present & (fire == "Creek")) / sum(fire == "Creek"),
              prop_recruitment_north = sum(seedl_present & (fire == "North")) / sum(fire == "North"),
    ) %>%
    mutate(across(everything(),~round(.x,3))) %>%
    mutate(species = species)
  
  return(summarized)

}


#### For core plots

# filter to core plots
unique(d$plot_type)
core = d %>%
  filter(toupper(plot_type) == "CORE")

# what's the range of distance to green?
hist(core$dist_grn_ALL)


core_nogrn_pine = summarize_across_plots(data = core, species = "PINES", seed_dist = "gt100")
core_nogrn_fir = summarize_across_plots(data = core, species = "FIRS", seed_dist = "gt100")
core_nogrn_cade = summarize_across_plots(data = core, species = "CADE", seed_dist = "gt100")
core_nogrn_psme = summarize_across_plots(data = core, species = "PSME", seed_dist = "gt100")
core_nogrn_ylwpines = summarize_across_plots(data = core, species = "YLWPINES", seed_dist = "gt100")
core_nogrn_pila = summarize_across_plots(data = core, species = "PILA", seed_dist = "gt100")

core_nogrn = bind_rows(core_nogrn_pine,
                       core_nogrn_fir,
                       core_nogrn_cade,
                       core_nogrn_psme,
                       core_nogrn_ylwpines,
                       core_nogrn_pila) %>%
  mutate(type = "core-nogreem")


core_grn_pine = summarize_across_plots(data = core, species = "PINES", seed_dist = "lt50")
core_grn_fir = summarize_across_plots(data = core, species = "FIRS", seed_dist = "lt50")
core_grn_cade = summarize_across_plots(data = core, species = "CADE", seed_dist = "lt50")
core_grn_psme = summarize_across_plots(data = core, species = "PSME", seed_dist = "lt50")
core_grn_ylwpines = summarize_across_plots(data = core, species = "YLWPINES", seed_dist = "lt50")
core_grn_pila = summarize_across_plots(data = core, species = "PILA", seed_dist = "lt50")

core_grn = bind_rows(core_grn_pine,
                       core_grn_fir,
                       core_grn_cade,
                       core_grn_psme,
                       core_grn_ylwpines,
                       core_grn_pila) %>%
  mutate(type = "core-green")

#### For seed wall plots

# filter to seed wall plots
unique(d$plot_type)
seedwall = d %>%
  filter(toupper(plot_type) == "SEEDWALL")

seedwall_pine = summarize_across_plots(data = seedwall, species = "PINES", seed_dist = "all")
seedwall_fir = summarize_across_plots(data = seedwall, species = "FIRS", seed_dist = "all")
seedwall_cade = summarize_across_plots(data = seedwall, species = "CADE", seed_dist = "all")
seedwall_psme = summarize_across_plots(data = seedwall, species = "PSME", seed_dist = "all")
seedwall_ylwpines = summarize_across_plots(data = seedwall, species = "YLWPINES", seed_dist = "all")
seedwall_pila = summarize_across_plots(data = seedwall, species = "PILA", seed_dist = "all")

seedwall_grn = bind_rows(seedwall_pine,
                         seedwall_fir,
                         seedwall_cade,
                         seedwall_psme,
                         seedwall_ylwpines,
                         seedwall_pila) %>%
  mutate(type = "seedwall")



summarized = bind_rows(core_grn,
                       core_nogrn,
                       seedwall_grn) %>%
  select(type, species, everything())



### How much does the amount of brown cover affect it?

## Away from green

# For pines
core_nogrn_pine = core %>%
  filter(dist_grn_PINES > 100 | is.na(dist_grn_PINES))
  
ggplot(core_nogrn_pine,aes(x=cov_brn_50m, y = seedl_dens_PINES)) +
  geom_point() +
  geom_smooth(method=lm)

# For firs
core_nogrn_fir = core %>%
  filter(dist_grn_FIRS > 100 | is.na(dist_grn_FIRS))

ggplot(core_nogrn_fir,aes(x=cov_brn_50m, y = seedl_dens_FIRS)) +
  geom_point()





####!!!! Analysis next steps:
# get overstory brown PINE and also by PIPJ and PILA
# relate overstory brown PINE to seedl dens

# repeate all above for PIPJ and PILA



### pull in burn date to see if it matters???



## for seed wall: see if seedling:cone ratio is higher. Include % green or some metrics of seed wall density, or species comp

### Qs for planning revisits:
## How important to know the seedlings survived the first year?
## Can we add some plots that are only surveyed in Yr 2? Will have dropped more needles. So, probably need to follow up on the Yr 1 ones to be able to back-estimate the brown canopy.

### For scouting:
## look for delayed mortality to make sure it happened. Because it if didn't, not worth sending the crew there yet (right??)
