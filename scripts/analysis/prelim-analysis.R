# Purpose: Preliminary plot data analysis to inform 2022 revisits

library(tidyverse)
library(here)
library(sf)
library(readxl)
library(magrittr)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


#### Load compiled plot data ####

d = read_csv(datadir("field-data/processed/plot_seedl_cone_grnseedsource_comp.csv"))


inspect = d %>%
  filter(toupper(plot_type) == "SEEDWALL")

inspect = d %>%
  select(fire, plot_id, seedl_dens_PINES, seedl_dens_YLWPINES)



# for pines, firs, cade: get plots > 100 m from green seed source and get the proportion that had seedlings, mean density of seedlings, and mean density of cones (at the plot and under seedwall)

summarize_across_plots = function(data, species, seed_dist) {
  
  dist_grn_var = paste0("dist_grn_", species)
  seedl_dens_var = paste0("seedl_dens_", species)
  cone_dens_var = paste0("cone_dens_", species)
  seedwall_cone_dens_var = paste0("seedwall_cone_dens_", species)
  
  # if we're asking for cones for a species that doesn't have cones, create a fake col with only NAs
  if(!(cone_dens_var %in% names(data))) {
    data[,cone_dens_var] = NA
  }
  
  # if we're asking for cones for a species that doesn't have cones, create a fake col with only NAs
  if(!(seedwall_cone_dens_var %in% names(data))) {
    data[,seedwall_cone_dens_var] = NA
  }
  
  if(seed_dist == "gt100") {
    focal_dist = data[,dist_grn_var] > 100 | is.na(data[,dist_grn_var]) # Confirm what NA means
  } else if(seed_dist == "lt50") {
    focal_dist = data[,dist_grn_var] < 50
  } else if(seed_dist == "all") {
    focal_dist = TRUE
  }
  
  summarized = data %>%
    filter(focal_dist) %>%
    mutate(seedl_present = .data[[seedl_dens_var]] > 0,
           seedl_dens_var = .data[[seedl_dens_var]],
           cone_dens_var = .data[[cone_dens_var]],
           seedwall_cone_dens_var = .data[[seedwall_cone_dens_var]]) %>%
    summarize(prop_recruitment = sum(seedl_present)/n(),
              n_plots = n(),
              mean_seedl_dens = mean(seedl_dens_var),
              mean_cone_dens = mean(cone_dens_var),
              mean_seedl_cone_ratio = mean_seedl_dens/mean_cone_dens,
              mean_underseedwall_cone_dens = mean(seedwall_cone_dens_var, na.rm=TRUE),
              mean_seedl_dens_present = mean(seedl_dens_var[seedl_dens_var > 0]),
              mean_cone_dens_present = mean(cone_dens_var[seedl_dens_var > 0]), # mean cone density where the *seedlings* are present
              # proportion of plots with seedlings present, by fire
              prop_recruitment_august = sum(seedl_present & (fire == "August")) / sum(fire == "August"),
              prop_recruitment_creek = sum(seedl_present & (fire == "Creek")) / sum(fire == "Creek"),
              prop_recruitment_north = sum(seedl_present & (fire == "North")) / sum(fire == "North"),
    ) %>%
    mutate(across(everything(),~round(.x,5))) %>%
    mutate(species = species)
  
  return(summarized)

}


#### For core plots

# filter to core plots
unique(d$plot_type)
core = d %>%
  filter(toupper(plot_type) == "CORE")

###!!! NOTE TODO check paper datasheets: why does C021040, P820, P821, C42 have seedwall cones? it is a core plot.

# what's the range of distance to green?
hist(core$dist_grn_ALL)

core_nogrn_all = summarize_across_plots(data = core, species = "ALL", seed_dist = "gt100")
core_nogrn_pine = summarize_across_plots(data = core, species = "PINES", seed_dist = "gt100")
core_nogrn_fir = summarize_across_plots(data = core, species = "FIRS", seed_dist = "gt100")
core_nogrn_cade = summarize_across_plots(data = core, species = "CADE", seed_dist = "gt100")
core_nogrn_psme = summarize_across_plots(data = core, species = "PSME", seed_dist = "gt100")
core_nogrn_ylwpines = summarize_across_plots(data = core, species = "YLWPINES", seed_dist = "gt100")
core_nogrn_pila = summarize_across_plots(data = core, species = "PILA", seed_dist = "gt100")

core_nogrn = bind_rows(core_nogrn_all,
                        core_nogrn_pine,
                       core_nogrn_fir,
                       core_nogrn_cade,
                       core_nogrn_psme,
                       core_nogrn_ylwpines,
                       core_nogrn_pila) %>%
  mutate(type = "core-nogreem")


core_grn_all = summarize_across_plots(data = core, species = "ALL", seed_dist = "lt50")
core_grn_pine = summarize_across_plots(data = core, species = "PINES", seed_dist = "lt50")
core_grn_fir = summarize_across_plots(data = core, species = "FIRS", seed_dist = "lt50")
core_grn_cade = summarize_across_plots(data = core, species = "CADE", seed_dist = "lt50")
core_grn_psme = summarize_across_plots(data = core, species = "PSME", seed_dist = "lt50")
core_grn_ylwpines = summarize_across_plots(data = core, species = "YLWPINES", seed_dist = "lt50")
core_grn_pila = summarize_across_plots(data = core, species = "PILA", seed_dist = "lt50")

core_grn = bind_rows(core_grn_all,
                      core_grn_pine,
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

seedwall_all = summarize_across_plots(data = seedwall, species = "ALL", seed_dist = "all")
seedwall_pine = summarize_across_plots(data = seedwall, species = "PINES", seed_dist = "all")
seedwall_fir = summarize_across_plots(data = seedwall, species = "FIRS", seed_dist = "all")
seedwall_cade = summarize_across_plots(data = seedwall, species = "CADE", seed_dist = "all")
seedwall_psme = summarize_across_plots(data = seedwall, species = "PSME", seed_dist = "all")
seedwall_ylwpines = summarize_across_plots(data = seedwall, species = "YLWPINES", seed_dist = "all")
seedwall_pila = summarize_across_plots(data = seedwall, species = "PILA", seed_dist = "all")

seedwall_grn = bind_rows(seedwall_all,
                          seedwall_pine,
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


## Need to make core plots not have records for seedwall cones
###!!! TODO: need to use datasheets to correct some core plots with seedwall cones recorded)
summarized[toupper(summarized$type) != "SEEDWALL" | is.nan(summarized$mean_underseedwall_cone_dens),] %<>%
  mutate(mean_underseedwall_cone_dens = NA)

write.csv(summarized,datadir("field-data/processed/summarized/summarized_densities.csv"))


### How much does the amount of brown cover affect it?

## Away from green

# For pines
core_nogrn_pine = core %>%
  filter(dist_grn_PINES > 100 | is.na(dist_grn_PINES))
  
ggplot(core_nogrn_pine,aes(x=PINES_predrop_cov_abs, y = seedl_dens_PINES)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "Overstory *scorched* pine cover (%)",
       y = "Pine seedling density (seedl/sq m)",
       title = "PINES: Green trees > 50 m away") +
  theme_bw()

# For yellow pines
core_nogrn_pine = core %>%
  filter(dist_grn_YLWPINES > 100 | is.na(dist_grn_YLWPINES))

ggplot(core_nogrn_pine,aes(x=YLWPINES_predrop_cov_abs, y = seedl_dens_YLWPINES)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "Overstory *scorched* yellow pine cover (%)",
       y = "Yellow pine seedling density (seedl/sq m)",
       title = "YELLOW PINES: Green trees > 50 m away") +
  theme_bw()

# For yellow pines: seedl/cone ratio
core_nogrn_pine = core %>%
  filter(dist_grn_YLWPINES > 100 | is.na(dist_grn_YLWPINES))

ggplot(core_nogrn_pine,aes(x=YLWPINES_predrop_cov_abs, y = seedl_dens_YLWPINES/cone_dens_YLWPINES)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "Overstory *scorched* yellow pine cover (%)",
       y = "Yellow pine seedling/cone ratio",
       title = "YELLOW PINES: Green trees > 50 m away") +
  theme_bw()

# For yellow pines: cone density
core_nogrn_pine = core %>%
  filter(dist_grn_YLWPINES > 100 | is.na(dist_grn_YLWPINES))

core_nogrn_pine = core_nogrn_pine %>%
  mutate(cone_dens_YLWPINES = ifelse(cone_dens_YLWPINES == 0, 0.0015,cone_dens_YLWPINES),
         seedl_dens_YLWPINES = ifelse(seedl_dens_YLWPINES == 0, 0.0015,seedl_dens_YLWPINES))

ggplot(core_nogrn_pine,aes(x=log(cone_dens_YLWPINES), y = log(seedl_dens_YLWPINES))) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "log Yellow pine cone density (cones / sq m)",
       y = "log Yellow pine seedling seedling density (seedl / sq m)",
       title = "YELLOW PINES: Green trees > 50 m away") +
  theme_bw()

# For PINES: cone density
core_nogrn_pine = core %>%
  filter(dist_grn_PINES > 100 | is.na(dist_grn_PINES))

ggplot(core_nogrn_pine,aes(x=log(cone_dens_PINES), y = log(seedl_dens_PINES))) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "Pine cone density (cones / sq m)",
       y = "Pine seedling seedling density (seedl / sq m)",
       title = "PINES: Green trees > 50 m away") +
  theme_bw()

# For PSME: cone density
core_nogrn_pine = core %>%
  filter(dist_grn_PSME > 100 | is.na(dist_grn_PSME))

ggplot(core_nogrn_pine,aes(x=cone_dens_PSME, y = seedl_dens_PSME)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "Yellow pine cone density (cones / sq m)",
       y = "Yellow pine seedling seedling density (seedl / sq m)",
       title = "PSME: Green trees > 50 m away") +
  theme_bw()

# For yellow pine needle volume
core_nogrn_pine = core %>%
  filter(dist_grn_YLWPINES > 100 | is.na(dist_grn_YLWPINES))

ggplot(core_nogrn_pine,aes(x=YLWPINES_predrop_vol_abs, y = seedl_dens_YLWPINES)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "Overstory yellow pine scorched needle voumne (%)",
       y = "Yellow pine seedling density (seedl/sq m)",
       title = "YELLOW PINES: Green trees > 50 m away") +
  theme_bw()

# For sugar pines
core_nogrn_pine = core %>%
  filter(dist_grn_PILA > 100 | is.na(dist_grn_PILA))

ggplot(core_nogrn_pine,aes(x=PILA_predrop_cov_abs, y = seedl_dens_PILA)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "Overstory *scorched* sugar pine cover (%)",
       y = "Sugar pine seedling density (seedl/sq m)",
       title = "SUGAR PINES: Green trees > 50 m away") +
  theme_bw()

# For PSME
core_nogrn_pine = core %>%
  filter(dist_grn_PSME > 100 | is.na(dist_grn_PSME))

ggplot(core_nogrn_pine,aes(x=PSME_predrop_cov_abs, y = seedl_dens_PSME)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "Overstory *scorched* PSME cover (%)",
       y = "PSME seedling density (seedl/sq m)",
       title = "PSME: Green trees > 50 m away") +
  theme_bw()

# For firs
core_nogrn_fir = core %>%
  filter(dist_grn_FIRS > 100 | is.na(dist_grn_FIRS))

ggplot(core_nogrn_fir,aes(x=FIRS_predrop_cov_abs, y = seedl_dens_FIRS)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "Overstory *scorched* fir cover (%)",
       y = "Fir seedling density (seedl/sq m)",
       title = "FIRS: Green trees > 50 m away") +
  theme_bw()

# For abies
core_nogrn_fir = core %>%
  filter(dist_grn_ABIES > 100 | is.na(dist_grn_ABIES))

ggplot(core_nogrn_fir,aes(x=ABIES_predrop_cov_abs, y = seedl_dens_ABIES)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x = "Overstory *scorched* Abies cover (%)",
       y = "Abies seedling density (seedl/sq m)",
       title = "ABIES: Green trees > 50 m away") +
  theme_bw()


##


#### Plots of the range of canopy conditions we sampled. Need to assemble data separately for each species


# assemble for pines

spp = c("PINES","YLWPINES","PILA","FIRS","ABIES","PSME","ALL","CADE")


get_species_covers = function(data, sp, type) {

  dist_grn_var = paste0("dist_grn_",sp)
  predrop_vol_abs_var = paste0(sp,"_predrop_vol_abs")
  green_vol_abs_var = paste0(sp,"_green_vol_abs")
  seedl_dens_var = paste0("seedl_dens_",sp)
  
  if(toupper(type) == "SEEDWALL") {
    predrop_vol_abs_var = "vol_brn_50m"
    green_vol_abs_var = "vol_grn_50m"
    dist_grn_var = "Seedwall_Dist"
  }
  
  d_foc = d %>%
    mutate(vol_brn_50m = vol_brn_50m/100,
           vol_grn_50m = vol_grn_50m/100) %>%
    filter(toupper(plot_type) == toupper(type)) %>%
    select(fire, plot_id, plot_type, scorched_canopy_volume = {{predrop_vol_abs_var}}, green_canopy_volume = {{green_vol_abs_var}}, seedl_dens = {{seedl_dens_var}},
           dist_to_green = {{dist_grn_var}},
           date) %>%
    mutate(species = sp)
  
  return(d_foc)

}

###!!! Need to make the distance to seedwall column
d = d %>%
  mutate(Seedwall_Dist = str_replace(Seedwall_Dist, ">","") %>% as.numeric)

d_species_covers_core = map_df(spp,get_species_covers,data=d, type = "CORE")
d_species_covers_seedwall = map_df(spp,get_species_covers,data=d, type = "SEEDWALL")

d_species_covers = bind_rows(d_species_covers_core,d_species_covers_seedwall) %>%
  mutate(plot_type = toupper(plot_type)) %>%
  mutate(fire_plottype = paste0(fire,"-", plot_type))

# specify which plots were the shasta-trinity ones
d_species_covers$forest = "Other"
d_species_covers$date = as.character(d_species_covers$date)
d_species_covers[d_species_covers$date %in% c("2021-07-17","2021-07-18","2021-07-19","2021-07-20"),]$forest = "Shasta-Trinity"

##!! make same plots for seedling-cone ratio


ggplot(d_species_covers, aes(x=scorched_canopy_volume,y=green_canopy_volume,color=seedl_dens > 0, shape=forest)) +
  geom_point(size=2) +
  facet_grid(fire_plottype~species) +
  theme_bw(15) +
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1)) +
  scale_color_viridis_d(begin = .2, end = .8)

# with the y axis distance to green
ggplot(d_species_covers, aes(x=scorched_canopy_volume,y=dist_to_green,color=seedl_dens > 0, shape=forest)) +
  geom_point(size=2) +
  facet_grid(fire_plottype~species) +
  theme_bw(15) +
  theme(axis.text.x = element_text(angle=45,vjust=1,hjust=1)) +
  scale_color_viridis_d(begin = .2, end = .8)



####!!!! Analysis next steps:

# needle cover







### pull in burn date to see if it matters???



## for seed wall: see if seedling:cone ratio is higher. Include % green or some metrics of seed wall density, or species comp

### Qs for planning revisits:
## How important to know the seedlings survived the first year?
## Can we add some plots that are only surveyed in Yr 2? Will have dropped more needles. So, probably need to follow up on the Yr 1 ones to be able to back-estimate the brown canopy.

### For scouting:
## look for delayed mortality to make sure it happened. Because it if didn't, not worth sending the crew there yet (right??)
