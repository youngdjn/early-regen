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



#### Core plots ####

# filter to core plots
unique(d$plot_type)
core = d %>%
  filter(toupper(plot_type) == "CORE")

# what's the range of distance to green?
hist(core$dist_grn_ALL)

# for pines, firs, cade: get plots > 100 m from green seed source and get the proportion that had seedlings

# pines
core_nogrn_pine = core %>%
  filter(dist_grn_PINES > 100 | is.na(dist_grn_PINES)) %>%
  mutate(seedl_present = seedl_dens_PINES > 0) %>%
  summarize(prop_recruitment = sum(seedl_present)/n(),
            n_plots = n(),
            mean_seedl_dens = mean(seedl_dens_PINES),
            mean_seedl_dens_present = mean(seedl_dens_PINES[seedl_dens_PINES > 0]),
            # proportion of plots by fire
            prop_recruitment_august = sum(seedl_present & (fire == "August")) / sum(fire == "August"),
            prop_recruitment_creek = sum(seedl_present & (fire == "Creek")) / sum(fire == "Creek"),
            prop_recruitment_north = sum(seedl_present & (fire == "North")) / sum(fire == "North"),
            )

# firs
core_nogrn_firs = core %>%
  filter(dist_grn_FIRS > 100 | is.na(dist_grn_FIRS)) %>%
  mutate(seedl_present = seedl_dens_FIRS > 0) %>%
  summarize(prop_recruitment = sum(seedl_present)/n(),
            n_plots = n(),
            mean_seedl_dens = mean(seedl_dens_FIRS),
            mean_seedl_dens_present = mean(seedl_dens_FIRS[seedl_dens_FIRS > 0]),
            # proportion of plots by fire
            prop_recruitment_august = sum(seedl_present & (fire == "August")) / sum(fire == "August"),
            prop_recruitment_creek = sum(seedl_present & (fire == "Creek")) / sum(fire == "Creek"),
            prop_recruitment_north = sum(seedl_present & (fire == "North")) / sum(fire == "North"),
  )


# cade
core_nogrn_cade = core %>%
  filter(dist_grn_CADE > 100 | is.na(dist_grn_CADE)) %>%
  mutate(seedl_present = seedl_dens_CADE > 0) %>%
  summarize(prop_recruitment = sum(seedl_present)/n(),
            n_plots = n(),
            mean_seedl_dens = mean(seedl_dens_CADE),
            mean_seedl_dens_present = mean(seedl_dens_CADE[seedl_dens_CADE > 0]),
            # proportion of plots by fire
            prop_recruitment_august = sum(seedl_present & (fire == "August")) / sum(fire == "August"),
            prop_recruitment_creek = sum(seedl_present & (fire == "Creek")) / sum(fire == "Creek"),
            prop_recruitment_north = sum(seedl_present & (fire == "North")) / sum(fire == "North"),
  )


## Now for ones close to green

# pines
core_grn_pine = core %>%
  filter(dist_grn_PINES < 50) %>%
  mutate(seedl_present = seedl_dens_PINES > 0) %>%
  summarize(prop_recruitment = sum(seedl_present)/n(),
            n_plots = n(),
            mean_seedl_dens = mean(seedl_dens_PINES),
            mean_seedl_dens_present = mean(seedl_dens_PINES[seedl_dens_PINES > 0]),
            # proportion of plots by fire
            prop_recruitment_august = sum(seedl_present & (fire == "August")) / sum(fire == "August"),
            prop_recruitment_creek = sum(seedl_present & (fire == "Creek")) / sum(fire == "Creek"),
            prop_recruitment_north = sum(seedl_present & (fire == "North")) / sum(fire == "North"),
  )

# firs
core_grn_firs = core %>%
  filter(dist_grn_FIRS < 50) %>%
  mutate(seedl_present = seedl_dens_FIRS > 0) %>%
  summarize(prop_recruitment = sum(seedl_present)/n(),
            n_plots = n(),
            mean_seedl_dens = mean(seedl_dens_FIRS),
            mean_seedl_dens_present = mean(seedl_dens_FIRS[seedl_dens_FIRS > 0]),
            # proportion of plots by fire
            prop_recruitment_august = sum(seedl_present & (fire == "August")) / sum(fire == "August"),
            prop_recruitment_creek = sum(seedl_present & (fire == "Creek")) / sum(fire == "Creek"),
            prop_recruitment_north = sum(seedl_present & (fire == "North")) / sum(fire == "North"),
  )


# cade
core_grn_cade = core %>%
  filter(dist_grn_CADE < 50) %>%
  mutate(seedl_present = seedl_dens_CADE > 0) %>%
  summarize(prop_recruitment = sum(seedl_present)/n(),
            n_plots = n(),
            mean_seedl_dens = mean(seedl_dens_CADE),
            mean_seedl_dens_present = mean(seedl_dens_CADE[seedl_dens_CADE > 0]),
            # proportion of plots by fire
            prop_recruitment_august = sum(seedl_present & (fire == "August")) / sum(fire == "August"),
            prop_recruitment_creek = sum(seedl_present & (fire == "Creek")) / sum(fire == "Creek"),
            prop_recruitment_north = sum(seedl_present & (fire == "North")) / sum(fire == "North"),
  )




## add mean cone densities to those tables ^
## then repeat it all for seed wall







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


## Qs for planning revisits: how important to know the seedlings survived the first year?


## Can we add some plots that are only surveyed in Yr 2? For that, probably need to follow up on the Yr 1 ones.


## For scouting: look for delayed mortality to make sure it happened. Because it if didn't, not worth sending the crew there yet (right??)
