# Purpose: Prepare manually entered plot data for analysis

library(tidyverse)
library(here)
library(sf)
library(readxl)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


#### Load plot data ####

plots = read_excel(datadir("field-data/dispersal-data-entry-2021.xlsx")) %>%
  filter(! plot_id %in% c("S100-1","S100-2")) # these were entered twice, but with different names (one set with dash, one set without)



#### Load plot coords ####

files = list.files(datadir("emlid-plot-coords"),full.names=TRUE)

coords = data.frame()

for(file in files) {
  d = read_csv(file) %>%
    mutate(Name = as.character(Name))
  coords = bind_rows(coords,d)
}

# Simmplify and remove duplicates
coords = coords %>%
  select(Name,Easting,Northing,Elevation,`Lateral RMS`,time  = `Averaging start`) %>%
  unique()

# See if any points were taken more than one time
duplicated(coords$Name)
# no


coords = coords %>%
  # correct some plot IDs to match gps IDs
  mutate(Name = recode(Name,
                       "C001-001" = "C001001",
                       "C32A" = "C032A",
                       "C32B" = "C032B",
                       "C34B" = "C034B",
                       "C051" = "C51",
                       "C0626" = "C062-6",
                       "C0627" = "C062-7",
                       "C0628" = "C062-8",
                       "C0629" = "C062-9",
                       "S03" = "S003",
                       "SO7A" = "S07A",
                       "SO7B" = "S07B",
                       "S43B" = "S043B",
                       "C73A" = "S73A",
                       "C73B" = "S73B",
                       "S036087" = "S086087",
                       "S036088" = "S086088"))


# Pull coords into plot data
plots = left_join(plots,coords,by=c("plot_id" = "Name"))
















## If 



### Question 1:

# CORE AREA
# In core area, what plots had conifer regen?
# In core area, how did regen relate to % green and % brown?
# In core area, how did regen relate to cone count?
# Version of is with plots > 100 m from green trees
# Consider adding species comp of green and brown
# 
#   
# SEED WALL
# In seed wall area, what plots had seedlings?
# How did seedling density relate to cone density?
# How did seedling density relate to cone density within the seed wall?


# To answer this, need:
# Plot coords
# Plot:
# Total seedlings, seedlings by species
# 50 m volume: % green, % brown
# 50 m cover (vol * predrop cov): % green, % brown
# Cones by species
# Seed source: minimum of all options (drop ">")




  
  
  
  