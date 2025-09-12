# Purpose: Prepare manually entered plot data for analysis

##!! TODO: make sure that scorched needle volume by species is being computed correctly

library(tidyverse)
library(here)
library(sf)
library(readxl)
library(exifr)
library(magrittr)
library(terra)
library(googlesheets4)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


#### Load plot data and correct some entry errors ####


plots = read_excel(datadir("field-data/raw/dispersal-data-entry-2023.xlsx"),sheet="plot_main") %>%
  mutate(date = as.character(as.Date(as.numeric(date), origin = "1899-12-30"))) %>% # convert the Excel date to a proper date in YYYY-MM-DD format
  filter(! plot_id %in% c("S100-1","S100-2")) %>% # these two plots were entered twice, but with different names (one set with dash, one set without, so remove the one with)
  filter(!(plot_id=="S027094" & entered_by == "Diego")) |> # this was entered twice by two differnt people
  # drop plots that were not surveyed (no camrea recorded)
  filter(!is.na(camera)) # there were some rows for "unsurveyed  plots" but we can just remove them. The camera field is always blank for them so filter with that column.
  
plots[plots$plot_id == "T009" & plots$Year == 2021, "date"] = "2021-07-02" # incorrectly recorded date

## incorrectly recorded plot coordinates
plots[plots$plot_id == "C12-007" & plots$Year == 2021, "lon"] = -121.28285
plots[plots$plot_id == "S032-545" & plots$Year == 2022, "lat"] = 38.58434
# the plot below seems to have been created by accidentally renaming an existing waypoint from the Creek Fire, so I pulled the coords from the photos instead.
plots[plots$plot_id == "C041-500" & plots$Year == 2022, "lat"] = 38.622276
plots[plots$plot_id == "C041-500" & plots$Year == 2022, "lon"] = -120.521693

## One plot was erroneously recorded as "core" even though it's seedwall
plots[plots$plot_id == "S02-282" & plots$Year == 2022, "plot_type"] = "seedwall"

## One seed wall plot had sight line listed as "missing" but distance to a dense seedwall was < 30 m so put sight line as 30 m
plots[plots$plot_id == "S039-157" & plots$Year == 2022, "sight_line"] = "30"

## One plot in 2023 was recorded as S039-156/158 because both of those plots in 2022 had the same coords recorded, meaning they were erroneous for one of the two. The 2023 data revealed that the coords were for plots S039-156 because it previously had some green and brown needles while the other didn't, and in 2023 it had some green and brown.
plots[plots$plot_id == "S039-156/158" & plots$Year == 2023, "plot_id"] = "S039-156"

## 2023 plot C01-266 had a date of "MISSING" (NA after date conversion) but we're assuming it was surveyed on 2023-08-21 because it is in a sequence of pltos surveyed on that date (or 2023-08-20)
plots[plots$plot_id == "C01-266" & plots$Year == 2023, "date"] = "2023-08-21"

#$$$$$$$$$$$$ added some conversions
plots = plots |>
    mutate(plot_id = recode(plot_id,
                            "209-250" = "S09-250",
                            "209-253" = "S09-253",
                            "204-279" = "S04-279",))


# TODO: use the photo coords from 2022 to get the correct coords for S039-158.

## NOTE that Derek manually corrected the entered data gsheet for plot D014-232 because there were two plots wtih this name. The second occurrence of this plot name was changed to D014-932

#### Load plot coords ####

#### First from Emlid for 2021 plots

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
  # correct some gps IDs to match plot IDs
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



#### For plots that don't have coords, get coords from the photo EXIF, but only for plots surveyed in 2021

plots$photo_lat = NA
plots$photo_lon = NA

for(i in 1:nrow(plots)) {
  
  plot = plots[i,]
  
  if(plot$date > "2022-01-01") next() # if this is for a 2022 plot or later, skip
  
  if(!is.na(plot$Easting)) next() # if coords are already there, skip
  
  # get the image directory to look in
  dir = paste0("early-regen-photos-2021-phone",toupper(plot$camera))
  
  # get the name of the photo to look for
  # use the most recent name (highest number)
  date = plot$date %>% str_replace_all("-","")
  
  time = max(c(plot$photo_fisheye,plot$photo_needle,plot$photo_plotcanopy,plot$photo_tripod,plot$photo_upward), na.rm=TRUE)
  time = time %>% as.character %>% as.numeric %>% as.character %>% str_pad(width=6,side="left",pad="0")
  
  photo = paste0(date,"_",time)
  
  
  # manually enter some photo locs looked up from google photos that weren't stored in EXIF
  if(photo == "20210729_160437") {
    plots[i,"photo_lat"] = 37.2607601
    plots[i,"photo_lon"] = -119.3777943
  }
  
  # look for the file
  file = list.files(path=datadir(paste0("field-photos/",dir)), pattern=photo, full.names=TRUE)
  
  # get the EXIF for that photo
  exif = read_exif(file,tags=c("GPSLatitude","GPSLongitude"))
  #write.csv(exif,datadir("temp/coords.csv"))
  if(! "GPSLatitude" %in% names(exif)) next() # no coords stored for that photo
  
  plots[i,"photo_lat"] = exif$GPSLatitude
  plots[i,"photo_lon"] = exif$GPSLongitude
  
  
}

## create a compiled lat/long column for 2021 plots (from Emlid if present, otherwise photo EXIF) and a column indicating if coords are from photo EXIF or from Emlid
# The exported Emlid coords use Northing and Easting
plots = plots %>%
  mutate(pre_lat = ifelse(!is.na(Northing),Northing,photo_lat),
         pre_lon = ifelse(!is.na(Easting),Easting,photo_lon),
         coords_from_photo = is.na(Northing) | is.na(Easting))

## merge lat/lon from above with lat/lon entered on datasheets (from Garmin GPS)
# lat/lon were fields on the datasheet starting in 2022, so if they were NA, that means they were 2021 plots
plots = plots %>%
  mutate(coords_from_gps = is.na(lat) | is.na(lon), #record that coords were from GPS
         lat = ifelse(is.na(lat) & Year < 2023,pre_lat,lat),
         lon = ifelse(is.na(lon) & Year < 2023,pre_lon,lon))

## Assign 2022 plots the lat/lon of the 2023 plots (if present) (since the crew updated coords for a few plots that were off), then assign any missing 2023 plot coords (most of them) the 2022 coords
plots_2023 = plots |>
  filter(Year == 2023) |>
  pull(plot_id)

for(plot in plots_2023) {
  lat = plots[plots$plot_id == plot & plots$Year == 2023, "lat"]
  lon = plots[plots$plot_id == plot & plots$Year == 2023, "lon"]
  
  if(!is.na(lat) & !is.na(lon)) {

    plots[plots$plot_id == plot & plots$Year == 2022,"lat"] = lat
    plots[plots$plot_id == plot & plots$Year == 2022,"lon"] = lon
  
  } else {
    
    row = plots[plots$plot_id == plot & plots$Year == 2022, ]
    
    if(nrow(row) == 0) next() # The plot was not surveyed in 2022
    
    lat = row$lat
    lon = row$lon
    
    plots[plots$plot_id == plot & plots$Year == 2023,"lat"] = lat
    plots[plots$plot_id == plot & plots$Year == 2023,"lon"] = lon
    
  }
}

## TODO: Why was plot C035-511 only surveyed in 2023 and not 2022? Typo in data entry of plot ID?


####!!!! Temporary! Keep only 2023 surveyed plots, Caldor and Dixie only

plots = plots |>
  filter(date > "2023-01-01") |>
  filter(Fire %in% c("Caldor", "Dixie"))

# Make sure there are no duplicated plots
plots_duplicated = plots |>
  mutate(duplicated = duplicated(plot_id)) |>
  select(plot_id, duplicated)



#### Summarize data in ways useful for research questions ####

### Question 1:

# CORE AREA
# In core area, what plots had conifer regen?
# In core area, how did regen relate to % green and % brown?
# In core area, how did regen relate to cone count?
# Version of this with plots > 100 m from green trees
# Consider adding species comp of green and brown
#   
# SEED WALL
# In seed wall area, what plots had seedlings?
# How did seedling density relate to cone density?
# How did seedling density relate to cone density within the seed wall?


# To answer this, need:
# Plot coords
# Plot-level:
# Total seedlings, seedlings by species
# 50 m volume: % green, % brown
# NOTE that we stopped collecting untorched ("pre-drop") cover partway through 2022 because we realized that the 2021 crew had been collecting it wrong, by recording this as a percentage of total pre-fire tree cover, not as an absolute percent cover
# Cones by species
# Seed source: minimum of all options (drop ">" which means there may be trees beyond this distance by they were not seen so we don't know)


#### Load seedling data

seedl = read_excel(datadir("field-data/raw/dispersal-data-entry-2023.xlsx"), sheet="seedls_cones", col_types = c("text")) %>%
  # correct some plot IDs that don't match the main plot table
  #$$$$$$$$$$$$ added some conversions
  mutate(plot_id = recode(plot_id,
                          "C062259" = "C062-259",
                          "C062332" = "C062-332",
                          "S012015" = "S102015",
                          "S0427094" = "S027094",
                          "S43B" = "S043B",
                          "C38" = "C28",
                          "S100-1" = "S1001",
                          "S035-551" = "C035-551",
                          "S005-534" = "S004-534",
                          "S039-156/158" = "S039-156",
                          "C003-14" = "C003-014",
                          "209-251" = "S09-251",
                          "209-253" = "S09-253"
                          )) |>
  select(1:13) |> # there were some extra mostly-empty columns I think with some notes, so keep only the relevant columns
  mutate(year = as.numeric(year))

## TEMPORARILY thin to 2023 data only
seedl = seedl |>
  filter(year == 2023)

#### TEMPORARY filtering to only Caldor and Dixie from 2022
# Get which fire the seedl are from so we can remove creek. first need to select only unique plotid-fire pairs.
plotid_fire = plots |>
  select(plot_id, Fire) |>
  unique() # this line would only become necessary when there are revisits in the plot dataset

# Pull in the fire name to the seedling data
seedl = left_join(seedl, plotid_fire)

#$$$$$$$$$$$$$ Get seedling plot IDs that don't match to the main plot table
seedl |>
  filter(is.na(Fire)) |>
  pull(plot_id) |>
  unique()
  
# Exclude Creek
seedl = seedl |>
  filter(Fire != "Creek")


#### Prep seedling data
####!!!! #TODO for 2021 data: For 2021 data, we recorded seedlings (and cones?) in categories like "100+" but then entered the data as "100". For analyzing the 2021 data, set "+" seedling/cone counts (round numbers at the floor of each bin) as the midpoint of the bin?? May also need set 2022 counts similarly to avoid bias.
####!!!! #TODO for 2021 data: When bringing in 2021 data, need to make sure that we match the observations to the correct years.


#$$$$$$$$$$$$$$$$$$$$$$$$$$ If a 'radius' or 'seedlings_0yr' entry has a "/" to indicate two radii,
#take the first value of each
seedl = seedl |>
  mutate(radius = str_split(radius, fixed("/")) |> map(1) |> unlist(),
         seedlings_0yr = str_split(seedlings_0yr, fixed("/")) |> map(1) |> unlist())






# Compute seedlings/sqm and cones/sqm, filter anomalously entered values
seedl = seedl %>%
  mutate(radius = ifelse(radius == "n/a" | radius == "MISSING", 8, radius), # if no radius entered, the default was 8 m
         cones_new = recode(cones_new,"50+" = "75","n/a"="0"), # 50+ was a notation from 2021, I think only used for one plot (generally when entering data, they omitted the + and any round number was assumed to represent the lower end of the density class assigned to the plot)
         cones_old = recode(cones_old,"50+" = "75","n/a"="0"), # 50+ was a notation from 2021, I think only used for one plot
         species = recode(species,"ANY" = "PIPJ"), # In 2021, sometimes (one plot?) they put species "ANY" and entered 0 as a record to show they surveyed the plot but found nothing. So change to an actual species so it gets processed properly (won't affect count because count is 0 whever "ANY" is used)
         seedwall_cones = recode(seedwall_cones, "No, too steep" = "na", "MISSING" = "na")) %>% # When they didn't record data for seed wall (I think just 2021), store as NA instead of 0. First store as "na" which will get set to real NA just below
  # Some 2022 seedl_0yr counts were recorded as 200+ and 400+. Recode as the midpoint of the categories (250 and 450).
  mutate(seedlings_0yr = recode(seedlings_0yr, "200+" = "250", "400+" = "450")) |>
  # no longer need this because we dealt with all the "+" properly (but confirm): mutate(across(c("seedlings_0yr", "seedlings_1yr", "caches", "cones_new", "cones_old", "seedwall_cones"), ~str_replace(., fixed("+"), ""))) |>
  # a plot radius listed as "Q" (always from 2022) means one quadrant of the smallest plot size (4 m). So give it the radius of a circular plot with equivalent area (2 m)
  mutate(radius = recode(radius, "Q" = "2", "q" = "2")) |>
  mutate(under_cones_new = recode(under_cones_new, "H" = "2", "L" = "1")) |> # Change letter coding for "high" and "low" to number levels
  mutate(under_cones_old = recode(under_cones_old, "H" = "2", "L" = "1")) |> # Change letter coding for "high" and "low" to number levels
    # #### FOR FINDING THE NON-NUMERIC VALUES (e.g., counts ending in "+"):
  # mutate(across(c("radius", "seedlings_0yr", "seedlings_1yr", "cones_new", "under_cones_new"), ~ifelse(is.na(.), "0", .))) |> # set all NAs to a number so that the next step will reveal which were non-numeric (because they'll get set to NAs) # TODO: If we eventually want to ask about cones-old and caches, we need to take care of this for those columns too.
  mutate(across(c("radius", "seedlings_0yr"), ~as.numeric(as.character(.)))) |> # Convert everything to numeric
  # 2022 plot S039-158 has a psme radius of 0 (also on data sheet) Assuming it's supposed to be 10.
  mutate(radius = ifelse(radius == .0, 10, radius)) |>
  # Some cones from 2022 were listed as "old" but this is a mistake because it is not possible for core area cones to be old
  # TODO for >=2nd yr analysis or if analyzing seed wall cone density where there can be older cones: need to make this more flexible to only fix core area cones from first year
  #      This is for both plot cones and under-tree cones
  # Compute densities
  mutate(seedl_dens = seedlings_0yr / (3.14*radius^2))



#$$$$$$$$$$$$$$$$$$$ Filter to only observationso of 0 yr seedl present
seedl = seedl |>
  filter(!is.na(seedlings_0yr)) |>
  filter(seedlings_0yr > 0)



####!!! TODO for 2021 data: 


# Set our placeholder "na" for seedwall cones to a true NA, make numeric and compute density
seedl[which(seedl$seedwall_cones == "na"),"seedwall_cones"] = NA
seedl = seedl %>%
  mutate(seedwall_cones = as.numeric(seedwall_cones),
         seedwall_cone_dens = seedwall_cones / (3.14*8^2)) # cones were always counted in the 8 m plots


# Get just the columns that are relevant, and remove irrelevant rows (like immature cones)
seedl_simp = seedl %>%
  select(plot_id,species,seedl_dens,radius) %>%
  # remove some types of cones that are not relevant
  filter(!(species %in% c("immature PIPO or PICO (open)"))) %>% # exclude because it is not a cone that dispersed seeds.
  # We are excluding stripped cones (only one recorded) from the cone count, with the logic that this fecundity was not relevant to regen
  filter(species != "EATEN PILA") %>%
  group_by(plot_id,species) %>%
  mutate(dup = duplicated(plot_id, species)) |>
  ## TODO for 2021: for processing revisit data (including 2022 revisits of Creek), here is where we will need to deal with the fact that some plots have two different counts for each species, for both radii
  summarize(across(everything(),~sum(.x,na.rm=TRUE)))


### Get seedling/cone tallies by species / species group  
# Compute total seedl and cones across all species, then by species groups, and bind all by columns into a table

seedl_tot = seedl_simp %>%
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                   ) %>%
  mutate(species = "ALL")

seedl_pines = seedl_simp %>%
  filter(species %in% c("PICO","PIJE","PILA","PILA/PIPJ","PIPJ","PIPJ/PILA","PIPO","PIxx","PIXX", "PI-", "PIMO")) %>% # many of the species ambiguities were recorded in 2021 data only
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                   ) %>%
  mutate(species = "PINES")
  
seedl_ylwpines = seedl_simp %>%
  filter(species %in% c("PIJE","PIPJ","PIPO")) %>%
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                   ) %>%
  mutate(species = "YLWPINES")

# "firs" including Douagls-fir
seedl_firs = seedl_simp %>%
  filter(species %in% c("ABCO","ABCO/ABMA","ABCO/PSME","PSME")) %>%  # many of the species ambiguities were recorded in 2021 data only
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                   ) %>%
  mutate(species = "FIRS")

# true firs
seedl_abies = seedl_simp %>%
  filter(species %in% c("ABCO","ABCO/ABMA", "ABMA")) %>% ##!! NOTE this drops one occurrence of abco/psme ambiguous seedlings (2021 only)
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                  ) %>%
  mutate(species = "ABIES")


# Get the species-specific observations: exclude ambiguous species IDs. This assumes the ambiguous species were not there. We should test the sensitivty to this assumption. For 2022 Dixie/Creek, there was only 1 plot with an ambiguous pine
seedl_simp = seedl_simp %>%
  filter(species %in% c("ABCO","CADE","PICO","PILA","PSME","PIPJ"))

seedl_comb = bind_rows(seedl_simp,seedl_tot,seedl_ylwpines,seedl_pines,seedl_firs, seedl_abies)

## Is there seedling data for every plot?
plot_ids = unique(plots$plot_id)
seedl_plot_ids = unique(seedl_simp$plot_id)
plots_no_seedlings = setdiff(plot_ids, seedl_plot_ids)
seedlings_no_plots = setdiff(seedl_plot_ids, plot_ids)


### TODO for 2021: Add a column for "this plot did not have seed wall cone density recorded", so we could optionally exclude those plots
### TODO in for 2021 and Creek: If would be useful in analysis, add a column for "plot has species ambiguity", so we could optionally exclude plots that have species ambiguity, rather than assuming 0 seedlings there. This may need to indicate what type(s) of species ambiguity because for some types we would not need to exclude the plot (e.g. PIJE/PILA if analysis is of PINES).


## Make seedling/cone table wide (a col for each species / species group)
# first make sure identifying cols are unique
a = duplicated(paste0(seedl_comb$plot_id,seedl_comb$species))
sum(a)

seedl_wide = pivot_wider(seedl_comb, id_cols = "plot_id", names_from = c("species"), values_from=c("seedl_dens","radius"))

seedl_wide = seedl_wide |>
  select(-seedl_dens_PIPJ,) # don't need these because we're using YLWPINES. But couldn't eliminate PIPJ earlier because radius is recorded specifically for PIPJ

# if a column is all NAs (only happens for radii for multi-species groups I think), remove it

seedl_wide = janitor::remove_empty(seedl_wide, which = "cols")


# Species counts that are NA are zero
seedl_wide = seedl_wide %>%
  mutate(across(everything(),~ ifelse(is.na(.x),0,.x)))


# The code commented out here is only needed for 2021 surveys:
#
# ### If a plot had zero seedwall cone records (even zeros) for ALL species, then make all its seedwall cone entries for all species and species groups NA
# ## The reason is that some plots may not have had a seedwall cone plot surveyed.
# ## TODO: see if there is a better way to determine whether it would have had a seedwall plot surveyed (e.g. all seedwall plots after a certain date?)
# ##!! NOTE current approach is likely dropping some true zeros (dropping all seedwall cone counts from pltos that had no seedwall cones recorded)
# 
# no_seedwall_cone_records = seedl %>%
#   select(plot_id,seedwall_cones) %>%
#   mutate(has_seedwall_cone_entry = !is.na(seedwall_cones)) %>%
#   group_by(plot_id) %>%
#   summarize(n_spp_w_seedwall_cone_entries = sum(has_seedwall_cone_entry)) %>%
#   filter(n_spp_w_seedwall_cone_entries == 0) %>%
#   pull(plot_id)
# 
# seedwall_cone_cols = grepl("seedwall_cone_dens",names(seedl_wide),)
# seedl_wide[seedl_wide$plot_id %in% no_seedwall_cone_records,seedwall_cone_cols] = NA

# TODO for 2021: The "under_cones_new" column from 2022 was similar to the "seedwall_cones" entry from 2021: possibly merge into a single column?


# Remove cone columns for species that don't produce cones
seedl_wide = seedl_wide %>%
  select(-contains("cone_dens_ABCO"),
         -contains("cone_dens_FIRS"),
         -contains("cone_dens_CADE"))

# Assign more meaningful names to the under-tree cone density categories (0, 1, 2) = (low, low, high)
seedl_wide = seedl_wide |>
  mutate(across(starts_with("under_cones_new_"), ~recode(paste0("level_", .), "level_0" = "low", "level_1" = "low", "level_2" = "high")))

## ^ with the above section, we now have cones and seedlings by species and species group!



#### Process seed sources ####

## Get the closest green tree overall and by species / species group (assuming there is a tree just beyond where they could see)
## TODO: assess sensitivity to assumption that there is a seed tree just beyond the ">" distance recorded

green_seedsource = read_excel(datadir("field-data/raw/dispersal-data-entry-2022.xlsx"),sheet="green_seedsource") %>%
  # correct some plot IDs that don't match the main plot table
  mutate(plot_id = recode(plot_id,
                          "C062259" = "C062-259",
                          "C062332" = "C062-332",
                          "C38" = "C28",
                          "S035-551" = "C035-551"
  )) |>
  filter(!is.na(plot_id))

## NOTE: In the data entry sheet for this table, DY manually removed a second identical instance of entered data for C038-229. It came right after the entry for C038-226.


## TEMPORARILY filter to 2022 data only, exclude Creek
green_seedsource = green_seedsource |>
  filter(year == 2022)

green_seedsource = left_join(green_seedsource, plotid_fire)
green_seedsource = green_seedsource |>
  filter(Fire %in% c("Caldor", "Dixie")) |>
  select(-Fire)

## Any duplicated? If so, need to fix because probably means incorrectly entered.
inspect = green_seedsource |>
  filter(metric == "distance",
         cluster_id == 1)

inspect = inspect |>
  mutate(duplicated = duplicated(inspect$plot_id))

## Is there a green seed source entry for every plot? To make sure properly entered
plot_ids = unique(plots$plot_id)
grnsrc_plot_ids = unique(green_seedsource$plot_id)

plots_no_grnsrc = setdiff(plot_ids, grnsrc_plot_ids) # yes, there were many plots for which green seedsource was not entered -- plots with no green visible. That's ok.
grnsrc_no_plot = setdiff(grnsrc_plot_ids, plot_ids)

dist_grn = green_seedsource %>%
  select(-`(add cols for other spp)`,-`Notes`) %>% # remove some data entry columns
  filter(!is.na(plot_id),
         metric == "distance") %>% # remove empty rows and remove the "number in cluster" (ntrees) rows
  mutate(across(any:`abco/abma`, ~ str_replace(.x,fixed(">"),""))) %>% # remove the ">" from distances (although this doesn't actually exist for 2022)
  # TODO for 2021: review the meaning of ">" for seed source distances and determine if we need to make a column noting whether there was a >. For 2022, it is taken care of with the column "sight_line". Review whether we can do the same thing for 2021. Perhaps we could use the ">" for 2021 to indicate the sight line, and use that as we use the explicit "sight_line" attribute from 2022.
  mutate(across(any:`abco/abma`, ~ ifelse(.x == "MISSING",NA,.x))) %>% # change "MISSING" to NA because that's OK for the one place it appears (which was in 2021) because on that plot they recorded distance to any species. TODO for 2021: make sure that this logic makes sense.
  select(-metric) %>%
  # within a plot, across seed source cluster IDs, get the closest cluster
  mutate(across(c(-year, -plot_id, -cluster_id), as.numeric)) |>
  group_by(plot_id) %>%
  summarize(across(-cluster_id, ~ min(.x,na.rm=TRUE)))

# TODO for more detailed analysis: compute some metric of seed source density & distance that composites both, using all clusters of trees seen. Currenly only using the closest cluster.

# Make composite-species columns (same as for seedlings). Still this is only recording the minimum distance to any trees in the species group. TODO for more detailed analysis: use a metric of seed source density & distance.
dist_grn = dist_grn %>%
  mutate(ALL = pmin(any,pipj,pila,psme,abco,cade,`abco/abma`, na.rm=TRUE),
         YLWPINES = pipj,
         PINES = pmin(pipj,pila, na.rm=TRUE),
         FIRS = pmin(psme,abco,`abco/abma`,na.rm=TRUE),
         ABIES = pmin(abco,`abco/abma`, na.rm=TRUE)) %>%
  select(-`abco/abma`, -any) |> ###!!! NOTE for 2021 analysis: we are dropping seed trees that were abco/abma ambiguous They are already incorporated into FIRS and ABIES. For analysis, need to keep in mind that can't use these plots for species-specific ABCO analysis. Or just do all analysis for ABIES. Potentially make a new column that flags whether the trees were ABCO/ABMA ambiguous if it would be useful to optionally exclude those plots. But might not be if always lumping to ABIES.
  # Inf can mean there were no trees observed for that species group (as the`summarize` step in the previous pipeline can return Inf if there were no trees in that species category at all), so set to NA
  mutate(across(c(-plot_id,-year), ~ifelse(. == Inf, NA, .)))
  
names(dist_grn) = toupper(names(dist_grn))  
names(dist_grn)[names(dist_grn) == "PLOT_ID"] = "plot_id"
names(dist_grn)[names(dist_grn) == "YEAR"] = "year"

#prepend "dist_grn_" to the seed dist cols
names(dist_grn)[!(names(dist_grn) %in% c("plot_id", "year"))] = paste0("dist_grn_",names(dist_grn)[!(names(dist_grn) %in% c("plot_id", "year"))])



#### Pull in seedlings/cones and dist_grn into the plot data

plots_seedl = left_join(plots,seedl_wide)
# rows with NA for seedl and cone are true zeros

# were there any seedl_cone records that didn't match plots?
matched = (seedl_wide$plot_id) %in% plots_seedl$plot_id
not_matched = seedl_wide[!matched,"plot_id"]


plots_complete = left_join(plots_seedl,dist_grn |> select(-year))
# lots of rows with NA for dist_grn that simply didn't have any grn recorded (bc not visible)

# Were there any dist_grn records that didn't match plots?
matched = (dist_grn$plot_id) %in% plots_seedl$plot_id
not_matched = dist_grn[!matched,"plot_id"]

## Make sure no plots are duplicated
any(duplicated(plots_complete$plot_id))

#$$$$$$$$$ temp for one-off
write_csv(plots_complete, "~/Downloads/plot_data_2023.csv")

