# Purpose: Prepare manually entered plot data for analysis

##!! TODO: make sure that scorched needle volume by species is being computed correctly

library(tidyverse)
library(here)
library(sf)
library(readxl)
library(exifr)
library(magrittr)
library(terra)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


#### Load plot data and correct some entry errors ####

plots = read_excel(datadir("field-data/raw/dispersal-data-entry-2022.xlsx"),sheet="plot_main") %>%
  mutate(date = as.character(date)) %>%
  filter(! plot_id %in% c("S100-1","S100-2")) %>% # these two plots were entered twice, but with different names (one set with dash, one set without, so remove the one with)
  filter(!(plot_id=="S027094" & entered_by == "Diego")) |> # this was entered twice by two differnt people
  # drop plots that were not surveyed (no camrea recorded)
  filter(!is.na(camera)) # there were some rows for "unsurveyed  plots" but we can just remove them. The camera field is always blank for them so filter with that column.
  
plots[plots$plot_id == "T009", "date"] = "2021-07-02" # incorrectly recorded date

## incorrectly recorded plot coordinates
plots[plots$plot_id == "C12-007", "lon"] = -121.28285
plots[plots$plot_id == "S032-545", "lat"] = 38.58434
# the plot below seems to have been created by accidentally renaming an existing waypoint from the Creek Fire, so I pulled the coords from the photos instead.
plots[plots$plot_id == "C041-500", "lat"] = 38.622276
plots[plots$plot_id == "C041-500", "lon"] = -120.521693

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

for(i in 1:nrow(plots)) {
  
  plot = plots[i,]
  
  if(plot$date > "2022-01-01") next() # if this is for a 2022 plot or later, skip
  
  if(!is.na(plot$Easting)) next() # if coords are already there, skip
  
  # get the image directory to look in
  dir = paste0("early-regen-photos-2021-phone",toupper(plot$camera))
  
  # get the name of the photo to look for
  # use the most recent name (highest number)
  date = plot$date %>% str_replace_all("-","")
  
  time = max(c(plot$photo_fisheye,plot$photo_needle,plot$photo_plotcanopy,plot$photo_tripod,plot$photo_updard), na.rm=TRUE)
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
         lat = ifelse(is.na(lat),pre_lat,lat),
         lon = ifelse(is.na(lon),pre_lon,lon))


####!!!! Temporary! Keep only 2022 surveyed plots. When we add 2021 plots, we will need to keep in mind that some 2022 plots were resurveys of 2021 plots and have the same plot name.
#           We will also need to make sure all the data prep steps below work properly on the full dataset

plots = plots |>
  filter(date > "2022-01-01")

# Make sure there are no duplicated plots
plots_duplicated = plots |>
  mutate(duplicated = duplicated(plot_id)) |>
  select(plot_id, duplicated)


#### Prep prefire prop by species (only collected 2022). This is a series of fields that have the estimated proportion of prefire BA by species (one field per species)
# Set it to 0 if it is NA (because NAs mean 0)
# One plot did not have this entered. Need this to remain as NA so this plot gets omitted from analyses that require this data
plots = plots |>
  mutate(across(starts_with("prefire_prop_"), ~ifelse(is.na(.x), "0", .x))) |> # make NAs zer0s
  mutate(across(starts_with("prefire_prop_"), ~ifelse(.x == "MISSING", NA, .x))) |> # make the MISSING fields NAs
  mutate(across(starts_with("prefire_prop_"), as.numeric))         
  
# Make sure these fields look good
inspect = plots |>
  select(plot_id, starts_with("prefire_prop_"))


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

seedl = read_excel(datadir("field-data/raw/dispersal-data-entry-2022.xlsx"), sheet="seedls_cones", col_types = c("text")) %>%
  # correct some plot IDs that don't match the main plot table
  mutate(plot_id = recode(plot_id,
                          "C062259" = "C062-259",
                          "C062332" = "C062-332",
                          "S012015" = "S102015",
                          "S0427094" = "S027094",
                          "S43B" = "S043B",
                          "C38" = "C28",
                          "S100-1" = "S1001",
                          "S035-551" = "C035-551"
                          )) |>
  select(1:13) |> # there were some extra mostly-empty columns I think with some notes, so keep only the relevant columns
  mutate(year = as.numeric(year))

## TEMPORARILY thin to 2022 data only
seedl = seedl |>
  filter(year == 2022)


#### Prep seedling data
####!!!! #TODO: For 2021 data, we recorded seedlings (and cones?) in categories like "100+" but then entered the data as "100". For analyzing the 2021 data, set "+" seedling/cone counts (round numbers at the floor of each bin) as the midpoint of the bin?? May also need set 2022 counts similarly to avoid bias.
####!!!! #TODO: When bringing in 2021 data, need to make sure that we match the observations to the correct years.
####!!!! TODO: Why does 2022 plot S039-158 have a psme radius of 0? Assuming below it's supposed to be 10. Need to check

## CAVEAT Have to assume the two instances of calling cones_new "H" was about 15 cones (for new cones within the plot)
seedl[which(seedl$cones_new == "H"),"cones_new"] = "15"


# Compute seedlings/sqm and cones/sqm, filter anomalously entered values
seedl = seedl %>%
  mutate(radius = ifelse(radius == "n/a" | radius == "MISSING", 8, radius), # if no radius entered, the default was 8 m
         cones_new = recode(cones_new,"50+" = "75","n/a"="0"), # 50+ was a notation from 2021, I think only used for one plot
         cones_old = recode(cones_old,"50+" = "50","n/a"="0"), # 50+ was a notation from 2021, I think only for one plot
         species = recode(species,"ANY" = "PIPJ"), # In 2021, sometimes (one plot?) they put species "ANY" and entered 0 as a record to show they surveyed the plot but found nothing. So change to an actual species so it gets processed properly (won't affect count because count is 0 whever "ANY" is used)
         seedwall_cones = recode(seedwall_cones, "No, too steep" = "na", "MISSING" = "na")) %>% # When they didn't record data for seed wall (I think just 2021), store as NA instead of 0. First store as "na" which will get set to real NA just below
  # Some 2022 seedl_0yr counts were recorded as 200+ and 400+. Recode as the midpoint of the categories (250 and 450).
  mutate(seedlings_0yr = recode(seedlings_0yr, "200+" = "250", "400+" = "450")) |>
  # no longer need this because we dealt with all the "+" properly (but confirm): mutate(across(c("seedlings_0yr", "seedlings_1yr", "caches", "cones_new", "cones_old", "seedwall_cones"), ~str_replace(., fixed("+"), ""))) |>
  # a plot radius listed as "Q" (always from 2022) means one quadrant of the smallest plot size (4 m). So give it the radius of a circular plot with equivalent area (2 m)
  mutate(radius = recode(radius, "Q" = "2", "q" = "2")) |>
  mutate(under_cones_new = recode(under_cones_new, "H" = "2", "L" = "1")) |> # Change letter coding for "high" and "low" to number levels
  # #### FOR FINDING THE NON-NUMERIC VALUES (e.g., counts ending in "+"):
  # mutate(across(c("radius", "seedlings_0yr", "seedlings_1yr", "cones_new", "under_cones_new"), ~ifelse(is.na(.), "0", .))) |> # set all NAs to a number so that the next step will reveal which were non-numeric (because they'll get set to NAs) # TODO: If we eventually want to ask about cones-old and caches, we need to take care of this for those columns too.
  mutate(across(c("radius", "seedlings_0yr", "seedlings_1yr", "cones_new", "under_cones_new"), ~as.numeric(as.character(.)))) |> # Convert everything to numeric
  mutate(radius = ifelse(radius == .0, 10, radius)) |> ## TODO: note this quick fix to address comment above
  mutate(seedl_dens = seedlings_0yr / (3.14*radius^2),
         cone_dens = cones_new / (3.14*8^2))

# Set our placeholder "na" for seedwall cones to a true NA, make numeric and compute density
seedl[which(seedl$seedwall_cones == "na"),"seedwall_cones"] = NA
seedl = seedl %>%
  mutate(seedwall_cones = as.numeric(seedwall_cones),
         seedwall_cone_dens = seedwall_cones / (3.14*8^2)) # cones were always counted in the 8 m plots

## DEREK: get which fire the seedl are from so we can remove creek. first need to select only unique plotid-fire pairs.

# Get just the columns that are relevant, and remove irrelevant rows
seedl_simp = seedl %>%
  select(plot_id,species,seedl_dens,cone_dens,under_cones_new) %>%
  # remove some types of cones that are not relevant
  filter(!(species %in% c("immature PIPO or PICO (open)"))) %>% # exclude eaten becaues it is not a cone that dispersed seeds.
  # We are excluding stripped cones (only one recorded) from the cone count, with the logic that this fecundity was not relevant to regen
  filter(species != "EATEN PILA") %>%
  group_by(plot_id,species) %>%
  mutate(dup = duplicated(plot_id, species))
  ## TODO: for processing revisit data (including 2022 revisits of Creek), here is where we will need to deal with the fact that some plots have two different counts for each species, for both radii

  # DEREK: need to filter out Creek data temporarily

  summarize(across(everything(),~sum(.x,na.rm=TRUE)))
  
# Compute total across all species (and also for pines and firs--incl douglas) for each plot and append

seedl_tot = seedl_simp %>%
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                   cone_dens = sum(cone_dens, na.rm=TRUE),
                   under_cones_new = max(under_cones_new, na.rm=TRUE)) %>%
  mutate(species = "ALL")

seedl_pines = seedl_simp %>%
  filter(species %in% c("PICO","PIJE","PILA","PILA/PIPJ","PIPJ","PIPJ/PILA","PIPO","PIxx","PIXX", "PI-", "PIMO")) %>%
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                   cone_dens = sum(cone_dens, na.rm=TRUE),
                   under_cones_new = max(under_cones_new, na.rm=TRUE)) %>%
  mutate(species = "PINES")
  
seedl_ylwpines = seedl_simp %>%
  filter(species %in% c("PIJE","PIPJ","PIPO")) %>%
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                   cone_dens = sum(cone_dens, na.rm=TRUE),
                   under_cones_new = max(under_cones_new, na.rm=TRUE)) %>%
  mutate(species = "YLWPINES")

seedl_firs = seedl_simp %>%
  filter(species %in% c("ABCO","ABCO/ABMA","ABCO/PSME","PSME")) %>%
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                   cone_dens = sum(cone_dens, na.rm=TRUE),
                   under_cones_new = max(under_cones_new, na.rm=TRUE)) %>%
  mutate(species = "FIRS")

seedl_abies = seedl_simp %>%
  filter(species %in% c("ABCO","ABCO/ABMA", "ABMA")) %>% ##!! NOTE this drops one occurrence of abco/psme ambiguous seedlings
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                   cone_dens = sum(cone_dens, na.rm=TRUE),
                   under_cones_new = max(under_cones_new, na.rm=TRUE)) %>%
  mutate(species = "ABIES")


# Get the species-specific observations: exclude ambiguous species IDs. Assume the seedlings weren't there.
###!!! TODO: test the sensitivity to this assumption
seedl_simp = seedl_simp %>%
  filter(species %in% c("ABCO","CADE","PICO","PILA","PSME"))

seedl_comb = bind_rows(seedl_simp,seedl_tot,seedl_ylwpines,seedl_pines,seedl_firs, seedl_abies)

### todo: column for has seedwall cones measured
### todo: column for plot has species ambiguity. currently assuming zero counts for all plots with species ambiguity, and looking at it by PINES/FIRS


## make wide
# first make sure identifying cols are unique
a = duplicated(paste0(seedl_comb$plot_id,seedl_comb$species))
sum(a)

seedl_wide = pivot_wider(seedl_comb, id_cols = "plot_id", names_from = c("species"), values_from=c("seedl_dens","cone_dens","under_cones_new"))

# cols with NAs are zeros
seedl_wide = seedl_wide %>%
  mutate(across(everything(),~ ifelse(is.na(.x),0,.x)))

## TODO: check for mismatched plot names: plots that have NO records in the seedling table may be miswritten in one table or the other. Same for seedsource.


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

# remove cone columns for species that don't produce cones
seedl_wide = seedl_wide %>%
  select(-contains("cone_dens_ABCO"),
         -contains("cone_dens_FIRS"),
         -contains("cone_dens_CADE"))

##TODO: if seedwall cones are blank, can we assume they are zero?

## ^ with the above section, we now have cones and seedlings by species and species group!



#### Process seed sources ####

## Get the closest green tree overall and by species / species group (assuming there is a tree just beyond where they could see)
##!!TODO: assess sensitivity to assumption that there is a seed tree just beyond the ">" distance recorded

green_seedsource = read_excel(datadir("field-data/raw/dispersal-data-entry-2022.xlsx"),sheet="green_seedsource") %>%
  # correct some plot IDs that don't match the main plot table
  mutate(plot_id = recode(plot_id,
                          "C062259" = "C062-259",
                          "C062332" = "C062-332",
                          "C38" = "C28",
                          "S035-551" = "C035-551"
  )) |>
  filter(!is.na(plot_id))




dist_grn = green_seedsource %>%
  select(-`(add cols for other spp)`,-`Notes`) %>%
  mutate(across(any:`abco/abma`, ~ str_replace(.x,fixed(">"),""))) %>% # remove the ">" from distances
  mutate(across(any:`abco/abma`, ~ ifelse(.x == "MISSING",NA,.x))) %>% # change "MISSING" to NA because that's OK for the one place it appears because on that plot they recorded distance to any species
  filter(!is.na(plot_id),
         metric == "distance") %>%
  select(-metric) %>%
  # within a plot, across cluster IDs, get the closest
  group_by(plot_id) %>%
  summarize(across(-cluster_id, ~ min(.x,na.rm=TRUE)))

# make composite-species columns
dist_grn = dist_grn %>%
  mutate(ALL = pmin(any,pipj,pila,psme,abco,cade,`abco/abma`, na.rm=TRUE),
         YLWPINES = pipj,
         PINES = pmin(pipj,pila, na.rm=TRUE),
         FIRS = pmin(psme,abco,`abco/abma`,na.rm=TRUE),
         ABIES = pmin(abco,`abco/abma`, na.rm=TRUE)) %>%
  select(-`abco/abma`, -any) ###!!! NOTE: dropping trees that were abco/abma ambiguous They are already incorporated into FIRS and ABIES. For analysis, need to keep in mind that can't use these plots for species-specific ABCO analysis. Or just do all analysis for ABIES.

names(dist_grn) = toupper(names(dist_grn))  
names(dist_grn)[names(dist_grn) == "PLOT_ID"] = "plot_id"

#prepend "dist_grn_" to the seed dist cols
names(dist_grn)[names(dist_grn) != "plot_id"] = paste0("dist_grn_",names(dist_grn)[names(dist_grn) != "plot_id"])



#### Pull in seedlings/cones and dist_grn into the plot data

plots_seedl = left_join(plots,seedl_wide)
# rows with NA for seedl and cone are true zeros

# were there any seedl_cone records that didn't match plots?
matched = (seedl_wide$plot_id) %in% plots_seedl$plot_id
not_matched = seedl_wide[!matched,"plot_id"]


plots_complete = left_join(plots_seedl,dist_grn)
# lots of rows with NA for dist_grn that simply didn't have any grn recorded (bc not visible)

# were there any dist_grn records that didn't match plots?
matched = (dist_grn$plot_id) %in% plots_seedl$plot_id
not_matched = dist_grn[!matched,"plot_id"]

## make sure no plots are duplicated
any(duplicated(plots_complete$plot_id))

## Combine seedwall_dist with the green seed sources (all)? Actually don't need to since we'll be anlyzing seedwall plots separately.



## for seedl_dens, and cone_dens (but not seedwall_cone_dens), set to 0 if NA
plots_complete = plots_complete %>%
  mutate(across(starts_with("cone_dens_"), ~ifelse(is.na(.x),0,.x)  )) %>%
  mutate(across(starts_with("seedl_dens_"), ~ifelse(is.na(.x),0,.x)  ))



#### Compute 
# 50 m cover : % green, % scorched
plots_complete = plots_complete %>%
  mutate(#untorched_cover_50m = cov_untorched_50m / 100,    #removed this because it turns it out was collected inconsistently in 2021 vs 2022 and therefore it was discontinued partway through 2022
         vol_brn_prop_50m = vol_brn_50m / 100,
         vol_grn_prop_50m = vol_grn_50m / 100)


#### Pull in species comp of overstory, compute percent of green and brown by species and species groups
sp_comp = read_excel(datadir("field-data/raw/dispersal-data-entry-2022.xlsx"), sheet="sp_comp+count") %>%
  # correct some plot IDs that don't match the main plot table
  mutate(plot_id = recode(plot_id,
                          "C062259" = "C062-259",
                          "C062332" = "C062-332",
                          "C38" = "C28",
                          "C0155166" = "C015166",
                          "S37B" = "S73B",
                          "T0007" = "T007",
                          "S035-551" = "C035-551"
  ))
  # remove a double-entered plot
  

# for green, get percent of the green by species (using green vol comp)
# for brown, multiply brown cover by pre-drop species comp (using pre-drop cover)
# also use overall pre-drop species comp by volume
# so we need to get pre-drop vol and green vol by species and species group

# make composite-species columns
sp_comp = sp_comp %>%
  filter(metric %in% c("untorched_vol", "untorched_cov", "green_vol")) %>%
  mutate(across(pipj:`abco/psme`, ~as.numeric(as.character(.x)))) %>%
  rowwise() %>%
  mutate(ALL = sum(pipj,pila,psme,abco,cade,`abco/abma`, `abco/psme`, pico, pimo, na.rm=TRUE),
         YLWPINES = pipj,
         PINES = sum(pipj,pila,pico, pimo, na.rm=TRUE),
         FIRS = sum(psme,abco,`abco/abma`, `abco/psme`,na.rm=TRUE),
         ABIES = sum(abco,`abco/abma`, na.rm=TRUE)) %>%
  select(-`abco/abma`, -`abco/psme`, -`(add cols for other spp)`) %>% ###!!! NOTE: dropping trees that were abco/abma ambiguous They are already incorporated into FIRS and ABIES. For analysis, need to keep in mind that can't use these plots for species-specific ABCO analysis. Or just do all analysis for ABIES.
  rename_with(~toupper(.), pipj:pimo) %>%
  unique() %>% # remove a row that was double-entered
  pivot_wider(names_from=metric, values_from = c("PIPJ","PILA","PSME","ABCO","CADE","PICO", "PIMO","ALL","YLWPINES","PINES","FIRS","ABIES"), values_fn = max) %>%
  # if it's NA then assume it's 0
  mutate(across(-plot_id,~ifelse(is.na(.x),0,.x)))
#TODO: without "values_fun" above, list cols are being generated. They are from revisited plots that are listed twice. need to deal with that if want to analyze revisits

# pull comp into plot data
plots_w_comp = left_join(plots_complete,sp_comp)

#check that the comp data matched the plot IDs of the main plot data
check = plots_w_comp %>%
  select(plot_id, ALL_untorched_vol, ALL_green_vol)

# which comp data didn't match with a main plot?
check2 = left_join(sp_comp,plots_complete) %>%
  select(plot_id, date)

# entries for comp without plot data:
# S100-2 (was double-entered under a different ID with no dashes)
# S100-1 (was double-entered under a different ID with no dashes)




####!! if the plot is a core plot and had some vol brown overall (vol_brn_50m), but ALL_untorched_cov is 0 and it's not a seedwall plot, set all the untorched covers to -5 because it means it was not entered properly
## same for green ^
####!!NOTE TODO need to re-enter these from the paper datasheets

which_incomplete_predrop = (plots_w_comp$vol_brn_50m > 1) & (plots_w_comp$ALL_untorched_cov == 0) & (toupper(plots_w_comp$plot_type) == "CORE")
which_incomplete_green = (plots_w_comp$vol_grn_50m > 1) & (plots_w_comp$ALL_green_vol == 0) & (toupper(plots_w_comp$plot_type) == "CORE")

plots_w_comp[which_incomplete_predrop | which_incomplete_green,] %<>%
  mutate(across(ends_with(c("_cov","_vol")),~-5))


## To get species-specific green, multiply *_green_vol by green_vol_prop_50m: this is "proportion of the prefire canopy that is green for species" and "proprotion of the pre
## To get species-specific scorched, multiply *_predrop_cov by scorched_cover_50m: this is "proportion of the scorched canopy cover that is composed of species X"

plots_w_comp = plots_w_comp %>%
  mutate(across(ends_with("_green_vol"), ~.x*vol_grn_prop_50m/100, .names = "{.col}_abs")) %>%
  #mutate(across(ends_with("_untorched_cov"), ~.x*untorched_cover_50m/100, .names = "{.col}_abs")) %>% # removed this because scorched_cover was being collected incorrectly
  mutate(across(ends_with("_untorched_vol"), ~.x*vol_brn_prop_50m/100, .names = "{.col}_abs"))




### Pull in date of burning

dob = vrt(list.files(datadir("day-of-burning-rasters/"), pattern="tif$", full.names=TRUE))

plots_sp = st_as_sf(plots_w_comp, coords=c("lon","lat"), crs="EPSG:4326")

dob_extract = extract(dob, plots_sp |> st_transform(st_crs(dob)))[,2]
plots_w_comp$day_of_burning = dob_extract
plots_sp$day_of_burning = dob_extract

write_csv(plots_w_comp, datadir("field-data/processed/plot_seedl_cone_grnseedsource_comp.csv"))
st_write(plots_sp, datadir("field-data/processed/early-regen-2022.gpkg"), delete_dsn = TRUE)

## Save a version with plot_id only
d_save = plots_sp |>
  filter(fire == "Caldor") |>
  select(plot_id)

st_write(d_save, datadir("field-data/processed/for-collabs/UCDavis_Young_Caldor_2022.gpkg"), delete_dsn = TRUE)
st_write(d_save, datadir("field-data/processed/for-collabs/UCDavis_Young_Caldor_2022.shp"), delete_dsn = TRUE)
