# Purpose: Prepare manually entered plot data for analysis

##!! TODO: make sure that scorched needle volume by species is being computed correctly

library(tidyverse)
library(sf)
library(readxl)
library(exifr)
library(magrittr)
library(terra)

# The root of the data directory
data_dir = readLines("data_dir.txt", n=1)

source("scripts/convenience_functions.R")


#### Load plot data and correct some entry errors ####

plots = read_excel(datadir("field-data/raw/dispersal-data-entry-2023.xlsx"),sheet="plot_main") %>%
  mutate(date = as.character(as.Date(as.numeric(date), origin = "1899-12-30"))) %>% # convert the Excel date to a proper date in YYYY-MM-DD format
  filter(! plot_id %in% c("S100-1","S100-2")) %>% # these two plots were entered twice, but with different names (one set with dash, one set without, so remove the one with)
  filter(!(plot_id == "S027094" & entered_by == "Diego")) |> # this was entered twice by two differnt people
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

## 2023 plot C035-551 was entered a second time erroneously as C035-511. Remove the error.
plots = plots |>
  filter(!(plot_id == "C035-511" & Year == 2023)) # remove the erroneous entry





# More typo corrections TODO: need to apply these to all tables of the data (e.g. also seedling
# counts). So writing it as a function so the same changes can be applied to all tables
plot_id_typo_correction = function(df) {
  # Some 2023 plot IDs have a " (Px)" suffix (e.g. "C68 (P4)"). I'm not sure what this is for but it
  # seems unnecessary and doesn't have anything to do with matching plots to previous years, so
  # remove it.
  df$plot_id = str_replace(df$plot_id, " \\(P[0-9]\\)", "")
    df$plot_id = str_replace(df$plot_id, "\\(P[0-9]\\)", "")
 
  df = df |>
    mutate(plot_id = recode(plot_id,
                            "209-250" = "S09-250",
                            "209-253" = "S09-253",
                            "204-279" = "S04-279",
                            "C12-08" = "C12-008",
                            "C012-010" = "C12-010",
                            "S014-264" = "S14-264",
                            "C013-260" = "C13-260",
                            "C062432" = "C062-432",
                            "C11-" = "C11",
                            "S49 B" = "S49B",))
    

  
  return(df)
}

plots = plot_id_typo_correction(plots)

  


# S07B was not surveyed in 2022 because it was close to road and on a skid trail, it was only
# accidentally surveyed again in 2023. Remove from all years data. Note that the other data tables
# will retain the data for this plot, but they should never be pulled in because they are not in the
# main plot table.
plots = plots |>
  filter(!(plot_id == "S07B"))


# Look for any plots surveyed in 2023 that are not in the 2022 data
plots_2023 = plots |>
  filter(Year == 2023) |>
  pull(plot_id)

plots_2022 = plots |>
  filter(Year == 2022) |>
  pull(plot_id)

plots_2023_not_2022 = setdiff(plots_2023, plots_2022)
plots_2023_not_2022
# 2022 survey did not include C72 or C062-9 but it's OK because 2021 survey did


# Look for any plots surveyed in 2022 that are not in the 2021 data (but not for Caldor and Dixie
# fires)
plots_2021 = plots |>
  filter(Year == 2021) |>
  pull(plot_id)
plots_2022_notCD = plots |>
  filter(Year == 2022) |>
  filter(Fire != "Caldor" & Fire != "Dixie") |>
  pull(plot_id)
plots_2022_not_2021 = setdiff(plots_2022_notCD, plots_2021)
plots_2022_not_2021
# Great there are none!

# TODO: use the photo coords from 2022 to get the correct coords for S039-158 (need to log in to get
# photos off of 2022 Phone C -- need to know what account that phone was using)

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


# For each plot, if it's missing coords, see if there are coords in a different year, and if so,
# pull them in.

for(i in 1:nrow(plots)) {

  plot = plots[i,]

  if(!is.na(plot$lat) & !is.na(plot$lon)) next() # if coords are already there, skip

  # get the plot ID
  plot_id_foc = plot$plot_id

  # get the year
  year = plot$Year

  # get the coords from the other year
  coords = plots %>%
    filter(plot_id == plot_id_foc & Year != year) %>%
    select(lat,lon)

  if(nrow(coords) == 0) next() # no coords in other year
  
  if(nrow(coords) > 1) {
    # if there are multiple rows, see if they are different and if so, take the second one but give
    # a warning
    if(coords$lat[1] != coords$lat[2] | coords$lon[1] != coords$lon[2]) {
      warning(paste0("Multiple lat/lon for plot ",plot_id_foc," in different year"))
    }
    coords = coords[2,]
  }

  plots[i,"lat"] = coords$lat
  plots[i,"lon"] = coords$lon

}


# The most recent coords for a plot are the ones to be trusted. So set 2022 coords to 2023 coords,
# and 2021 coords to 2022 coords, and 2021 coords to 2023 coords. Effecitvely, for each plot, see if
# there are coords in a more recent plot, and if so, pull them in.
plots = plots |>
  arrange(Year)
for(i in 1:nrow(plots)) {

  plot = plots[i,]

  # get the plot ID
  plot_id_foc = plot$plot_id

  # get the year
  year = plot$Year

  # get the coords from the other more recent year(s)
  coords = plots %>%
    filter(plot_id == plot_id_foc & Year > year) %>%
    select(lat, lon)

  if(nrow(coords) == 0) next() # no coords in other year
  
  if(nrow(coords) > 1) {
    # if there are multiple rows, see if they are different and if so, take the second one but give
    # a warning
    if(coords$lat[1] != coords$lat[2] | coords$lon[1] != coords$lon[2]) {
      warning(paste0("Multiple lat/lon for plot ",plot_id_foc," in different year"))
    }
    coords = coords[2,]
  }

  plots[i, "lat"] = coords$lat
  plots[i, "lon"] = coords$lon

}

# Make sure there are no duplicated plots within a year
plots_duplicated = plots |>
  mutate(duplicated = duplicated(paste0(plot_id, Year))) |>
  select(plot_id, duplicated) |>
  filter(duplicated == TRUE)
plots_duplicated


## Set plot type to all lowercase
plots$plot_type = tolower(plots$plot_type)


### For each plot, check if it is missiong plot_type, and if so, pull it in from a previous year

for(i in 1:nrow(plots)) {

  plot = plots[i,]

  if(!is.na(plot$plot_type)) next() # if coords are already there, skip

  # get the plot ID
  plot_id_foc = plot$plot_id

  # get the year
  year = plot$Year

  # get the plot_type from the other year
  plot_type = plots %>%
    filter(plot_id == plot_id_foc & Year != year) %>%
    select(plot_type)

  if(nrow(plot_type) == 0) next() # no plot type in other year
  
  if(nrow(plot_type) > 1) {
    # if there are multiple rows, see if they are different and if so, take the second one but give
    # a warning
    if(plot_type$plot_type[1] != plot_type$plot_type[2]) {
      warning(paste0("Multiple plot types for plot ",plot_id_foc," in different year:", 
                     plot_type$plot_type[1], " and ", plot_type$plot_type[2], ". Taking the second."))
    }
    plot_type = plot_type[2,]
  }

  plots[i,"plot_type"] = plot_type$plot_type

}





#### Prep prefire prop by species (only collected 2022). This is a series of fields that have the estimated proportion of prefire BA by species (one field per species)
# Set it to 0 if it is NA (because NAs mean 0)
# One plot did not have this entered. Need this to remain as NA so this plot gets omitted from analyses that require this data
plots = plots |>
  mutate(across(starts_with("prefire_prop_"), ~ifelse(is.na(.x), "0", .x))) |> # make NAs zer0s
  mutate(across(starts_with("prefire_prop_"), ~ifelse(.x == "MISSING", NA, .x))) |> # make the MISSING fields NAs
  mutate(across(starts_with("prefire_prop_"), as.numeric))         

# Make 2021 and 2023 values NA since it was not collected
plots = plots |>
  mutate(across(starts_with("prefire_prop_"), ~ifelse(Year != 2022, NA, .x)))
  
# For prefire species comp that sums to > or < 100 across species, normalize it so that it does sum to 100
plots = plots |>
  mutate(total_prefire_prop = rowSums(across(starts_with("prefire_prop_")))) |>
  mutate(across(starts_with("prefire_prop_"), ~./(total_prefire_prop/100))) |>
  select(-total_prefire_prop)

# Make sure these fields look good
inspect = plots |>
  select(plot_id, starts_with("prefire_prop_"))

# One plot is missing prefire prop sp, so set it equal to the mean of the other plots at that seed wall
plots_s039 = plots |>
  filter(grepl("S039-", plot_id, fixed = TRUE)) |>
  mutate(across(starts_with("prefire_prop_"), ~ifelse(is.na(.), mean(., na.rm=TRUE), .)))

# Store those new values back in the main plots table
plots[grepl("S039-", plots$plot_id, fixed = TRUE),] = plots_s039


# Compute prefire_prop by species groups
plots = plots |>
  mutate(prefire_prop_PINES = prefire_prop_pipj + prefire_prop_pila + prefire_prop_pico + prefire_prop_pimo,
         prefire_prop_ABIES = prefire_prop_abco + prefire_prop_abma,
         prefire_prop_ALL = 100) |>
  rename(prefire_prop_YLWPINES = prefire_prop_pipj,
         prefire_prop_ABCO = prefire_prop_abco,
         prefire_prop_PSME = prefire_prop_psme,
         prefire_prop_PILA = prefire_prop_pila,
         prefire_prop_CADE = prefire_prop_cade)


# Remove the columns that were added as intermediate data prep steps
plots = plots |>
  select(-Easting, -Northing, -Elevation, -`Lateral RMS`, -time, -photo_lat, -photo_lon, -coords_from_photo, -coords_from_gps, -pre_lat, -pre_lon)

# Save the cleaned plot data
write_csv(plots, datadir("field-data/processed/allplots/cleaned/plots.csv"))

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
  # remove a 2021 entry for plot S1001 because it was entered a second time as S100-1, and we are
  # retaining (and fixing the name of) that one
  filter(!(plot_id == "S1001" & year == 2021)) %>%
  # Same for a 2021 plot S027094 that was entered a second time as S0427094
  filter(!(plot_id == "S027094" & year == 2021)) %>%
  # correct some plot IDs that don't match the main plot table
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
  
seedl = plot_id_typo_correction(seedl)

# Note that Derek deleted duplicated seedling count entries for plot_ids D026-565 and C035-551 from
# the google sheet. Also for 2023 plot C041-161.

# Get seedling plot IDs that don't match to the main plot table in any given year, because they
# indicate typos
seedl_plot_year = paste0(seedl$plot_id, "_",  seedl$year)
plots_plot_year = paste0(plots$plot_id, "_",  plots$Year)
setdiff(seedl_plot_year, plots_plot_year)
# The only plot that doesn't have a match is S07B, as expected (see above)

# Get plot IDs from plot table that don't have an entry in the seedling table, also a search for
# typos
setdiff(plots_plot_year, seedl_plot_year)
# All the missing ones are from 2021 when we didn't enter anything if there were no seedlings, plus
# S019-558 which we'll need to find from the paper data sheet and enter, making sure it was not
# entered as another.

# 2023	C003-017	CADE	for radius 10 with seedlings_0yr of 2 appears it should have been for radius 8. Correct it.
seedl[seedl$plot_id == "C003-017" & seedl$year == 2023 & seedl$species == "CADE" & seedl$radius == 10 & seedl$seedlings_0yr == 2, "radius"] = "8"



# Make sure there are no duplicate entries in the seedling table
seedl_check = seedl
seedl_check$duplicated = duplicated(seedl_check)
any(seedl_check$duplicated)

# Another way to check for duplicates that has a higher false-positive rate but also a higher
# sensitivity
seedl_check = seedl |>
  group_by(plot_id, year, species, radius, seedlings_0yr) |>
  filter(n() > 1)
  
seedl_check = seedl |>
  group_by(plot_id, year, species, radius, seedlings_1yr) |>
  filter(n() > 1)
seedl_check

seedl_check = seedl |>
  filter(!is.na(seedlings_2yr) & seedlings_2yr != "0") |>
  group_by(plot_id, year, species, radius, seedlings_2yr) |>
  filter(n() > 1)
seedl_check

# The only duplicates after fixing (above) are ones that had zeros, which is inconsequential.


#### Prep seedling data

# For 2021 data: For 2021 data, we recorded seedlings (and cones?) in categories like "100+" but
# then entered the data as "100". For analyzing the 2021 data, set "+" seedling/cone counts (round
# numbers at the floor of each bin) as the midpoint of the bin?? May also need set 2022 counts
# similarly to avoid bias.

table(seedl |> filter(year == 2021) |> pull(seedlings_0yr)) |> sort()
seedl[which(seedl$seedlings_0yr == "300" & seedl$year == 2021), "seedlings_0yr"] = "350"
seedl[which(seedl$seedlings_0yr == "25" & seedl$year == 2021), "seedlings_0yr"] = "38"
seedl[which(seedl$seedlings_0yr == "50" & seedl$year == 2021), "seedlings_0yr"] = "63"
seedl[which(seedl$seedlings_0yr == "75" & seedl$year == 2021), "seedlings_0yr"] = "88"
seedl[which(seedl$seedlings_0yr == "100" & seedl$year == 2021), "seedlings_0yr"] = "125"
seedl[which(seedl$seedlings_0yr == "150" & seedl$year == 2021), "seedlings_0yr"] = "175"
seedl[which(seedl$seedlings_0yr == "200" & seedl$year == 2021), "seedlings_0yr"] = "250"

table(seedl |> filter(year == 2021) |> pull(cones_new)) |> sort()
seedl[which(seedl$cones_new == "300" & seedl$year == 2021), "cones_new"] = "350"
seedl[which(seedl$cones_new == "50+" & seedl$year == 2021), "cones_new"] = "63"
seedl[which(seedl$cones_new == "100" & seedl$year == 2021), "cones_new"] = "125"
seedl[which(seedl$cones_new == "150" & seedl$year == 2021), "cones_new"] = "175"
seedl[which(seedl$cones_new == "200" & seedl$year == 2021), "cones_new"] = "250"
seedl[which(seedl$cones_new == "75" & seedl$year == 2021), "cones_new"] = "88"
seedl[which(seedl$cones_new == "50" & seedl$year == 2021), "cones_new"] = "63"
seedl[which(seedl$cones_new == "25" & seedl$year == 2021), "cones_new"] = "38"
seedl[which(seedl$cones_new == "n/a" & seedl$year == 2021), "cones_new"] = "0"

## CAVEAT Have to assume the two instances of calling cones_new "H" was about 15 cones (for new cones within the plot)
seedl[which(seedl$cones_new == "H"),"cones_new"] = "15"
seedl[which(seedl$cones_old == "H"),"cones_old"] = "15"


# Except for Creek: For 1 yr counts, if a 'radius' or 'seedlings_0yr' entry has a "/" to indicate two radii, then it
# means the plot had a previous radius but it was reduced because of a high number of seedlings. So
# take the smaller value.

# For 0 yr counts, if there is a slash in the radius, it means two radii were surveyed (8 and 10)
# and count for the higher radius is the ADDITIONAL number of seedlings found by surveying that
# radius. If there is a single number, then no additional were found. Did this because suspected we
# would not want to survey to 10 m in Y4.

##!!!!!! NOTE: Confirm in the field if the higher radius ^ was the ADDITIONAL number or the
#cumulative number. It's possible that it's cumulative for non-Creek but additional for Creek. There
#is not contrary evidence for non-Creek, other than some plots have a radius recorded as 8/10 but a
#single seedling count recorded, suggesting that when seedling counts are recorded as 1/1 it means
#something different (i.e. that the second number is not cumulative).

# In creek, for 1-2 yr seedl, we always did 8 m radius, except apparently sometimes 8 and 10.
# For creek, if there is no value for an 8 m radius for Y0, the 8 m count is 0 (I think but not 100%
# sure).

# For creek, seedling counts over 25 were recorded as 25+ or > 25

##

# Pull in the fire ID
seedl = left_join(seedl, plots[,c("plot_id","Fire","Year")], by=join_by("plot_id" == "plot_id", "year" == "Year"))

# Loop through all these records and make all these corrections

# Initiate it with dummy data to get the data types we want
split_seedl_data = data.frame(year = 1000, 
                              plot_id = "S000",
                              radius = "0",
                              species = "PIPJ",
                              seedlings_0yr = "0",
                              seedlings_1yr = "0",
                              seedlings_2yr = "0",
                              stringsAsFactors = FALSE)

for(i in 1:nrow(seedl)) {
  
  row = seedl[i,]
  if(is.na(row$radius)) next() # if there is no radius, skip
  radius_has_slash = str_detect(row$radius, fixed("/"))
  if(!radius_has_slash) next() # if there is no slash, skip
  
  # get the radii
  radii = str_split(row$radius, fixed("/")) |> unlist()
  
  # If there is an entry for 0 yr seedlings
  if(!is.na(row$seedlings_0yr)) {
    
    seedlings = str_split(row$seedlings_0yr, fixed("/")) |> unlist()
    
    # If there are 2 seedling numbers, then match them with their respective radii (the second
    # number is cumulative to the first)
    if(length(seedlings) == 2) {
      
      seedlings[2] = as.character(as.numeric(seedlings[1]) + as.numeric(seedlings[2]))
      
      newdata = data.frame(year = row$year,
                           plot_id = row$plot_id,
                           radius = radii,
                           species = row$species,
                           seedlings_0yr = seedlings)
      split_seedl_data = bind_rows(split_seedl_data, newdata)
    } else { 
      # if there is only one entry, it was the same count for both radii
      newdata = data.frame(year = row$year,
                           plot_id = row$plot_id,
                           radius = radii,
                           species = row$species,
                           seedlings_0yr = seedlings)
      split_seedl_data = bind_rows(split_seedl_data, newdata)
    }
  }
  
  # If there is an entry for 1 yr seedlings
  if(!is.na(row$seedlings_1yr)) {

    seedlings = str_split(row$seedlings_1yr, fixed("/")) |> unlist()

    # If there are 2 seedling numbers, then match them with their respective radii (the second
    # number is cumulative to the first) (this only happens on Creek)
    if(length(seedlings) == 2) {

      seedlings[2] = as.character(as.numeric(seedlings[1]) + as.numeric(seedlings[2]))

      newdata = data.frame(year = row$year,
                           plot_id = row$plot_id,
                           radius = radii,
                           species = row$species,
                           seedlings_1yr = seedlings)
      split_seedl_data = bind_rows(split_seedl_data, newdata)
    } else { 
      # if there is only one count entry, then the second radius is the radius that was actually surveyed

      newdata = data.frame(year = row$year,
                           plot_id = row$plot_id,
                           radius = radii[2],
                           species = row$species,
                           seedlings_1yr = seedlings)
      split_seedl_data = bind_rows(split_seedl_data, newdata)
    }
  }
  
  # If there is an entry for 2 yr seedlings
  if(!is.na(row$seedlings_2yr)) {

    seedlings = str_split(row$seedlings_2yr, fixed("/")) |> unlist()

    # If there are 2 seedling numbers, then match them with their respective radii (the second
    # number is cumulative to the first) (this only happens on Creek)
    if(length(seedlings) == 2) {

      seedlings[2] = as.character(as.numeric(seedlings[1]) + as.numeric(seedlings[2]))

      newdata = data.frame(year = row$year,
                           plot_id = row$plot_id,
                           radius = radii,
                           species = row$species,
                           seedlings_2yr = seedlings)
      split_seedl_data = bind_rows(split_seedl_data, newdata)
    } else { 
      # if there is only one count entry, then the second radius is the radius that was actually surveyed
      newdata = data.frame(year = row$year,
                           plot_id = row$plot_id,
                           radius = radii[2],
                           species = row$species,
                           seedlings_2yr = seedlings)
      split_seedl_data = bind_rows(split_seedl_data, newdata)
    }
  }
}

# Checking that the split_seedl_data looks good. Filter out the rows that have radius
# slashes and see if there is a good correspondence. Then remove them from the seedl data.

# Get the rows that have radius slashes (indexes)
radius_slash_rows_indexes = which(str_detect(seedl$radius, fixed("/")))
radius_slash_rows = seedl[radius_slash_rows_indexes,]

# Remove the rows that have radius slashes
seedl = seedl[-which(str_detect(seedl$radius, fixed("/"))),]

# Are there any duplicated entries with the same year, plot_id, radius, and species?
duplicated_rows = split_seedl_data %>%
  group_by(year, plot_id, radius, species) %>%
  filter(n() > 1) %>%
  ungroup()
# Nope, awesome!

## For 2022 Creek seedlings, set zeros to NA

seedl[which(seedl$year == 2022 & seedl$Fire == "Creek" & seedl$seedlings_0yr == 0), "seedlings_0yr"] = NA
seedl[which(seedl$year == 2022 & seedl$Fire == "Creek" & seedl$seedlings_1yr == 0), "seedlings_1yr"] = NA



## Make seedling data long form
seedl_long = seedl |>
  select(-caches, -cones_new, -cones_old, -under_cones_new, -under_cones_old, -seedwall_cones) |>
  pivot_longer(cols = c(seedlings_0yr, seedlings_1yr, seedlings_2yr),
               names_to = "seedling_age",
               values_to = "seedling_count") |>
  mutate(seedling_age = str_sub(seedling_age, 11, 11)) |>
  # remove the dummy rows from plot ID "A"
  filter(plot_id != "A")

## Go through each Creek 2022 plot, and for each seedling age, if all counts are NA, then add a new row for 10 m radius where count is set to 0. Also if there are non-NA values for both 8 and 10, set 10 to the sum

plot_ids = unique(seedl_long$plot_id)

for(plot_id_foc in plot_ids) {

  plot_rows = seedl_long |>
    filter(year == "2022", Fire == "Creek", plot_id == plot_id_foc)
  plot_rows$radius = str_replace(plot_rows$radius, fixed("Q"), "2")
  sps = unique(plot_rows$species)

  for(sp_foc in sps) {

    plot_sp_rows = plot_rows[which(plot_rows$species == sp_foc),]

    plot_sp_rows = plot_sp_rows |>
      arrange(as.numeric(radius))

    for(seedl_age_foc in c("0", "1")) {

      plot_sp_age_rows = plot_sp_rows |>
         filter(seedling_age == seedl_age_foc)

      counts = plot_sp_age_rows$seedling_count

      two_counts = !is.na(counts[1]) & !is.na(counts[2])

      if(two_counts) {
        # If the radii are 8 and 10, set 10 to the sum of 8 and 10
        if(plot_sp_age_rows$radius[1] == 8 & plot_sp_age_rows$radius[2] == 10) {
          message("Because of counts for 8 m and 10 m radiius for plot ", plot_id_foc, " and species ", sp_foc, " for seedling age ", seedl_age_foc, " adding the 8 m count to the 10 m count")

          seedl_long[seedl_long$plot_id == plot_id_foc & seedl_long$species == sp_foc & seedl_long$seedling_age == seedl_age_foc & seedl_long$radius == 10 & seedl_long$year == 2022, "seedling_count"] = as.character(as.numeric(plot_sp_age_rows$seedling_count[1]) + as.numeric(plot_sp_age_rows$seedling_count[2]))

          plot_sp_age_rows$seedling_count[2] = as.numeric(plot_sp_age_rows$seedling_count[1]) + as.numeric(plot_sp_age_rows$seedling_count[2])
        } else {
          message("There are counts for two radii for plot", plot_id_foc, " and species ", sp_foc, " for seedling age ", seedl_age_foc, " but they are not 8 and 10 m")
        }
      }

      # If all counts are NA, add a new row for 10 m radius where count is set to 0
      if(all(is.na(counts))) {
        message("Adding a new row for plot ", plot_id_foc, " and species ", sp_foc, " for seedling age ", seedl_age_foc, " with 10 m radius and count 0")
        new_row = data.frame(year = 2022,
                              plot_id = plot_id_foc,
                              species = sp_foc,
                              radius = "10",
                              Fire = "Creek",
                              seedling_age = seedl_age_foc,
                              seedling_count = "0")
        seedl_long = bind_rows(seedl_long, new_row)
      }
    }
  }
}



# Remove rows with NA seedling counts and sort
seedl_long = seedl_long |>
  filter(!is.na(seedling_count)) |>
  arrange(year, Fire, plot_id, species, seedling_age, radius)








# This is now the cleaned version of the raw data since the next step is aggregation, so save it out
  write_csv(seedl_long, datadir("field-data/processed/allplots/cleaned/seedlings.csv"))





# Clean and export the cone data
cones = seedl %>%
  # Remove the first row which is a dummy to force R to read the data as text
  filter(!(plot_id == "A")) %>%
  select(year, plot_id, species, cones_new, cones_old, under_cones_new, under_cones_old, seedwall_cones, Fire) |>
  mutate(across(contains("cones"), ~ifelse(. == 0, NA, .))) |> # set 0 to NA so we can filter it out along with all the other NAs (observations of zero)
  filter(!is.na(cones_new) | !is.na(cones_old) | !is.na(under_cones_new) | !is.na(under_cones_old) | !is.na(seedwall_cones)) |>
  # A single "PI-" cone observed in plot C08-214B must be a mistake since there are already counts
  # of PIPJ and PICO cones, so remove it
  # There were 0 cones found for "Any" species in plot T006, so remove it because an absence is
  # equivalent to a zero
  filter(!(species == "ANY" & cones_new == 0)) |>
  # A few times the crew could not distinguish between PIPJ and PICO cones, so they entered names
  # "immature PIPO or PICO (open)", "PI-", "PIxx", "PIXX", so change all these to "PIPJ/PICO"
  mutate(species = recode(species, "immature PIPO or PICO (open)" = "PIPJ/PICO", "PI-" = "PIPJ/PICO", "PIxx" = "PIPJ/PICO", "PIXX" = "PIPJ/PICO")) |>
  # An "EATEN PILA" is still a PILA, so remove it and add its count to the PILA count for that plot
  filter(!(species == "EATEN PILA"))

cones[cones$year == 2021 & cones$plot_id == "C043144" & cones$species == "PILA", "seedwall_cones"] = "7" #2 + 5 # fix the plot ID


# Check for any duplicated entries per year, plot_id, and species
cones_check = cones |>
  group_by(year, plot_id, species) |>
  filter(n() > 1)
cones_check

# Check for weird count values
table(cones$cones_new) |> sort()
table(cones$cones_old) |> sort()
table(cones$under_cones_new) |> sort()
table(cones$under_cones_old) |> sort()

# This is now the cleaned version of the raw data since the next step is aggregation, so save it out
write_csv(cones, datadir("field-data/processed/allplots/cleaned/cones.csv"))



# The code commented out here is only needed for 2021 surveys, and only if we wnt to use seedwall
# cones. It will need to be adapted if we want to use it because now we are prepping the cone data
# in a separate 'cones' table and not in the 'seedl' table as we were previously.
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




#### Process seed sources ####

## Get the closest green tree overall and by species / species group

green_seedsource = read_excel(datadir("field-data/raw/dispersal-data-entry-2023.xlsx"), sheet = "green_seedsource") %>%
  # correct some plot IDs that don't match the main plot table
  mutate(plot_id = recode(plot_id,
                          "C062259" = "C062-259",
                          "C062332" = "C062-332",
                          "C38" = "C28",
                          "S035-551" = "C035-551",
                          "S039-156/158" = "S039-156",
                          "209-251" = "S09-251",
                          "209-253" = "S09-253"
  )) |>
  filter(!is.na(plot_id))

## NOTE: In the data entry sheet for this table, DY manually removed a second identical instance of entered data for C038-229. It came right after the entry for C038-226.

green_seedsource = plot_id_typo_correction(green_seedsource)


# Get seedsource plot IDs that don't match to the main plot table in any given year, because they
# indicate typos
seedsource_plot_year = paste0(green_seedsource$plot_id, "_",  green_seedsource$year)
plots_plot_year = paste0(plots$plot_id, "_",  plots$Year)
setdiff(seedsource_plot_year, plots_plot_year)
# The only plot that doesn't have a match is S07B, as expected (see above)

# Get plot IDs from plot table that don't have an entry in the seedling table, also a search for
# typos
# First exclude the seedwall plots because they would not have an entry here
plots_noseedwall = plots |>
  filter(plot_type != "seedwall") |>
  select(plot_id, Year)
plots_plot_year = paste0(plots_noseedwall$plot_id, "_",  plots_noseedwall$Year)
setdiff(plots_plot_year, seedsource_plot_year)
# All the missing ones are from 2021 when we didn't enter anything if there were no seedlings, plus
# S019-558 which we'll need to find from the paper data sheet and enter, making sure it was not
# entered as another.

## Any duplicated? If so, need to fix because probably means incorrectly entered.
inspect = green_seedsource |>
  filter(metric == "distance",
         cluster_id == 1)

inspect = inspect |>
  mutate(duplicated = duplicated(paste0(inspect$plot_id, "_", inspect$year)))
inspect = inspect |>
  filter(duplicated == TRUE)
inspect

green_seedsource = green_seedsource %>%
  select(-`(add cols for other spp)`,-`Notes`) # remove some data entry columns
  
  
# Replace "200+" to ">200"
green_seedsource = green_seedsource |>
  mutate(across(any:`abco/abma`, ~ ifelse(.x == "200+", ">200", .x))) 


# For 2021, if any of the distances starts with a ">", pull that out, take the minimum per plot, and
# save it out in a separate table (to update the plot table for 2021 with sight line). Then remove
# it from the green seed source table.

plot_ids_2021 = unique(green_seedsource$plot_id[green_seedsource$year == 2021])

sight_lines_2021 = data.frame()

for(plot_id_foc in plot_ids_2021) {
  plot_rows = green_seedsource |>
    filter(year == 2021, plot_id == plot_id_foc)

  sight_line = 5000  # dummy
  
  for(i in 1:nrow(plot_rows)) {
    row = plot_rows[i,]
    if(row$metric == "distance") {
      
      col_indexes_with_gt = which(str_detect(row, fixed(">")))
      
      if(length(col_indexes_with_gt) == 0) {
        next()
      }
      
      vals_with_gt = row[col_indexes_with_gt]
      vals_with_gt_removed = str_replace(vals_with_gt, fixed(">"), "") |> as.numeric()
      
      max_dist_row = max(vals_with_gt_removed, na.rm=TRUE)
      
      sight_line = min(c(sight_line, max_dist_row), na.rm=TRUE)
      

    }
  }
  
  if(sight_line == 5000) next()
  
  sight_lines_plot = data.frame(plot_id = plot_id_foc,
                   year = 2021,
                   sight_line = sight_line)
  
  sight_lines_2021 = bind_rows(sight_lines_2021, sight_lines_plot)
}

write_csv(sight_lines_2021, datadir("field-data/processed/allplots/cleaned/sight_lines_extracted_2021.csv"))



# Now set any distances that started with a ">" to NA
green_seedsource = green_seedsource |>
  mutate(across(any:`abco/abma`, ~ ifelse(str_detect(.x, fixed(">")), NA, .x)))



# Pull out the "dist50green" metric (collected starting 2022) to another table
dist50green = green_seedsource |>
  filter(metric == "dist50green") |>
  select(-cluster_id, -metric, -any)


# Save to CSV
write_csv(dist50green, datadir("field-data/processed/allplots/cleaned/dist_50pct_green.csv"))

# Now create the main table of distance to >5% green
dist_grn = green_seedsource %>%
  filter(metric == "distance") %>% # remove empty rows and remove the "number in cluster" (ntrees) rows
  mutate(across(any:`abco/abma`, ~ ifelse(.x == "MISSING",NA,.x))) %>% # change "MISSING" to NA because that's OK for the one place it appears (which was in 2021) because on that plot they recorded distance to any species. TODO for 2021: make sure that this logic makes sense.
  select(-metric) %>%
  # within a plot, across seed source cluster IDs, get the closest cluster
  mutate(across(c(-year, -plot_id, -cluster_id), as.numeric)) |>
  group_by(year, plot_id) %>%
  summarize(across(any:`abco/abma`, ~ min(.x,na.rm=TRUE))) |>
  # Set any Inf (meaning no observations of that species) to NA
  mutate(across(any:`abco/abma`, ~ ifelse(.x == Inf, NA, .x)))
  
dist_grn = dist_grn |>
  mutate(across(any:`abco/abma`,as.numeric)) # set 0 to NA so we can filter it out along
  
# TODO for more detailed analysis: compute some metric of seed source density & distance that composites both, using all clusters of trees seen. Currenly only using the closest cluster.

# # Make composite-species columns (same as for seedlings). Still this is only recording the minimum distance to any trees in the species group. TODO for more detailed analysis: use a metric of seed source density & distance.
# dist_grn = dist_grn %>%
#   mutate(ALL = pmin(any,pipj,pila,psme,abco,cade,`abco/abma`, na.rm=TRUE),
#          YLWPINES = pipj,
#          PINES = pmin(pipj,pila, na.rm=TRUE),
#          FIRS = pmin(psme,abco,`abco/abma`,na.rm=TRUE),
#          ABIES = pmin(abco,`abco/abma`, na.rm=TRUE)) %>%
#   select(-`abco/abma`, -any) |> ###!!! NOTE for 2021 analysis: we are dropping seed trees that were abco/abma ambiguous They are already incorporated into FIRS and ABIES. For analysis, need to keep in mind that can't use these plots for species-specific ABCO analysis. Or just do all analysis for ABIES. Potentially make a new column that flags whether the trees were ABCO/ABMA ambiguous if it would be useful to optionally exclude those plots. But might not be if always lumping to ABIES.
#   # Inf can mean there were no trees observed for that species group (as the`summarize` step in the previous pipeline can return Inf if there were no trees in that species category at all), so set to NA
#   mutate(across(c(-plot_id,-year), ~ifelse(. == Inf, NA, .)))

# Make long form and write out as CSV

write_csv(dist_grn, datadir("field-data/processed/allplots/cleaned/dist_green_seedsources.csv"))





#### Species comp of overstory and count of trees of each species in each plot


sp_comp = read_excel(datadir("field-data/raw/dispersal-data-entry-2023.xlsx"), sheet="sp_comp+count") %>%
  # correct some plot IDs that don't match the main plot table
  mutate(plot_id = recode(plot_id,
                          "C062259" = "C062-259",
                          "C062332" = "C062-332",
                          "C38" = "C28",
                          "C0155166" = "C015166",
                          "S37B" = "S73B",
                          "T0007" = "T007",
                          "S035-551" = "C035-551",
                          "S039-156/158" = "S039-156",
                          "209-251" = "S09-251",
                          "209-253" = "S09-253",
                          "C0S-271" = "C03-271"
  )) |>
  filter(! plot_id %in% c("S100-1","S100-2")) %>% # these two plots were entered twice, but with different names (one set with dash, one set without, so remove the one with)
  filter(!is.na(plot_id)) |>
  mutate(pimo = ifelse(pimo == TRUE, 2, 1)) # TODO for 2021: when this xls gets imported, weirdly it calls all cells with a value of 2 TRUE and 1 FALSE, so we can hack this back to what it should be

# Apply plot id typo correction function
sp_comp = plot_id_typo_correction(sp_comp)

## One seed wall plot did not have green species comp recorded (must have forgotten in field). Assume it was the average of the other plots in this seed wall
plots_foc = sp_comp |>
  filter(year == 2022,
         str_starts(plot_id, fixed("S005")),
         metric == "green_vol") |>
  mutate(across(pipj:pico, as.numeric)) |>
  mutate(across(pipj:pico, ~ifelse(is.na(.), 0, .))) |>
  summarize(across(pipj:pico, mean))

new_row = data.frame(year = 2022,
                     plot_id = "S005-029",
                     metric = "green_vol",
                     pipj = plots_foc$pipj,
                     pila = plots_foc$pila,
                     psme = plots_foc$psme,
                     abco = plots_foc$abco,
                     cade = plots_foc$cade,
                     pico = plots_foc$pico) |>
  mutate(across(pipj:cade, as.character))

sp_comp = bind_rows(sp_comp, new_row)

table(sp_comp$pipj) |> sort()
table(sp_comp$pila) |> sort()
table(sp_comp$psme) |> sort()
table(sp_comp$abco) |> sort()
table(sp_comp$cade) |> sort()
table(sp_comp$pico) |> sort()
table(sp_comp$pimo) |> sort()
table(sp_comp$`abco/abma`) |> sort()
table(sp_comp$`abco/psme`) |> sort()

# There are two text notes that need to be changed to a value per the contents of the notes
sp_comp[which(str_detect(sp_comp$pipj, fixed("should be 0"))), "pipj"] = "0"

# Whtere there are 3 or >3 untorched PIPJ and PILA in a plot doesn't really matter, so make it quantitative
sp_comp[which(str_detect(sp_comp$pipj, fixed(">3"))), "pipj"] = "3"
sp_comp[which(str_detect(sp_comp$pila, fixed(">3"))), "pila"] = "3"


## Deal with some text notes in the comp data (just "MISSING") -- remove the values, since I noted elsewhere which two plots had missing values
sp_comp = sp_comp |>
  mutate(across(pipj:`abco/psme`, ~ifelse(.x == "MISSING", NA, .x)))

## for plots where the comp sums to > 100, divide the comp values by the total comp so that they sum to 100
sp_comp_norm = sp_comp |>
  mutate(across(pipj:"abco/psme", as.numeric)) |> # TODO for 2021: when including 2021 data, there are a couple cells that are non-numeric (text notes) that need to be dealt with
  mutate(comp_total = rowSums(across(pipj:"abco/psme"), na.rm = TRUE) / 100) |>
  mutate(comp_total = ifelse(metric %in% c("green_n", "untorched_n"), 1, comp_total)) |> # don't normalize the count columns
  mutate(comp_total = ifelse(comp_total == 0, 1, comp_total)) |>
  mutate(across(pipj:`abco/psme`, ~./comp_total))

sp_comp = sp_comp_norm |>
  select(-comp_total)

## Check if any plots are double-entered
inspect = sp_comp |>
  filter(metric == "green_vol") |>
  mutate(duplicated = duplicated(paste0(plot_id, "_", year))) |>
  filter(duplicated == TRUE)
inspect

## Check that the comp data matched the plot IDs of the main plot data
plot_ids = unique(paste0(plots$plot_id, "_", plots$Year))
comp_plot_ids = unique(paste0(sp_comp$plot_id, "_", sp_comp$year))

plot_no_comp = setdiff(plot_ids, comp_plot_ids)
comp_no_plot = setdiff(comp_plot_ids, plot_ids)

plot_no_comp
comp_no_plot

# The only comp observation that doesn't have a plot is S07B, as expected (see above)

# The only plots that don't have comp observations are the seed wall plots surveyed in previous
# years, which is expected


# # For green, get percent of the green by species (using green vol comp)
# # For brown, multiply brown cover by pre-drop species comp (using pre-drop cover)
# # also use overall pre-drop species comp by volume
# # so we need to get pre-drop vol and green vol by species and species group

# # make composite-species (species group) columns by summing the cover/volume across member species.
# sp_comp = sp_comp %>%
#   filter(metric %in% c("untorched_vol", "untorched_cov", "green_vol")) %>%
#   mutate(across(pipj:`abco/psme`, ~as.numeric(as.character(.x)))) %>%
#   rowwise() %>%
#   mutate(ALL = sum(pipj,pila,psme,abco,cade,`abco/abma`, `abco/psme`, pico, pimo, na.rm=TRUE),
#          YLWPINES = pipj,
#          PINES = sum(pipj,pila,pico, pimo, na.rm=TRUE),
#          FIRS = sum(psme,abco,`abco/abma`, `abco/psme`,na.rm=TRUE),
#          ABIES = sum(abco,`abco/abma`, na.rm=TRUE)) %>%
#   select(-`abco/abma`, -`abco/psme`, -`(add cols for other spp)`) %>% ###!!! NOTE: dropping trees that were abco/abma ambiguous They are already incorporated into FIRS and ABIES. For analysis, need to keep in mind that can't use these plots for species-specific ABCO analysis. Or just do all analysis for ABIES.
#   rename_with(~toupper(.), pipj:pimo) %>%
#   #TODO for 2021: Make sure there are no duplicate entries. There would be for revisited plots unless those plot IDs are changed to something like "year_plotid"
#   pivot_wider(names_from=metric, values_from = c("PIPJ","PILA","PSME","ABCO","CADE","PICO", "PIMO","ALL","YLWPINES","PINES","FIRS","ABIES")) %>% # if there are any warnings here about list columns, or ambiguous/multiple values per row, it probably means there were duplicated rows, so need to address that before this stip.
#   # if it's NA then it's 0
#   mutate(across(-plot_id,~ifelse(is.na(.x),0,.x)))

# # pull comp into plot data
# plots_w_comp = left_join(plots_complete,sp_comp) # TODO for 2021: make sure it only joins by plot_id (or year_plotid if you switch to that nomenclature, or possibly plot_id and year together)



#### If the plot is a core plot and had some vol brown overall (vol_brn_50m), but ALL_untorched_cov is 0 (meaning the species breakdown of the cover was not recorded) and it's not a seedwall plot, set all the untorched covers to -5 because it means it was not entered properly
## same for green ^
#### TODO for 2021: See if there are any plots that are found by the lines below that have any meaningful discrepancies like this and see if they can be fixed by returning to the paper dataseheets.

# From the plot data, pull in vol_brn_50m, and compute the sum of the species-specific untorched
# volume from the sp_comp sheet to make sure all plots with untorched cover had its composition
# recorded (but exlucde seed wall plots because this wasn't recorded for them)
plots_simp = plots |>
  select(plot_id, plot_type, year = Year, vol_brn_50m)

comp_with_volbrn = left_join(sp_comp, plots_simp) |>
  filter(metric == "untorched_vol") |>
  filter(plot_type != "seedwall") |>
  mutate(ALL_untorched_vol = rowSums(across(pipj:`abco/psme`), na.rm=TRUE))

# ^ this approach was abandoned because of the fact that untorched species comp may have only been
# recorded at the first plot survey, so there could be 

# Now our goal is a data frame with the the green_vol, untorched_vol, and untorched_cov for each
# plot, but only for plots were these were recorded (i.e. drop rows with all NAs), and if a row had
# some values and some blanks (NAs), set those NAs to 0.

sp_comp2 = sp_comp |>
  select(-`(add cols for other spp)`) |>
  filter(metric %in% c("green_vol", "untorched_vol", "untorched_cov")) |>
  # If all species values are NA, remove the row
  mutate(all_na = if_all(pipj:`abco/psme`, ~is.na(.x))) |>
  filter(all_na == FALSE) |>
  select(-all_na) |>
  # If any species values are NA, set them to 0
  mutate(across(pipj:`abco/psme`, ~ifelse(is.na(.x), 0, .x)))


# Save out
write_csv(sp_comp2, datadir("field-data/processed/allplots/cleaned/plots_w_comp.csv"))
