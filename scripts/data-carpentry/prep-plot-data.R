# Purpose: Prepare manually entered plot data for analysis

library(tidyverse)
library(here)
library(sf)
library(readxl)
library(exifr)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))


#### Load plot data ####

plots = read_excel(datadir("field-data/raw/dispersal-data-entry-2021.xlsx"),sheet="plot_main") %>%
  mutate(date = as.character(date)) %>%
  filter(! plot_id %in% c("S100-1","S100-2")) %>% # these were entered twice, but with different names (one set with dash, one set without)
  filter(!(plot_id=="S027094" & entered_by == "Diego")) # this was entered twice by two differnt people

plots[plots$plot_id == "T009", "date"] = "2021-07-02" # incorrectly recorded date


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



#### For plots that don't have coords, get coords from the photo EXIF

for(i in 1:nrow(plots)) {
  
  plot = plots[i,]
  
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

## create a compiled lat/long column and a column indicating if coords from photos
plots = plots %>%
  mutate(lat = ifelse(!is.na(Northing),Northing,photo_lat),
         lon = ifelse(!is.na(Easting),Easting,photo_lon),
         coords_from_photo = is.na(Northing) | is.na(Easting))



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


#### Load seedling data

seedl = read_excel(datadir("field-data/raw/dispersal-data-entry-2021.xlsx"), sheet="seedls_cones") %>%
  # correct some plot IDs that don't match the main plot table
  mutate(plot_id = recode(plot_id,
                          "C062259" = "C062-259",
                          "C062332" = "C062-332",
                          "S012015" = "S102015",
                          "S0427094" = "S027094",
                          "S43B" = "S043B",
                          "C38" = "C28",
                          "S100-1" = "S1001"
                          ))




#### Prep seedling data
####!!!! #TODO: Set "+" seedlings counts as the midpoing of the categories??

# Compute seedlings/sqm and cones/sqm, filter anomalously entered values
seedl = seedl %>%
  mutate(radius = ifelse(radius == "n/a" | radius == "MISSING", 8, radius),
         cones = recode(cones,"50+" = "50","n/a"="0"),
         species = recode(species,"ANY" = "PIPJ"), # this is a record to show they surveyed the plot but found nothing. So change to an actual species (won't affect count because count is 0)
         seedwall_cones = str_replace(seedwall_cones,fixed("+"),""),
         seedwall_cones = recode(seedwall_cones, "No, too steep" = "na", "MISSING" = "na")) %>%
  mutate(radius = as.numeric(radius),
         seedlings = as.numeric(seedlings),
         cones = as.numeric(cones)) %>%
  mutate(seedl_dens = seedlings / (3.14*radius^2),
         cone_dens = cones / (3.14*8^2))
  
seedl[which(seedl$seedwall_cones == "na"),"seedwall_cones"] = NA

seedl = seedl %>%
  mutate(seedwall_cones = as.numeric(seedwall_cones),
         seedwall_cone_dens = seedwall_cones / (3.14*8^2))

seedl_simp = seedl %>%
  select(plot_id,species,seedl_dens,cone_dens,seedwall_cone_dens) %>%
  # remove some types of cones that are not relevant
  ####!!!! #TODO decide if appropriate to exclude an eaten cone. Does it still represent overstory fecundity relevant to regen?
  filter(!(species %in% c("immature PIPO or PICO (open)"))) %>% # exclude eaten becaues it is not a cone that dispersed seeds.
  mutate(species = recode(species,"EATEN PILA" = "PILA")) %>%
  group_by(plot_id,species) %>%
  summarize(across(everything(),~sum(.x,na.rm=TRUE)))
  
# Compute total across all species (and also for pines and firs--incl douglas) for each plot and append

seedl_tot = seedl %>%
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                   cone_dens = sum(cone_dens, na.rm=TRUE),
                   seedwall_cone_dens = sum(seedwall_cone_dens, na.rm=TRUE)) %>%
  mutate(species = "ALL")

seedl_pines = seedl %>%
  filter(species %in% c("PICO","PIJE","PILA","PILA/PIPJ","PIPJ","PIPJ/PILA","PIPO","PIxx","PIXX")) %>%
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                   cone_dens = sum(cone_dens, na.rm=TRUE),
                   seedwall_cone_dens = sum(seedwall_cone_dens, na.rm=TRUE)) %>%
  mutate(species = "PINES")
  
seedl_ylwpines = seedl %>%
  filter(species %in% c("PIJE","PIPJ","PIPO")) %>%
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                   cone_dens = sum(cone_dens, na.rm=TRUE),
                   seedwall_cone_dens = sum(seedwall_cone_dens, na.rm=TRUE)) %>%
  mutate(species = "YLWPINES")

seedl_firs = seedl %>%
  filter(species %in% c("ABCO","ABCO/ABMA","ABCO/PSME","PSME")) %>%
  group_by(plot_id) %>%
  dplyr::summarize(seedl_dens = sum(seedl_dens, na.rm=TRUE),
                   cone_dens = sum(cone_dens, na.rm=TRUE),
                   seedwall_cone_dens = sum(seedwall_cone_dens, na.rm=TRUE)) %>%
  mutate(species = "FIRS")

# exclude ambiguous species IDs. Assume the seedlings weren't there.
###!!! TODO: test the sensitivity to this assumption
seedl_simp = seedl_simp %>%
  filter(species %in% c("ABCO","CADE","PICO","PILA","PSME"))

seedl_comb = bind_rows(seedl_simp,seedl_tot,seedl_ylwpines,seedl_pines,seedl_firs)

### todo: column for has seedwall cones measured
### todo: column for plot has species ambiguity. currently assuming zero counts for all plots with species ambiguity, and looking at it by PINES/FIRS


## make wide
# first make sure identifying cols are unique
a = duplicated(paste0(seedl_comb$plot_id,seedl_comb$species))

seedl_wide = pivot_wider(seedl_comb, id_cols = "plot_id", names_from = c("species"), values_from=c("seedl_dens","cone_dens","seedwall_cone_dens"))

# cols with NAs are zeros
seedl_wide = seedl_wide %>%
  mutate(across(everything(),~ ifelse(is.na(.x),0,.x)))

## TODO: check for mismatched plot names: plots that have NO records in the seedling table may be miswritten in one table or the other. Same for seedsource.


### If a plot had zero seedwall cone records (even zeros) for ALL species, then make all its seedwall cone entries for all species and species groups zero
## The reason is that some plots may not have had a seedwall cone plot surveyed.
## TODO: see if there is a better way to determine whether it would have had a seedwall plot surveyed (e.g. all seedwall plots after a certain date?)
##!! NOTE current approach is likely dropping some true zeros (dropping all seedwall cone counts from pltos that had no seedwall cones recorded)

no_seedwall_cone_records = seedl %>%
  select(plot_id,seedwall_cones) %>%
  mutate(has_seedwall_cone_entry = !is.na(seedwall_cones)) %>%
  group_by(plot_id) %>%
  summarize(n_spp_w_seedwall_cone_entries = sum(has_seedwall_cone_entry)) %>%
  filter(n_spp_w_seedwall_cone_entries == 0) %>%
  pull(plot_id)

seedwall_cone_cols = grepl("seedwall_cone_dens",names(seedl_wide),)
seedl_wide[seedl_wide$plot_id %in% no_seedwall_cone_records,seedwall_cone_cols] = NA

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

green_seedsource = read_excel(datadir("field-data/raw/dispersal-data-entry-2021.xlsx"),sheet="green_seedsource") %>%
  # correct some plot IDs that don't match the main plot table
  mutate(plot_id = recode(plot_id,
                          "C062259" = "C062-259",
                          "C062332" = "C062-332",
                          "C38" = "C28"
  ))




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
# get the column numbers
plots_complete = plots_complete %>%
  mutate(across(starts_with("cone_dens_"), ~ifelse(is.na(.x),0,.x)  )) %>%
  mutate(across(starts_with("seedl_dens_"), ~ifelse(is.na(.x),0,.x)  ))



#### Compute 
# 50 m cover (vol * predrop cov): % green, % brown
plots_complete = plots_complete %>%
  mutate(cov_brn_50m = vol_brn_50m * cov_predrop_50m / 100,
         cov_grn_50m = vol_grn_50m * cov_predrop_50m / 100)


write_csv(plots_complete,datadir("field-data/processed/plot_seedl_cone_grnseedsource.csv"))


