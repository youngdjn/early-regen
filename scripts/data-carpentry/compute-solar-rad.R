## Take a DEM and compute solar rad

library(terra)
library(suncalc)
library(here)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))

fire = "dixie"

if(fire == "caldor") {
  perim = vect(datadir("fire-perims/caldor/ca3858612053820210815_20201011_20211016_burn_bndy.shp"))
} else if(fire == "dixie") {
  perim = vect(datadir("fire-perims/dixie/ca3987612137920210714_20201012_20211015_burn_bndy.shp"))
}

# Load DEM
dem = rast(datadir("dem/CAmerged15_albers.tif"))

# Crop and mask to perim
perim = project(perim, crs(dem))
perim = buffer(perim, 5000) # allow solar shading from up to 5 km away

dem = crop(dem,perim)
dem = mask(dem,perim)

# Compute slope and aspect in radians
slope <- terrain(dem,v="slope",unit="radians")
aspect <- terrain(dem,v="aspect",unit="radians")

# Get average latitude and longitude for the fire
#     Note, must reproject to latlong to get latitude and longitude
geo.prj="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
perim = project(perim, geo.prj)
cent <- centroids(perim)
lat <- as.vector(ext(cent))[3] #latitude
lon <- as.vector(ext(cent))[1] #longitude

# Create vector of dates for growing season
grow_dates<-c('04/15/2021','05/15/2021','06/15/2021','07/15/2021','08/15/2021','09/15/2021')
grow_dates<-as.Date(grow_dates,format='%m/%d/%y')
grow_dates<-julian(grow_dates)
grow_dates<-as.Date(grow_dates,origin=as.Date("1970-01-01"))

# Calculate sunrise/sunset for growing season days
solar_period<-getSunlightTimes(date=grow_dates,lat=lat,lon=lon,
                               keep=c('sunrise','sunset'),tz='US/Pacific')

# Compute solar period for each day
solar_period$solar_period<-as.numeric(solar_period$sunset-solar_period$sunrise)


### Loop through each growing season day (midpoint of the growing-season months) and compute SRI
for(i in 1:nrow(solar_period)){
  
  # Select day
  day <- solar_period[i,]
  
  # Find 10 equal spaced times between sunrise and sunset
  z <- day$solar_period/11
  
  # Loop through times of day  
  for(j in 1:10){
    
    print(paste0('processing day ',i,' of ',nrow(solar_period),' time ',j,' of 10'))
    
    # Compute day-time
    day_time <- day$sunrise+(z*j*60*60)
    
    #getSunlightPosition
    sunlight_position <- getSunlightPosition(date=day_time, lat=lat, lon=lon)
    
    # Convert altitude and azimuth to degrees
    sunlight_position$angle <- sunlight_position$altitude*(180/pi)
    sunlight_position$direction <- sunlight_position$azimuth*(180/pi)
    
    # Rotate direction by 180 degrees so that 0 is north
    sunlight_position$direction <- sunlight_position$direction+180
    
    # Compute shade raster
    shade <- shade(slope=slope, aspect=aspect, angle=sunlight_position$angle, direction=sunlight_position$direction,
                   normalize=TRUE)
    
    if(j == 1) { 
      shade_stack = shade
    } else {
      shade_stack <- c(shade_stack, shade)
    }
  }
  
  # Sum across the 10 timepoints for the day to get daily rad
  day_sum <- sum(shade_stack, na.rm=TRUE)
  
  # Multiply by day solar_period
  day_sri <- day_sum * day$solar_period
  
  # Stack across days
  if(i == 1) {
    sri_stack = day_sri
  } else {
    sri_stack = c(sri_stack, day_sri)
  }
  
}

# Sum across days to get growing-season SRI sum
sri_tot = sum(sri_stack, na.rm=TRUE)

# Write
name = paste0("sri/sri_", fire, ".tif")
writeRaster(sri_tot, datadir(name), datatype='FLT4S', overwrite=TRUE)
