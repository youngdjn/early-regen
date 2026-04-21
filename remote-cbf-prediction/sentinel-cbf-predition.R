# Purpose: Determine how reliably Sentinel-2 can predict CBF or percent brown at first-year,
# high-severity field plots

library(tidyverse)
library(sf)
library(terra)


datadir = "/ofo-share/repos/derek/early-regen_data"

plots_filepath = paste0(datadir, "/field-data/processed/allplots/cleaned/plots.csv")
plots = read_csv(plots_filepath)

# Filter to 2022 survey of Caldor and Dixie only, no green in plot
plots = plots |>
    filter(Year == 2022, Fire %in% c("Caldor", "Dixie")) |>
    filter(vol_grn_50m == 0)

# Keep only relevant colummns and make spatial
plots = plots |>
    select(plot_id, Fire, vol_brn_50m, vol_brn_10m, cov_untorched_50m, cov_untorched_10m, litter_cover, date, ba_factor, ba_tally, pct_dead_prefire = "%_dead_prefire", lon, lat) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

plots

plots$litter_cover = as.numeric(plots$litter_cover)
plots$cov_untorched_50m = as.numeric(plots$cov_untorched_50m)
plots$cov_untorched_10m = as.numeric(plots$cov_untorched_10m)

plots$cbf50 = plots$litter_cover + plots$vol_brn_50m
plots$cbf10 = plots$litter_cover + plots$vol_brn_10m

# Create convex hulls for Caldor and Dixie plots and save as a single geopackage
hulls = plots |>
    group_by(Fire) |>
    summarize(geometry = st_convex_hull(st_union(geometry))) |>
    ungroup()

st_write(hulls, paste0(datadir, "/remote-cbf-prediction/focal_footprints_per_fire.gpkg"), delete_dsn = TRUE)

plot(hulls)

# Load and merge the Sentinel-2

caldor_jun_filepath = paste0(datadir, "/remote-cbf-prediction/sentinel-stacks/caldor/jun2022_stack_cropped.tif")
dixie_jun_filepath = paste0(datadir, "/remote-cbf-prediction/sentinel-stacks/dixie/jun2022_stack_cropped.tif")

caldor_nov_filepath = paste0(datadir, "/remote-cbf-prediction/sentinel-stacks/caldor/nov2021_stack_cropped.tif")
dixie_nov_filepath = paste0(datadir, "/remote-cbf-prediction/sentinel-stacks/dixie/nov2021_stack_cropped.tif")

caldor_jun = rast(caldor_jun_filepath)
# dixie_jun = rast(dixie_jun_filepath)

# caldor_nov = rast(caldor_nov_filepath)
# dixie_nov = rast(dixie_nov_filepath)

nov = vrt(c(caldor_nov_filepath, dixie_nov_filepath))
jun = vrt(c(caldor_jun_filepath, dixie_jun_filepath))

# Extract all sentinel bands for all plots, for both June and November stacks
plots_jun = st_transform(plots, crs(jun))
plots_nov = st_transform(plots, crs(nov))

jun_values = terra::extract(jun, vect(plots_jun), ID = FALSE, method = "nearest")
nov_values = terra::extract(nov, vect(plots_nov), ID = FALSE, method = "nearest")

band_names = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B11", "B12", "SCL")
names(jun_values) = paste0("jun_", band_names)
names(nov_values) = paste0("nov_", band_names)

# Compute spectral indices
# NDVI = (NIR - Red) / (NIR + Red) using B08, B04
jun_values$jun_NDVI = (jun_values$jun_B08 - jun_values$jun_B04) / (jun_values$jun_B08 + jun_values$jun_B04)
nov_values$nov_NDVI = (nov_values$nov_B08 - nov_values$nov_B04) / (nov_values$nov_B08 + nov_values$nov_B04)

# NBR = (NIR - SWIR2) / (NIR + SWIR2) using B08, B12
jun_values$jun_NBR = (jun_values$jun_B08 - jun_values$jun_B12) / (jun_values$jun_B08 + jun_values$jun_B12)
nov_values$nov_NBR = (nov_values$nov_B08 - nov_values$nov_B12) / (nov_values$nov_B08 + nov_values$nov_B12)


plots = bind_cols(plots, jun_values, nov_values)

# Fit random forest models

library(randomForest)

# Exclude SCL (scene classification, band 13) from predictors
ref_bands = band_names[! band_names %in% c("SCL", "B01", "B09")]
nov_cols  = paste(paste0("nov_", ref_bands), collapse = " + ")
idx_cols  = "nov_NDVI + nov_NBR"
rf_formula = as.formula(paste("cbf50 ~", nov_cols, "+", idx_cols))
# # try NBR only
# rf_formula = as.formula(paste("cbf50 ~", "nov_NBR"))


rf_model = randomForest(rf_formula, data = plots, importance = TRUE, na.action = na.omit)
rf_model

importance(rf_model)       # raw importance values (matrix)
varImpPlot(rf_model)       # quick plot


# Redo for June predictors
jun_cols  = paste(paste0("jun_", ref_bands), collapse = " + ")
idx_cols  = "jun_NDVI + jun_NBR"
rf_formula = as.formula(paste("cbf50 ~", jun_cols, "+", idx_cols))
# # try NBR only
# rf_formula = as.formula(paste("cbf50 ~", "jun_NBR"))


rf_model_jun = randomForest(rf_formula, data = plots, importance = TRUE, na.action = na.omit)
rf_model_jun

varImpPlot(rf_model_jun)       # quick plot
