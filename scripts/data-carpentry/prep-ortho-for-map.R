library(terra)
library(here)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))

# load full res ortho
r = rast(file.path(data_dir, "orthomosaic/Caldor-C013-S012_20230305T0046_ortho_dsm.tif"))

# load mask polygon
m = vect(file.path(data_dir, "orthomosaic/c013-mask-2.gpkg"))
m = project(m, crs(r))

r = aggregate(r, fact = 8)
r = crop(r, m)
r = mask(r, m)


writeRaster(r, file.path(data_dir, "orthomosaic/Caldor-C013_cropped_agg.tif"), overwrite = TRUE)
