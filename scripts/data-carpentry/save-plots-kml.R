# Purpose: Save plot locations as a KML for loading into Avenza

library(tidyverse)
library(here)
library(sf)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)


d = st_read(file.path(data_dir, "field-data/processed/early-regen-2022.gpkg"))

d = d |>
  select(Name = plot_id)

st_write(d, file.path(data_dir, "field-data/processed/early-regen-2022_idonly.kml"))

