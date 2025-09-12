# select the priority plots to resurvey (the ones selected for the first year analysis)
library(tidyverse)
library(sf)

d = read_csv("~/Documents/temp/dixie_priority_plots.csv")$plot_id

# add two more random plots
d = c(d, "C10-003", "C12-006")


# load the plots KML
kml1 = st_read("/home/derek/Documents/repo-data-local/early-regen_data/resurvey-prep/previous-plots_core.kml")
kml2 = st_read("/home/derek/Documents/repo-data-local/early-regen_data/resurvey-prep/previous-plots_seedwall.kml")

kml = bind_rows(kml1, kml2)

foc = kml[kml$Name %in% d,]

st_write(foc, "/home/derek/Documents/repo-data-local/early-regen_data/resurvey-prep/previous-plots_dixie-priority.kml", delete_dsn = TRUE)
