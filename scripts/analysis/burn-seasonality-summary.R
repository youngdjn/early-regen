# Summarize tabular data on number of pixels burning per day per fire into proportion of pixels burning in a focal window

library(tidyverse)
library(here)

# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)


# Load data
d = read_csv(file.path(datadir,"day-of-burning-tabular-summary/fire_daily_area_2002_2021.csv")) |>
  mutate(month_day = str_sub(date, 6, 10))

d_s = d |>
  filter(state == "CA")

### Proportion of area burning 1 Aug to 15 Sept
area_state = sum(d_s$area)
area_window = d_s |>
  filter(between(month_day, "08-01", "09-15")) |>
  pull(area) |>
  sum()

prop_in_window = area_window / area_state
prop_in_window


### Same, but for Caldor and Dixie (combined) only
focal_fires = c("ENF-024030", "PNF-001273")

d_f = d_s |>
  filter(fire.id %in% focal_fires)

area_state = sum(d_f$area)
area_window = d_f |>
  filter(between(month_day, "08-01", "09-15")) |>
  pull(area) |>
  sum()

prop_in_window = area_window / area_state
prop_in_window
