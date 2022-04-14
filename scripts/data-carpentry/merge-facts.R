# Purpose: Prepare manually entered plot data for analysis

library(tidyverse)
library(here)
library(sf)
library(readxl)

# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))
