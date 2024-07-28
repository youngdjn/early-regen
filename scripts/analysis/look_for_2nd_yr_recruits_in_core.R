#C021-523 may have had its 2023 ABCOs mislabeled as 0y when they were 1yr


library(tidyverse)
library(sf)

d = read_csv("~/Downloads/plot_data_2023.csv")

# All sp

# Filter to core plots with no seed source within 60 m

d2 = d |>
    filter((dist_grn_ALL > 60 | is.na(dist_grn_ALL)) & as.numeric(sight_line) > 60,
           vol_grn_50m == 0,
           !(plot_id %in% c("D042-209", "D042-506", "C021-523", "S005-036", "S11-255")))


# show seedling density by plot

# d2 |>
#     ggplot(aes(x = plot_id, y = seedl_dens_ALL)) +
#     geom_boxplot() +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#     labs(y = "Seedling density (stems/m2)", x = "Plot") +
#     scale_fill_viridis_d() +
#     theme(legend.position = "none") +
#     theme(axis.title.x = element_blank())

d3 = d2 |>
    select(Fire, plot_id, seedl_dens_ALL, dist_grn_ALL, sight_line, lat, lon) |>
    arrange(-seedl_dens_ALL) |>
    mutate(seedl_dens_ALL = ifelse(is.na(seedl_dens_ALL), 0, seedl_dens_ALL)) |>
    filter(!is.na(lat))

nonzero_seedl = d3$seedl_dens_ALL[d3$seedl_dens_ALL > 0]
median(nonzero_seedl, na.rm = TRUE)

dsp = st_as_sf(d3, coords = c("lon", "lat"), crs = 4326)

st_write(dsp, "~/Downloads/seedl_dens_2023_formapping.gpkg")

### Save just the selected core plots, with their new IDs

core_selected = d3 |>
    mutate(plot_prefix = recode(plot_id,
                            "C041-160" = "T1",
                            "C041-163" = "T1",
                            "C041-167" = "T1",
                            "C041-159" = "T1",
                            "D042-505" = "T2",
                            "C010-503" = "T2",
                            "C006-044" = "T1",
                            "C006-046" = "T2",
                            "C003-015" = "T1",
                            "C003-014" = "T1",
                            "C003-013" = "T2",
                            "C003-017" = "T2",
                            .default = "")) |>
    filter(plot_prefix != "") |>
    mutate(new_plot_id = paste0(plot_prefix, "_", plot_id)) |>
    select(Name = new_plot_id, everything()) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

st_write(core_selected, "~/Downloads/shrubclearing_core_selected.kml", delete_dsn = TRUE)

### Save all core plots, minus these

core_unselected = d3 |>
    filter(!(plot_id %in% core_selected$plot_id)) |>
    select(Name = plot_id, everything()) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

st_write(core_unselected, "~/Downloads/shrubclearing_core_unselected.kml", delete_dsn = TRUE)

### Save all the seed wall plots

seedwall_selected = d |>
    filter(str_sub(plot_id, 1, 1) == "S",
           dist_grn_ALL < 60,
           Fire == "Caldor",
           seedl_dens_ALL > 0.003) |>
    select(Name = plot_id, everything()) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
st_write(seedwall_selected, "~/Downloads/shrubclearing_seedwall_selected.kml", delete_dsn = TRUE)

seedwall_unselected = d |>
    filter(str_sub(plot_id, 1, 1) == "S",
           dist_grn_ALL < 60,
           Fire == "Caldor") |>
    filter(seedl_dens_ALL <= 0.003 | is.na(seedl_dens_ALL)) |>
    select(Name = plot_id, everything()) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

st_write(seedwall_unselected, "~/Downloads/shrubclearing_seedwall_unselected.kml", delete_dsn = TRUE)


## What is seed wall seedling density?
d4 = d |>
    filter(dist_grn_ALL < 60 ,
            str_sub(plot_id, 1, 1) == "S")
    
d5 =  d4 |>
    select(Fire, plot_id, seedl_dens_ALL, dist_grn_ALL, sight_line, lat, lon) |>
    arrange(-seedl_dens_ALL)

median(d5$seedl_dens_ALL, na.rm = TRUE)
