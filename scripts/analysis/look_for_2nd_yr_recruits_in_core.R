#C021-523 may have had its 2023 ABCOs mislabeled as 0y when they were 1yr


library(tidyverse)

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
    arrange(-seedl_dens_ALL)

nonzero_seedl = d3$seedl_dens_ALL[d3$seedl_dens_ALL > 0]
median(nonzero_seedl, na.rm = TRUE)

## What is seed wall seedling density?
d4 = d |>
    filter(dist_grn_ALL < 60 ,
            str_sub(plot_id, 1, 1) == "S")
    
d5 =  d4 |>
    select(Fire, plot_id, seedl_dens_ALL, dist_grn_ALL, sight_line, lat, lon) |>
    arrange(-seedl_dens_ALL)

median(d5$seedl_dens_ALL, na.rm = TRUE)
