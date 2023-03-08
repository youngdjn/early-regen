library(tidyverse)
library(here)

# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)

d = read_csv(file.path(datadir, "external-field/compiled_welch-young.csv"))

min_nonzero = d[d$conif_seedl_sqm > 0, "conif_seedl_sqm"] |> min()
min_nonzero = 0.005

# include a random jitter
d = d |>
  mutate(conif_seedl_sqm_nozero = ifelse(conif_seedl_sqm == 0, min_nonzero - runif(n = nrow(d), min = 0, max = .004), conif_seedl_sqm))

# remove observations that are >= 400 m because those can't be measured by laser, must be shorthand for not observed
d = d |>
  filter(distance_seed_source < 400) |>
  filter(distance_seed_source != 200)


p = ggplot(d, aes(x = distance_seed_source, y = conif_seedl_sqm_nozero)) +
  geom_point(pch = 1) +
  scale_y_continuous(breaks = c(.001, .005, 0.01,.1,1,10,100, 1000), labels = c("[0]", "[0]","0.01","0.1","1","10","100","1000" ), minor_breaks = c(0.005, 0.05, 0.5, 5.0, 50, 500)) +
  coord_trans(y = "log") +
  theme_bw(14) +
  labs(x = "Distance to seed source (m)",
       y = "Conifer seedlings / sq m")
p

png(file.path(datadir, "figures/welch-young-seedl-dens.png"), res = 300, width = 1000, height = 1000)
p
dev.off()


# calc the median density in a few distance bins

d_summ = d |>
  mutate(dist_bins = cut(distance_seed_source, breaks = c(-1,25,50, 75, 100, 125, 150, 175, 200, 300), labels = c(25, 50, 75, 100, 125, 150, 175, 200, 300))) |>
  group_by(dist_bins) |>
  summarize(median_dens = median(conif_seedl_sqm),
            mean_dens = mean(conif_seedl_sqm))


