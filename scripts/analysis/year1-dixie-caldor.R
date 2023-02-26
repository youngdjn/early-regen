library(tidyverse)
library(mgcv)
library(here)

# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)


# Load data
d = read_csv(file.path(datadir,"field-data/processed/plot-data-prepped.csv"))

# Keep Caldor and Dixie 2022 only
d = d |>
  filter(fire %in% c("Caldor", "Dixie"))

# Check distrib of seedling density
hist(d$seedl_dens_ALL, breaks=200)

# Calc fire intensity (scorch vs torch) indices
d = d |>
  mutate(fire_intens =  100 - pmax(litter_cover,vol_brn_50m),
         fire_intens2 = 200 - (litter_cover + vol_brn_50m))


### Environmental context across all plots
## Precip by date burned

ggplot(d, aes(x=day_of_burning, y = ppt, color = fire)) +
  geom_point()


#### Constants

## Define the date windows over which to summarize raw data (by day of year)
windows = data.frame(fire = c("Caldor","Caldor", "Dixie", "Dixie", "Dixie"),
                     start = c(229, 239, 203, 225, 252),
                     end =   c(231, 242, 205, 230, 252))


#### Analysis

### Prep data frame for a species

prep_d_sp = function(sp) {

  dist_grn_var = paste0("dist_grn_", sp)
  seedl_dens_var = paste0("seedl_dens_",sp)
  untorched_vol_abs_var = paste0(sp, "_untorched_vol_abs")
  grn_vol_abs_var = paste0(sp, "_green_vol_abs")
  under_cones_new_var = paste0("under_cones_new_", sp)
  cone_dens_var = paste0("cone_dens_", sp)
  prefire_prop_var = paste0("prefire_prop_", sp)
  
  d_sp = d |>
    select(fire, plot_id, date, shrub_cover, shrub_ht, nongrowing_cover, litter_cover,
           litter_depth, moss_cover, branches_cover, trample, plot_type, ba_factor, ba_tally,
           vol_grn_50m, vol_brn_50m, vol_blk_50m, sight_line, mean_tree_dbh,
           mean_seedwall_height, day_of_burning, sri, ppt, tmean, fire_intens, fire_intens2,
           dist_grn_sp = starts_with(dist_grn_var),
           seedl_dens_sp = starts_with(seedl_dens_var),
           untorched_vol_abs_sp = starts_with(untorched_vol_abs_var),
           grn_vol_abs_sp = starts_with(grn_vol_abs_var),
           under_cones_new_sp = starts_with(under_cones_new_var),
           cone_dens_sp = starts_with(cone_dens_var),
           prefire_prop_sp = starts_with(prefire_prop_var)) |>
    filter(prefire_prop_var > 20)
  
  return(d_sp)
}


#### Plot raw seedling density data vs day of burning

plot_raw_data = function(d_sp) {
  
  # make zeros nonzero
  d_sp = d_sp |>
    mutate(seedl_dens_sp = ifelse(seedl_dens_sp < 0.001592357, 0.001592357, seedl_dens_sp))
  
  # core area plots, far from any green of the focal speices
  d_sp_nogrn = d_sp |>
    filter(grn_vol_abs_sp == 0,
      ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
      plot_type %in% c("core", "delayed"))
  
  # seed wall plots
  d_sp_sw = d_sp |>
    filter(plot_type == "seedwall")
  
  # prep for figure: classify fire intens
  d_sp_nogrn_fig = d_sp_nogrn |>
    mutate(fire_intens2_cat = ifelse(fire_intens2 < median(fire_intens2), "scorched", "torched"),
           fire_intens_cat = ifelse(fire_intens < median(fire_intens), "scorched", "torched"),
           vol_brn_50m_cat = ifelse(vol_brn_50m > median(vol_brn_50m), "scorched", "torched") ) |>
    mutate(fire_intens_cat_foc = fire_intens2_cat)
  
  
  
  # make a copy of the constant df in order to store the median vals for this species
  windows_foc = windows
  
  # compute median seel density by seedwall, scorch, torch within each predefined date window
  for(i in 1:nrow(windows_foc)) {
    
    window = windows_foc[i,]
    
    core_blk_foc = d_sp_nogrn_fig |>
      filter(fire == window$fire) |>
      filter(between(day_of_burning, window$start, window$end)) |>
      filter(fire_intens_cat_foc == "torched",)
    core_blk_median = median(core_blk_foc$seedl_dens_sp)
    
    core_brn_foc = d_sp_nogrn_fig |>
      filter(fire == window$fire) |>
      filter(between(day_of_burning, window$start, window$end)) |>
      filter(fire_intens_cat_foc == "scorched")
    core_brn_median = median(core_brn_foc$seedl_dens_sp)
    
    sw_foc = d_sp_sw |>
      filter(between(day_of_burning, window$start, window$end))
    sw_median = median(sw_foc$seedl_dens_sp)
    
    windows_foc[i,"seedwall_median"] = sw_median
    windows_foc[i,"core_blk_median"] = core_blk_median
    windows_foc[i,"core_brn_median"] = core_brn_median
    
  }
  
  
  ggplot(d_sp_nogrn_fig, aes(x = day_of_burning, y = seedl_dens_sp)) +
    geom_hline(yintercept = 0.0173, linetype = "dashed", color="gray70") +
    geom_jitter(data = d_sp_sw, color="#A2D435", size=3, height=0, width=2, aes(shape="")) +
    geom_jitter(size=3, height = 0, width=2, aes(color = fire_intens_cat_foc)) +
    labs(shape = "Seed wall") +
    scale_color_manual(values = c(torched = "black", scorched = "#9D5B0B"), name = "Core area") +
    facet_grid(~fire) +
    theme_bw(15) +
    labs(x = "Day of Burning", y = "Seedlings / sq m") +
    scale_y_continuous(breaks = c(.01,.1,1,10,100), minor_breaks = c(0.005, 0.05, 0.5, 5.0, 50)) +
    coord_trans(y = "log") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = seedwall_median, yend = seedwall_median), linewidth = 1.5, color = "white") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_blk_median, yend = core_blk_median), linewidth = 1.5, color = "white") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_brn_median, yend = core_brn_median), linewidth = 1.5, color = "white") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = seedwall_median, yend = seedwall_median), linewidth = 1, color = "#A2D435") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_blk_median, yend = core_blk_median), linewidth = 1, color = "black") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_brn_median, yend = core_brn_median), linewidth = 1, color = "#9D5B0B")

}


d_sp = prep_d_sp("YLWPINES")
plot_raw_data(d_sp)

