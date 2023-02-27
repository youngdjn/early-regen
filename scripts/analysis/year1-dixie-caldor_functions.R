
prep_d_sp = function(sp) {
  
  dist_grn_var = paste0("dist_grn_", sp)
  seedl_dens_var = paste0("seedl_dens_",sp)
  untorched_vol_abs_var = paste0(sp, "_untorched_vol_abs")
  grn_vol_abs_var = paste0(sp, "_green_vol_abs")
  under_cones_new_var = paste0("under_cones_new_", sp)
  cone_dens_var = paste0("cone_dens_", sp)
  prefire_prop_var = paste0("prefire_prop_", sp)
  
  d_sp = d |>
    select(fire, plot_id, date, shrub_cover, shrub_ht, nongrowing_cover, capable_growing_area, litter_cover,
           litter_depth, moss_cover, branches_cover, trample, plot_type, ba_factor, ba_tally,
           vol_grn_50m, vol_brn_50m, vol_blk_50m, sight_line, mean_tree_dbh,
           mean_seedwall_height, day_of_burning, sri, ppt, tmean, fire_intens, fire_intens2,
           dist_sw = dist_grn_ALL, # seed wall seed sources are only ever recorded under ALL. This var is not meaningful for core area plots.
           dist_grn_sp = starts_with(dist_grn_var),
           seedl_dens_sp = starts_with(seedl_dens_var),
           untorched_vol_abs_sp = starts_with(untorched_vol_abs_var),
           grn_vol_abs_sp = starts_with(grn_vol_abs_var),
           under_cones_new_sp = starts_with(under_cones_new_var),
           cone_dens_sp = starts_with(cone_dens_var),
           prefire_prop_sp = starts_with(prefire_prop_var)) |>
    filter(prefire_prop_var > 20) |>
    mutate(under_cones_new_sp = recode(paste0("level_", under_cones_new_sp), "level_0" = "low", "level_1" = "low", "level_2" = "high"))
  
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
    filter(plot_type == "seedwall") |>
    filter(dist_sw < 60)
  
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
    scale_y_continuous(breaks = c(.001,.01,.1,1,10,100), minor_breaks = c(0.0005,0.005, 0.05, 0.5, 5.0, 50), labels = label_comma()) +
    coord_trans(y = "log") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = seedwall_median, yend = seedwall_median), linewidth = 1.5, color = "white") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_blk_median, yend = core_blk_median), linewidth = 1.5, color = "white") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_brn_median, yend = core_brn_median), linewidth = 1.5, color = "white") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = seedwall_median, yend = seedwall_median), linewidth = 1, color = "#A2D435") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_blk_median, yend = core_blk_median), linewidth = 1, color = "black") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_brn_median, yend = core_brn_median), linewidth = 1, color = "#9D5B0B")
  
}

# Prep core-area data for modeling
prep_d_core_mod = function(d_sp) {
  d_mod = d_sp |>
    filter(day_of_burning > 220) |>
    filter(grn_vol_abs_sp == 0,
           ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
           plot_type %in% c("core", "delayed")) |>
    mutate(seedl_dens_sp = ifelse(seedl_dens_sp > 10, 10, seedl_dens_sp),
           cone_dens_sp = ifelse(cone_dens_sp < 0.001592357, 0.001592357, cone_dens_sp))
  
  return(d_mod)
}

get_scenario_preds = function(m, d_mod, predictors, sp) {
  
  predictor_foc_preds = data.frame()
  for(predictor_foc in predictors) {
    
    # get fitted line for hypothetical variation along focal var (all else set to mean)
    # start with a data frame of all predictors set at their means
    newdat_predictor_foc = d_mod |>
      select(fire_intens2, ppt, capable_growing_area, prefire_prop_sp, vol_brn_50m, cone_dens_sp) |>
      summarize_all(mean) |>
      slice(rep(row_number(), 100)) # get 100 identical (repeated) rows
    
    # now make the focal col a seq from the min obs to max obs
    newdat_predictor_foc[,predictor_foc] =  seq(min(d_mod[,predictor_foc]), max(d_mod[,predictor_foc]), length.out = 100)
    
    pred = predict(m, newdat_predictor_foc, type = "link", se.fit=TRUE, unconditional = TRUE)
    newdat_predictor_foc$preds = pred$fit
    newdat_predictor_foc$preds_lwr = pred$fit - 2*pred$se.fit
    newdat_predictor_foc$preds_upr = pred$fit + 2*pred$se.fit
    
    newdat_predictor_foc = newdat_predictor_foc |>
      mutate(across(c(preds, preds_lwr, preds_upr), exp)) |>
      mutate(across(c(preds, preds_lwr, preds_upr), ~./314)) |> # divide by plot area to get seedl per sq m
      mutate(predictor_foc = predictor_foc,
             species = sp)
    
    predictor_foc_preds = bind_rows(predictor_foc_preds, newdat_predictor_foc)
  }
  
  return(predictor_foc_preds)
}





# Prep seedwall data for modeling
prep_d_sw_mod = function(d_sp, max_sw_dist) {
  d_mod = d_sp |>
    filter(day_of_burning > 220) |>
    filter(plot_type == "seedwall") |>
    filter(dist_grn_sp < max_sw_dist)
  
  return(d_mod)
}

make_scenario_ggplot = function(scenario_preds, focal_predictor, ymin, ymax) {
  
  d_fig = scenario_preds |> filter(predictor_foc == focal_predictor)
  # plot the fit with the raw data
  ggplot(data = d_fig, mapping = aes(x = !!ensym(focal_predictor), y = preds, color = species, fill = species)) +
    # optional to display data:
    #geom_point(data = d_mod, mapping = aes(y = ifelse(seedl_dens_sp < .5, .5, seedl_dens_sp), color = ppt > 1200)) +
    geom_ribbon(aes(ymin = preds_lwr, ymax = preds_upr, fill=species), color=NA, alpha = .3) +
    geom_line() +
    scale_y_continuous(breaks = c(.001,.01,.1,1,10,100, 1000), minor_breaks = c(0.0005,0.005, 0.05, 0.5, 5.0, 50, 500), limits = c(ymin, ymax), labels = label_comma()) +
    coord_trans(y = "log") +
    theme_bw() +
    labs(y = "Seedlings / sq m")
}
