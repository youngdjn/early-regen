
prep_d_sp = function(sp) {
  
  dist_grn_var = paste0("dist_grn_", sp)
  seedl_dens_var = paste0("seedl_dens_",sp)
  untorched_vol_abs_var = paste0(sp, "_untorched_vol_abs")
  grn_vol_var = paste0(sp, "_green_vol")
  grn_vol_abs_var = paste0(sp, "_green_vol_abs")
  under_cones_new_var = paste0("under_cones_new_", sp)
  cone_dens_var = paste0("cone_dens_", sp)
  prefire_prop_var = paste0("prefire_prop_", sp)
  
  d_sp = d |>
    select(fire, plot_id, date, shrub_cover, shrub_ht, nongrowing_cover, capable_growing_area, litter_cover,
           litter_depth, moss_cover, branches_cover, trample, plot_type, ba_factor, ba_tally,
           vol_grn_50m, vol_brn_50m, vol_blk_50m, vol_grn_10m, sight_line, mean_tree_dbh, seedwall_density_cat,
           mean_seedwall_height, day_of_burning, sri, ppt, tmean, fire_intens, fire_intens2, fire_intens10, PINES_green_vol,
           dist_sw = dist_grn_ALL, # seed wall seed sources are only ever recorded under ALL. This var is not meaningful for core area plots.
           dist_grn_sp = starts_with(dist_grn_var),
           seedl_dens_sp = starts_with(seedl_dens_var),
           untorched_vol_abs_sp = starts_with(untorched_vol_abs_var),
           grn_vol_sp = matches(paste0("^",grn_vol_var,"$")),
           grn_vol_abs_sp = starts_with(grn_vol_abs_var),
           under_cones_new_sp = starts_with(under_cones_new_var),
           cone_dens_sp = starts_with(cone_dens_var),
           prefire_prop_sp = matches(paste0("^", prefire_prop_var, "$"), ignore.case = FALSE),
           starts_with("seedl_dens_"),
           starts_with("prefire_prop_")) |>
    filter(ifelse(plot_type == "seedwall", grn_vol_sp > 20, prefire_prop_sp > 20)) |> # the criteria for keeping the plot based on its species comp depend on whether it's a seed wall or core area plot (for seed wall use seed wall comp)
    filter(ifelse(plot_type == "seedwall", vol_grn_10m < 20, TRUE)) |> # seed wall plots need to have < 20% pre-fire green canopy within 10 m
    mutate(under_cones_new_sp = recode(paste0("level_", under_cones_new_sp), "level_0" = "low", "level_1" = "low", "level_2" = "high"))
  
  if(! sp %in% c("ABIES","ABCO", "CADE")) {
    d_sp = d_sp |>
      mutate(cone_dens_sp_log = log(ifelse(cone_dens_sp == 0, 0.5/(3.14*8^2), cone_dens_sp)))
  }
  
  return(d_sp)
}


#### Plot raw seedling density data vs day of burning

plot_raw_data = function(d_sp, axis_label, plot_title, filename) {
  
  # make zeros nonzero
  d_sp = d_sp |>
    mutate(seedl_dens_sp = ifelse(seedl_dens_sp < 0.00001, 0.0005, seedl_dens_sp))
  
  ## Turn day of burning to a date
  d_sp = d_sp |>
    mutate(date_of_burning = ymd("2021-01-01") + day_of_burning-1) # For fires the burned in other years will need more flexible here
  
  # core area plots, far from any green of the focal speices
  d_sp_nogrn = d_sp |>
    filter(grn_vol_abs_sp == 0,
           ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
           plot_type %in% c("core", "delayed"))
  
  # seed wall plots
  d_sp_sw = d_sp |>
    filter(plot_type == "seedwall") |>
    filter(dist_sw <= 60) |>
    # classify into near and far seed wall plots
    mutate(dist_sw_cat = ifelse(dist_sw < 30, "Very near", "Near")) |>
    mutate(dist_sw_cat = as.factor(dist_sw_cat)) |>
    mutate(dist_sw_cat = factor(dist_sw_cat, levels = c("Very near", "Near")))
  
  median_scorching_extent = median(d_sp_nogrn$fire_intens2)
  median_scorching_extent = 85
  cat("Median scorching extent", median_scorching_extent, "\n")
  
  # prep for figure: classify fire intens
  d_sp_nogrn_fig = d_sp_nogrn |>
    mutate(fire_intens2_cat = ifelse(fire_intens2 < median_scorching_extent, "Scorched", "Torched")) |>
    mutate(fire_intens_cat_foc = fire_intens2_cat)

  
  ### Context figure: plot day of burning, precip, and plot type
  allplots = bind_rows(d_sp_nogrn_fig, d_sp_sw) |>
    mutate(plot_type = recode(plot_type, "delayed" = "core")) # this may select some delayed mortality plots that behave as core plots because they're > 100 m from green.
  
  d_fig = allplots |>
    mutate(plot_type = recode(plot_type, "core" = "Interior", "seedwall" = "Edge"))
  
  d_fig$plot_type_w_prox = d_fig$plot_type
  d_fig[d_fig$plot_type == "Edge" & d_fig$dist_sw_cat == "Near",]$plot_type_w_prox = "Edge near"
  d_fig[d_fig$plot_type == "Edge" & d_fig$dist_sw_cat == "Very near",]$plot_type_w_prox = "Edge very near"
  
  d_fig = d_fig |>
    mutate(plot_type_w_prox = factor(plot_type_w_prox, levels = c("Edge very near", "Edge near", "Interior")))
  
  p = ggplot(d_fig, aes(x = date_of_burning, y = ppt, color = fire, shape = plot_type_w_prox)) +
    geom_jitter(width = 3, size = 3) +
    theme_bw(15) +
    scale_shape_manual(name = "Plot type", values = c(19, 1, 17)) +
    scale_color_viridis_d(name = "Fire", begin = 0.2, end = 0.8) +
    labs(x = "Date of burning", y = "Mean annual precipitation (mm)")
  
  png(file.path(datadir, paste0("figures/supp_ppt_dob_", filename, ".png")), res = 200, width = 1500, height = 1100)
  print(p)
  dev.off()  
  
  
  # make a copy of the constant df defining the windows in order to store the median vals for this species
  windows_foc = windows |>
  # convert the windows day of year to date
    mutate(across(c(start,end), ~ (ymd("2021-01-01") + . - 1)))
    
  # compute median seel density by seedwall, scorch, torch within each predefined date window
  for(i in 1:nrow(windows_foc)) {
    
    window = windows_foc[i,]
    
    core_blk_foc = d_sp_nogrn_fig |>
      filter(fire == window$fire) |>
      filter(between(date_of_burning, window$start, window$end)) |>
      filter(fire_intens_cat_foc == "Torched",)
    core_blk_median = median(core_blk_foc$seedl_dens_sp)
    
    core_brn_foc = d_sp_nogrn_fig |>
      filter(fire == window$fire) |>
      filter(between(date_of_burning, window$start, window$end)) |>
      filter(fire_intens_cat_foc == "Scorched")
    core_brn_median = median(core_brn_foc$seedl_dens_sp)
    
    sw_foc = d_sp_sw |>
      filter(fire == window$fire) |>
      filter(between(date_of_burning, window$start, window$end))
    sw_median = median(sw_foc$seedl_dens_sp)
    
    windows_foc[i,"seedwall_median"] = sw_median
    windows_foc[i,"core_blk_median"] = core_blk_median
    windows_foc[i,"core_brn_median"] = core_brn_median
    
  }
  
  # Make fig
  p = ggplot(d_sp_nogrn_fig, aes(x = date_of_burning, y = seedl_dens_sp)) +
    geom_hline(yintercept = 0.0173, linetype = "dashed", color="gray70") +
    #geom_hline(yintercept = 0.0005, color = "orange") +
    geom_jitter(data = d_sp_sw, color="#A2D435", size=4, height=0, width=2, aes(shape=dist_sw_cat)) +
    geom_jitter(size=4, height = 0, width=2, aes(color = fire_intens_cat_foc)) +
    labs(shape = "Edge") +
    scale_color_manual(values = c(Torched = "black", Scorched = "#9D5B0B"), name = "Interior") +
    scale_shape_manual(values = c("Near" = 1, "Very near" = 19)) +
    facet_grid(~fire) +
    theme_bw(15) +
    theme(strip.background = element_rect(fill = "white"),
          strip.text.x = element_text(size = 16),
          legend.position = c(0.12,.67),
          legend.background = element_blank(),
          legend.box.background = element_rect(fill="white", color = "black", linewidth = 0.3)) +
    labs(x = "Day of Burning", y = axis_label, title = plot_title) +
    scale_y_continuous(breaks = c(0.0005, .001,.01,.1,1,10,100), minor_breaks = c(0.005, 0.05, 0.5, 5.0, 50), labels = c("[0]", "0.001","0.01", "0.1", "1", "10","100")  ) +
    scale_x_date(date_labels = "%d-%b", minor_breaks = NULL) +
    coord_trans(y = "log") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = seedwall_median, yend = seedwall_median), linewidth = 1.5, color = "white") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_blk_median, yend = core_blk_median), linewidth = 1.5, color = "white") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_brn_median, yend = core_brn_median), linewidth = 1.5, color = "white") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = seedwall_median, yend = seedwall_median), linewidth = 1, color = "#A2D435") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_blk_median, yend = core_blk_median), linewidth = 1, color = "black") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_brn_median, yend = core_brn_median), linewidth = 1, color = "#9D5B0B")

  png(file.path(datadir, paste0("figures/raw_data_", filename, ".png")), res = 350*2, width = 4500*2, height = 2400*2)
  print(p)
  dev.off()
  
  return(median_scorching_extent)
  
}

# Prep core-area data for modeling
prep_d_core_mod = function(d_sp) {
  d_mod = d_sp |>
    filter(day_of_burning > 220) |>
    filter(grn_vol_abs_sp == 0,
           ((is.na(dist_grn_sp) | dist_grn_sp > 100) & sight_line > 100),
           plot_type %in% c("core", "delayed"))
  
  return(d_mod)
}

get_scenario_preds = function(m, d_mod, predictors, sp, percentile_exclude, interacting_predictor = NA, interacting_splits = NA) { # interacting_predictor is for a predictor var that you want to predict multiple levels for, and interacting_levels is a vector of quantiles on which to split the predictor for separate predictions

  predictor_foc_preds = data.frame()
  for(predictor_foc in predictors) {
    
    ## truncate each prediction range to exclude the upper and lower x percentile extremes of observed data for each species
    range = d_mod |>
      summarize(lwr = quantile(!!ensym(predictor_foc),percentile_exclude),
                upr = quantile(!!ensym(predictor_foc), 1-percentile_exclude))
    
    # get fitted line for hypothetical variation along focal var (all else set to mean)
    # start with a data frame of all predictors set at their means
    newdat_predictor_foc = d_mod |>
      select(fire_intens2, fire_intens10, ppt, capable_growing_area, prefire_prop_sp, vol_brn_50m, cone_dens_sp, cone_dens_sp_log, dist_grn_sp, dist_sw, grn_vol_sp) |>
      summarize_all(mean) |>
      mutate(under_cones_new_sp = "high") |> # assume high density of cones under nearby trees (need to do this manually instead of mean because it's categorical)
      mutate(seedwall_density_cat = "H") |>
      slice(rep(row_number(), 100)) # get 100 identical (repeated) rows
    
    # now make the focal col a seq from the min obs to max obs
    newdat_predictor_foc[,predictor_foc] =  seq(range$lwr, range$upr, length.out = 100)
    
    # now, replicate that for every level of the interacting predictor, if specified
    # for now, assuming two levels (one quantile split)
    if(!is.na(interacting_predictor)) {
      
      pred_vals = d_mod[,interacting_predictor]
      
      min = min(pred_vals)
      max = max(pred_vals)
      split = interacting_splits
      
      
      lwr_mid = median(pred_vals[pred_vals < split])
      upr_mid = median(pred_vals[pred_vals >= split])
      
      cat("Upper torching prediction val:", upr_mid, "\n")
      cat("Lower torching prediction val:", lwr_mid, "\n")
      
      newdat_predictor_foc_l1 = newdat_predictor_foc |>
        mutate(across(matches(paste0("^",interacting_predictor,"$")), ~lwr_mid),
               interacting_level = "low")
      
      newdat_predictor_foc_l2 = newdat_predictor_foc |>
        mutate(across(matches(paste0("^",interacting_predictor,"$")), ~upr_mid),
               interacting_level = "high")
      
      newdat_predictor_foc = bind_rows(newdat_predictor_foc_l1, newdat_predictor_foc_l2)
      
    }
    
    pred = predict(m, newdat_predictor_foc, type = "link", se.fit=TRUE)
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
    filter(plot_type == "seedwall") |>
    filter(dist_sw <= max_sw_dist)
    # optionally to use 10 m fire intens for seed wall: mutate(fire_intens2 = fire_intens10)
  
  return(d_mod)
}

make_scenario_ggplot = function(scenario_preds, d_mod, focal_predictor, predictor_label, ymin, ymax) {
  
  d_mod = d_mod |>
    mutate(seedl_dens_sp = ifelse(seedl_dens_sp < 0.5, 0.5, seedl_dens_sp)) |>
    mutate(type = as.factor(type))
    
  
  d_fig = scenario_preds |> filter(predictor_foc == focal_predictor) |>
    mutate(type = factor(type, levels = c("Interior", "Edge")))
    
    
  p = ggplot(data = d_fig, mapping = aes(x = !!ensym(focal_predictor), y = preds, color = species, fill = species, linetype = type)) +
    scale_linetype(name = "Plot type", limits = c("Interior", "Edge")) +
    scale_color_viridis_d(begin = .2, end = .8, name = "Species") +
    scale_fill_viridis_d(begin = .2, end = .8, name = "Species") +
    geom_ribbon(aes(ymin = preds_lwr, ymax = preds_upr, fill=species), color=NA, alpha = .3, show.legend = FALSE) +
    geom_line() +
    #geom_rug(data = d_mod, sides = "b", aes(x = !!ensym(focal_predictor), y = NULL, color = NULL, linetype = NULL), alpha = 0.2) +
    scale_y_continuous(breaks = c(.001,.01,.1,1,10,100, 1000), minor_breaks = c(0.0005,0.005, 0.05, 0.5, 5.0, 50, 500), limits = c(ymin, ymax), labels = label_comma()) +
    coord_trans(y = "log") +
    theme_bw() +
    labs(y = bquote(Seedlings~m^-2), x = predictor_label)
  
  p

}


make_scenario_w_ppt_ggplot = function(scenario_preds, d_mod, focal_predictor, predictor_label, ymin, ymax, interacting_splits = NA, show_data = FALSE) {
  
  
  d_mod = d_mod |> # in d_mod, seedl_dens_sp is actually count of seedlings in a 10 m radius plot (for poisson purposes)
    mutate(seedl_dens_sp = ifelse(seedl_dens_sp < 0.5, 0.0005*314, seedl_dens_sp) / 314) |> # get seedlings per sq m, but with zeros set to 0.0005
    mutate(ppt_cat = ifelse(ppt >= interacting_splits, paste0(">= ", interacting_splits, " mm"), paste0("< ", interacting_splits, " mm")))
  
  d_fig = scenario_preds |> filter(predictor_foc == focal_predictor) |>
    mutate(ppt_cat = ifelse(ppt >= interacting_splits, paste0(">= ", interacting_splits, " mm"), paste0("< ", interacting_splits, " mm")))
  
  d_mod = d_mod |>
    mutate(ppt_cat = as.factor(ppt_cat)) |>
    mutate(ppt_cat = factor(ppt_cat, levels = rev(levels(ppt_cat)))) 
  d_fig = d_fig |>
    mutate(ppt_cat = as.factor(ppt_cat)) |>
    mutate(ppt_cat = factor(ppt_cat, levels = rev(levels(ppt_cat)))) 
  
  p = ggplot(data = d_fig, mapping = aes(x = !!ensym(focal_predictor), y = preds, color = ppt_cat, fill = ppt_cat))
  
  if(show_data) {
    p = p + geom_point(data = d_mod, mapping = aes(y = seedl_dens_sp))
  }
  
  p2 = p +
    scale_color_viridis_d(option = "plasma", begin = .2, end = .8, name = "Normal annual\nprecipitation") +
    scale_fill_viridis_d(option = "plasma", begin = .2, end = .8, name = "Normal annual\nprecipitation") +
    geom_ribbon(aes(ymin = preds_lwr, ymax = preds_upr), color=NA, alpha = .3, show.legend = FALSE) +
    geom_line() +
    scale_y_continuous(breaks = c(0.0005, .001,.01,.1,1,10,100, 1000), minor_breaks = c(0.005, 0.05, 0.5, 5.0, 50, 500), limits = c(ymin, ymax), labels = c("[0]", "0.001","0.01", "0.1", "1", "10","100", "1000")) +
    coord_trans(y = "log") +
    theme_bw() +
    theme(legend.position = c(0.2,.2),
          legend.background = element_blank(),
          legend.box.background = element_rect(fill="white", color = "black", linewidth = 0.3)) +
    labs(y = bquote(Conifer~seedlings~m^-2), x = predictor_label)
  
  p2
  
}





make_scenario_w_intens_ggplot = function(scenario_preds, d_mod, focal_predictor, predictor_label, ymin, ymax, interacting_splits = NA, interacting_units = "mm", show_data = FALSE) {
  
  # prep the dataset that was used to fit the model (the observed dataset) for plotting. Because we will log transform, have to make zeros nonzero. Model is fitted with count of seedl per plot, so convert to density
  d_mod = d_mod |>
    mutate(seedl_dens_sp = ifelse(seedl_dens_sp < 0.5, 0.5, seedl_dens_sp) / 314) |>
    mutate(cone_dens_sp = ifelse(cone_dens_sp < 0.5/314, 0.5/314, cone_dens_sp)) |>
    mutate(intens_cat = ifelse(fire_intens2 >= interacting_splits, "Torched", "Scorched"))
  
  d_fig = scenario_preds |> filter(predictor_foc == "cone_dens_sp_log") |>
    mutate(intens_cat = ifelse(fire_intens2 >= interacting_splits, "Torched", "Scorched")) |>
    mutate(cone_dens_sp = exp(cone_dens_sp_log))
  
  d_mod = d_mod |>
    mutate(intens_cat = as.factor(intens_cat)) |>
    mutate(intens_cat = factor(intens_cat, levels = rev(levels(intens_cat)))) 
  d_fig = d_fig |>
    mutate(intens_cat = as.factor(intens_cat)) |>
    mutate(intens_cat = factor(intens_cat, levels = rev(levels(intens_cat)))) 
  
  p = ggplot(data = d_fig, mapping = aes(x = !!ensym(focal_predictor), y = preds, color = intens_cat, fill = intens_cat))
  
  if(show_data) {
    p = p + geom_point(data = d_mod, mapping = aes(y = seedl_dens_sp))
  }
  
  p2 = p +
    scale_color_viridis_d(begin = .2, end = .8, name = "Torching extent") +
    scale_fill_viridis_d(begin = .2, end = .8, name = "Torching extent") +
    geom_ribbon(aes(ymin = preds_lwr, ymax = preds_upr), color=NA, alpha = .3, show.legend = FALSE) +
    geom_line() +
    scale_y_continuous(breaks = c(.001,.01,.1,1,10,100, 1000), minor_breaks = c(0.0005,0.005, 0.05, 0.5, 5.0, 50, 500), limits = c(ymin, ymax), labels = label_comma()) +
    coord_trans(y = "log", x = "log") +
    theme_bw() +
    theme(legend.background = element_blank(),
          legend.box.background = element_rect(fill="white", color = "black", linewidth = 0.3)) +
    labs(y = bquote(Seedlings~m^-2), x = predictor_label)
  
  p2
  
}
