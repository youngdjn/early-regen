
newdat_partial = d_mod |>
  select(seedl_dens_sp, fire_intens2, ppt, nongrowing_cover, prefire_prop_sp, vol_brn_50m) |>
  mutate(across(c(-seedl_dens_sp, -fire_intens2), mean))

newdat_partial$fitted = fitted(m, type = "response")
newdat_partial$partial_preds = predict(m, newdat_partial, type = "response")

newdat_partial = newdat_partial |>
  mutate(observed = seedl_dens_sp) |>
  mutate(full_resids = observed - fitted) |>
  mutate(partial_resids = full_resids + partial_preds)

# get fitted line for variation along focal var (all else set to mean)
# start with a data frame of all predictors set at their means
newdat_scenario = d_mod |>
  select(fire_intens2, ppt, nongrowing_cover, prefire_prop_sp, vol_brn_50m) |>
  summarize_all(mean) |>
  slice(rep(row_number(), 100)) # get 100 identical (repeated) rows

# now make the focal col a seq from the min obs to max obs
newdat_scenario = newdat_scenario |>
  mutate(fire_intens2 = seq(min(d_mod$fire_intens2), max(d_mod$fire_intens2), length.out = 100))

preds_scenario = predict(m, newdat_scenario)
newdat_scenario$preds = preds_scenario

ggplot(newdat_partial, aes(x = fire_intens2)) +
  geom_point(aes(y = ifelse(partial_resids < .5, .5, partial_resids))) +
  geom_line(data = newdat_scenario, aes(y = exp(preds))) +
  scale_y_continuous(breaks = c(.01,.1,1,10,100, 1000), minor_breaks = c(0.005, 0.05, 0.5, 5.0, 50, 500)) +
  coord_trans(y = "log")
