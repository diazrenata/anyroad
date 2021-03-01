library(ggplot2)
library(dplyr)
library(gratia)
load_mgcv()
sourcefiles <- c((list.files(here::here("neutral_rats", "R"), full.names = T)), (list.files(here::here("neutral_rats", "games"), full.names = T)))

for(script in sourcefiles) {
  source(script)
  rm(script)
}


rodent_counts <- portalr::abundance(level = "Treatment", time = "all")  %>%
  filter(treatment == "control")

annual_counts <- rodent_counts %>%
  mutate(censusyear = format.Date(censusdate, "%Y")) %>%
  select(-newmoonnumber,  -censusdate, -treatment, -period) %>%
  tidyr::pivot_longer(-censusyear, names_to = "species", values_to = "abund") %>%
  group_by(species, censusyear) %>%
  summarize(abund = sum(abund))  %>%
  ungroup() %>%
  group_by(censusyear) %>%
  mutate(totalabund = sum(abund)) %>%
  ungroup() %>%
  mutate(propabund = abund / totalabund)

mean_sd_table = get_rodent_sizes()

annual_isds <- list()

for(i in 1:length(unique(annual_counts$censusyear))) {
  
  thisyear <- unique(annual_counts$censusyear)[i]
    annual_isds[[i]] <- simulate_isd(filter(annual_counts, censusyear == thisyear), mean_sd_table = mean_sd_table) %>%
      mutate(censusyear = thisyear)
  
}

annual_isds_df <- bind_rows(annual_isds)

annual_biomass <- annual_isds_df %>%
  group_by(censusyear)  %>%
  summarize(total_biomass = sum(wgt),
            total_logwgt = sum(logwgt)) %>%
  ungroup() %>%
  mutate(log_total_biomass = log(total_biomass))

annual_kdes <- list()

kde_df <- data.frame()

for(i in 1:length(annual_isds)) {
  
    
    this_kde <- density(annual_isds[[i]]$logwgt, n = 1000, from = 0, to = max(annual_isds_df$logwgt) * 1.5)
  
    annual_kdes[[i]] <- data.frame(
      censusyear = annual_isds[[i]]$censusyear[1],
      logwgt = this_kde$x,
      density = this_kde$y / sum(this_kde$y)
    )
      
}


mean_total_biomass <- mean(annual_biomass$total_biomass)

mean_total_logbiomass <- mean(annual_biomass$total_logwgt)

annual_kdes_df <- bind_rows(annual_kdes) %>%
  left_join(annual_biomass) %>%
  mutate(scaled_density  = (density * total_logwgt),
         even_scaled_density = density * mean_total_logbiomass) %>%
  filter(as.numeric(censusyear) %in% c(1990:2009)) %>%
  filter(logwgt > 1, logwgt < 6) %>%
  mutate(fcensusyear = as.factor(censusyear)) %>%
  mutate(fiveyr = ceiling((as.numeric(censusyear) - 1989.5) / 5))  %>%
  mutate(fiveyrf = as.factor(fiveyr))

# just density performs the same, you don't need to rescale

ggplot(annual_kdes_df, aes(logwgt, scaled_density, group = censusyear, color = fiveyrf)) +
  geom_line() +
  theme(legend.position = "none")


ggplot(annual_kdes_df, aes(logwgt, even_scaled_density, color = censusyear)) +
  geom_line() +
  theme(legend.position = "none")


ggplot(annual_kdes_df, aes(logwgt, density, color = censusyear)) +
  geom_line() +
  theme(legend.position = "none")

sgam_oney <- gam(density ~ s(logwgt, k = 15), data = filter(annual_kdes_df, censusyear == "1994"), family = "tw", method = "REML")

sgam_oney_confint <- confint(sgam_oney, parm = "s(logwgt)", transform = T, shift = T)

ggplot(sgam_oney_confint, aes(logwgt, est)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .3) +
  geom_line(data = sgam_oney$model, aes(logwgt, density), color = "pink")


sgam <- gam(density ~ s(logwgt, k = 50) , data = filter(annual_kdes_df), family = "tw", method = "REML")

sgam_confint <- confint(sgam, parm = "s(logwgt)", transform = T, shift = T, type = "simultaneous")


ggplot(sgam_confint, aes(logwgt, est)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .5) +
  geom_line(data = annual_kdes_df, aes(logwgt, density, group = censusyear, color = fiveyrf), alpha = .2)


sgam2 <- gam(density ~ s(logwgt, k = 50) + s(logwgt, by = fiveyrf), data = filter(annual_kdes_df), family = "tw", method = "REML")


sgam2_fitted <- annual_kdes_df %>%
  select(logwgt, fiveyrf) %>%
  distinct() %>%
  add_fitted(sgam2)

ggplot(sgam2_fitted, aes(logwgt, .value, color =fiveyrf)) +
  geom_line() +
  geom_line(data = annual_kdes_df, aes(group = censusyear, y = density), inherit.aes = T, linetype = 2, alpha = .3) +
  facet_wrap(vars(fiveyrf))

# +  
#   geom_line(data = annual_kdes_df, aes(logwgt, density, group = censusyear), color = "pink", alpha = .2)


sgam2_lp <- predict(sgam2, type = "lpmatrix")

excoef <- colnames(sgam2_lp)[ grepl("fiveyrf", colnames(sgam2_lp))]

sgam2_lp_ex <-sgam2_lp
sgam2_lp_ex[, excoef] <- 0


sgam2_pred_ex <- sgam2$family$linkinv(sgam2_lp_ex %*% coefficients(sgam2))

sgam2_fitted_nof <- annual_kdes_df %>%
  select(logwgt, fiveyrf) %>%
  mutate(pred_ex = sgam2_pred_ex) %>%
  select(logwgt, pred_ex) %>%
  distinct()


ggplot(sgam2_fitted_nof, aes(logwgt, pred_ex)) +
  geom_line(size = 2) +
 geom_line(data = sgam2_fitted, aes(y = .value, color = fiveyrf), inherit.aes = T) +
  geom_line(data = annual_kdes_df, aes(logwgt, density, group = censusyear, color = fiveyrf), alpha = .2)

# as ever, it's kind of thorny extracting confints per smooth or smooth diffs with factors. 
# but this is SUPER FING COOL

# consider doing a moving average five year time window and then comparing the end to the beginning

# it doesn't really seem to be reliable to look only at the s(wgt) without the factor smooths if factors are included. this is because the factor additions are adding important stuff, it's not like s(nofactor) is the mean of the factor smooths or anything.