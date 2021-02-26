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

year1 = filter(annual_counts, censusyear == 1994)
year2 = filter(annual_counts, censusyear == 1995)

year1isd <- simulate_isd(year1) %>% mutate(year = 1994) 
year2isd <- simulate_isd(year2)  %>% mutate(year = 1995)

isds <- bind_rows(year1isd, year2isd)

ggplot(isds, aes(wgt, color = as.factor(year))) +
  geom_density() 

isds_overlap <- pair_overlap(list(sp1 = year1isd$wgt, sp2 = year2isd$wgt))

many_qn <- replicate(100, expr = qn(year1, year2), simplify = F) %>% bind_rows(.id = "sim")

ggplot(many_qn, aes(species, propabund)) +
  geom_point() 

qn_over_time <- list()

qn_over_time[[1]] <- year1 #%>%
  # mutate(abund = ceiling(totalabund / nrow(year1))) %>%
  # mutate(totalabund = sum(abund)) %>%
  # mutate(propabund =abund/totalabund)


for(i in 2:100) {
  qn_over_time[[i]] <- qn(start = qn_over_time[[i - 1]], birth_rate = .3, death_rate = .4, imm_rate = .05)
}

qn_over_time <- bind_rows(qn_over_time, .id = "tstep") %>%
  mutate(tstep = ordered(as.numeric(tstep)))


ggplot(filter(qn_over_time, tstep == 10), aes(species, propabund)) +
  geom_point() +
  facet_wrap(vars(tstep))


ggplot(filter(actuals_over_time, tstep == 10), aes(species, propabund)) +
  geom_point() +
  facet_wrap(vars(tstep))


actuals_over_time <- list()

for(i in c(1:20)) {
  actuals_over_time[[i]] <- filter(annual_counts, censusyear == 1993 + i)
}

qn_over_time_actuals <- list()

qn_over_time_actuals[[1]] <- actuals_over_time[[1]]

for(i in 2:20) {
  qn_over_time_actuals[[i]] <- qn(start = qn_over_time_actuals[[i-1]], t1 = actuals_over_time[[i - 1]], t2 = actuals_over_time[[i]])
}

qn_over_time_actuals <- bind_rows(qn_over_time_actuals, .id = "tstep") %>%
  mutate(tstep = ordered(as.numeric(tstep)))


actuals_over_time <- bind_rows(actuals_over_time, .id = "tstep") %>%
  mutate(tstep = ordered(as.numeric(tstep)))



ggplot(qn_over_time_actuals, aes(species, propabund)) +
  geom_point() +facet_wrap(vars(tstep)) 

ggplot(actuals_over_time, aes(species, propabund)) +
  geom_point() +facet_wrap(vars(tstep)) 


ggplot(qn_over_time_actuals, aes(species, abund)) +
  geom_errorbar(aes(ymin = 0, ymax = start), color = "blue") +
  geom_errorbar(aes(ymin = start - deaths, ymax = start), color = "red") +
  geom_errorbar(aes(ymin = start - deaths, ymax = start - deaths + births), color = "pink") +
  geom_errorbar(aes(ymin = start - deaths + births, ymax = start - deaths + births + immigrants), color = "yellow") +
  facet_wrap(vars(tstep), scales = "free") +
  geom_point() +
  scale_y_log10()


year1isd <- simulate_isd(filter(actuals_over_time, tstep == 1)) %>% mutate(time = "start") 
year2isd <- simulate_isd(filter(qn_over_time_actuals, tstep == 10))  %>% mutate(time = "sim-end")
year2isdreal <-simulate_isd(filter(actuals_over_time, tstep == 10)) %>% mutate(time = "real-end") 

isds <- bind_rows(year1isd, year2isd, year2isdreal)

ggplot(isds, aes(wgt, color = as.factor(time))) +
  geom_density() 

isds_overlap <- pair_overlap(list(sp1 = year1isd$wgt, sp2 = year2isd$wgt))


# 
# the following now implemented as "games/quasineutral" qn.
# changes <- year2$abund - year1$abund
# 
# ndeaths <- abs(sum(changes[ changes < 0]))
# nbirths <- abs(sum(changes[ ((changes > 0) + (year1$abund > 0)) == 2]))
# nimmigrants <- abs(sum(changes[((changes > 0) + (year1$abund == 0)) == 2]))
# 
# year2_quasineutral <- data.frame(
#   species = year1$species,
#   start = year1$abund,
#   deaths = rmultinom(1, size = ndeaths, prob = year1$propabund),
#   births = rmultinom(1, size = nbirths, prob = year1$propabund),
#   immigrants = rmultinom(1, size = nimmigrants, prob = (year1$propabund == 0) / sum(year1$propabund == 0))
# ) %>%
#   mutate(abund = start + births + immigrants - deaths) %>%
#   mutate(totalabund = sum(abund)) %>% mutate(propabund = abund / totalabund)
# 
# year1isd <- simulate_isd(year1) %>% mutate(year = "1994") 
# year2isd <- simulate_isd(year2)  %>% mutate(year = "1995")
# year2isd_qn <- simulate_isd(year2_quasineutral)  %>% mutate(year = "1995_qn")
# 
# isds <- bind_rows(year1isd, year2isd, year2isd_qn)
# 
# ggplot(isds, aes(wgt, color = as.factor(year))) +
#   geom_density() 
# 
# ggplot(year1, aes(species, propabund)) +
#   geom_point() +
#   geom_point(data = year2, inherit.aes = T, color = "blue") +
#   geom_point(data = year2_quasineutral, color = "red")
