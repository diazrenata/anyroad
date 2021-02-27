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

real_counts = filter(annual_counts, censusyear %in% c(1994, 2004)) %>%
  mutate(source = "real")

shuffle_counts = replicate(n = 1000, expr = shuffle_species(real_counts), simplify = F)

real_isds <- list() 
shuffle_isds <- list()

#mean_sd_table = get_rodent_sizes()

for(i in 1:2) {
  
  thisyear <- unique(real_counts$censusyear)[i]
  
  real_isds[[i]] <- simulate_isd(filter(real_counts, censusyear == thisyear), mean_sd_table = mean_sd_table) %>%
    mutate(censusyear = thisyear,
           source = "real", 
           sim = "real")
  
  some_shuffles <- list()
  
  for(j in 1:length(shuffle_counts)) {
    some_shuffles[[j]] <- simulate_isd(filter(shuffle_counts[[j]], censusyear == thisyear), mean_sd_table = mean_sd_table) %>%
      mutate(censusyear = thisyear)
  }
  
  some_shuffles <- bind_rows(some_shuffles, .id = "sim")
  
  shuffle_isds[[i]] <- some_shuffles
  
}


real_isds_plot <- bind_rows(real_isds) 
shuffle_isds_plot <- bind_rows(shuffle_isds)

all_isds_plot <- bind_rows(real_isds_plot, shuffle_isds_plot)

ggplot(filter(all_isds_plot, sim  %in% c("real", sample(unique(all_isds_plot$sim), size = 9, replace = F))), aes(logwgt, color = censusyear)) +
  geom_density() +
  facet_wrap(vars(sim), scales = "free_y")

isds_overlaps <- list()

for(i in 1:length(unique(all_isds_plot$sim))) {
  
  this_sim <- filter(all_isds_plot, sim == unique(all_isds_plot$sim)[i])
  
  y1 <- filter(this_sim, censusyear == unique(this_sim$censusyear)[1])$logwgt
  y2 <- filter(this_sim, censusyear == unique(this_sim$censusyear)[2])$logwgt
  
  isds_overlaps[[i]]<- this_sim %>% mutate(
    overlap = pair_overlap(list(sp1 = y1, sp2 = y2), min_size = 0, max_size = max(all_isds_plot$logwgt) * 1.1))
  
  
}

isds_overlaps_plot <- bind_rows(isds_overlaps)

isds_overlaps_res <- isds_overlaps_plot %>%
  select(sim, overlap) %>%
  distinct()

ggplot(filter(isds_overlaps_res, sim != "real"), aes(overlap)) +
  geom_histogram() +
  geom_vline(xintercept = filter(isds_overlaps_res, sim == "real")$overlap)
