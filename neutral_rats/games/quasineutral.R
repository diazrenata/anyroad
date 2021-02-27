qn <- function(start, t1 = NULL, t2 = NULL, birth_rate = NULL, death_rate = NULL, imm_rate = NULL) {
  
  
  if(!is.null(t2)) {
    changes <- t2$abund - t1$abund
    
    ndeaths <- abs(sum(changes[ changes < 0]))
    nbirths <- abs(sum(changes[ ((changes > 0) + (t1$abund > 0)) == 2]))
    nimmigrants <- abs(sum(changes[((changes > 0) + (t1$abund == 0)) == 2]))
  } else {
    
    nbirths <- ceiling(birth_rate * sum(start$abund))
    ndeaths <- ceiling(death_rate * sum(start$abund))
    nimmigrants <- ceiling(imm_rate * sum(start$abund))
    
  }
  
  if(sum(start$abund ==0) == 0) {
    immigrants <- 0
  } else{
    immigrants = rmultinom(1, size = nimmigrants, prob = (start$propabund == 0) / sum(start$propabund == 0))
  }
  
  deaths = rmultinom(1, size = ndeaths, prob = start$abund)
  
  births = rmultinom(1, size = nbirths, prob = (start$propabund >= 0) / sum(start$propabund >= 0))
  
  
  
  t2_sim <- data.frame(
    species = start$species,
    start = start$abund,
    births = births,
    deaths = deaths,
    immigrants = immigrants
  ) %>%
    group_by_all() %>%
    mutate(abund = max(0, start + births + immigrants - deaths)) %>%
    ungroup() %>%
    mutate(totalabund = sum(abund)) %>% mutate(propabund = abund / totalabund)
  
  return(t2_sim)
  
}
