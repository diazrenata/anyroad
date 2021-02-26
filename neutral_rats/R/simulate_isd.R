simulate_isd <- function(species_counts_table, mean_sd_table = NULL, use_rodents = TRUE) {
  
  if(is.null(mean_sd_table)) {
    if(use_rodents) {
      mean_sd_table = get_rodent_sizes()
    }
  }
  
  
  inds_to_draw <- left_join(species_counts_table, mean_sd_table) %>%
    filter(abund > 0) %>%
    as.matrix()
  
  inds_dfs <- apply(inds_to_draw, MARGIN = 1, FUN = draw_individuals)
  
  isd <- bind_rows(inds_dfs)
  
  return(isd)
}

draw_individuals <- function(input_vect = NULL, species_name = NULL, nind = NULL, mean_size = NULL, sd_size = NULL) {
  
  if(!is.null(input_vect)) {
    
    species_name <- input_vect[["species"]]
    nind = as.numeric(input_vect[["abund"]])
    mean_size = as.numeric(input_vect[["meanwgt"]])
    sd_size = as.numeric(input_vect[["sdwgt"]])
    
  }
  

  inds <- rnorm(n = nind, mean = mean_size, sd = sd_size)
 
  while(any(inds <= 0)) {
    
    inds[ which(inds <= 0)] <- rnorm(n = sum(inds <= 0), mean = mean_size, sd = sd_size)
    
  }
  
  return(data.frame(
    species = species_name,
    wgt = inds
  ))
}
