get_rodent_sizes <- function() {
  
  all_species_caps <- portalr::summarise_individual_rodents(unknowns = F)

  mean_sds <- all_species_caps %>%
    filter(!is.na(species), !is.na(wgt)) %>%
    group_by(species) %>%
    summarize(meanwgt = mean(wgt),
              sdwgt = sd(wgt)) %>%
    ungroup()
    
  return(mean_sds)
}