shuffle_species <- function(counts_table) {
  
  species <- counts_table %>%
    select(species) %>%
    distinct() %>%
    rename(orig_species = species) %>%
    mutate(new_species = sample(orig_species, size = length(unique(counts_table$species)), replace = F))
  
  counts_table <- counts_table %>%
    rename(orig_species = species) %>%
    left_join(species) %>%
    rename(species = new_species) %>%
    select(-orig_species)%>%
    mutate(source = "shuffled")
  
  return(counts_table)
  
}
