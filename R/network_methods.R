
#' Get information about piscivorous composition in network
get_piscivory_stat_from_network <- function (net) {

  resource <- str_extract(colnames(net), "[a-z]+") %>%
    na.omit()

  fish <- function(string) {
    return(!string %in% resource)
  } 

  fish_fish <- net[fish(rownames(net)), fish(colnames(net))] 

  if (length(fish_fish) == 1) {
    fish_fish %<>% as.matrix() 
    colnames(fish_fish) <-
      row.names(fish_fish) <-
	colnames(net)[fish(colnames(net))]
  }

  fish_fish %<>%
    colSums()

  tibble(
    nb_pisc_node = length(fish_fish[fish_fish > 0]),
    prop_pisc_node = nb_pisc_node / length(fish_fish),
    nb_pisc_rich =  str_extract(names(fish_fish[fish_fish > 0]), "[A-Z]+") %>%
      unique() %>% length(),
    prop_pisc_rich = nb_pisc_rich / str_extract(names(fish_fish), "[A-Z]+") %>%
      unique() %>% length()
  )


}
