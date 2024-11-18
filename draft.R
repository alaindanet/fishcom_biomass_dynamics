library(targets)
library(tidyverse)
library(magrittr)
library(lubridate)
library(cowplot)
library(viridis)
library(arrow)
library(glmmTMB)
library(piecewiseSEM)
library(semEff)
library(lme4)
library(INLA)
lapply(list.files(here::here("R"), full.names = TRUE), source)

library(sf)
tar_load(c(basin_dce, station_analysis))

tar_load(full_data2)

library(igraph)
g <- get_root_graph4fish()



```{r}
tar_load(op_data)
st <- op_data %>%
  filter(station == "9561", year %in% c(1995, 2005))
load(here::here("data", "classes", "network_analysis.rda"))
network_analysis %<>%
  filter(opcod %in% st$opcod)
```

```{r}
set_color_nodes <- function (
  node_list = NULL,
  color_species_resources = NULL
  ) {

  node_list_sp <- stringr::str_replace_all(node_list, "_\\d", "")

  colour <- color_species_resources[node_list_sp]
  names(colour) <- node_list
  return(colour)
}
set_node_resources_abun <- function (
  node_list = NULL,
  node_abun = NULL, resource_abun = NULL) {

  abun <- vector("numeric", length(node_list))
  names(abun) <- node_list

  if (is.null(resource_abun)) {
    resource_abun <- mean(node_abun)
  }

  abun[names(node_abun)] <- node_abun
  abun[abun == 0] <- resource_abun
  return(abun)

}
node_color <- set_color_species(
      node_list = unique(rownames(TL)),
      species_list = NULL, resource_list = NULL, col_resource = NULL)

net95 <- network_analysis$network[[1]] %>%
    igraph::graph_from_data_frame() %>%
    igraph::as_adjacency_matrix(., sparse = FALSE)

TL <- NetIndices::TrophInd(net95,
  Dead = "biof")
PlotWeb(
      TL = TL$TL,
      webTL = net95,
      colnode = set_color_nodes(
	node_list = rownames(TL),
	color_species_resources = node_color),
      abund = set_node_resources_abun(
	node_list = rownames(TL),
	node_abun = 1,
	resource_abun = 1
	)
      )

root_graph <- get_root_graph4fish()
g95 <- igraph::graph_from_adjacency_matrix(net95, mode = "directed", diag = FALSE)
g95rooted <- g95 + root_graph
g95_dtree <- dominator_tree(g95rooted, root = "R")


TL <- NetIndices::TrophInd(g95_dtree$domtree %>%
    igraph::as_adjacency_matrix(., sparse = FALSE),
  Dead = "R")
PlotWeb(
  TL = TL$TL,
  webTL = g95_dtree$domtree %>%
    igraph::as_adjacency_matrix(., sparse = FALSE),
  colnode = set_color_nodes(
    node_list = rownames(TL),
    color_species_resources = node_color),
  abund = set_node_resources_abun(
    node_list = rownames(TL),
    node_abun = 1,
    resource_abun = 1
  )
)
```



```{r}
root_graph <- get_root_graph4fish()
g95 <- igraph::graph_from_adjacency_matrix(ex_net$network[[1]], mode = "directed", diag = FALSE)
g95rooted <- g95 + root_graph
p_g95rooted_all <- ~{
  lg <- layout_as_tree(g95rooted, root = "R")
  lg[, 2] <- -lg[, 2]
  plot(g95rooted, layout = lg, vertex.color = vertex_color, vertex.size = vertex_size)
}
ggdraw(p_g95rooted_all)

g95_dtree <- dominator_tree(g95rooted, root = "R")
p_g95_functional <- ~{
  layout <- layout_as_tree(g95_dtree$domtree, root = "R")
  layout[, 2] <- -layout[, 2]
  plot(
    g95_dtree$domtree,
    layout = layout,
    vertex.color = vertex_color,
    vertex.size = vertex_size
  )
}

p_g95 <- plot_grid(
  ggdraw(p_g95rooted_all), ggdraw(p_g95_functional),
  nrow = 1,
  labels = "auto"
)

```

```{r}
g05 <- igraph::graph_from_adjacency_matrix(ex_net$network[[2]], mode = "directed", diag = FALSE)
g05rooted <- g05 + root_graph
p_g05rooted_all <- ~{
  lg <- layout_as_tree(g05rooted, root = "R")
  lg[, 2] <- -lg[, 2]
  plot(g05rooted, layout = lg, vertex.color = vertex_color, vertex.size = vertex_size)
}
g05_dtree <- dominator_tree(g05rooted, root = "R")
p_g05_functional <- ~{
  layout <- layout_as_tree(g05_dtree$domtree, root = "R")
  layout[, 2] <- -layout[, 2]
  plot(
    g05_dtree$domtree,
    layout = layout,
    vertex.color = vertex_color,
    vertex.size = vertex_size
  )
}
p_g <- plot_grid(
  ggdraw(p_g95rooted_all), ggdraw(p_g95_functional),
  ggdraw(p_g05rooted_all), ggdraw(p_g05_functional),
  nrow = 2,
  labels = "auto"
)
p_g
ggsave_multiple(
  paste0("p_g9505", c(".png", ".pdf")),
  plot = p_g,
  path = here::here("figures"),
  scale = 4.6,
  units = c("mm"),
  width = 80,
  height = 80 * 1.5)
```
