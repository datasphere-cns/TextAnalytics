install.packages("tidyverse") 
library(tidyverse)
edge_list <- tibble(from = c(1, 2, 2, 3, 4), to = c(2, 3, 4, 2, 1))
node_list <- tibble(id = 1:4)

edge_list
node_list

library(tidyverse)
letters <- read_csv("C:/Users/nelso/Desktop/Datasphere/TextAnalytics-main/AnalisisComunidades/correspondence-data-1585.csv")

letters

sources <- letters %>%
  distinct(source) %>%
  rename(label = source)

destinations <- letters %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by = "label")
nodes

nodes <- nodes %>% rowid_to_column("id")
nodes

per_route <- letters %>%  
  group_by(source, destination) %>%
  summarise(weight = n()) %>% 
  ungroup()
per_route

edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)


edges <- select(edges, from, to, weight)
edges 
install.packages("network")
library(network)
routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

class(routes_network)

routes_network

plot(routes_network, vertex.cex = 3)

plot(routes_network, vertex.cex = 3, mode = "circle")

detach(package:network)
rm(routes_network)

install.packages("igraph")
library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

routes_igraph

plot(routes_igraph, edge.arrow.size = 0.2)

plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)

install.packages("tidygraph")
install.packages("ggraph")
library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

routes_igraph_tidy <- as_tbl_graph(routes_igraph)

class(routes_tidy)

class(routes_igraph_tidy)

class(routes_igraph)

routes_tidy

routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()

install.packages("visNetwork")
install.packages("networkD3")

library(visNetwork)
library(networkD3)

visNetwork(nodes, edges)

edges <- mutate(edges, width = weight/5 + 1)

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")

nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "id", Value = "weight", 
             opacity = 1, fontSize = 16, zoom = TRUE)

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")
