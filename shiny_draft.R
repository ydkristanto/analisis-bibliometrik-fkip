library(tidyverse)
library(readxl)
library(openxlsx)
library(igraph)
library(sigmajs)
library(RColorBrewer)

# Data mentah ----
df_tugasakhir <- read_excel("fkip_tugasakhir_22-23.xlsx")
df_tugasakhir <- df_tugasakhir |> 
  mutate(
    DE = str_replace(DE, "pedagogi ignasian", "pedagogi reflektif"),
    DE = str_replace(DE, "paradigma pedagogi reflektif", "pedagogi reflektif"),
    DE = str_replace(DE, "pendekatan saintifik", "saintifik"),
    DE = str_replace(DE, "penguatan profil pelajar pancasila", "p3_pancasila"),
    DE = str_replace(DE, "profil pelajar pancasila", "p3_pancasila"),
    DE = str_replace(DE, "pelajar pancasila", "p3_pancasila"),
    ID = str_replace(ID, "pedagogi ignasian", "pedagogi reflektif"),
    ID = str_replace(ID, "paradigma pedagogi reflektif", "pedagogi reflektif"),
    ID = str_replace(ID, "pendekatan saintifik", "saintifik"),
    ID = str_replace(ID, "penguatan profil pelajar pancasila", "p3_pancasila"),
    ID = str_replace(ID, "profil pelajar pancasila", "p3_pancasila"),
    ID = str_replace(ID, "pelajar pancasila", "p3_pancasila")
  ) |> 
  mutate(
    DE = str_replace(DE, "p3_pancasila", "profil pelajar pancasila"),
    ID = str_replace(ID, "p3_pancasila", "profil pelajar pancasila"),
  ) |> 
  select(
    where(~ !all(is.na(.)))
  )
# Filter by department
df_tugasakhir <- df_tugasakhir |> 
  filter(
    C1 == "PBio"
  )

# Cooc keywords ----
edgelist_keywords <- df_tugasakhir |> 
  select(DE) |> 
  rename(keywords = DE) |> 
  mutate(
    keywords = str_split(keywords, "; ")
  ) |> 
  mutate(keywords = lapply(keywords, sort)) |> 
  filter(lengths(keywords) > 1) |> 
  rowwise() |> 
  mutate(
    pairs = list(as.data.frame(t(combn(keywords, 2))))
  ) |> 
  unnest(pairs) |> 
  select(-keywords) |> 
  rename(
    source = V1,
    target = V2
  ) |> 
  group_by(source, target) |> 
  summarise(
    weight = n(),
    .groups = "drop"
  ) |> 
  arrange(desc(weight))

nodelist_keywords <- df_tugasakhir |> 
  select(DE) |> 
  separate_longer_delim(
    cols = DE,
    delim = "; "
  ) |> 
  group_by(DE) |> 
  summarise(
    size = n(),
    .groups = "drop"
  ) |> 
  rename(
    name = DE
  ) |> 
  arrange(desc(size))

# Create graph ----
graph_keywords <- graph_from_data_frame(
  d = edgelist_keywords,
  directed = FALSE,
  vertices = nodelist_keywords
)

# Remove isolated one point
graph_keywords <- delete_vertices(
  graph_keywords,
  which(degree(graph_keywords) == 0)
)

# Cluster and centrality ----
# Calculate degree centrality for each node
degree_centrality <- degree(graph_keywords)
V(graph_keywords)$degree <- degree_centrality

# Clustering
cluster_keywords <- cluster_walktrap(
  graph = graph_keywords
)
V(graph_keywords)$cluster <- cluster_keywords$membership
# Label
V(graph_keywords)$label <- V(graph_keywords)$name

# Density and centrality for each cluster ----

# Compute centrality (average degree) and density for each cluster
cluster_metrics <- tibble(
  cluster = unique(V(graph_keywords)$cluster),
  centrality = sapply(unique(V(graph_keywords)$cluster), function(c) {
    mean(degree_centrality[V(graph_keywords)$cluster == c])
  }),
  density = sapply(unique(V(graph_keywords)$cluster), function(c) {
    subgraph <- induced_subgraph(graph_keywords, which(V(graph_keywords)$cluster == c))
    edge_density(subgraph)
  }),
  size = as.integer(table(V(graph_keywords)$cluster))
)

# Create color palette
generate_palette <- function(n) {
  if (n <= 8) {
    # Use Dark2 palette directly
    colors <- brewer.pal(n, name = "Dark2")
  } else {
    # Use Dark2 for the first 8 colors, then add shades of grey
    dark2_colors <- brewer.pal(8, name = "Dark2")
    grey_colors <- grey.colors(n - 8, start = 0.3, end = 0.9) # Shades of grey
    colors <- c(dark2_colors, grey_colors)
  }
  return(colors)
}

# Plot graph
sigmajs() |> 
  sg_from_igraph(
    igraph = graph_keywords,
    layout = igraph::layout_with_fr(graph_keywords)
  ) |> 
  sg_cluster(
    colors = generate_palette(length(unique(V(graph_keywords)$cluster))),
    directed = FALSE,
    algo = igraph::cluster_walktrap
  ) |> 
  sg_drag_nodes() |> 
  sg_neighbors()

