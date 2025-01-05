library(igraph)
library(tidyverse)
library(readxl)

# Preparing data ----
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
  )

# Filter by department
df_tugasakhir_filtered <- df_tugasakhir |> 
  select(where(~ all(!is.na(.)))) |> 
  filter(
    C1 == "PKim"
  )

edgelist_keywords <- df_tugasakhir_filtered |> 
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
    Source = V1,
    Target = V2
  ) |> 
  group_by(Source, Target) |> 
  summarise(
    Weight = n(),
    .groups = "drop"
  ) |> 
  arrange(desc(Weight))

nodelist_keywords <- edgelist_keywords |> 
  select(-Weight) |> 
  pivot_longer(
    cols = c(Source, Target),
    names_to = "Node",
    values_to = "Id"
  ) |> 
  select(Id) |> 
  group_by(Id) |> 
  summarise(
    Count = n(),
    .groups = "drop"
  ) |> 
  arrange(desc(Count)) |> 
  select(-Count)

# Create graph ----
edgelist_keywords <- edgelist_keywords |> 
  rename(
    source = Source,
    target = Target,
    weight = Weight
  )
graph_keywords <- graph_from_data_frame(
  d = edgelist_keywords,
  directed = FALSE
)

# Cluster the network ----
# Apply Louvain clustering
clusters_keywords <- cluster_louvain(graph_keywords)

# Assign cluster membership to nodes
V(graph_keywords)$cluster <- clusters_keywords$membership

# Calculate centrality and density for clusters ----

# Calculate degree centrality for each node
degree_centrality <- degree(graph_keywords)

# Compute centrality (average degree) and density for each cluster
cluster_metrics <- tibble(
  cluster = as.integer(unique(V(graph_keywords)$cluster)),
  centrality = sapply(unique(V(graph_keywords)$cluster), function(c) {
    mean(degree_centrality[V(graph_keywords)$cluster == c])
  }),
  density = sapply(unique(V(graph_keywords)$cluster), function(c) {
    subgraph <- induced_subgraph(graph_keywords, which(V(graph_keywords)$cluster == c))
    edge_density(subgraph)
  }),
  size = as.integer(table(V(graph_keywords)$cluster)),
  cluster_title = sapply(unique(V(graph_keywords)$cluster), function(c) {
    subgraph_vertices <- which(V(graph_keywords)$cluster == c)
    vertex_with_max_degree <- subgraph_vertices[which.max(degree(graph_keywords)[subgraph_vertices])]
    V(graph_keywords)$name[vertex_with_max_degree] # Get the name of the keyword
  }),
  cluster_members = sapply(unique(V(graph_keywords)$cluster), function(c) {
    subgraph_vertices <- which(V(graph_keywords)$cluster == c)
    degrees <- degree(graph_keywords)[subgraph_vertices]
    sorted_vertices <- subgraph_vertices[order(-degrees)] # Sort vertices by degree (descending)
    paste(V(graph_keywords)$name[sorted_vertices], collapse = "; ") # Combine sorted keywords
  })
) |> 
  mutate(
    sample_members = sapply(cluster_members, function(members) {
      # Split the cluster_members string into individual keywords
      keywords <- unlist(strsplit(members, ";\\s*")) # Split by ';' and trim spaces
      # Select the first 10 keywords if there are at least 10, otherwise take all
      sampled <- keywords[seq_len(min(10, length(keywords)))]
      # Combine the sampled keywords back into a semicolon-separated string
      paste(sampled, collapse = "; ")
    }),
    department = "PKim"
  )

print(cluster_metrics)

write.csv(
  cluster_metrics,
  file = "cluster_metrics_PKim.csv",
  row.names = FALSE
)

# Plot the thematic map ----
ggplot(cluster_metrics, aes(x = Centrality, y = Density, size = Size.Freq)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Thematic Map",
    x = "Centrality (Importance)",
    y = "Density (Development)",
    size = "Cluster Size"
  )

# Load data ----
cluster_metrics_BK <- read_csv(
  "cluster_metrics_BK.csv",
  show_col_types = FALSE
)
cluster_metrics_PAK <- read_csv(
  "cluster_metrics_PAK.csv",
  show_col_types = FALSE
)
cluster_metrics_PBI <- read_csv(
  "cluster_metrics_PBI.csv",
  show_col_types = FALSE
)
cluster_metrics_PBio <- read_csv(
  "cluster_metrics_PBio.csv",
  show_col_types = FALSE
)
cluster_metrics_PBSI <- read_csv(
  "cluster_metrics_PBSI.csv",
  show_col_types = FALSE
)
cluster_metrics_PE_Akuntansi <- read_csv(
  "cluster_metrics_PE_Akuntansi.csv",
  show_col_types = FALSE
)
cluster_metrics_PE_Ekonomi <- read_csv(
  "cluster_metrics_PE_Ekonomi.csv",
  show_col_types = FALSE
)
cluster_metrics_PFis <- read_csv(
  "cluster_metrics_PFis.csv",
  show_col_types = FALSE
)
cluster_metrics_PGSD <- read_csv(
  "cluster_metrics_PGSD.csv",
  show_col_types = FALSE
)
cluster_metrics_PKim <- read_csv(
  "cluster_metrics_PKim.csv",
  show_col_types = FALSE
)
cluster_metrics_PMat <- read_csv(
  "cluster_metrics_PMat.csv",
  show_col_types = FALSE
)
cluster_metrics_PSej <- read_csv(
  "cluster_metrics_PSej.csv",
  show_col_types = FALSE
)
cluster_metrics_all <- bind_rows(
  cluster_metrics_BK, cluster_metrics_PAK,
  cluster_metrics_PBI, cluster_metrics_PBio,
  cluster_metrics_PBSI, cluster_metrics_PE_Akuntansi,
  cluster_metrics_PE_Ekonomi, cluster_metrics_PFis,
  cluster_metrics_PGSD, cluster_metrics_PKim,
  cluster_metrics_PMat, cluster_metrics_PSej
) |> 
  group_by(department) |> 
  mutate(
    centrality_std = (centrality - mean(centrality, na.rm = TRUE)) / sd(centrality, na.rm = TRUE),
    density_std = (density - mean(density, na.rm = TRUE)) / sd(density, na.rm = TRUE)
  ) |> 
  ungroup()
write.csv(
  cluster_metrics_all, file = "cluster_metrics_all.csv"
)

# Plot thematic map ----
cluster_metrics_all |> 
  ggplot(
    aes(x = centrality_std, y = density_std)
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_point(
    aes(size = size, colour = factor(cluster)),
    alpha = .5,
    show.legend = FALSE
  ) + 
  facet_wrap(vars(department), ncol = 4) + 
  theme_minimal()






