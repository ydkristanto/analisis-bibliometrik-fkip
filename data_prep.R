library(tidyverse)
library(readxl)
library(openxlsx)

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

write.xlsx(
  edgelist_keywords,
  file = "edgelist_keywords.xlsx"
)
write.xlsx(
  nodelist_keywords,
  file = "nodelist_keywords.xlsx"
)

# Prodi - keywords, ekslusif ----
tugasakhir_divider <- df_tugasakhir |> 
  group_by(C1) |> 
  summarise(
    divider = n(),
    .groups = "drop"
  ) |> 
  rename(
    Source = C1
  )
edgelist_prodi_keywords <- df_tugasakhir |> 
  select(C1, ID) |> 
  rename(
    Source = C1,
    Target = ID
  ) |> 
  separate_longer_delim(
    Target, delim = "; "
  ) |> 
  group_by(Source, Target) |> 
  summarise(
    Weight = n(),
    .groups = "drop"
  ) |> 
  left_join(
    tugasakhir_divider, by = "Source"
  ) |> 
  mutate(
    Weight = Weight / divider
  ) |> 
  select(-divider)


nodelist_prodi_keywords <- edgelist_prodi_keywords |> 
  select(-Weight) |> 
  pivot_longer(
    cols = c(Source, Target),
    names_to = "nodeType",
    values_to = "Id"
  ) |> 
  mutate(
    nodeType = ifelse(
      nodeType == "Source", "Prodi", "Kata kunci"
    )
  ) |> 
  unique() |> 
  select(Id, nodeType) |> 
  arrange(desc(nodeType), Id)

write.xlsx(
  edgelist_prodi_keywords,
  file = "edgelist_prodi_keywords.xlsx"
)
write.xlsx(
  nodelist_prodi_keywords,
  file = "nodelist_prodi_keywords.xlsx"
)
