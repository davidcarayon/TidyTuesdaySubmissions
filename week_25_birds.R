library(tidyverse)
library(FactoMineR)
library(factoextra)
library(vegan)
library(patchwork)
library(ggthemes)
library(ggrepel)
library(janitor)
library(purrr)

bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

# CA analysis -------------------------------------------------------------
bird_matrix <- bird_counts %>%
  filter(total_hours > 8) %>%
  distinct(year, species, how_many_counted_by_hour) %>%
  filter(how_many_counted_by_hour > 0) %>%
  drop_na() %>%
  spread(key = species, value = how_many_counted_by_hour, fill = 0) %>%
  column_to_rownames("year") %>%
  as.matrix()

## Hierarchical clustering
bray <- vegdist(bird_matrix, method = "bray")
clust <- hclust(bray, method = "ward.D2")
cluster <- cutree(clust, k = 3) %>%
  as.data.frame() %>%
  rownames_to_column("year") %>%
  rename("cluster" = ".") %>%
  tbl_df()

## Correspondence analysis
res.CA <- CA(bird_matrix, graph = FALSE)

# Scree plot (Keeping 2 dimensions)
fviz_eig(res.CA, addlabels = TRUE)

## Extracting rows coordinates
rows <- res.CA$row$coord %>%
  as.data.frame() %>%
  rownames_to_column("year") %>%
  tbl_df() %>%
  clean_names() %>%
  select(year:dim_2) %>%
  inner_join(cluster, by = "year") %>%
  mutate(cluster = as.factor(cluster))

## Extracting cols coordinates
cols <- res.CA$col$coord %>%
  as.data.frame() %>%
  rownames_to_column("species") %>%
  tbl_df() %>%
  clean_names() %>%
  select(species:dim_2)

extreme_species <- bind_rows(
  cols %>% top_n(5, dim_1),
  cols %>% top_n(-5, dim_1),
  cols %>% top_n(-5, dim_2),
)

## Rows plot with clustering
g1 <- ggplot(data = rows, aes(x = dim_1, y = dim_2)) +
  geom_label(aes(label = year)) +
  stat_ellipse(aes(fill = cluster), geom = "polygon", alpha = 0.4, level = 0.95, color = "black") +
  labs(
    fill = "Cluster", x = paste0("Dim 1: ", round(res.CA$eig[1, 2], 2), " %"),
    y = paste0("Dim 2: ", round(res.CA$eig[2, 2], 2), " %"), title = "Correspondence analysis - Rows"
  ) +
  theme_base() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())


## Species plot
g2 <- ggplot(data = cols, aes(x = dim_1, y = dim_2)) +
  geom_point() +
  geom_label_repel(data = extreme_species, aes(label = species)) +
  theme_base() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(
    x = paste0("Dim 1: ", round(res.CA$eig[1, 2], 2), " %"),
    y = paste0("Dim 2: ", round(res.CA$eig[2, 2], 2), " %"), title = "Correspondence analysis - Columns",
    caption = "Data from Bird Studies Canada | Plot by @david_carayon"
  )


# Time series analysis ------------------------------------------------------
Metrics <- bird_counts %>%
  filter(total_hours > 8) %>%
  distinct(year, species, how_many_counted_by_hour) %>%
  drop_na() %>%
  spread(key = species, value = how_many_counted_by_hour, fill = 0) %>%
  group_by(year) %>%
  nest() %>%
  mutate(
    SpecRichness = map_dbl(data, specnumber),
    shannon = map_dbl(data, diversity, index = "shannon")
  ) %>%
  inner_join(distinct(bird_counts, year, total_hours), by = "year") %>%
  select(-data) %>%
  gather(key = index, value = value, -year) %>%
  mutate(index = recode(index, "shannon" = "Shannon's diversity index", "SpecRichness" = "Species richness", "total_hours" = "Total hours"))

g3 <- ggplot(Metrics, aes(x = year, y = value)) +
  geom_line(aes(group = index, color = index), key_glyph = "timeseries") +
  scale_color_brewer(palette = "Set1") +
  geom_point(shape = 21, color = "black", fill = "white") +
  facet_wrap(~index, scales = "free") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.background = element_rect(fill = "white", color = "black"),
        axis.text = element_text(size = 12),
        legend.position = "top",
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15, face = "bold")) +
  labs(color = "Index", x = "Year") 

## Patchwork plotting
g3 / (g1 | g2) + plot_layout(heights = c(0.75, 1)) +
  ggsave("README_figs/birds_tidytuesday.png", dpi = "retina", width = 13.5, height = 10)
