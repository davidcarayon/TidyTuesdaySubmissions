# Data and packages loading -----------------------------------------------
library(tidyverse)
library(skimr)
library(data.tree)
library(circlepackeR)
library(ggforce)
library(cowplot)
library(ggthemes)
library(ggrepel)


media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")


# EDA  --------------------------------------------------------------------
media_franchises %>%
  group_by(creators) %>%
  summarise(nd = n_distinct(franchise)) %>%
  filter(nd > 1)

top_15_franchises <- media_franchises %>%
  group_by(franchise) %>%
  summarise(total_revenue = sum(revenue)) %>%
  top_n(15, total_revenue) %>%
  pull(franchise)

media_franchises %>%
  unique() %>%
  filter(franchise %in% top_15_franchises) %>%
  group_by(franchise) %>%
  mutate(total_revenue = sum(revenue)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(franchise, -total_revenue), y = revenue, group = revenue_category, fill = revenue_category)) +
  geom_bar(stat = "identity") +
  theme_bw()

media_franchises %>%
  group_by(owners) %>%
  summarise(sum = sum(revenue)) %>%
  arrange(desc(sum))

# CirclePackR -------------------------------------------------------------
media_franchises -> tab

tab$pathString <- paste("world", tab$franchise, tab$revenue_category, sep = "/")
population <- as.Node(tab)

circlepackeR(population,
             size = "revenue", color_min = "hsl(152,80%,80%)",
             color_max = "hsl(228,30%,40%)"
)

# Final plots -------------------------------------------------------------

## Media

top_5_media <- media_franchises %>%
  group_by(original_media) %>%
  summarise(total_revenue = sum(revenue)) %>%
  top_n(5, total_revenue) %>%
  pull(original_media)

p1 <- media_franchises %>%
  unique() %>%
  mutate(revenue = revenue * 1000000000) %>%
  filter(original_media %in% top_5_media) %>%
  ggplot(aes(x = original_media, y = revenue)) +
  geom_violin(alpha = 0.3, aes(fill = original_media)) +
  geom_sina(aes(fill = original_media), color = "black", size = 3, shape = 21) +
  geom_label_repel(
    data = media_franchises %>%
      unique() %>%
      mutate(revenue_category = recode(revenue_category, "Merchandise, Licensing & Retail" = "Merchandise", "Video Games/Games" = "Games")) %>%
      filter(original_media %in% top_5_media) %>%
      filter(revenue > 19) %>%
      mutate(categ = paste0(franchise, " (", revenue_category, ")")),
    aes(x = original_media, y = revenue * 1000000000, label = categ), color = "black", fill = "wheat", family = "mono", size = 4
  ) +
  guides(fill = FALSE, color = FALSE) +
  labs(x = "Original Media", y = "Total revenue") +
  scale_y_continuous(label = scales::dollar) +
  theme_wsj() +
  labs(title = "Revenue generated for the 5 most successful media")


## Categories

top_10_categ <- media_franchises %>%
  group_by(revenue_category) %>%
  summarise(total_revenue = sum(revenue)) %>%
  top_n(10, total_revenue) %>%
  pull(revenue_category)


p2 <- media_franchises %>%
  unique() %>%
  mutate(revenue = revenue * 1000000000) %>%
  filter(revenue_category %in% top_10_categ) %>%
  group_by(revenue_category) %>%
  summarise(total_categ = sum(revenue)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(revenue_category, total_categ), y = total_categ)) +
  geom_bar(stat = "identity", aes(fill = revenue_category), color = "black") +
  guides(fill = FALSE, color = FALSE) +
  labs(x = "Original Media", y = "Total revenue") +
  scale_y_continuous(label = scales::dollar, limits = c(0, 10e+11)) +
  theme_wsj() +
  labs(title = "Total revenue generated for the 10 most successful categories") +
  coord_flip() +
  geom_label(aes(label = paste0("$", round(total_categ / 1000000000), " Bn")), fill = "wheat", color = "black", size = 4, family = "mono", nudge_y = 69000000000) +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 9)
  )

# Timeline
p3 <- distinct(media_franchises, franchise, year_created) %>%
  group_by(year_created) %>%
  summarise(n_franchise = n_distinct(franchise)) %>%
  ungroup() %>%
  unique() %>%
  ggplot(aes(x = year_created, y = n_franchise)) +
  geom_bar(aes(x = year_created, y = n_franchise, fill = n_franchise), stat = "identity", color = "black") +
  scale_fill_viridis_c(direction = -1) +
  guides(fill = FALSE) +
  theme_wsj() +
  geom_curve(aes(x = 1975, y = 5.2, xend = 1993, yend = 6), curvature = -0.3, arrow = arrow(length = unit(2, "mm")), color = "black") +
  annotate("label", x = 1970, y = 5, label = "6 franchises were created in 1994", color = "black", fill = "wheat", family = "mono") +
  labs(title = "Number of franchises created each year", subtitle = "1994 was the most creative year", caption = "\n Data from Wikipedia | plot by @david_carayon") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "bold")
  )

plot_grid(p1, plot_grid(p2, p3), ncol = 1, rel_heights = c(1.45, 1)) +
  ggsave("README_figs/media_tidytuesday.png", dpi = "retina", width = 20, height = 11.83)