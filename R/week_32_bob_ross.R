library(tidyverse)
library(janitor)
library(ggstatsplot)
library(ggridges)
library(ggwordcloud)
library(cowplot)

bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv") %>%
  clean_names() %>%
  separate(episode, into = c("season", "episode"), sep = "E") %>%
  mutate(season = str_extract(season, "[:digit:]+")) %>%
  mutate_at(vars(season, episode), as.integer) %>%
  mutate(title = str_to_title(title))


long <- bob_ross %>%
  gather(key = element, value = presence, -season, -episode, -title) %>%
  filter(presence > 0) %>%
  mutate(element = str_to_title(str_replace_all(element, "_", " "))) %>%
  mutate(element = ifelse(str_detect(element, "Tree"), yes = "Trees", no = element)) %>%
  mutate(element = fct_lump_min(element, min = 5)) %>%
  group_by(season, element) %>%
  summarise(freq = sum(presence)) %>%
  ungroup()

# Word cloud --------------------------------------------------------------


g1 <- long %>%
  group_by(element) %>%
  summarise(freq = sum(freq)) %>%
  ggplot(aes(label = element, size = freq, color = freq)) +
  geom_text_wordcloud(eccentricity = 1) +
  theme_minimal() +
  scale_size_area(max_size = 30) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Frequency of elements in Bob Ross's paintings") +
  theme(plot.title = element_text(family = "Roboto condensed", face = "bold", size = 25, hjust = 0.5))


# Circular barplot --------------------------------------------------------------------
# Inspiration : @jmcastagnetto


br_elements <- bob_ross %>%
  gather(key = element, value = presence, -season, -episode, -title) %>%
  filter(presence > 0) %>%
  mutate(element = str_to_title(str_replace_all(element, "_", " "))) %>%
  mutate(element = ifelse(str_detect(element, "Tree"), yes = "Trees", no = element)) %>%
  mutate(element = fct_lump_min(element, min = 5)) %>%
  group_by(element) %>%
  count() %>%
  ungroup() %>%
  mutate(
    element = paste0(element, "\n(N = ", n, ")") %>%
      forcats::fct_reorder(n)
  )


br_elements$id <- seq(1, nrow(br_elements))
angle <- 90 - (360 * (br_elements$id - 0.5) / nrow(br_elements))
br_elements$hjust <- as.numeric(angle < -90)
br_elements$angle <- angle

g2 <- ggplot(
  br_elements,
  aes(x = element, y = n)
) +
  geom_segment(aes(
    x = element, xend = element,
    y = 0, yend = 375
  ),
  color = "lightgrey", size = .25,
  linetype = "dashed"
  ) +
  geom_col(aes(fill = element), width = 1, color = "black") +
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  scale_y_log10() + # no reason, except that it looks nicer and colorful
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = br_elements$angle, size = 11, color = "black", family = "Roboto condensed"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.caption = element_text(size = 15, family = "Roboto condensed"),
    plot.title = element_text(family = "Roboto condensed", face = "bold", size = 17, hjust = 0.5)
  ) +
  coord_polar(start = 0)



g3 <- ggplot(data = long, aes(x = freq, y = element, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges() +
  scale_fill_viridis_c(name = "Frequency") +
  theme(
    text = element_text(colour = "black", family = "Roboto condensed"),
    axis.text = element_text(colour = "black", family = "Roboto condensed", size = 13),
    axis.title.x = element_blank(),
    axis.title = element_text(colour = "black", family = "Roboto condensed", face = "bold"),
    panel.grid = element_blank()
  ) +
  labs(
    y = "Element",
    caption = "Data provided by 538 | Plot by @david_carayon\nInspiration : @jmcastagnetto"
  )

# Plot --------------------------------------------------------------------

plot_grid(g1, plot_grid(g2, g3), ncol = 1, rel_heights = c(1, 1.45)) +
  ggsave("README_figs/bob_ross_tidytuesday.png", dpi = "retina", width = 13.1, height = 10.4)
