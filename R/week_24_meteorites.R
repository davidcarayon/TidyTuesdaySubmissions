library(tidyverse)
library(sf)
library(ggforce)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggdark)
library(cowplot)
library(ggsn)

# Data -------------------------------------------------------------------

# Import data and remove outliers
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv") %>%
  filter(long < 180 & year <= 2013)

# World map
world <- ne_countries(scale = "medium", returnclass = "sf")

world_map <- ggplot() +
  geom_sf(data = world) +
  geom_point(data = meteorites, aes(x = long, y = lat), color = "darkturquoise", alpha = .1) +
  guides(size = FALSE) +
  blank() +
  dark_mode() +
  labs(title = "Location of meteorite crashes") +
  theme(
    axis.title = element_blank(),
    plot.title = element_text(size = 17, face = "bold", family = "mono"),
    plot.caption = element_text(size = 13, family = "mono")
  )


# Mass distribution  ------------------------------------------------------
mass_d <- meteorites %>%
  mutate(class = fct_lump(class, n = 11)) %>%
  filter(class != "Other") %>%
  group_by(class) %>%
  mutate(mean_mass = median(mass, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(class = fct_reorder(class, mean_mass)) %>%
  ggplot(aes(x = class, y = mass, color = class)) +
  scale_fill_viridis_c() +
  geom_violin() +
  geom_point(aes(x = class, y = mean_mass, color = class), size = 8, shape = 18) +
  scale_y_log10() +
  coord_flip() +
  dark_theme_classic() +
  theme(
    axis.text = element_text(size = 13, family = "mono"),
    axis.title = element_text(size = 17, family = "mono"),
    plot.title = element_text(size = 15, face = "bold", family = "mono"),
    legend.text = element_text(family = "mono", size = 12),
    legend.title = element_text(family = "mono", size = 13, face = "bold"),
    plot.caption = element_text(size = 13, family = "mono")
  ) +
  labs(y = "Mass in grams", x = "Meteorite class", title = "Median mass value and distribution of the 10 most common meteorite classes", color = "Meteorite class")


# Number of meteorites ----------------------------------------------------
meteorites %>%
  distinct(id, year, mass) %>%
  filter(year >= 1900 & year != 2101) %>%
  ggplot(aes(x = year)) +
  geom_histogram(fill = "pink", color = "black", binwidth = 10) +
  dark_theme_classic() +
  theme(
    axis.text = element_text(size = 13, family = "mono"),
    axis.title = element_text(size = 17, family = "mono"),
    plot.title = element_text(size = 17, face = "bold", family = "mono"),
    legend.text = element_text(family = "mono", size = 12),
    legend.title = element_text(family = "mono", size = 13, face = "bold"),
    plot.caption = element_text(size = 13, family = "mono")
  ) +
  labs(y = "Count", title = "Number of meteorites found from 1900 to 2013")


# Time series -------------------------------------------------------------

## Mann-Kendal trend test
counts <- meteorites %>%
  distinct(id, year, mass) %>%
  filter(year >= 1975 & year != 2101) %>%
  count(year) %>%
  arrange(year) %>%
  mutate(n = as.numeric(n))

mk_test <- round(trend::mk.test(ts(counts$n))$p.value, 2)

## Time series plot with a second order polynomial trend
ts <- meteorites %>%
  distinct(id, year, mass) %>%
  filter(year >= 1975) %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "purple", size = 4, shape = 15) +
  geom_line(color = "purple", size = 2) +
  dark_theme_classic() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1.5, level = 0.95, se = TRUE, fill = "green") +
  theme(
    axis.text = element_text(size = 13, family = "mono"),
    axis.title = element_text(size = 17, family = "mono"),
    plot.title = element_text(size = 17, face = "bold", family = "mono"),
    legend.text = element_text(family = "mono", size = 12),
    legend.title = element_text(family = "mono", size = 13, face = "bold"),
    plot.caption = element_text(size = 13, family = "mono"),
    plot.subtitle = element_text(family = "mono")
  ) +
  labs(
    y = "Number of meteorites",
    x = "Time",
    title = "Number of meteorites observed worldwide between 1975 and 2013",
    color = "Meteorite class",
    caption = "Data from the Meteoritical Society and shared by NASA \nPlot from @david_carayon",
    subtitle = paste0("The Mann-Kendal trend tests indicates a significant increasing trend (P = ", 0.02, ")")
  ) +
  geom_curve(aes(x = 1984, y = 3000, xend = 1979.4, yend = 3100), curvature = 0.3, arrow = arrow(length = unit(2, "mm")), color = "white") +
  annotate("text", x = 1990, y = 2920, label = paste0(max(counts$n), " meteorites in 1979"), color = "white", family = "mono", size = 5)


# Final plot layout -------------------------------------------------------
plot_grid(world_map, plot_grid(mass_d, ts), ncol = 1, rel_heights = c(1.45, 1)) +
  ggsave("README_figs/meteorites_tidytuesday.png", dpi = "retina", bg = "black", width = 21, height = 13)
