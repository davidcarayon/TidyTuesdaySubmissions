library(tidyverse)
library(janitor)
library(ggridges)
library(lubridate)
library(cowplot)

# clean dataset from lizawood's github
url <- "https://raw.githubusercontent.com/lizawood/apps-and-games/master/PC_Games/PCgames_2004_2018_raw.csv"

# read in raw data
raw_df <- url %>%
  read_csv() %>%
  janitor::clean_names()

# clean up some of the factors and playtime data
clean_df <- raw_df %>%
  mutate(
    price = as.numeric(price),
    score_rank = word(score_rank_userscore_metascore, 1),
    average_playtime = word(playtime_median, 1),
    median_playtime = word(playtime_median, 2),
    median_playtime = str_remove(median_playtime, "\\("),
    median_playtime = str_remove(median_playtime, "\\)"),
    average_playtime = 60 * as.numeric(str_sub(average_playtime, 1, 2)) +
      as.numeric(str_sub(average_playtime, 4, 5)),
    median_playtime = 60 * as.numeric(str_sub(median_playtime, 1, 2)) +
      as.numeric(str_sub(median_playtime, 4, 5)),
    metascore = as.double(str_sub(score_rank_userscore_metascore, start = -4, end = -3))
  ) %>%
  select(-score_rank_userscore_metascore, -score_rank, -playtime_median) %>%
  rename(publisher = publisher_s, developer = developer_s) %>%
  mutate(release_date = mdy(release_date))

library(ggdark)
library(ggforce)


## Custom function to find the max value for each owners category, to create an appropriately ordered factor.

return_max <- function(str) {
  val <- unlist(str_split(str, "\\.."))[2] %>%
    str_trim() %>%
    str_remove_all(",")
  return(val)
}

## Vector of levels in the right order
levs <- clean_df %>%
  distinct(owners) %>%
  mutate(max_owners = as.numeric(map_chr(owners, return_max))) %>%
  arrange(max_owners) %>%
  pull(owners)

## Modifying the dataframe
clean_df$owners <- factor(clean_df$owners, levels = levs)


# Flipped violin plot -----------------------------------------------------

## Separate date with median values
median_values <- clean_df %>%
  group_by(owners) %>%
  summarise(
    med = median(metascore, na.rm = TRUE),
    mean = median(metascore, na.rm = TRUE)
  )


g1 <- ggplot(clean_df, aes(x = owners, y = metascore)) +
  geom_boxplot(aes(color = owners)) +
  geom_point(data = median_values, aes(x = owners, y = med, fill = owners), shape = 23, size = 4, color = "black") +
  dark_mode() +
  guides(fill = FALSE, color = FALSE) +
  coord_flip() +
  labs(y = "Metascore", x = "# of owners", title = "Metascore values according to \nthe number of owners") +
  theme(
    axis.text = element_text(family = "mono", size = 11),
    axis.title = element_text(family = "mono", face = "bold", size = 15),
    plot.title = element_text(family = "mono", face = "bold", size = 17)
  ) +
  scale_color_viridis_d(option = "plasma") +
  scale_fill_viridis_d(option = "plasma")


# Geom_tiles  -------------------------------------------------------------

## Credits to @Argaadya1 for inspiration.

g2 <- clean_df %>%
  mutate(
    release_year = year(release_date),
    release_month = month(release_date)
  ) %>%
  group_by(release_year, release_month) %>%
  summarise(
    med_price = median(price, na.rm = TRUE),
    med_playtime = median(average_playtime, na.rm = TRUE),
    mean_metascores = mean(metascore, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = release_month, y = release_year)) +
  geom_tile(aes(fill = mean_metascores), color = "black") +
  scale_fill_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_ridges() +
  scale_x_continuous(breaks = seq(1, 12, 1), labels = month.abb) +
  scale_y_continuous(breaks = seq(2004, 2020, 2)) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    text = element_text(colour = "white", family = "mono"),
    axis.text = element_text(colour = "white", family = "mono"),
    axis.title = element_text(colour = "white", family = "mono", face = "bold"),
    panel.grid = element_blank()
  ) +
  labs(x = "Release month", y = "Release year", fill = "Metascore", title = "Average metascore of games according to month \nand year of release", caption = "Data provided by @brightcdns | Plot by @david_carayon")



# Line plot ---------------------------------------------------------------

g3 <- clean_df %>%
  mutate(release_year = year(release_date)) %>%
  group_by(release_year) %>%
  summarise(
    med_price = median(price, na.rm = TRUE),
    med_playtime = median(average_playtime, na.rm = TRUE),
    mean_metascores = mean(metascore, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = release_year, y = mean_metascores)) +
  geom_line(color = "green2") +
  geom_smooth(color = "gold", se = FALSE, size = 2) +
  geom_point(color = "white", shape = 24, aes(fill = mean_metascores), size = 5) +
  scale_fill_viridis_c(option = "plasma") +
  geom_curve(aes(x = 2010, y = 77.3, xend = 2005.2, yend = 78.5), curvature = 0.2, arrow = arrow(length = unit(2, "mm")), color = "white") +
  annotate("label", x = 2010, y = 77.2, label = "Games released in 2005 have an avg.\n metascore of 78.3/100", color = "white", fill = "black", family = "mono", size = 4) +
  dark_mode(.theme = theme_classic()) +
  theme(
    axis.text = element_text(size = 13, family = "mono"),
    axis.title = element_text(size = 15, face = "bold", family = "mono"),
    plot.title = element_text(size = 17, family = "mono", face = "bold"),
    plot.subtitle = element_text(size = 15, family = "mono", face = "bold")
  ) +
  labs(x = "Release year", y = "Average metascore", title = "Average metascore over the years") +
  guides(fill = FALSE)



# Final plot grid ---------------------------------------------------------
plot_grid(g3, plot_grid(g1, g2), ncol = 1, rel_heights = c(1.45, 1)) +
  ggsave("README_figs/videogames_tidytuesday.png", dpi = "retina", width = 15.7, height = 9.47)
