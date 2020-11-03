library(ggplot2)

# Tidyquant colors
blue  <- "#2c3e50"
green <- "#18BC9C"
white <- "#FFFFFF"
grey  <- "grey80"
base_family = "Avenir Next Pro"
base_size = 15

theme_set(theme_grey(base_family = "Avenir Next Pro", base_size = 15))

## Theme inspired from the {tidyquant} package
theme_update(
  # Base Inherited Elements
  line               =  element_line(colour = blue, size = 0.5, linetype = 1,
                                              lineend = "butt"),
  rect               =  element_rect(fill = white, colour = blue,
                                              size = 0.5, linetype = 1),
  text               =  element_text(family = base_family, face = "plain",
                                              colour = blue, size = base_size,
                                              lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                                              margin = margin(), debug = FALSE),
  
  # Axes
  axis.line          = element_blank(),
  axis.text          = element_text(size = rel(0.8)),
  axis.ticks         = element_line(color = grey, size = rel(1/3)),
  axis.title         = element_text(size = rel(1.0), face = "bold"),
  
  # Panel
  panel.background   = element_rect(fill = white, color = NA),
  panel.border       = element_rect(fill = NA, size = rel(1/2), color = blue),
  panel.grid.major   = element_line(color = grey, size = rel(1/3)),
  panel.grid.minor   = element_line(color = grey, size = rel(1/3)),
  panel.grid.minor.x = element_blank(),
  panel.spacing      = unit(.75, "cm"),
  
  # Legend
  legend.key         = element_rect(fill = white, color = NA),
  legend.position    = "top",
  legend.title = element_text(face = "bold"),
  
  # Strip (Used with multiple panels)
  strip.background   = element_rect(fill = blue, color = blue),
  strip.text         = element_text(color = white, size = rel(0.8), margin = margin(t = 5, b = 5)),
  
  # Plot
  plot.title         = element_text(size = rel(1.2), hjust = 0,
                                             margin = margin(t = 0, r = 0, b = 4, l = 0, unit = "pt")),
  plot.subtitle      = element_text(size = rel(0.9), hjust = 0,
                                             margin = margin(t = 0, r = 0, b = 3, l = 0, unit = "pt")),
  
  # Complete theme
  complete = TRUE
)

scale_color_cust <- function(...) {
  pal <- unname(palette_light()) %>% rep(100)
  scale_color_manual(values = pal)
}

scale_fill_cust <- function(...) {
  pal <- unname(palette_light()) %>% rep(100)
  scale_fill_manual(values = pal)
}

palette_light <- function() {
  c(
    blue         = "#2c3e50", # blue
    red          = "#e31a1c", # red
    green        = "#18BC9C", # green
    yellow       = "#CCBE93", # yellow
    steel_blue   = "#a6cee3", # steel_blue
    navy_blue    = "#1f78b4", # navy_blue
    light_green  = "#b2df8a", # light_green
    pink         = "#fb9a99", # pink
    light_orange = "#fdbf6f", # light_orange
    orange       = "#ff7f00", # orange
    light_purple = "#cab2d6", # light_purple
    purple       = "#6a3d9a"  # purple
  ) %>% toupper()
}
