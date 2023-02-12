# make hex sticker for Exeter Data Analytics

# based on a simple bar plot

# load in packages
library(hexSticker)
library(tidyverse)
library(palmerpenguins)
library(BrewerUoE) # remotes::install_github('padpadpadpad/brewerUoE')
library(showtext)

# load in dataset
data("penguins")

d <- penguins

cols = uoe_colours(c('Exeter Dark Green', 'Exeter Deep Green', 'Exeter Highlight Green'))

d_summary <- group_by(d, species) %>%
  summarise(mean = mean(body_mass_g, na.rm = TRUE), .groups = 'drop') %>%
  mutate(group = 1)

# make box plot
p1 <- ggplot(d, aes(species, body_mass_g)) +
  geom_boxplot(aes(fill = species, col = species), outlier.shape = NA, show.legend = FALSE) +
  stat_summary(geom = "crossbar", fatten = 0, color = "white", width = 0.4,
    fun.data = function(x) {
      return(c(
        y = stats::median(x),
        ymin = stats::median(x),
        ymax = stats::median(x)
      ))
    }
  ) +
  #geom_point(position = position_jitter(width = 0.1), fill = 'white', shape = 21, size = 0.3, stroke = 0.2) +
  scale_fill_manual(values = unname(cols)) +
  scale_color_manual(values = unname(cols)) +
  geom_line(aes(species, mean, group = group), d_summary, linewidth = 0.8) +
  geom_point(aes(species, mean), d_summary, size = 3, shape = 21, fill = 'white') +
  theme_void()

p1

# check what fonts are available
showtext_auto()
sysfonts::font_families()

# add font
font_add_google("Bebas Neue")

# check fonts again
sysfonts::font_families()

# make sticker
sticker(p1,
        package="Exeter Data",
        url = 'Analytics Hub',
        p_color = 'black',
        p_family = "Bebas Neue",
        p_size = 5,
        p_x = 1,
        p_y = 1.6,
        u_color = 'white',
        u_family = "Bebas Neue",
        u_size = 5,
        u_x = 0.52,
        u_y = 1.42,
        u_angle = 0,
        s_x = 1,
        s_y = 0.8,
        s_width = 1.3,
        s_height = 1.1,
        h_fill = 'light grey',
        h_color = uoe_colours('Exeter Highlight Green'),
        h_size = 5,
        filename="hex_sticker.png",
        white_around_sticker = TRUE)  
