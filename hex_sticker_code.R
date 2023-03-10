# make hex sticker for Exeter Data Analytics

# based on a simple bar plot

# load in packages
library(hexSticker)
library(tidyverse)
library(palmerpenguins)
library(showtext)
if(!require(BrewerUoE))remotes::install_github('padpadpadpad/BrewerUoE')
library(BrewerUoE)

# load in dataset
data("penguins")

d <- penguins

cols = uoe_colours(c('Exeter Dark Green', 'Exeter Deep Green', 'Exeter Highlight Green'))

# randomly sample 50 points
d_sample <- slice_sample(d, n = 50)

d_summary <- group_by(d_sample, species) %>%
  summarise(mean = mean(body_mass_g, na.rm = TRUE), .groups = 'drop') %>%
  mutate(group = 1)

# make box plot
p1 <- ggplot(d_sample, aes(forcats::fct_relevel(species, 'Gentoo', after=1) , -1*body_mass_g)) +
  geom_boxplot(aes(fill = species, col = species), outlier.shape = NA, show.legend = FALSE) +
  #stat_summary(geom = "crossbar", fatten = 0, color = "white", width = 0.4,
    #fun.data = function(x) {
      #return(c(
        #y = stats::median(x),
        #ymin = stats::median(x),
        #ymax = stats::median(x)
      #))
    #}
  #) +
  geom_point(position = position_jitter(width = 0.1), fill = 'white', shape = 21, size = 0.65, stroke = 0.2) +
  scale_fill_manual(values = unname(cols)) +
  scale_color_manual(values = unname(cols)) +
  geom_line(aes(species, -1*mean, group = group), d_summary, col = 'black', linewidth = 1.4, lineend = 'round') +
  geom_line(aes(species, -1*mean, group = group), d_summary, col = 'white', linewidth = 0.8, lineend = 'round') +
  #geom_point(aes(species, mean), d_summary, size = 2, shape = 21, fill = 'white') +
  theme_void()

p1

# check what fonts are available
showtext_auto()
sysfonts::font_families()

# add font
font_add_google("Outfit")

# check fonts again
sysfonts::font_families()

# make sticker
sticker(p1,
        package="Exeter Data",
        url = 'Analytics Hub',
        p_color = 'black',
        p_family = "Outfit",
        p_size = 5,
        p_x = 1.03,
        p_y = 1.5,
        u_color = 'dark grey',
        u_family = "Outfit",
        u_size = 5,
        u_x = 0.42,
        u_y = 1.32,
        u_angle = 0,
        s_x = 1,
        s_y = 0.7,
        s_width = 1.4,
        s_height = 1,
        h_fill = 'white',
        h_color = uoe_colours('Exeter Highlight Green'),
        h_size = 5,
        filename="hex_sticker.png",
        white_around_sticker = TRUE)

# make new dataset with defined bins
d2 <- filter(d, !is.na(body_mass_g)) %>%
  mutate(., bin = cut_interval(body_mass_g, n = 8)) %>%
  separate(bin, c('start', 'end'), sep = ',', remove = FALSE) %>%
  mutate(mid_point = ((parse_number(start) + parse_number(end)) /2)) %>%
  mutate(group = 1)
  

#Alternative version
p2 <- filter(d2, !is.na(body_mass_g)) %>%
  ggplot(., aes(x = as.character(mid_point), y = bill_depth_mm)) +
  geom_boxplot(fill = uoe_colours('Exeter Highlight Green'), outlier.shape = NA, linewidth = 0) +
  stat_summary(aes(group = group), fun = mean , colour=uoe_colours('Exeter Deep Green'), geom="line", linewidth = 2) +
  stat_summary(fun = mean , colour=uoe_colours('Exeter Deep Green'), geom="point", size = 3) +
  stat_summary(fun = mean , colour="white", geom="point", size = 1.6) +
  stat_summary(aes(group = group), fun = mean , colour="white", geom="line", linewidth = 0.7) +
  theme_void()

#Make alternative sticker
sticker(p2,
        package="Exeter Data",
        url = 'Analytics Hub',
        p_color = 'black',
        p_family = "Outfit",
        p_size = 5,
        p_x = 1.03,
        p_y = 1.5,
        u_color = 'dark grey',
        u_family = "Outfit",
        u_size = 5,
        u_x = 0.42,
        u_y = 1.32,
        u_angle = 0,
        s_x = 1,
        s_y = 0.8,
        s_width = 1.4,
        s_height = 1.4,
        h_fill = 'white',
        h_color = uoe_colours('Exeter Highlight Green'),
        h_size = 5,
        filename="hex_sticker.png",
        white_around_sticker = TRUE)
