#################################################################################################
# The is the first animation for the blog post and is inspired by Han Roslings Gapminder Data Viz
# by: Isaac J. Faber most of the code borrowed from: https://github.com/thomasp85/gganimate
# WARNING!!!! some changes
# make sure you install FFmpeg on the terminal (here in RStuido) first. Use these three lines
# -----------------------------------------------
# sudo add-apt-repository ppa:jonathonf/ffmpeg-4
# sudo apt-get update
# sudo apt-get install ffmpeg
#################################################################################################

library(gapminder)
library(ggplot2)
#use devtools::install_github('thomasp85/gganimate') to install as it is not on CRAN
library(gganimate)

# ggplot from the github README
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')
