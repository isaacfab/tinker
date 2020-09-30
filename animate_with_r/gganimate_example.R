#################################################################################################
# The is the first animation for the blog post and is inspired by Han Roslings Gapminder Data Viz
# by: Isaac J. Faber most of the code borrowed from: https://github.com/thomasp85/gganimate
# WARNING!!!!
# make sure you install FFmpeg on the terminal (here in RStuido) first. Use these three lines
# -----------------------------------------------
# sudo add-apt-repository ppa:jonathonf/ffmpeg-4
# sudo apt-get update
# sudo apt-get install ffmpeg
#################################################################################################

#for saving purposes
setwd('~/html')

library(datasauRus)
library(ggplot2)
library(gganimate)

ggplot(datasaurus_dozen, aes(x=x, y=y))+
  geom_point()+
  theme_minimal() +
  transition_states(dataset, 3, 1) + 
  ease_aes('cubic-in-out')