##################################################################################################
# A script with animation examples using R 
# by: Isaac J. Faber, examples derived from https://yihui.name/animation/examples/
# WARNING!!!! without depfendcies installed you can only render to HTML
# Make sure you install FFmpeg (for videos) and Graphics Magick (for gifs) on the terminal (here in RStuido) first. 
# Use these commands for FFmepg
# -----------------------------------------------
# sudo add-apt-repository ppa:jonathonf/ffmpeg-4
# sudo apt-get update
# sudo apt-get install ffmpeg
# -----------------------------------------------
# Use these commands for Graphics Magick
# -----------------------------------------------
# sudo apt-get install python-software-properties
# sudo apt-get install software-properties-common
# sudo add-apt-repository ppa:rwky/graphicsmagick
# sudo apt-get update
# sudo apt-get install graphicsmagick
#################################################################################################

# The package is written by Yihui with a companion website here: https://yihui.name/animation/
library(animation)

# Some helper libraries for data munging
library(tidyverse)

# check the animation options with
ani.options()

# set the working directory to the html folder. 
# This way we can host our animation as a public web page using MatrixDS

setwd("~/html/lease_squares")

ani.options(interval = 0.05, 
            nmax = 100)

# create and html version with controls
desc = c("This is a super cool example of Gradient Descent")
saveHTML({
  f1 = function(x, y) x^2 + 3 * sin(y)
  xx = grad.desc(f1, pi * c(-2, -2, 2, 2), c(-2 * pi, 2))
  xx$persp(col = "lightblue", theta = 30, phi = 30)
},title = "Demo of Gradient Descent", 
description = desc, verbose = FALSE)

# create a gif with no controls
saveGIF({
  f1 = function(x, y) x^2 + 3 * sin(y)
  xx = grad.desc(f1, pi * c(-2, -2, 2, 2), c(-2 * pi, 2))
  xx$persp(col = "lightblue", theta = 30, phi = 30)
},title = "Demo of Gradient Descent", 
description = desc, verbose = FALSE,
convert = 'gm convert', movie.name = 'grad_des_animation.gif')

#another html example for least squres
setwd("~/html/least_squares")

par(mar = c(5, 4, 0.5, 0.1))
ani.options(interval = 0.3, nmax = 50)

saveHTML({
  least.squares()
},title = "Demo of Least Squares", 
description = desc, verbose = FALSE)
