##################################################################################
# Building Anamations of Strange Attractors
# by: Isaac J. Faber
# some source code used from: https://codingclubuc3m.rbind.io/post/2019-10-15/
# make sure to set your working directory 
##################################################################################

library(tidyverse)
library(animation)
library(progress) #for long running examples

source('strange_attractor_functions.R')

######### populate data frame with attractor values ##################
#number of iterations
n <- 1000000
#parameter inits
a <- -1.25
b <- -1.25
c <- -1.82
d <- -1.91
x_n <- 0
y_n <- 0

# data frame inits
clifford_data <- data.frame(x = rep(0,n),
                            y = rep(0,n))

# with a large n this R loop will take quite a while
# I already did this for 1 million points and it is saved here as clifford_R.RData

pb <- progress_bar$new(total = n)
for(i in 1:n){
  temp <- clifford_attractor(a,b,c,d,x_n,y_n)
  x_n <- temp[1]
  y_n <- temp[2]
  clifford_data[i,1] <- temp[1]
  clifford_data[i,2] <- temp[2]
  pb$tick()
}

ggplot() +
  geom_point(data = clifford_data, aes(x = x, y = y),shape=46, alpha=0.05, color = "#2ca25f") +
  geom_point(data = clifford_data[1:10000,], aes(x = x, y = y) ,shape=46, alpha=.3,color ="#f03b20") +
  coord_equal() +
  theme_void() -> plot

ggsave("clifford_R.png", plot, height = 5, width = 5, units = 'in')

# Or use the cpp function... much faster! 

clifford_data <- clifford_attractor_cpp(10000000, 0, 0, a, b, c, d)

ggplot() +
  geom_point(data = clifford_data, aes(x = x, y = y),shape=46, alpha=0.01, color = "#2ca25f") +
  geom_point(data = clifford_data[1:100000,], aes(x = x, y = y) ,shape=46, alpha=.1,color ="#f03b20") +
  coord_equal() +
  theme_void() -> plot

ggsave("clifford_cpp.png", plot, height = 5, width = 5, units = 'in')

ani.options(interval = 0.25, 
            nmax = 300,
            ani.width = 720,
            ani.height = 720)

saveVideo({
  num_iters <- 100
  pb <- progress_bar$new(total = num_iters)
  for(i in 1:num_iters){
    pb$tick()
    #write the plot with a subset
    p<-ggplot() +
          geom_point(data = clifford_data[1:(100000*i),], aes(x = x, y = y),shape=46, alpha=0.01, color = "#2ca25f") +
          geom_point(data = clifford_data[(100000*(i-1)):(100000*i),], aes(x = x, y = y) ,shape=46, alpha =.1, color ="#f03b20") +
          coord_equal() +
          theme_void()
    print(p)
  }#close the for loop
}, video.name = "clifford_R.mp4", other.opts = "-pix_fmt yuv420p -b 1000k")
