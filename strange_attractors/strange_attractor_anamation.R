#############################################
# Building Anamations of Strange Attractors
# by: Isaac J. Faber
# some source code used from: https://codingclubuc3m.rbind.io/post/2019-10-15/
# make sure to set your working directory 
#############################################

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

# with a large n this will take quite a while
pb <- progress_bar$new(total = n)
for(i in 1:n){
  temp <- clifford_attractor(a,b,c,d,x_n,y_n)
  x_n <- temp[1]
  y_n <- temp[2]
  clifford_data[i,1] <- temp[1]
  clifford_data[i,2] <- temp[2]
  pb$tick()
}

ggplot(clifford_data[1:1000,], aes(x = x, y = y)) +
  geom_path(linejoin = "round",alpha=0.1) +
  coord_equal() +
  theme_void()
