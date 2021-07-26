##########################################################
# Estimate Pi using R!
# This estimate considers a quarter 1/4 circle and square!
# this script partialy depends on this blog post
# https://helloacm.com/r-programming-tutorial-how-to-compute-pi-using-monte-carlo-in-r/
# script by Isaac J. Faber
##########################################################

# create two uniform random variables (points on an xy grid)

x <- runif(100000)
y <- runif(100000)

# distance of points to the center of the circle

z <- sqrt(x^2 + y^2)

# When z is less than 1 the point is within the circle

length(which(z <= 1))

# Calculate Your Pi Estimate!
# Pi is the proportion of points within the circle (area) divided by the total number of points
# we multiply by 4 to readjust from the 1/4 circle

pi_estimate <- length(which(z <= 1)) * 4 / 
                                      length(z)

pi_estimate

# Now plot your Points and Estimate

plot(x[which(z <= 1)], y[which(z <= 1)],
     xlab="X",
     ylab="Y",
     main=paste0("Pi Estimate: ",
                 round(pi_estimate,4)))

points(x[which(z > 1)],y[which(z > 1)],
       col='blue')

# Animate it!
library(animation)

# make sure you install FFmpeg on the terminal (here in RStuido) first. Use these three lines
# -----------------------------------------------
# sudo add-apt-repository ppa:jonathonf/ffmpeg-4
# sudo apt-get update
# sudo apt-get install ffmpeg
#################################################################################################

saveVideo({
  ani.options(interval = 0.05, nmax = 300)
  
  #create a loop!
  x <- runif(10)
  y <- runif(10)

  for(i in 1:1000){
    
    z <- sqrt(x^2 + y^2)
    
    pi_estimate <- length(which(z <= 1)) * 4 / 
                                        length(z)
    
    plot(x[which(z <= 1)], y[which(z <= 1)],
         xlab="X",
         ylab="Y",
         main=paste0("Pi Estimate: ",
                     round(pi_estimate,4)))
    
    points(x[which(z > 1)],y[which(z > 1)],
           col='blue')
      
      x <- c(x, runif(10))
      y <- c(y, runif(10))
    print(i)
  }

  
}, video.name = "Pi.mp4", other.opts = "-pix_fmt yuv420p -b 300k")




