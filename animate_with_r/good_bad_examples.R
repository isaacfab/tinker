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

########################## Data Prep ####################
# using gdp data from the world bank as an example
gdp<-read_csv('~/data/GDP.csv',skip = 3)

gdp_filtered <- gdp %>% filter(`Country Name`=='China'|
                               `Country Name`=='India'|
                               `Country Name`=='United States') %>%
                    select(c(1,5:62)) %>%
                    gather(year,gdp,-`Country Name`)

#change some variable classes for plotting convience
gdp_filtered$gdp<-as.numeric(gdp_filtered$gdp)
gdp_filtered$year<-as.numeric(gdp_filtered$year)
gdp_filtered$`Country Name`<-as.factor(gdp_filtered$`Country Name`)

summary(gdp_filtered)

######################## End Data Prep and Start Plotting!! ######################
# let's use ggplot for our base plot 

ggplot(gdp_filtered,aes(x=year,y=gdp,group=`Country Name`,colour=`Country Name`)) +
  geom_line() +
  scale_x_continuous(breaks=c(1960,1980,2000,2017)) +
  ylim(0,(2*10^13))+
  xlab('') +
  ylab('Gross Domestic Product') +
  theme_bw() +
  theme(legend.title=element_blank())

# set some of the options by specifiying the commands
ani.options(interval = 0.15, 
            nmax = 100)

## The good animation as a simple GIF
saveGIF({
  end_year = 2017 #last year of the plot
  num_years = 30 #number of years in the animation
  #create a loop the does the subsetting
  for(i in 1:num_years){
    gdp_subset <- gdp_filtered %>% filter(year <= end_year-(num_years-i))
    #write the plot with a subset
    p<-ggplot(gdp_subset,aes(x=year,y=gdp,group=`Country Name`,colour=`Country Name`)) +
      geom_line(size = 2) +
      scale_x_continuous(breaks=c(1960,1980,2000,2017)) +
      ylim(0,(2*10^13))+
      xlab('') +
      ylab('Gross Domestic Product') +
      theme_bw() +
      theme(legend.title=element_blank())
    print(p)
  }#close the for loop
  
}, convert = 'gm convert', movie.name = 'good_animation.gif') #close the animation builder

# set some of the options by specifiying the commands
ani.options(interval = 1, 
            nmax = 100)

## The bad animation as a simple GIF
saveGIF({
  end_year = 2017 #last year of the plot
  num_years = 30 #number of years in the animation
  #create a loop the does the subsetting
  for(i in c('China','India','United States')){
    gdp_subset <- gdp_filtered %>% filter(`Country Name` == i)
    #write the plot with a subset
    p<-ggplot(gdp_subset,aes(x=year,y=gdp,group=`Country Name`,colour=`Country Name`)) +
      geom_line(size = 2) +
      scale_x_continuous(breaks=c(1960,1980,2000,2017)) +
      ylim(0,(2*10^13))+
      xlab('') +
      ylab('Gross Domestic Product') +
      theme_bw() +
      theme(legend.title=element_blank())
    print(p)
  }#close the for loop
  
}, convert = 'gm convert', movie.name = 'bad_animation.gif') #close the animation builder

