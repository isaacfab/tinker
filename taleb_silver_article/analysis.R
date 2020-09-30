#####################################################################################
# A script to analyze fivethrityeight data for the Taleb and Nate Silver twitter war
# by Isaac J. Faber
#####################################################################################

# use some data from the fivethirtyeight github site and the R package of the same name
# remotes::install_github("rudeboybert/fivethirtyeight", build_vignettes = TRUE)
library(fivethirtyeight)
library(tidyverse)
library(jsonlite)
library(lubridate)

# read the JSON object from the fivethirtyeight (this data was scraped from the 2016 forecast website)
fte_2016_all <- fromJSON("~/data/538_2016.json", flatten=TRUE)

fte_2016_forecasts <- fte_2016_all$forecasts$all

#quick interm plot will all of the data
ggplot(data=fte_2016_forecasts, aes(x=date,y=models.plus.winprob,group=candidate,colour=candidate)) + 
  geom_line()

fte_final <- fte_2016_forecasts %>% 
              filter(candidate %in% c('Clinton','Trump','Johnson')) %>%
              mutate(date = as.Date(date, origin = lubridate::origin)) 


break_dates <- as.Date( c('2016-6-1',
                          '2016-7-1',
                          '2016-8-1',
                          '2016-9-1',
                          '2016-10-1',
                          '2016-11-1'), origin = lubridate::origin)

label_dates <- c('June',
                 'July',
                 'August',
                 'September',
                 'October',
                 'November')

# basic election plot
ggplot(data=fte_final, aes(x=date,y=models.plus.winprob,group=candidate,colour=candidate)) +
  geom_line() +
  scale_colour_manual(values = c('Blue','Yellow','Red')) +
  scale_x_date(breaks=break_dates, labels = label_dates) +
  xlab('') +
  ylab('Forecast Probability of Winning (Polls Plus)') +
  theme_bw()

fte_last_forecast <- fte_final[1:3,] %>%
                      select(c('date','models.plus.winprob')) %>%
                      mutate(candidate = 'hold')

# basic election plot showing the difference between taleb and silver's view
ggplot(data=fte_final, aes(x=date,y=models.plus.winprob,group=candidate,colour=candidate)) +
  geom_line() +
  geom_point(data=fte_last_forecast,aes(x=date,y=models.plus.winprob,size = 8)) +
  scale_colour_manual(values = c('blue','red','blue','blue')) +
  scale_x_date(breaks=break_dates, labels = label_dates) +
  xlab('') +
  ylab('Forecast Probability of Winning (Polls Plus)') +
  theme_bw() + 
  theme(legend.position="none")

#get a list 538 public of datasets
head(hist_senate_preds)

# the majority of the forecasts for the senate are 0 or 1.. not very probabilistic
hist(hist_senate_preds$forecast_prob)

probs_only <- hist_senate_preds %>% filter(!(forecast_prob %in% c(1,0))) %>%
  mutate(results_num = 0) %>%
  mutate(results_num=replace(results_num, result=='Loss', 0)) %>%
  mutate(results_num=replace(results_num, result=='Lost', 0))%>%
  mutate(results_num=replace(results_num, result=='Win', 1))

# quick plot of senate race results                                                 
qplot(probs_only$forecast_prob,probs_only$results_num)

senate_agg <- data.frame()
# use a loop to gather the info row by row
for(i in 1:nrow(probs_only)){
  
  #collect the probability and result from the first team
  temp_data <- data.frame(
    win_prob = probs_only$forecast_prob[i],
    result = probs_only$results_num[i]
  )
  
  senate_agg <- rbind(senate_agg, temp_data)
} 

#a quick look at win probabilies
hist(senate_agg$win_prob)

#with 32000 games we can do some fine bining to

senate_probs_vs_portions <- data.frame()

for(i in seq(5,100,5)){
  z1 <- i/100
  z2 <- (i-5)/100
  temp_probs <- senate_agg %>% filter(win_prob > z2 & 
                                        win_prob< z1)

  portion <- mean(temp_probs$result)
  prob <- (z1-.025)
  sample_size <- nrow(temp_probs)
  temp <- data.frame(prob = prob, 
                     realized_portion = portion,
                     sample_size = sample_size)
  
  senate_probs_vs_portions <- rbind(senate_probs_vs_portions, temp)
}

# plot the difference between forecasted probs and realized portions
ggplot(data = senate_probs_vs_portions, aes(x=prob,y=realized_portion, colour=sample_size)) +
  geom_point() +
  theme_bw()

# look at the nfl_elo data set add collect some meta data

nfl_agg_results <-data.frame()
nfl_elo <- fivethirtyeight::nfl_elo

# use a loop to gather the info row by row
for(i in 1:nrow(nfl_elo)){
  result_1 <- 0
  result_2 <- 0
  
  if(nfl_elo$score1[i]>nfl_elo$score2[i]) result_1 <- 1
  else result_2 <- 1
  
  #collect the probability and result from the first team
  temp_data <- data.frame(
    win_prob = nfl_elo$elo_prob1[i],
    result = result_1
  )
  
  nfl_agg_results <- rbind(nfl_agg_results, temp_data)
  
  #collect the probability and result from the second team
  temp_data <- data.frame(
    win_prob = nfl_elo$elo_prob2[i],
    result = result_2
  )
  
  nfl_agg_results <- rbind(nfl_agg_results, temp_data)
} 

#a quick look at win probabilies
hist(nfl_agg_results$win_prob)

#with 32000 games we can do some fine bining to

nfl_probs_vs_portions <- data.frame()

for(i in 1:100){
  z1 <- i/100
  z2 <- (i-1)/100
  temp_probs <- nfl_agg_results %>% filter(win_prob>(z2)&win_prob<z1)
  
  portion <- mean(temp_probs$result)
  prob <- (z1-.005)
  sample_size <- nrow(temp_probs)
  temp <- data.frame(prob = prob, 
                     realized_portion = portion,
                     sample_size = sample_size)
  
  nfl_probs_vs_portions <- rbind(nfl_probs_vs_portions, temp)
}

# plot the difference between forecasted probs and realized portions
ggplot(data = nfl_probs_vs_portions, aes(x=prob,y=realized_portion, colour=sample_size)) +
  geom_point() +
  theme_bw()

senate_probs_vs_portions <- senate_probs_vs_portions %>% mutate(type = 'Senate')
nfl_probs_vs_portions <- nfl_probs_vs_portions %>% mutate(type = 'NFL')

# combine the nfl and senate data
combined_data <- rbind(senate_probs_vs_portions, nfl_probs_vs_portions)

ggplot(data = combined_data, aes(x=prob,y=realized_portion, colour=type)) +
  geom_point() + 
  scale_color_manual(values = c('Blue','Red')) +
  xlab('538 Last Reported Probability') +
  ylab('Actual Portion of Outcomes') +
  theme_bw() +
  theme(legend.title=element_blank())
