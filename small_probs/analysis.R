###########################################################
# Analysis script for Small Probabilites Article
# by: Isaac J. Faber
###########################################################

library(tidyverse)
library(readxl)
library(rmetalog)
library(extrafont)

# data for shooting comes from a derivative of stanfords msa data posted on kaggle: 
# https://www.kaggle.com/zusmani/us-mass-shootings-last-50-years#Mass%20Shootings%20Dataset%20Ver%205.csv

# data for terror comes from the GTD: 
# https://www.start.umd.edu/gtd/about/

# both raw datasets can be found in the data folder
# average est us population with respect to the dataset 
us_pop <- 270 * 10^6

################### some munging here #################################
shooting_data<-read_csv('~/data/shooting_data/Mass Shootings Dataset Ver 5.csv') %>% 
                mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>% 
                filter_all(any_vars(!is.na(.))) %>%
                mutate(Year = format(Date , '%Y'))

terror_data<-read_excel('~/data/terror_data/globalterrorismdb_0718dist.xlsx') %>%
                filter(country_txt == 'United States') %>% 
                mutate(Date = as.Date(paste0(iyear,'-',iday,'-',imonth),format='%Y-%d-%m')) %>%
                mutate(TotalVictims = (nkill + nwound)) %>%
                filter(!is.na(TotalVictims) && !is.na(Date)) %>% 
                filter_all(any_vars(!is.na(.)))

############ some interesting summary stats and EDA plots ############
table(shooting_data$`Total Number of Victims`)
table(terror_data$TotalVictims)

mean(shooting_data$`Total victims`, na.rm=TRUE)
mean(terror_data$TotalVictims, na.rm=TRUE)

# average yearly victims per 10 million over data set
(sum(shooting_data$`Total victims`, na.rm=TRUE)*10000000 / 48) / us_pop

(sum(terror_data$TotalVictims, na.rm=TRUE)*10000000 / 48) / us_pop

ggplot(data = shooting_data, aes(x=Date,y=`Total victims`)) + 
  geom_point(colour='#636363') +
  scale_y_log10() +
  xlab('') +
  ylab('') +
  ggtitle('Mass Shooting Victims',subtitle = 'Wounded and Fatal by Incident') +
  theme_bw() +
  theme(text=element_text(family="Noe"))

ggplot(data = terror_data, aes(Date,TotalVictims)) + 
  geom_point(colour='#636363') +
  scale_y_log10() +
  xlab('') +
  ylab('') +
  ggtitle('Terrorism Victims',subtitle = 'Wounded and Fatal by Incident') +
  theme_bw() +
  theme(text=element_text(family="Noe"))

########################### Comparative Analysis ###########################################

combined <- data.frame(Date = shooting_data$Date,
                       Total_Victims = shooting_data$`Total victims`,
                       type = 'Mass Shooting') %>% 
            rbind(data.frame(Date = terror_data$Date,
                             Total_Victims = terror_data$TotalVictims,
                             type = 'Terrorism'))

ggplot(data = combined, aes(Date,Total_Victims)) + 
  geom_point(colour='#ff3030') +
  scale_y_log10(breaks = c(0, 1,10,100,1000,10000)) +
  xlab('') +
  ylab('Log Scale') +
  ggtitle('Total Victims',subtitle = 'Wounded and Fatal by Incident') +
  theme_bw() +
  theme(text=element_text(family="Noe")) +
  facet_grid(type ~ .)

# distribution of interarrival time
window <- 5
combined_trends <- data.frame()
for(i in 1975:2017){
  shooting_temp <- shooting_data %>% 
                      filter(Year>=(i-window) & Year<=i) %>% 
                      arrange(Date)
    
  terror_temp <- terror_data %>% 
                    filter(iyear>=(i-window) & iyear<=i) %>% 
                    arrange(Date)
  
  #calculate interarrival times for events
  shooting_interarrival <- diff(shooting_temp$Date) %>% as.numeric() %>% mean(na.rm=TRUE)
  terror_interarrival <- diff(terror_temp$Date) %>% as.numeric() %>% mean(na.rm=TRUE)
  
  shooting_vics <- mean(shooting_temp$`Total victims`, na.rm=TRUE)
  terror_vics <- mean(terror_temp$TotalVictims, na.rm=TRUE)
  
  combined_trends <- combined_trends %>% rbind(data.frame(
                                                Average_Interarrival = c(shooting_interarrival,terror_interarrival),
                                                Average_Total_Victims = c(shooting_vics,terror_vics),
                                                Incident_Type = c('Mass Shooting','Terrorism'),
                                                year = c(i,i)
                                                ))
}

colnames(combined_trends) <- c('Average_Interarrival','Average Total Victims', 'Incident Type', 'year')

ggplot(data=combined_trends,aes(x=year,y=Average_Interarrival, color=`Incident Type`)) +
  geom_point(aes(size=`Average Total Victims`)) +
  geom_line()+
  scale_color_manual(values=c("#800026", "#fd8d3c")) +
  xlab('') +
  ylab('Days') +
  ggtitle('Time Between Incidents',subtitle = '5-Year Rolling Average') +
  theme_bw() +
  theme(text=element_text(family="Noe"))

# now lets look at interarrival rates by magnitude of total victims
step <- 1 
rate_data <- data.frame()
for(i in c(seq(1,max(terror_data$nkill, na.rm=TRUE),step),max(terror_data$nkill, na.rm=TRUE))){
  
  shooting_temp <- shooting_data %>% filter(Fatalities < (i+step) & Fatalities >= (i)) %>% 
                      arrange(Date)
  terror_temp <- terror_data %>%  filter(nkill < i & nkill >= (i-step)) %>% 
                      arrange(Date)
  
  #calculate interarrival times for events
  shooting_interarrival <- diff(shooting_temp$Date) %>% as.numeric() %>% mean(na.rm=TRUE) %>% abs()
  terror_interarrival <- diff(terror_temp$Date) %>% as.numeric() %>% mean(na.rm=TRUE) %>% abs()
  mid_point <- (i+(step/2))
  
  if(nrow(shooting_temp)==1){
    shooting_interarrival <- ((as.numeric(shooting_temp$Year[1]) - 1970) * 365) %>% abs()
    mid_point <- i
  }
  
  if(nrow(terror_temp)==1){
    terror_interarrival <- ((terror_temp$iyear[1] - 1970) * 365) %>% abs()
    mid_point <- i
  }
  
  
  
  rate_data <- rate_data %>% rbind(data.frame(
                                        interarrival = c(shooting_interarrival, terror_interarrival),
                                        mid_point = c(mid_point,mid_point),
                                        sample_size = c(nrow(shooting_temp),nrow(terror_temp)),
                                        type = c('Mass Shooting','Terrorism')))

  }

rate_data <- rate_data %>% 
              filter(interarrival != 0) 

colnames(rate_data) <- c('interarrival','mid_point','Sample Size','type')

ggplot(data=rate_data,aes(x = mid_point,y = interarrival, color=type)) +
  geom_point() +
  geom_smooth(method = "lm", color='black',fullrange=TRUE,se=TRUE) +
  scale_x_log10() +
  scale_y_continuous(#limits=c(0,(365*55)),
                     breaks = c(1,3650,(365*30),(365*50)), 
                     labels = c('1 Day','10 Years','30 Years','50 Years')) +
  scale_color_manual(values=c("#800026", "#fd8d3c"), guide = FALSE) +
  xlab('Fatality Magnitude') +
  ylab('Historical Average Time Between Incidents') +
  ggtitle('Magnitude of Fatalities vs. Time Between Incidents',subtitle = 'Fatalities - Log Scale') +
  theme_bw() +
  theme(text=element_text(family="Noe")) +
  facet_grid(. ~ type,scales='free_x',space='free_x')

rate_data_terror <- rate_data %>% filter(type == 'Terrorism')

my_lm_model <- lm(data = rate_data_terror, interarrival ~ mid_point)

summary(my_lm_model)
