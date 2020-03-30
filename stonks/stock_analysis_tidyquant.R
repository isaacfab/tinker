##########################################
# Quick Analysis of Stock Prices
# by Isaac J. Faber
##########################################

library(tidyverse)
library(tidyquant)

spy <- tq_get("SPY", get = "stock.prices", from = " 2020-02-01") %>% 
  mutate(adjusted = adjusted/adjusted[1])
zm_prices  <- tq_get("ZM", get = "stock.prices", from = " 2020-02-01") %>% 
  mutate(adjusted = adjusted/adjusted[1])
ctxs_prices  <- tq_get("CTXS", get = "stock.prices", from = " 2020-02-01") %>% 
  mutate(adjusted = adjusted/adjusted[1])
msft_prices  <- tq_get("MSFT", get = "stock.prices", from = " 2020-02-01") %>% 
  mutate(adjusted = adjusted/adjusted[1])
slack_prices <- tq_get("WORK", get = "stock.prices", from = " 2020-02-01") %>% 
  mutate(adjusted = adjusted/adjusted[1])



prices <- data.frame(date = spy$date,
                     Market = spy$adjusted,
                     Zoom = zm_prices$adjusted,
                     Citrix = ctxs_prices$adjusted,
                     Slack = slack_prices$adjusted) %>% 
  gather(-date, key = "Stock", value = "Price") %>% 
  mutate(Stock = factor(Stock, levels = c('Market','Citrix','Slack','Zoom')))

prices %>%
  ggplot(aes(x = date, y = Price, colour = Stock)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = 'Set1') +
  scale_y_continuous(breaks = c(.785, 1, 1.145, 1.362, 1.730), labels = c('$0.78','$1','$1.15','$1.36','$1.73')) +
  geom_hline(yintercept = c(.785, 1, 1.145, 1.362, 1.730), alpha = .4) +
  scale_x_date(limits = as.Date(c('2020-02-03','2020-03-27')), 
               expand = c(0, 0))  +
    labs(title = "Remote Work Collaboration Companies", 
       subtitle = "$1 Investment on February 3rd 2020 vs Market (S&P500)",
       y = "$1 Investment Performance", 
       x = "") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
