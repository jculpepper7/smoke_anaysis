library(tidyverse)
library(here)
library(ggridges)

smoke <- read_csv(here('data/monthly_smoke_area.csv'))


summary(smoke)

smoke_ts <- smoke %>% 
  mutate(date = with(., sprintf("%d-%02d", year, month)))

ggplot()+
  geom_point(data = smoke_ts, aes(x = date, y = smoke_all_area_percent))+
  theme_classic()+
  ylab('Smoke Area (%)')+
  xlab('Date')

#Seasons
ggplot(smoke_ts, aes(x = smoke_all_area_percent, y = as.factor(year))) +
  #geom_density_ridges(rel_min_height = 0.005) +
  geom_density_ridges() +
  #scale_y_discrete(expand = c(0.01, 0)) +
  #scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges()


ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_density_ridges(rel_min_height = 0.005) +
  #scale_y_discrete(expand = c(0.01, 0)) +
  #scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges()
