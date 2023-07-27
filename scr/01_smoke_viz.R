library(tidyverse)
library(here)
library(ggridges)


# 1. Read data ------------------------------------------------------------

smoke <- read_csv(here('data/monthly_smoke_area.csv'))

summary(smoke)


# 2. Clean data -----------------------------------------------------------

smoke_ts <- smoke %>% 
  mutate(date = with(., sprintf("%d-%02d", year, month)),
         smoke_all_area_km2 = round(smoke_all_area_m2/1000000, digits = 0))


# 3. Initial plot ---------------------------------------------------------

ggplot()+
  geom_point(data = smoke_ts, aes(x = date, y = smoke_all_area_percent))+
  theme_classic()+
  ylab('Smoke Area (%)')+
  xlab('Date')


# 4. Ridgeline Plot ----------------------------------------------------------

#Ridgeline plot shows area of smoke cover by year
ggplot(smoke_ts, aes(x = smoke_all_area_km2, y = as.factor(year), group = year, height = ..density..)) +
  geom_density_ridges(
    stat = 'binline',
    binwidth = 50000,
    draw_baseline = F,
    rel_min_height = 0,
    scale = 0.9
  )+
  theme_ridges(grid = T)+
  ylab('Year')+
  xlab( bquote('Smoke Cover'~(km^2)))+
  theme(
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )
  
  
# 5. Seeasonal Boxplot ----------------------------------------------------

# Add seasons

smoke_ts2 <- smoke_ts %>% 
  mutate(
    season = as.factor(if_else(month>=3 & month<=5, 'spring', 
                     if_else(month>=6 & month<=8, 'summer',
                             if_else(month>=9 & month<=11, 'fall', 'winter'))))
  )

# Seasonal boxplot shows area covered by season, colored by year (to see standouts?)




