library(tidyverse)
library(here)
library(ggridges)
library(viridis)
library(wql) #trend analysis

# 1. Read data ------------------------------------------------------------

smoke <- read_csv(here('data/monthly_smoke_area.csv'))

summary(smoke)

ifelse(!dir.exists("results"), dir.create("results"), "Folder exists already")

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

ggsave(here('results/ridgeline_plt.png'), dpi = 300, units = 'in', width = 14, height = 8)
  
  
# 5. Seeasonal Boxplot ----------------------------------------------------

# Add seasons

smoke_ts2 <- smoke_ts %>% 
  mutate(
    season = as.factor(
      if_else(month>=3 & month<=5, 'Spring', 
             if_else(month>=6 & month<=8, 'Summer',
                    if_else(month>=9 & month<=11, 'Fall', 'Winter')))
      )
  )

# Seasonal boxplot shows area covered by season, colored by year (to see standouts?)

ggplot(data = smoke_ts2, aes(x = season, y = smoke_all_area_km2))+
  geom_boxplot()+
  geom_jitter(aes(color=year), size=2, alpha=0.9)+
  scale_color_viridis()+
  xlab('Season')+
  ylab(bquote('Smoke Cover'~(km^2)))+
  theme_classic()+
  theme(
    text = element_text(size = 25),
    legend.title = element_blank()
  )

ggsave(here('results/box_plt_by_season.png'), dpi = 300, units = 'in', width = 14, height = 8)

# 6. Time series plot  ----------------------------------------------------

#Adjust the data for the time series

smoke_ts3 = smoke_ts2 %>% 
  group_by(year, month) %>% 
  summarise(mean_smoke_km2 = mean(smoke_all_area_km2),
            median_smoke_km2 = median(smoke_all_area_km2)) %>% 
  unite(month_year, c(month, year), sep = "-") %>%
  mutate(date = parse_date_time(month_year, "my")) %>% 
  select(-month_year) %>% 
  select(3,1,2)

ggplot(data = smoke_ts3, aes(x = date, y = mean_smoke_km2))+
  geom_point()+
  geom_line()

smoke_ts4 = smoke_ts2 %>% 
  group_by(year) %>% 
  summarise(
    mean_smoke_km2 = mean(smoke_all_area_km2),
    median_smoke_km2 = median(smoke_all_area_km2)
  )

ggplot(data = smoke_ts4, aes(x = year, y = mean_smoke_km2))+
  geom_smooth(method = lm, color = 'grey50', linetype = 'dashed')+
  geom_point(size = 2.5)+
  theme_classic()+
  theme(
    text = element_text(size = 25)
  )+
  xlab('Year')+
  ylab(bquote('Smoke Cover'~(km^2)))

ggsave(here('results/smoke_ts_yearly.png'), dpi = 300, units = 'in', width = 14, height = 8)

# Looks interesting. What does a Mann-Kendall test tell us and what is the Sen's slope?

smoke_mk <- smoke_ts4 %>% 
  summarize(smoke_sen = mannKen(mean_smoke_km2)$sen.slope,
            smoke_pval = mannKen(mean_smoke_km2)$p.value,
            smoke_med_sen = mannKen(median_smoke_km2)$sen.slope,
            smoke_med_pval = mannKen(median_smoke_km2)$p.value)


#mean smoke cover
#p value < 0.01
#slope 11,189 km^2 per year

#median smoke cover
#p value < 0.05
#slope 8,203




# 7. More time series analysis --------------------------------------------



# Could you potentially test for trends in the following variables (I attached the datasets Minmeng generated for me last week):
#   -annual smoke area in CA (all densities) (what you already showed in the meeting)
# -annual smoke area in CA (med-high density smoke only; there's now a new column in the csv file with this number)
# -annual number of smoke days (med-high density) for each of our study sites (the spreadsheet contains more sites than we actually used, so there's no need to compute trends for them all).
# You could just dump the results in our outline for now. Let me know if you have questions or want to meet separately about this.




