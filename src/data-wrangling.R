library(tidyverse)
library(lubridate)

movie_profit_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-23/movie_profit.csv")

movie_profit <- movie_profit_raw %>% 
  mutate(
    release_date = mdy(release_date),
    profit_ratio = worldwide_gross / production_budget,
    decade = 10 * floor(year(release_date) / 10),
    distributor_top7 = fct_lump(distributor, 7),
    md = yday(release_date),
    month_day = paste(month(release_date, label = TRUE), day(release_date),sep = "-")
  ) %>% 
  select(-X1) %>% 
  filter(
    release_date < "2018-10-13",
    worldwide_gross > 0
  )

movie_profit$distributor_top7[is.na(movie_profit$distributor_top7)] <- "Other"
movie_profit$decade <- as.integer(movie_profit$decade)

write.csv(movie_profit, file = "data/movie_profit.csv")