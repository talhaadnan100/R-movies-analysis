library(tidyverse)
library(scales)
library(lubridate)
library(plotly)
library(gridExtra)
library(gganimate)
theme_set(theme_light())

movie_top_days <- movie_profit %>% 
  group_by(md, month_day) %>% 
  tally(sort = TRUE) %>% 
  head(10)

movie_profit %>% 
  filter(month_day %in% movie_top_days$month_day) %>% 
  select(md, month_day) %>% 
  ggplot(aes(fct_reorder(month_day, md), fill = month_day)) + 
  geom_bar(stat = "count") + theme(legend.position = "none") + 
  geom_label(stat='count',aes(label=..count..), size=5) +
  labs(
    title = "Top 10 Most Popular Release Dates",
    x = "",
    y = "Number of Movies"
  )

ggsave("img/10-most-popular-release-dates.png")

movie_profit %>% 
  group_by(distributor_top7) %>% 
  summarise(budget_per_movie = mean(production_budget)) %>%
  ggplot(aes(fct_reorder(distributor_top7, budget_per_movie), budget_per_movie, fill = distributor_top7)) +
  geom_col() + theme(legend.position = "none") + 
  scale_y_continuous(labels = dollar_format()) +
  coord_flip() + 
  labs(
    title = "Average Budget Per Movie",
    x = "Distributor",
    y = "Budget per Movie"
  )

ggsave("img/average-budget-per-movie.png")

movie_profit %>%
  mutate(profit = worldwide_gross - production_budget) %>% 
  arrange(desc(profit)) %>%
  head(15) %>% 
  ggplot(aes(y=profit, x=fct_reorder(movie, profit), fill = mpaa_rating)) + 
  geom_col() + 
  scale_y_continuous(labels = dollar_format()) +
  coord_flip() + 
  labs(
    title = "Hollywood's Best Pay Offs",
    x = "",
    y = "Net Profit"
  ) + theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave("img/hollywoods-best-pay-offs.png")

movie_profit %>% 
  ggplot(aes(production_budget, worldwide_gross)) + 
  geom_point(alpha = 0.2) + 
  scale_x_continuous(labels = dollar_format()) + scale_y_continuous(labels = dollar_format()) +
  geom_smooth(method = "lm", se = FALSE, colour = "red") + 
  labs(
    title = "Movie Return on Investment",
    x = "Production Budget (USD)",
    y = "Worldwide Gross (USD)"
  ) 

ggsave("img/movie-roi.png")

distributor_top5_revenue_budget_50years <- movie_profit %>% 
  filter(year > 1968,
         distributor_top7 != "Other" & distributor_top7 != "Lionsgate") %>% 
  group_by(distributor_top7, year) %>% 
  summarise(
    production_budget = sum(production_budget),
    worldwide_gross = sum(worldwide_gross),
    movies = n()
  ) %>% 
  gather(key = RevenueBudget, value = Amount, -distributor_top7, -year, -movies)

p <- distributor_top5_revenue_budget_50years %>% 
  ggplot(aes(x = distributor_top7, y = Amount, fill = RevenueBudget)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  scale_y_continuous(labels = dollar_format()) + coord_flip() + 
  theme(legend.position = "none") +
  # Add time transition to above plot
  labs(title = '50 Years of The Five Biggest Movie Distributors',
       subtitle = 'Worldwide Gross (Teal) & Production Budgets (Pink) Year: {frame_time}', x = "",
       y = 'U.S. Dollars') +
  transition_time(as.integer(year)) +
  ease_aes('linear')

animate(p, fps=5)

anim_save("img/distributor-top5-revenue-budget-50years.gif")