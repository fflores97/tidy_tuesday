
# Import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(usmap)

# Import data -------------------------------------------------------------

# From week 15
beer <- read_xlsx("week15_beers.xlsx")
brewery <- read_xlsx("week15_beers.xlsx", sheet = "breweries") %>% 
  rename(brewery_id = id)

# From week 5
census <- read_csv("week5_acs2015_county_data.csv")

# From https://github.com/jasonong/List-of-US-States/blob/master/states.csv
state_symbols <- read_csv("states.csv") %>% 
  rename(State_name = State) %>% 
  rename(state = Abbreviation)

# Preprocessing -----------------------------------------------------------

population <- census %>% 
  select(State, TotalPop) %>% 
  group_by(State) %>% 
  summarize(sum(TotalPop)) %>% 
  rename(Population = `sum(TotalPop)`) %>% 
  rename(State_name = State) %>% 
  mutate(Population = Population/100000) 

population <- population %>% 
  right_join(state_symbols)

state_df <- brewery %>% 
  group_by(state) %>% 
  nest(.key = "brewery_data") %>%
  right_join(population)

beerCounter <- function(brewery_data) {
 ids <- brewery_data %>% 
   pull(brewery_id)
 state_count <- beer %>% 
   filter(brewery_id %in% ids) %>% 
   count()
 return(as.numeric(state_count))
}

averageABV <- function(brewery_data){
  ids <- brewery_data %>% 
    pull(brewery_id)
  averageABV <- beer %>% 
    na.omit() %>% 
    filter(brewery_id %in% ids) %>% 
    summarise(mean(abv))
  return(as.numeric(averageABV))
}

state_beers <- function(brewery_data){
  ids <- brewery_data %>% 
    pull(brewery_id)
  state_beers <- beer %>% 
    na.omit() %>% 
    filter(brewery_id %in% ids)
  return(state_beers)
}

## Testing
state_df %>% 
  filter(state == "CA") %>% 
  select(brewery_data) %>%
  unnest() %>% 
  beerCounter()

beers_per_state <- state_df %>% 
  mutate(beers = map_dbl(.x = brewery_data,.f = beerCounter)) %>% 
  mutate(average_ABV = map_dbl(.x = brewery_data, .f = averageABV)) %>% 
  mutate(state_beers = map(brewery_data, state_beers))
  select(state, beers)

usmap::plot_usmap(data = beers_per_state, regions = "state", values = "average_ABV")




data %>% 
  group_by(style) %>%
  select(style) %>% 

  count() %>% 
  ungroup() %>% 
  top_n(10) %>% 
  mutate(style = fct_reorder(style, n, .desc = FALSE)) %>% 
  # count() %>% 
  ggplot() + 
  geom_col(aes(x=style, y = n))+
  coord_flip()
