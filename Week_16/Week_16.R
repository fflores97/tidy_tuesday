
# Load packages -------------------------------------------------------------------------------

library(tidyverse)
library(readxl)

# Import data ---------------------------------------------------------------------------------

data <- readxl::read_xlsx("week16_exercise.xlsx") %>%
  # slice(-1) %>% 
  select(-count) %>% 
  type_convert()

# Tidy data -----------------------------------------------------------------------------------

data_tidy <- data %>% 
  select(state, men_working, men_nonworking, women_working, women_nonworking) %>% 
  gather(key = "class", value = "value", -state) %>% 
  na.omit() %>% 
  separate(col = class, into = c("Gender","Working")) 


# Plot 1: Differences in gender, working ------------------------------------------------------

data_tidy %>% 
  filter(Working == "working") %>% 
  arrange(desc(Gender)) %>% 
  ggplot(aes(x = value, y = reorder(state, value)))+
  geom_line(aes(group = state))+
  geom_point(aes(color = Gender), size = 4)+
  scale_color_hue(direction = -1, h.start = 150)+ # For some reason men were shown pink, kinda confusing
  theme_minimal()+
  labs(title = "Exercise differences between genders (Working)",
       # subtitle = "",
       x = "Percentage of adults who meet federal guidelines",
       y ="")
ggsave(filename = "plot1.png", width = 7, height = 7)

# Plot 2: Differences in gender, non-working --------------------------------------------------

data_tidy %>% 
  filter(Working == "nonworking") %>% 
  arrange(desc(Gender)) %>% 
  ggplot(aes(x = value, y = reorder(state, value)))+
  geom_line(aes(group = state))+
  geom_point(aes(color = Gender), size = 4)+
  scale_color_hue(direction = -1, h.start = 150)+ # For some reason men were shown pink, kinda confusing
  theme_minimal()+
  labs(title = "Exercise differences between genders (Working)",
       # subtitle = "",
       x = "Percentage of adults who meet federal guidelines",
       y ="")
ggsave(filename = "plot2.png", width = 7, height = 7)

# Plot 3: Differences in work status, men -----------------------------------------------------

data_tidy %>% 
  filter(Gender == "men") %>% 
  arrange(desc(Gender)) %>% 
  ggplot(aes(x = value, y = reorder(state, value)))+
  geom_line(aes(group = state))+
  geom_point(aes(color = Working), size = 4)+
  scale_color_hue(direction = 1, h.start = 150)+ # For some reason men were shown pink, kinda confusing
  theme_minimal()+
  labs(title = "Exercise differences between working status (Men)",
       # subtitle = "",
       x = "Percentage of adults who meet federal guidelines",
       y ="")
ggsave(filename = "plot3.png", width = 7, height = 7)

# Plot 4: Differences in work status, women ---------------------------------------------------

data_tidy %>% 
  filter(Gender == "women") %>% 
  arrange(desc(Gender)) %>% 
  ggplot(aes(x = value, y = reorder(state, value)))+
  geom_line(aes(group = state))+
  geom_point(aes(color = Working), size = 4)+
  scale_color_hue(direction = 1, h.start = 150)+ # For some reason men were shown pink, kinda confusing
  theme_minimal()+
  labs(title = "Exercise differences between working status (Men)",
       # subtitle = "",
       x = "Percentage of adults who meet federal guidelines",
       y ="")
ggsave(filename = "plot4.png", width = 7, height = 7)


data_tidy %>% 
  # spread(key = "Women", value = "value", Working, value)
  ggplot(aes(x = value, y = reorder(state, value)))+
  # geom_line(aes(group = interaction(state, Gender)))+
  geom_line(aes(group = interaction(state, Gender)))+
  # geom_line(aes(group = state))+
  geom_point(aes(color = Gender, shape = Working), size = 4)+
  theme_minimal()+
  labs(title = "Exercise differences between working status (Men)",
       # subtitle = "",
       x = "Percentage of adults who meet federal guidelines",
       y ="")
ggsave("plot5.png", width=7,height=7)


# For map plotting ----------------------------------------------------------------------------

# From https://github.com/jasonong/List-of-US-States/blob/master/states.csv
state_symbols <- read_csv("states.csv") %>% 
  rename(state = State) %>% 
  rename(state_short = Abbreviation)

data <- data %>% 
  inner_join(state_symbols) %>% 
  mutate(diff_working = men_working - women_working) %>% 
  mutate(diff_nonworking = men_working - women_working)
usmap::plot_usmap(data = data, regions = "state", values = "diff_working") +
  scale_fill_gradientn(values=c(-2,0,16), colours = c("red", "white", "blue"))
