library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

data <- read.csv('https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')

my_data <- data %>% select("year", "black_jail_pop", "latinx_jail_pop", "white_jail_pop", "native_jail_pop", "total_jail_pop")
total_pop_per_year <- my_data %>% group_by(year) %>% summarize(total_jail_pop = sum(total_jail_pop, na.rm=T))
total_black_pop_per_year <- my_data %>% group_by(year) %>% summarize(black_jail_pop = sum(black_jail_pop, na.rm=T))
total_latinx_pop_per_year <- my_data %>% group_by(year) %>% summarize(latinx_jail_pop = sum(latinx_jail_pop, na.rm=T))
total_white_pop_per_year <- my_data %>% group_by(year) %>% summarize(white_jail_pop = sum(white_jail_pop, na.rm=T))
total_native_pop_per_year <- my_data %>% group_by(year) %>% summarize(native_jail_pop = sum(native_jail_pop, na.rm=T))

full_totals <- full_join(total_black_pop_per_year, total_latinx_pop_per_year)
full_totals <- full_join(full_totals, total_white_pop_per_year)
full_totals <- full_join(full_totals, total_pop_per_year)
full_totals <- full_join(full_totals, total_native_pop_per_year)

pop_over_time_plot <- ggplot(data = full_totals) + 
  geom_line(mapping = aes(x = year, y = total_jail_pop), color="Red") +
  geom_line(mapping = aes(x = year, y = black_jail_pop), color="Blue") +
  geom_line(mapping = aes(x = year, y = latinx_jail_pop), color="Yellow") +
  geom_line(mapping = aes(x = year, y = native_jail_pop), color="Green") +
  geom_line(mapping = aes(x = year, y = white_jail_pop)) +
  labs(title = "Jail Population Per Year", x = "Year", y = "Population")
pop_over_time_plot


#Black vs White Pop Bar Chart
total_white <- full_totals %>% pull(sum(white_jail_pop))
total_black <- full_totals %>% pull(sum(black_jail_pop))

black_v_white_pop_plot <- ggplot(data = total_black_white) + 
  geom_bar(mapping = aes(x = "total_white"), color="Red") +
  geom_bar(mapping = aes(x = "total_black"), color="Blue") +
  labs(title = "Comparison of Black vs White Jail Population", x = 'Race', y = "Population")
black_v_white_pop_plot

#map data of black pop

state_data <- data %>% select("year", "state", "total_jail_pop", "black_jail_pop")
state_data[is.na(state_data)] <- 0

state_data <- state_data %>% group_by(state) %>% summarize(percent_black = (sum(black_jail_pop) / sum(total_jail_pop)) * 100)

state_shape <- map_data("state")
state_abbrevs <- data.frame(state.abb, state.name)

state_data <- left_join(state_data, state_abbrevs, by=c('state' = 'state.abb'))

state_data <- state_data %>% mutate(region = tolower(state.name))

state_shape <- left_join(state_shape, state_data)

state_plot <- ggplot(state_shape) + geom_polygon(mapping = aes(x=long, y=lat, group = group, fill = percent_black_white)) + scale_fill_continuous(low = 'black', high = 'red') + coord_map() + 
  labs(title = "Percentage of black per state")

state_plot
