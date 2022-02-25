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

pop_over_time_plot <- ggplot(data = total_pop_per_year) + geom_line(mapping = aes(x = year, y = total_jail_pop, color=total_jail_pop)) +
  labs(title = "Total Jail Population Per Year", x = "Year", y = "Total Population")
