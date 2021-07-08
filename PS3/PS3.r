# Problem Set 3
# Christian Hilscher
# 08.07.2021

library(dplyr)
library(nycflights13)
library(ggplot2)

## Problem 1
flights <- nycflights13::flights
flights_short <- flights %>% slice(1:5000)

# Selecting variables gives dataframe, pulling a vector
flights_short %>% select(sched_dep_time)
flights_short %>% select(sched_dep_time, sched_arr_time)
flights_short %>% select(tailnum, sched_dep_time, sched_arr_time)

# Filtering
flights_short %>% filter(carrier == "AA")
flights_short %>% filter(carrier %in% c("AA", "DL"))

flights_short %>% filter(year==2013 & month==1 & day==5)

flights_short %>% filter(dep_delay > 0 | arr_delay > 0)

# Chained operations
flights_new <- flights_short %>% mutate(speed = distance/air_time) %>% select(tailnum, distance, air_time, speed)
flights_new %>% arrange(speed, tailnum)
flights_new %>% arrange(desc(speed), desc(tailnum))

# Mutate keeps original dataframe intact
# Summarise only reports new columns
flights_short %>% 
  group_by(carrier) %>% 
  summarise(mean_arr_delay = mean(arr_delay, na.rm=TRUE), 
            mean_dep_delay = mean(dep_delay, na.rm=TRUE),
            perc_arr_delay = mean(arr_delay>0, na.rm=TRUE),
            perc_dep_delay = mean(dep_delay>0, na.rm=TRUE))

flights_short %>%
  group_by(tailnum) %>%
  summarise(mean_air_time = mean(air_time, na.rm=TRUE),
            variace_air_time = var(air_time, na.rm=TRUE))

## Problem 2

# Select data set
plot_df <- flights_short %>% 
  group_by(tailnum) %>% 
  filter(carrier %in% c("DL", "AA", "UA", "B6")) %>%
  summarise(mean_dep_delay = mean(dep_delay, na.rm=TRUE),
            mean_arr_delay = mean(arr_delay, na.rm=TRUE),
            carrier = first(carrier))

# Raw plot
p = ggplot(data=plot_df, aes(x=mean_dep_delay))

# Simple plot 
p + geom_point(aes(y=mean_arr_delay))

# Colors by carrier
p + geom_point(aes(y=mean_arr_delay, col=carrier))

# Facet for each carrier
p + geom_point(aes(y=mean_arr_delay)) +
  facet_grid(rows=vars(carrier))

# Histogram
ggplot(data=plot_df) + 
  geom_histogram(aes(mean_arr_delay))

# Histogram with different bin size
ggplot(data=plot_df) + 
  geom_histogram(aes(mean_arr_delay), bins=25)

# Histogram with color
ggplot(data=plot_df) + 
  geom_histogram(aes(mean_arr_delay), fill="red")

# Histogram with added density
ggplot(data=plot_df) + 
  geom_histogram(aes(mean_arr_delay, y=..density..)) +
  geom_density(aes(mean_arr_delay), color="blue")
