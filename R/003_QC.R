library(tidyverse)
library(lubridate)
library(here)
library(knitr)


# pick a reserve
# (can we do this in shiny?)
path <- here('data', 'intermediate', 'GND.csv')

# read in data
dat_full <- read_csv(path)


# pick a station
# (should be able to do this in shiny)
dat <- dat_full %>%
    filter(set_id == 'SPALT-1')


# generate some of the tables and graphs from the NPS spreadsheet


#### Cumulative change (change since first reading) - by pin

change_cumulative_pin <- dat %>%
    group_by(reserve, set_id, arm_position, pin_number) %>%
    mutate(cumulative = pin_height - pin_height[1]) %>%
    select(-pin_height) %>%
    ungroup()

# view it, with dates along the top and pins as rows
change_cumulative_pin %>%
    spread(key = date, value = cumulative) %>%
    print()

#### Avg change since first reading by arm
#### in spreadsheets, this is average of 9 pin values in an arm for each date
#### so that's how it is here too  

change_cumulative_arm <- change_cumulative_pin %>%
    group_by(reserve, set_id, arm_position, date) %>%
    select(-pin_number) %>%
    summarize(mean_cumulative = mean(cumulative, na.rm = TRUE),
              sd_cumulative = sd(cumulative, na.rm = TRUE),
              se_cumulative = sd(cumulative, na.rm = TRUE)/sqrt(length(!is.na(cumulative)))) %>%
    ungroup()

# view it, with dates along the top and arms as rows
change_cumulative_arm %>%
    gather(key = summary_stat, value = value, mean_cumulative, sd_cumulative, se_cumulative) %>%
    spread(key = date, value = value) %>%
    print()
    

#### Avg change since first reading by SET
#### this is the average of the 4 arm positions

change_cumulative_set <- change_cumulative_arm %>%
    group_by(reserve, set_id, date) %>%
    select(-arm_position, mean_value = mean_cumulative) %>%
    summarize(mean_cumulative = mean(mean_value, na.rm = TRUE),
              sd_cumulative = sd(mean_value, na.rm = TRUE),
              se_cumulative = sd(mean_value, na.rm = TRUE)/sqrt(length(!is.na(mean_cumulative)))) %>%
    ungroup()


# view it, with dates along the top and sets as rows
change_cumulative_set %>%
    gather(key = summary_stat, value = value, mean_cumulative, sd_cumulative, se_cumulative) %>%
    spread(key = date, value = value) %>%
    print()
