library(tidyverse)
library(lubridate)
library(here)


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
    spread(key = date, value = cumulative) %>%
    ungroup()

#### Avg change since first reading by arm
#### in spreadsheets, this is average of 9 pin values in an arm for each date
#### so that's how it is here too  

change_cumulative_arm <- change_cumulative_pin %>%
    group_by(reserve, set_id, arm_position) %>%
    select(-pin_number) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ungroup()

#### Avg change since first reading by SET
#### this is the average of the 4 arm positions

change_cumulative_set <- change_cumulative_arm %>%
    group_by(reserve, set_id) %>%
    select(-arm_position) %>%
    summarize_all(mean, sd) %>%
    ungroup()
    