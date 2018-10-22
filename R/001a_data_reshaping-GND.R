# generally already in wide format because of how we've been working on it. 
# adding flag columns for pins, and spitting out csv files for each SET id.

library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)


#############################
# Deal with main data frame
#############################

path <- here('data', 'raw_original', 'GND.csv')
dat <- read_csv(path) 
dat <- dat %>%
    mutate(reserve = 'GND',
           set_id = SET,
           arm_position = arm,
           f_pin_1 = 0,
           f_pin_2 = 0,
           f_pin_3 = 0,
           f_pin_4 = 0,
           f_pin_5 = 0,
           f_pin_6 = 0,
           f_pin_7 = 0,
           f_pin_8 = 0,
           f_pin_9 = 0) %>%
    select(reserve, set_id, arm_position, everything(), -SET, -arm) %>%
    arrange(set_id, date, arm_position)

unique_sets <- unique(dat$set_id)

for(i in seq_along(unique_sets)) {
    dat_sub <- dat[dat$set_id == unique_sets[i], ]
    filename <-  paste0(tolower(unique_sets[i]), '.csv')
    filepath <- here('data', 'split_by_site', filename)
    write_csv(dat_sub, filepath)
}
