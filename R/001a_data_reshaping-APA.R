library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(here)
library(janitor)
library(readr)


path <- here::here('data', 'raw_original', 'APA.xlsx')

dat <- read_excel(path) %>%
    clean_names()

dat_long <- dat %>%
    gather(key = arm_pin, value = pin_height_mm, -set_date, -marsh_site, -set_name, -set_measurer, -notes) %>%
    separate(arm_pin, into = c('arm_position', 'pin_number'), sep = '_') %>%
    mutate(reserve = 'APA') %>%
    select(reserve, marsh_site, set_id = set_name, date = set_date, arm_position, pin_number, pin_height_mm, set_measurer, notes) %>%
    arrange(marsh_site, set_id, date, arm_position, pin_number)

outpath <- here::here('data', 'intermediate', 'APA.csv')
write_csv(dat_long, outpath)
