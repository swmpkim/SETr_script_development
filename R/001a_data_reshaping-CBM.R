library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(here)
library(janitor)
library(readr)


path <- here::here('data', 'raw_original', 'CBM.xlsx')

dat <- read_excel(path, sheet = 'Raw SET Data', skip = 25) %>%
    janitor::clean_names() %>%
    rename(set_id = set_name,
           notes = observations,
           pin_number = pin) %>%
    mutate(reserve = 'CBM') %>%
    select(reserve, set_id, date, arm_position, pin_number, pin_height_cm, everything()) %>%
    arrange(set_id, date, arm_position, pin_number)

outpath <- here::here('data', 'intermediate', 'CBM.csv')
write_csv(dat, outpath)
