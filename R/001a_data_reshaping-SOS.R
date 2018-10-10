library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)


#############################
# Deal with main data frame
#############################

# I modified a column header in the spreadsheet (Pin7 Code --> Pin 7 Code) because a space was missing and it screwed up the code. Unless otherwise noted, I have not modified the spreadsheet.

path <- 'data/raw_corrected/SOS_mod.xlsx'

# read tabular data
dat <- read_excel(path, sheet = 'RSET data') %>%
    clean_names() %>%
    filter(is.na(set_id) == FALSE)

dat_long <- dat %>%
    gather(key = pin, value = value,
           -reserve, -set_id, -position, -date, -comments) %>%
    separate(pin, into = c('pin', 'pin_number', 'unit'), sep = '_') %>%
    spread(key = unit, value = value) %>%
    mutate(pin_height = mm,
           mm = NULL) %>%
    select(reserve, set_id, date, arm_position = position, pin_number, pin_height, code, comments)

write_csv(dat_long, 'data/intermediate/SOS.csv')

