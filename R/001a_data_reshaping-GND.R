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

unique(dat$set_id)

# change set names

# panne to juro_high
dat$set_id <- gsub('PANNE', 'JURO_High', dat$set_id)

# juro_1,2,3 to juro_low
dat$set_id <- gsub('JURO-1', 'JURO_Low-1', dat$set_id)
dat$set_id <- gsub('JURO-2', 'JURO_Low-2', dat$set_id)
dat$set_id <- gsub('JURO-3', 'JURO_Low-3', dat$set_id)

# juro 4,5,6 to juro_mid
dat$set_id <- gsub('JURO-4', 'JURO_Mid-1', dat$set_id)
dat$set_id <- gsub('JURO-5', 'JURO_Mid-2', dat$set_id)
dat$set_id <- gsub('JURO-6', 'JURO_Mid-3', dat$set_id)

# spal to spalt
dat$set_id <- gsub('SPAL', 'SPALT', dat$set_id)

unique(dat$set_id)

unique_sets <- unique(dat$set_id)

for(i in seq_along(unique_sets)) {
    dat_sub <- dat[dat$set_id == unique_sets[i], ]
    csvname <-  paste0(tolower(unique_sets[i]), '.csv')
    csvpath <- here('data', 'split_by_site', csvname)
    xlname <-  paste0(tolower(unique_sets[i]), '.xlsx')
    xlpath <- here('data', 'split_by_site', xlname)
    # write_csv(dat_sub, filepath)
    wb <- createWorkbook()
    addWorksheet(wb, 'data')
    writeDataTable(wb, 1, dat_sub, startRow = 1, startCol = 1,
              tableStyle = 'TableStyleLight9',
              bandedRows = TRUE,
              bandedCols = TRUE)
    saveWorkbook(wb, xlpath, overwrite = TRUE)
}
