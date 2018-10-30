library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)


#############################
# Figure out what's in the file
#############################

path <- here('data', 'raw_original', 'PAD.xlsx')

#########################
# read_excel plus other strategies

sheetnames <- tolower(excel_sheets(path))
sheetnames <- gsub(' ', '_', sheetnames)

# manually figuring out the last column of the main data table in each sheet:
lastcols <- c('M', 'F', 'P', 'M', 'P', 
              'M', 'P', 'P', 'P', 'M', 
              'P', 'P', 'Q', 'P', 'P',
              'L', 'L', 'L', 'L', 'H',
              'J', 'J', 'K', 'F')
# make sure that's the same length as the sheet names
length(sheetnames) == length(lastcols)

# generate data ranges for reading in sheets
ranges <- paste0('A1:', lastcols, '37')

#############################################
# read in all the sheets
#############################################

# set up the list
dat <- list()

# read the sheets in
for(i in seq_along(sheetnames)){
    dat_in <- read_excel(path, sheet = i, range = ranges[i])
    dat_tidied <- dat_in %>%
        rename(arm_position = 'arm position',
               pin_number = pin) %>%
        gather(key = date, value = pin_height, -arm_position, -pin_number) %>%
        mutate(date = excel_numeric_to_date(as.numeric(date)),
               reserve = 'PAD',
               set_id = sheetnames[i]) %>%
        select(reserve, set_id, date, arm_position, pin_number, pin_height) %>%
        arrange(set_id, date, arm_position, pin_number)
    dat[[i]] <- dat_tidied
}


############################################
# glue everything together into one big list
############################################

dat_all <- reshape::merge_recurse(dat)

# spit it back out
path_out <- here('data', 'intermediate', 'PAD.csv')
write_csv(dat_all, path_out)

