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

path <- here::here('data', 'raw_original', 'WAQ.xlsx')


##### tasks #######

# make sure the core columns are named the same and have the same class in every sheet
####### dates, done (all Date class)
####### pin_height, done
####### front_back, done

# figure out which columns have codes and notes, done
####### 2013: none; using x_2 because it's column P like all the others (all blank but I want the column header)
####### 2014: column P; unlabeled; 2nd unlabeled column (row 29 of data has note 'hole': this is x_2)
####### 2015: column P; labeled 'Notes'
####### 2016: column P; labeled 'Notes'
####### 2017: column P; labeled 'Notes'; contains name of reader
####### 2018: column S; labeled 'Notes'; columns P,Q,R are new: time readings started, reader, recorder, respectively.


# make non-core columns consistent (reader, recorder, reading time, etc.)
# bind all together


# read tabular data

# every sheet (named by years) will be an element of a list
# set it up
years <- 2013:2018
dat <- list()


# read in the sheets
for(i in seq_along(years)){
    dat[[i]] <- read_excel(path, sheet = as.character(years[i])) %>%
        clean_names()
}

# name the elements of the list
names(dat) <- paste0('dat', years)



# split the list up into separate data frames
#  list2env(dat, envir = .GlobalEnv)  # this actually isn't a great idea


###############################
## Clean up 2013
###############################

## fix the dates in the 2013 data frame
dat$dat2013$date <- gsub("1020", NA, dat$dat2013$date)  # replace that comment about 1020 not being read with NA

dat$dat2013 <- dat$dat2013 %>%
    mutate(date = excel_numeric_to_date(as.numeric(dat$dat2013$date)),
           pin_height = as.numeric(x2013_measured_pin_height_cm),
           notes = x_2) %>%
    select(set_id = set_code, date, arm_position = position, pin_number, pin_height, front_back = f_b, notes)



###############################
## Clean up 2014  
###############################

# same thing here; typed NAs
dat$dat2014 <- dat$dat2014 %>%
    mutate(date = as.Date(date),
           pin_height = as.numeric(x2014_measured_pin_height_cm),
           front_back = f_b,
           notes = x_2) %>%
    select(set_id = set_code, date, arm_position = position, pin_number, pin_height, front_back = f_b, notes)


###############################
## Clean up 2015 
###############################

dat$dat2015 <- dat$dat2015 %>%
    mutate(date = as.Date(date),
           pin_height = as.numeric(x2015_measured_pin_height_cm),
           front_back = f_b) %>%
    select(set_id = set_code, date, arm_position = position, pin_number, pin_height, front_back = f_b, notes)


###############################
## Clean up 2016 
###############################

dat$dat2016 <- dat$dat2016 %>%
    mutate(date = as.Date(date),
           pin_height = as.numeric(x2016_measured_pin_height_cm),
           front_back = f_b) %>%
    select(set_id = set_code, date, arm_position = position, pin_number, pin_height, front_back = f_b, notes)


###############################
## Clean up 2017
###############################
# this one will be tougher because of the cross-comparisons between readers
# for now only focusing on the main data table

dat$dat2017 <- dat$dat2017 %>%
    mutate(date = as.Date(date),
           pin_height = as.numeric(pin_length),
           front_back = case_when(f_b_front_back == 'F' ~ 'Front',
                                  f_b_front_back == 'B' ~ 'Back',
                                  TRUE ~ f_b_front_back)) %>%
    select(set_id = set_code, date, arm_position = position, pin_number, pin_height, front_back, notes)


###############################
## Clean up 2018
###############################

dat$dat2018 <- dat$dat2018 %>%
    mutate(date = as.Date(date),
           pin_height = as.numeric(pin_length_cm),
           front_back = case_when(f_b_front_back == 'F' ~ 'Front',
                                  f_b_front_back == 'B' ~ 'Back',
                                  TRUE ~ f_b_front_back)) %>%
    select(set_id = set_code, date, arm_position = position, pin_number, pin_height, front_back, notes)

###############################
###############################

# join together all the data frames in the list 'dat'
# this does NOT deal with column class differences
dat_all <- reshape::merge_recurse(dat) %>%
    mutate(sum_na = is.na(set_id) + is.na(date) + is.na(pin_height),
           reserve = 'WQB',
           date = ymd(date)) %>%
    rename(pin_height_cm = pin_height) %>%
    filter(sum_na <3) %>%
    select(reserve, everything(), -sum_na)

write_csv(dat_all, here('data', 'intermediate', 'WQB.csv'))

###
