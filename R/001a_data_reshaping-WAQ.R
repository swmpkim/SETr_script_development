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

path <- here('data', 'raw_original', 'WAQ.xlsx')


##### tasks #######
# make sure the core columns are named the same and have the same class in every sheet
####### dates, check (?)
####### pin_height, done
####### front_back, done




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
           front_back = f_b) %>%
    select(-x2013_measured_pin_height_cm, -f_b)



###############################
## Clean up 2014  
###############################

# same thing here; typed NAs
dat$dat2014 <- dat$dat2014 %>%
    mutate(pin_height = as.numeric(x2014_measured_pin_height_cm),
           front_back = f_b) %>%
    select(-x2014_measured_pin_height_cm, -f_b)


###############################
## Clean up 2015 
###############################

dat$dat2015 <- dat$dat2015 %>%
    mutate(pin_height = as.numeric(x2015_measured_pin_height_cm),
           front_back = f_b) %>%
    select(-x2015_measured_pin_height_cm, -f_b)


###############################
## Clean up 2016 
###############################

dat$dat2016 <- dat$dat2016 %>%
    mutate(pin_height = as.numeric(x2016_measured_pin_height_cm),
           front_back = f_b) %>%
    select(-x2016_measured_pin_height_cm, -f_b)


###############################
## Clean up 2017
###############################
# this one will be tougher because of the cross-comparisons between readers
# for now only focusing on the main data table

dat$dat2017 <- dat$dat2017 %>%
    mutate(pin_height = as.numeric(pin_length),
           front_back = case_when(f_b_front_back == 'F' ~ 'Front',
                                  f_b_front_back == 'B' ~ 'Back',
                                  TRUE ~ f_b_front_back)) %>%
    select(-pin_length, -f_b_front_back)


###############################
## Clean up 2018
###############################

dat$dat2018 <- dat$dat2018 %>%
    mutate(pin_height = as.numeric(pin_length_cm),
           front_back = case_when(f_b_front_back == 'F' ~ 'Front',
                                  f_b_front_back == 'B' ~ 'Back',
                                  TRUE ~ f_b_front_back)) %>%
    select(-pin_length_cm, -f_b_front_back)

###############################
###############################

# join together all the data frames in the list 'dat'
# this does NOT deal with column class differences
dat_all <- reshape::merge_recurse(dat)

###


















# read formats
fill_colors <- xlsx_formats(path)$local$fill$patternFill$fgColor$rgb

# generate a dataframe showing fill colors for each cell
fills <-
    xlsx_cells(path, sheets = "SET data") %>%
    dplyr::filter(row >= 2) %>% # Omit the header row and name column
    mutate(fill_color = fill_colors[local_format_id]) %>%
    select(row, col, fill_color) %>%
    spread(col, fill_color) %>%
    select(-row, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22) %>%
    set_names(paste0(colnames(dat), "_fill"))

# find out how many values there are in each column  
names <- colnames(fills)
sums <- rep(0, length(names))
for(i in seq_along(colnames(fills))){
    col <- fills[,i]
    sums[i] <- sum(!is.na(col))
}
fill_summary <- data.frame(names, sums)

# only need to keep fill columns that don't have 0
fills_exist <- fill_summary$names[fill_summary$sums > 0]
fills <- fills[ , as.character(fills_exist)]

# bind it with the data frame
dat_all <- bind_cols(dat, fills)

#############################
#############################



#############################
# Deal with color key
#############################

codes <- read_excel(path, sheet = 'SET data') %>%
    select(X__1) %>%
    set_names('code') %>%
    filter(!is.na(code))
    
codefills <- xlsx_cells(path, sheets = "SET data") %>%
    dplyr::filter(row >= 2) %>% # Omit the header row and name column
    mutate(fill_color = fill_colors[local_format_id]) %>%
    select(row, col, fill_color) %>%
    spread(col, fill_color) %>%
    select(11) %>%   # looked in spreadsheet to make sure this is correct column
    set_names(paste0(colnames(codes), "_fill")) %>%
    filter(!is.na(code_fill))

codes$fill <- codefills$code_fill

#############################
#############################



# get rid of some intermediate objects
rm('codefills', 'col', 'fill_colors', 'fills_exist', 'i', 'names', 'sums')




#############################
# Match fills and codes in main dataframe
#############################

# pin_measurement_mm is typically the column that has color coding
# sometimes set_pin_no does, but that may be resolved - so I'm writing this
# in such a way that the code will run with or without it
# using the if(exists) stuff

dat_coded <- left_join(dat_all, codes, by = c('pin_measurement_mm_fill' = 'fill')) %>%
    mutate(pin_code = code,
           code = NULL)  


if(exists('set_pin_no_fill', dat_coded) == TRUE) {
    dat_coded <- left_join(dat_coded, codes, by = c('set_pin_no_fill' = 'fill')) %>%
        mutate(pin_code2 = code,
               code = NULL)
}


dat_coded <- dat_coded %>%
    mutate(code = case_when(
        !is.na(pin_measurement_mm_fill) & !is.na(pin_code) ~ pin_code,
        !is.na(pin_measurement_mm_fill) & is.na(pin_code) ~ 'other, not in key',
        TRUE ~ '0'
    )
    ) %>%
    select(-pin_measurement_mm_fill, -pin_code)



if(exists('set_pin_no_fill', dat_coded) == TRUE) {
    dat_coded <- dat_coded %>%
        mutate(code = case_when(
            !is.na(set_pin_no_fill) & !is.na(pin_code2) ~ pin_code2,
            !is.na(set_pin_no_fill) & is.na(pin_code2) ~ 'other, not in key',
            !is.na(code) & !is.na(pin_code2) ~ paste0(code, pin_code2),
            TRUE ~ code
        )) %>%
        select(-set_pin_no_fill, -pin_code2)
}



# see how many codes are not in key
# hopefully after the original file is looked at, this will be 0
sum(dat_coded$code == 'other, not in key')


# change names to be slightly more consistent
# also get rid of any extra rows
dat_coded <- dat_coded %>%
    select(set_id = site_label, date, arm_position = set_arm_position, pin_number = set_pin_no, pin_height = pin_measurement_mm, code, notes, latitude, longitude) %>%
    filter(is.na(set_id) == FALSE)

# spit out csv file
write_csv(dat_coded, 'data/intermediate/DEL.csv')


# neg_pin_hts <- dat_coded %>% filter(pin_height < 0)
# write_csv(neg_pin_hts, 'data/needs_attention/DEL_neg_pin_heights.csv')
