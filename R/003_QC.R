library(tidyverse)
library(lubridate)
library(here)
library(knitr)

funs_path <- here('R', '000_functions.R')
source(funs_path)

# pick a reserve
# (can we do this in shiny?)
path <- here('data', 'intermediate', 'GND.csv')

# read in data
dat_full <- read_csv(path)


### generate some of the tables and graphs from the NPS spreadsheet
####################################################################
# cumulative change
calc_change_cumu(dat_full)
# incremental change
calc_change_incr(dat_full)


######################################
# it's going to be best to only print these tables for one SET at a time
# because with dates as column headers, and different dates that different SETs are read,
# it gets a bit crazy
# really i hope to do these in shiny, and the user can select what they want to see
# which SET, which level (pin/arm/set), etc
######################################



####################################################################
### View cumulative change, with dates along the top and groups as rows
####################################################################

# by pin
change_cumu_pin %>%
    spread(key = date, value = cumu) %>%
    print()

# by arm
change_cumu_arm %>%
    gather(key = summary_stat, value = value, mean_cumu, sd_cumu, se_cumu) %>%
    spread(key = date, value = value) %>%
    print()
    
# by SET
change_cumu_set %>%
    gather(key = summary_stat, value = value, mean_cumu, sd_cumu, se_cumu) %>%
    spread(key = date, value = value) %>%
    print()



####################################################################
### View incremental change (change since last reading), with dates along the top and groups as rows
####################################################################

# by pin
change_incr_pin %>%
    spread(key = date, value = incr) %>%
    print()

# by arm
change_incr_arm %>%
    gather(key = summary_stat, value = value, mean_incr, sd_incr, se_incr) %>%
    spread(key = date, value = value) %>%
    print()

# by SET
change_incr_set %>%
    gather(key = summary_stat, value = value, mean_incr, sd_incr, se_incr) %>%
    spread(key = date, value = value) %>%
    print()

