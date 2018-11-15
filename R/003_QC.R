# trying to only load necessary packages
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(here)
# library(tidyverse)
# library(lubridate)
# library(knitr)


################################################
#### Specify the reserve
################################################
reserve <- 'GND'


## specify output folder and default saving options for graphs
outpath <- here('qc_figs')

my.ggsave <- function(filename = outname, width = 9, height = 7, units = 'in', dpi = 500){
    ggsave(filename = filename, width = width, height = height, units = units, dpi = dpi)
}



# source functions script
funs_path <- here('R', '000_functions.R')
source(funs_path)

# read in data
path <- here('data', 'intermediate', paste0(reserve, '.csv'))
dat_full <- read_csv(path)



# make sure pin heights are in mm so flagging works properly
if(exists('pin_height_cm', dat_full)) {
dat_full <- dat_full %>%
    mutate(pin_height = pin_height_cm * 10) %>%
    select(-pin_height_cm)
}
if(exists('pin_height_mm', dat_full)){
    dat_full <- dat_full %>%
        mutate(pin_height = pin_height_mm) %>%
        select(-pin_height_mm)
}


# filter to a single site or fewer if you want
dat <- dat_full


###### Calculations of Change
#############
# cumulative change
calc_change_cumu(dat)
# incremental change
calc_change_incr(dat)

###################################################
## Graphs of raw measurements
##################################################

##### histogram of all pin readings
hist_by_arm(dat, 3)

# save it
figtype <- 'hist_by_arm'
outname <- paste0(outpath, '/', reserve, '_', figtype, '.png')
my.ggsave()


##### graphs of raw pin readings (not cumulative change; just the raw readings)

## by arm
plot_raw_arm(dat, 3)

figtype <- 'raw_by_arm'
outname <- paste0(outpath, '/', reserve, '_', figtype, '.png')
my.ggsave()



## by pin (pick a SET)
plot_raw_pin(dat, set = 'SPALT-1')

figtype <- 'raw_by_pin'
outname <- paste0(outpath, '/', reserve, '_SPALT-1_', figtype, '.png')
my.ggsave()


####################################################################
### Graphs of cumulative change
####################################################################


# by arm
plot_cumu_arm(3)

figtype <- 'cumu_by_arm'
outname <- paste0(outpath, '/', reserve, '_', figtype, '.png')
my.ggsave()



# by SET
plot_cumu_set(3)

figtype <- 'cumu_by_set'
outname <- paste0(outpath, '/', reserve, '_', figtype, '.png')
my.ggsave()


####################################################################
### Incremental change
####################################################################

plot_incr_arm(3)
# for a single set, use plot_incr_arm(set = 'name of SET you want to graph')

figtype <- 'incr_by_arm'
outname <- paste0(outpath, '/', reserve, '_', figtype, '.png')
my.ggsave()


plot_incr_pin('SPALT-1', columns = 2)
figtype <- 'incr_by_pin'
outname <- paste0(outpath, '/', reserve, '_SPALT-1_', figtype, '.png')
my.ggsave()



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
### Tables of Incremental Change
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

