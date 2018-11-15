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


## specify output folder for graphs
outpath <- here('qc_figs')


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
hist_by_arm(dat)

# save it
figtype <- 'hist_by_arm'
outname <- paste0(outpath, '/', reserve, '_', figtype, '.png')
ggsave(outname, width = 6.2, height = 4.6, units = 'in', dpi = 400)


##### graphs of raw pin readings (not cumulative change; just the raw readings)

## by arm
plot_raw_arm(dat)

figtype <- 'raw_by_arm'
outname <- paste0(outpath, '/', reserve, '_', figtype, '.png')
ggsave(outname, width = 6.2, height = 4.6, units = 'in', dpi = 400)


## by pin (pick a SET)
plot_raw_pin(dat, set = 'SPALT-1')

figtype <- 'raw_by_pin'
outname <- paste0(outpath, '/', reserve, '_SPALT-1_', figtype, '.png')
ggsave(outname, width = 6.2, height = 4.6, units = 'in', dpi = 400)


####################################################################
### Graphs of cumulative change
####################################################################


# by arm
plot_cumu_arm(3)

figtype <- 'cumu_by_arm'
outname <- paste0(outpath, '/', reserve, '_', figtype, '.png')
ggsave(outname, width = 6.2, height = 4.6, units = 'in', dpi = 400)



# by SET
plot_cumu_set(3)

figtype <- 'cumu_by_set'
outname <- paste0(outpath, '/', reserve, '_', figtype, '.png')
ggsave(outname, width = 6.2, height = 4.6, units = 'in', dpi = 400)



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
## 2 columns
ggplot(change_incr_arm, aes(x = date, y = mean_incr, col = as.factor(arm_position))) +
    geom_point(size = 2) +
    geom_hline(yintercept = 25, col = "red", size = 1) +
    geom_hline(yintercept = -25, col = "red", size = 1) +
    facet_wrap(~set_id, ncol = 2, scales = 'free_y') +
    labs(title = 'Incremental Change', 
         subtitle = 'red lines at +/- 25 mm',
         x = 'Date',
         y = 'Change since prior reading (mm)') +
    theme_bw() +
    scale_color_discrete(name = 'Arm Position') +
    theme(legend.position = 'bottom')
## 3 columns
ggplot(change_incr_arm, aes(x = date, y = mean_incr, col = as.factor(arm_position))) +
    geom_point(size = 2) +
    geom_hline(yintercept = 25, col = "red", size = 1) +
    geom_hline(yintercept = -25, col = "red", size = 1) +
    facet_wrap(~set_id, ncol = 3, scales = 'free_y') +
    labs(title = 'Incremental Change', 
         subtitle = 'red lines at +/- 25 mm',
         x = 'Date',
         y = 'Change since prior reading (mm)') +
    theme_bw() +
    scale_color_discrete(name = 'Arm Position') +
    theme(legend.position = 'bottom')
# 4 columns
ggplot(change_incr_arm, aes(x = date, y = mean_incr, col = as.factor(arm_position))) +
    geom_point(size = 2) +
    geom_hline(yintercept = 25, col = "red", size = 1) +
    geom_hline(yintercept = -25, col = "red", size = 1) +
    facet_wrap(~set_id, ncol = 4, scales = 'free_y') +
    labs(title = 'Incremental Change', 
         subtitle = 'red lines at +/- 25 mm',
         x = 'Date',
         y = 'Change since prior reading (mm)') +
    theme_bw() +
    scale_color_discrete(name = 'Arm Position') +
    theme(legend.position = 'bottom')

# by SET
change_incr_set %>%
    gather(key = summary_stat, value = value, mean_incr, sd_incr, se_incr) %>%
    spread(key = date, value = value) %>%
    print()

