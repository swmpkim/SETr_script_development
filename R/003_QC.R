library(tidyverse)
library(lubridate)
library(here)
library(knitr)

################################################
#### Specify the reserve
################################################
reserve <- 'WAQ'



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

# wonky site at DEL
# dat <- dat_full %>%
#    filter(set_id %in% c("Delon_Deep","Delon_Shallow"))

dat <- dat_full


### generate some of the tables and graphs from the NPS spreadsheet
####################################################################
# cumulative change
calc_change_cumu(dat)
# incremental change
calc_change_incr(dat)



# graphs of raw pin readings (not cumulative change; just the raw readings)
dat %>%
    group_by(set_id, arm_position, date) %>%
    summarize(mean = mean(pin_height, na.rm = TRUE)) %>%
    ggplot(aes(x = date, y = mean, col = as.factor(arm_position))) +
    geom_point(size = 2.5) +
    geom_line(alpha = 0.6) +
    facet_wrap(~set_id, ncol = 2, scales = 'free_y') +
    ggtitle('Pin Height (raw measurement)') +
    theme_bw()


# not with free-y scales
dat %>%
    group_by(set_id, arm_position, date) %>%
    summarize(mean = mean(pin_height, na.rm = TRUE)) %>%
    ggplot(aes(x = date, y = mean, col = arm_position)) +
    geom_point(size = 2.5) +
    geom_line(alpha = 0.6) +
    facet_wrap(~set_id, ncol = 2) +
    ggtitle('Pin Height (raw measurement)') +
    theme_bw()

# 4 columns, which is better when there are a lot of SETs (ahem, PAD)
dat %>%
    group_by(set_id, arm_position, date) %>%
    summarize(mean = mean(pin_height, na.rm = TRUE)) %>%
    ggplot(aes(x = date, y = mean, col = as.factor(arm_position))) +
    geom_point(size = 2.5) +
    geom_line(alpha = 0.6) +
    facet_wrap(~set_id, ncol = 4, scales = 'free_y') +
    ggtitle('Pin Height (raw measurement)') +
    theme_bw()

# individual pins
dat %>%
    group_by(set_id, arm_position, pin_number, date) %>%
    ggplot(aes(x = date, y = pin_height, col = as.factor(pin_number))) +
    geom_point(size = 2.5) +
    geom_line(alpha = 0.6) +
    facet_wrap(arm_position~set_id, ncol = 2) +
    ggtitle('Pin Height (raw measurement)') +
    theme_bw()



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
ggplot(change_cumu_arm, aes(x = date, y = mean_cumu, col = as.factor(arm_position))) +
    geom_point(size = 2) +
    geom_line() +
    facet_wrap(~set_id, ncol = 2, scales = 'free_y') +
    ggtitle('Cumulative Change') +
    theme_bw()
    
# by SET
change_cumu_set %>%
    gather(key = summary_stat, value = value, mean_cumu, sd_cumu, se_cumu) %>%
    spread(key = date, value = value) %>%
    print()
ggplot(change_cumu_set, aes(x = date, y = mean_cumu)) +
    geom_line(col = 'gray60') +
    geom_point(col = 'cadetblue3', size = 2) +
    geom_smooth(se = FALSE, method = 'lm', col = 'gray70', lty = 2) +
    facet_wrap(~set_id, ncol = 2, scales = 'free_y') +
    labs(title = 'Cumulative Change since first reading', subtitle = 'dashed line is linear regression') +
    theme_classic()


# 4 columns, which is better when there are a lot of SETs (ahem, PAD)
ggplot(change_cumu_set, aes(x = date, y = mean_cumu)) +
    geom_line(col = 'gray60') +
    geom_point(col = 'cadetblue3', size = 2) +
    geom_smooth(se = FALSE, method = 'lm', col = 'gray70', lty = 2) +
    facet_wrap(~set_id, ncol = 4, scales = 'free_y') +
    labs(title = 'Cumulative Change since first reading', subtitle = 'dashed line is linear regression') +
    theme_classic()


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
ggplot(change_incr_arm, aes(x = date, y = mean_incr, col = as.factor(arm_position))) +
    geom_point(size = 2) +
    geom_hline(yintercept = 25, col = "red") +
    geom_hline(yintercept = -25, col = "red") +
    facet_wrap(~set_id, ncol = 2, scales = 'free_y') +
    ggtitle('Incremental Change', subtitle = 'red lines at +/- 25 mm') +
    theme_bw()
# 4 columns
ggplot(change_incr_arm, aes(x = date, y = mean_incr, col = as.factor(arm_position))) +
    geom_point(size = 2) +
    geom_hline(yintercept = 25, col = "red") +
    geom_hline(yintercept = -25, col = "red") +
    facet_wrap(~set_id, ncol = 4) +
    ggtitle('Incremental Change', subtitle = 'red lines at +/- 25 mm') +
    theme_bw()

# by SET
change_incr_set %>%
    gather(key = summary_stat, value = value, mean_incr, sd_incr, se_incr) %>%
    spread(key = date, value = value) %>%
    print()

