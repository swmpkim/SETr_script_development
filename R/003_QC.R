library(tidyverse)
library(lubridate)
library(here)
library(knitr)

funs_path <- here('R', '000_functions.R')
source(funs_path)

# pick a reserve
# (can we do this in shiny?)
path <- here('data', 'intermediate', 'TEST.csv')

# read in data
dat_full <- read_csv(path)

dat <- dat_full %>%
    filter(set_id == "CLMAJ-1")

# dat <- dat_full



# graphs of raw pin readings (not cumulative change; just the raw readings)
dat %>%
    group_by(set_id, arm_position, date) %>%
    summarize(mean = mean(pin_height, na.rm = TRUE)) %>%
    ggplot(aes(x = date, y = mean, col = arm_position)) +
    geom_point(size = 2.5) +
    geom_line(alpha = 0.6) +
    facet_wrap(~set_id, ncol = 1, scales = 'free_y') +
    ggtitle('Pin Height (raw measurement)') +
    theme_bw()

### generate some of the tables and graphs from the NPS spreadsheet
####################################################################
# cumulative change
calc_change_cumu(dat)
# incremental change
calc_change_incr(dat)


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
ggplot(change_cumu_arm, aes(x = date, y = mean_cumu, col = arm_position)) +
    geom_point(size = 2) +
    geom_line() +
    facet_wrap(~set_id, ncol = 1, scales = 'free_y') +
    ggtitle('Cumulative Change') +
    theme_bw()
    
# by SET
change_cumu_set %>%
    gather(key = summary_stat, value = value, mean_cumu, sd_cumu, se_cumu) %>%
    spread(key = date, value = value) %>%
    print()
ggplot(change_cumu_set, aes(x = date, y = mean_cumu)) +
    geom_line(col = 'gray80') +
    geom_point(col = 'cadetblue3', size = 2) +
    geom_smooth(se = FALSE, method = 'lm', col = 'gray60', lty = 2) +
    facet_wrap(~set_id, ncol = 1, scales = 'free_y') +
    labs(title = 'Cumulative Change since first reading') +
    theme_bw()


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

