######################################################
######################################################
#### Cumulative change (change since first reading)
######################################################

## calculate all levels of cumulative change, in one function?
## returns three data frames (puts them into the Global Environment)

## going to need better documentation, like what kind of data frame is needed as input (long); what column names are required; maybe some if statements to throw errors

calc_change_cumu <- function(dat) {
    # by pin
    change_cumu_pin <<- dat %>%
        group_by(reserve, set_id, arm_position, pin_number) %>%
        mutate(cumu = pin_height - pin_height[1]) %>%
        select(-pin_height) %>%
        ungroup()
    
    # pins averaged up to arms
    change_cumu_arm <<- change_cumu_pin %>%
        group_by(reserve, set_id, arm_position, date) %>%
        select(-pin_number) %>%
        summarize(mean_cumu = mean(cumu, na.rm = TRUE),
                  sd_cumu = sd(cumu, na.rm = TRUE),
                  se_cumu = sd(cumu, na.rm = TRUE)/sqrt(length(!is.na(cumu)))) %>%
        ungroup()
    
    # arms averaged up to SETs
    change_cumu_set <<- change_cumu_arm %>%
        group_by(reserve, set_id, date) %>%
        select(-arm_position, mean_value = mean_cumu) %>%
        summarize(mean_cumu = mean(mean_value, na.rm = TRUE),
                  sd_cumu = sd(mean_value, na.rm = TRUE),
                  se_cumu = sd(mean_value, na.rm = TRUE)/sqrt(length(!is.na(mean_value)))) %>%
        ungroup()
    
}




######################################################
######################################################
#### Incremental Change (change since last reading)
######################################################

calc_change_incr <- function(dat){
    # by pin
    change_incr_pin <<- dat %>%
        arrange(reserve, set_id, arm_position, pin_number, date) %>%
        group_by(reserve, set_id, arm_position, pin_number) %>%
        mutate(incr = pin_height - lag(pin_height, 1)) %>%
        ungroup()
    
    # pins averaged up to arms
    change_incr_arm <<- change_incr_pin %>%
        group_by(reserve, set_id, arm_position, date) %>%
        select(-pin_number) %>%
        summarize(mean_incr = mean(incr, na.rm = TRUE),
                  sd_incr = sd(incr, na.rm = TRUE),
                  se_incr = sd(incr, na.rm = TRUE)/sqrt(length(!is.na(incr)))) %>%
        ungroup()
    
    # arms averaged up to SETs
    change_incr_set <<- change_incr_arm %>%
        group_by(reserve, set_id, date) %>%
        select(-arm_position, mean_value = mean_incr) %>%
        summarize(mean_incr = mean(mean_value, na.rm = TRUE),
                  sd_incr = sd(mean_value, na.rm = TRUE),
                  se_incr = sd(mean_value, na.rm = TRUE)/sqrt(length(!is.na(mean_value)))) %>%
        ungroup()
}