# just working with one dataset until this all works
# basically copied and adapted code from this app:
# https://shiny.rstudio.com/#code-app

# load packages
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(here)
library(ggiraph)



## load and shape data
reserve <- 'GND'
# source functions script
funs_path <- here('R', '000_functions.R')
source(funs_path)
funs_path2 <- here('R', '000_functions_extra.R')
source(funs_path2)
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
dat <- dat_full
###### Calculations of Change
#############
# cumulative change
calc_change_cumu(dat)
# incremental change
calc_change_incr(dat)


### this function uses ggiraph
h <- plot_cumu_set_interactive(columns = 3)
girafe(ggobj = h)
girafe(code = print(h))


### this is from plotly and doesn't require anything special first
g <- plot_cumu_set(columns = 3)
ggplotly(g)


j <- plot_cumu_arm(columns = 3)
ggplotly(j)


k <- plot_incr_arm2(columns = 3)
ggplotly(k)

m <- plot_incr_pin2(set = 'JURO_Low-2')
ggplotly(m)
