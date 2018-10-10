library(tidyverse)

path <- 'data/intermediate'

files_to_check <- dir(path)

DEL <- paste0(path, '/DEL.csv')
SOS <- paste0(path, '/SOS.csv')

##########################################
file_of_choice <- DEL
##########################################



dat <- read_csv(file_of_choice)

# general: what's in here and what does each column look like?
names(dat)
str(dat)

# check to make sure everything is within boundaries
# look for duplication in names; pin numbers that are not 1-9; negative pin heights

unique(dat$set_id)
summary(dat$date)
unique(dat$arm_position)
unique(dat$pin_number)
summary(dat$pin_number)
summary(dat$pin_height)


ggplot(dat) +
    geom_histogram(aes(pin_height), fill = 'navyblue', bins = 50) +
    facet_wrap(~set_id, ncol = 2, scales = 'free_y') +
    theme_bw()

# check for negative pin heights
if(sum(dat$pin_height < 0, na.rm = TRUE) > 1) {
    print(paste0("There are ", sum(dat$pin_height < 0, na.rm = TRUE), " pin heights <0. The first 10 are below. To see more, enter the command View(neg_pin_hts)."))
    neg_pin_hts <- dat %>% filter(pin_height < 0)
    head(neg_pin_hts, 10)
    } else {print("all pin heights are >0")}
