library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)

##### DELAWARE
# Lots of color coding in this one

############################
############################

# read in contents of cells; each cell gets its own row here
# way to reference the formats of these cells is through 'local_format_id' column
dat_raw <- xlsx_cells('../data/DEL.xlsx', sheets = 'SET data')

# read in formatting of cells
dat_raw_formats <- xlsx_formats('../data/DEL.xlsx')



# color code ID cells are J2:J12 in the excel sheet; put these 'addresses' into a vector
color_code_ids <- paste0('J', seq(from = 2, to = 12))



# pull out the 'local_format_id's and contents of those cells
color_code_cells <- dat_raw[dat_raw$address %in% color_code_ids, c('address', 'local_format_id', 'character', 'style_format')]



# make the hex codes new columns in that data frame
# fg
color_code_cells$fghex <- dat_raw_formats$local$fill$patternFill$fgColor$rgb[color_code_cells$local_format_id]
#bg
color_code_cells$bghex <- dat_raw_formats$local$fill$patternFill$bgColor$rgb[color_code_cells$local_format_id]

#view it
color_code_cells


############################
############################


# how many cells have each of the codes?
out <- data.frame(fghex = color_code_cells$fghex, fgnumber = 0, bghex = color_code_cells$bghex, bgnumber = 0)

for(i in 1:nrow(out)){
    out$fgnumber[i] <- sum(dat_raw_formats$local$fill$patternFill$fgColor$rgb == out$fghex[i], na.rm = TRUE)
    
    out$bgnumber[i] <- sum(dat_raw_formats$local$fill$patternFill$bgColor$rgb == out$bghex[i], na.rm = TRUE)
}

# view results
out

############################
############################


dat_raw[dat_raw$local_format_id == 91,]
# there are several of these; this format doesn't match any of the ones in the color key table

# let's find the color then
dat_raw_formats$local$fill$patternFill$fgColor$rgb[91]
# FF92D050

table(dat_raw$local_format_id)



# let's get all the fgcolors
formatids <- unique(dat_raw$local_format_id)
out2 <- data.frame(formatid = formatids, fghex = NA, bghex = NA)

for(i in 1:nrow(out2)){
    out2$fghex[i] <- dat_raw_formats$local$fill$patternFill$fgColor$rgb[out2$formatid[i]]
    
    out2$bghex[i] <- dat_raw_formats$local$fill$patternFill$bgColor$rgb[out2$formatid[i]]
}

# view results
out2


# what are the oddballs? (and where?)
not_in_key <- unique(out2$fghex[!(out2$fghex %in% color_code_cells$fghex)])
not_in_key <- not_in_key[!is.na(not_in_key)]
not_in_key


cells_with_diff_colors <- dat_raw[dat_raw$local_format_id %in% which(dat_raw_formats$local$fill$patternFill$fgColor$rgb %in% not_in_key), 'address']

write.csv(cells_with_diff_colors, '../data_intermed/DEL_need_attention.csv', row.names = FALSE)

# next, will need to find cells with these hex colors, and assign the charachter strings from that table to them - these are QC flags
