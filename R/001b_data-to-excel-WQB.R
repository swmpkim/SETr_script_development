# taking the already-generated csv files with all data
# and spitting out an Excel workbook with
# one sheet per SET

# this only needs to happen once per reserve; then reserves
# need to use these spreadsheets for future data entry
# other scripts will read the data back into a single CSV for processing


# reserve:
reserve <- "WQB"

#### this file has some duplicate readings


# load packages
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
library(here)
library(XLConnect)

# generate name for output file
xlname <-  paste0(tolower(reserve),'set.xlsx')
xlpath <- here('data', 'intermediate', 'excel', xlname)


# read in data
path <- here::here('data/intermediate')
dat <- read_csv(paste0(path, '/', reserve, '.csv'))


# if pin number is just a number, make it a string
if (class(dat$pin_number) == "numeric")
    dat$pin_number <- paste0("pin_", dat$pin_number)

##################################################################
# find duplicate pin readings - value may not be duplicated, but 
# reserve, set_id, date, arm_position, and pin_number all are
dups <- dat[duplicated(dat[ , 1:5])==TRUE, ]


## for now, remove those by only keeping rows where duplicated == FALSE
dat <- dat[duplicated(dat[ , 1:5])==FALSE, ]

# also, for ease of use, get rid of the "notes" column
# i hate doing this but i just need a data file for now
dat <- dat %>%
    select(-notes)

#################################################################


# reshape the dataset, back to wide format
# first look for either pin_height_mm or pin_height_cm to use as the key
pin_col_name <- grep("pin_height", names(dat), value = TRUE)
# hold onto those units
pin_units <- str_extract(pin_col_name, "[^_]*$")
# now group, spread, and arrange
# also turn date into a character string because otherwise it saves crazily
dat_wide <- dat %>%
    mutate(pin_number = paste0(pin_number, "_", pin_units)) %>%
    group_by(reserve, set_id, arm_position, date) %>%
    spread(key = pin_number, value = !!pin_col_name) %>%
    arrange(reserve, set_id, date, arm_position) %>%
    ungroup() %>%
    mutate(date = as.character(date))

# make new column names for flags, based on pins that exist in the data
pin_ids <- grep("pin_", names(dat_wide), value = TRUE) %>%
    str_extract("pin_[:digit:]")
flag_ids <- paste0(pin_ids, "_flag")

# loop through each flag id and create the column in the data frame
for(i in 1:length(flag_ids)){
    # how many columns do we already have?
    col_num <- length(names(dat_wide))
    # generate a column of 0s at the end
    dat_wide[[1 + col_num]] <- as.numeric(0)
    # name the column
    names(dat_wide)[1 + col_num] <- flag_ids[i]
}


#############################################
#############################################
#############################################

# create the workbook
wb <- loadWorkbook(xlpath, create = TRUE)


# create worksheets by looping through all SETs. first make list of each SET id
unique_sets <- unique(dat_wide$set_id)

# create styles for the worksheets
# create the style for highlighted rows
highlighted2 <- createCellStyle(wb, 'highlighted2')
setFillPattern(highlighted2, fill = XLC$"FILL.SOLID_FOREGROUND")
setFillForegroundColor(highlighted2, color = XLC$"COLOR.GREY_25_PERCENT")

# create the style for the header row
header <- createCellStyle(wb, 'header')
setFillPattern(header, fill = XLC$"FILL.SOLID_FOREGROUND")
setFillForegroundColor(header, color = XLC$"COLOR.WHITE")
setBorder(header, 'bottom', type = XLC$"BORDER.DOUBLE", color = XLC$"COLOR.BLACK")


# build the worksheets, one SET at a time
for(i in seq_along(unique_sets)) {
    # subset the data
    dat_sub <- dat_wide[dat_wide$set_id == unique_sets[i], ]
    
    # generate a name for the worksheet based on the SET id
    sheetname <- as.character(unique_sets[i])
    # if there are more than 31 characters, use the first 31
    # and generate a warning
    if (nchar(sheetname) > 31) {
        warning(paste(sheetname, "is too long of a worksheet name. Only the first 31 characters will be used."))
        sheetname <- substr(sheetname, 1, 31)
    }
    
    
    
    ### GENERATE A FORMATTED EXCEL SHEET
    # generate a vector of row numbers to highlight in a formatted Excel sheet
    groupsof4 <- nrow(dat_sub)/4
    rowstohighlight <- rep(0, groupsof4*2)
    nexttohighlight <- 1:4
    nexttoindex <- 1:4
    nloop <- ceiling(length(rowstohighlight) / 4)
    for(j in 1:nloop){
        rowstohighlight[nexttoindex] <- nexttohighlight
        nexttohighlight <- nexttohighlight + 8
        nexttoindex <- nexttoindex + 4
    }
    
    
    # create the worksheet
    createSheet(wb, name = sheetname)
    
    # write the subsetted data into the worksheet
    writeWorksheet(wb, dat_sub, sheet = sheetname)
    
    
    # highlight the rows
    rowIndex <- rowstohighlight + 1    
    colIndex <- 1:ncol(dat_sub)
    rc = expand.grid(row = rowIndex, col = colIndex)
    setCellStyle(wb, sheet = sheetname, row = rc$row, col = rc$col, cellstyle = highlighted2)
    
    # format the header row
    setCellStyle(wb, sheet = sheetname, row = 1, col = colIndex, cellstyle = header)
}

saveWorkbook(wb)
