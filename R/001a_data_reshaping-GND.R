# generally already in wide format because of how we've been working on it. 
# adding flag columns for pins, and spitting out csv files for each SET id.

library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)
# library(xlsx)
library(XLConnect)
# library(r2excel)
# if problems installing/loading xlsx with rJava error, see the top answer here:
# https://stackoverflow.com/questions/17376939/problems-when-trying-to-load-a-package-in-r-due-to-rjava



#############################
# Deal with main data frame
#############################

path <- here('data', 'raw_original', 'GND.csv')
dat <- read_csv(path) 
dat <- dat %>%
    mutate(reserve = 'GND',
           set_id = SET,
           arm_position = arm,
           date = as.character(mdy(date)),
           f_pin_1 = 0,
           f_pin_2 = 0,
           f_pin_3 = 0,
           f_pin_4 = 0,
           f_pin_5 = 0,
           f_pin_6 = 0,
           f_pin_7 = 0,
           f_pin_8 = 0,
           f_pin_9 = 0) %>%
    select(reserve, set_id, arm_position, everything(), -SET, -arm) %>%
    arrange(set_id, date, arm_position)

unique(dat$set_id)

# change set names

# panne to juro_high
dat$set_id <- gsub('PANNE', 'JURO_High', dat$set_id)

# juro_1,2,3 to juro_low
dat$set_id <- gsub('JURO-1', 'JURO_Low-1', dat$set_id)
dat$set_id <- gsub('JURO-2', 'JURO_Low-2', dat$set_id)
dat$set_id <- gsub('JURO-3', 'JURO_Low-3', dat$set_id)

# juro 4,5,6 to juro_mid
dat$set_id <- gsub('JURO-4', 'JURO_Mid-1', dat$set_id)
dat$set_id <- gsub('JURO-5', 'JURO_Mid-2', dat$set_id)
dat$set_id <- gsub('JURO-6', 'JURO_Mid-3', dat$set_id)

# spal to spalt
dat$set_id <- gsub('SPAL', 'SPALT', dat$set_id)

unique(dat$set_id)

unique_sets <- unique(dat$set_id)

for(i in seq_along(unique_sets)) {
    dat_sub <- dat[dat$set_id == unique_sets[i], ]
    csvname <-  paste0(tolower(unique_sets[i]), '.csv')
    csvpath <- here('data', 'split_by_site', csvname)
    xlname <-  paste0(tolower(unique_sets[i]), '.xlsx')
    xlpath <- here('data', 'split_by_site', xlname)
    # write_csv(dat_sub, filepath)
    
    
    
    ## r2excel
    # wb <- createWorkbook()
    # sheet <- createSheet(wb, sheetName = "data")
    # xlsx.addTable(wb, sheet, dat_sub, startCol=1, rowFill = c("white", "gray88"))
    # saveWorkbook(wb, xlpath)
    
    
    ## from xlsx
    # write.xlsx(dat_sub, file = xlpath, sheetName = "data")
    # file <- xlpath
    # wb <- loadWorkbook(file)
    # fo <- Fill(foregroundColor = "gray88")
    # sheets <- getSheets(wb)
    # sheet <- sheets[['data']]
    # cs <- CellStyle(wb, fill = fo)
    # rows <- getRows(sheet, rowIndex=2:(nrow(dat_sub)+1))     
    # cells <- getCells(rows, colIndex = 2:cols) 
    

    
    
    # generate a vector of row numbers to highlight
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
    

    # last step for xlsx
    # saveWorkbook(wb, file)
    
    
    # maybe try XLConnect
    wb <- loadWorkbook(xlpath, create = TRUE)
    createSheet(wb, name = 'data')
    writeWorksheet(wb, dat_sub, sheet = 'data')
    
    
    # create the style for highlighted rows
    highlighted2 <- createCellStyle(wb, 'highlighted2')
    setFillPattern(highlighted2, fill = XLC$"FILL.SOLID_FOREGROUND")
    setFillForegroundColor(highlighted2, color = XLC$"COLOR.GREY_25_PERCENT")

    # highlight the rows
    rowIndex <- rowstohighlight + 1    
    colIndex <- 1:ncol(dat_sub)

    rc = expand.grid(row = rowIndex, col = colIndex)
    setCellStyle(wb, sheet = 'data', row = rc$row, col = rc$col, cellstyle = highlighted2)
    
    
    # make a header row format
    header <- createCellStyle(wb, 'header')
    setFillPattern(header, fill = XLC$"FILL.SOLID_FOREGROUND")
    setFillForegroundColor(header, color = XLC$"COLOR.LIGHT_CORNFLOWER_BLUE")
    setBorder(header, 'bottom', type = XLC$"BORDER.DOUBLE", color = XLC$"COLOR.BLACK")

    setCellStyle(wb, sheet = 'data', row = 1, col = colIndex, cellstyle = header)
    

    saveWorkbook(wb)
    
    # might want to look for groupings where SET and date are the same
    # i'm imagining a situation where an arm position wasn't read for some reason, so i don't want to just use multiples of 4
    # rowstohighlight <- ()
    # 
    # wb <- createWorkbook()
    # 
    # 
    # # Create a new sheet in the workbook
    # sheet <- createSheet(wb, sheetName = "data")
    # addDataFrame(dat_sub, sheet)
    # saveWorkbook(wb, xlpath)
    
    ## from openxlsx
    
#     wb <- createWorkbook()
#     addWorksheet(wb, 'data')
#     writeDataTable(wb, 1, dat_sub, startRow = 1, startCol = 1,
#               tableStyle = 'TableStyleLight9',
#               bandedRows = TRUE,
#               bandedCols = TRUE)
#     saveWorkbook(wb, xlpath, overwrite = TRUE)
}
