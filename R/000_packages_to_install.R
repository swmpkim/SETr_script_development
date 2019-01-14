# Run one chunk at a time below, and when you get the 'success' message,
# move on to the next line
# sucess looks something like this:
# package ‘here’ successfully unpacked and MD5 sums checked

# if you have any errors, try that chunk again later. 
# If it still doesn't work, copy and paste the whole chunk (including the code you ran) into an email and send it to Kim



install.packages(c("broom", "ggplot2", "here", "janitor"), 
                 dependencies = TRUE, 
                 quiet = TRUE)


install.packages(c("knitr", "lubridate", "nlme", "plotly"), 
                 dependencies = TRUE, 
                 quiet = TRUE)


install.packages(c("readr", "readxl", "rmarkdown", "shiny"), 
                 dependencies = TRUE, 
                 quiet = TRUE)


install.packages(c("dplyr", "tidyr"), 
                 dependencies = TRUE, 
                 quiet = TRUE)
