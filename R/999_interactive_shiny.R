# just working with one dataset until this all works
# basically copied and adapted code from this app:
# https://shiny.rstudio.com/#code-app

# load packages
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(here)
library(shiny)
library(plotly)



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


### define UI
ui <- fluidPage(
    titlePanel("SET graphing"),
    sidebarLayout(
        sidebarPanel(
            
            # select a SET to plot
            selectInput(inputId = "SET", label = strong("SET ID"),
                        choices = unique(dat$set_id),
                        selected = unique(dat$set_id)[1]
            ),
            
            # select date range to plot
            dateRangeInput("date", strong("Date range"), 
                           start = min(dat$date), end = max(dat$date),
                           min = min(dat$date), max = max(dat$date)
            ),
            
            # select whether to overlay a trend line
            checkboxInput(inputId = "lmsmooth", 
                          label = strong("Overlay linear regression"),
                          value = FALSE
            ),
            
            # select whether to overlay a loess line
            checkboxInput(inputId = "loesssmooth", 
                          label = strong("Overlay loess smooth"),
                          value = FALSE
            )
            
            
        ),
        
        
        mainPanel(
            plotlyOutput(outputId = "plotlyscatter")
        )
        
    )
)



# Define server function
server <- function(input, output) {
    
    
    # subset data, reactively
    dat2 <- reactive({
        req(input$SET)
        req(input$date)
        dat %>%
            filter(set_id == input$SET,
                   date >= as.Date(input$date[1]),
                   date <= as.Date(input$date[2]))
    })

    
    # create plotly object
    output$plotlyscatter <- renderPlotly({
        
        # create the base plot
        p <- ggplot(dat2()) +
            geom_point(aes(x = date, y = pin_height, col = arm_position)) +
            labs(title = paste("Raw pin measurements at", input$SET), x = "Date", y = "pin height (mm)") +
            theme_bw() +
            theme(legend.position = 'bottom')
        
        # add smoothing layers if checked
        if (input$lmsmooth == TRUE) p <- p + geom_smooth(aes(x = date, y = pin_height), method = "lm", se = FALSE)
        
        if (input$loesssmooth == TRUE) p <- p + geom_smooth(aes(x = date, y = pin_height), method = "loess", se = FALSE)
        
        # return the plot
        p
        
    })
    
}

# Create Shiny object
shinyApp(ui = ui, server = server)
