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
            checkboxInput(inputId = "smoother", 
                          label = strong("Overlay smooth trend line"),
                          value = FALSE
            )
            
            
        ),
        
        
        mainPanel(
            plotOutput(outputId = "scatterplot"),
            ggiraphOutput(outputId = "ggiraphscatter")
        )
        
    )
)



# Define server function
server <- function(input, output) {
    
    # Subset data
    # dat_subset <- reactive({
    #     req(input$date)
    #     validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    #     validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    #     dat %>%
    #         filter(
    #             set_id == input$SET,
    #             date >= as.POSIXct(input$date[1]) & date <= as.POSIXct(input$date[2]
    #             ))
    # })
    
    
    # Create scatterplot object the plotOutput function is expecting
    output$scatterplot <- renderPlot({
        ggplot(dat) +
            geom_point(aes(x = date, y = pin_height))
        })
    
    # output$ggiraphscatter <- renderggiraph({
    #     g <- ggplot(dat) +
    #         geom_point_interactive(aes(x = date, y = pin_height))
    #     ggiraph(code = print(g))
    # })

}

# Create Shiny object
shinyApp(ui = ui, server = server)
