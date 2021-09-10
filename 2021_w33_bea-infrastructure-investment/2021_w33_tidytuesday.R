# 2021_w33_tidytuesday.R
# BEA Infrastructure Investment
# Alexander de Groot


# Environment setup -------------------------------------------------------

library(tidyverse)
library(shiny)


# Get Data ----------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 33)
chain_investment <- tuesdata$chain_investment


# Tidy Data ---------------------------------------------------------------

dash_data_chained <- chain_investment %>%
  filter(group_num %in% c(1, 2, 3, 4, 16, 17, 22))


# User Interface ----------------------------------------------------------

ui <- fluidPage(
    
    titlePanel("United States Historical Infrastucture Investment Dashboard"),
    
    sidebarPanel(
        
        textInput(
            inputId = "plot_title",
            label = "Plot Title:",
            value = "Title goes here",
            placeholder = "Enter a plot title"
        ),
        
        sliderInput(
            inputId = "selected_years",
            label = "Year Range:",
            min = min(dash_data_chained$year), max = max(dash_data_chained$year) ,
            value = c(min(dash_data_chained$year), max(dash_data_chained$year)),
            step = 1
        ),
        
        checkboxGroupInput(
            inputId = "selected_meta_groups",
            label = "Infrastrucutre Type:",
            choices = unique(dash_data_chained$meta_cat),
            selected = unique(dash_data_chained$meta_cat)
        ),
        
        selectInput(
            inputId = "selected_sub_groups",
            label = "Infrastructure Sub-Group:",
            choices = unique(dash_data_chained$category),
            selected = NULL,
            multiple = TRUE,
            selectize = TRUE
        )
    ),
    
    mainPanel(
        
        plotOutput(outputId = "plot")
        
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
    
    plot_data_chained <- reactive({
        dash_data_chained %>% 
            filter(
                year >= input$selected_years[1],
                year <= input$selected_years[2],
                meta_cat %in% input$selected_meta_groups,
                category %in% input$selected_sub_groups
            )
    })
    
    output$plot <- renderPlot({
        ggplot(plot_data_chained(), aes(x = year, y = gross_inv_chain)) +
            geom_area(aes(fill = category), color = "white") +
            labs(
                title = input$plot_title,
                caption = "Source: US Bureau of Labor Economics",
                x = "Year", y = "US Dollars"
            ) +
            theme_minimal()
    })

}


# Run Shiny App -----------------------------------------------------------

shinyApp(ui = ui, server = server)


