# 2021_w33_tidytuesday.R
# BEA Infrastructure Investment
# Alexander de Groot


# Environment setup -------------------------------------------------------

library(tidyverse)
library(shiny)
library(readxl)
library(here)

# Get Data ----------------------------------------------------------------

inv_gross_data_raw <- read_xlsx(
    path = here("2021_w33_bea-infrastructure-investment", "infrastructure-data-may-2020.xlsx"),
    sheet = "cu$inv",
    range = "A3:BU108",
    na = ""
)


# Tidy Data ---------------------------------------------------------------

inv_data <- inv_gross_data_raw %>% 
    rename(group_num = `...1`, category = `...2`) %>% 
    mutate(
        meta_cat = ifelse(!is.na(group_num), category, NA_character_)
    ) %>% 
    fill(group_num, meta_cat) %>% 
    group_by(group_num) %>% 
    slice(-1) %>% 
    ungroup() %>% 
    pivot_longer(
        cols = `1947`:`2017`,
        names_to = "year",
        names_transform = c(year = as.integer),
        values_to = "inv_gross"
    ) %>% 
    filter(group_num %in% c(1, 2, 3, 4, 16, 17, 22)) %>% 
    mutate(
        meta_cat = case_when(
            group_num == 1 ~ "Total infrastructure by type",
            group_num == 2 ~ "Total infrastructure by funding source",
            group_num == 3 ~ "Basic infrastructure by funding source",
            group_num == 4 ~ "Basic infrastructure by type",
            group_num == 16 ~ "Social infrastructure by funding source",
            group_num == 17 ~ "Social infrastructure by type",
            group_num == 22 ~ "Digital infrastructure by type"
        )
    ) %>% 
    select(meta_cat, category, year, inv_gross)


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


