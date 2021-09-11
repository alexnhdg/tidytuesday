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
            value = "United States Basic Infrastructure Investment, 1947-2017",
            placeholder = "Enter a plot title"
        ),
        
        sliderInput(
            inputId = "selected_years",
            label = "Year Range:",
            min = min(inv_data$year), max = max(inv_data$year) ,
            value = c(min(inv_data$year), max(inv_data$year)),
            step = 1,
            sep = ""
        ),
        
        selectInput(
            inputId = "selected_group",
            label = "Infrastructure Type:",
            choices = unique(inv_data$meta_cat)
        ),
        
        checkboxGroupInput(
            inputId = "selected_sub_groups",
            label = "Infrastructure Sub-Type:",
            choices = unique(inv_data$category),
            selected = unique(inv_data$category)
        ),
        
        downloadButton(
            outputId = "download_plot",
            label = "Download Plot"
        ),
        
        downloadButton(
            outputId = "download_data",
            label = "Download Data"
        )
        
    ),
    
    mainPanel(
        
        plotOutput(outputId = "plot"),
        DT::dataTableOutput(outputId = "plot_data")
        
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
    
    sub_groups <- reactive({
        filter(inv_data, meta_cat == input$selected_group)
    })
    observeEvent(sub_groups(), {
        choices <- unique(sub_groups()$category)
        updateCheckboxGroupInput(
            inputId = "selected_sub_groups",
            choices = choices,
            selected = choices)
    })
    
    plot_data <- reactive({
        inv_data %>% 
            filter(
                meta_cat == input$selected_group,
                category %in% input$selected_sub_groups,
                year >= input$selected_years[1],
                year <= input$selected_years[2]
            )
    })
    
    plot <- reactive({
        ggplot(plot_data(), aes(x = year, y = inv_gross)) +
            geom_area(
                aes(fill = category),
                position = "fill",
                color = "white"
            ) +
            scale_y_continuous(labels = scales::percent) +
            labs(
                title = input$plot_title,
                caption = "Source: US Bureau of Labor Economics",
                x = "", y = ""
            ) +
            theme_minimal() +
            theme(
                legend.position = "bottom"
            )
    })
    
    output$plot <- renderPlot({
        print(plot())
    })
    
    output$plot_data <- DT::renderDataTable({
        DT::datatable(
            plot_data(),
            colnames = c("Infrastructure Category" = "meta_cat",
                         "Infrastructure Sub-Category" = "category",
                         "Year" = "year",
                         "Gross Investment 2012 Dollars" = "inv_gross")
        )
    })
    
    output$download_plot <- downloadHandler(
        filename = function() {
            "myplot.png"
        },
        content = function(file) {
            ggsave(file, plot = plot(), device = "png")
        }
    )
    
    output$download_data <- downloadHandler(
        filename = function() {
            "mydata.csv"
        },
        content = function(file) {
            write_csv(plot_data(), file)
        }
    )

}


# Run Shiny App -----------------------------------------------------------

shinyApp(ui = ui, server = server)


