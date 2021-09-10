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

)


# Server ------------------------------------------------------------------

server <- function(input, output) {
    
}


# Run Shiny App -----------------------------------------------------------

shinyApp(ui = ui, server = server)




# get data ----------------------------------------------------------------

# chained investment data by state
chained_inv_data_raw <- read_excel(
    path = "week-33_bea-infrastructure-investment/infrastructure-data-may-2020.xlsx",
    sheet = "By state data",
    range = "B117:AB169"
)

chained_inv_data <- chained_inv_data_raw %>% 
    rename(state = ...1) %>% 
    mutate(state = str_squish(state)) %>% 
    filter(state != "S&L Highways") %>% 
    pivot_longer(
        cols = `1992`:`2017`,
        names_to = "year",
        values_to = "gross_inv_chained",
        names_transform = list(year = as.integer)
    )


# population data
state_pop_raw <- read_excel(
    path = "week-33_bea-infrastructure-investment/infrastructure-data-may-2020.xlsx",
    sheet = "By state data",
    range = "B174:AB226"
)


state_pop <- state_pop_raw %>% 
    rename(state = ...1) %>% 
    mutate(state = str_squish(state)) %>% 
    filter(state != "United States") %>% 
    pivot_longer(
        cols = `1992`:`2017`,
        names_to = "year",
        values_to = "population",
        names_transform = list(year = as.integer)
    )


# highway and street infrastructure age data
# hwy_age_raw <- read_excel(
#     path = "week-33_bea-infrastructure-investment/infrastructure-data-may-2020.xlsx",
#     sheet = "Avg age govt FA",
#     range = "A7:CR81",
#     na = "....."
# )
# 
# hwy_age <- hwy_age_raw %>% 
#     rename(investment = ...2) %>% 
#     mutate(investment = str_squish(investment)) %>% 
#     filter(Line == "67") %>% 
#     select(`1925`:`2018`) %>% 
#     pivot_longer(
#         cols = `1925`:`2018`,
#         names_to = "year",
#         values_to = "hwy_age",
#         names_transform = list(year = as.integer)
#     )


# prepare date for charting -----------------------------------------------

hwy_inv_data <- chained_inv_data %>% 
    left_join(state_pop, by = c("state", "year")) %>% 
    # left_join(hwy_age, by = "year") %>% 
    mutate(
        inv_per_capita = gross_inv_chained / population
    ) %>% 
    group_by(state) %>% 
    mutate(
        label = if_else(year == max(year), state, NA_character_)
    )


# make chart --------------------------------------------------------------

ggplot(hwy_inv_data, aes(x = year, y = inv_per_capita, color = state)) +
    geom_line(size = 2) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_continuous(breaks = seq(1992, 2017, 3)) +
    gghighlight(state == "{closest_state}")
    labs(
        title = "{closest_state} Highway Infrastructure Investment Per Capita, 1992-2017",
        subtitle = "US Dollars, Millions (2012 Adjusted)",
        x = "",
        y = "",
        caption = "Source: United States Bureau of Economic Analysis"
    ) +
    theme_minimal() +
    theme(
        legend.position = "none"
    ) +
    transition_states(
        states = state,
        transition_length = 1,
        state_length = 2,
        wrap = TRUE
    )


