# billboard-100.R
# TidyTuesday 2021 Week 38
# Alexander de Groot


# Environment Setup -------------------------------------------------------

library(tidyverse)
library(lubridate)


# Load Data ---------------------------------------------------------------

# Load data from TidyTuesday and then save to repo
# tuesdata <- tidytuesdayR::tt_load(2021, week = 38)
# 
# billboard_raw <- tuesdata$billboard
# write_csv(billboard_raw, here::here("2021_week38", "billboard_raw.csv"))


billboard_raw <- read_csv(
    file = here::here("2021_week38", "billboard_raw.csv"),
    col_types = cols(
        url = col_character(),
        week_id = col_character(),
        week_position = col_double(),
        song = col_character(),
        performer = col_character(),
        song_id = col_character(),
        instance = col_double(),
        previous_week_position = col_double(),
        peak_position = col_double(),
        weeks_on_chart = col_double()
    )
)


# Tidy Data ---------------------------------------------------------------

billboard_decades_top5 <- billboard_raw %>% 
    mutate(
        week_id = mdy(week_id),
        year = year(week_id),
        decade = case_when(
            between(year, 1950, 1959) ~ "1950s",
            between(year, 1960, 1969) ~ "1960s",
            between(year, 1970, 1979) ~ "1970s",
            between(year, 1980, 1989) ~ "1980s",
            between(year, 1990, 1999) ~ "1990s",
            between(year, 2000, 2009) ~ "2000s",
            between(year, 2010, 2019) ~ "2010s",
            between(year, 2020, 2029) ~ "2020s"
        )
    ) %>% 
    filter(between(year, 1960, 2019)) %>%
    group_by(decade, song_id, song, performer, instance) %>% 
    summarize(weeks_top_100 = n()) %>% 
    ungroup() %>% 
    arrange(decade, desc(weeks_top_100)) %>% 
    group_by(decade) %>% 
    slice(1) %>%
    ungroup()


# Make Chart --------------------------------------------------------------

billboard_decades_top100 <- billboard_decades_top5 %>% 
    mutate(
        chart_label_song = str_c('"', song, '"', "\n", performer),
        chart_label_weeks = str_c(weeks_top_100, " weeks")
    )


ggplot(
    billboard_decades_top100,
    aes(x = decade, y = weeks_top_100, fill = decade)
) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = chart_label_song, hjust = "right"), nudge_y = -0.75, size = 3, fontface = "italic") +
    geom_text(aes(label = chart_label_weeks, hjust = "left"), nudge_y = 0.75, size = 3, fontface = "italic") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 95)) +
    coord_flip() +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 12),
        axis.line = element_line(linetype = "solid"),
        axis.title.x = element_text(hjust = 0),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)
    ) +
    labs(
        title = "By decade, which song had the longest continuous streak on the Billboard 100?",
        x = "", y = "",
        caption = str_c(
            "Source: Data.World by way of Sean Miller, Billboard.com, and Spotify", " / ",
            "Chart by Alexander de Groot"
        )
    )

ggsave(here::here("2021_week38", "2021_week38_chart.png"))
