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
# 
# audio_features_raw <- tuesdata$audio_features
# write_csv(audio_features_raw, here::here("2021_week38", "audio_features_raw.csv"))

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

audio_features_raw <- read_csv(
    file = here::here("2021_week38", "audio_features_raw.csv"),
    col_types = cols(
        song_id = col_character(),
        performer = col_character(),
        song = col_character(),
        spotify_genre = col_character(),
        spotify_track_id = col_character(),
        spotify_track_preview_url = col_character(),
        spotify_track_duration_ms = col_double(),
        spotify_track_explicit = col_logical(),
        spotify_track_album = col_character(),
        danceability = col_double(),
        energy = col_double(),
        key = col_double(),
        loudness = col_double(),
        mode = col_double(),
        speechiness = col_double(),
        acousticness = col_double(),
        instrumentalness = col_double(),
        liveness = col_double(),
        valence = col_double(),
        tempo = col_double(),
        time_signature = col_double(),
        spotify_track_popularity = col_double()
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
        ),
        song_performer = str_c(song, performer, sep = " by ")
    ) %>% 
    filter(instance == 1, week_position <= 5, between(year, 1960, 2019)) %>%
    group_by(decade, song_id, song, performer, song_performer) %>% 
    summarize(weeks_top_5 = n()) %>% 
    ungroup() %>% 
    arrange(decade, desc(weeks_top_5)) %>% 
    group_by(decade) %>% 
    slice(1) %>% 
    ungroup()

# Make Chart --------------------------------------------------------------

ggplot(billboard_decades_top5, aes(x = decade, y = weeks_top_5, fill = decade)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    theme_minimal() +
    labs(
        title = "Songs with the longest initial streak in the top 5 on the Billboard 100 by decade",
        x = "Decade", y = "Weeks",
        caption = "Source: Data.World by way of Sean Miller, Billboard.com and Spotify"
    )
