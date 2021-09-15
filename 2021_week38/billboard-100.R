# billboard-100.R
# TidyTuesday 2021 Week 38
# Alexander de Groot


# Environment Setup -------------------------------------------------------

library(tidyverse)


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
