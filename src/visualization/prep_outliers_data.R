library(tidyverse)

# Read in track, album, and outlier dataframes

track_df = read_csv('~/tswift/data/tracks.csv')
album_df = read_csv('~/tswift/data/albums.csv')
outliers_df = read_csv('~/tswift/data/outliers.csv')

# Add outliers flags
viz_df <- track_df %>%
  select(
    track_name, track_length_ms, track_key, track_loudness, track_mode, 
    track_tempo, track_time_signature, album_name
  ) %>%
  mutate(
    outlier_track_length = if_else(
      condition = track_name %in% 
        outliers_df[outliers_df$variable == 'track_length_ms',]$track_name,
      true = TRUE,
      false = FALSE
    ),
    outlier_track_key = if_else(
      condition = track_name %in% 
        outliers_df[outliers_df$variable == 'track_key',]$track_name,
      true = TRUE,
      false = FALSE
    ),
    outlier_track_loudness = if_else(
      condition = track_name %in% 
        outliers_df[outliers_df$variable == 'track_loudness',]$track_name,
      true = TRUE,
      false = FALSE
    ),
    outlier_track_mode = if_else(
      condition = track_name %in% 
        outliers_df[outliers_df$variable == 'track_mode',]$track_name,
      true = TRUE,
      false = FALSE
    ),
    outlier_track_tempo = if_else(
      condition = track_name %in% 
        outliers_df[outliers_df$variable == 'track_tempo',]$track_name,
      true = TRUE,
      false = FALSE
    ),
    outlier_track_time_signature = if_else(
      condition = track_name %in% 
        outliers_df[outliers_df$variable == 'track_time_signature',]$track_name,
      true = TRUE,
      false = FALSE
    )
  ) %>% left_join(
    album_df[c('album_name', 'release_date')],
    by = 'album_name'
  )
