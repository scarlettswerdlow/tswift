library(tidyverse)
library(taylor)

# Read in track and outlier data

track_df = read_csv('tswift/data/tracks.csv')
album_df = read_csv('tswift/data/albums.csv')
outliers_df = read_csv('tswift/data/outliers.csv')

# Merge track and album data

track_df <- left_join(
  track_df,
  album_df, 
  by = 'album_name'
)

# Add column that is true if track is an outlier on track length

track_df$outlier_track_length <- if_else(
  condition = track_df$track_name %in% 
    outliers_df[outliers_df$variable == 'track_length_ms',]$track_name,
  true = TRUE,
  false = FALSE
)

# Make violen and dot plot

track_df %>%
  ggplot(
    aes(
      x = reorder(album_name, release_date), 
      y = track_length_ms/1000
    )
  ) +
  geom_violin(
    alpha = .3,
    aes(color = album_name, fill = album_name)
  ) +
  geom_dotplot(
    binaxis = 'y',
    dotsize = .6,
    fill = NA,
    stackdir = 'center',
    aes(color = album_name)
  ) +
  geom_dotplot(
    data = track_df[track_df$outlier_track_length,],
    binaxis = 'y',
    color = 'black',
    dotsize = .6,
    fill = 'black',
    stackdir = 'center',
  ) +
  geom_text( 
    data = track_df[track_df$outlier_track_length & track_df$album_name != 'Red',],
    family = 'Andale Mono',
    hjust = 'right',
    aes(
      label = paste(
        track_name, 
        format( as.POSIXct(Sys.Date()) + track_length_ms/1000, '(%M:%S)'), 
        ' '
      )
    ),
  ) +
  geom_text( 
    data = track_df[track_df$outlier_track_length & track_df$album_name == 'Red',],
    family = 'Andale Mono',
    hjust = 'left',
    aes(
      label = paste(
        ' ', 
        track_name, 
        format( as.POSIXct(Sys.Date()) + track_length_ms/1000, '(%M:%S)')
      )
    )
  ) +
  ggtitle('Longest and Shortest Taylor Swift Songs') +
  xlab('Album') +
  ylab('Song Length') +
  scale_y_time(labels = function(l) strftime(l, '%M:%S')) +
  scale_color_albums() +
  scale_fill_albums() +
  theme_minimal() +
  theme(
    legend.position = 'none',
    text = element_text(family = 'Andale Mono', size = 14),
    plot.title = element_text(margin = margin(10, 0, 10, 0)),
    axis.title.x = element_text(margin = margin(10, 0, 10, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 10))
  )
