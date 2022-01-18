library(tidyverse)
library(scales)
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

# Bin tempo
track_df <- track_df %>%
  mutate(
    track_tempo_binned = cut(
      track_tempo,
      breaks = c(0, 20, 40, 60, 66, 76, 108, 120, 168, 200, Inf),
      labels = c('Larghissimo', 'Grave', 'Largo', 'Larghetto', 'Adagio', 
                 'Andante', 'Moderato', 'Allegro', 'Presto', 'Prestissimo'),
      include.lowest = TRUE
    )
  )

# Add column that is true if track is an outlier on track tempo
track_df$outlier_track_tempo <- if_else(
  condition = track_df$track_name %in% 
    outliers_df[outliers_df$variable == 'track_tempo',]$track_name,
  true = TRUE,
  false = FALSE
)

track_df %>%
  # Identify median to set alpha
  group_by(track_tempo_binned) %>%
  mutate(
    alpha = if_else(
      condition = track_tempo == quantile(track_tempo, p = 0.5, type = 3),
      true = 0.8,
      false = 0.6
    )
  ) %>%
  ggplot(aes(x = track_tempo_binned, y = track_tempo, color = track_tempo_binned)) +
  geom_point(aes(alpha = alpha), size = 12) +
  geom_point(
    data = function(x) subset(x, outlier_track_tempo),
    aes(color = track_tempo_binned),
    shape = 1,
    size = 12
  ) +
  geom_text(
    data = function(x) subset(x, outlier_track_tempo),
    aes(label = str_wrap(track_name, width = 12)),
    family = 'Andale Mono',
    size = 6,
    vjust = 'top',
    nudge_y = - 0.05
  ) +
  scale_color_taylor_d(album = 'Red', direction = -1) +
  ggtitle('Taylor Swift Song Tempo') +
  xlab('Track Tempo') +
  ylab('Beats Per Minute') +
  guides(color = 'none', alpha = 'none') +
  theme_minimal() +
  theme(
    text = element_text(family = 'Andale Mono', size = 25),
    plot.title = element_text(margin = margin(10, 0, 10, 0)),
    axis.title.x = element_text(margin = margin(10, 0, 10, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 10))
  )
