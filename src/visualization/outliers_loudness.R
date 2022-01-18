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

# Add column that is true if track is an outlier on track loudness
track_df$outlier_track_loudness <- if_else(
  condition = track_df$track_name %in% 
    outliers_df[outliers_df$variable == 'track_loudness',]$track_name,
  true = TRUE,
  false = FALSE
)

track_df %>%
  # Identify median to set alpha
  group_by(album_name) %>%
  mutate(
    alpha = if_else(
      condition = track_loudness == quantile(track_loudness, p = 0.5, type = 3),
      true = 0.8,
      false = 0.6
    )
  ) %>%
  ggplot(aes(x = reorder(album_name, release_date), y = track_loudness, color = album_name)) +
  geom_point(aes(alpha = alpha), size = 12) +
  geom_point(
    data = function(x) subset(x, outlier_track_loudness),
    shape = 1,
    size = 12
  ) +
  geom_text(
    data = function(x) subset(x, outlier_track_loudness),
    aes(label = str_wrap(track_name, width = 12)),
    family = 'Andale Mono',
    size = 6,
    vjust = 'top',
    nudge_y = - 0.05
  ) +
  scale_color_albums(name = 'Album') +
  scale_fill_albums(name = 'Album') +
  ggtitle('Taylor Swift Song Loudness') +
  xlab('Album') +
  ylab('Decibles') +
  guides(color = 'none', fill = 'none', alpha = 'none') +
  theme_minimal() +
  theme(
    text = element_text(family = 'Andale Mono', size = 25),
    plot.title = element_text(margin = margin(10, 0, 10, 0)),
    axis.title.x = element_text(margin = margin(10, 0, 10, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 10))
  )
