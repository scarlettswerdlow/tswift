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

# Add column that is true if track is an outlier on track key

track_df$outlier_track_key <- if_else(
  condition = track_df$track_name %in% 
    outliers_df[outliers_df$variable == 'track_key',]$track_name,
  true = TRUE,
  false = FALSE
)

# Recode track key based on https://en.wikipedia.org/wiki/Pitch_class

track_df$track_key_recoded <- recode(
  track_df$track_key,
  '0' = 'C',
  '1' = 'C♯, D\u266d',
  '2' = 'D',
  '3' = 'D♯, E♭',
  '4' = 'E',
  '5' = 'F',
  '6' = 'F♯, G♭',
  '7' = 'G',
  '8' = 'G♯, A♭',
  '9' = 'A',
  '10' = 'A♯, B♭',
  '11' = 'B'
)

# Make radial histogram

plot_data <- track_df %>%
  count(album_name, release_date, track_key_recoded)

label_data <- track_df %>%
  group_by(track_key, track_key_recoded) %>%
  summarize(
    n_outliers = sum(outlier_track_key),
    tracks = ifelse(n_outliers > 0 & n_outliers < 6, paste(track_name, collapse = ", "), '')
  ) %>%
  mutate(
    angle = 180 - 360 * (track_key + 0.5) / 12,
    hjust = ifelse(angle < -90, 0, 1),
    angle = ifelse(angle < -90, angle + 180, angle)
  )

plot_data %>%
  ggplot(aes(x = track_key_recoded)) +
  geom_col(aes(y = n, fill = fct_reorder(album_name, release_date, .desc = TRUE))) +
  scale_color_albums() +
  scale_fill_albums(name = 'Album') +
  coord_polar(start = 0) +
  geom_text(
    data = label_data,
    aes(y = n_outliers + 1, label = str_wrap(tracks, width = 35), angle = angle, hjust = hjust),
    family = 'Andale Mono',
    size = 2.5
  ) +
  scale_y_continuous(
    limits = c(-5, 25),
    expand = c(0, 0),
    breaks = c(0, 5, 10, 15, 20, 25)
  ) + 
  ggtitle('Taylor Swift Song Keys') +
  theme_minimal() +
  theme(
    text = element_text(family = 'Andale Mono', size = 14),
    axis.text.x = element_text(family = 'Arial Unicode MS'),
    axis.text.y = element_blank(),
    plot.title = element_text(margin = margin(0, 0, 10, 0)),
    axis.title = element_blank()
  )

# Point plot

track_df %>%
  group_by(album_name, release_date, track_key_recoded) %>%
  summarize(
    n = n(),
    n_outliers = sum(outlier_track_key),
    tracks = paste(track_name, collapse = ", ")
  ) %>%
  ggplot(
    aes(
      x = fct_reorder(album_name, release_date), 
      y = track_key_recoded, 
      size = n
    )
  ) +
  geom_point(aes(color = album_name)) +
  geom_text(
    data = function(x) subset(x, n_outliers == 1),
    family = 'Andale Mono',
    nudge_y = -0.25,
    size = 4,
    vjust = 'top',
    aes(label = str_wrap(tracks, width = 20))
  ) +
  scale_size_continuous(range = c(5, 30)) +
  scale_color_albums() +
  ggtitle('Taylor Swift Song Keys') +
  xlab('Album') +
  ylab('Song Key') +
  guides(color = 'none', size = 'none') +
  theme_minimal() +
  theme(
    text = element_text(family = 'Andale Mono', size = 25),
    axis.text.y = element_text(family = 'Arial Unicode MS'),
    plot.title = element_text(margin = margin(10, 0, 10, 0)),
    axis.title.x = element_text(margin = margin(10, 0, 10, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 10))
  )

ggsave(
  'tswift/reports/figures/outliers_track_key_point.jpeg', 
  device = 'jpeg',
  width = 18,
  height = 12,
  units = 'in',
  dpi = 600
)
