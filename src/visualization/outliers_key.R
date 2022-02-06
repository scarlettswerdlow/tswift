library(taylor)

viz_df %>%
  mutate(
    # Recode track key based on https://en.wikipedia.org/wiki/Pitch_class
    track_key_recoded = recode(
      track_key,
      '0' = 'C',
      '1' = 'C♯, D♭',
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
  ) %>%
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
      color = album_name,
      size = n
    )
  ) +
  geom_point(alpha = 0.5) +
  geom_point(
    data = function(x) subset(x, n_outliers == 1), 
    shape = 1, 
    stroke = 1
  ) +
  geom_text(
    data = function(x) subset(x, n_outliers == 1),
    aes(label = str_wrap(tracks, width = 20)),
    color = 'black',
    family = 'Andale Mono',
    nudge_y = -0.25,
    size = 4,
    vjust = 'top',
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
  '~/tswift/reports/figures/outliers_track_key.jpeg', 
  device = 'jpeg',
  width = 18,
  height = 12,
  units = 'in',
  dpi = 600
)