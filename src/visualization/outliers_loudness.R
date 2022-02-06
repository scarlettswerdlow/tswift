library(taylor)

viz_df %>%
  # Identify median to set alpha
  group_by(album_name) %>%
  mutate(
    alpha = if_else(
      condition = track_loudness == quantile(track_loudness, p = 0.5, type = 3),
      true = 0.8,
      false = 0.6
    )
  ) %>%
  ggplot(
    aes(
      x = reorder(album_name, release_date), 
      y = track_loudness, 
      color = album_name
    )
  ) +
  geom_point(aes(alpha = alpha), size = 20) +
  geom_point(
    data = function(x) subset(x, outlier_track_loudness),
    shape = 1,
    size = 20
  ) +
  geom_text(
    data = function(x) subset(x, outlier_track_loudness & track_name != 'peace'),
    aes(label = str_wrap(track_name, width = 12)),
    color = 'black',
    family = 'Andale Mono',
    size = 6,
    vjust = 'top',
    nudge_y = -0.55
  ) +
  geom_text(
    data = function(x) subset(x, outlier_track_loudness & track_name == 'peace'),
    aes(label = str_wrap(track_name, width = 12)),
    color = 'black',
    family = 'Andale Mono',
    size = 6,
    vjust = 'bottom',
    nudge_y = 0.55
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

ggsave(
  '~/tswift/reports/figures/outliers_track_loudness.jpeg', 
  device = 'jpeg',
  width = 18,
  height = 12,
  units = 'in',
  dpi = 600
)
