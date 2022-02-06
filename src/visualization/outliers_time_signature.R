library(taylor)

viz_df %>%
  # Fix data errors
  mutate(
    track_time_signature = if_else(
      condition = track_name == 'Last Kiss',
      true = 3,
      false = track_time_signature
    ),
    track_time_signature = if_else(
      condition = track_name == 'evermore (feat. Bon Iver)',
      true = 4,
      false = track_time_signature
    )
  ) %>% 
  group_by(album_name, release_date) %>%
  summarize(
    n_outliers = length(track_name[track_time_signature != 4]),
    tracks = if_else(
      condition = n_outliers != 0,
      true = paste(
        paste0(
          track_name[track_time_signature != 4], 
          ' (', track_time_signature[track_time_signature != 4], ')'
        ), 
        collapse = '\n'),
      false = ''
    )
  ) %>%
  ggplot(
    aes(
      x = fct_reorder(album_name, release_date), 
      y = n_outliers, 
      color = album_name
    )
  ) +
  geom_segment(
    aes(
      xend = fct_reorder(album_name, release_date), 
      y = 0,
      yend = n_outliers
    ), 
    color = 'black'
  ) +
  geom_point(alpha = 0.5, size = 20) +
  geom_text(
    data = function(x) subset(x, album_name != 'Red'),
    aes(label = tracks),
    color = 'black',
    family = 'Andale Mono',
    size = 6,
    hjust = 'right',
    nudge_x = -0.2
  ) +
  geom_text(
    data = function(x) subset(x, album_name == 'Red'),
    aes(label = tracks),
    color = 'black',
    family = 'Andale Mono',
    size = 6,
    hjust = 'left',
    nudge_x = 0.2
  ) +
  scale_y_continuous(breaks = c(0, 1, 2)) +
  scale_color_albums() +
  ggtitle('Taylor Swift Song Time Signatures') +
  xlab('Album') +
  ylab('Number of Songs Not in 4 Time Signature') +
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
  '~/tswift/reports/figures/outliers_track_time_signature.jpeg', 
  device = 'jpeg',
  width = 18,
  height = 12,
  units = 'in',
  dpi = 600
)
