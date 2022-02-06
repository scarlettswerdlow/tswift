library(taylor)

# Make violen and dot plot

viz_df %>%
  ggplot(
    aes(
      x = reorder(album_name, release_date), 
      y = track_length_ms/1000
    )
  ) +
  geom_violin(
    aes(fill = album_name),
    alpha = 0.1,
    color = NA
  ) +
  geom_dotplot(
    aes(fill = album_name),
    alpha = 0.3,
    binaxis = 'y',
    color = NA,
    dotsize = .8,
    stackdir = 'center'
  ) +
  geom_dotplot(
    data = function(x) subset(x, outlier_track_length),
    aes(color = album_name),
    binaxis = 'y',
    dotsize = .8,
    fill = NA,
    stackdir = 'center',
    stroke = 1
  ) +
  geom_text( 
    data = function(x) subset(x, outlier_track_length & album_name != 'Red'),
    aes(
      label = paste(
        track_name, 
        format( as.POSIXct(Sys.Date()) + track_length_ms/1000, '(%M:%S)'), 
        ' '
      )
    ),
    family = 'Andale Mono',
    hjust = 'right',
    size = 6
  ) +
  geom_text( 
    data = function(x) subset(x, outlier_track_length & album_name == 'Red'),
    aes(
      label = paste(
        ' ', 
        track_name, 
        format( as.POSIXct(Sys.Date()) + track_length_ms/1000, '(%M:%S)')
      )
    ),
    family = 'Andale Mono',
    hjust = 'left',
    size = 6
  ) +
  ggtitle('Taylor Swift Song Length') +
  xlab('Album') +
  ylab('Song Length') +
  scale_y_time(labels = function(l) strftime(l, '%M:%S')) +
  scale_color_albums() +
  scale_fill_albums() +
  theme_minimal() +
  theme(
    legend.position = 'none',
    text = element_text(family = 'Andale Mono', size = 25),
    plot.title = element_text(margin = margin(10, 0, 10, 0)),
    axis.title.x = element_text(margin = margin(10, 0, 10, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 10))
  )

ggsave(
  '~/tswift/reports/figures/outliers_track_length.jpeg', 
  device = 'jpeg',
  width = 18,
  height = 12,
  units = 'in',
  dpi = 600
)
