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

# Function to get track names of outliers on arbitrary feature
get_outliers <- function(df, feature_name) {
  
  outliers <- outliers_df[
    outliers_df$variable == feature_name,
  ]$track_name
  
  return(outliers)
  
}

# Function to create plot with outliers on arbitrary feature labeled
make_outlier_plot <- function(track_df, outlier_df, feature, feature_name) {
  
  outliers <- get_outliers(outlier_df, feature_name)
  
  label <- str_to_title(str_replace(feature_name, 'track_', 'Song '))
  
  track_df$outlier <- if_else(
    condition = track_df$track_name %in% outliers,
    true = TRUE,
    false = FALSE
  )
  
  plt <- track_df %>%
    # Identify median to set alpha
    group_by(album_name) %>%
    mutate(
      alpha = if_else(
        condition = {{feature}} == quantile({{feature}}, p = 0.5, type = 3),
        true = 0.8,
        false = 0.6
      )
    ) %>%
    ggplot(
      aes(
        x = fct_reorder(album_name, release_date),
        y = {{feature}}, 
        color = album_name
      )
    ) +
    geom_point(
      aes(alpha = alpha),
      size = 12
    ) +
    geom_point(
      data = function(x) subset(x, outlier),
      aes(color = album_name),
      shape = 1,
      size = 12
    ) +
    scale_y_continuous(
      labels = label_percent(suffix = ''),
      limits = c(0, 1)
    ) +
    scale_color_albums(name = 'Album') +
    scale_fill_albums(name = 'Album') +
    ggtitle(paste('Taylor Swift', label)) +
    xlab('Album') +
    ylab(label) +
    guides(color = 'none', fill = 'none', alpha = 'none') +
    theme_minimal() +
    theme(
      text = element_text(family = 'Andale Mono', size = 25),
      plot.title = element_text(margin = margin(10, 0, 10, 0)),
      axis.title.x = element_text(margin = margin(10, 0, 10, 0)),
      axis.title.y = element_text(margin = margin(0, 10, 0, 10))
    )
  
  return(plt)
  
}

# Energy plot
plt_energy <- make_outlier_plot(
  track_df = track_df, 
  outlier_df = outlier_df, 
  feature = track_energy, 
  feature_name = 'track_energy'
)

plt_energy + geom_text(
  data = subset(
    left_join(outliers_df, track_df, by = 'track_name'), 
    variable == 'track_energy' & str_starts(track_name, 'Soon', TRUE)
  ),
  aes(label = str_wrap(track_name, width = 12)),
  family = 'Andale Mono',
  size = 6,
  vjust = 'top',
  nudge_y = -0.03
) +
  geom_text(
    data = subset(
      left_join(outliers_df, track_df, by = 'track_name'), 
      variable == 'track_energy' & str_starts(track_name, 'Soon')
    ),  aes(label = str_wrap(track_name, width = 12)),
  family = 'Andale Mono',
  size = 6,
  vjust = 'bottom',
  nudge_y = 0.03
) +
  labs(subtitle = 'Measure of Intensity and Activity')

ggsave(
  'tswift/reports/figures/outliers_track_energy_point.jpeg', 
  device = 'jpeg',
  width = 18,
  height = 12,
  units = 'in',
  dpi = 600
)

# Valence plot
plt_valence <- make_outlier_plot(
  track_df = track_df, 
  outlier_df = outlier_df, 
  feature = track_valence, 
  feature_name = 'track_valence'
)

plt_valence + 
  geom_text(
  data = subset(
    left_join(outliers_df, track_df, by = 'track_name'), 
    variable == 'track_valence'
  ),
  aes(label = str_wrap(track_name, width = 12)),
  family = 'Andale Mono',
  size = 6,
  vjust = 'bottom',
  nudge_y = 0.05
) +
  labs(subtitle = 'Measure of Positiveness')

ggsave(
  'tswift/reports/figures/outliers_track_valence_point.jpeg', 
  device = 'jpeg',
  width = 18,
  height = 12,
  units = 'in',
  dpi = 600
)

  
