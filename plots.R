library(tidyverse)
library(spotifyr)
library(plotly)
library(compmus)
library(ggpubr)
metal <- get_playlist_audio_features("", "4jOlWW7XLRli6rjThfqlVl?si=781f9fafb8574da1") |> add_audio_analysis()
rock <- get_playlist_audio_features("", "43rPmb2v9YWmbotTymQLQE?si=fd6f70a6a61045a3") |> add_audio_analysis()

metal_stats <- metal |> 
  summarise(
    mean_valence = mean(metal$valence),
    mean_energy = mean(metal$energy),
    mean_dance = mean(metal$danceability),
    mean_tempo = mean(metal$tempo),
    mean_loudness = mean(metal$loudness),
    mean_pop = mean(metal$track.popularity)
    )
rock_stats <- rock |> 
  summarise(
    mean_valence = mean(rock$valence),
    mean_energy = mean(rock$energy),
    mean_dance = mean(rock$danceability),
    mean_tempo = mean(rock$tempo),
    mean_loudness = mean(rock$loudness),
    mean_pop = mean(rock$track.popularity)
    )

corp_stats <-
  bind_rows(
    rock_stats |> mutate(genre = "Rock"),
    metal_stats |> mutate(genre = "Metal")
)
corpus <-
  bind_rows(
    rock |> mutate(genre = "Rock"),
    metal |> mutate(genre = "Metal")
  )

plot_2 <- corpus |>                    # Start with corpus.
  mutate(
    name = corpus$track.name,
    popularity = corpus$track.popularity,
    duration = track.duration_ms,
    time_signature = ifelse(time_signature == 4, "Even", "Odd")
  ) |>
  ggplot(                     # Set up the plot.
    aes(
      name = name,
      x = popularity,
      y = tempo,
      size = duration/60,
      colour = time_signature
    )
  ) +
  geom_point() +              # Scatter plot.
  geom_rug(linewidth = 0.1) + # Add 'fringes' to show data distribution.
  geom_vline(aes(xintercept = mean_pop), corp_stats, color = "purple", linewidth = 0.2, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean_tempo), corp_stats, color = "purple", linewidth = 0.2, show.legend = TRUE) +
  facet_wrap(~ genre) +    # Separate charts per playlist.
  scale_x_continuous( # Fine-tune the x axis.
    limits = c(0, 100),
    breaks = c(0, 50, 100),   # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(60, 200),
    breaks = c(60, 130, 200),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(        # Use the Color Brewer to choose a palette.
    type = "qual",            # Qualitative set.
    palette = "Set1"        # Name of the palette is 'Paired'.
  ) +
  theme_minimal() +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Popularity",
    y = "Tempo",
    colour = "Time signature",
    size = ''
  )
ggplotly(p = plot_2)


