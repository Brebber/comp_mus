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
    mean_tempo = mean(metal$tempo)
    )
rock_stats <- rock |> 
  summarise(
    mean_valence = mean(rock$valence),
    mean_energy = mean(rock$energy),
    mean_dance = mean(rock$danceability),
    mean_tempo = mean(rock$tempo)
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

plt_3 <- corpus |>
  mutate(
    popularity = track.popularity,
    name = track.name,
    sections =
      map(
        sections,                                    # sections or segments
        summarise_at,
        vars(tempo, duration),             # features of interest
        list(section_mean = mean, section_sd = sd)   # aggregation functions
      )
  ) |>
  unnest(sections) |>
  ggplot(
    aes(
      name = name,
      x = tempo,
      y = tempo_section_sd,
      colour = genre
    )
  ) +
  geom_point(aes(size = duration / 60)) +
  geom_rug() +
  theme_minimal() +
  ylim(0, 5) +
  labs(
    x = "Mean Tempo (bpm)",
    y = "SD Tempo",
    colour = "Genre",
    size = "Duration (min)"
  )
ggplotly(p=plt_3)
