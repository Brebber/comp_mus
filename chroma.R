library(tidyverse)
library(spotifyr)
library(compmus)
to_sirius <-
  get_tidy_audio_analysis("1BcuFfskHNf1WvqpyCs4wT?si=188253483ba54d55") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)
to_sirius |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

creep <-
  get_tidy_audio_analysis("70LcF31zb1H0PyJoS1Sx1r?si=b7cfbb858fd645ac") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)
creep |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()
