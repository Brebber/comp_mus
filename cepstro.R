library(tidyverse)
library(spotifyr)
library(plotly)
library(compmus)
library(ggpubr)
metal <- get_playlist_audio_features("", "4jOlWW7XLRli6rjThfqlVl?si=781f9fafb8574da1")
rock <- get_playlist_audio_features("", "43rPmb2v9YWmbotTymQLQE?si=fd6f70a6a61045a3")
corpus <-
  bind_rows(
    rock |> mutate(genre = "Rock"),
    metal |> mutate(genre = "Metal")
  )
deliv <-
  get_tidy_audio_analysis("4siXNiLG9VJR6Z2kP6fFjv?si=f2a0dc8cf3eb4957") |> # Change URI.
  compmus_align(beats, segments) |>                     # Change `bars`
  select(beats) |>                                      #   in all three
  unnest(beats) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )

del_plt <- deliv |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c() +                              
  theme_classic() +
  ggtitle("Cepstrogram")

del_timbre <- deliv |>
  compmus_self_similarity(timbre, "cosine") |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "")+ 
  ggtitle("Timbre-based SSM") 


del_chroma <- deliv |>
  compmus_self_similarity(pitches, "cosine") |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "")+
  ggtitle("Chroma-based SSM")

ggarrange(del_timbre, del_chroma, del_plt, ncol = 2, nrow = 2)
