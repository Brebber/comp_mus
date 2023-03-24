library(tidyverse)
library(spotifyr)
library(plotly)
library(compmus)
library(ggpubr)
library(class)
library(caret)
library(corrplot)
library(ConfusionTableR)

metal <- get_playlist_audio_features("", "4jOlWW7XLRli6rjThfqlVl?si=781f9fafb8574da1") |> add_audio_analysis()
rock <- get_playlist_audio_features("", "43rPmb2v9YWmbotTymQLQE?si=fd6f70a6a61045a3") |> add_audio_analysis()
corpus <-
  bind_rows(
    rock |> mutate(genre = 0),
    metal |> mutate(genre = 1)
  )

corpus.subset <- as.data.frame(corpus[c('genre', 'danceability', 'energy', 'key', 'loudness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'track.duration_ms', 'track.popularity')])
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
corpus.subset.n <- as.data.frame(lapply(corpus.subset[,1:12], normalize))
correlationMatrix <- cor(corpus.subset.n)
print(correlationMatrix[,1])

data_cor <- cor(corpus.subset.n[ , colnames(corpus.subset.n) != "genre"],  # Calculate correlations
                corpus.subset.n$genre)
corrplot(correlationMatrix)                                          # Print correlation values
