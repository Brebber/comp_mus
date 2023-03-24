library(tidyverse)
library(spotifyr)
library(plotly)
library(compmus)
library(ggpubr)
library(class)
library(caret)
library(ConfusionTableR)
metal <- get_playlist_audio_features("", "4jOlWW7XLRli6rjThfqlVl?si=781f9fafb8574da1")
rock <- get_playlist_audio_features("", "43rPmb2v9YWmbotTymQLQE?si=fd6f70a6a61045a3")
corpus <-
  bind_rows(
    rock |> mutate(genre = 0),
    metal |> mutate(genre = 1)
  )

corpus.subset7 <- as.data.frame(corpus[c('genre', 'danceability', 'energy', 'acousticness', 'instrumentalness', 'valence', 'track.duration_ms', 'track.popularity')])
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
corpus.subset7.n <- as.data.frame(lapply(corpus.subset7[,2:8], normalize))

set.seed(123)
dat7.d <- sample(1:nrow(corpus.subset7.n),size=nrow(corpus.subset7.n)*0.7,replace = FALSE) #random selection of 70% data.
train.corpus7 <- corpus.subset7[dat.d,] # 70% training data
test.corpus7 <- corpus.subset7[-dat.d,] # remaining 30% test data
train.corpus7_labels <- corpus.subset7[dat.d,1]
test.corpus7_labels <-corpus.subset7[-dat.d,1]
knn.7 <- knn(train=train.corpus7, test=test.corpus7, cl=train.corpus7_labels, k=10)

confusionMatrix(table(knn.7 ,test.corpus7_labels))
