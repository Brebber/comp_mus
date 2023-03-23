library(tidyverse)
library(spotifyr)
library(plotly)
library(compmus)
library(ggpubr)
library(class)
library(caret)
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
corpus.subset.n <- as.data.frame(lapply(corpus.subset[,2:12], normalize))

set.seed(123)
dat.d <- sample(1:nrow(corpus.subset.n),size=nrow(corpus.subset.n)*0.7,replace = FALSE) #random selection of 70% data.
train.corpus <- corpus.subset[dat.d,] # 70% training data
test.corpus <- corpus.subset[-dat.d,] # remaining 30% test data
train.corpus_labels <- corpus.subset[dat.d,1]
test.corpus_labels <-corpus.subset[-dat.d,1]
knn.10 <- knn(train=train.corpus, test=test.corpus, cl=train.corpus_labels, k=10)

confusionMatrix(table(knn.10 ,test.corpus_labels))

glam <- get_playlist_audio_features("", "1DAC3LdsyquzqziE5Ucja1?si=56748b38d9164813")|> mutate(genre = 2)
glam.subset <- as.data.frame(glam[c('genre', 'danceability', 'energy', 'key', 'loudness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'track.duration_ms', 'track.popularity')])
glam.subset.n <- as.data.frame(lapply(glam.subset[,2:12], normalize))
test.glam <- glam.subset[1:10,]
glam.results <- knn(train=train.corpus, test=test.glam, cl=train.corpus_labels, k=10)
glam.results

grun <- get_playlist_audio_features("", "1rWZDin7auum63WpMfva5j?si=e0ad2138bac14ee7")|> mutate(genre = 2)
grun.subset <- as.data.frame(grun[c('genre', 'danceability', 'energy', 'key', 'loudness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'track.duration_ms', 'track.popularity')])
grun.subset.n <- as.data.frame(lapply(grun.subset[,2:12], normalize))
test.grun <- grun.subset[1:10,]
grun.results <- knn(train=train.corpus, test=test.grun, cl=train.corpus_labels, k=10)
grun.results

nu <- get_playlist_audio_features("", "1OVisrN9f3HCJ92DSnRAbd?si=354a6ac523564826")|> mutate(genre = 2)
nu.subset <- as.data.frame(nu[c('genre', 'danceability', 'energy', 'key', 'loudness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'track.duration_ms', 'track.popularity')])
nu.subset.n <- as.data.frame(lapply(nu.subset[,2:12], normalize))
test.nu <- nu.subset[1:10,]
nu.results <- knn(train=train.corpus, test=test.nu, cl=train.corpus_labels, k=10)
nu.results

binary_visualiseR(train_labels = knn.10,
                 truth_labels = factor(test.corpus_labels, levels = c("0", "1")),
                 class_label1 = "Rock",
                 class_label2 = "Metal",
                 quadrant_col1 = "#28ACB4",
                 quadrant_col2 = "#4397D2",
                 custom_title = "KNN Confusion Matrix",
                 cm_stat_size = 1,
                 cm_stat_lbl_size = 1,
                 text_col = 'black')
