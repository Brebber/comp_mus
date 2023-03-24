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

corpus.subset3 <- as.data.frame(corpus[c('genre', 'valence', 'track.duration_ms', 'track.popularity')])
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
corpus.subset3.n <- as.data.frame(lapply(corpus.subset3[,2:4], normalize))

set.seed(123)
dat3.d <- sample(1:nrow(corpus.subset3.n),size=nrow(corpus.subset3.n)*0.7,replace = FALSE) #random selection of 70% data.
train.corpus3 <- corpus.subset3[dat.d,] # 70% training data
test.corpus3 <- corpus.subset3[-dat.d,] # remaining 30% test data
train.corpus3_labels <- corpus.subset3[dat.d,1]
test.corpus3_labels <-corpus.subset3[-dat.d,1]
knn.3 <- knn(train=train.corpus3, test=test.corpus3, cl=train.corpus3_labels, k=8)

confusionMatrix(table(knn.3 ,test.corpus3_labels))

glam <- get_playlist_audio_features("", "1DAC3LdsyquzqziE5Ucja1?si=56748b38d9164813")|> mutate(genre = 2)
glam.subset <- as.data.frame(glam[c('genre', 'valence', 'track.duration_ms', 'track.popularity')])
glam.subset.n <- as.data.frame(lapply(glam.subset[,2:4], normalize))
test.glam <- glam.subset[1:10,]
glam.results <- knn(train=train.corpus3, test=test.glam, cl=train.corpus3_labels, k=8)
fin_glam <- glam |> mutate(
  genre = "Glam", 
  results = glam.results,
  pred_genre = ifelse(results == 0, "Rock", "Metal"))

grun <- get_playlist_audio_features("", "1rWZDin7auum63WpMfva5j?si=e0ad2138bac14ee7")|> mutate(genre = 2)
grun.subset <- as.data.frame(grun[c('genre', 'valence', 'track.duration_ms', 'track.popularity')])
grun.subset.n <- as.data.frame(lapply(grun.subset[,2:4], normalize))
test.grun <- grun.subset[1:10,]
grun.results <- knn(train=train.corpus3, test=test.grun, cl=train.corpus3_labels, k=8)
fin_grun <- grun |> mutate(
  genre = "Grunge",
  results = grun.results,
  pred_genre = ifelse(results == 0, "Rock", "Metal"))

nu <- get_playlist_audio_features("", "1OVisrN9f3HCJ92DSnRAbd?si=354a6ac523564826")|> mutate(genre = 2)
nu.subset <- as.data.frame(nu[c('genre', 'valence', 'track.duration_ms', 'track.popularity')])
nu.subset.n <- as.data.frame(lapply(nu.subset[,2:4], normalize))
test.nu <- nu.subset[1:10,]
nu.results <- knn(train=train.corpus3, test=test.nu, cl=train.corpus3_labels, k=8)
fin_nu <- nu |> mutate(
  genre = "Nu-Metal",
  results = nu.results,
  pred_genre = ifelse(results == 0, "Rock", "Metal"))

new_corpus <- bind_rows(fin_glam, fin_grun, fin_nu)

plot_1 <- new_corpus |>                    # Start with corpus.
  mutate(
    name = new_corpus$track.name
  ) |>
  ggplot(                     # Set up the plot.
    aes(
      name = name,
      x = valence,
      y = track.popularity,
      size = track.duration_ms,
      colour = pred_genre
    )
  ) +
  geom_point() +              # Scatter plot.
  geom_rug(linewidth = 0.1) + # Add 'fringes' to show data distribution.

  facet_wrap(~ genre) +    # Separate charts per playlist.
  scale_x_continuous(         # Fine-tune the x axis.
    limits = c(0.1, 0.8),
    breaks = c(0, 0.5, 1),   # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(0, 100),
    breaks = c(0, 50, 100),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(        # Use the Color Brewer to choose a palette.
    type = "qual",            # Qualitative set.
    palette = "Set1"        # Name of the palette is 'Paired'.
  ) +
  theme_minimal() +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Popularity",
    colour = "Predicted Genre"
  )
knn_res_plot <- ggplotly(p = plot_1)
saveRDS(object = knn_res_plot, file = "data/knn_res_plot.RDS")
