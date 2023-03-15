library(tidyverse)
library(spotifyr)
library(plotly)
library(compmus)
library(ggpubr)
metal <- get_playlist_audio_features("", "4jOlWW7XLRli6rjThfqlVl?si=781f9fafb8574da1") |> add_audio_analysis()
rock <- get_playlist_audio_features("", "43rPmb2v9YWmbotTymQLQE?si=fd6f70a6a61045a3") |> add_audio_analysis()
corpus <-
  bind_rows(
    rock |> mutate(genre = "Rock"),
    metal |> mutate(genre = "Metal")
  )
hist <-  
  ggplot(corpus, aes(x=10*round(tempo/10))) + 
  geom_histogram(stat="count", binwidth = 1, color = 'black', fill = 'blue') +
  facet_wrap(~ genre) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

ggplotly(p=hist)
