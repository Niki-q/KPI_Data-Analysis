source("cleaning.R")
library(qqplotr)
library(GGally)

data <- clean_date()

str(data)

summary(data)

box_plot <- function(aes_y, title_y, title_size, text_size) {
  ggplot(data, aes(y = aes_y)) +
    geom_boxplot() +
    labs(y = title_y) +
    theme(axis.title = element_text(size = title_size),
        axis.text = element_text(size = text_size))
}

box_plot(data$popularity, "Popularity of tracks", 25,20)
box_plot(data$valence, "Valence of tracks", 40,40)
box_plot(data$duration_ms, "Duration of tracks", 40,40)
box_plot(data$energy, "The energy of the tracks", 40,40)
box_plot(data$tempo, "Tempo tracks", 40,40)
box_plot(data$speechiness, "The speechiness of the tracks", 40,40)


Hampel_filter <- function (arg){
    data %>% filter(arg < median(arg) - 3 * mad(arg) |
    arg > median(arg) + 3 * mad(arg))
}

print(Hampel_filter(data$popularity) %>%
    arrange(desc(popularity)))

print(Hampel_filter(data$duration_ms) %>%
    arrange(desc(duration_ms)))

print(Hampel_filter(data$danceability) %>%
    arrange(desc(danceability)))

print(Hampel_filter(data$energy) %>%
    arrange(desc(energy)))

print(Hampel_filter(data$loudness) %>%
    arrange(desc(loudness)))

print(Hampel_filter(data$speechiness) %>%
    arrange(desc(speechiness)))

print(Hampel_filter(data$acousticness) %>%
    arrange(desc(acousticness)))

print(Hampel_filter(data$instrumentalness) %>%
    arrange(desc(instrumentalness)))

print(Hampel_filter(data$liveness) %>%
    arrange(desc(liveness)))

print(Hampel_filter(data$valence) %>%
    arrange(desc(valence)))

print(Hampel_filter(data$tempo) %>%
    arrange(desc(tempo)))

print(Hampel_filter(data$time_signature) %>%
    arrange(desc(time_signature)))

qq_plot_builder <- function (aes,  y_label){
  ggplot(data, aes) +
    stat_qq_point() + stat_qq_line() + stat_qq_band() +
    labs(x = "Quantiles of the normal distribution", y = y_label) +
    theme(axis.title = element_text(size = 25),
    axis.text = element_text(size = 20))
}

qq_plot_builder(aes(sample = popularity),"Popularity")
qq_plot_builder(aes(sample = valence),"Valence")
qq_plot_builder(aes(sample = duration_ms),"Duration, ms")
qq_plot_builder(aes(sample = energy),"Energy")
qq_plot_builder(aes(sample = tempo),"Tempo")
qq_plot_builder(aes(sample = speechiness),"Speechiness")

# Побудова гістограм для деяких неперервних змінних

diagram_plot_builder <- function (aes){
  data %>%
    ggplot(aes) +
    geom_histogram(color = "#e9ecef", alpha = 0.6, position = 'identity') +
    labs(fill = "")
}

diagram_plot_builder(aes(x = popularity))
diagram_plot_builder(aes(x = valence))
diagram_plot_builder(aes(x = duration_ms))
diagram_plot_builder(aes(x = energy))
diagram_plot_builder(aes(x = tempo))
diagram_plot_builder(aes(x = speechiness))


# Побудова гістограм та QQ-графіків прологаритмованих змінних

diagram_plot_builder(aes(x = log(popularity)))
qq_plot_builder(aes(sample = log(popularity)),"Popularity")

diagram_plot_builder(aes(x = log(valence)))
qq_plot_builder(aes(sample = log(valence)),"Valence")

diagram_plot_builder(aes(x = log(duration_ms)))
qq_plot_builder(aes(sample = log(duration_ms)),"Duration, ms")

diagram_plot_builder(aes(x = log(energy)))
qq_plot_builder(aes(sample = log(energy)),"Energy")

diagram_plot_builder(aes(x = log(tempo)))
qq_plot_builder(aes(sample = log(tempo)),"Tempo")

diagram_plot_builder(aes(x = log(speechiness)))
qq_plot_builder(aes(sample = log(speechiness)),"Speechiness")


most_popular_artists <-
    data %>%
    group_by(artists) %>%
    summarise(popularity = sum(popularity)) %>%
    arrange(desc(popularity)) %>%
    head(5)

print(most_popular_artists)

most_popular_albums <-
    data %>%
    group_by(album_name) %>%
    summarise(popularity = sum(popularity)) %>%
    arrange(desc(popularity)) %>%
    head(5)

print(most_popular_albums)

most_popular_genres <-
    data %>%
    group_by(track_genre) %>%
    summarise(popularity = sum(popularity)) %>%
    arrange(desc(popularity)) %>%
    head(5)

print(most_popular_genres)

ggcorr(data %>% select(where(is.numeric)), label = TRUE)

ggcorr(data %>% select(popularity, valence), label = TRUE)

ggcorr(data %>% select(duration_ms, energy), label = TRUE)

ggcorr(data %>% select(tempo, speechiness), label = TRUE)

data %>%
    ggplot(aes(x = explicit, y = speechiness)) +
    geom_bar(stat = "identity")

