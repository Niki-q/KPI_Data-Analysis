source("cleaning.R")
library(qqplotr)
library(GGally)

# Завантаження попередньо очищених даних 

data <- clean_date()

# Перегляд основної інформації про датасет

str(data)

# Перегляд основних статистик для кожної змінної датасету

summary(data)

# Побудова boxplots для змінних, що нас цікавлять

box_plot <- function(aes_y, title_y, title_size, text_size) {
  ggplot(data, aes(y = aes_y)) +
    geom_boxplot() +
    labs(y = title_y) +
    theme(axis.title = element_text(size = title_size),
        axis.text = element_text(size = text_size))
}

box_plot(data$popularity, "Popularity of tracks", 15, 15)
box_plot(data$valence, "Valence of tracks", 15,15)

data %>%
  mutate(duration_m = duration_ms / 60000) %>%
  select(duration_m) -> duration_m

box_plot(duration_m$duration_m, "Duration of tracks", 15,15)

box_plot(data$energy, "The energy of the tracks", 15,15)
box_plot(data$tempo, "Tempo tracks", 15,15)
box_plot(data$speechiness, "The speechiness of the tracks", 15,15)

# Перевірка викидів за допомогою функції Гампеля

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

# Виведення QQ plots для змінних, що нас цікавлять

qq_plot_builder <- function (aes,  y_label){
  ggplot(data, aes) +
    stat_qq_point() + stat_qq_line() + stat_qq_band() +
    labs(x = "Quantiles of the normal distribution", y = y_label) +
    theme(axis.title = element_text(size = 15),
    axis.text = element_text(size = 15))
}

qq_plot_builder(aes(sample = popularity),"Popularity")
qq_plot_builder(aes(sample = valence),"Valence")
qq_plot_builder(aes(sample = duration_ms),"Duration, ms")
qq_plot_builder(aes(sample = energy),"Energy")
qq_plot_builder(aes(sample = tempo),"Tempo")
qq_plot_builder(aes(sample = speechiness),"Speechiness")

# Побудова гістограм для деяких неперервних змінних

histogram_plot_builder <- function (aes){
  data %>%
    ggplot(aes) +
    geom_histogram(color = "#e9ecef", alpha = 0.6, position = 'identity') +
    labs(fill = "")
}

histogram_plot_builder(aes(x = popularity))
histogram_plot_builder(aes(x = valence))
histogram_plot_builder(aes(x = duration_ms))
histogram_plot_builder(aes(x = energy))
histogram_plot_builder(aes(x = tempo))
histogram_plot_builder(aes(x = speechiness))


# Побудова гістограм та QQ-графіків прологаритмованих змінних

histogram_plot_builder(aes(x = log(popularity)))
qq_plot_builder(aes(sample = log(popularity)),"Popularity")

histogram_plot_builder(aes(x = log(valence)))
qq_plot_builder(aes(sample = log(valence)),"Valence")

histogram_plot_builder(aes(x = log(duration_ms)))
qq_plot_builder(aes(sample = log(duration_ms)),"Duration, ms")

histogram_plot_builder(aes(x = log(energy)))
qq_plot_builder(aes(sample = log(energy)),"Energy")

histogram_plot_builder(aes(x = log(tempo)))
qq_plot_builder(aes(sample = log(tempo)),"Tempo")

histogram_plot_builder(aes(x = log(speechiness)))
qq_plot_builder(aes(sample = log(speechiness)),"Speechiness")

# Виведення найбільш популярних об'єктів в залежності від певних харакеристик

most_popular_artists <-
    data %>%
    group_by(artists) %>%
    summarise(popularity = sum(popularity)) %>%
    arrange(desc(popularity)) %>%
    head(5)

print(most_popular_artists)

ggplot(most_popular_artists, aes(x=artists, y=popularity, fill = artists)) + 
  geom_bar(stat = 'identity' ) + coord_flip()

most_popular_albums <-
    data %>%
    group_by(album_name) %>%
    summarise(popularity = sum(popularity)) %>%
    arrange(desc(popularity)) %>%
    head(20)

print(most_popular_albums)

# Перегляд most_popular_albums за допомгою lollipop графіка 

ggplot(most_popular_albums, aes(x=album_name, y=popularity)) +
  geom_segment( aes(x=album_name, xend=album_name, y=0, yend=popularity), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

data %>% filter(album_name == 'Un Verano Sin Ti') %>% print(n=56)

most_popular_genres <-
    data %>%
    group_by(track_genre) %>%
    summarise(popularity = sum(popularity)) %>%
    arrange(desc(popularity)) %>%
    head(5)

print(most_popular_genres)

ggplot(most_popular_genres, aes(x=track_genre, y=popularity, fill = track_genre)) + 
  geom_bar(stat = 'identity' ) + coord_flip()

# Підготовка даних для відображення violin plot

five_genres = data$track_genre[data$track_genre %in% c('pop-film', 'k-pop', 'chill', 'sad', 'grunge')]
data_filtered_by_genre = data[data$track_genre %in% c('pop-film', 'k-pop', 'chill', 'sad', 'grunge'),]

ggplot(data_filtered_by_genre, aes(x=five_genres, y=popularity, fill=five_genres)) +
  geom_violin()

# Графік кореляціїї числових неперервних змінних

ggcorr(data %>% select(where(is.numeric)), label = TRUE)

histogram_plot_builder(aes(x = loudness))
qq_plot_builder(aes(sample=loudness), "Loudness")
box_plot(data$loudness, "The loudness of the tracks", 15,15)

# Перегляд залежності змінної energy та loudness на точковому графіку

data %>%
  ggplot(aes(x=energy, y=loudness)) +
  geom_point(alpha=0.5)

ggplot(data=data, aes(x=energy)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

ggplot(data=data, aes(x=loudness)) +
  geom_density(fill="#43c4d9", color="#e9ecef", alpha=0.8)

# Об'єднання двох стовпців в один датасет для візуалізації їх гістограм розподілу (loudness, energy)

energy <-  data.frame(length=data$energy)
energy$type <- 'energy'
loudness <- data.frame(length=data$loudness)
loudness$type <- 'loudness'
energy_and_loudness <- rbind(energy, loudness)

ggplot(data=energy_and_loudness, aes(x=length, group=type, fill=type)) +
  geom_density(alpha=.4)

# Стовпчаста діаграма speechiness для кожної групи explicit

data %>%
    ggplot(aes(x = explicit, y = speechiness, fill = explicit)) +
    geom_bar(stat = "identity")
    