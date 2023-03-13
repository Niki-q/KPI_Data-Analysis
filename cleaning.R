# Імпорт бібліотек
library(tidyverse)

clean_date <- function() { # nolint
    # Завантаження файлу, перевизначення типів
    data <- read_csv('data/dataset.csv')

    print(str(data))

    # data.mutate
    data <- data[, -2]
    data <- data %>% mutate(explicit = as.logical(explicit), track_genre = as.factor(track_genre),
        mode = as.factor(mode))

    # print(str(data)) # nolint

    # Відображення кількості рядків з невизначеними полями
    print(data %>% summarise(across(everything(), ~ sum(is.na(.)))) %>%  # nolint
        select(where(~ all(.) > 0)))

    #  Відображення рядків з невизначеними полями
    print(data[!complete.cases(data), ])

    # Видалення рядків з невизначеними полями
    data <- data[rowSums(is.na(data)) == 0, ]
}