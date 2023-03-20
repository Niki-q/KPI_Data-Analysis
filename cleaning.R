# Імпорт бібліотек
library(tidyverse)

# Функція ппопередньої очистки даних

clean_date <- function() {
    # Завантаження файлу

    print("Завантаження файлу з датасетом...")
    data <- read_csv('data/dataset.csv')

    # Виведення основної інформації про датасет
    
    print("")
    print(str(data))

    # Перевизначення типів деяких змінних

    data <- data[, -2]
    data <- data %>% mutate(explicit = as.logical(explicit), track_genre = as.factor(track_genre),
        mode = as.factor(mode))

    print("Перевизначено типи даних змінних explicit, track_genre, mode")

    # Відображення кількості рядків з невизначеними полями

    print("Кількість рядків з NA значеннями:")
    print(data %>% summarise(across(everything(), ~ sum(is.na(.)))) %>%
        select(where(~ all(.) > 0)))

    #  Відображення рядків з невизначеними полями

    print("Рядки з NA значеннями:")
    print(data[!complete.cases(data), ])
    
    # Видалення рядків з невизначеними полями

    data <- data[rowSums(is.na(data)) == 0, ]
    print("Видалено рядки з NA значеннями.")
}