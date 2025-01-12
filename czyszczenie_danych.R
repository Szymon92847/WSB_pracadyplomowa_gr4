# Instalacja wymaganych pakietów (jeśli nie są jeszcze zainstalowane)
# install.packages("readr")
# install.packages("dplyr")
# install.packages("stringr")

# Załadowanie bibliotek
library(readr)
library(dplyr)
library(stringr)

# 1. Wczytaj dane z pliku CSV
file_path <- "C:/Users/sqladmin/Desktop/diamonds_noisy.csv"
diamonds <- read_csv(file_path, quote = "")

str(diamonds)

# 2. Ustawienie nazw kolumn, aby pasowały do struktury danych
colnames(diamonds) <- c("Id", "Carat", "Cut", "Color", "Clarity", 
                        "Depth", "TableColumn", "Price", "X", "Y", "Z")

# 3. Usuwanie niepotrzebnych znaków (takich jak cudzysłowy, apostrofy, backticki)
diamonds <- diamonds %>%
  mutate_all(~ str_replace_all(., "[\"'`]", ""))

table(diamonds$Cut)

# 4. Poprawa błędów w kolumnie Cut (zamiana "Pre_AEiu_AE" na "Premium" i innych błędów)
diamonds <- diamonds %>%
  mutate(Cut = str_replace_all(Cut, "Pre_AEiu_AE", "Premium"),   
         Cut = str_replace_all(Cut, "Go0d", "Good"),               
         Cut = str_replace_all(Cut, "very Go0d", "Very Good"))    

# 5. Usunięcie wierszy, gdzie kolor jest zapisany jako "null"
diamonds <- diamonds %>%
  filter(Color != "null")

# 6. Ujednolicenie wartości w kolumnie Clarity do wielkich liter
diamonds$Clarity <- toupper(diamonds$Clarity)

# 7. Zamiana kolumn numerycznych na typ numeric
diamonds$Id <- as.numeric(diamonds$Id)
diamonds$Carat <- as.numeric(diamonds$Carat)
diamonds$Depth <- as.numeric(diamonds$Depth)
diamonds$TableColumn <- as.numeric(diamonds$TableColumn)
diamonds$Price <- as.numeric(diamonds$Price)
diamonds$X <- as.numeric(diamonds$X)
diamonds$Y <- as.numeric(diamonds$Y)
diamonds$Z <- as.numeric(diamonds$Z)

# 8. Sprawdzanie struktury danych po przekształceniach
str(diamonds)

# 9. Zapisanie oczyszczonego pliku CSV
write_csv(diamonds, "C:/Users/sqladmin/Desktop/diamonds_cleaned.csv")

cat("Dane zostały zapisane do pliku: C:/Users/sqladmin/Desktop/diamonds_cleaned.csv\n")


