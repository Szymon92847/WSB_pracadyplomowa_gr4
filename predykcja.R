# Instalowanie i łądowanie wymaganych pakietów
required_packages <- c("ggplot2", "dplyr", "tidyr", "caret", "Metrics", "GGally", "car", "glmnet", "MASS", "gridExtra", "ggcorrplot", "grid")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(Metrics)
library(GGally)
library(car)
library(glmnet)
library(MASS)
library(gridExtra)
library(ggcorrplot)
library(grid)

# ustawianie katalogu roboczego
setwd("C:/Users/sqladmin/Desktop/")

# Wczytanie danych
diamonds_cleaned <- read.csv("diamonds_cleaned.csv")

# Zmiana nazwy kolumny
diamonds_cleaned <- diamonds_cleaned %>%
  dplyr::rename(Table = TableColumn)

# Zmiana wartości 'very Good' na 'VeryGood' w kolumnie Cut
diamonds_cleaned$Cut <- gsub("very Good", "VeryGood", diamonds_cleaned$Cut)

# Usunięcie kolumny 'Id'
diamonds_cleaned <- diamonds_cleaned %>%
  dplyr::select(-Id)

# Zmiana typu zmiennych kategorycznych na factor
diamonds_cleaned <- diamonds_cleaned %>%
  dplyr::mutate(Cut = as.factor(Cut),
                Color = as.factor(Color),
                Clarity = as.factor(Clarity))

# Funkcja do usuwania outlierów na podstawie IQR dla zmiennych numerycznych
remove_outliers <- function(df) {
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {  # Tylko kolumny numeryczne
      iqr_value <- IQR(df[[col]], na.rm = TRUE)
      Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
      lower_bound <- Q1 - 1.5 * iqr_value
      upper_bound <- Q3 + 1.5 * iqr_value
      df <- df %>%
        dplyr::filter(df[[col]] >= lower_bound & df[[col]] <= upper_bound)
    }
  }
  return(df)
}

# Usuwanie outlierów w całej ramce (zmienne numeryczne)
diamonds_cleaned_no_outliers <- remove_outliers(diamonds_cleaned)

# Macierz korelacji
numeric_data <- dplyr::select(diamonds_cleaned_no_outliers, where(is.numeric))
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Wykresy relacji między zmiennymi
relation_plot <- ggpairs(numeric_data)
ggsave("wykresy/relation_plot.png", plot = relation_plot, width = 10, height = 8)

# Wykres korelacji
correlation_plot <- ggcorrplot(correlation_matrix, method = "circle", type = "lower", lab = TRUE)
ggsave("wykresy/correlation_plot.png", plot = correlation_plot, width = 8, height = 6)

# Wykresy gęstości dla zmiennych numerycznych (ciągłych)
density_plots <- lapply(names(numeric_data), function(col) {
  ggplot(numeric_data, aes_string(x = col)) +
    geom_density(fill = "skyblue", color = "black", alpha = 0.7) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14) 
    ) +
    ggtitle(col)
})

overall_title <- textGrob("Funkcje gęstości", gp = gpar(fontsize = 16, fontface = "bold"))

combined_density_plot <- grid.arrange(
  overall_title,  
  arrangeGrob(grobs = density_plots, ncol = 3),
  nrow = 2,         
  heights = c(0.1, 0.9)  # 10% wysokości dla tytułu
)

ggsave("wykresy/density_plots.png", plot = combined_density_plot, width = 14, height = 10)

# Kodowanie zmiennych kategorycznych  - One-Hot encoding
diamonds_cleaned_no_outliers <- diamonds_cleaned_no_outliers %>%
  dplyr::mutate(Cut = as.factor(Cut),
                Color = as.factor(Color),
                Clarity = as.factor(Clarity)) %>%
  cbind(
    model.matrix(~ Cut - 1, data = diamonds_cleaned_no_outliers),  
    model.matrix(~ Color - 1, data = diamonds_cleaned_no_outliers), 
    model.matrix(~ Clarity - 1, data = diamonds_cleaned_no_outliers)
  ) %>%
  dplyr::select(-Cut, -Color, -Clarity)

# Podział na dane treningowe i testowe
set.seed(123)
trainIndex <- createDataPartition(diamonds_cleaned_no_outliers$Price, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)

trainData <- diamonds_cleaned_no_outliers[trainIndex,]
testData <- diamonds_cleaned_no_outliers[-trainIndex,]

# Odpowiedzi
y_train <- trainData$Price
y_test <- testData$Price
X_train <- dplyr::select(trainData, -Price)
X_test <- dplyr::select(testData, -Price)

# Różne modele regresji
lm_model <- lm(Price ~ ., data = trainData)
lasso_model <- cv.glmnet(as.matrix(X_train), y_train, alpha = 1)
ridge_model <- cv.glmnet(as.matrix(X_train), y_train, alpha = 0)
elastic_net_model <- cv.glmnet(as.matrix(X_train), y_train, alpha = 0.5)
poly_model <- lm(Price ~ poly(Carat, 2) + poly(Depth, 2) + poly(Table, 2) + 
                   CutFair + CutGood + CutIdeal + CutPremium + CutVeryGood + 
                   ColorD + ColorE + ColorG + ColorH + ColorI + ColorJ + 
                   ClarityI1 + ClarityIF + ClaritySI1 + ClaritySI2 + ClarityVS1 + 
                   ClarityVS2 + ClarityVVS1 + ClarityVVS2, data = trainData)

# Predykcje dla zbioru testowego
predictions <- list(
  "Liniowy" = predict(lm_model, newdata = testData),
  "Lasso" = predict(lasso_model, newx = as.matrix(X_test), s = "lambda.min"),
  "Ridge" = predict(ridge_model, newx = as.matrix(X_test), s = "lambda.min"),
  "Elastic Net" = predict(elastic_net_model, newx = as.matrix(X_test), s = "lambda.min"),
  "Wielomianowy" = predict(poly_model, newdata = testData)
)

# Lista i ramka modeli
models_list <- list(lm_model, lasso_model, ridge_model, elastic_net_model, poly_model)
results <- data.frame(Model = character(), RMSE = numeric(), R2 = numeric(), stringsAsFactors = FALSE)

# Tworzenie ramki z wynikami
for (model_name in names(predictions)) {
  model_pred <- as.vector(predictions[[model_name]])  # prognozy to wektor?
  
  # Obliczenie RMSE i R2
  rmse_val <- sqrt(mean((y_test - model_pred)^2))
  r2_val <- cor(y_test, model_pred)^2
  
  results <- dplyr::bind_rows(results, data.frame(Model = model_name, RMSE = rmse_val, R2 = r2_val))
}

print(results)

# Funkcja do tworzenia wykresu dla pojedynczego modelu
plot_predictions <- function(true_values, predicted_values, model_name) {
  ggplot() +
    geom_point(aes(x = true_values, y = predicted_values), alpha = 0.5, color = "blue") +
    geom_abline(slope = 1, intercept = 0, color = "red", size = 1) +
    xlab("Rzeczywiste wartości") +
    ylab("Prognoza") +
    ggtitle(paste("Model", model_name)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14)  # Wyśrodkowanie i pogrubienie tytułu
    )
}

# Tworzenie listy wykresów dla każdego modelu
plots <- lapply(names(predictions), function(model_name) {
  predicted_values <- predictions[[model_name]]
  
  # Obsługa formatu macierzy (dla modeli glmnet)
  if (is.matrix(predicted_values)) {
    predicted_values <- as.vector(predicted_values)
  }
  
  # Tworzenie wykresu
  plot_predictions(y_test, predicted_values, model_name)
})

# Siatka wykresów
overall_title <- textGrob("Porównanie prognoz modeli", gp = gpar(fontsize = 16, fontface = "bold"))

grid_plot <- grid.arrange(
  grobs = plots,
  ncol = 2,   # Liczba kolumn w siatce (można dostosować)
  top = overall_title
)

ggsave("wykresy/comparison_model_plot.png", plot = grid_plot, width = 14, height = 10)

# Sprawdzamy, który model ma najniższe RMSE
best_rmse_model_index <- which.min(results$RMSE)
best_rmse_model <- results$Model[best_rmse_model_index]
best_rmse <- results$RMSE[best_rmse_model_index]

# Sprawdzamy, który model ma najwyższe R²
best_r2_model_index <- which.max(results$R2)
best_r2_model <- results$Model[best_r2_model_index]
best_r2 <- results$R2[best_r2_model_index]

# Sprawdzamy, czy któryś model jest najlepszy w obu metrykach
if (best_rmse_model == best_r2_model) {
  cat("Model", best_rmse_model, "jest najlepszy pod względem obu metryk!\n",
      "Ma RMSE:", best_rmse, "oraz R²:", best_r2, "\n\n")
} else {
  cat("Nie ma modelu, który byłby najlepszy pod względem obu metryk jednocześnie.\n",
      "Najlepszy model pod względem RMSE to:", best_rmse_model, "z RMSE:", best_rmse, "\n",
      "Najlepszy model pod względem R² to:", best_r2_model, "z R²:", best_r2, "\n\n")
}

cat("Podsumowanie wyników:\n",
    paste("Model", results$Model[1], ": RMSE =", results$RMSE[1], ", R² =", results$R2[1], "\n"),
    paste("Model", results$Model[2], ": RMSE =", results$RMSE[2], ", R² =", results$R2[2], "\n"),
    paste("Model", results$Model[3], ": RMSE =", results$RMSE[3], ", R² =", results$R2[3], "\n"),
    paste("Model", results$Model[4], ": RMSE =", results$RMSE[4], ", R² =", results$R2[4], "\n"),
    paste("Model", results$Model[5], ": RMSE =", results$RMSE[5], ", R² =", results$R2[5], "\n\n"),
    
    "Interpretacja metryk:\n",
    "1. RMSE (Root Mean Squared Error) – Błąd średniokwadratowy:\n",
    "   RMSE mierzy średnią różnicę pomiędzy wartościami rzeczywistymi a prognozowanymi przez model.\n",
    "   Im mniejsze RMSE, tym lepszy model, ponieważ oznacza to, że prognozy modelu są bliższe rzeczywistym wartościom.\n",
    "   W naszym przypadku, model Liniowy ma najniższe RMSE, co wskazuje na lepszą jakość prognoz.\n\n",
    
    "2. R² (współczynnik determinacji):\n",
    "   R² mierzy, jaka część zmienności zmiennej zależnej (ceny diamentu) jest wyjaśniona przez model.\n",
    "   Wartość R² mieści się w przedziale od 0 do 1, gdzie wartość bliska 1 oznacza, że model dobrze wyjaśnia zmienność danych,\n",
    "   a wartość bliska 0 sugeruje, że model nie jest w stanie wyjaśnić zmienności danych.\n",
    "   W naszym przypadku, modele mają bardzo wysokie wartości R² (powyżej 0.9), co wskazuje, że nasze modele dobrze wyjaśniają zmienność ceny diamentów.\n\n",
    
    "Na podstawie RMSE i R², model Liniowy jest najlepszym wyborem.\n")

# Podsumowanie modelu
summary(lm_model)

# Współczynniki modelu
coef_values <- coef(lm_model)
coef_values_clean <- coef_values[!is.na(coef_values)]

# Wzór regresji
cat("Wzór regresji liniowej:\nPrice =", round(coef_values_clean[1], 2), "+", 
    paste(round(coef_values_clean[-1], 2), names(coef_values_clean)[-1], sep=" * ", collapse = " + "), "\n")


# PRZYKŁAD - predykcja danych na podstawie modelu liniowego
new_diamonds_data_encoded <- data.frame(
  Carat = c(0.5, 1.0, 1.5),
  Depth = c(61.5, 62.3, 63.0),
  Table = c(58, 57, 59),
  X = c(5.1, 5.2, 5.5), 
  Y = c(3.5, 3.6, 3.8),
  Z = c(2.3, 2.5, 2.6),
  CutFair = c(0, 0, 1),
  CutGood = c(0, 1, 0),
  CutIdeal = c(1, 0, 0),
  CutPremium = c(0, 0, 0),
  CutVeryGood = c(0, 0, 0),
  ColorD = c(0, 1, 0),
  ColorE = c(1, 0, 0),
  ColorG = c(0, 0, 1),
  ColorH = c(0, 0, 0),
  ColorI = c(0, 0, 0),
  ColorJ = c(0, 0, 0),
  ClarityI1 = c(0, 0, 0),
  ClarityIF = c(1, 0, 0),
  ClaritySI1 = c(0, 1, 0),
  ClaritySI2 = c(0, 0, 1),
  ClarityVS1 = c(0, 0, 0),
  ClarityVS2 = c(0, 0, 0),
  ClarityVVS1 = c(0, 0, 0),
  ClarityVVS2 = c(0, 0, 0)
)

predictions <- predict(lm_model, new_diamonds_data_encoded)

print(predictions)
