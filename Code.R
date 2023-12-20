
# Installing required packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages('openintro')
install.packages("knitr")
install.packages("rsample")
install.packages("broom")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")

# Importing libraries
library(tidyverse)
library(openintro)
library(readr)
library(dplyr)
library(knitr)
library(ggplot2)

# Pre-processing Data

# Importing CSV-file
df <- read.csv("C:\\Users\\jija0\\OneDrive\\Универ\\DataScience\\Nastea\\dar\\diabetes_prediction_dataset.csv")

# Displaying the first few rows of the data
head(df)

# Displaying summary information about the data
glimpse(df)

# Printing data types of each column
variable_types <- sapply(df, class)
print(variable_types)

# Displaying data types in a formatted table
kable(as.data.frame(sapply(df, class)), 
      col.names = c("Column Name", "Data Type"), 
      caption = "Data Types in 'diabetes'")

# Changing Data Types from character to factor
df <- data.frame(lapply(df, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else {
    x
  }
}))

# Displaying data types after conversion
kable(as.data.frame(sapply(df, class)), 
      col.names = c("Column Name", "Data Type"), 
      caption = "Data Types in 'diabetes'")

# Checking for missing data
sum(is.na(df))

# Counting the number of missing values in each column
null_counts <- df %>%
  summarise_all(~ sum(is.na(.)))

# Converting the results to a convenient format
null_counts_df <- as.data.frame(t(null_counts))
print(null_counts_df)

# Determining the distribution of the target variable
outcome_counts <- df %>%
  group_by(diabetes) %>%
  summarise(count = n())

# Displaying outcome counts
outcome_counts

# Displaying unique values and their count
unique_values <- unique(df$ColumnName)
count_unique <- length(unique_values)

# Displaying summary statistics of the data
summary(df)

# Counting the number of unique values in each column
unique_counts <- sapply(df, function(x) length(unique(x)))
print(unique_counts)

# Converting the target variable to a factor
df$diabetes <- as.factor(df$diabetes)
glimpse(df)

# Creating a pie chart with percentage labels
ggplot(outcome_counts, aes(x = "", y = count, fill = factor(diabetes))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#094D74", '#73BBE4')) +
labs(title = "Distribution of Diabetes Cases",
     fill = "Diabetic Status") +
  theme_void() +
  geom_text(aes(label = scales::percent(count / sum(count)), y = count), position = position_stack(vjust = 0.5), color = "white")

# Installing and loading the 'corrplot' package
install.packages('corrplot')
library(corrplot)

# Displaying correlation plot
corrplot(cor(df))

# Checking the levels of the 'gender' column
gender_levels <- levels(df$gender)
print(gender_levels)

# Displaying unique values of the 'gender' column
unique_values <- unique(df$gender)
print(count(unique_values))

# Filtering out rows with 'gender' as 'Other'
df <- df %>%
  filter(gender != "Other")

# Creating a side-by-side bar chart for 'gender' and 'diabetes'
ggplot(data = df, aes(x = gender, fill = diabetes)) + 
  geom_bar(position = "dodge", stat = "count") +
  scale_fill_manual(values = c("#094D74", '#73BBE4')) +
  labs(title = "Side-by-Side Bar Chart of Gender and Diabetes",
       x = "Gender",
       y = "Count")

# Displaying information about the data after filtering
glimpse(df)

# Creating a side-by-side bar chart for 'smoking_history' and 'diabetes'
ggplot(data = df, aes(x = smoking_history, fill = diabetes)) + 
  geom_bar(position = "dodge", stat = "count") +
  scale_fill_manual(values = c("#094D74", '#73BBE4')) +
  labs(title = "Side-by-Side Bar Chart of Smoking History and Diabetes",
       x = "Smoking History",
       y = "Count")

# Displaying information about the data after filtering
glimpse(df)

# Selecting only numeric variables from the dataset
numeric_diabetes <- df[, sapply(df, is.numeric)]

# Creating a correlation heatmap with specified colors
corr_matrix <- cor(numeric_diabetes)
green_palette <- colorRampPalette(c("#094D74", '#73BBE4'))(100)
var_names <- colnames(numeric_diabetes)
par(plt.width = 10, plt.height = 10)
corrplot(corr_matrix, method = "color", tl.col = "black", tl.srt = 45, addCoef.col = "black", col = green_palette, diag.col = "black", colnames = var_names)

# Defining bins for categories
level_bins <- c(-Inf, 5.7, 6.5, Inf)

# Defining labels for categories
level_labels <- c('Normal', 'Pre-Diabetic', 'Diabetic')

# Replacing values in the 'HbA1c_level' column
df <- df %>%
  mutate(HbA1c_level = cut(HbA1c_level, breaks = level_bins, labels = level_labels))

# Creating a bar plot using ggplot2
ggplot(df, aes(x = HbA1c_level, fill = diabetes)) +
  geom_bar(position = "dodge", color = "white") +
  labs(title = "Diabetes vs. HbA1c_level", x = "HbA1c level", y = "Count") +
  scale_fill_manual(values = c("#094D74", '#73BBE4')) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0))

# Creating a contingency table
table_df <- table(df$HbA1c_level, df$diabetes)

# Adding row and column sums
table_df <- addmargins(table_df)

# Displaying the table
print(table_df)

# Creating a pie chart for hypertension percentage
ggplot(count_dict, aes(x = "", y = Freq, fill = as.factor(Var1))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", (Freq/sum(Freq))*100)),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0) +
  labs(title = "Percentage of Patients with Hypertension

",
       fill = "Hypertension",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("#094D74", '#73BBE4'), name = "Hypertension") +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.85))

# Checking the levels of the 'smoking_history' column
smoking_history_levels <- levels(df$smoking_history)
print(smoking_history_levels)

# Converting 'diabetes' column to factor
df$diabetes <- as.factor(df$diabetes)
glimpse(df)

# Displaying information about the data
glimpse(df)

# Converting 'diabetes' column to numeric
df$diabetes <- as.numeric(df$diabetes)

# Checking changes
glimpse(df)

# Converting binary variable to factor
df$diabetes <- as.factor(df$diabetes)

# Creating a scatter plot with color based on the binary variable
ggplot(df, aes(x = bmi, y = age, color = diabetes)) +
  geom_point() +
  scale_fill_manual(values = c("#094D74", '#73BBE4')) +
  labs(title = "Scatter Plot of Numeric and Binary Variables", 
       x = "BMI", y = "Age")

# Установка необходимых пакетов
install.packages("rsample")
install.packages("broom")
install.packages("gridExtra")

# Загрузка библиотек
library(rsample)
library(broom)
library(gridExtra)

# Создание обучающего (70%) и тестового (30%) наборов данных
df_split <- initial_split(df, prop = .7, strata = 'diabetes')
df_train <- training(df_split)
df_test <- testing(df_split)

# Вывод количества строк в обучающем и тестовом наборах
nrow(df_train)
nrow(df_test)

# Множественная логистическая регрессия
model3 <- glm(
  diabetes ~ gender + age + hypertension + bmi + smoking_history + HbA1c_level + blood_glucose_level, family = 'binomial',
  data = df_train
)
tidy(model3)

# Создание Confusion Matrix
conf_matrix <- table(predicted_classes, df_train$diabetes)
print(conf_matrix)

# Вычисление Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Random Forest модель
model_rf <- randomForest(diabetes ~ gender + age + hypertension + bmi + smoking_history + HbA1c_level + blood_glucose_level, data = df_train)
predicted_rf <- predict(model_rf, df_test)

# Создание Confusion Matrix для Random Forest
conf_matrix_rf <- table(predicted_rf, df_test$diabetes)

# Вывод Confusion Matrix
print(conf_matrix_rf)

# Вычисление Accuracy для Random Forest
accuracy_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
cat("Random Forest Accuracy:", accuracy_rf, "\n")

# Support Vector Machine (SVM)
model_svm <- svm(diabetes ~ gender + age + hypertension + bmi + smoking_history + HbA1c_level + blood_glucose_level, data = df_train)
predicted_svm <- predict(model_svm, df_test)

# Создание Confusion Matrix для SVM
conf_matrix_svm <- table(predicted_svm, df_test$diabetes)

# Вывод Confusion Matrix
print(conf_matrix_svm)

# Вычисление Accuracy для SVM
accuracy_svm <- sum(diag(conf_matrix_svm)) / sum(conf_matrix_svm)
cat("SVM Accuracy:", accuracy_svm, "\n")

# Вывод сводной статистики по Accuracy для всех моделей
summary( 
  resamples(
    list(
      model1 = model_rf, model_2 = model_3, model3 = model_svm
    ))
)$statistics$Accuracy
