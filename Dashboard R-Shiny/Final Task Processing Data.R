# Mengimpor paket yang diperlukan
library(dplyr)
library(readr)
library(tidyverse)
library(zoo)
library(psych)
library(corrplot)
library(shinyWidgets)

#install.packages("shinyWidgets")
#install.packages("corrplot")
#install.packages("zoo")
#install.packages("psych")


# 1-Inputkan Datanya
df <- read_csv("C:/Users/rendi/ITTS DATA SCIENCE/Semester 6/SIB - BISA AI/Materi/1. Data Science/4. Data Science with R/Enroll Bisa AI Course - Data Science With R/Data Visualization R/data/Automobile_data.csv")

# Melihat jumlah null data di setiap kolom
missing_data <- sapply(df, function(x) sum(is.na(x)))
missing_data

# Ubah nilai "?" menjadi NaN di semua kolom
df <- df %>%
  mutate_all(~ifelse(. == "?", NA, .))

View(df)

# Menampilkan nama-nama kolom dalam dataframe
colnames(df)

# Menghapus kolom 'normalized.losses'
df_clean <- subset(df, select = -`normalized-losses`)

# Menghapus baris yang mengandung nilai NA
df_clean <- na.omit(df_clean)

View(df_clean)

# Melihat jumlah null data di setiap kolom
missing_data_now <- sapply(df_clean, function(x) sum(is.na(x)))
missing_data_now

library(psych)

# Menggunakan describe() untuk mendapatkan objek describe
describe(df_clean) 

# Menampilkan nama-nama kolom dalam dataframe
colnames(df_clean)

# Konversi kolom kolom menjadi Numeric int atau float sesuai kebutuhan
df_clean$price <- as.integer(df_clean$price)
df_clean$bore <- as.numeric(df_clean$bore)
df_clean$`engine-size` <- as.numeric(df_clean$`engine-size`)
df_clean$`highway-mpg` <- as.numeric(df_clean$`highway-mpg`)
view(df_clean["highway-mpg"])


# Melihat corelasi antar kolom dengan bantuan library "corrplot"
correlation_matrix <- cor(df_clean[c("engine-size", "bore", "highway-mpg")])
correlation_matrix
corrplot(correlation_matrix, method = "color")


