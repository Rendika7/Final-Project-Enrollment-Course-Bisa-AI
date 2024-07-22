df <- read.csv('C:/Users/rendi/ITTS DATA SCIENCE/Semester 6/SIB - BISA AI/Materi/3. Praktikum Data Science/12. R-Studio/Enroll kelas Analisa Data Spatial dengan R/Final Project/Dataset/housing.csv', stringsAsFactors = TRUE)
head(df)

summary(df)

# ===================================================================

# Mengecek jumlah nilai NA di seluruh data frame
sum(is.na(df))

# Drop Island level
df <- df[df$ocean_proximity != 'ISLAND', ]
df$ocean_proximity <- droplevels(df$ocean_proximity)

# Impute using median
df$total_bedrooms[is.na(df$total_bedrooms)] = median(df$total_bedrooms , na.rm = TRUE)

# Normalise per household and log-transform
df$log_rooms_per_household <- log(df$total_rooms / df$households)
df$log_bedrooms_per_household <- log(df$total_bedrooms / df$households)

# Log-transform the other all-positive quantities
df$log_median_income <- log(df$median_income)
df$log_households <- log(df$households)
df$log_population <- log(df$population)
df$log_median_house_value <- log(df$median_house_value)
df$log_housing_median_age <- log(df$housing_median_age)

# ===================================================================

library(ggplot2)
library(reshape2)
ggplot(data = melt(df), mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x') +
  theme_minimal()

# ===================================================================

# Instal paket sf
# install.packages("sf", dependencies = TRUE)
# install.packages("rnaturalearth", dependencies = TRUE)
# install.packages("dbscan", dependencies = TRUE)

library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library("maps")

spat_df <- st_as_sf(df, coords=c('longitude', 'latitude'), crs=4326)
spat_df$log_median_house_value <- log10(spat_df$median_house_value)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

ggplot(data = states[4, ]) +
  geom_sf() +
  geom_sf(aes(colour=log_median_house_value), data=spat_df) +
  theme_classic()

# ===================================================================

# Let's first make a train/test split
set.seed(2)

# We'll use 90% of the data in our training set and the rest in the test set
is_train <- sample(c(TRUE, FALSE), size=nrow(df), replace=TRUE, prob=c(0.9, 0.1))

spat_df$is_train <- factor(is_train, levels=c(TRUE, FALSE))

ggplot(data = states[4, ]) +
  geom_sf() +
  geom_sf(aes(colour=is_train), data=spat_df, alpha=0.4) +
  theme_classic()

# ===================================================================

# Data peta California
california_map <- map_data("state", region = "california")

# Plot dengan ggplot2
ggplot() +
  geom_polygon(data = california_map, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA) +
  geom_point(data = df, aes(x = longitude, y = latitude), color = "blue", size = 0.5) +
  labs(title = "Distribution of Records", x = "Longitude", y = "Latitude") +
  theme_minimal()

# ===================================================================

# Machine learning and clustering algorithms
library(dbscan)     # Density-based clustering

# Set seed for reproducibility
set.seed(123)

# Apply DBSCAN clustering based on geographic coordinates
dbscan_result <- dbscan(df[, c('longitude', 'latitude')], eps = 0.2, minPts = 5)

# Simplify cluster assignments: 0 for noise, 1 for any cluster
clusters <- ifelse(dbscan_result$cluster != 0, 1, 0)

# Convert cluster assignments for plotting
clusters <- as.factor(clusters)

# Visualize the clusters
ggplot(data = df, aes(x = longitude, y = latitude, color = clusters)) + 
  geom_point(alpha = 0.5) + 
  theme_minimal() +
  labs(title = "DBSCAN Clustering of Geographic Data", 
       x = "Longitude", y = "Latitude") +
  scale_color_discrete(name = "Cluster")

# ===================================================================

# Calculate ratios as new features for analysis
df$bedrooms_to_rooms_ratio <- df$total_bedrooms / df$total_rooms
df$population_to_households_ratio <- df$population / df$households
df$population_density <- df$population / df$total_rooms 

# Scale 'housing_median_age' for normalization
df$normalized_housing_age <- scale(df$housing_median_age)

# ===================================================================

# Define a function to count outliers based on the IQR method
count_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  sum(column < lower_bound | column > upper_bound, na.rm = TRUE)
}

# Apply the outlier counting function to all numeric columns
outlier_counts <- sapply(df[, sapply(df, is.numeric)], count_outliers)

# Create a table to display the count of outliers per column
outlier_table <- data.frame(Column = names(outlier_counts), OutlierCount = outlier_counts)

# Print the table of outlier counts
print(outlier_table)

# ===================================================================

# Define a function to exclude rows with outliers
exclude_rows_with_outliers <- function(df, columns) {
  for (col in columns) {
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    df <- df[!(df[[col]] < lower_bound | df[[col]] > upper_bound), ]
  }
  return(df)
}

# Specify columns to check for outliers
columns_with_outliers <- c("total_rooms", "total_bedrooms", "population", "households", "median_income", "median_house_value", "bedrooms_to_rooms_ratio", "population_to_households_ratio")

# Apply the function to remove rows with outliers
df <- exclude_rows_with_outliers(df, columns_with_outliers)

# ===================================================================

# Let's first make a train/test split
set.seed(2)

# We'll use 90% of the data in our training set and the rest in the test set
is_train <- sample(c(TRUE, FALSE), size=nrow(df), replace=TRUE, prob=c(0.9, 0.1))

train_df <- df[is_train, ]
test_df <- df[!is_train, ]

train_y = train_df[,'median_house_value']
train_x = train_df[, names(train_df) !='median_house_value']
test_y = test_df[,'median_house_value']
test_x = test_df[, names(train_df) !='median_house_value']

# Check number of points in train and test:
print(nrow(train_df))
print(nrow(test_df))

print(nrow(train_x))
print(nrow(test_x))

# ===================================================================
# install.packages("randomForest", dependencies = TRUE)
library('randomForest')

#rf_model = randomForest(median_house_value~. , data = train, ntree =500, importance = TRUE)
rf_model = randomForest(train_x, y = train_y , ntree = 500, importance = TRUE)

names(rf_model) #these are all the different things you can call from the model.

dd <- rf_model$importance
# Higher number == more important predictor.
dd

# ===================================================================

oob_prediction = predict(rf_model) #leaving out a data source forces OOB predictions

train_mse = mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
print(oob_rmse)
print(train_mse)

y_pred = predict(rf_model , test_x)
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
print(test_rmse)
print(test_mse)