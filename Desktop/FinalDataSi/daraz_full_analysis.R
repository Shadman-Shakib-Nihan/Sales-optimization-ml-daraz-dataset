# ===============================
# 📦 Load Required Libraries
# ===============================
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(reshape2)


# ── Required Packages ────────────────────────────────────────
required_packages <- c(
  "caret", "rpart", "rpart.plot", "randomForest",
  "ggplot2", "dplyr", "tibble",
  "gridExtra", "Metrics"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(dplyr)
library(tibble)
library(gridExtra)
library(Metrics)

# ===============================
# 📂 Load Raw Data
# ===============================
daraz_data <- read.csv("/Users/shadmanshakib/Desktop/FinalDataSi/daraz_final_2.csv",
                       stringsAsFactors = FALSE)


colSums(is.na(daraz_data))

# ===============================
# 🧹 INITIAL CLEANING (YOUR PIPELINE FIXED)
# ===============================
cleaned_data <- daraz_data %>%
  # Remove unnecessary columns
  select(-Price_Show, -In_Stock, -SKU_ID, -Product_ID, -Image_URL) %>%
  
  # Trim whitespace
  mutate(across(where(is.character), ~ trimws(.))) %>%
  
  # Remove duplicates
  distinct()

# Use cleaned data
df <- cleaned_data

# ===============================
# 📊 1. DATASET STRUCTURE
# ===============================
cat("🔹 Structure:\n")
str(df)

cat("\n🔹 Dimensions:\n")
print(dim(df))

cat("\n🔹 Missing Values:\n")
print(colSums(is.na(df)))

cat("\n🔹 Duplicate Rows:\n")
print(sum(duplicated(df)))

# ===============================
# 🔧 2. NUMERIC CLEANING
# ===============================

# ---- Units_Sold ----
raw <- df$Units_Sold
is_k <- grepl("K", raw)

clean <- gsub("[^0-9.]", "", raw)
clean <- as.numeric(clean)

clean[is_k] <- clean[is_k] * 1000
df$Units_Sold <- clean

# ---- Price ----
if("Price" %in% colnames(df)) {
  df$Price <- gsub("[^0-9.]", "", df$Price)
  df$Price <- as.numeric(df$Price)
}

# ===============================
# ⚠️ 3. HANDLE NA (SAFE)
# ===============================

df$Units_Sold[is.na(df$Units_Sold)] <- 0

if("Price" %in% colnames(df)) {
  df$Price[is.na(df$Price)] <- median(df$Price, na.rm = TRUE)
}




print(
  ggplot(df, aes(y = Units_Sold)) +
    geom_boxplot() +
    ggtitle("Before Outlier Handling - Units Sold") +
    theme_minimal()
)

if("Price" %in% colnames(df)) {
  print(
    ggplot(df, aes(y = Price)) +
      geom_boxplot() +
      ggtitle("Before Outlier Handling - Price") +
      theme_minimal()
  )
}


#--------------------------===============================



print(
  ggplot(df, aes(y = Units_Sold)) +
    geom_boxplot() +
    ggtitle("Before Outlier Handling - Units Sold") +
    theme_minimal()
)

if("Price" %in% colnames(df)) {
  print(
    ggplot(df, aes(y = Price)) +
      geom_boxplot() +
      ggtitle("Before Outlier Handling - Price") +
      theme_minimal()
  )
}


# ===============================
# 🚨 4. OUTLIER HANDLING (CAPPING)
# ===============================

cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  x[x > upper] <- upper
  x[x < lower] <- lower
  
  return(x)
}

df$Units_Sold <- cap_outliers(df$Units_Sold)

if("Price" %in% colnames(df)) {
  df$Price <- cap_outliers(df$Price)
}

# ===============================
# 📈 5. SUMMARY
# ===============================
cat("\n🔹 Summary:\n")
print(summary(df))

# ===============================
# 📊 6. DISTRIBUTION PLOTS
# ===============================

print(
  ggplot(df, aes(x = Units_Sold)) +
    geom_histogram(bins = 50) +
    ggtitle("Distribution of Units Sold") +
    theme_minimal()
)

if("Price" %in% colnames(df)) {
  print(
    ggplot(df, aes(x = Price)) +
      geom_histogram(bins = 50) +
      ggtitle("Distribution of Price") +
      theme_minimal()
  )
}

# ===============================
# 📦 7. BOXPLOTS
# ===============================

print(
  ggplot(df, aes(y = Units_Sold)) +
    geom_boxplot() +
    ggtitle("Units Sold (Outliers Capped)") +
    theme_minimal()
)

if("Price" %in% colnames(df)) {
  print(
    ggplot(df, aes(y = Price)) +
      geom_boxplot() +
      ggtitle("Price (Outliers Capped)") +
      theme_minimal()
  )
}

# ===============================
# 🔗 8. RELATIONSHIP
# ===============================

if(all(c("Price", "Units_Sold") %in% colnames(df))) {
  print(
    ggplot(df, aes(x = Price, y = Units_Sold)) +
      geom_point(alpha = 0.5) +
      ggtitle("Price vs Units Sold") +
      theme_minimal()
  )
}

# ===============================
# 📊 9. TOP PRODUCTS
# ===============================

if("Product_Name" %in% colnames(df)) {
  top_products <- df %>%
    group_by(Product_Name) %>%
    summarise(total_sold = sum(Units_Sold, na.rm = TRUE)) %>%
    arrange(desc(total_sold)) %>%
    head(10)
  
  print(
    ggplot(top_products, aes(x = reorder(Product_Name, total_sold), y = total_sold)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      ggtitle("Top 10 Products by Units Sold") +
      theme_minimal()
  )
}

# ===============================
# 📊 10. CORRELATION HEATMAP
# ===============================

numeric_df <- df %>% select(where(is.numeric))

if(ncol(numeric_df) > 1) {
  corr <- cor(numeric_df)
  melted_corr <- melt(corr)
  
  print(
    ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      ggtitle("Correlation Heatmap") +
      theme_minimal()
  )
}

# ===============================
# 📌 11. SAVE FINAL DATA
# ===============================
write.csv(df, "/Users/shadmanshakib/Desktop/FinalDataSi/final_cleaned_dataset.csv", row.names = FALSE)






data <- read.csv("/Users/shadmanshakib/Desktop/FinalDataSi/final_cleaned_dataset.csv",
               stringsAsFactors = TRUE)

df_model <- data |>
  select(
    Category, Brand, Price_Current, Price_MRP,
    Rating, Reviews, Units_Sold, Location, Discount_Pct
  ) |>
  filter(!is.na(Discount_Pct))

cat(sprintf("Dataset: %d rows × %d columns\n", nrow(df_model), ncol(df_model)))

df_model <- df_model %>%
  na.omit()

# ════════════════════════════════════════════════════════════
#  SECTION 2 — TRAIN / TEST SPLIT
# ════════════════════════════════════════════════════════════

set.seed(42)
train_idx <- createDataPartition(df_model$Discount_Pct, p = 0.8, list = FALSE)

train_df <- df_model[train_idx, ]
test_df  <- df_model[-train_idx, ]

cat(sprintf("Train: %d rows | Test: %d rows\n", nrow(train_df), nrow(test_df)))

# ════════════════════════════════════════════════════════════
#  SECTION 3 — CROSS VALIDATION
# ════════════════════════════════════════════════════════════

ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = FALSE
)

# ════════════════════════════════════════════════════════════
#  MODEL 1 — LINEAR REGRESSION
# ════════════════════════════════════════════════════════════

cat("\n[1/3] Training Linear Regression...\n")

lm_fit <- train(Discount_Pct ~ .,
                data = train_df,
                method = "lm",
                trControl = ctrl)

lm_pred <- predict(lm_fit, newdata = test_df)

lm_rmse <- rmse(test_df$Discount_Pct, lm_pred)
lm_mae  <- mae(test_df$Discount_Pct, lm_pred)
lm_r2   <- cor(test_df$Discount_Pct, lm_pred)^2

cat(sprintf("RMSE: %.3f | MAE: %.3f | R²: %.4f\n",
            lm_rmse, lm_mae, lm_r2))

# ════════════════════════════════════════════════════════════
#  MODEL 2 — DECISION TREE
# ════════════════════════════════════════════════════════════

cat("\n[2/3] Training Decision Tree...\n")

dt_fit <- train(Discount_Pct ~ .,
                data = train_df,
                method = "rpart",
                trControl = ctrl,
                tuneLength = 10)

rpart.plot(dt_fit$finalModel,
           type = 4, extra = 101, fallen.leaves = TRUE,
           main = "Decision Tree — Discount Prediction",
           cex = 0.7)

dt_pred <- predict(dt_fit, newdata = test_df)

dt_rmse <- rmse(test_df$Discount_Pct, dt_pred)
dt_mae  <- mae(test_df$Discount_Pct, dt_pred)
dt_r2   <- cor(test_df$Discount_Pct, dt_pred)^2

cat(sprintf("RMSE: %.3f | MAE: %.3f | R²: %.4f\n",
            dt_rmse, dt_mae, dt_r2))

# ════════════════════════════════════════════════════════════
#  MODEL 3 — RANDOM FOREST
# ════════════════════════════════════════════════════════════

cat("\n[3/3] Training Random Forest...\n")

rf_fit <- train(Discount_Pct ~ .,
                data = train_df,
                method = "rf",
                trControl = ctrl,
                tuneLength = 5,
                ntree = 300)

rf_pred <- predict(rf_fit, newdata = test_df)

rf_rmse <- rmse(test_df$Discount_Pct, rf_pred)
rf_mae  <- mae(test_df$Discount_Pct, rf_pred)
rf_r2   <- cor(test_df$Discount_Pct, rf_pred)^2

cat(sprintf("RMSE: %.3f | MAE: %.3f | R²: %.4f\n",
            rf_rmse, rf_mae, rf_r2))

# Variable importance
imp_df <- varImp(rf_fit)$importance |>
  rownames_to_column("Feature") |>
  arrange(desc(Overall))

ggplot(imp_df, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_col(fill = "#1D7E5F") +
  coord_flip() +
  labs(title = "Random Forest — Variable Importance",
       x = NULL, y = "Importance") +
  theme_minimal()

# ════════════════════════════════════════════════════════════
#  SECTION 4 — MODEL COMPARISON
# ════════════════════════════════════════════════════════════

results <- tibble(
  Model = c("Linear Regression", "Decision Tree", "Random Forest"),
  RMSE  = c(lm_rmse, dt_rmse, rf_rmse),
  MAE   = c(lm_mae, dt_mae, rf_mae),
  R2    = c(lm_r2, dt_r2, rf_r2)
) |>
  mutate(RMSE = round(RMSE, 3),
         MAE  = round(MAE, 3),
         R2   = round(R2, 4),
         Rank = rank(RMSE))

cat("\n════════ MODEL COMPARISON ════════\n")
print(results |> arrange(Rank))

# RMSE plot
p_rmse <- ggplot(results,
                 aes(x = reorder(Model, RMSE), y = RMSE)) +
  geom_col(fill = "#6B8CAE") +
  coord_flip() +
  labs(title = "RMSE Comparison", x = NULL)

# R2 plot
p_r2 <- ggplot(results,
               aes(x = reorder(Model, R2), y = R2)) +
  geom_col(fill = "#6B8CAE") +
  coord_flip() +
  labs(title = "R² Comparison", x = NULL)

grid.arrange(p_rmse, p_r2, ncol = 2)

# ════════════════════════════════════════════════════════════
#  SECTION 5 — BEST MODEL
# ════════════════════════════════════════════════════════════

best_model <- results |> arrange(Rank) |> slice(1) |> pull(Model)

cat("\nBEST MODEL:", best_model, "\n")

best_pred <- switch(best_model,
                    "Linear Regression" = lm_pred,
                    "Decision Tree" = dt_pred,
                    "Random Forest" = rf_pred)

pred_df <- data.frame(
  Actual = test_df$Discount_Pct,
  Predicted = best_pred
)

# Predicted vs Actual
ggplot(pred_df, aes(Actual, Predicted)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal() +
  labs(title = paste("Best Model:", best_model))

# Residuals
pred_df$Residual <- pred_df$Actual - pred_df$Predicted

ggplot(pred_df, aes(Predicted, Residual)) +
  geom_point(alpha = 0.4, color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Residual Plot")

cat("\n✔ Done Successfully (XGBoost Removed)\n")


# ════════════════════════════════════════════════════════════
#  SECTION 5 — For Linear Regression
# ════════════════════════════════════════════════════════════

lm_train_pred <- predict(lm_fit, newdata = train_df)

train_lm_df <- data.frame(
  Actual = train_df$Discount_Pct,
  Predicted = lm_train_pred
)

ggplot(train_lm_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal() +
  labs(
    title = "Linear Regression (Train Data)",
    x = "Actual Discount",
    y = "Predicted Discount"
  )


# ════════════════════════════════════════════════════════════
#  SECTION 5 — For Random Forest
# ════════════════════════════════════════════════════════════

rf_train_pred <- predict(rf_fit, newdata = train_df)

train_rf_df <- data.frame(
  Actual = train_df$Discount_Pct,
  Predicted = rf_train_pred
)

ggplot(train_rf_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal() +
  labs(
    title = "Random Forest (Train Data)",
    x = "Actual Discount",
    y = "Predicted Discount"
  )

