
# ----- Setup and Libraries --------

install.packages("randomForest")

library(randomForest)

data <- read.csv("data_for_model.csv")

# ----- Random Forest Model --------

# Shuffle the rows of the dataframe
set.seed(333)  # Set a seed for reproducibility
data <- data[sample(1:nrow(data)), ]

# Define the training size
train_size <- 0.8  # 80% for training

# Split the data into training and testing sets
data_train <- data[1:(train_size * nrow(data)), ]
data_test <- data[(nrow(data_train) + 1):nrow(data), ]

cat("Training set size:", nrow(data_train), "\n")  # Training set size
cat("Testing set size:", nrow(data_test), "\n")    # Testing set size

# Fit a Random Forest model
rf_model <- randomForest(
  current_market_value ~ ., 
  data = data_train, 
  ntree = 300,        # Number of trees
  importance = TRUE   # Enable variable importance calculation
)

# Extract OOB Mean Squared Error (MSE) from the Random Forest model
error_df <- data.frame(
  mse = rf_model$mse,             # Out-of-Bag Mean Squared Error
  num_trees = 1:rf_model$ntree    # Number of trees
)

# Predict on the test set
predictions <- predict(rf_model, newdata = data_test)

# Add predictions to the test dataset
data_test$predicted_market_value <- predictions

# Plot OOB MSE vs. Number of Trees
ggplot(data = error_df, aes(x = num_trees, y = mse)) +
  geom_line(color = "blue") +
  labs(
    title = "Out-of-Bag MSE vs. Number of Trees: \nRandom Forest",
    x = "Number of Trees",
    y = "OOB Mean Squared Error"
  ) +
  theme_minimal()

actual <- data_test$current_market_value
sst <- sum((actual - mean(actual))^2)  # Total sum of squares
sse <- sum((actual - predictions)^2)  # Sum of squared errors
r_squared <- 1 - (sse / sst)

# Calculate MSE and RMSE
mse <- mean((actual - predictions)^2)  # Mean Squared Error
rmse <- sqrt(mse)                      # Root Mean Squared Error

# Print metrics
cat("R-squared:", r_squared, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

plot_data <- data.frame(Actual = actual, Predicted = predictions)

# Plot actual vs predicted values
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Actual vs Predicted Values",
    x = "Actual Values",
    y = "Predicted Values"
  ) +
  theme_minimal()

residuals <- actual - predictions
plot(predictions, residuals, 
     main = "Residual Plot", 
     xlab = "Predicted Values", 
     ylab = "Residuals", 
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# importance(rf_model)
# Extract variable importance and create a dataframe
feat_imp_df <- importance(rf_model) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.))  

# Plot feature importance using ggplot2
ggplot(feat_imp_df, aes(x = reorder(feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  coord_flip() +  # Flip coordinates for better readability
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "% Increase in MSE",
    title = "Feature Importance: Random Forest"
  ) +
  theme(
    axis.text.y = element_text(size = 10),  
    plot.title = element_text(hjust = 0.5, size = 12)  #
  )
