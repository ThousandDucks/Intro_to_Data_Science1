
# ----- Setup and Libraries --------

install.packages("lm.beta")

library(lm.beta)

data <- read.csv("data_for_model.csv") 
dim(data) # 2312 x 12

# ----- Multiple Linear Regression Model --------

# Shuffle the rows of the dataframe
set.seed(333)  # Set a seed for reproducibility
data <- data[sample(1:nrow(data)), ]

# Define the training size
train_size <- 0.8  # 80% for training

# Split the data into training and testing sets
data_train <- data[1:(train_size * nrow(data)), ]
data_test <- data[(nrow(data_train) + 1):nrow(data), ]

cat("Training set size:", nrow(data_train), "\n") # 1849
cat("Testing set size:", nrow(data_test), "\n") # 463 

# Fit a multiple linear regression model
mlr_model <- lm(current_market_value ~ ., data = data_train)

# Calculate standardized coefficients (Beta values)
mlr_model_beta <- lm.beta(mlr_model)

summary(mlr_model_beta)

# Predict on the testing set
predictions <- predict(mlr_model, newdata = data_test)

# Compare predictions with actual values
actual <- data_test$current_market_value

# Calculate MSE and RMSE
mse <- mean((actual - predictions)^2)  # Mean Squared Error
rmse <- sqrt(mse)                      # Root Mean Squared Error

# Print metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Calculate residuals
residuals <- residuals(mlr_model)

plot_data <- data.frame(Actual = actual, Predicted = predictions)

# Plot actual vs predicted values
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Predicted Market Value vs. Transfermarkt Market Value:\n Multiple Linear Regression",
    x = "Transfermarkt Market Value (€)",
    y = "Predicted Market Value (€)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, hjust = 0.5)  # Adjust title size and centering
  )

# Residual Plot
plot(fitted(mlr_model), residuals, 
     main = "Residual Plot for \nmultiple Linear Regression", 
     xlab = "Predicted Values", 
     ylab = "Residuals", 
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# Histogram of Residuals
hist(residuals, 
     main = "Histogram of Residuals for\n Multiple Linear Regression", 
     xlab = "Residuals", 
     breaks = 30, col = "lightblue", border = "black")

# Q-Q plot for residuals 
qqnorm(residuals, 
       main = "Q-Q Plot of Residuals for \nMultiple Linear Regression",
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", 
       pch = 20, col = "blue")

qqline(residuals, col = "red", lwd = 2)
