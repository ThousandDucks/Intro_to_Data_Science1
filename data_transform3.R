
# ----- Setup and Libraries --------

install.packages("car")

# Load the car package for VIF
library(car)

final_table <- read_csv("cleaned_dataset.csv") 

# ----- Exclude Goalkeepers Before Encoding --------

final_table <- final_table %>%
  filter(position != "Goalkeeper")  

# ----- Preparing data for Multiple Linear Regression --------

# Recoding sub_position, position and foot variable into numerical variables
encoded_data <- final_table %>%
  mutate(
    foot = as.numeric(as.factor(foot)),                 
    position = as.numeric(as.factor(position))          
  )

# Define the home country
home_country <- "Netherlands"

# Create a binary variable for player nationality 
encoded_data <- encoded_data %>%
  mutate(
    is_domestic_player = ifelse(country_of_citizenship == home_country, 1, 0)  # 1 for Domestic, 0 for Foreign
  )

# Select variables for modeling
encoded_data <- encoded_data %>%
  select(
    total_red_cards, total_yellow_cards, total_goals, total_assists, 
    total_minutes_played, total_matches, age, height_in_cm, 
     position, foot, previous_market_value, is_domestic_player, current_market_value
  )

# ----- Descriptive statistics --------

# Select numeric columns
numeric_data <- encoded_data %>%
  select(
    total_red_cards, total_yellow_cards, total_goals, total_assists,
    total_minutes_played, age, height_in_cm,
    previous_market_value, current_market_value
  )

# Calculate descriptive statistics
descriptive_stats <- numeric_data %>%
  summarise_all(list(
    mean = ~mean(.),
    median = ~median(.),
    sd = ~sd(.),
    min = ~min(.),
    max = ~max(.)
  ))

# Convert to data frame
descriptive_stats <- as.data.frame(descriptive_stats)

print(descriptive_stats)
# View(descriptive_stats)

categorical_data <- encoded_data %>%
  select(position, foot, is_domestic_player)  

# Calculate frequency tables and proportions for each categorical variable
frequency_and_proportion <- lapply(categorical_data, function(x) {
  freq <- table(x)  # Frequency counts
  prop <- prop.table(freq)  # Proportions
  list(Frequency = freq, Proportion = prop)
})

frequency_and_proportion

# ----- Check for multicollinearity --------

# Fit a linear model with all predictors
lm_model <- lm(current_market_value ~ ., data = encoded_data)
# summary(lm_model)

# Calculate VIF for the linear model
vif_values <- vif(lm_model)
print(vif_values) # High VIF value for total_minutes_played (7.03) and total_matches(6.14)

# Drop the unnecessary variables
encoded_data <- encoded_data %>%
  select(-total_matches)

# ----- Saving final dataset --------

# Save the encoded_data to a CSV file
write.csv(encoded_data, "data_for_model.csv", row.names = FALSE)
