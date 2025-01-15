
# ----- Setup and Libraries --------

install.packages("ggcorrplot")

library(ggcorrplot)

final_table <- read_csv("cleaned_dataset.csv") 
colnames(final_table)
dim(final_table) # 2519 x 19

# ----- Facet scatter plot for continous variables --------

# Rename variables to fit title in facet plot
eda_data <- final_table %>%
  select(
    current_market_value,
    prev_market_value = previous_market_value,   
    red_cards = total_red_cards,                 
    yellow_cards = total_yellow_cards,           
    goals = total_goals,
    assists = total_assists,
    minutes_played = total_minutes_played,
    matches = total_matches,
    age,
    height = height_in_cm                        
  )

# Reshape data into long format for faceting
eda_long <- eda_data %>%
  pivot_longer(
    cols = -current_market_value,  
    names_to = "Variable",         
    values_to = "Value"            
  )

# Create facet scatter plots 
ggplot(eda_long, aes(x = Value, y = current_market_value)) +
  geom_point(alpha = 0.4, size = 1.5) +                
  geom_smooth(method = "lm", color = "blue", size = 0.7) +  
  facet_wrap(~ Variable, scales = "free", ncol = 3) +       
  scale_y_log10() +                                        # Log scale for market value
  labs(
    x = "Variable Value",
    y = "Current Market Value (€) - Log Scale",
    title = "Numerical Predictors vs. \nPlayer Season Market Value",
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 6, face = "bold"),  
    axis.text.x = element_text(angle = 45, hjust = 1),   # Rotate x-axis labels for readability
    plot.title = element_text(size = 12, face = "bold"), 
  )

# ----- Facet histogram plot for continous variables --------

# Create facet for histograms 
ggplot(eda_long, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) + 
  facet_wrap(~ Variable, scales = "free") +    # Facet by variable using different scales
  labs(
    x = "Variable Value",
    y = "Frequency",
    title = "Distributions of Numerical \nVariables in the Dataset"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

# ----- Histogram of dependent variable (current_market_value) --------

# Create a histogram for current_market_value
hist(final_table$current_market_value,
     main = "Histogram of Current Market Value",
     xlab = "Current Market Value (in Euros)",
     ylab = "Frequency",
     col = "lightblue",
     border = "black",
     breaks = 30)  

# ----- Distribution of categorical variables --------

top_countries <- final_table %>%
  count(country_of_citizenship, sort = TRUE) %>%
  top_n(10, n)

ggplot(top_countries, aes(x = reorder(country_of_citizenship, n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(
    title = "Top 10 Countries of Citizenship",
    x = "Country of Citizenship",
    y = "Count"
  ) +
  theme_minimal()

# Position
ggplot(final_table, aes(x = fct_infreq(position))) +
  geom_bar(fill = "blue") +
  labs(
    title = "Distribution of Players by Position",
    x = "Position",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# Foot
ggplot(final_table, aes(x = fct_infreq(foot))) +
  geom_bar(fill = "blue") +
  labs(
    title = "Distribution of Players by Foot Preference",
    x = "Foot",
    y = "Count"
  ) +
  theme_minimal()

# ----- Distribution of categorical variables --------

# Boxplot for Position
ggplot(final_table, aes(x = position, y = current_market_value)) +
  geom_boxplot(fill = "blue", alpha = 0.6) +
  labs(
    title = "Market Value by Position",
    x = "Position",
    y = "Market Value (€)"
  ) +
  theme_minimal()

# Boxplot for Foot Preference
ggplot(final_table, aes(x = foot, y = current_market_value)) +
  geom_boxplot(fill = "purple", alpha = 0.6) +
  labs(
    title = "Market Value by Foot Preference",
    x = "Foot",
    y = "Market Value (€)"
  ) +
  theme_minimal()

# ----- Multicollinearity Check --------


# Select numeric variables for correlation
numeric_variable <- final_table %>%
  select(
    current_market_value, previous_market_value, total_red_cards, total_yellow_cards,
    total_goals, total_assists, total_minutes_played, total_matches,
    age, height_in_cm
  )

# Calculate Spearman correlation matrix and round up to 2 decimal places
correlation_matrix <- round(cor(numeric_variable, method = "spearman"), 2)

# Correlation heatmap
ggcorrplot(
  correlation_matrix, 
  type = "lower",         # Displays the lower triangle
  lab = TRUE,             # Display correlation values
  lab_size = 2.3,         # Correlation label size
  tl.cex = 10,            # Adjust axis text size
  tl.srt = 45,            # Rotate axis labels for better readability
  colors = c("blue", "white", "red"), # Color gradient for correlations
  legend.title = "Corr"   
) +
  labs(
    title = "Spearman Correlation Heatmap \nof Numerical Predictors and Market Value", 
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  
  ) 


