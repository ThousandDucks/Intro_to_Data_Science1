# data_cleaning.R

# ----- Setup and Libraries --------

install.packages(c("tidyverse", "moments"))

# Load libraries
library(tidyverse)
library(moments)

# ----- Import Raw Data --------

# Import datasets
# game_lineups <- read_csv("game_lineups.csv") #x
# games <- read_csv("games.csv") #x
player_valuations <- read_csv("player_valuations.csv")
players <- read_csv("players.csv")   
# transfers <- read_csv("transfers.csv")
# game_events <- read_csv("game_events.csv") #x
appearances <- read_csv("appearances.csv") 
# club_games <- read_csv("club_games.csv") # x
clubs <- read_csv("clubs.csv")   
# competitions <- read_csv("competitions.csv") # x

# ----- Initial Data Exploration --------

# Function to calculate and display missing percentages
missing_percentage <- function(data) {
  missing_df <- data.frame(
    Column = names(data),
    Missing_Percentage = sapply(data, function(x) {
      sum(is.na(x)) / length(x) * 100
    })
  )
  missing_df <- missing_df[order(-missing_df$Missing_Percentage), ]
  print(missing_df)
  return(missing_df)
}

# Function to check rows with missing values
check_missing_values <- function(data, column_name) {
  result <- data %>%
    filter(is.na(.data[[column_name]]))
  
  View(result)
  return(result)
}

datasets_to_check <- list(
  player_valuations = player_valuations,
  players = players,
  appearances = appearances,
  clubs = clubs
)

# Display missing percentages for each dataset
lapply(datasets_to_check, function(data) {
  missing_percentage(data)
})

lapply(datasets_to_check, summary)

# ----- Extracting Player Career Statistics from appearances dataset --------

season_dates <- data.frame(
  league = c("ES1", "L1", "NL1", "PO1"),
  start_date = as.Date(c(
    "2018-08-17", "2018-08-24", "2018-08-10", "2018-08-10", # 18/19
    "2019-08-16", "2019-08-16", "2019-08-02", "2019-08-09", # 19/20
    "2020-09-12", "2020-09-18", "2020-09-12", "2020-09-18", # 20/21
    "2021-08-13", "2021-08-13", "2021-08-14", "2021-08-06", # 21/22
    "2022-08-12", "2022-08-05", "2022-08-05", "2022-08-05", # 22/23
    "2023-08-11", "2023-08-18", "2023-08-11", "2023-08-11"  # 23/24
  )),
  end_date = as.Date(c(
    "2019-05-19", "2019-05-18", "2019-05-15", "2019-05-19", # 18/19
    "2020-05-24", "2020-06-27", "2020-04-24", "2020-07-26", # 19/20
    "2021-05-23", "2021-05-22", "2021-05-16", "2021-05-19", # 20/21
    "2022-05-22", "2022-05-14", "2022-05-15", "2022-05-15", # 21/22
    "2023-06-04", "2023-05-27", "2023-05-28", "2023-05-27", # 22/23
    "2024-05-26", "2024-05-18", "2024-05-19", "2024-05-19"  # 23/24
  )),
  season = c(
    "18/19", "18/19", "18/19", "18/19",
    "19/20", "19/20", "19/20", "19/20",
    "20/21", "20/21", "20/21", "20/21",
    "21/22", "21/22", "21/22", "21/22",
    "22/23", "22/23", "22/23", "22/23",
    "23/24", "23/24", "23/24", "23/24"
  )
)

# Filter the data for the Dutch league
dutch_league_dates <- season_dates[season_dates$league == "NL1", ]

# Filter appearances using dutch_league_dates
filtered_appearances <- appearances %>%
  left_join(dutch_league_dates, by = c("competition_id" = "league"), relationship = "many-to-many") %>%
  filter(date >= start_date & date <= end_date)  # Within season dates

# Aggregate stats per season
career_stats_per_season <- filtered_appearances %>%
  group_by(player_id, season, player_club_id, competition_id, player_name) %>% # Keep club and competition IDs
  summarise(
    total_red_cards = sum(red_cards, na.rm = TRUE),
    total_yellow_cards = sum(yellow_cards, na.rm = TRUE),
    total_goals = sum(goals, na.rm = TRUE),
    total_assists = sum(assists, na.rm = TRUE),
    total_minutes_played = sum(minutes_played, na.rm = TRUE),
    total_matches = n()
  ) %>%
  filter(total_minutes_played >= 90)  # Exclude players with < 90 minutes

# Merge with club names
career_stats_with_club <- career_stats_per_season %>%
  left_join(clubs %>% select(club_id, name), by = c("player_club_id" = "club_id")) %>%
  rename(club_name = name)

# ----- Combining stats from players and appearances dataset --------

# Extract age from DoB
players$age <- year(as.period(interval(start=players$date_of_birth, end=Sys.Date())))

# Select the required columns
players_filtered <- players %>%
  select(
    player_id,              # Include player ID for merging later
    name,                   # Player name
    country_of_citizenship, # Nationality
    age,                    # Age
    position,               # Position
    foot,                   # Preferred foot
    height_in_cm,           # Height
  )

# Merge player characteristics
final_table <- career_stats_with_club %>%
  left_join(players %>% 
              select(player_id, country_of_citizenship, age, position, foot, height_in_cm),
            by = "player_id")

# ----- Combining previous and current market value from player valuations dataset to main table  --------

# Assign seasons to player valuations
player_valuations1 <- player_valuations %>%
  mutate(season = case_when(
    date >= as.Date("2016-06-01") & date <= as.Date("2017-05-31") ~ "16/17",
    date >= as.Date("2017-06-01") & date <= as.Date("2018-05-31") ~ "17/18",
    date >= as.Date("2018-06-01") & date <= as.Date("2019-05-31") ~ "18/19",
    date >= as.Date("2019-06-01") & date <= as.Date("2020-05-31") ~ "19/20",
    date >= as.Date("2020-06-01") & date <= as.Date("2021-05-31") ~ "20/21",
    date >= as.Date("2021-06-01") & date <= as.Date("2022-05-31") ~ "21/22",
    date >= as.Date("2022-06-01") & date <= as.Date("2023-05-31") ~ "22/23",
    date >= as.Date("2023-06-01") & date <= as.Date("2024-05-31") ~ "23/24",
    TRUE ~ NA_character_
  )) %>%
  arrange(player_id, season, desc(date)) %>%
  group_by(player_id, season) %>%
  slice(1) %>%
  ungroup()

# Add previous season to final table
final_table1 <- final_table %>%
  mutate(previous_season = case_when(
    season == "18/19" ~ "17/18",
    season == "19/20" ~ "18/19",
    season == "20/21" ~ "19/20",
    season == "21/22" ~ "20/21",
    season == "22/23" ~ "21/22",
    season == "23/24" ~ "22/23",
    TRUE ~ NA_character_
  ))

# Join previous market values
previous_market_values <- player_valuations1 %>%
  select(player_id, season, market_value_in_eur) %>%
  rename(previous_season = season, previous_market_value = market_value_in_eur)

final_table_with_values <- final_table1 %>%
  left_join(previous_market_values, by = c("player_id", "previous_season"), relationship = "many-to-many")

# Join current market values
current_market_values <- player_valuations1 %>%
  select(player_id, season, market_value_in_eur) %>%
  rename(current_market_value = market_value_in_eur)

final_table_with_values <- final_table_with_values %>%
  left_join(current_market_values, by = c("player_id", "season"), relationship = "many-to-many")

# Final cleanup
final_table_with_values <- final_table_with_values %>%
  select(-previous_season)  

# missing_percentage(final_table_with_values) 
# Missing data: previous_market_value (5.07%), country_of_citizenship (2.16%), height_in_cm (1.74%), foot (1.50%)
# current_market_value (0.31%)

# ----- Data Cleaning (position) --------
final_table_with_values <- final_table_with_values %>%
  mutate(position = ifelse(position == "Missing", NA, position))

# check_missing_values(final_table_with_values, "position")

# Missing position: Anco Jansen, Lars Hutten, James Efmorfidis 

final_table_with_values <- final_table_with_values %>%
  mutate(
    position = case_when(
      player_id == 56607 ~ "Attack",       
      player_id == 85670 ~ "Attack",         
      player_id == 261071 ~ "Attack",      
      TRUE ~ position  
    )
  )

# Checking for values labelled as "Missing"
#print(unique(final_table_with_values$position)) 

# ----- Data Cleaning (height) --------

# summary(final_table_with_values$height_in_cm)
# Median = 183.0, Mean = 182.3, NA's = 45, Min = 163.0, Max = 206.0

hist(final_table_with_values$height_in_cm, 
     main = "Distribution of Player Heights", 
     xlab = "Height (cm)", 
     ylab = "Frequency", 
     col = "skyblue", 
     border = "black", 
     breaks = 20)

# skewness(final_table_with_values$height_in_cm, na.rm = TRUE) # Skew = -0.007079585
# names(sort(-table(final_table_with_values$height_in_cm)))[1] # Mode = 185

mean_height <- median(final_table_with_values$height_in_cm, na.rm = TRUE)

final_table_with_values <- final_table_with_values %>%
  mutate(height_in_cm = ifelse(is.na(height_in_cm), mean_height, height_in_cm))

# ----- Data Cleaning (foot) --------

# check_missing_values(final_table_with_values, "foot")
# table(final_table_with_values$foot) # both = 98, left = 705, right = 1741
# prop.table(table(final_table_with_values$foot)) # both = 0.03852201, left = 0.27712264, right = 0.68435535

# Calculate mode
mode_foot <- names(sort(-table(final_table_with_values$foot)))[1]

# Impute missing values in 'foot' with the mode
final_table_with_values <- final_table_with_values %>%
  mutate(foot = ifelse(is.na(foot), mode_foot, foot))

# ----- Data Cleaning (Nationality) --------

# check_missing_values(final_table_with_values, "country_of_citizenship")
# table(final_table_with_values$country_of_citizenship)
# missing_percentage(final_table_with_values) # missing values: 2.16802168%, 23 players missing nationality

# Remove rows where 'country_of_citizenship' is NA
final_table_with_values <- final_table_with_values %>%
  filter(!is.na(country_of_citizenship))

# ----- Data Cleaning (Current market value) --------

# Filter out rows with missing current market value
final_table_with_values <- final_table_with_values %>%
  filter(!is.na(current_market_value))

# ----- Data Cleaning (Previous market value) --------

# summary(final_table_with_values$previous_market_value)
# Median = 750000, Mean = 2220656, NA's = 125, Min = 25000, Max = 36000000

hist(final_table_with_values$previous_market_value, 
     main = "Distribution of player's prior \n season's market value", 
     xlab = "Player's Prior Season Market Value (in Euros)", 
     ylab = "Frequency", 
     col = "skyblue", 
     border = "black", 
     breaks = 20)

median_previous_market_value <- median(final_table_with_values$previous_market_value, na.rm = TRUE)

final_table_with_values <- final_table_with_values %>%
  mutate(previous_market_value = ifelse(is.na(previous_market_value), median_previous_market_value, previous_market_value))

# ----- Saving final dataset --------

write.csv(final_table_with_values, "cleaned_dataset.csv", row.names = FALSE)
