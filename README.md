# INF6027 Introduction to Data Science (AUTUMN 2024~25)

## Dutch Eredivisie Player Market Value Analysis

This project explores the factors influencing football players' market values in the Dutch Eredivisie league from the 2018/19 to 2023/24 seasons. The project uses data from [Kaggle](https://www.kaggle.com/datasets/davidcariboo/player-scores), accessed on November 10, 2024. This dataset includes player characteristics, performance metrics, and prior market valuations. The study employs both Multiple Linear Regression (MLR) and Random Forest (RF) to identify key predictors and evaluate their impact on market value.

## Research Questions 

- **What are the key player characteristics and performance metrics that influence football players' market values in the Dutch Eredivisie league?**
- **How can machine learning techniques, such as random forests, improve the prediction of football player market values?**

## Key Findings 

- Previous market value was the strongest predictor of a player’s current market value, as shown by both the Multiple Linear Regression (MLR) and Random Forest (RF) models, aligning with prior research. 
- Performance metrics such as goals, assists, minutes played, and yellow cards positively influenced market value, while age negatively affected it, consistent with previous studies.  
- Height and footedness were found to be insignificant predictors, perhaps due to the dominance of prior market value in the MLR model.
- Red cards were deemed insignificant, possibly due to their infrequent occurrence.
- Nationality also proved insignificant, potentially reflecting the Eredivisie’s focus on talent development.
- Both models showed similar accuracy, with MLR explaining 68.8% of the variance and RF explaining 66.4%.

## Instructions for Downloading and Running the Code 

### Prerequisites

- Ensure that **R** and **RStudio** are installed on your system.
- Install the necessary R libraries listed in each script.

### Steps

1. Download the repository as a ZIP file and extract it.
2. Download the dataset from [this Google Drive link](https://drive.google.com/drive/folders/1WN5OKXjajaXnlEzohpFEapYtmp38itX4?usp=sharing).
3. Place the CSV files in the main project directory (the same directory as the `.Rproj` file).
4. Open the `Introduction_to_data_science.Rproj` file in RStudio. This will set the correct working directory.

### Script Execution Order

To replicate the analysis, execute the scripts in the following order:

1. **`data_cleaning6.R`**: Cleans and preprocesses the raw data, saving the cleaned dataset as a CSV file.
2. **`data_EDA3.R`**: Performs exploratory data analysis, including visualizations and summary statistics.
3. **`data_transform3.R`**: Encodes categorical variables, handles multicollinearity, and prepares the dataset for modeling.
4. **`mlr_model.R`**: Fits the Multiple Linear Regression (MLR) model and evaluates its performance.
5. **`random_forest_model.R`**: Fits the Random Forest (RF) model, calculates feature importance, and evaluates its performance.

### Outputs

- **Visualizations**: Scatter plots, histograms, and feature importance charts will appear in the RStudio Plots pane.
- **Statistical Outputs**: Model summaries, evaluation metrics (MSE, RMSE, $R^2$) and other statistical outputs will be displayed in the RStudio Console.
- **Saved Data**: Cleaned and transformed datasets will be saved as CSV files in the project directory.
