# multiple-linear-regression-prestige-r
Multiple linear regression analysis in R using the Prestige dataset, including EDA, model comparison, diagnostics, and predictions.
# Multiple Linear Regression – Occupational Prestige (R)

## Project Overview
This project performs a **multiple linear regression analysis** using the
`Prestige` dataset from the **car** package in R.

The objective is to **model and predict occupational prestige** based on:
- Years of education
- Average income
- Percentage of women in the occupation

This project was developed as an **academic portfolio example**, with a strong
focus on statistical reasoning, interpretability, and model evaluation.

---

## Dataset
- **Source:** `Prestige` dataset (`car` package)
- **Observations:** 102 occupations
- **Variables used:**
  - `education`: Average years of education
  - `income`: Average income (USD)
  - `women`: Percentage of women in the occupation
  - `prestige`: Occupational prestige score (target variable)

---

## Methodology
The analysis follows a complete and structured workflow:

1. Exploratory Data Analysis (EDA)
   - Histograms
   - Boxplots
   - Scatter plots
   - Correlation analysis
2. Construction of multiple linear regression models
3. Model comparison using:
   - R²
   - Adjusted R²
   - AIC / BIC
4. Diagnostic analysis and assumption checking
5. Selection of the best model
6. Prediction of new hypothetical cases

---

## Regression Model
The general form of the model is:

**Prestige = β₀ + β₁(Education) + β₂(Income) + β₃(Women)**

### Key findings:
- **Education** is the most influential predictor of occupational prestige
- **Income** is also statistically significant
- Percentage of women shows a negative association with prestige
- The selected model explains a high proportion of variance (R² > 0.7)

---

## Predictions
The final model is used to predict occupational prestige for new hypothetical
cases with different education and income levels, including prediction
intervals at 95% confidence.

---

## Tools & Libraries
- **R**
- `car`
- `corrplot`

---


---

## Notes
This project prioritizes **clarity, statistical validity, and interpretability**
over model complexity. It is intended as an introductory portfolio project in
data analysis and applied statistics.
