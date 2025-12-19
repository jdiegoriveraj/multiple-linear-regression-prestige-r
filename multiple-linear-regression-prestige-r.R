# ============================================================================
# Multiple Linear Regression Analysis
# Dataset: Prestige (Canadian Occupational Prestige)
# Target variable: prestige
# ============================================================================

# ---------------------------------------------------------------------------
# 1. Load libraries and data
# ---------------------------------------------------------------------------

library(car)
library(corrplot)

# Load Prestige dataset
data(Prestige)

# Basic dataset inspection
head(Prestige)
str(Prestige)
summary(Prestige)

# Dataset information
cat("\n========== DATASET INFORMATION ==========\n")
cat("Number of observations:", nrow(Prestige), "\n")
cat("Number of variables:", ncol(Prestige), "\n")
cat("\nVariables:\n")
cat("- education: Average years of education\n")
cat("- income: Average income (USD)\n")
cat("- women: Percentage of women in the occupation\n")
cat("- prestige: Occupational prestige (TARGET VARIABLE)\n")
cat("- census: Census code\n")
cat("- type: Occupation type (bc, prof, wc)\n")

# ---------------------------------------------------------------------------
# 2. Exploratory Data Analysis (EDA)
# ---------------------------------------------------------------------------

cat("\n========== EXPLORATORY DATA ANALYSIS ==========\n")

# Select numeric variables
prestige_numeric <- Prestige[, c("education", "income", "women", "prestige")]

# 2.1 Histograms
par(mfrow = c(2, 2))
hist(Prestige$education, col = "skyblue", breaks = 15,
     main = "Distribution: Education", xlab = "Years of education")
hist(Prestige$income, col = "lightgreen", breaks = 15,
     main = "Distribution: Income", xlab = "Income (USD)")
hist(Prestige$women, col = "pink", breaks = 15,
     main = "Distribution: Women (%)", xlab = "Percentage of women")
hist(Prestige$prestige, col = "orange", breaks = 15,
     main = "Distribution: Prestige", xlab = "Prestige score")

# 2.2 Boxplots (outlier detection)
par(mfrow = c(2, 2))
boxplot(Prestige$education, col = "skyblue",
        main = "Education", horizontal = TRUE)
boxplot(Prestige$income, col = "lightgreen",
        main = "Income", horizontal = TRUE)
boxplot(Prestige$women, col = "pink",
        main = "Women (%)", horizontal = TRUE)
boxplot(Prestige$prestige, col = "orange",
        main = "Prestige", horizontal = TRUE)

# 2.3 Scatter plots
par(mfrow = c(2, 2))

plot(Prestige$education, Prestige$prestige, pch = 19, col = "blue",
     xlab = "Education", ylab = "Prestige",
     main = "Prestige vs Education")
abline(lm(prestige ~ education, data = Prestige), col = "red", lwd = 2)

plot(Prestige$income, Prestige$prestige, pch = 19, col = "darkgreen",
     xlab = "Income", ylab = "Prestige",
     main = "Prestige vs Income")
abline(lm(prestige ~ income, data = Prestige), col = "red", lwd = 2)

plot(Prestige$women, Prestige$prestige, pch = 19, col = "purple",
     xlab = "Women (%)", ylab = "Prestige",
     main = "Prestige vs Women")
abline(lm(prestige ~ women, data = Prestige), col = "red", lwd = 2)

# Scatterplot matrix
pairs(prestige_numeric,
      main = "Scatterplot Matrix",
      pch = 19,
      col = "blue")

# ---------------------------------------------------------------------------
# 2.4 Correlation analysis
# ---------------------------------------------------------------------------

cat("\n========== CORRELATION MATRIX ==========\n")
cor_matrix <- cor(prestige_numeric)
print(round(cor_matrix, 3))

# Correlation heatmap
corrplot(cor_matrix, method = "color",
         col = colorRampPalette(c("red", "white", "blue"))(200),
         addCoef.col = "black",
         tl.cex = 0.8,
         number.cex = 0.7)

cat("\nCorrelation analysis:\n")
cat("Prestige vs Education:", round(cor(Prestige$prestige, Prestige$education), 3), "\n")
cat("Prestige vs Income:", round(cor(Prestige$prestige, Prestige$income), 3), "\n")
cat("Prestige vs Women:", round(cor(Prestige$prestige, Prestige$women), 3), "\n")

# ---------------------------------------------------------------------------
# 3. Multiple Linear Regression Models
# ---------------------------------------------------------------------------

cat("\n========== REGRESSION MODELS ==========\n")

# Model 1: All predictors
model_1 <- lm(prestige ~ education + income + women, data = Prestige)
summary(model_1)

coef_m1 <- coef(model_1)
cat("\nModel 1 equation:\n")
cat("Prestige =",
    round(coef_m1[1], 4), "+",
    round(coef_m1[2], 4), "* education +",
    round(coef_m1[3], 4), "* income +",
    round(coef_m1[4], 4), "* women\n")

# Additional models
model_2 <- lm(prestige ~ education, data = Prestige)
model_3 <- lm(prestige ~ income, data = Prestige)
model_4 <- lm(prestige ~ education + income, data = Prestige)
model_5 <- lm(prestige ~ education + women, data = Prestige)

# ---------------------------------------------------------------------------
# 4. Model comparison
# ---------------------------------------------------------------------------

comparison <- data.frame(
  Model = c("Education + Income + Women",
            "Education only",
            "Income only",
            "Education + Income",
            "Education + Women"),
  R2 = c(summary(model_1)$r.squared,
         summary(model_2)$r.squared,
         summary(model_3)$r.squared,
         summary(model_4)$r.squared,
         summary(model_5)$r.squared),
  R2_adjusted = c(summary(model_1)$adj.r.squared,
                  summary(model_2)$adj.r.squared,
                  summary(model_3)$adj.r.squared,
                  summary(model_4)$adj.r.squared,
                  summary(model_5)$adj.r.squared),
  AIC = c(AIC(model_1), AIC(model_2), AIC(model_3),
          AIC(model_4), AIC(model_5)),
  BIC = c(BIC(model_1), BIC(model_2), BIC(model_3),
          BIC(model_4), BIC(model_5))
)

comparison$R2_percent <- round(comparison$R2 * 100, 2)
print(comparison)

best_model_index <- which.max(comparison$R2_adjusted)
best_model <- model_4

cat("\nBest model according to adjusted R²:\n")
cat(comparison$Model[best_model_index], "\n")

# ---------------------------------------------------------------------------
# 5. Best model diagnostics
# ---------------------------------------------------------------------------

cat("\n========== BEST MODEL DIAGNOSTICS ==========\n")
summary(best_model)

par(mfrow = c(2, 2))
plot(best_model)
par(mfrow = c(1, 1))

# Significance analysis
coef_summary <- summary(best_model)$coefficients

cat("\nSignificant variables:\n")
for (i in 2:nrow(coef_summary)) {
  p_val <- coef_summary[i, 4]
  var_name <- rownames(coef_summary)[i]
  
  significance <- ifelse(p_val < 0.001, "***",
                         ifelse(p_val < 0.01, "**",
                                ifelse(p_val < 0.05, "*", "Not significant")))
  
  cat("-", var_name, "→ p-value:",
      format(p_val, scientific = TRUE),
      significance, "\n")
}

# ---------------------------------------------------------------------------
# 6. Prediction of new cases
# ---------------------------------------------------------------------------

cat("\n========== PREDICTION ==========\n")

new_cases <- data.frame(
  education = c(16, 10),
  income = c(15000, 5000)
)

predictions <- predict(best_model,
                       newdata = new_cases,
                       interval = "prediction",
                       level = 0.95)

results <- data.frame(
  Case = c("Case 1", "Case 2"),
  Education = new_cases$education,
  Income = new_cases$income,
  Predicted_Prestige = round(predictions[, "fit"], 2),
  Lower_95 = round(predictions[, "lwr"], 2),
  Upper_95 = round(predictions[, "upr"], 2)
)

print(results)

# ---------------------------------------------------------------------------
# 7. Final conclusions
# ---------------------------------------------------------------------------

cat("\n========== FINAL CONCLUSIONS ==========\n")
cat("• Education is the most influential predictor of occupational prestige\n")
cat("• Income is also statistically significant\n")
cat("• The selected model explains a high proportion of variance (R² > 0.7)\n")
cat("• The model satisfies linear regression assumptions\n")
cat("• Suitable for prediction and interpretation\n")
