# Install required packages
install.packages(c("car", "corrplot", "caret", "readxl", "psych", "ggplot2", 
                   "datarium", "tidyverse", 
                   "rcompanion", "RVAideMemoire", "carData", "lattice", 
                   "glmnet"))

# Load libraries
library(car)
library(corrplot)
library(caret)
library(readxl)
library(psych)
library(ggplot2)
library(datarium)
library(tidyverse)
library(rcompanion)
library(RVAideMemoire)
library(carData)
library(lattice)
library(glmnet)


# Load the dataset
library(readxl)
data <- read_excel("concrete compressive strength.xlsx")

# Rename columns for clarity
colnames(data) <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", 
                    "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", 
                    "Age", "Concrete_Category", "Contains_Fly_Ash", "Concrete_Strength")

# Convert necessary columns to factors
data$Age <- as.factor(data$Age)
data$Contains_Fly_Ash <- as.factor(data$Contains_Fly_Ash)

# Display summary statistics
head(data)
summary(data)


library(ggplot2)
ggplot(data, aes(x = Concrete_Strength)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Concrete Strength", 
       x = "Concrete Strength", y = "Frequency")

ggplot(data, aes(x = Cement, y = Concrete_Strength)) +
  geom_point(color = "red", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Cement vs. Concrete Strength", 
       x = "Cement Content", y = "Concrete Strength")



# Select numeric columns
numeric_cols <- sapply(data, is.numeric)
numeric_data <- data[, numeric_cols]

# Calculate Z-scores
z_scores <- as.data.frame(scale(numeric_data))

# Identify rows with any Z-score > 3 or < -3
outliers <- which(apply(z_scores, 1, function(row) any(abs(row) > 3)))
print("Rows with Potential Outliers:")
print(outliers)

# Boxplots for numeric variables
for (col in names(numeric_data)) {
  print(
    ggplot(data, aes_string(y = col)) +
      geom_boxplot(fill = "red", alpha = 0.7) +
      labs(title = paste("Boxplot of", col), y = col) +
      theme_minimal()
  )
}

# Remove rows with Z-scores > 3 or < -3
data <- data[!apply(z_scores, 1, function(row) any(abs(row) > 3)), ]

# Check the dimensions of the cleaned data
print(dim(data))


# doing correlation
pearson_corr <- cor(numeric_data, use = "complete.obs", method = "pearson")
spearman_corr <- cor(numeric_data, use = "complete.obs", method = "spearman")

# Print correlations
print(round(pearson_corr, 2))
print(round(spearman_corr, 2))

# Visualize correlation matrix
library(corrplot)
corrplot(cor(numeric_data), method = "number", type = "upper")


# Load required library
library(ggplot2)
library(reshape2)


# Melt the correlation matrix for ggplot2
melted_cor_matrix <- melt(pearson_corr)

# Create the heatmap
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables")


# Shapiro-Wilk test for Concrete Strength
shapiro_test <- shapiro.test(data$Concrete_Strength)
print(shapiro_test)

# Kolmogorov-Smirnov test
ks_test <- ks.test(data$Concrete_Strength, "pnorm", 
                   mean(data$Concrete_Strength), 
                   sd(data$Concrete_Strength))
print(ks_test)

# Log transformation
data$Log_Concrete_Strength <- log(data$Concrete_Strength)
shapiro_test_log <- shapiro.test(data$Log_Concrete_Strength)

# Square root transformation
data$Sqrt_Concrete_Strength <- sqrt(data$Concrete_Strength)
shapiro_test_sqrt <- shapiro.test(data$Sqrt_Concrete_Strength)

# Cube root transformation
data$CubeRoot_Concrete_Strength <- data$Concrete_Strength^(1/3)
shapiro_test_cube <- shapiro.test(data$CubeRoot_Concrete_Strength)

# Print results
print(shapiro_test_log)
print(shapiro_test_sqrt)
print(shapiro_test_cube)

# Combine data for QQ plotting
qq_plot_data <- data.frame(
  Value = c(data$Concrete_Strength, data$Log_Concrete_Strength, 
            data$Sqrt_Concrete_Strength, data$CubeRoot_Concrete_Strength),
  Transformation = rep(c("Original", "Log", "Square Root", "Cube Root"), 
                       each = nrow(data))
)

# Create the QQ plots
ggplot(qq_plot_data, aes(sample = Value)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red", linetype = "dashed") +
  facet_wrap(~ Transformation, scales = "free") +
  labs(title = "QQ Plots of Concrete Strength (Original and Transformed)",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()



# One-sample t-test
t_test_one_sample <- t.test(data$Concrete_Strength, mu = 30)
print(t_test_one_sample)

# Two-sample t-test
t_test_two_sample <- t.test(Concrete_Strength ~ Contains_Fly_Ash, data = data)
print(t_test_two_sample)


# One-way ANOVA
one_way_anova <- aov(Concrete_Strength ~ Age, data = data)
summary(one_way_anova)

# Tukey HSD Post Hoc
tukey_one_way <- TukeyHSD(one_way_anova)
print(tukey_one_way)

# Two-way ANOVA
two_way_anova <- aov(Concrete_Strength ~ Age * Contains_Fly_Ash, data = data)
summary(two_way_anova)

# Interaction plot
interaction.plot(data$Age, data$Contains_Fly_Ash, data$Concrete_Strength,
                 col = c("red", "blue"), lty = 1, pch = c(16, 18),
                 main = "Interaction Plot of Age and Fly Ash on Concrete Strength",
                 xlab = "Age", ylab = "Concrete Strength")


linear_model <- lm(Sqrt_Concrete_Strength ~ Cement, data = data)
summary(linear_model)

# Visualize regression
ggplot(data, aes(x = Cement, y = Sqrt_Concrete_Strength)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Linear Regression: Sqrt(Concrete Strength) vs. Cement", 
       x = "Cement Content", y = "Sqrt(Concrete Strength)") +
  theme_minimal()



multiple_model <- lm(Sqrt_Concrete_Strength ~ Cement + Water + Age + Blast_Furnace_Slag + Fly_Ash + Superplasticizer, data = data)
summary(multiple_model)

# Residual diagnostics
par(mfrow = c(2, 2))
plot(multiple_model)

# Calculate VIF
library(car)
vif_values <- vif(multiple_model)
print(vif_values)



# Prepare data
X <- model.matrix(Sqrt_Concrete_Strength ~ Cement + Water + Age + Blast_Furnace_Slag + Fly_Ash + Superplasticizer, data = data)[, -1]
y <- data$Sqrt_Concrete_Strength

# Ridge regression
library(glmnet)
ridge_model <- glmnet(X, y, alpha = 0)
ridge_cv <- cv.glmnet(X, y, alpha = 0)
best_lambda_ridge <- ridge_cv$lambda.min
ridge_coefficients <- coef(ridge_model, s = best_lambda_ridge)
print(ridge_coefficients)

# Lasso regression
lasso_model <- glmnet(X, y, alpha = 1)
lasso_cv <- cv.glmnet(X, y, alpha = 1)
best_lambda_lasso <- lasso_cv$lambda.min
lasso_coefficients <- coef(lasso_model, s = best_lambda_lasso)
print(lasso_coefficients)

# Predictions
ridge_predictions <- predict(ridge_model, s = best_lambda_ridge, newx = X)
lasso_predictions <- predict(lasso_model, s = best_lambda_lasso, newx = X)

# Mean Squared Error (MSE)
ridge_mse <- mean((y - ridge_predictions)^2)
lasso_mse <- mean((y - lasso_predictions)^2)

# Comparison table
model_comparison <- data.frame(
  Model = c("Linear Regression", "Multiple Regression", "Ridge Regression", "Lasso Regression"),
  Adjusted_R2 = c(summary(linear_model)$adj.r.squared, summary(multiple_model)$adj.r.squared, NA, NA),
  MSE = c(sigma(linear_model)^2, sigma(multiple_model)^2, ridge_mse, lasso_mse)
)

print(model_comparison)


