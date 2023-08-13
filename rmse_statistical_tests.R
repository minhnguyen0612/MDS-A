library(tidyr)
library(car)
library(FSA)

# Read the CSV containing all the RMSE scores returned by the sampling methods
all_rmse_records <- read.csv("supplementary/all_rmse_records.csv")

# Converting the 'Sampling_strategy' variable to a factor
all_rmse_records$Sampling_strategy <- as.factor(all_rmse_records$Sampling_strategy)

# Run an ANOVA model
rmse_anova = aov(RMSE_score ~ Sampling_strategy, data = all_rmse_records)

# Get the Q-Q plot to check the normality assumption of ANOVA
plot(rmse_anova, 2)
# Get the residuals-versus-fits plot to check the homoscedasticity assumption of ANOVA
plot(rmse_anova, 1)

# Check the normality assumption with a Shapiro-Wilk test
shapiro.test(rmse_anova$residuals)
# Check the homoscedasticity assumption with a Shapiro-Wilk test
leveneTest(RMSE_score ~ Sampling_strategy, data = all_rmse_records)

# Because of the ANOVA assumption violations, perform a Kruskal-Wallis test
kruskal.test(RMSE_score ~ Sampling_strategy, data = all_rmse_records)

# As the Kruskal-Wallis test shows an overall difference, run a post-hoc analysis with a Dunn's test
dunn_test <- dunnTest(all_rmse_records$RMSE_score, all_rmse_records$Sampling_strategy, method = "holm")

# Export the Dunn's test results as a CSV file for visualisation in Python
dunn_test$res |> data.frame() |> write.csv("supplementary/dunn_test.csv", row.names = FALSE)
