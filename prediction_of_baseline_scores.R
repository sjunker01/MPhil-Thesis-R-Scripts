# Load necessary libraries
library(tidyverse)
library(stargazer)


# For this analysis, we use the dataset which contains data for all the people, even the ones who dropped out after answering
# the first questionnaire. Load the dataset
wide <- read_csv("data_input/mindful_drop.csv")


# Create function to round numeric values in a heterogenic data frame to x digits (to visualise outputs later)
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


### Modelling
## CORE-OM
# Create linear model predicting CORE-OM scores at baseline from all available predictors
lm.core <- lm(core_base ~ age + gender + intake + disability + ethnicity + med_base,
              data = wide)
summary(lm.core)


# Extract statistical measures including 95% confidence intervals, computed using the confint() function
stargazer(cbind(Estimate = coef(lm.core), Std.Error = coef(summary(lm.core))[,2],
                z.value = coef(summary(lm.core))[,3], confint(lm.core),
                p_value = coef(summary(lm.core))[,4]), type = "text", style = "qje", digits = 3)


## WEMWBS
# Create linear model predicting WEMWBS scores at baseline from all available predictors
lm.wemwbs <- lm(wb_base ~ age + gender + intake + disability + ethnicity + med_base,
                data = wide)
summary(lm.wemwbs)

# Extract statistical measures including 95% confidence intervals
stargazer(cbind(Estimate = coef(lm.wemwbs), Std.Error = coef(summary(lm.wemwbs))[,2],
                z.value = coef(summary(lm.wemwbs))[,3], confint(lm.wemwbs),
                p_value = coef(summary(lm.wemwbs))[,4]), type = "text", style = "qje", digits = 3)


