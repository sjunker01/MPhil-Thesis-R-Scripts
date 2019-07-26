library(tidyverse)


# For this analysis, use the datasets which have all the people, even the ones who dropped
# out after the first questionnaire
# Also make sure arm and gender are coded as factors
wide <- read_csv("data_input(4)/mindful_drop(att).csv", 
                 col_types = cols(arm = col_factor(levels = c("intervention", 
                                                              "control", "MMJ")),
                                  gender = col_factor(levels = c("Female", "Male"))))


# Create function to round numeric values in a heterogenic data frame to x digits
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


wide <- wide %>% select(-X1)
wide$cohort <- with(wide, ifelse(cohort == "michaelmas", "Michaelmas",
                                 ifelse(cohort == "lent", "Lent",
                                        ifelse(cohort == "Michelmas", "Michaelmas", cohort))))
wide$cohort <- factor(wide$cohort, levels = c("Michaelmas", "Lent"))
wide$ethnicity <- with(wide, ifelse(ethnicity == "White", "White", "Non-White"))
wide$ethnicity <- factor(wide$ethnicity, levels = c("White", "Non-White"))

# Rename arm
wide <- wide %>% 
  mutate(intake = ifelse(arm == "intervention", "Intake 1",
                         ifelse(arm == "control", "Intake 2",
                                ifelse(arm == "MMJ", "Intake 3", NA))))
wide$intake <- factor(wide$intake, levels = c("Intake 1", "Intake 2", "Intake 3"))


### Modelling


### First, check whether anything has an effect on the scores at baseline
lm.core <- lm(core_base ~ age + gender + intake + disability + ethnicity + med_base,
              data = wide)
summary(lm.core)

# Extract statistical measures
stargazer(cbind(Estimate = coef(lm.core), Std.Error = coef(summary(lm.core))[,2],
                z.value = coef(summary(lm.core))[,3], confint(lm.core),
                p_value = coef(summary(lm.core))[,4]), type = "text", style = "qje", digits = 3)

output.core <- round_df(cbind(Estimate = coef(lm.core), Std.Error = coef(summary(lm.core))[,2],
                     confint(lm.core), p_value = coef(summary(lm.core))[,4]), 3)

write.csv(output.core, file = "model_output/1.1_predictors_core.csv")


# On average, the CORE score at baseline is higher if you have a disability.
# Which type of disability is significant?
wide$disability_type <- factor(wide$disability_type, levels = c("No disability", "Mental",
                                                                "Physical", "Learning difficulty",
                                                                "Other"))
lm.core.2 <- lm(core_base ~ intake + ethnicity + age + gender + disability_type + med_base + cohort,
              data = wide) # n = 520
summary(lm.core.2)
# Only the mental disability makes a difference! If you have a mental disability, your baseline
# CORE score is 0.58 higher than if you didn't have any.
table(wide$disability_type) # Keep in mind we don't have many people for each of the disabilities.


### WEMWBS
lm.wemwbs <- lm(wb_base ~ age + gender + intake + disability + ethnicity + med_base,
                data = wide) # n = 519
summary(lm.wemwbs)

# Extract statistical measures
stargazer(cbind(Estimate = coef(lm.wemwbs), Std.Error = coef(summary(lm.wemwbs))[,2],
                z.value = coef(summary(lm.wemwbs))[,3], confint(lm.wemwbs),
                p_value = coef(summary(lm.wemwbs))[,4]), type = "text", style = "qje", digits = 3)

output.wemwbs <- round_df(cbind(Estimate = coef(lm.wemwbs), Std.Error = coef(summary(lm.wemwbs))[,2],
                     confint(lm.wemwbs), p_value = coef(summary(lm.wemwbs))[,4]), 3)

write.csv(output.wemwbs, file = "model_output/1.1_predictors_wemwbs.csv")
# WEMWBS is also lower in people with disability (-5 for mental).
# The arm seems to have an effect here as well.

