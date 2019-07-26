library(MASS)
library(tidyverse)
library(sandwich)
library(fmsb)
library(AER)


# For this analysis, use the datasets which have all the people, even the ones who dropped
# out after the first questionnaire; but also only include those controls who INTENDED to
# take the course!
# Also make sure arm and gender are coded as factors
wide <- read_csv("data_input(4)/mindful_drop(FUbias).csv", 
                 col_types = cols(arm = col_factor(levels = c("intervention", 
                                                              "control", "MMJ")),
                                  gender = col_factor(levels = c("Female", "Male"))))

# Create colors
### Create color palette (darkest)
cols <- c("#e47e32", "#ff9e1f", "#ae9764", "#719f8d", "#509094", "#d2c078")
col1 <- cols[1]
col2 <- cols[2]
col3 <- cols[3]
col4 <- cols[4]
col5 <- cols[5]
col6 <- cols[6]


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
wide$cohort <- with(wide, ifelse(cohort == "Michelmas", "michaelmas", cohort))
wide$ethnicity <- with(wide, ifelse(ethnicity == "White", "White", "Non-White"))
wide$ethnicity <- factor(wide$ethnicity, levels = c("White", "Non-White"))
wide$arm <- factor(wide$arm, levels = c("intervention", "control", "MMJ"))


## Check distribution of attendance
ggplot(wide, aes(sessions_attended)) +
  geom_histogram(bins = 17, fill = "lightgrey", color = "black") +
  theme_bw()
# Quite some people who attended 0 sessions.






##### Overall poisson model (exploratory)

# Create dataset which contains all tested variables
wide_p1 <- wide %>%
  filter(!is.na(age) & !is.na(gender) & !is.na(disability) & !is.na(ethnicity) & !is.na(med_base)
         & !is.na(wb_base))


# Start with the full model (n = 509)
glm.poisson <- glm(sessions_attended ~ arm + age + gender + disability + ethnicity
                     + med_base + core_base + wb_base,
                   family = "poisson", data = wide_p1)
summary(glm.poisson)


# Let's check for multicollinearity
lm.collinear.scores <- lm(core_base ~ wb_base, data = wide_p1)
VIF(lm.collinear.scores) # 1.97 (R^2 here is 0.49, it's not ideal... have separate models,
# especially since one of the mental health scores seems to be significantly predicting
# the outcome.


## Have separate models for CORE and WEMWBS

# CORE
# Create dataset which contains only participants who have data for all variables. (n = 521)
wide_pC <- wide %>% # n = 391
  filter(!is.na(age) & !is.na(gender) & !is.na(disability) & !is.na(ethnicity) & !is.na(med_base)
         & !is.na(core_base))

glm.p.core <- glm(sessions_attended ~ age + gender + arm + disability + ethnicity + med_base
                          + core_base,
                        family = "poisson", data = wide_pC)
summary(glm.p.core)

# Export relevant values
output.p.core <- round_df(cbind(Estimate = coef(glm.p.core),
                                 OR = exp(coef(glm.p.core)), exp(confint(glm.p.core)),
                                 p_value = coef(summary(glm.p.core))[,4]), 3)

write.csv(output.p.core, file = "model_output/1_p_core.csv")

# The dispersion parameter is 1.
# Poisson models assume that the variance and mean are equal.
# Quasi-poisson regressions assume the variance is 'only' proportional to the mean,
# but not necessarily the same.

# Test for overdispersion
dispersiontest(glm.p.core, trafo = 1)
# Definitely overdispersed. Need for quasipoisson

# Quasipoisson
glm.qp.core <- glm(sessions_attended ~ age + gender + arm + disability + ethnicity + med_base
                  + core_base,
                  family = "quasipoisson", data = wide_pC)
summary(glm.qp.core)
# Okay arm, disability, med_base and core_base are significant!?


# Extract OR, CIs, etc.
stargazer(cbind(Estimate = coef(glm.qp.core), Std.Error = coef(summary(glm.qp.core))[,2],
                z.value = coef(summary(glm.qp.core))[,3],
                OR = exp(coef(glm.qp.core)), exp(confint(glm.qp.core)),
                p_value = coef(summary(glm.qp.core))[,4]), type = "text", style = "qje", digits = 3)

# Export relevant values
output.qp.core <- round_df(cbind(Estimate = coef(glm.qp.core),
      OR = exp(coef(glm.qp.core)), exp(confint(glm.qp.core)),
      p_value = coef(summary(glm.qp.core))[,4]), 3)

write.csv(output.qp.core, file = "model_output/1_qp_core.csv")

# Interpretation of exp(estimate) = OR:
# The incident rate for the MMJ arm is 1.4 times the incident rate for the reference group (intervention).
# The percent change in the incident rate of attended sessions is by -16.2% for every unit
# increase in core_base in the intervention group ((0.838-1)*100 = -16.2).

# stepAIC not possible for quasipoisson models.



### Get overall significance of arm on the models

# Update model dropping arm
glm.noarm <- update(glm.qp.core, . ~ . - arm)
# Test model differences with chi square test
anova(glm.noarm, glm.qp.core, test="Chisq") # p = 0.001

# Using the Wald Test
wald.test(b = coef(glm.qp.core), Sigma = vcov(glm.qp.core), Terms = 4:5) # p = 9e -04



##### Model exploration

# Let's hold all variables by their means / by the factor which is used most and let arm differ.
wide_explore <- wide_pC %>% select(sessions_attended, age, gender, arm, ethnicity, disability,
                                             core_base, med_base)
summary(wide_explore)
test1 <- with(wide_explore, data.frame(age = mean(age, na.rm = TRUE), gender =  "Female",
                                       arm = c("control", "MMJ", "intervention"),
                                       disability = FALSE, ethnicity = "White",
                                       med_base = FALSE,
                                       core_base = mean(core_base, na.rm = TRUE)))

test1$sessionsP <- predict(glm.qp.core, newdata = test1, type = "response")
test1
# Seeing the differences within arms - e.g. 2.7 sessions attended when in control group,
# 6.4 sessions attended when in MMJ group.


# Let's hold all variables by their means / by the factor which is used most and let core_base differ.
summary(wide_explore)
test2 <- with(wide_explore, data.frame(age = mean(age, na.rm = TRUE), gender =  "Female",
                                       arm = "intervention",
                                       disability = FALSE, ethnicity = "White",
                                       med_base = FALSE,
                                       core_base = c(0,0.5,1,1.5,2,2.5,3)))
test2$sessionsP <- predict(glm.qp.core, newdata = test2, type = "response")
test2
# 5.4 sessions attended when core_base = 0, 3.2 attended when core_base = 3.




##### Create new dataset and plot predicted values for the test dataset.

test3 <- with(wide_explore, data.frame(age = mean(age, na.rm = TRUE), gender =  "Female",
                                       arm = factor(rep(c("control", "MMJ", "intervention"), each = 240)),
                                       disability = FALSE, ethnicity = "White", med_base = FALSE,
                                       core_base = rep(seq(from = 0, to = 3, length.out = 20),4)))


testdata <- cbind(test3, predict(glm.qp.core, newdata = test3, type = "response", se = TRUE))
testdata <- within(testdata, {
  PredictedSess <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Rename arm
testdata <- testdata %>% 
  mutate(Intake = ifelse(arm == "intervention", "Intake 1",
                         ifelse(arm == "control", "Intake 2",
                                ifelse(arm == "MMJ", "Intake 3", NA))))


# Plot for non-disabled, white females of the mean age.
ggplot(testdata, aes(x = core_base, y = PredictedSess)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Intake), alpha = 0.2) +
  geom_line(aes(colour = Intake), size = 1) +
  theme_bw() +
  scale_color_manual(values = c(col5, col2, "#e44932")) +
  scale_fill_manual(values = c(col5, col2, "#e44932")) +
  labs(x = "CORE-OM at baseline", y = "Number of attended sessions") +
  theme(
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 12, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 14, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 14, face = "bold", margin=margin(0,10,0,5))
  )




##### Plotting all participants
# To create a plot which represents all data points, we create a new model with only 
# the significant predictors (arm and core_base).
wide_pC_red <- wide %>% filter(!is.na(core_base)) # 507
glm.core.red <- glm(sessions_attended ~ arm + core_base, family = "quasipoisson", data = wide_pC_red)
summary(glm.core.red)


# Add data points to the dataset which contain information about arm and core_base,
# but not sessions, since those will be predicted. The points will not show up in the plot,
# but we need those data points to predict for CORE scores from 0-3.3.

# Make data frame
add <- data.frame(core_base = rep(seq(from = 0, to = 3.3, length.out = 2),3),
                  arm = factor(rep(c("control", "MMJ", "intervention"), each = 2)))
# Add to dataset
plot.core <- bind_rows(wide_pC_red, add)

#
plot.core <- cbind(plot.core, predict(glm.core.red, newdata = plot.core, type = "response", se = TRUE))
plot.core <- within(plot.core, {
  PredictedSess <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Rename arm
plot.core <- plot.core %>% 
  mutate(Intake = ifelse(arm == "intervention", "Intake 1",
                         ifelse(arm == "control", "Intake 2",
                                ifelse(arm == "MMJ", "Intake 3", NA))))


# Plot reduced model
ggplot(plot.core, aes(x = core_base, y = PredictedSess)) +
  geom_point(aes(y = sessions_attended, color = Intake, shape = Intake),
             position = position_jitter(h = 0.2), alpha = 0.3) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Intake), alpha = 0.2) +
  geom_line(aes(colour = Intake), size = 1) +
  theme_bw() +
  scale_color_manual(values = c(col5, col2, "#e44932")) +
  scale_fill_manual(values = c(col5, col2, "#e44932")) +
  labs(x = "CORE-OM at baseline", y = "Number of attended sessions") +
  theme(
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 12, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 14, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 14, face = "bold", margin=margin(0,10,0,5))
  )



# WEMWBS

# Create dataset which only contains the data we need
wide_pW <- wide %>% # n = 390
  filter(!is.na(age) & !is.na(gender) & !is.na(disability) & !is.na(ethnicity) & !is.na(med_base)
         & !is.na(wb_base))

glm.p.wemwbs <- glm(sessions_attended ~ age + gender + arm + disability + ethnicity
                    + med_base + wb_base,
                          family = "poisson", data = wide_pW)
summary(glm.p.wemwbs)

# Export relevant values
output.p.wemwbs <- round_df(cbind(Estimate = coef(glm.p.wemwbs),
                                   OR = exp(coef(glm.p.wemwbs)), exp(confint(glm.p.wemwbs)),
                                   p_value = coef(summary(glm.p.wemwbs))[,4]), 3)

write.csv(output.p.wemwbs, file = "model_output/1_p_wemwbs.csv")

# Arm, disability and wb_base may have an effect. The dispersion parameter is 1.
# Check for overdispersion.
dispersiontest(glm.p.wemwbs, trafo = 1)
# Overdispersed.

# Let's use a quasipoisson model.
glm.qp.wemwbs <- glm(sessions_attended ~ age + gender + arm + disability + ethnicity
                    + med_base + wb_base,
                    family = "quasipoisson", data = wide_pW)
summary(glm.qp.wemwbs)
# Disability and med_base still significant


# Compute important variables
stargazer(cbind(Estimate = coef(glm.qp.wemwbs), Std.Error = coef(summary(glm.qp.wemwbs))[,2],
                z.value = coef(summary(glm.qp.wemwbs))[,3],
                OR = exp(coef(glm.qp.wemwbs)), exp(confint(glm.qp.wemwbs)),
                p_value = coef(summary(glm.qp.wemwbs))[,4]), type = "text", style = "qje", digits = 3)

# Export relevant values
output.qp.wemwbs <- round_df(cbind(Estimate = coef(glm.qp.wemwbs),
                                   OR = exp(coef(glm.qp.wemwbs)), exp(confint(glm.qp.wemwbs)),
                                   p_value = coef(summary(glm.qp.wemwbs))[,4]), 3)

write.csv(output.qp.wemwbs, file = "model_output/1_qp_wemwbs.csv")




##### Sensitivity analysis with binary model and poisson model excluding people who attended
# 0 sessions.


### Logistic regression: Attended vs not attended


# Create variable which states whether or not someone has attended
wide <- wide %>% mutate(attended = ifelse(sessions_attended == 0, 0, 1))

# Create dataset which contains only participants who have data for all variables.
wide_reduced <- wide %>% filter(!is.na(gender) & !is.na(disability) & !is.na(med_base) &
                                  !is.na(ethnicity) & !is.na(age) & !is.na(core_base)
                                & !is.na(wb_base)) # n = 389

wide_reduced$arm <- factor(wide_reduced$arm, levels = c("intervention", "control", "MMJ"))
# Create generalized linear model
glm.logistic <- glm(attended ~ arm + ethnicity + age + gender + disability + med_base 
                    + core_base + wb_base, family = "binomial", data = wide_reduced)
summary(glm.logistic)
# med_base, disability & arm (disability less likely, med_base more likely)

# Extract OR, CIs, etc.
stargazer(cbind(Estimate = coef(glm.logistic), OR = exp(coef(glm.logistic)), exp(confint(glm.logistic)),
                p_value = coef(summary(glm.logistic))[,4]), type = "text", style = "qje", digits = 3)
# Numbers for MMJ seem weird. That's probably since all of the 35 MMJ people attended at least
# 1 session, but since it's only 35 people, the difference to the other arms is not significant.

# We can see it here
wide_reduced %>% 
  count(arm, attended) %>% 
ggplot(aes(arm, attended)) +
  geom_point(aes(size = n))


## Let's check for multicollinearity

# A VIF of 1.8 tells us that the variance (the square of the standard error)
# of a particular coefficient is 80% larger than it would be if that predictor
# was completely uncorrelated with all the other predictors.
# A VIF of greater than 2.50 corresponds to an R2 of .60 with the other variables.

lm.collinear.scores <- lm(core_base ~ wb_base, data = wide_reduced)
VIF(lm.collinear.scores) # 1.8 (R^2 here is 0.49, not really ideal.)
# WRONG

vif(glm.logistic) # CORE and WEMWBS are 2.3 and 2.2, therefore use different.


### Let's have 2 separate models and see if any numbers change dramatically. If not,
# we report the numbers from the initial model.

## CORE
wide_red_C <- wide %>%  filter(!is.na(gender) & !is.na(disability) & !is.na(med_base) &
                       !is.na(ethnicity) & !is.na(age) & !is.na(core_base)) # n = 391
glm.logistic.core <- glm(attended ~ age + gender + arm + disability + ethnicity + med_base
                         + core_base, family = "binomial", data = wide_red_C)

## WEMWBS
wide_red_W <- wide %>%  filter(!is.na(gender) & !is.na(disability) & !is.na(med_base) &
                                 !is.na(ethnicity) & !is.na(age) & !is.na(wb_base)) # n = 390
glm.logistic.wemwbs <- glm(attended ~ age + gender + arm + disability + ethnicity + med_base
                           + wb_base, family = "binomial", data = wide_red_W)

## Compare the models
summary(glm.logistic)
summary(glm.logistic.core)
summary(glm.logistic.wemwbs)
# Only disability and arm significant in both models


### Get pretty output like Julieta
summary(glm.small)
logistic.core.odds <- coef(summary(glm.small))
logistic.core.odds[, "Estimate"] <- exp(coef(glm.small))
print ("odds for intercept, OR for the rest")
print (logistic.core.odds)
stargazer (logistic.core.odds, type = "text", style = "qje", digits = 2)


# Export relevant values

output.log.core <- round_df(cbind(Estimate = coef(glm.logistic.core),
                                  OR = exp(coef(glm.logistic.core)), exp(confint(glm.logistic.core)),
                                  p_value = coef(summary(glm.logistic.core))[,4]), 3)

write.csv(output.log.core, file = "model_output/1_logistic_core.csv")

output.log.wemwbs <- round_df(cbind(Estimate = coef(glm.logistic.wemwbs),
                                   OR = exp(coef(glm.logistic.wemwbs)), exp(confint(glm.logistic.wemwbs)),
                                   p_value = coef(summary(glm.logistic.wemwbs))[,4]), 3)

write.csv(output.log.wemwbs, file = "model_output/1_logistic_wemwbs.csv")


# Look at disability.
wide_red_C %>% 
  group_by(disability, attended) %>% 
  summarize(n_obs = n()) %>% 
  mutate(perc = c(0.13, 0.87, 0.28, 0.72)) %>% 
  ggplot(aes(disability, attended)) +
  geom_point(aes(size = perc))





### Poisson regression: effect on number of sessions attended (if you have attended at all)

# Create dataset which only contains the people who have attended at least 1 session.
wide_poisson <- wide %>% filter(sessions_attended != 0)


# Start with the full model
glm.poisson <- glm(sessions_attended ~ arm + wb_base + ethnicity + age + gender +
                      disability + core_base + med_base + wb_base,
                    family = "poisson", data = wide_poisson)
summary(glm.poisson)


# Let's check for multicollinearity
lm.collinear.scores <- lm(core_base ~ wb_base, data = wide_poisson)
VIF(lm.collinear.scores) # 1.66 (R^2 here is 0.42, it's not ideal... have separate models,
# especially since one of the mental health scores seems to be significantly predicting
# the outcome.)
vif(glm.poisson) # core 2.3, wem 2.8


## Have separate models for CORE and WEMWBS

# CORE
# Create dataset which contains only participants who have data for all variables.
wide_red_C <- wide_poisson %>% filter(!is.na(gender) & !is.na(disability) & !is.na(med_base) &
                                              !is.na(ethnicity) & !is.na(age) & !is.na(core_base))
# n = 332

glm.poisson.core <- glm(sessions_attended ~ age + gender + arm + disability + ethnicity
                    + med_base + core_base,
                   family = "poisson", data = wide_red_C)
summary(glm.poisson.core)
# Arm and core_base seem to have an effect. Ethnicity + disability maybe. The dispersion parameter is 1.
# Poisson models assume that the variance and mean are equal.
# Quasi-poisson regressions assume the variance is 'only' proportional to the mean,
# but not necessarily the same.

# Test for overdispersion
dispersiontest(glm.poisson.core, trafo = 1)
# No need for quasipoisson


# Extract OR, CIs, etc.
stargazer(cbind(Estimate = coef(glm.poisson.core), Std.Error = coef(summary(glm.poisson.core))[,2],
                z.value = coef(summary(glm.poisson.core))[,3],
                OR = exp(coef(glm.poisson.core)), exp(confint(glm.poisson.core)),
                p_value = coef(summary(glm.poisson.core))[,4]), type = "text", style = "qje", digits = 3)

# Interpretation of exp(estimate) = OR:
# The incident rate for the MMJ arm is 1.25 times the incident rate for the reference group (intervention).
# The percent change in the incident rate of attended sessions is by -14.2% for every unit
# increase in core_base ((0.858-1)*100 = -14.2).


### Get overall significance of arm on the models


## For the full model

# Update model dropping arm
glm3 <- update(glm.poisson.core, . ~ . - arm)
# Test model differences with chi square test
anova(glm3, glm.poisson.core, test="Chisq") # p = 0.00005

# Using the Wald Test
wald.test(b = coef(glm.poisson.core), Sigma = vcov(glm.poisson.core), Terms = 2:3) # p = 0.0004


### Compare models

# Full
stargazer(cbind(Estimate = coef(glm.poisson.core), Std.Error = coef(summary(glm.poisson.core))[,2],
                z.value = coef(summary(glm.poisson.core))[,3],
                OR = exp(coef(glm.poisson.core)), exp(confint(glm.poisson.core)),
                p_value = coef(summary(glm.poisson.core))[,4]), type = "text", style = "qje", digits = 3)


### Since I am searching for the predictors of sessions_attended, I will report the
### model which fits my data best / the model with the lowest AIC. This is also what I will plot.
# No bullshit




# WEMWBS

# Create dataset which only contains the data we need
wide_red_W <- wide_poisson %>% filter(!is.na(gender) & !is.na(disability) & !is.na(med_base) &
                                              !is.na(ethnicity) & !is.na(age) & !is.na(wb_base))
# n = 330
glm.poisson.wemwbs <- glm(sessions_attended ~ age + gender + arm + disability + ethnicity + med_base
                          + wb_base, family = "quasipoisson", data = wide_red_W)
summary(glm.poisson.wemwbs)
# Arm and disability may have an effect. Ethnicity maybe? The dispersion parameter is 1.
dispersiontest(glm.poisson.wemwbs, trafo = 1)
# Maybe overdispersed, but not significant. Let's do the stepAIC


# Export relevant values

output.poisson.core <- round_df(cbind(Estimate = coef(glm.poisson.core),
                                  OR = exp(coef(glm.poisson.core)), exp(confint(glm.poisson.core)),
                                  p_value = coef(summary(glm.poisson.core))[,4]), 3)

write.csv(output.poisson.core, file = "model_output/1_poisson_core.csv")

output.poisson.wemwbs <- round_df(cbind(Estimate = coef(glm.poisson.wemwbs),
                                    OR = exp(coef(glm.poisson.wemwbs)), exp(confint(glm.poisson.wemwbs)),
                                    p_value = coef(summary(glm.poisson.wemwbs))[,4]), 3)

write.csv(output.poisson.wemwbs, file = "model_output/1_poisson_wemwbs.csv")





##### Plotting for CORE poisson

### CORE
# To create a plot which represents all data points, we create a new model with only 
# the significant predictors (arm and core_base).
core_plot <- wide %>% filter(!is.na(core_base) & sessions_attended != 0) # 443
glm.core.red <- glm(sessions_attended ~ arm + core_base, family = "quasipoisson", data = core_plot)
summary(glm.core.red)

# Add data points to the dataset which contain information about arm and core_base,
# but not sessions, since those will be predicted. The points will not show up in the plot,
# but we need those data points to predict for CORE scores from 0-3.3.

# Make data frame
add <- data.frame(core_base = rep(seq(from = 0, to = 3.3, length.out = 2),3),
                  arm = factor(rep(c("control", "MMJ", "intervention"), each = 2)))
# Add to dataset
plot.core <- bind_rows(core_plot, add)

#
plot.core <- cbind(plot.core, predict(glm.core.red, newdata = plot.core, type = "response", se = TRUE))
plot.core <- within(plot.core, {
  PredictedSess <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Rename arm
plot.core <- plot.core %>% 
  mutate(Intake = ifelse(arm == "intervention", "Intake 1",
                         ifelse(arm == "control", "Intake 2",
                                ifelse(arm == "MMJ", "Intake 3", NA))))


# Plot reduced model
ggplot(plot.core, aes(x = core_base, y = PredictedSess)) +
  geom_point(aes(y = sessions_attended, color = Intake, shape = Intake),
             position = position_jitter(h = 0.2), alpha = 0.3) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Intake), alpha = 0.2) +
  geom_line(aes(colour = Intake), size = 1) +
  theme_bw() +
  scale_color_manual(values = c(col5, col2, "#e44932")) +
  scale_fill_manual(values = c(col5, col2, "#e44932")) +
  labs(x = "CORE-OM at baseline", y = "Number of attended sessions") +
  scale_y_continuous(breaks = c(0:8)) +
  theme(
    text = element_text(family = "Leelawadee UI Semilight"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 14, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 16, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 16, face = "bold", margin=margin(0,15,0,5)),
    panel.grid.minor = element_blank()
  ) -> a

ggsave(a, filename = "plots_thesis/1_core_on_att.png", device = "png",
       width = 7, height= 5, units = "in")

# Done









##### Not sure what I'm doing here but this is the robust analysis which I can maybe do
##### if I'm concerned.

# Control for mild violation of the distribution assumption that the variance equals the mean:
# Use robust standard errors for the parameter estimates
# Estimating a robust covariance matrix of parameters:
cov.glm.reduced3 <- vcovHC(glm.reduced3, type = "HC0")
# Extracting the diagonals and squaring them (standard error for each variable)
std.err <- sqrt(diag(cov.glm.reduced3))
# Get a matrix with coefficients, standard errors, p-values and 95% confidence intervall.
r.est <- cbind(Estimate = coef(glm.reduced3), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(glm.reduced3)/std.err), lower.tail = FALSE),
               LL = coef(glm.reduced3) - 1.96 * std.err,
               UL = coef(glm.reduced3) + 1.96 * std.err)

r.est
# Looks like only core_base is significant, but not med_base or disability.

# Goodness of fit: comparing deviance of the model with ideal deviance
# Residual deviance = difference btw. deviance of the current model and the
# maximum deviance of the ideal model where the predicted values are identical to the observed
# If residual deviance is small enough, goodness of fit test won't be significant.
with(glm.reduced3, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))
# Ahm... have you ever seen a higher p-value?




##### Older Plotting


### For plotting
wide <- wide %>% 
  mutate(arm = str_replace(arm, "intervention", "Intervention Arm"),
         arm = str_replace(arm, "control", "Control Arm"),
         arm = str_replace(arm, "MMJ", "MMJ Study"))


### Plot influence of CORE score at baseline.
# But differentiate between the different arms, as we know they have a random bias.
wide %>%
  ggplot(aes(factor(sessions_attended), core_base)) +
  geom_boxplot() +
  theme_bw() +
  annotate("text",
           x = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
           y = c(3,3,3,3,3,3,3,3,3),
           label = c(table(wide$sessions_attended)),
           fontface = 1, size=4) +
  coord_flip()

wide %>%
  ggplot(aes(core_base, sessions_attended)) +
  geom_point(alpha = 0.5, aes(color = arm)) +
  scale_color_manual(values = c(col1, col2, col5)) +
  guides(color = FALSE) +
  theme_bw() +
  facet_wrap(~arm) +
  labs(x = "CORE-OM at baseline", y = "Number of sessions") +
  theme(
    axis.title.x = element_text(size = 12, face = "bold", margin=margin(7,0,5,0)),
    axis.title.y = element_text(size = 12, face = "bold", margin=margin(0,7,0,5)),
    strip.text.x = element_text(size = 11)
  ) -> CbATT

ggsave(CbATT, filename = "plots_2019-06-06/3_core_on_att.png", device = "png",
       width = 5, units = "in")



### Plotting comparatively which percentage of people has attended how many sessions
# in the different arms.

wide %>% 
  group_by(sessions_attended, arm) %>% 
  summarize(n_obs = n()) %>% 
  mutate(percent_obs = ifelse(arm == "Control Arm", n_obs/192*100,
                       ifelse(arm == "Intervention Arm", n_obs/336*100,
                              ifelse(arm == "MMJ Study", n_obs/128*100, NA)))) %>% 
ggplot(aes(arm, sessions_attended, size = percent_obs, color = arm)) +
  geom_point(aes(color = arm)) +
  scale_color_manual(values = c(col1, col2, col5)) +
  guides(color = FALSE) +
  theme_bw() +
  scale_size_continuous(name = "Observations (%)") +
  labs(x = "Study Group", y = "Number of sessions") +
  theme(
  legend.background = element_rect(linetype = 1, color = "gray86"),
  legend.title = element_text(size = 12),
  axis.text.x = element_text(size = 10, margin=margin(5,0,8,0)),
  axis.title.x = element_text(size = 12, face = "bold", margin=margin(0,0,8,0)),
  axis.title.y = element_text(size = 12, face = "bold", margin=margin(0,7,0,5)),
  legend.position = "bottom"
  ) -> armATT

ggsave(armATT, filename = "plots_2019-06-06/3_arm_on_att.png", device = "png",
       width = 5, units = "in")


### A higher percentage of control people has only completed 0 sessions.
# Due to reasons which I can discuss.


### Plot disability effect on number of sessions attended
wide %>% 
  group_by(sessions_attended, disability) %>% 
  summarize(n_obs = n()) %>% 
  mutate(percent_obs = ifelse(is.na(disability), n_obs/82,
                              ifelse(disability == TRUE, n_obs/73,
                                     ifelse(disability == FALSE, n_obs/501, NA)))) %>% 
  ggplot(aes(disability, sessions_attended)) +
  geom_point(aes(size = percent_obs)) +
  theme_bw()
# We can see that if you have a disability you may be more likely to attend 0 sessions
# and less likel to attend 8 sessions.
