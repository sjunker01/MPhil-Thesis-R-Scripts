library(MASS)
library(gamlss)
library(tidyverse)
library(plm)
library(msm)
library(AER)

# Create function to round numeric values in a heterogenic data frame to x digits
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

### Read in data
wide <- read_csv("data_input(4)/mindful_drop(FUbias).csv")

### Create color palette
cols <- c("#e47e32", "#ff9e1f", "#ae9764", "#719f8d", "#509094", "#d2c078")
col1 <- cols[1]
col2 <- cols[2]
col3 <- cols[3]
col4 <- cols[4]
col5 <- cols[5]
col6 <- cols[6]



##### STATS

# Start with the full model, and only with those people who have a value for all included
# variables. Therefore, make new data frames.

# Since disability and core_base were very insignificant and didn't change the last
# model much, we won't include them as confounders since we have no reason to believe
# they will confound the model.

wide_unadj <- wide %>% filter(!is.na(gender) & !is.na(age)) %>% 
  select(intake, sessions_attended, gender, age, questionnaire_no) # 429

# Model
glm.unadj <- glm(questionnaire_no ~ sessions_attended + age + gender + intake
                 , family = "poisson", data = wide_unadj)

summary(glm.unadj)
# Interpretation: The estimate is the expected log count for a one-unit increase in x.

# Check for over-dispersion
dispersiontest(glm.unadj)
# Not over-dispersed! No need for quasi-poisson model.


### Extract data
# Of unadjusted model
stargazer(cbind(Estimate = coef(glm.unadj), Std.Error = coef(summary(glm.unadj))[,2],
                z.value = coef(summary(glm.unadj))[,3], OR = exp(coef(glm.unadj)),
                exp(confint(glm.unadj)),
                p_value = coef(summary(glm.unadj))[,4]), type = "text", style = "qje", digits = 3)

# Of adjusted model
stargazer(cbind(Estimate = coef(glm.adj), Std.Error = coef(summary(glm.adj))[,2],
                z.value = coef(summary(glm.adj))[,3], OR = exp(coef(glm.adj)),
                exp(confint(glm.adj)),
                p_value = coef(summary(glm.adj))[,4]), type = "text", style = "qje", digits = 3)

# Interpretation of exp(estimate):
# The incident rate for the MMJ arm is 0.77 times the incident rate for the reference group (control).
# The percent change in the incident rate of questionnaire_no is by 6.4% for every unit
# increase in sessions_attended.


### Compute robust analysis

# Control for mild violation of the distribution assumption that the variance equals the mean:
# Use robust standard errors for the parameter estimates
# Estimating a robust covariance matrix of parameters:
cov.glm <- vcovHC(glm.full, type="HC0")
# Extracting the diagonals and squaring them (standard error for each variable)
std.err <- sqrt(diag(cov.glm))
# Get a matrix with coefficients, standard errors, p-values and 95% confidence intervall.
r.est <- cbind(Estimate= coef(glm.full), "Robust SE" = std.err,
               LL = coef(glm.full) - 1.96 * std.err,
               UL = coef(glm.full) + 1.96 * std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(glm.full)/std.err), lower.tail=FALSE))


## Compare to the normal output
stargazer(r.est, type = "text", style = "qje", digits = 3)
stargazer(cbind(Estimate = coef(glm.full), SE = coef(summary(glm.full))[,2],
                confint(glm.full), p_value = coef(summary(glm.full))[,4]),
          type = "text", style = "qje", digits = 3)
# Interestingly enough, the 'robust analysis' suggested by the UCLA website
# https://stats.idre.ucla.edu/r/dae/poisson-regression/
# seems to generally report lower standard errors, smaller CIs and lower p-values
# for all variables.




##### Model exploration

# Let's hold all variables by their means / by the factor which is used most and let arm differ.
summary(wide_unadj)
test1 <- with(wide_unadj, data.frame(sessions_attended = mean(sessions_attended), age = mean(age, na.rm = TRUE),
                                   disability = FALSE, gender = "Female",
                                   core_base = mean(core_base, na.rm = TRUE),
                                   wb_base = mean(wb_base, na.rm = TRUE), arm = c("control", "MMJ",
                                                                                  "intervention")))
test1$questionnaireP <- predict(glm.full, newdata = test1, type = "response")
test1
# Seeing the differences within arms - e.g. 2.7 questionnaires answered when in control group,
# 2.0 questionnaires answered when in MMJ group.


# Let's hold all variables by their means / by the factor which is used most and let sessions differ.
summary(wide_unadj)
test2 <- with(wide_unadj, data.frame(sessions_attended = c(0,1,2,3,4,5,6,7,8), age = mean(age, na.rm = TRUE),
                                   disability = FALSE, gender = "Female",
                                   core_base = mean(core_base, na.rm = TRUE),
                                   wb_base = mean(wb_base, na.rm = TRUE), arm = "intervention"))
test2$questionnaireP <- predict(glm.full, newdata = test2, type = "response")
test2
# 1.9 questionnaires answered when 0 sessions attended, 3.2 answered when 8 attended.




##### Plotting

# Create new model with only intake and sessions as predictor
glm.reduced <- glm(questionnaire_no ~ intake + sessions_attended, 
                   family = "poisson", data = wide)

summary(glm.reduced)
dispersiontest(glm.reduced)

# Predict probability and standard error
plot.simple <- cbind(wide, predict(glm.reduced, type = "response", se = TRUE))
plot.simple <- within(plot.simple, {
  PredictedQ <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})


# Plot reduced model
ggplot(plot.simple, aes(x = sessions_attended, y = PredictedQ)) +
  geom_point(aes(y = questionnaire_no, color = intake, shape = intake),
             position = position_jitter(h = 0.1), alpha = 0.3) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = intake), alpha = 0.2) +
  geom_line(aes(colour = intake), size = 1) +
  theme_bw() +
  scale_color_manual(values = c(col5, col2, "#e44932")) +
  scale_fill_manual(values = c(col5, col2, "#e44932")) +
  labs(color = "Intake", fill = "Intake", shape = "Intake", x = "Number of attended sessions",
       y = "Number of answered questionnaires") +
  scale_x_continuous(breaks = c(0:8)) +
  theme(
    text = element_text(family = "Leelawadee UI Semilight"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 14, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 16, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 16, face = "bold", margin=margin(0,15,0,5)),
    panel.grid.minor = element_blank()
  ) -> a

# Save
ggsave(a, filename = "plots_thesis/4_sess_on_questionnaires.png", device = "png",
       width = 7, height= 5, units = "in")
