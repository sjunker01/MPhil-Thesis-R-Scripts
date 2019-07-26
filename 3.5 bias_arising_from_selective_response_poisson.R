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


wide <- read_csv("data_input(4)/mindful_drop(FUbias).csv",
                 col_types = cols(arm = col_factor(levels = c("intervention", "control", "MMJ")),
                                  gender = col_factor(levels = c("Female", "Male"))))
wide <- wide %>% select(-X1)
# Bin ethnicity
wide$ethnicity <- with(wide, ifelse(ethnicity == "White", "White", "Non-White"))

# Rename arm
wide <- wide %>% 
  mutate(intake = ifelse(arm == "intervention", "Intake 1",
                         ifelse(arm == "control", "Intake 2",
                                ifelse(arm == "MMJ", "Intake 3", NA))))
wide$intake <- factor(wide$intake, levels = c("Intake 1", "Intake 2", "Intake 3"))


### Create color palette (darkest)
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
# they will confound the model. Ignore the adjusted model for now

wide_unadj <- wide %>% filter(!is.na(gender) & !is.na(age)) %>% 
  select(intake, sessions_attended, gender, age, questionnaire_no) # 429
wide_adj <- wide %>% filter(!is.na(core_base) & !is.na(wb_base) # 418
                            & !is.na(gender) & !is.na(age) & !is.na(disability)) %>% 
  select(intake, sessions_attended, gender, age, disability, core_base, wb_base, questionnaire_no)

# Model without adjustment for disability
glm.unadj <- glm(questionnaire_no ~ sessions_attended + age + gender + intake
                 , family = "poisson", data = wide_unadj)




# Start with the full model
glm.adj <- glm(questionnaire_no ~ sessions_attended + age + gender + intake + disability
               + core_base + wb_base, family = "poisson", data = wide_adj)

summary(glm.unadj)
summary(glm.adj)
# Interpretation: The estimate is the expected log count for a one-unit increase in x.


# Check for over-dispersion
dispersiontest(glm.unadj)
# Not over-dispersed! Wooh!


### Extract data

# Of unadjusted model
stargazer(cbind(Estimate = coef(glm.unadj), Std.Error = coef(summary(glm.unadj))[,2],
                z.value = coef(summary(glm.unadj))[,3], OR = exp(coef(glm.unadj)),
                exp(confint(glm.unadj)),
                p_value = coef(summary(glm.unadj))[,4]), type = "text", style = "qje", digits = 3)

output.unadj <- round_df(cbind(Estimate = coef(glm.unadj),OR = exp(coef(glm.unadj)),
                                exp(confint(glm.unadj)),
                                p_value = coef(summary(glm.unadj))[,4]), 3)

write.csv(output.unadj, file = "model_output/4_questionnaires_unadj.csv")

# Of adjusted model
stargazer(cbind(Estimate = coef(glm.adj), Std.Error = coef(summary(glm.adj))[,2],
                z.value = coef(summary(glm.adj))[,3], OR = exp(coef(glm.adj)),
                exp(confint(glm.adj)),
                p_value = coef(summary(glm.adj))[,4]), type = "text", style = "qje", digits = 3)

output.adj <- round_df(cbind(Estimate = coef(glm.adj),OR = exp(coef(glm.adj)),
                               exp(confint(glm.adj)),
                               p_value = coef(summary(glm.adj))[,4]), 3)

write.csv(output.adj, file = "model_output/4_questionnaires_adj.csv")



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


### Compare to the normal output

stargazer(r.est, type = "text", style = "qje", digits = 3)
stargazer(cbind(Estimate = coef(glm.full), SE = coef(summary(glm.full))[,2],
                confint(glm.full), p_value = coef(summary(glm.full))[,4]),
          type = "text", style = "qje", digits = 3)
# Interestingly enough, the 'robust analysis' suggested by the UCLA website
# https://stats.idre.ucla.edu/r/dae/poisson-regression/
# seems to generally report lower standard errors, smaller CIs and lower p-values
# for all variables.


# Goodness of fit: comparing deviance of the model with ideal deviance
# Residual deviance = difference btw. deviance of the current model and the
# maximum deviance of the ideal model where the predicted values are identical to the observed
# If residual deviance is small enough, goodness of fit test won't be significant.
with(glm.full, cbind(res.deviance = deviance, df = df.residual,
                         p = pchisq(deviance, df.residual, lower.tail=FALSE))) # p = 1?
# Not sure if this is the correct way to do it...


### Compute incident rate ratios and their standard errors using the delta method
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5), ~ exp(x6),
                      ~ exp(x7), ~ exp(x8), ~ exp(x9)), 
                 coef(glm.full), cov.glm)
## Exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -5])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s

## Compare to normal output
stargazer(rexp.est, type = "text", style = "qje", digits = 3)
stargazer(cbind(exp_Estimate = exp(coef(glm.full)), SE = NA,
                exp(confint(glm.full)), p_value = coef(summary(glm.full))[,4]),
          type = "text", style = "qje", digits = 3)
# Not sure how to compute the Standard Error of the exponentiated estimate.
# However, we do get the 95% CI using the confint() function.
# Again, the robust analysis seems to decrease the confidence interval in every case.

# Interpretation of exp(estimate):
# The incident rate for the MMJ arm is 0.77 times the incident rate for the reference group (control).
# The percent change in the incident rate of questionnaire_no is by 6.4% for every unit
# increase in sessions_attended.



##### Model exploration

# Let's hold all variables by their means / by the factor which is used most and let arm differ.
summary(wide_adj)
test1 <- with(wide_adj, data.frame(sessions_attended = mean(sessions_attended), age = mean(age, na.rm = TRUE),
                                   disability = FALSE, gender = "Female",
                                   core_base = mean(core_base, na.rm = TRUE),
                                   wb_base = mean(wb_base, na.rm = TRUE), arm = c("control", "MMJ",
                                                                                  "intervention")))
test1$questionnaireP <- predict(glm.full, newdata = test1, type = "response")
test1
# Seeing the differences within arms - e.g. 2.7 questionnaires answered when in control group,
# 2.0 questionnaires answered when in MMJ group.


# Let's hold all variables by their means / by the factor which is used most and let sessions differ.
summary(wide_adj)
test2 <- with(wide_adj, data.frame(sessions_attended = c(0,1,2,3,4,5,6,7,8), age = mean(age, na.rm = TRUE),
                                   disability = FALSE, gender = "Female",
                                   core_base = mean(core_base, na.rm = TRUE),
                                   wb_base = mean(wb_base, na.rm = TRUE), arm = "intervention"))
test2$questionnaireP <- predict(glm.full, newdata = test2, type = "response")
test2
# 1.9 questionnaires answered when 0 sessions attended, 3.2 answered when 8 attended.




##### Create new dataset and plot predicted values for the test dataset.

test3 <- with(wide_adj, data.frame(sessions_attended = rep(seq(from = 0, to = 8, length.out = 20),4),
                                   age = mean(age, na.rm = TRUE), disability = FALSE, gender = "Female",
                                   core_base = mean(core_base, na.rm = TRUE),
                                   wb_base = mean(wb_base, na.rm = TRUE),
                                   intake = factor(rep(c("Intake 1", "Intake 2", "Intake 3"), each = 240))))


testdata <- cbind(test3, predict(glm.adj, newdata = test3, type = "response", se = TRUE))
testdata <- within(testdata, {
  PredictedProb <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})


# Rename arm
testdata <- testdata %>% 
  mutate(Intake = ifelse(arm == "intervention", "Intake 1",
                         ifelse(arm == "control", "Intake 2",
                                ifelse(arm == "MMJ", "Intake 3", NA))))


# Plot for non-disabled females of the mean age (23.64), mean core_base (0.98) and mean wb_base (46.7)
ggplot(testdata, aes(x = sessions_attended, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Intake), alpha = 0.2) +
  geom_line(aes(colour = Intake), size = 1) +
  theme_bw() +
  scale_color_manual(values = c(col5, col2, "#e44932")) +
  scale_fill_manual(values = c(col5, col2, "#e44932")) +
  labs(x = "Number of attended sessions", y = "Number of answered questionnaire sets") +
  theme(
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 12, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 14, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 14, face = "bold", margin=margin(0,10,0,5))
  )




##### Plotting everyone

# Have new model with only intake and sessions as predictor
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

ggsave(a, filename = "plots_thesis/4_sess_on_questionnaires.png", device = "png",
       width = 7, height= 5, units = "in")
