library(MASS)
library(gamlss)
library(tidyverse)
library(ISLR)
library(aod)

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



##### Stats

# Create variables stating whether or not people have answered the post-intervention questionnaire.
wide <- wide %>% mutate(answered_post = !is.na(core_post)) %>% 
  mutate(answered_post = ifelse(answered_post == TRUE, 1, 0))
wide$answered_post <- as.numeric(wide$answered_post) # Converd to numbers

# Create dataset with only needed variables
wide_att <- wide %>% select(sessions_attended, answered_post, intake, age, disability, gender,
                            core_base)


# Fit overall model, accounting for known confounding factors, excluding disability (unadjusted)
# Create dataset
wide_unadj <- wide_att %>% # n = 429
  filter(!is.na(age) & !is.na(gender))

# Model (basic adjustment)
glm.unadj <- glm(answered_post ~ sessions_attended + age + gender + intake,
               data = wide_unadj, family = binomial)

# Fit model adjusted for disability
# Create dataset
wide_adj <- wide_att %>% # n = 422
  filter(!is.na(age) & !is.na(gender) & !is.na(disability) & !is.na(core_base))

# Model (adjusted for disability)
glm.adj <- glm(answered_post ~ sessions_attended + age + gender + intake + disability +
                 core_base,
               data = wide_adj, family = binomial)


### Modelfit
# Check model with gamlss package.
fit.gamlss <- gamlss(answered_post ~ sessions_attended + age + gender + intake + disability +
                       core_base,
                     + gender, data = na.omit(wide_adj), family = BI)
plot(fit.gamlss)


# Call summary on the unadjusted and adjusted models.
summary(glm.unadj)
summary(glm.adj)
# Interpretation: For every unit of change in x, the log odds of answering the questionnaire vs.
# not answering it increase/decrease by the estimate.



## Extract data
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




### Let us assess the effect on the model when disability and core at baseline are removed.

## Using a model comparison
# Update the glm model dropping arm
glm.noadj <- update(update(glm.adj, . ~ . - core_base), . ~ . -disability)
# Test model differences with chi square test
anova(glm.adj, glm.noadj, test="Chisq") # p = 0.36

## Using the Wald Test
wald.test(b = coef(glm.adj), Sigma = vcov(glm.adj), Terms = 7:8) p = 0.37
# Disability and core_base did not significantly change the model. But more important is whether the
# other variable estimates change, and they do not.

## Check whether disability and core improve the model
glm.nodis <- update(glm.adj, . ~ . -disability)
glm.nocore <- update(glm.adj, . ~ . -core_base)

AIC(glm.adj)
AIC(glm.nodis)
AIC(glm.nocore)
AIC(glm.noadj)


### Use predicted probabilities to help us understand the model.

## Let's hold all variables by their means / by the factor which is used most and let arm differ.
summary(wide_att)
test1 <- with(wide_att, data.frame(sessions_attended = mean(sessions_attended), age = mean(age, na.rm = TRUE),
                                   disability = FALSE, gender = "Female", arm = c("control", "MMJ",
                                                                                  "intervention")))
test1$answerP <- predict(glm.att, newdata = test1, type = "response")
test1
# Seeing the differences within arms - 14% chance of answering when in control or MMJ group,
# 90% chance of answering in intervention group with the same other variables.


## Let's hold all variables by their means / by the factor which is used most and let sessions differ.
test2 <- with(wide_att, data.frame(sessions_attended = c(0,1,2,3,4,5,6,7,8), age = mean(age, na.rm = TRUE),
                                   disability = FALSE, gender = "Female", arm = "intervention"))
test2$answerP <- predict(glm.att, newdata = test2, type = "response")
test2
# Chance of answering increases from 49% to 98% from 0 attended sessions to 8 attended sessions for
# people in the intervention group (all variables held at their means, no disab, female).



## Let's hold all variables by their means / by the factor which is used most and let sessions and
## arm differ.
test3 <- with(wide_att, data.frame(sessions_attended = rep(seq(from = 0, to = 8, length.out = 20),4),
                                   age = mean(age, na.rm = TRUE), disability = FALSE, gender = "Female",
                                   arm = factor(rep(c("control", "MMJ", "intervention"), each = 240))))
# Get estimates on the link scale and back transform both the predicted values and
# confidence limits into probabilities.
testdata <- cbind(test3, predict(glm.att, newdata = test3, type = "link", se = TRUE))
testdata <- within(testdata, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})


# Visualize for non-disabled females of the mean age (23.7)
ggplot(testdata, aes(x = sessions_attended, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Intake), alpha = 0.2) +
  geom_line(aes(colour = Intake), size = 1) +
  theme_bw() +
  scale_color_manual(values = c(col5, col2, "#e44932")) +
  scale_fill_manual(values = c(col5, col2, "#e44932")) +
  labs(x = "Number of attended sessions", y = "Probability to answer survey") +
  theme(
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 12, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 14, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 14, face = "bold", margin=margin(0,10,0,5))
  )



##### Plotting

# Create a model which only contains the significant predictors, which we will also
# visualize in the plot
glm.simple <- glm(answered_post ~ intake + sessions_attended, data = wide_att, family = "binomial")
summary(glm.simple)

# Predict probability and standard error
plot.simple <- cbind(wide_att, predict(glm.simple, type = "response", se = TRUE))
plot.simple <- within(plot.simple, {
  PredictedProb <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})


# Plot reduced model
ggplot(plot.simple, aes(x = sessions_attended, y = PredictedProb)) +
  geom_point(aes(y = answered_post, color = intake, shape = intake),
             position = position_jitter(h = 0.05), alpha = 0.3) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = intake), alpha = 0.2) +
  geom_line(aes(colour = intake), size = 1) +
  theme_bw() +
  scale_color_manual(values = c(col5, col2, "#e44932")) +
  scale_fill_manual(values = c(col5, col2, "#e44932")) +
  scale_x_continuous(breaks = c(0:8)) +
  labs(color = "Intake", fill = "Intake", shape = "Intake", x = "Number of attended sessions",
       y = expression(atop("Probability of answering", paste("post-intervention questionnaire")))) +
  theme(
    text = element_text(family = "Leelawadee UI Semilight"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 14, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 16, face = "bold", margin=margin(15,0,0,0)),
    axis.title.y = element_text(size = 16, face = "bold", margin=margin(0,15,0,5)),
    panel.grid.minor = element_blank()
  ) -> a

# Save
ggsave(a, filename = "plots_thesis/4_sess_on_prob.png", device = "png",
       width = 7.5, height= 5, units = "in")
