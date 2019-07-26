library(tidyverse)
library(olsrr)
library(lmtest)
library(car)

# Create function to round numeric values in a heterogenic data frame to x digits
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


wide <-  read_csv("data_input(4)/mindful_clean.csv", 
                  col_types = cols(arm = col_factor(levels = c("intervention", "control", "MMJ")),
                                   gender = col_factor(levels = c("Female", "Male"))))
wide <- wide %>% rename(row = X1)
wide$cohort <- with(wide, ifelse(cohort == "Michelmas", "michaelmas", cohort))
wide$disability_type <- factor(wide$disability_type, levels = c("No disability", "Mental",
                                                                "Physical", "Learning difficulty",
                                                                "Other"))
wide$ethnicity <- with(wide, ifelse(ethnicity == "White", "White", "Non-White"))

# Rename arm
wide <- wide %>% 
  mutate(intake = ifelse(arm == "intervention", "Intake 1",
                         ifelse(arm == "control", "Intake 2",
                                ifelse(arm == "MMJ", "Intake 3", NA))))
wide$intake <- factor(wide$intake, levels = c("Intake 1", "Intake 2", "Intake 3"))


### Check attendance for normality

ggplot(wide, aes(sessions_attended)) +
  geom_histogram(bins = 17, fill = "lightgrey", color = "black") +
  theme_bw()
# Clearly not normally distributed
shapiro.test(wide$sessions_attended)


### Assumptions for linear regression models:
# 1) Linear relationship. Check with scatter plot.
# 2) Variables should be multivariate normal. My data is not. Important is that the residuals are normalls
#     distributed.
# 3) No multicollinearity of the predictor variables.
# 4) Homocedacity (residuals should be equal across the regression line). gqtest(lm)



### Stats

# Prediction of CORE at post-intervention from attendance and CORE at baseline, controlled for
# gender, age, arm.

# Create dataset which has no NAs for the tested variables
wide_c1 <- wide %>% # n = 304
  filter(!is.na(core_post) & !is.na(core_base) & !is.na(age) & !is.na(gender) & !is.na(intake)) %>% 
  mutate(row = row_number())
# Model
lm.core.1 <- lm(core_post ~ core_base + sessions_attended + age + gender + intake,
               data = wide_c1)
summary(lm.core.1)

# Reason to believe disability exerts an influence - include in the model
# Create dataset which has no NAs for the tested variables
wide_c2 <- wide %>% # n = 301
  filter(!is.na(core_post) &!is.na(core_base) & !is.na(age) & !is.na(gender) & !is.na(intake)
         & !is.na(disability)) %>% 
  mutate(row = row_number())
# Model
lm.core.2 <- lm(core_post ~ core_base + sessions_attended + age + gender + intake + disability,
                data = wide_c2)
summary(lm.core.2)
# Fair enough, disability seems to exert an influence!


### Retrieve statistical measures

# Of unadjusted model
stargazer(cbind(Estimate = coef(lm.core.1), Std.Error = coef(summary(lm.core.1))[,2],
                z.value = coef(summary(lm.core.1))[,3], confint(lm.core.1),
                p_value = coef(summary(lm.core.1))[,4]), type = "text", style = "qje", digits = 3)

output.core.1 <- round_df(cbind(Estimate = coef(lm.core.1), Std.Error = coef(summary(lm.core.1))[,2],
                       confint(lm.core.1), p_value = coef(summary(lm.core.1))[,4]), 3)

write.csv(output.core.1, file = "model_output/3_core_unadj.csv")

# Of adjusted model
stargazer(cbind(Estimate = coef(lm.core.2), Std.Error = coef(summary(lm.core.2))[,2],
                z.value = coef(summary(lm.core.2))[,3], confint(lm.core.2),
                p_value = coef(summary(lm.core.2))[,4]), type = "text", style = "qje", digits = 3)

output.core.2 <- round_df(cbind(Estimate = coef(lm.core.2), Std.Error = coef(summary(lm.core.2))[,2],
                       confint(lm.core.2), p_value = coef(summary(lm.core.2))[,4]), 3)

write.csv(output.core.2, file = "model_output/3_core_adj.csv")



### Check this model for normality

par(mfrow = c(2,2))
plot(lm.core1)
par(mfrow = c(1,1))
hist(lm.core1$residuals)
ols_test_normality(lm.core1)
# The residuals are not normally distributed.
# Also, it looks like there might be an issue with heteroscedacity.

# We will transform the dependend variable with a square root
# Keep in mind that this is difficult to interpret. Therefore, we will only check whether
# the variables which are significant in the model above remain significant in this model.
lm.core.2.sqrt <- lm(sqrt(core_post) ~ core_base + sessions_attended + age + gender + intake
                    + disability, data = wide_c2)

## Check this model for normality
par(mfrow = c(2,2))
plot(lm.core.2.sqrt)
par(mfrow = c(1,1))
hist(lm.core.2.sqrt$residuals)
ols_test_normality(lm.core.2.sqrt)

# This is more normal. It seems as though there is an outlier which significantly changes
# the slope of the linear model. Let's check the outliers.

# Compute the cook's distance for each point
cooksd <- cooks.distance(lm.core.2.sqrt)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=10:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 6.

wide_out <- wide_c2 %>% filter(row != 6) %>% mutate(row = row_number())

lm.core.2.sqrt.2 <- lm(sqrt(core_post) ~ core_base + sessions_attended + age + gender + arm + disability,
                     data = wide_out)
par(mfrow = c(2,2))
plot(lm.core.2.sqrt.2)
par(mfrow = c(1,1))

summary(lm.core.2)
summary(lm.core.2.sqrt)
summary(lm.core.2.sqrt.2)
# Significant variables are still significant in the models with squared outcome variable.



##### WEMWBS

# Create dataset which has no NAs for the tested variables
wide_w1 <- wide %>% # n = 300
  filter(!is.na(wb_post) & !is.na(wb_base) & !is.na(age) & !is.na(gender) & !is.na(intake)) %>% 
  mutate(row = row_number())
# Model
lm.wemwbs.1 <- lm(wb_post ~ wb_base + sessions_attended + age + gender + intake,
                  data = wide_w1)
summary(lm.wemwbs.1)

# Reason to believe disability exerts an influence - include in the model
# Create dataset which has no NAs for the tested variables
wide_w2 <- wide %>% # n = 297
  filter(!is.na(wb_post) & !is.na(wb_base) & !is.na(age) & !is.na(gender) & !is.na(intake)
         & !is.na(disability)) %>% 
  mutate(row = row_number())
# Model
lm.wemwbs.2 <- lm(wb_post ~ wb_base + sessions_attended + age + gender + intake + disability,
                  data = wide_w2)
summary(lm.wemwbs.2)
# Disability is not significant, but the influence of arm and sessions is reduced.

### Check this model for normality

par(mfrow = c(2,2))
plot(lm.wemwbs.2)
par(mfrow = c(1,1))
hist(lm.wemwbs.2$residuals)
ols_test_normality(lm.wemwbs.2)
# Looks alright.


### Retrieve statistical measures

# Of unadjusted model
stargazer(cbind(Estimate = coef(lm.wemwbs.1), Std.Error = coef(summary(lm.wemwbs.1))[,2],
                z.value = coef(summary(lm.wemwbs.1))[,3], confint(lm.wemwbs.1),
                p_value = coef(summary(lm.wemwbs.1))[,4]), type = "text", style = "qje", digits = 3)

output.wemwbs.1 <- round_df(cbind(Estimate = coef(lm.wemwbs.1), Std.Error = coef(summary(lm.wemwbs.1))[,2],
                       confint(lm.wemwbs.1), p_value = coef(summary(lm.wemwbs.1))[,4]), 3)

write.csv(output.wemwbs.1, file = "model_output/3_wemwbs_unadj.csv")

# Of adjusted model
stargazer(cbind(Estimate = coef(lm.wemwbs.2), Std.Error = coef(summary(lm.wemwbs.2))[,2],
                z.value = coef(summary(lm.wemwbs.2))[,3], confint(lm.wemwbs.2),
                p_value = coef(summary(lm.wemwbs.2))[,4]), type = "text", style = "qje", digits = 3)

output.wemwbs.2 <- round_df(cbind(Estimate = coef(lm.wemwbs.2), Std.Error = coef(summary(lm.wemwbs.2))[,2],
                       confint(lm.wemwbs.2), p_value = coef(summary(lm.wemwbs.2))[,4]), 3)

write.csv(output.wemwbs.2, file = "model_output/3_wemwbs_adj.csv")



##### Risk ratio and NNT

# Run a logistic regression whereby whether or not you are above the 1.0 CORE-OM cut-off
# is the binary outcome, predicted by sessions and core_base

# Add variable which states whether or not you are above the cut-off
wide$abovecutoff <- with(wide, ifelse(core_post < 1, FALSE,
                                 ifelse(is.na(core_post), NA, TRUE)))

glm.risk <- glm(abovecutoff ~ core_base + sessions_attended, data = wide)
summary(glm.risk)


# Extract OR, CIs, etc.
stargazer(cbind(Estimate = coef(glm.risk), Std.Error = coef(summary(glm.risk))[,2],
                z.value = coef(summary(glm.risk))[,3],
                OR = exp(coef(glm.risk)), exp(confint(glm.risk)),
                p_value = coef(summary(glm.risk))[,4]), type = "text", style = "qje", digits = 3)
# For one session attended, the odds of being above the cut-off decrease by 3.1%...

# Export
risk.table <- round_df(cbind(Estimate = coef(glm.risk), Std.Error = coef(summary(glm.risk))[,2],
                             confint(glm.risk), p_value = coef(summary(glm.risk))[,4]), 3)
write.csv(risk.table, file = "model_output/3_risk.csv")


coef_intercept <- 0.202492
coef_sess <- -0.031573
coef_base <- 0.270725*median(wide$core_base, rn.rm = TRUE)
confint(glm.risk)
LL_sess <- -0.04887289
UL_sess <- -0.01427239

### Let's try this again
# Odds to be above cut-off when attending 0 sessions:
exp(coef_intercept)+exp(coef_base)+exp(coef_sess*0)

# Odds to be above cut-off when attending 8 sessions:
exp(coef_intercept)+exp(coef_base)+exp(coef_sess*8)



# Calculating for someone with a median core_base (0.88)

# Odds to be above cut-off when attending 0 sessions =
exp(coef_intercept+coef_base+coef_sess*0) # Odds = 1.55
# Probability to be above cut-off when attending 0 sessions =
p0 <- (exp(coef_intercept+coef_base+coef_sess*0))/(1+(exp(coef_intercept+coef_base+coef_sess*0)))
# probability (absolute risk) = 0.61

# Odds to be above cut-off when attending 8 sessions =
exp(coef_intercept+coef_base+coef_sess*8) # Odds = 1.20
# Probability to be above cut-off when attending 8 sessions =
p8 <- (exp(coef_intercept+coef_base+coef_sess*8))/(1+(exp(coef_intercept+coef_base+coef_sess*8)))
# probability (absolute risk) = 0.55

# Risk ratio: ratio of the odds for 8 sessions to the odds for 0 sessions:
# I don't know where the heck I got this from...
exp(coef_intercept+coef_base+coef_sess*8) / exp(coef_intercept+coef_base+coef_sess*0) # 0.777

# Get 95% CI for this
exp(coef_intercept+coef_base+LL_sess*8) / exp(coef_intercept+coef_base+LL_sess*0) # 0.676
exp(coef_intercept+coef_base+UL_sess*8) / exp(coef_intercept+coef_base+UL_sess*0) # 0.892


# The odds to be below the cut-off for people who attended 8 sessions are 22.6% lower compared
# to people who attended 0 sessions. -> Risk reduction = 22.6%
reduction <- 1 - (exp(coef_intercept+coef_base+coef_sess*8) / exp(coef_intercept+coef_base+coef_sess*0))

# Get 95% CIs for this
reductionLL <- 1 - (exp(coef_intercept+coef_base+LL_sess*8) / exp(coef_intercept+coef_base+LL_sess*0))
reductionUL <- 1 - (exp(coef_intercept+coef_base+UL_sess*8) / exp(coef_intercept+coef_base+UL_sess*0))


# Number to treat
nnt <- 1/reduction
nnt # 4.48

# NNT 95% CI
nntLL <- 1/reductionLL
nntLL # 3.09
nntUL <- 1/reductionUL
nntUL # 9.27


### Different
# ARR = AR control (0 sessions) - AR treatment (8 sessions)
ARR <- p0 - p8
# 0.615 !?

# NNT
1/ARR # 16!?!? wtf

# Relative risk
p8/p0 # 0.89

# Relative risk reduction
1-p8/p0 # 10% !?



### Calculate how many people are below CORE cut-off after treatment (0 vs 8 sessions),
### absolute and relative risk, and NNT

# Absolute risk of being above the cut-off 0 sessions
risk0 <- sum((wide %>% filter(sessions_attended == 0))$core_post >= 1, na.rm = TRUE)/
  nrow((wide %>% filter(sessions_attended == 0 & !is.na(core_post)))) # 56%

# Absolute risk of being above the cut-off 0-3 sessions
risk3 <- sum((wide %>% filter(sessions_attended < 4))$core_post >= 1, na.rm = TRUE)/
  nrow((wide %>% filter(sessions_attended < 4 & !is.na(core_post)))) # 47.31%

# Absolute risk of being above the cut-off when having attended the minimum dose of sessions
risk8 <- sum((wide %>% filter(sessions_attended >= 4))$core_post >= 1, na.rm = TRUE)/
  nrow((wide %>% filter(sessions_attended >= 4 & !is.na(core_post)))) # 23.45%

# Reduction of risk in percentage points (control event rate - experimental event rate)
reduction <- abs(risk0 - risk8) # 32.55
reduction3 <- abs(risk3 - risk8) # 23.86

# NNT
nnt <- 1/reduction
nnt # 3.07
nnt3 <- 1/reduction3
nnt3 # 4.19
# Need to treat 3.1 / 4.2 people to prevent 1 from being above the cut-off line after the course.





# Chance of getting below the cut-off 0 sessions (good if high)
risk0 <- sum((wide %>% filter(sessions_attended == 0))$core_post < 1 &
               (wide %>% filter(sessions_attended == 0))$core_base >= 1, na.rm = TRUE)/
  nrow((wide %>% filter(sessions_attended == 0 & !is.na(core_post)))) # 8%

# Chance of getting below the cut-off 0-3 sessions
risk3 <- sum((wide %>% filter(sessions_attended < 4))$core_post < 1 &
               (wide %>% filter(sessions_attended < 4))$core_base >= 1, na.rm = TRUE)/
  nrow((wide %>% filter(sessions_attended < 4 & !is.na(core_post)))) # 16%

# Chance of getting below the cut-off when having attended the minimum dose of sessions
risk8 <- sum((wide %>% filter(sessions_attended >= 4))$core_post < 1 &
               (wide %>% filter(sessions_attended >= 4))$core_base >= 1, na.rm = TRUE)/
  nrow((wide %>% filter(sessions_attended >= 4 & !is.na(core_post)))) # 26%

# Increase of chance in percentage points
increase <- abs(100*risk8 - 100*risk0) # 17.7
increase3 <- abs(100*risk8 - 100*risk3) # 9.5

# NNT
nnt <- 100/increase
nnt # 5.7
nnt3 <- 100/increase3
nnt3 # 10.5





##### Plotting


### Create color palette (darkest)
cols <- c("#e47e32", "#ff9e1f", "#ae9764", "#719f8d", "#509094", "#d2c078")
col1 <- cols[1]
col2 <- cols[2]
col3 <- cols[3]
col4 <- cols[4]
col5 <- cols[5]
col6 <- cols[6]


### Regressions

## CORE
# Make a new regression containing disability and core_base as predictors.
# Create reduced dataset
wide_c_red <- wide %>% filter(!is.na(core_post)) # n = 319
lm.core.red <- lm(core_post ~ sessions_attended, data = wide_c_red)
summary(lm.core.red)


# Add predicted values
core.plot <- cbind(wide_c_red, predict(lm.core.red, type = "response", se = TRUE))
core.plot <- within(core.plot, {
  PredictedCore <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Plot
ggplot(core.plot, aes(x = sessions_attended, y = PredictedCore)) +
  geom_point(aes(y = core_post), alpha = 0.5,
             position = position_jitter(w = 0.1)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2, fill = col1) +
  geom_line(size = 1, color = col1) +
  theme_bw() +
  scale_x_continuous(breaks = c(0:8)) +
  labs(x = "Number of attended sessions", y = "CORE-OM post-intervention") +
  theme(
    text = element_text(family = "Leelawadee UI Semilight"),
    legend.title = element_text(size = 18),
    axis.text = element_text(size = 16, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 18, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 18, face = "bold", margin=margin(0,15,0,5)),
    panel.grid.minor = element_blank()
  ) -> a

ggsave(a, filename = "plots_thesis/3_att_on_core.png", device = "png",
       width = 7, height= 5, units = "in")

## WEMWBS
# Make a new regression containing disability and core_base as predictors.
# Create reduced dataset
wide_w_red <- wide %>% filter(!is.na(wb_post)) # n = 319
lm.wemwbs.red <- lm(wb_post ~ sessions_attended, data = wide_w_red)
summary(lm.wemwbs.red)


# Add predicted values
wemwbs.plot <- cbind(wide_w_red, predict(lm.wemwbs.red, type = "response", se = TRUE))
wemwbs.plot <- within(wemwbs.plot, {
  PredictedWb <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Plot
ggplot(wemwbs.plot, aes(x = sessions_attended, y = PredictedWb)) +
  geom_point(aes(y = wb_post), alpha = 0.5,
             position = position_jitter(w = 0.1)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2, fill = col1) +
  geom_line(size = 1, color = col1) +
  theme_bw() +
  scale_x_continuous(breaks = c(0:8)) +
  labs(x = "Number of attended sessions", y = "WEMWBS post-intervention") +
  theme(
    text = element_text(family = "Leelawadee UI Semilight"),
    legend.title = element_text(size = 18),
    axis.text = element_text(size = 16, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 18, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 18, face = "bold", margin=margin(0,15,0,5)),
    panel.grid.minor = element_blank()
  ) -> b

ggsave(b, filename = "plots_thesis/3_att_on_wemwbs.png", device = "png",
       width = 7, height= 5, units = "in")

### Boxplots

## CORE
wide %>% 
  ggplot(aes(factor(sessions_attended), core_post)) +
   geom_hline(yintercept = median((filter(wide, sessions_attended == 0))$core_post, na.rm = TRUE),
             colour = col1, linetype = 2, size = 1) +
  geom_boxplot(fill = "transparent", outlier.colour = "gray60") +
  annotate("text",
           x = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
           y = c(3,3,3,3,3, 3, 3, 3, 3),
           label = c(table((wide %>% filter(!is.na(core_post)))$sessions_attended)),
           fontface = 1, size=5, colour = col5) +
  theme_bw() +
  labs(x = "Number of attended sessions", y = "CORE-OM after mindfulness course") +
  theme(
    axis.text = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin=margin(7,0,5,0)),
    axis.title.y = element_text(size = 14, face = "bold", margin=margin(0,7,0,5))
  ) -> ATTCp

ggsave(ATTCp, filename = "plots_poster/0_att_on_Cp.png", device = "png",
       width = 5, height = 5.44, units = "in")


wide %>% 
  ggplot(aes(sessions_attended, core_post-core_base)) +
  geom_point(alpha = 0.4) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fill = "red") +
  labs(x = "Number of sessions", y = "CORE-OM difference") +
  theme(
    axis.title.x = element_text(size = 12, face = "bold", margin=margin(7,0,5,0)),
    axis.title.y = element_text(size = 12, face = "bold", margin=margin(0,7,0,5))
  )



## WEMWBS
wide %>% 
  ggplot(aes(factor(sessions_attended), wb_post)) +
  geom_hline(yintercept = median((filter(wide, sessions_attended == 0))$wb_post, na.rm = TRUE),
             colour = col1, linetype = 2, size = 1) +
  geom_boxplot(fill = "transparent", outlier.colour = "gray76") +
  annotate("text",
           x = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
           y = c(15,15,15,15,15,15,15,15,15),
           label = c(table((wide %>% filter(!is.na(wb_post)))$sessions_attended)),
           fontface = 1, size=5, colour = col5) +
  theme_bw() +
  labs(x = "Number of sessions", y = "WEMWBS after mindfulness course") +
  theme(
    axis.text = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin=margin(7,0,5,0)),
    axis.title.y = element_text(size = 14, face = "bold", margin=margin(0,7,0,5))
  )

ggsave(ATTWp, filename = "plots_2019-06-06/0_att_on_Wp.png", device = "png",
       width = 5, units = "in")
