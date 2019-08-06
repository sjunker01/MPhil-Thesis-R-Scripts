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


### Read in file
wide <-  read_csv("data_input(4)/mindful_clean.csv")


### Check attendance for normality
ggplot(wide, aes(sessions_attended)) +
  geom_histogram(bins = 17, fill = "lightgrey", color = "black") +
  theme_bw()
# Clearly not normally distributed
shapiro.test(wide$sessions_attended)



##### Stats

### CORE-OM post-intervention prediction from attendance

# Create dataset which has no NAs for the tested variables
wide_c1 <- wide %>% # n = 304
  filter(!is.na(core_post) & !is.na(core_base) & !is.na(age) & !is.na(gender) & !is.na(intake)) %>% 
  mutate(row = row_number())

# Model (basic adjustment)
lm.core.1 <- lm(core_post ~ core_base + sessions_attended + age + gender + intake,
               data = wide_c1)
summary(lm.core.1)

# Reason to believe disability exerts an influence - include in the model
# Create dataset which has no NAs for the tested variables
wide_c2 <- wide %>% # n = 301
  filter(!is.na(core_post) &!is.na(core_base) & !is.na(age) & !is.na(gender) & !is.na(intake)
         & !is.na(disability)) %>% 
  mutate(row = row_number())

# Model (adjusted for disability)
lm.core.2 <- lm(core_post ~ core_base + sessions_attended + age + gender + intake + disability,
                data = wide_c2)
summary(lm.core.2)


# Retrieve statistical measures CORE-OM
# Of unadjusted model
stargazer(cbind(Estimate = coef(lm.core.1), Std.Error = coef(summary(lm.core.1))[,2],
                z.value = coef(summary(lm.core.1))[,3], confint(lm.core.1),
                p_value = coef(summary(lm.core.1))[,4]), type = "text", style = "qje", digits = 3)

# Of adjusted model
stargazer(cbind(Estimate = coef(lm.core.2), Std.Error = coef(summary(lm.core.2))[,2],
                z.value = coef(summary(lm.core.2))[,3], confint(lm.core.2),
                p_value = coef(summary(lm.core.2))[,4]), type = "text", style = "qje", digits = 3)



## Check this model for normality
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
# Significant variables are still significant in the models with squared outcome variable, even if the outlier is removed.



### WEMWBS

# Create dataset which has no NAs for the tested variables
wide_w1 <- wide %>% # n = 300
  filter(!is.na(wb_post) & !is.na(wb_base) & !is.na(age) & !is.na(gender) & !is.na(intake)) %>% 
  mutate(row = row_number())

# Model (basic adjustment)
lm.wemwbs.1 <- lm(wb_post ~ wb_base + sessions_attended + age + gender + intake,
                  data = wide_w1)
summary(lm.wemwbs.1)

# Reason to believe disability exerts an influence - include in the model
# Create dataset which has no NAs for the tested variables
wide_w2 <- wide %>% # n = 297
  filter(!is.na(wb_post) & !is.na(wb_base) & !is.na(age) & !is.na(gender) & !is.na(intake)
         & !is.na(disability)) %>% 
  mutate(row = row_number())

# Model (adjusted for disability)
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


## Retrieve statistical measures WEMWBS
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



##### Absolute risk, absolute risk reduction reduction, and NTT

## Calculate how many people are below CORE cut-off after treatment (0 vs 8 sessions),
## absolute risk reduction, and NTT.

# Absolute risk of being above the cut-off 0 sessions (just to see)
risk0 <- sum((wide %>% filter(sessions_attended == 0))$core_post >= 1, na.rm = TRUE)/
  nrow((wide %>% filter(sessions_attended == 0 & !is.na(core_post)))) # 56%

# Absolute risk of being above the cut-off 0-3 sessions
risk3 <- sum((wide %>% filter(sessions_attended < 4))$core_post >= 1, na.rm = TRUE)/
  nrow((wide %>% filter(sessions_attended < 4 & !is.na(core_post)))) # 47.31%

# Absolute risk of being above the cut-off when having attended the minimum dose of sessions
risk4 <- sum((wide %>% filter(sessions_attended >= 4))$core_post >= 1, na.rm = TRUE)/
  nrow((wide %>% filter(sessions_attended >= 4 & !is.na(core_post)))) # 23.45%

# Reduction of risk in percentage points (control event rate - experimental event rate)
reduction <- abs(risk3 - risk) # 23.86

# NNT
nnt <- 1/reduction
nnt # 4.19
# Need to treat 4.2 people to prevent 1 from being above the cut-off line after the course.




##### Plotting

### Create color palette
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

# Save
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

# Save
ggsave(b, filename = "plots_thesis/3_att_on_wemwbs.png", device = "png",
       width = 7, height= 5, units = "in")
