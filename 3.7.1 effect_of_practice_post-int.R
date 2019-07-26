library(MASS)
library(tidyverse)
library(lme4)
library(lmerTest)
library(olsrr)
library(fmsb)
library(yhat)

# Create function to round numeric values in a heterogenic data frame to x digits
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}



wide <- read_csv("data_input(4)/mindful_clean.csv",
                 col_types = cols(arm = col_factor(levels = c("intervention", "control", "MMJ")),
                                  gender = col_factor(levels = c("Female", "Male"))))
long <- read_csv("data_input(4)/mindful_long.csv")

wide <- wide %>% rename(row = X1)
long <- long %>% dplyr::select(-X1)

### Cleaning

# Bin ethnicity
wide$ethnicity <- with(wide, ifelse(ethnicity == "White", "White", "Non-White"))

# Rename arm
wide <- wide %>% 
  mutate(intake = ifelse(arm == "intervention", "Intake 1",
                         ifelse(arm == "control", "Intake 2",
                                ifelse(arm == "MMJ", "Intake 3", NA))))
wide$intake <- factor(wide$intake, levels = c("Intake 1", "Intake 2", "Intake 3"))



long$timepoint <- factor(long$timepoint, levels = c("base", "post", "fu1", "fu2", "fu3"))



### Plotting
# Let us plot how formal and informal practice at post-intervention are related
lm.1 <-lm(formal_h_total_post ~ informal_total_post, data = wide)
summary(lm.1)
wide %>%
  group_by(informal_total_post, formal_h_total_post) %>% 
  summarize(n_obs = n()) %>% 
  ggplot(aes(informal_total_post, formal_h_total_post)) +
  geom_point(aes(size = n_obs)) +
  theme_bw() +
  geom_smooth(method = "lm", fill = NA)+
  labs(x = "Informal practice during the course", y = "Formal practice during the course")



##### Effect of practice on scores: For each timepoint with the respective total amount of practice done.


### Models without disability adjustment

## CORE
wide_core_unadj <- wide %>% # n = 276
  filter(!is.na(core_post) & !is.na(formal_freq_h_week_post) & !is.na(informal_freq_post)
                & !is.na(age) & !is.na(gender) & !is.na(core_base))

lm.core.unadj <- lm(core_post ~ formal_freq_h_week_post + informal_freq_post +
                      sessions_attended + age + gender + intake + core_base, data = wide_core_unadj)
summary(lm.core.unadj)


## WEMWBS
wide_wemwbs_unadj <- wide %>% # 275
  filter(!is.na(wb_post) & !is.na(formal_freq_h_week_post) & !is.na(informal_freq_post)
                & !is.na(age) & !is.na(gender) & !is.na(wb_base))

lm.wemwbs.unadj <- lm(wb_post ~ formal_freq_h_week_post + informal_freq_post +
                            sessions_attended + age + gender + intake + wb_base, data = wide_wemwbs_unadj)
summary(lm.wemwbs.unadj)



### Models with disability adjustment

## CORE 
wide_core_adj <- wide %>% # 273
  filter(!is.na(core_post) & !is.na(formal_freq_h_week_post) & !is.na(informal_freq_post)
                & !is.na(age) & !is.na(gender) & !is.na(disability) & !is.na(core_base))


## Model with and without square root transformation of dependent variable.

lm.core.adj <- lm(core_post ~ formal_freq_h_week_post + informal_freq_post + sessions_attended
                      + age + gender + intake + disability + core_base, data = wide_core_adj)
lm.core.adj.sqrt <- lm(sqrt(core_post) ~ formal_h_total_post + informal_total_post +
                      sessions_attended + age + gender + intake + disability + core_base,
                      data = wide_core_adj)

# Check this model for normality
par(mfrow = c(2,2))
plot(lm.core.adj)
par(mfrow = c(1,1))
hist(lm.core.adj$residuals)
ols_test_normality(lm.core.adj)
# Not great

par(mfrow = c(2,2))
plot(lm.core.adj.sqrt)
par(mfrow = c(1,1))
hist(lm.core.adj.sqrt$residuals)
ols_test_normality(lm.core.adj.sqrt)
# Better. Check whether results are similar.


## Let's investigate the outliers.
# Compute the cook's distance for each point
cooksd <- cooks.distance(lm.core.adj)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=6:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 3 and 195

wide_core_out <- wide_core_adj %>% filter(row != 3 & row != 195) %>% 
  mutate(row = row_number())

# Looks alright apart from one outlier which may affect the model quite a bit.
# Let's remove the outlier and compare the two models
lm.core.adj.out <- lm(core_post ~ formal_h_total_post + informal_freq_post + sessions_attended
                      + age + gender + disability + core_base, data = wide_core_out)

summary(lm.core.adj)
summary(lm.core.adj.sqrt)
summary(lm.core.adj.out)

# Don't need to adjust WEMWBS model since disability never showed an impact on these scores



### Check for multicolinearity of practice / attendance variables
lm.collinear <- lm(informal_freq_post ~ formal_freq_h_week_post, data = wide)
lm.collinear2 <- lm(sessions_attended ~ formal_freq_h_week_post, data = wide)
lm.collinear3 <- lm(informal_freq_post ~ sessions_attended, data = wide)
VIF(lm.collinear) # 1.23 (R^2 here is 0.19, it's fine)
VIF(lm.collinear2) # 1.38
VIF(lm.collinear3) # 1.19
### WRONG

vif(lm.core.adj)
vif(lm.wemwbs.unadj)


# A VIF of 1.8 tells us that the variance (the square of the standard error)
# of a particular coefficient is 80% larger than it would be if that predictor
# was completely uncorrelated with all the other predictors.
# A VIF of greater than 2.50 corresponds to an R2 of .60 with the other variables.


##### Exract values

### Unadjusted

# CORE
stargazer(cbind(Estimate = coef(lm.core.unadj), Std.Error = coef(summary(lm.core.unadj))[,2],
                z.value = coef(summary(lm.core.unadj))[,3], confint(lm.core.unadj),
                p_value = coef(summary(lm.core.unadj))[,4]), type = "text", style = "qje", digits = 3)

# Export
core.unadj <- round_df(cbind(Estimate = coef(lm.core.unadj), Std.Error = coef(summary(lm.core.unadj))[,2],
                             confint(lm.core.unadj),
                             p_value = coef(summary(lm.core.unadj))[,4]), 3)
write.csv(core.unadj, file = "model_output/6_core_unadj.csv")


# WEMWBS
stargazer(cbind(Estimate = coef(lm.wemwbs.unadj), Std.Error = coef(summary(lm.wemwbs.unadj))[,2],
                z.value = coef(summary(lm.wemwbs.unadj))[,3], confint(lm.wemwbs.unadj),
                p_value = coef(summary(lm.wemwbs.unadj))[,4]), type = "text", style = "qje", digits = 3)

# Export
wemwbs.unadj <- round_df(cbind(Estimate = coef(lm.wemwbs.unadj), Std.Error = coef(summary(lm.wemwbs.unadj))[,2],
                               confint(lm.wemwbs.unadj),
                             p_value = coef(summary(lm.wemwbs.unadj))[,4]), 3)
write.csv(wemwbs.unadj, file = "model_output/6_wemwbs_unadj.csv")


### Unadjusted

# CORE
stargazer(cbind(Estimate = coef(lm.core.adj), Std.Error = coef(summary(lm.core.adj))[,2],
                z.value = coef(summary(lm.core.adj))[,3], confint(lm.core.adj),
                p_value = coef(summary(lm.core.adj))[,4]), type = "text", style = "qje", digits = 3)

# Export
core.adj <- round_df(cbind(Estimate = coef(lm.core.adj), Std.Error = coef(summary(lm.core.adj))[,2],
                           confint(lm.core.adj),
                             p_value = coef(summary(lm.core.adj))[,4]), 3)
write.csv(core.adj, file = "model_output/6_core_adj.csv")


# WEMWBS
stargazer(cbind(Estimate = coef(lm.wemwbs.adj), Std.Error = coef(summary(lm.wemwbs.adj))[,2],
                z.value = coef(summary(lm.wemwbs.adj))[,3], confint(lm.wemwbs.adj),
                p_value = coef(summary(lm.wemwbs.adj))[,4]), type = "text", style = "qje", digits = 3)

# Export
wemwbs.adj <- round_df(cbind(Estimate = coef(lm.wemwbs.adj), Std.Error = coef(summary(lm.wemwbs.adj))[,2],
                             confint(lm.wemwbs.adj),
                               p_value = coef(summary(lm.wemwbs.adj))[,4]), 3)
write.csv(wemwbs.adj, file = "model_output/6_wemwbs_adj.csv")




##### Plotting

### CORE

## Formal practice

wide_core_form <- wide %>% filter(!is.na(core_post) & !is.na(formal_freq_h_week_post)) # n = 280
lm.core.form <- lm(core_post ~ formal_freq_h_week_post, data = wide_core_form)
summary(lm.core.form)

# Predict probability and standard error
core.form <- cbind(wide_core_form, predict(lm.core.form, type = "response", se = TRUE))
core.form <- within(core.form, {
  PredictedCore <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Plot
ggplot(core.form, aes(x = formal_freq_h_week_post, y = PredictedCore)) +
  geom_point(aes(y = core_post),
             position = position_jitter(w = 0.05), alpha = 0.3) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = col1, alpha = 0.2) +
  geom_line(size = 1, col = col1) +
  theme_bw() +
  labs(x = "Formal practice frequency (h/week)",
       y = "CORE-OM post-intervention") +
  theme(
    text = element_text(family = "Leelawadee UI Semilight"),
    axis.text = element_text(size = 16, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 18, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 18, face = "bold", margin=margin(0,15,0,5))
  ) -> c.form

ggsave(c.form, filename = "plots_thesis/6_core_formal.png", device = "png",
       width = 7, height= 5, units = "in")


## Informal practice

wide_core_inf <- wide %>% filter(!is.na(core_post) & !is.na(informal_freq_post)) # n = 279
lm.core.inf <- lm(core_post ~ informal_freq_post, data = wide_core_inf)
summary(lm.core.inf)

# Predict probability and standard error
core.inf <- cbind(wide_core_inf, predict(lm.core.inf, type = "response", se = TRUE))
core.inf <- within(core.inf, {
  PredictedCore <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Plot
ggplot(core.inf, aes(x = informal_freq_post, y = PredictedCore)) +
  geom_point(aes(y = core_post),
             position = position_jitter(w = 0.07), alpha = 0.3) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = col1, alpha = 0.2) +
  geom_line(size = 1, col = col1) +
  theme_bw() +
  labs(x = "Informal practice frequency",
       y = "CORE-OM post-intervention") +
  theme(
    text = element_text(family = "Leelawadee UI Semilight"),
    axis.text = element_text(size = 16, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 18, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 18, face = "bold", margin=margin(0,15,0,5))
  ) -> c.inf

ggsave(c.inf, filename = "plots_thesis/6_core_informal.png", device = "png",
       width = 7, height= 5, units = "in")


### WEMWBS

## Formal practice

wide_wemwbs_form <- wide %>% filter(!is.na(wb_post) & !is.na(formal_freq_h_week_post)) # n = 280
lm.wemwbs.form <- lm(wb_post ~ formal_freq_h_week_post, data = wide_wemwbs_form)
summary(lm.wemwbs.form)

# Predict probability and standard error
wemwbs.form <- cbind(wide_wemwbs_form, predict(lm.wemwbs.form, type = "response", se = TRUE))
wemwbs.form <- within(wemwbs.form, {
  PredictedWemwbs <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Plot
ggplot(wemwbs.form, aes(x = formal_freq_h_week_post, y = PredictedWemwbs)) +
  geom_point(aes(y = wb_post),
             position = position_jitter(w = 0.05), alpha = 0.3) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = col1, alpha = 0.2) +
  geom_line(size = 1, col = col1) +
  theme_bw() +
  labs(x = "Formal practice frequency (h/week)",
       y = "WEMWBS post-intervention") +
  scale_y_continuous(breaks = c(20,40,60)) +
  theme(
    text = element_text(family = "Leelawadee UI Semilight"),
    axis.text = element_text(size = 16, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 18, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 18, face = "bold", margin=margin(0,15,0,5))
  ) -> w.form

ggsave(w.form, filename = "plots_thesis/6_wemwbs_formal.png", device = "png",
       width = 7, height= 5, units = "in")

## Informal practice

wide_wemwbs_inf <- wide %>% filter(!is.na(wb_post) & !is.na(informal_freq_post)) # n = 279
lm.wemwbs.inf <- lm(wb_post ~ informal_freq_post, data = wide_wemwbs_inf)
summary(lm.wemwbs.inf)

# Predict probability and standard error
wemwbs.inf <- cbind(wide_wemwbs_inf, predict(lm.wemwbs.inf, type = "response", se = TRUE))
wemwbs.inf <- within(wemwbs.inf, {
  PredictedWemwbs <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Plot
ggplot(wemwbs.inf, aes(x = informal_freq_post, y = PredictedWemwbs)) +
  geom_point(aes(y = wb_post),
             position = position_jitter(w = 0.07), alpha = 0.3) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = col1, alpha = 0.2) +
  geom_line(size = 1, color = col1) +
  theme_bw() +
  labs(x = "Informal practice frequency",
       y = "WEMWBS post-intervention") +
  scale_y_continuous(breaks = c(20,40,60)) +
  theme(
    text = element_text(family = "Leelawadee UI Semilight"),
    axis.text = element_text(size = 16, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 18, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 18, face = "bold", margin=margin(0,15,0,5))
  ) -> w.inf

ggsave(w.inf, filename = "plots_thesis/6_wemwbs_informal.png", device = "png",
       width = 7, height= 5, units = "in")





####### Idk if I need this

### See how many people got >3 WEMWBS points worse or better
wide %>%
  mutate(wb_diff = wb_post - wb_base) %>% 
  mutate(diff_positive = wb_diff >= 3,
         diff_negative = wb_diff <= -3) %>% 
  summarize(sum(diff_positive, na.rm = TRUE), sum(diff_negative, na.rm = TRUE))
# 142 people got more than 3 points better, 84 got more than 3 points worse




ggplot(wide, aes(formal_h_total_post, core_post)) +
  geom_point(position = position_jitter(w = 0.3)) +
  geom_smooth(fill = NA) +
  theme_bw()
# NON-LINEAR RELATIONSHIP INDICATION
ggplot(wide, aes(formal_h_total_post, wb_post)) +
  geom_point(position = position_jitter(w = 0.3)) +
  geom_smooth(fill = NA) +
  theme_bw()

# Same
wide <- wide %>% mutate(formal_freq_post = ifelse(formal_freq_post == 0, 1, formal_freq_post))
ggplot(wide, aes(formal_freq_post, core_post)) +
  geom_point(position = position_jitter(w = 0.3)) +
  geom_smooth(fill = NA) +
  theme_bw()

