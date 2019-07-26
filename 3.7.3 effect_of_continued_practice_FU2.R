library(MASS)
library(tidyverse)
library(lme4)
library(lmerTest)
library(olsrr)
library(fmsb)
library(yhat)
library(stargazer)


wide <- read_csv("data_input(4)/mindful_clean.csv")

wide <- wide %>% rename(row = X1)


##### Effect of practice on scores: For each timepoint with the respective total amount of practice done.

#### CORE scores

### 2-year FU

## Model not corrected for anything except disability (since that showed significance in some cases,
## we don't have enough data to correct for more)


##### Cleaning - make practice comparable

# Want variables which are comparable.
summary(wide$formal_h_total_fu2) # Number of hours per week is this/104 (1 year)
wide$formal_h_total_fu2 <- (wide$formal_h_total_fu2)/104

summary(wide$formal_h_after_fu2) # Number of hours per week is this/96 (1 year - course)
# (which is formal_freq_h_week_fu1 really but ok)
wide$formal_h_after_fu2 <- (wide$formal_h_after_fu2)/96

summary(wide$informal_total_fu2) # To convert back to Never - Very often: need 5 levels.
# Maximum can be 5*52 = 260; rarely would be 1*52 = 52.
wide$informal_total_fu2 <- (wide$informal_total_fu2)/104

summary(wide$informal_after_fu2) # Maximum is 5*44 = 220 (informal_freq_fu1 but ok)
wide$informal_after_fu2 <- (wide$informal_after_fu2)/96




# Make data frame which contains only people who have data for all the tested variables
wide.reduced <- wide %>% filter(!is.na(core_fu2) & !is.na(formal_h_total_fu2) &
                                      !is.na(informal_total_fu2)) %>% 
  mutate(row = row_number()) # Has 28 observations...



## Baseline score and practice from baseline
lm.core.1 <- lm(core_fu2 ~ formal_h_total_fu2 + informal_total_fu2 +
                      sessions_attended + core_base, data = wide.reduced)

summary(lm.core.1)

# Check this model for normality
par(mfrow = c(2,2))
plot(lm.core.1) 
par(mfrow = c(1,1))
hist(lm.core.1$residuals)
ols_test_normality(lm.core.1)
# Not the best, but ok.

cooksd <- cooks.distance(lm.core.1)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Looks fine but Let's remove row 12 & 3

wide.out <- wide.reduced %>% filter(row != 12 & row != 3) %>%
  mutate(row = row_number())

# Model without outliers
lm.core.2 <- lm(core_fu2 ~ formal_h_total_fu2 + informal_total_fu2 +
                      sessions_attended + core_base, data = wide.out)

# Cooks distance
cooksd <- cooks.distance(lm.core.2)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Fine!

# Compare the analyses
summary(lm.core.1)
summary(lm.core.2)

# Both not significant.


### WEMWBS

# Make data frame which contains only people who have data for all the tested variables
wide.reduced <- wide %>% filter(!is.na(wb_fu2) & !is.na(formal_h_total_fu2) &
                                  !is.na(informal_total_fu2)) %>% 
  mutate(row = row_number()) # Has 28 observations...



## Baseline score and practice from baseline
lm.wemwbs.1 <- lm(wb_fu2 ~ formal_h_total_fu2 + informal_total_fu2 +
                  sessions_attended + wb_base, data = wide.reduced)

summary(lm.wemwbs.1)

# Check this model for normality
par(mfrow = c(2,2))
plot(lm.wemwbs.1) 
par(mfrow = c(1,1))
hist(lm.wemwbs.1$residuals)
ols_test_normality(lm.wemwbs.1)
# Not the best, but ok.

cooksd <- cooks.distance(lm.wemwbs.1)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 3

wide.out <- wide.reduced %>% filter(row != 3) %>%
  mutate(row = row_number())

# Model without outliers
lm.wemwbs.2 <- lm(wb_fu2 ~ formal_h_total_fu2 + informal_total_fu2 +
                  sessions_attended + wb_base, data = wide.out)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.2)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Fine!

# Compare the analyses
summary(lm.wemwbs.1)
summary(lm.wemwbs.2)
# Not much different






##### Post-int score + practice after post-int

# Create dataset
wide.reduced.2 <- wide %>% filter(!is.na(core_fu2) & !is.na(formal_h_after_fu2) &
                                      !is.na(informal_after_fu2) & !is.na(core_fu1)) %>% 
  mutate(row = row_number()) # Has 52 observations...

## Model with square root transformation of dependent variable.
lm.core.a1 <- lm(core_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                      sessions_attended + core_fu1, data = wide.reduced.2)

# Check this model for normality
par(mfrow = c(2,2))
plot(lm.core.a1) 
par(mfrow = c(1,1))
hist(lm.core.a1$residuals)
ols_test_normality(lm.core.a1) # Ok

# Compute the cook's distance for each point
cooksd <- cooks.distance(lm.core.a1)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 10.

wide.out2 <- wide.reduced.2 %>% filter(row != 10) %>% mutate(row = row_number())

# Next model
lm.core.a2 <- lm(core_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                      sessions_attended + core_fu1, data = wide.out2)

# Cooks distance
cooksd <- cooks.distance(lm.core.a2)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 35

wide.out3 <- wide.out2 %>% filter(row != 35) %>% mutate(row = row_number())

# Next model
lm.core.a3 <- lm(core_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                      sessions_attended + core_fu1, data = wide.out3)

# Cooks distance
cooksd <- cooks.distance(lm.core.a3)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 29

wide.out4 <- wide.out3 %>% filter(row != 29) %>% mutate(row = row_number())

# Next model
lm.core.a4 <- lm(core_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                      sessions_attended + core_fu1, data = wide.out4)

# Cooks distance
cooksd <- cooks.distance(lm.core.a4)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 45

wide.out5 <- wide.out4 %>% filter(row != 45) %>% mutate(row = row_number())

# Next model
lm.core.a5 <- lm(core_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                      sessions_attended + core_fu1, data = wide.out5)

# Cooks distance
cooksd <- cooks.distance(lm.core.a5)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 7

wide.out6 <- wide.out5 %>% filter(row != 7) %>% mutate(row = row_number())

# Next model
lm.core.a6 <- lm(core_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                      sessions_attended + core_fu1, data = wide.out6)

# Cooks distance
cooksd <- cooks.distance(lm.core.a6)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Ok whatever

# Let's compare the analyses of all models.
summary(lm.core.a1)
summary(lm.core.a2)
summary(lm.core.a3)
summary(lm.core.a4)
summary(lm.core.a5)
summary(lm.core.a6)

# The more outliers we remove, the more likely formal practice becomes to be significantly
# affecting the scores. Help?



##### WEMWBS

# Create dataset
wide.reduced.2 <- wide %>% filter(!is.na(wb_fu2) & !is.na(formal_h_after_fu2) &
                                    !is.na(informal_after_fu2) & !is.na(wb_fu1)) %>% 
  mutate(row = row_number()) # Has 52 observations...

## Model with square root transformation of dependent variable.
lm.wemwbs.a1 <- lm(wb_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                   sessions_attended + wb_fu1, data = wide.reduced.2)

# Check this model for normality
par(mfrow = c(2,2))
plot(lm.wemwbs.a1) 
par(mfrow = c(1,1))
hist(lm.wemwbs.a1$residuals)
ols_test_normality(lm.wemwbs.a1) # Ok

# Compute the cook's distance for each point
cooksd <- cooks.distance(lm.wemwbs.a1)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 36.

wide.out2 <- wide.reduced.2 %>% filter(row != 36) %>% mutate(row = row_number())

# Next model
lm.wemwbs.a2 <- lm(wb_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                   sessions_attended + wb_fu1, data = wide.out2)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.a2)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 10

wide.out3 <- wide.out2 %>% filter(row != 10) %>% mutate(row = row_number())

# Next model
lm.wemwbs.a3 <- lm(wb_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                   sessions_attended + wb_fu1, data = wide.out3)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.a3)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 29

wide.out4 <- wide.out3 %>% filter(row != 29) %>% mutate(row = row_number())

# Next model
lm.wemwbs.a4 <- lm(wb_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                   sessions_attended + wb_fu1, data = wide.out4)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.a4)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 45

wide.out5 <- wide.out4 %>% filter(row != 45) %>% mutate(row = row_number())

# Next model
lm.wemwbs.a5 <- lm(wb_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                   sessions_attended + wb_fu1, data = wide.out5)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.a5)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 40

wide.out6 <- wide.out5 %>% filter(row != 40) %>% mutate(row = row_number())

# Next model
lm.wemwbs.a6 <- lm(wb_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                   sessions_attended + wb_fu1, data = wide.out6)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.a6)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Ok whatever

# Let's compare the analyses of all models.
summary(lm.wemwbs.a1)
summary(lm.wemwbs.a2)
summary(lm.wemwbs.a3)
summary(lm.wemwbs.a4)
summary(lm.wemwbs.a5)
summary(lm.wemwbs.a6)

# The more outliers we remove, the more likely formal practice becomes to be significantly
# affecting the scores. Help?


### From baseline

## CORE

stargazer(cbind(Estimate = coef(lm.core.1), Std.Error = coef(summary(lm.core.1))[,2],
                z.value = coef(summary(lm.core.1))[,3], confint(lm.core.1),
                p_value = coef(summary(lm.core.1))[,4]), type = "text", style = "qje", digits = 3)

# Export
c1 <- round_df(cbind(Estimate = coef(lm.core.1), Std.Error = coef(summary(lm.core.1))[,2],
                     confint(lm.core.1),
                     p_value = coef(summary(lm.core.1))[,4]), 3)
write.csv(c1, file = "model_output/6_fu2_c1.csv")


### WEMWBS


stargazer(cbind(Estimate = coef(lm.wemwbs.1), Std.Error = coef(summary(lm.wemwbs.1))[,2],
                z.value = coef(summary(lm.wemwbs.1))[,3], confint(lm.wemwbs.1),
                p_value = coef(summary(lm.wemwbs.1))[,4]), type = "text", style = "qje", digits = 3)

# Export
w1 <- round_df(cbind(Estimate = coef(lm.wemwbs.1), Std.Error = coef(summary(lm.wemwbs.1))[,2],
                     confint(lm.wemwbs.1),
                     p_value = coef(summary(lm.wemwbs.1))[,4]), 3)
write.csv(w1, file = "model_output/6_fu2_w1.csv")




### From post-int

## CORE

# Everything

stargazer(cbind(Estimate = coef(lm.core.a1), Std.Error = coef(summary(lm.core.a1))[,2],
                z.value = coef(summary(lm.core.a1))[,3], confint(lm.core.a1),
                p_value = coef(summary(lm.core.a1))[,4]), type = "text", style = "qje", digits = 3)

# Export
ca1 <- round_df(cbind(Estimate = coef(lm.core.a1), Std.Error = coef(summary(lm.core.a1))[,2],
                     confint(lm.core.a1),
                     p_value = coef(summary(lm.core.a1))[,4]), 3)
write.csv(ca1, file = "model_output/6_fu2_ca1.csv")


# Without outliers

stargazer(cbind(Estimate = coef(lm.core.a6), Std.Error = coef(summary(lm.core.a6))[,2],
                z.value = coef(summary(lm.core.a6))[,3], confint(lm.core.a6),
                p_value = coef(summary(lm.core.a6))[,4]), type = "text", style = "qje", digits = 3)

# Export
ca6 <- round_df(cbind(Estimate = coef(lm.core.a6), Std.Error = coef(summary(lm.core.a6))[,2],
                      confint(lm.core.a6),
                      p_value = coef(summary(lm.core.a6))[,4]), 3)
write.csv(ca6, file = "model_output/6_fu2_ca6.csv")


### WEMWBS

# Everyone

stargazer(cbind(Estimate = coef(lm.wemwbs.a1), Std.Error = coef(summary(lm.wemwbs.a1))[,2],
                z.value = coef(summary(lm.wemwbs.a1))[,3], confint(lm.wemwbs.a1),
                p_value = coef(summary(lm.wemwbs.a1))[,4]), type = "text", style = "qje", digits = 3)

# Export
wa1 <- round_df(cbind(Estimate = coef(lm.wemwbs.a1), Std.Error = coef(summary(lm.wemwbs.a1))[,2],
                     confint(lm.wemwbs.a1),
                     p_value = coef(summary(lm.wemwbs.a1))[,4]), 3)
write.csv(wa1, file = "model_output/6_fu2_wa1.csv")


# Without outliers

stargazer(cbind(Estimate = coef(lm.wemwbs.a6), Std.Error = coef(summary(lm.wemwbs.a6))[,2],
                z.value = coef(summary(lm.wemwbs.a6))[,3], confint(lm.wemwbs.a6),
                p_value = coef(summary(lm.wemwbs.a6))[,4]), type = "text", style = "qje", digits = 3)

# Export
wa6 <- round_df(cbind(Estimate = coef(lm.wemwbs.a6), Std.Error = coef(summary(lm.wemwbs.a6))[,2],
                      confint(lm.wemwbs.a6),
                      p_value = coef(summary(lm.wemwbs.a6))[,4]), 3)
write.csv(wa6, file = "model_output/6_fu2_wa6.csv")




