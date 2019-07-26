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

### Create color palette (darkest)
cols <- c("#e47e32", "#ff9e1f", "#ae9764", "#719f8d", "#509094", "#d2c078")
col1 <- cols[1]
col2 <- cols[2]
col3 <- cols[3]
col4 <- cols[4]
col5 <- cols[5]
col6 <- cols[6]


wide <- read_csv("data_input(4)/mindful_clean.csv")

wide <- wide %>% rename(row = X1)

# Want variables which are comparable.
summary(wide$formal_h_total_fu3) # Number of hours per week is this/156 (1 year)
wide$formal_h_total_fu3 <- (wide$formal_h_total_fu3)/156

summary(wide$formal_h_after_fu3) # Number of hours per week is this/96 (1 year - course)
# (which is formal_freq_h_week_fu1 really but ok)
wide$formal_h_after_fu3 <- (wide$formal_h_after_fu3)/148

summary(wide$informal_total_fu3) # To convert back to Never - Very often: need 5 levels.
# Maximum can be 5*52 = 260; rarely would be 1*52 = 52.
wide$informal_total_fu3 <- (wide$informal_total_fu3)/156

summary(wide$informal_after_fu3) # Maximum is 5*44 = 220 (informal_freq_fu1 but ok)
wide$informal_after_fu3 <- (wide$informal_after_fu3)/148




# Make data frame which contains only people who have data for all the tested variables
wide.reduced <- wide %>% filter(!is.na(core_fu3) & !is.na(formal_h_total_fu3) &
                                  !is.na(informal_total_fu3)) %>% 
  mutate(row = row_number()) # Has 32 observations...



## Baseline score and practice from baseline
lm.core.1 <- lm(core_fu3 ~ formal_h_total_fu3 + informal_total_fu3 +
                  sessions_attended, data = wide.reduced)

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
# Looks fine but Let's remove row 11

wide.out <- wide.reduced %>% filter(row != 11) %>%
  mutate(row = row_number())

# Model without outliers
lm.core.2 <- lm(core_fu3 ~ formal_h_total_fu3 + informal_total_fu3 +
                  sessions_attended, data = wide.out)

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
wide.reduced <- wide %>% filter(!is.na(wb_fu3) & !is.na(formal_h_total_fu3) &
                                  !is.na(informal_total_fu3)) %>% 
  mutate(row = row_number()) # Has 32 observations...



## Baseline score and practice from baseline
lm.wemwbs.1 <- lm(wb_fu3 ~ formal_h_total_fu3 + informal_total_fu3 +
                    sessions_attended, data = wide.reduced)

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
# Let's remove row 11

wide.out <- wide.reduced %>% filter(row != 27) %>%
  mutate(row = row_number())

# Model without outliers
lm.wemwbs.2 <- lm(wb_fu3 ~ formal_h_total_fu3 + informal_total_fu3 +
                    sessions_attended, data = wide.out)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.2)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Remove 28


wide.out1 <- wide.out %>% filter(row != 28) %>%
  mutate(row = row_number())

# Model without outliers
lm.wemwbs.3 <- lm(wb_fu3 ~ formal_h_total_fu3 + informal_total_fu3 +
                    sessions_attended, data = wide.out1)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.3)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Remove 21


wide.out2 <- wide.out1 %>% filter(row != 21) %>%
  mutate(row = row_number())

# Model without outliers
lm.wemwbs.4 <- lm(wb_fu3 ~ formal_h_total_fu3 + informal_total_fu3 +
                    sessions_attended, data = wide.out2)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.4)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Remove 11 

wide.out3 <- wide.out2 %>% filter(row != 11) %>%
  mutate(row = row_number())

# Model without outliers
lm.wemwbs.5 <- lm(wb_fu3 ~ formal_h_total_fu3 + informal_total_fu3 +
                    sessions_attended, data = wide.out3)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.5)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Fine


# Compare the analyses
summary(lm.wemwbs.1)
summary(lm.wemwbs.2)
summary(lm.wemwbs.3)
summary(lm.wemwbs.4)
summary(lm.wemwbs.5)
# Suddently sessions is significant, and informal practice is almost below p = 0.05

ggplot(wide.out3, aes(sessions_attended, wb_fu3)) +
  geom_point() +
  geom_smooth(method = "lm") + theme_bw()
# Lolol






##### Post-int score + practice after post-int

# Create dataset
wide.reduced.2 <- wide %>% filter(!is.na(core_fu3) & !is.na(formal_h_after_fu3) &
                                    !is.na(informal_after_fu3)) %>% 
  mutate(row = row_number()) # Has 32 observations...

## Model with square root transformation of dependent variable.
lm.core.a1 <- lm(core_fu3 ~ formal_h_after_fu3 + informal_after_fu3 +
                   sessions_attended, data = wide.reduced.2)

# Check this model for normality
par(mfrow = c(2,2))
plot(lm.core.a1) 
par(mfrow = c(1,1))
hist(lm.core.a1$residuals)
ols_test_normality(lm.core.a1) # Not great

# Compute the cook's distance for each point
cooksd <- cooks.distance(lm.core.a1)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 11

wide.out2 <- wide.reduced.2 %>% filter(row != 11) %>% mutate(row = row_number())

# Next model
lm.core.a2 <- lm(core_fu3 ~ formal_h_after_fu3 + informal_after_fu3 +
                   sessions_attended, data = wide.out2)

# Cooks distance
cooksd <- cooks.distance(lm.core.a2)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's leave it


summary(lm.core.a1)
summary(lm.core.a2)





##### WEMWBS

# Create dataset
wide.reduced.2 <- wide %>% filter(!is.na(wb_fu3) & !is.na(formal_h_after_fu3) &
                                    !is.na(informal_after_fu3)) %>% 
  mutate(row = row_number()) # Has 32 observations...

## Model with square root transformation of dependent variable.
lm.wemwbs.a1 <- lm(wb_fu3 ~ formal_h_after_fu3 + informal_after_fu3 +
                     sessions_attended, data = wide.reduced.2)

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
# Let's remove row 27

wide.out2 <- wide.reduced.2 %>% filter(row != 27) %>% mutate(row = row_number())

# Next model
lm.wemwbs.a2 <- lm(wb_fu3 ~ formal_h_after_fu3 + informal_after_fu3 +
                     sessions_attended, data = wide.out2)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.a2)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Remove 28

wide.out3 <- wide.out2 %>% filter(row != 28) %>% mutate(row = row_number())

# Next model
lm.wemwbs.a3 <- lm(wb_fu3 ~ formal_h_after_fu3 + informal_after_fu3 +
                     sessions_attended, data = wide.out3)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.a3)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's just leave it


summary(lm.wemwbs.a1)
summary(lm.wemwbs.a2)
summary(lm.wemwbs.a3)




## CORE

stargazer(cbind(Estimate = coef(lm.core.1), Std.Error = coef(summary(lm.core.1))[,2],
                z.value = coef(summary(lm.core.1))[,3], confint(lm.core.1),
                p_value = coef(summary(lm.core.1))[,4]), type = "text", style = "qje", digits = 3)

# Export
c1 <- round_df(cbind(Estimate = coef(lm.core.1), Std.Error = coef(summary(lm.core.1))[,2],
                     confint(lm.core.1),
                     p_value = coef(summary(lm.core.1))[,4]), 3)
write.csv(c1, file = "model_output/6_fu3_c1.csv")


### WEMWBS


stargazer(cbind(Estimate = coef(lm.wemwbs.1), Std.Error = coef(summary(lm.wemwbs.1))[,2],
                z.value = coef(summary(lm.wemwbs.1))[,3], confint(lm.wemwbs.1),
                p_value = coef(summary(lm.wemwbs.1))[,4]), type = "text", style = "qje", digits = 3)

# Export
w1 <- round_df(cbind(Estimate = coef(lm.wemwbs.1), Std.Error = coef(summary(lm.wemwbs.1))[,2],
                     confint(lm.wemwbs.1),
                     p_value = coef(summary(lm.wemwbs.1))[,4]), 3)
write.csv(w1, file = "model_output/6_fu3_w1.csv")




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
write.csv(ca1, file = "model_output/6_fu3_ca1.csv")


### WEMWBS

# Everyone

stargazer(cbind(Estimate = coef(lm.wemwbs.a1), Std.Error = coef(summary(lm.wemwbs.a1))[,2],
                z.value = coef(summary(lm.wemwbs.a1))[,3], confint(lm.wemwbs.a1),
                p_value = coef(summary(lm.wemwbs.a1))[,4]), type = "text", style = "qje", digits = 3)

# Export
wa1 <- round_df(cbind(Estimate = coef(lm.wemwbs.a1), Std.Error = coef(summary(lm.wemwbs.a1))[,2],
                      confint(lm.wemwbs.a1),
                      p_value = coef(summary(lm.wemwbs.a1))[,4]), 3)
write.csv(wa1, file = "model_output/6_fu3_wa1.csv")
