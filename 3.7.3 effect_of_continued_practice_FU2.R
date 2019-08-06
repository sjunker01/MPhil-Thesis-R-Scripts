library(MASS)
library(tidyverse)
library(lme4)
library(lmerTest)
library(olsrr)
library(fmsb)
library(yhat)
library(stargazer)


### Read in file
wide <- read_csv("data_input(4)/mindful_clean.csv")



##### Stats

### Not enough data to add any confounders.

# Create data frame which contains only people who have data for all the tested variable
wide.reduced.c <- wide %>% filter(!is.na(core_fu2) & !is.na(formal_h_after_fu2) &
                                      !is.na(informal_after_fu2) & !is.na(core_fu1)) %>% 
  mutate(row = row_number()) # Has 52 observations...

# Model
lm.core.1 <- lm(core_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                      sessions_attended + core_fu1, data = wide.reduced.c)

# Check this model for normality
par(mfrow = c(2,2))
plot(lm.core.1) 
par(mfrow = c(1,1))
hist(lm.core.1$residuals)
ols_test_normality(lm.core.1) # Ok

# Compute the cook's distance for each point
cooksd <- cooks.distance(lm.core.1)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 10.

wide.out2 <- wide.reduced.c %>% filter(row != 10) %>% mutate(row = row_number())

# Next model
lm.core.2 <- lm(core_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                      sessions_attended + core_fu1, data = wide.out2)

# Cooks distance
cooksd <- cooks.distance(lm.core.2)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 35

wide.out3 <- wide.out2 %>% filter(row != 35) %>% mutate(row = row_number())

# Next model
lm.core.3 <- lm(core_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                      sessions_attended + core_fu1, data = wide.out3)

# Cooks distance
cooksd <- cooks.distance(lm.core.3)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 29

wide.out4 <- wide.out3 %>% filter(row != 29) %>% mutate(row = row_number())

# Next model
lm.core.4 <- lm(core_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                      sessions_attended + core_fu1, data = wide.out4)

# Cooks distance
cooksd <- cooks.distance(lm.core.4)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 45

wide.out5 <- wide.out4 %>% filter(row != 45) %>% mutate(row = row_number())

# Next model
lm.core.5 <- lm(core_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                      sessions_attended + core_fu1, data = wide.out5)

# Cooks distance
cooksd <- cooks.distance(lm.core.5)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 7

wide.out6 <- wide.out5 %>% filter(row != 7) %>% mutate(row = row_number())

# Next model
lm.core.6 <- lm(core_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                      sessions_attended + core_fu1, data = wide.out6)

# Cooks distance
cooksd <- cooks.distance(lm.core.6)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Ok whatever

# Let's compare the analyses of all models.
summary(lm.core.1)
summary(lm.core.2)
summary(lm.core.3)
summary(lm.core.4)
summary(lm.core.5)
summary(lm.core.6)

# The more outliers we remove, the more likely formal practice becomes to be significantly
# affecting the scores... Needs more investigation.



##### WEMWBS

# Create dataset
wide.reduced.w <- wide %>% filter(!is.na(wb_fu2) & !is.na(formal_h_after_fu2) &
                                    !is.na(informal_after_fu2) & !is.na(wb_fu1)) %>% 
  mutate(row = row_number()) # Has 52 observations...

# Model
lm.wemwbs.1 <- lm(wb_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                   sessions_attended + wb_fu1, data = wide.reduced.w)

# Check this model for normality
par(mfrow = c(2,2))
plot(lm.wemwbs.1) 
par(mfrow = c(1,1))
hist(lm.wemwbs.1$residuals)
ols_test_normality(lm.wemwbs.1) # Ok

# Compute the cook's distance for each point
cooksd <- cooks.distance(lm.wemwbs.1)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 36.

wide.out2 <- wide.reduced.2 %>% filter(row != 36) %>% mutate(row = row_number())

# Next model
lm.wemwbs.2 <- lm(wb_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                   sessions_attended + wb_fu1, data = wide.out2)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.2)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 10

wide.out3 <- wide.out2 %>% filter(row != 10) %>% mutate(row = row_number())

# Next model
lm.wemwbs.3 <- lm(wb_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                   sessions_attended + wb_fu1, data = wide.out3)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.3)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 29

wide.out4 <- wide.out3 %>% filter(row != 29) %>% mutate(row = row_number())

# Next model
lm.wemwbs.4 <- lm(wb_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                   sessions_attended + wb_fu1, data = wide.out4)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.4)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 45

wide.out5 <- wide.out4 %>% filter(row != 45) %>% mutate(row = row_number())

# Next model
lm.wemwbs.5 <- lm(wb_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                   sessions_attended + wb_fu1, data = wide.out5)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.5)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 40

wide.out6 <- wide.out5 %>% filter(row != 40) %>% mutate(row = row_number())

# Next model
lm.wemwbs.6 <- lm(wb_fu2 ~ formal_h_after_fu2 + informal_after_fu2 +
                   sessions_attended + wb_fu1, data = wide.out6)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.6)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Ok

# Let's compare the analyses of all models.
summary(lm.wemwbs.1)
summary(lm.wemwbs.2)
summary(lm.wemwbs.3)
summary(lm.wemwbs.4)
summary(lm.wemwbs.5)
summary(lm.wemwbs.6)

# The more outliers we remove, the more likely formal practice becomes to be significantly
# affecting the scores (getting worse).


##### Extract values

## CORE
stargazer(cbind(Estimate = coef(lm.core.1), Std.Error = coef(summary(lm.core.1))[,2],
                z.value = coef(summary(lm.core.1))[,3], confint(lm.core.1),
                p_value = coef(summary(lm.core.1))[,4]), type = "text", style = "qje", digits = 3)

## WEMWBS
stargazer(cbind(Estimate = coef(lm.wemwbs.1), Std.Error = coef(summary(lm.wemwbs.1))[,2],
                z.value = coef(summary(lm.wemwbs.1))[,3], confint(lm.wemwbs.1),
                p_value = coef(summary(lm.wemwbs.1))[,4]), type = "text", style = "qje", digits = 3)

