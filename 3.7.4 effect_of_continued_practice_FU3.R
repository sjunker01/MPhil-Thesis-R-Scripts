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

### Create color palette
cols <- c("#e47e32", "#ff9e1f", "#ae9764", "#719f8d", "#509094", "#d2c078")
col1 <- cols[1]
col2 <- cols[2]
col3 <- cols[3]
col4 <- cols[4]
col5 <- cols[5]
col6 <- cols[6]

# Read in file
wide <- read_csv("data_input(4)/mindful_clean.csv")



# Create data frame which contains only people who have data for all the tested variables
wide.reduced.c <- wide %>% filter(!is.na(core_fu3) & !is.na(formal_h_after_fu3) &
                                    !is.na(informal_after_fu3)) %>% 
  mutate(row = row_number()) # Has 32 observations...

## Model
lm.core.1 <- lm(core_fu3 ~ formal_h_after_fu3 + informal_after_fu3 +
                   sessions_attended, data = wide.reduced.c)

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
# Let's remove row 11

wide.out2 <- wide.reduced.c %>% filter(row != 11) %>% mutate(row = row_number())

# Next model
lm.core.2 <- lm(core_fu3 ~ formal_h_after_fu3 + informal_after_fu3 +
                   sessions_attended, data = wide.out2)

# Cooks distance
cooksd <- cooks.distance(lm.core.2)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's leave it


summary(lm.core.1)
summary(lm.core.2)
# Similar




##### WEMWBS

# Create dataset
wide.reduced.w <- wide %>% filter(!is.na(wb_fu3) & !is.na(formal_h_after_fu3) &
                                    !is.na(informal_after_fu3)) %>% 
  mutate(row = row_number()) # Has 32 observations...

## Model with square root transformation of dependent variable.
lm.wemwbs.1 <- lm(wb_fu3 ~ formal_h_after_fu3 + informal_after_fu3 +
                     sessions_attended, data = wide.reduced.w)

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
# Let's remove row 27

wide.out2 <- wide.reduced.w %>% filter(row != 27) %>% mutate(row = row_number())

# Next model
lm.wemwbs.2 <- lm(wb_fu3 ~ formal_h_after_fu3 + informal_after_fu3 +
                     sessions_attended, data = wide.out2)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.2)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Remove 28

wide.out3 <- wide.out2 %>% filter(row != 28) %>% mutate(row = row_number())

# Next model
lm.wemwbs.3 <- lm(wb_fu3 ~ formal_h_after_fu3 + informal_after_fu3 +
                     sessions_attended, data = wide.out3)

# Cooks distance
cooksd <- cooks.distance(lm.wemwbs.3)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Ok


summary(lm.wemwbs.1)
summary(lm.wemwbs.2)
summary(lm.wemwbs.3)
# Similar


### Extract values

## CORE
stargazer(cbind(Estimate = coef(lm.core.1), Std.Error = coef(summary(lm.core.1))[,2],
                z.value = coef(summary(lm.core.1))[,3], confint(lm.core.1),
                p_value = coef(summary(lm.core.1))[,4]), type = "text", style = "qje", digits = 3)

## WEMWBS
stargazer(cbind(Estimate = coef(lm.wemwbs.1), Std.Error = coef(summary(lm.wemwbs.1))[,2],
                z.value = coef(summary(lm.wemwbs.1))[,3], confint(lm.wemwbs.1),
                p_value = coef(summary(lm.wemwbs.1))[,4]), type = "text", style = "qje", digits = 3)
