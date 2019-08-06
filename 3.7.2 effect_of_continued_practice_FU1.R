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


### Read in data
wide <- read_csv("data_input(4)/mindful_clean.csv")



##### Stats

### CORE

# Unadjusted
wide_c1_unadj <- wide %>% # n = 176
  filter(!is.na(formal_h_total_fu1) & !is.na(informal_total_fu1) &
           !is.na(age) & !is.na(gender) & !is.na(core_post)) %>%
  mutate(row = row_number())

lm.c1.unadj <- lm(core_fu1 ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended +
                      age + gender + intake + core_post, data = wide_c1_unadj)
summary(lm.c1.unadj)

# Adjusted for diasbility
wide_c1_adj <- wide %>% # n = 175
  filter(!is.na(formal_h_total_fu1) & !is.na(informal_total_fu1) & !is.na(age) &
           !is.na(gender) & !is.na(disability) & !is.na(core_post)) %>%
  mutate(row = row_number())

lm.c1.adj <- lm(core_fu1 ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended +
                    age + gender + intake + disability + core_post, data = wide_c1_adj)
summary(lm.c1.adj) 


# Check for collinearity of formal practice, informal practice, and attendance
VIF(lm.c1.adj)
# Fine


# Check model for normality
par(mfrow = c(2,2))
plot(lm.c1.adj)
par(mfrow = c(1,1))
hist(lm.c1.adj$residuals)
ols_test_normality(lm.c1.adj)

# Not very normally distributed. Let's check the output of a model which squares the outcome.
# We cannot interpret the estimate but we can check whether the significance is similar.
lm.c1.adj.sqrt <- lm(sqrt(core_fu1) ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended +
                         age + gender + intake + disability + core_post, data = wide_c1_adj)
# Check for normality
par(mfrow = c(2,2))
plot(lm.c1.adj.sqrt)
par(mfrow = c(1,1))
hist(lm.c1.adj.sqrt$residuals)
ols_test_normality(lm.c1.adj.sqrt) # Better

# Compare sqrt and non-sqrt significances
summary(lm.c1.adj.sqrt)
summary(lm.c1.adj) # Similar p-values, except that disability becomes significant (good that
# we are correcting for it).


## Let's investigate the outliers.
# Compute the cook's distance for each point
cooksd <- cooks.distance(lm.c1.adj)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=6:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 2, 3, 11, 162, 56, 82

# Create new data set
wide_c1_out <- wide_c1_adj %>% filter(row != 2 & row != 3 & row != 11 & row != 162
                                     & row != 56 & row != 82) %>% 
  mutate(row = row_number())

# Check whether significance changes dramatically
lm.c1.adj.out <- lm(core_fu1 ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended +
                        age + gender + intake + disability + core_post, data = wide_c1_out)
# Error. Remove intake
lm.c1.adj.out <- lm(core_fu1 ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended +
                        age + gender + disability + core_post, data = wide_c1_out)
summary(lm.c1.adj.out)
# Same variables are significant



### Extract all the CORE values

# Unadjusted
stargazer(cbind(Estimate = coef(lm.c1.unadj), Std.Error = coef(summary(lm.c1.unadj))[,2],
                z.value = coef(summary(lm.c1.unadj))[,3], confint(lm.c1.unadj),
                p_value = coef(summary(lm.c1.unadj))[,4]), type = "text", style = "qje", digits = 3)

# Adjusted
stargazer(cbind(Estimate = coef(lm.c1.adj), Std.Error = coef(summary(lm.c1.adj))[,2],
                z.value = coef(summary(lm.c1.adj))[,3], confint(lm.c1.adj),
                p_value = coef(summary(lm.c1.adj))[,4]), type = "text", style = "qje", digits = 3)




### WEMWBS

# Create dataset
wide_w1 <- wide %>% # n = 175
  filter(!is.na(formal_h_total_fu1) & !is.na(informal_total_fu1) & !is.na(age) &
           !is.na(gender) & !is.na(wb_post)) %>% 
  mutate(row = row_number())

# Model
lm.w1 <- lm(wb_fu1 ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended + age +
              gender + intake + wb_post, data = wide_w1)
summary(lm.w1)

# Check model for normality
par(mfrow = c(2,2))
plot(lm.w1)
par(mfrow = c(1,1))
hist(lm.w1$residuals)
ols_test_normality(lm.w1) # Fine!


## Let's investigate the outliers.
# Compute the cook's distance for each point
cooksd <- cooks.distance(lm.w1)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=6:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 2, 3

wide_w1_out <- wide_w1 %>% filter(row != 2 & row != 3) %>% 
  mutate(row = row_number())

# Check whether significance changes dramatically
lm.w1_out <- lm(wb_fu1 ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended + age +
                  gender + intake + wb_post, data = wide_w1_out)
# Error. Remove intake
lm.w1.out <- lm(wb_fu1 ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended + age +
                  gender + wb_post, data = wide_w1_out)
summary(lm.w1.out) # Similar


## Check for multicollinearity between the practices and attendance
vif(lm.w1)
# Fine.

# Compute the cook's distance for each point
cooksd <- cooks.distance(lm.w2)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=6:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 188, 112

wide_w2_out <- wide_w2 %>% filter(row != 188 & row != 112) %>% 
  mutate(row = row_number())

# Check whether significance changes dramatically
lm.w2.out <- lm(wb_fu1 ~ formal_h_after_fu1 + informal_after_fu1 + sessions_attended + age +
                  gender + intake + wb_post, data = wide_w2_out)
summary(lm.w2.out) # Less but still significant



### Extract WEMWBS values

stargazer(cbind(Estimate = coef(lm.w1), Std.Error = coef(summary(lm.w1))[,2],
                z.value = coef(summary(lm.w1))[,3], confint(lm.w1),
                p_value = coef(summary(lm.w1))[,4]), type = "text", style = "qje", digits = 3)



##### Plotting

### CORE

# Create dataset
wide_cplot <- wide %>% filter(!is.na(core_fu1) & !is.na(informal_after_fu1)) # n = 263

# Create model which only contains those variables which will be plotted
lm.cplot <- lm(core_fu1 ~ informal_after_fu1, data = wide_cplot)
summary(lm.cplot)


# Predict probability and standard error
core.plot <- cbind(wide_cplot, predict(lm.cplot, type = "response", se = TRUE))
core.plot <- within(core.plot, {
  PredictedCore <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Plot
ggplot(core.plot, aes(x = informal_after_fu1, y = PredictedCore)) +
  geom_point(aes(y = core_fu1),
             position = position_jitter(w = 0.08), alpha = 0.3) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = col1, alpha = 0.2) +
  geom_line(size = 1, col = col1) +
  theme_bw() +
  labs(x = "Informal practice after course",
       y = "CORE-OM 1-year FU") +
  theme(
    text = element_text(family = "Leelawadee UI Semilight"),
    axis.text = element_text(size = 16, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 18, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 18, face = "bold", margin=margin(0,15,0,5))
  ) -> fu1.c

ggsave(fu1.c, filename = "plots_thesis/6_fu1_core.png", device = "png",
       width = 7, height= 5, units = "in")




### WEMWBS

wide_wplot <- wide %>% filter(!is.na(wb_fu1) & !is.na(informal_after_fu1)) # n = 264

lm.wplot <- lm(wb_fu1 ~ informal_after_fu1, data = wide_wplot)
summary(lm.wplot)


# Predict probability and standard error
wemwbs.plot <- cbind(wide_wplot, predict(lm.wplot, type = "response", se = TRUE))
wemwbs.plot <- within(wemwbs.plot, {
  PredictedWemwbs <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Plot
ggplot(wemwbs.plot, aes(x = informal_after_fu1, y = PredictedWemwbs)) +
  geom_point(aes(y = wb_fu1),
             position = position_jitter(w = 0.08), alpha = 0.3) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = col1, alpha = 0.2) +
  geom_line(size = 1, col = col1) +
  theme_bw() +
  labs(x = "Informal practice after course",
       y = "WEMWBS 1-year FU") +
  scale_y_continuous(breaks = c(20,40,60)) +
  theme(
    text = element_text(family = "Leelawadee UI Semilight"),
    axis.text = element_text(size = 16, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 18, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 18, face = "bold", margin=margin(0,15,0,5))
  ) -> fu1.w

ggsave(fu1.w, filename = "plots_thesis/6_fu1_wemwbs.png", device = "png",
       width = 7, height= 5, units = "in")
