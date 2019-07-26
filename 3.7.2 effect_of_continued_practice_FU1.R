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

wide <- read_csv("data_input(4)/mindful_clean.csv",
                 col_types = cols(arm = col_factor(levels = c("intervention", "control", "MMJ")),
                                  gender = col_factor(levels = c("Female", "Male"))))

wide <- wide %>% rename(row = X1)

# Bin ethnicity
wide$ethnicity <- with(wide, ifelse(ethnicity == "White", "White", "Non-White"))

# Rename arm
wide <- wide %>% 
  mutate(intake = ifelse(arm == "intervention", "Intake 1",
                         ifelse(arm == "control", "Intake 2",
                                ifelse(arm == "MMJ", "Intake 3", NA))))
wide$intake <- factor(wide$intake, levels = c("Intake 1", "Intake 2", "Intake 3"))



### Plotting
# Let us plot how formal and informal practice at 1-year FU are related
lm.1 <-lm(formal_h_total_fu1 ~ informal_total_fu1, data = wide)
anova(lm.1)
wide %>%
  ggplot(aes(formal_h_total_fu1, informal_total_fu1)) +
  geom_point(alpha = 0.5, position = position_jitter(w = 4, h = 4)) +
  theme_bw() +
  geom_smooth(method = "lm") +
  labs(x = "Formal practice from start of course", y = "Informal practice from start of course")

wide %>%
  ggplot(aes(formal_h_after_fu1, informal_after_fu1)) +
  geom_point(position = position_jitter(w = 4, h = 4), alpha = 0.5) +
  theme_bw() +
  geom_smooth(method = "lm")+
  labs(x = "Formal practice after course", y = "Informal practice after course")





# Let's see if we get the same
wide.fun <- wide %>% filter(!is.na(core_fu1) & !is.na(formal_h_total_fu1))
lm.plotti <- lm(core_fu1 ~ formal_h_total_fu1, data = wide.fun)
summary(lm.plotti)
wide.fun$fun_predict <- predict(lm.plotti, type = "response")


ggplot(wide.fun, aes(x = formal_h_total_fu1, y = fun_predict)) +
  geom_point(aes(y = core_fu1),
             alpha=.5, position=position_jitter(h=.2), size = 1.8) +
  geom_line(size = 1.2)


##### Cleaning

# Want variables which are comparable.
summary(wide$formal_h_total_fu1) # Number of hours per week is this/52 (1 year)
wide$formal_h_total_fu1 <- (wide$formal_h_total_fu1)/52

summary(wide$formal_h_after_fu1) # Number of hours per week is this/44 (1 year - course)
# (which is formal_freq_h_week_fu1 really but ok)
wide$formal_h_after_fu1 <- (wide$formal_h_after_fu1)/44

summary(wide$informal_total_fu1) # To convert back to Never - Very often: need 5 levels.
# Maximum can be 5*52 = 260; rarely would be 1*52 = 52.
wide$informal_total_fu1 <- (wide$informal_total_fu1)/52

summary(wide$informal_after_fu1) # Maximum is 5*44 = 220 (informal_freq_fu1 but ok)
wide$informal_after_fu1 <- (wide$informal_after_fu1)/44


# Correlations
wide.cor <- wide %>%
  mutate(diff_c = core_post - core_base, diff_w = wb_post - wb_base) %>% 
  select(diff_c, diff_w, starts_with("core"), starts_with("wb"), contains("total"), contains("after"))
cor(wide.cor, method = "pearson", use = "na.or.complete")
pairs(wide.cor, lower.panel = NULL)

correlations <- cor(wide.cor, use = "pairwise.complete.obs", method = "pearson")
corrplot(correlations, method = "circle")

# This is the weirdest thing. Scores post-int seem to correlate
# with subsequenc informal practice (fu2, fu3)



##### 1-year FU

### CORE

## Baseline score + practice from baseline

wide_c1_unadj <- wide %>% # n = 176
  filter(!is.na(formal_h_total_fu1) & !is.na(informal_total_fu1) &
           !is.na(age) & !is.na(gender) & !is.na(core_base)) %>%
  mutate(row = row_number())

lm.c1.unadj <- lm(core_fu1 ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended +
                      age + gender + intake + core_base, data = wide_c1_unadj)
summary(lm.c1.unadj) # R^2 = 0.18, adj. R^2 = 0.15

# Adjusted for diasbility
wide_c1_adj <- wide %>% # n = 175
  filter(!is.na(formal_h_total_fu1) & !is.na(informal_total_fu1) & !is.na(age) &
           !is.na(gender) & !is.na(disability) & !is.na(core_base)) %>%
  mutate(row = row_number())

lm.c1.adj <- lm(core_fu1 ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended +
                    age + gender + intake + disability + core_base, data = wide_c1_adj)
summary(lm.c1.adj) 


# Check for collinearity
lm.col1 <- lm(formal_h_total_fu1 ~ informal_total_fu1, data = wide_c1_adj)
lm.col2 <- lm(formal_h_total_fu1 ~ sessions_attended, data = wide_c1_adj)
lm.col3 <- lm(informal_total_fu1 ~ informal_total_fu1, data = wide_c1_adj)
VIF(lm.col1) # 1.18 fine
VIF(lm.col2) # 1.01 fine
VIF(lm.col3) # 1.00 fine


# Check model for normality
par(mfrow = c(2,2))
plot(lm.c1.adj)
par(mfrow = c(1,1))
hist(lm.c1.adj$residuals)
ols_test_normality(lm.c1.adj)

# Not very normally distributed. Let's check the output of a model which squares the outcome.
# We cannot interpret the estimate but we can check whether the significance is similar.
lm.c1.adj.sqrt <- lm(sqrt(core_fu1) ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended +
                         age + gender + intake + disability + core_base, data = wide_c1_adj)
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

wide_c1_out <- wide_c1_adj %>% filter(row != 2 & row != 3 & row != 11 & row != 162
                                     & row != 56 & row != 82) %>% 
  mutate(row = row_number())

# Check whether significance changes dramatically
lm.c1.adj.out <- lm(core_fu1 ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended +
                        age + gender + intake + disability + core_base, data = wide_c1_out)
# Error. Remove intake
lm.c1.adj.out <- lm(core_fu1 ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended +
                        age + gender + disability + core_base, data = wide_c1_out)
summary(lm.c1.adj.out)
# Same variables are significant





## Post-int score + practice from post-int

wide_c2_unadj <- wide %>% # n = 198
  filter(!is.na(formal_h_after_fu1) & !is.na(informal_after_fu1) & !is.na(age) & !is.na(gender) &
           !is.na(core_post)) %>%
  mutate(row = row_number())

lm.c2.unadj <- lm(core_fu1 ~ formal_h_after_fu1 + informal_after_fu1 + sessions_attended +
                    age + gender + intake + core_post, data = wide_c2_unadj)
summary(lm.c2.unadj)


# Adjusted for disability

wide_c2_adj <- wide %>% # 197
  filter(!is.na(formal_h_after_fu1) & !is.na(informal_after_fu1) & !is.na(age) & !is.na(gender) &
           !is.na(disability) & !is.na(core_post)) %>%
  mutate(row = row_number())

lm.c2.adj <- lm(core_fu1 ~ formal_h_after_fu1 + informal_after_fu1 + sessions_attended +
                    age + gender + intake + disability + core_post, data = wide_c2_adj)
summary(lm.c2.adj)


# Check again whether p-values of model with sqrt() transformation are similar
lm.c2.adj.sqrt <- lm(sqrt(core_fu1) ~ formal_h_after_fu1 + informal_after_fu1 + sessions_attended +
                       age + gender + intake + disability + core_post, data = wide_cp_adj)
summary(lm.c2.adj.sqrt)
summary(lm.c2.adj) # ish, with disability being significant



## Let's investigate the outliers.
# Compute the cook's distance for each point
cooksd <- cooks.distance(lm.c2.adj)
# Plot that.
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=6:length(cooksd)+2, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# Let's remove row 188, 193, 56, 12

wide_c2_out <- wide_c2_adj %>% filter(row != 188 & row != 193 & row != 56 & row != 12) %>% 
  mutate(row = row_number())

# Check whether significance changes dramatically
lm.c2.adj.out <- lm(sqrt(core_fu1) ~ formal_h_after_fu1 + informal_after_fu1 + sessions_attended +
                       age + gender + intake + disability + core_post, data = wide_c2_out)
summary(lm.c2.adj.out)
# Less but still significant



### Extract all the CORE values

## Baseline scores and practice from baseline

# Unadjusted

stargazer(cbind(Estimate = coef(lm.c1.unadj), Std.Error = coef(summary(lm.c1.unadj))[,2],
                z.value = coef(summary(lm.c1.unadj))[,3], confint(lm.c1.unadj),
                p_value = coef(summary(lm.c1.unadj))[,4]), type = "text", style = "qje", digits = 3)

# Export
c1.unadj <- round_df(cbind(Estimate = coef(lm.c1.unadj), Std.Error = coef(summary(lm.c1.unadj))[,2],
                             confint(lm.c1.unadj),
                             p_value = coef(summary(lm.c1.unadj))[,4]), 3)
write.csv(c1.unadj, file = "model_output/6_fu1_c1_unadj.csv")


# Adjusted

stargazer(cbind(Estimate = coef(lm.c1.adj), Std.Error = coef(summary(lm.c1.adj))[,2],
                z.value = coef(summary(lm.c1.adj))[,3], confint(lm.c1.adj),
                p_value = coef(summary(lm.c1.adj))[,4]), type = "text", style = "qje", digits = 3)

# Export
c1.adj <- round_df(cbind(Estimate = coef(lm.c1.adj), Std.Error = coef(summary(lm.c1.adj))[,2],
                           confint(lm.c1.adj),
                           p_value = coef(summary(lm.c1.adj))[,4]), 3)
write.csv(c1.adj, file = "model_output/6_fu1_c1_adj.csv")



## Post-int scores and practice after post-int

# Unadjusted

stargazer(cbind(Estimate = coef(lm.c2.unadj), Std.Error = coef(summary(lm.c2.unadj))[,2],
                z.value = coef(summary(lm.c2.unadj))[,3], confint(lm.c2.unadj),
                p_value = coef(summary(lm.c2.unadj))[,4]), type = "text", style = "qje", digits = 3)

# Export
c2.unadj <- round_df(cbind(Estimate = coef(lm.c2.unadj), Std.Error = coef(summary(lm.c2.unadj))[,2],
                           confint(lm.c2.unadj),
                           p_value = coef(summary(lm.c2.unadj))[,4]), 3)
write.csv(c2.unadj, file = "model_output/6_fu1_c2_unadj.csv")


# Adjusted

stargazer(cbind(Estimate = coef(lm.c2.adj), Std.Error = coef(summary(lm.c2.adj))[,2],
                z.value = coef(summary(lm.c2.adj))[,3], confint(lm.c2.adj),
                p_value = coef(summary(lm.c2.adj))[,4]), type = "text", style = "qje", digits = 3)

# Export
c2.adj <- round_df(cbind(Estimate = coef(lm.c2.adj), Std.Error = coef(summary(lm.c2.adj))[,2],
                           confint(lm.c2.adj),
                           p_value = coef(summary(lm.c2.adj))[,4]), 3)
write.csv(c2.adj, file = "model_output/6_fu1_c2_adj.csv")




##### WEMWBS


## Baseline score + practice from baseline

wide_w1 <- wide %>% # n = 175
  filter(!is.na(formal_h_total_fu1) & !is.na(informal_total_fu1) & !is.na(age) &
           !is.na(gender) & !is.na(wb_base)) %>% 
  mutate(row = row_number())
lm.w1 <- lm(wb_fu1 ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended + age +
              gender + intake + wb_base, data = wide_w1)

summary(lm.w1) # R^2 = 0.23, adj. R^2 = 0.20



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
                  gender + intake + wb_base, data = wide_w1_out)
# Error. Remove intake
lm.w1.out <- lm(wb_fu1 ~ formal_h_total_fu1 + informal_total_fu1 + sessions_attended + age +
                  gender + wb_base, data = wide_w1_out)
summary(lm.w1.out) # Similar





## Post-int score + practice from post-int


wide_w2 <- wide %>% # n = 197
  filter(!is.na(formal_h_after_fu1) & !is.na(informal_after_fu1) & !is.na(age) &
           !is.na(gender) & !is.na(wb_post)) %>% 
  mutate(row = row_number())
lm.w2 <- lm(wb_fu1 ~ formal_h_after_fu1 + informal_after_fu1 + sessions_attended + age +
              gender + intake + wb_post, data = wide_w2)

summary(lm.w2) # R^2 = 0.29, adj. R^2 = 0.36


# Check model for normality
par(mfrow = c(2,2))
plot(lm.w2)
par(mfrow = c(1,1))
hist(lm.w2$residuals)
ols_test_normality(lm.w2) # Fine!

lm.col1 <- lm(formal_h_after_fu1 ~ informal_after_fu1, data = wide_w2)
lm.col2 <- lm(formal_h_after_fu1 ~ sessions_attended, data = wide_w2)
lm.col3 <- lm(informal_after_fu1 ~ sessions_attended, data = wide_w2)
VIF(lm.col1) # 1.14 fine
VIF(lm.col2) # 1.00 fine
VIF(lm.col3) # 1.01 what



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



### Extract all the WEMWBS values

## Baseline scores and practice from baseline

stargazer(cbind(Estimate = coef(lm.w1), Std.Error = coef(summary(lm.w1))[,2],
                z.value = coef(summary(lm.w1))[,3], confint(lm.w1),
                p_value = coef(summary(lm.w1))[,4]), type = "text", style = "qje", digits = 3)

# Export
w1 <- round_df(cbind(Estimate = coef(lm.w1), Std.Error = coef(summary(lm.w1))[,2],
                           confint(lm.w1),
                           p_value = coef(summary(lm.w1))[,4]), 3)
write.csv(w1, file = "model_output/6_fu1_w1.csv")



## Post-int scores and practice from post-int

stargazer(cbind(Estimate = coef(lm.w2), Std.Error = coef(summary(lm.w2))[,2],
                z.value = coef(summary(lm.w2))[,3], confint(lm.w2),
                p_value = coef(summary(lm.w2))[,4]), type = "text", style = "qje", digits = 3)

# Export
w2 <- round_df(cbind(Estimate = coef(lm.w2), Std.Error = coef(summary(lm.w2))[,2],
                     confint(lm.w2),
                     p_value = coef(summary(lm.w2))[,4]), 3)
write.csv(w2, file = "model_output/6_fu1_w2.csv")




##### Plotting (only after course)

# CORE

wide_cplot <- wide %>% filter(!is.na(core_fu1) & !is.na(informal_after_fu1)) # n = 263

lm.cplot <- lm(core_fu1 ~ informal_after_fu1, data = wide_cplot)
summary(lm.cplot) # not significant anymore, but if we correct for core_post, it is

data <- with(wide_cplot, data.frame(core_post = mean(core_post, na.rm = TRUE),
                                    informal_after_fu1 = rep(seq(from = 0, to = 175,
                                                                 length.out = 20),4)))

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




# WEMWBS

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

# WEMWBS sessions

wide_splot <- wide %>% filter(!is.na(wb_fu1)) # n = 268

lm.splot <- lm(wb_fu1 ~ sessions_attended, data = wide_splot)
summary(lm.splot)


# Predict probability and standard error
s.plot <- cbind(wide_splot, predict(lm.splot, type = "response", se = TRUE))
s.plot <- within(s.plot, {
  PredictedWemwbs <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Plot
ggplot(s.plot, aes(x = sessions_attended, y = PredictedWemwbs)) +
  geom_point(aes(y = wb_fu1),
             position = position_jitter(w = 0.05), alpha = 0.3) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = col1, alpha = 0.2) +
  geom_line(size = 1, col = col1) +
  theme_bw() +
  labs(x = "Attended course sessions",
       y = "1-year FU WEMWBS") +
  theme(
    axis.text = element_text(size = 12, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 14, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 14, face = "bold", margin=margin(0,10,0,5))
  )


### CORE plotting from baseline


wide_cplot <- wide %>% filter(!is.na(core_fu1) & !is.na(informal_total_fu1)) # n = 177

lm.cplot <- lm(core_fu1 ~ informal_total_fu1, data = wide_cplot)
summary(lm.cplot)


# Predict probability and standard error
core.plot <- cbind(wide_cplot, predict(lm.cplot, type = "response", se = TRUE))
core.plot <- within(core.plot, {
  PredictedCore <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Plot
ggplot(core.plot, aes(x = informal_total_fu1, y = PredictedCore)) +
  geom_point(aes(y = core_fu1), alpha = 0.3) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = col1, alpha = 0.2) +
  geom_line(size = 1, col = col1) +
  theme_bw() +
  labs(x = "Informal practice up to 1 year after",
       y = "1-year FU CORE-OM") +
  theme(
    axis.text = element_text(size = 12, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 14, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 14, face = "bold", margin=margin(0,10,0,5))
  )


# WEMWBS

wide_wplot <- wide %>% filter(!is.na(wb_fu1) & !is.na(informal_total_fu1)) # n = 264

lm.wplot <- lm(wb_fu1 ~ informal_total_fu1, data = wide_wplot)
summary(lm.wplot)


# Predict probability and standard error
wemwbs.plot <- cbind(wide_wplot, predict(lm.wplot, type = "response", se = TRUE))
wemwbs.plot <- within(wemwbs.plot, {
  PredictedWemwbs <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

# Plot
ggplot(wemwbs.plot, aes(x = informal_total_fu1, y = PredictedWemwbs)) +
  geom_point(aes(y = wb_fu1),
             position = position_jitter(w = 0.02), alpha = 0.3) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = col1, alpha = 0.2) +
  geom_line(size = 1, col = col1) +
  theme_bw() +
  labs(x = "Informal practice up to 1 year after",
       y = "1-year FU WEMWBS") +
  theme(
    axis.text = element_text(size = 12, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 14, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 14, face = "bold", margin=margin(0,10,0,5))
  )
