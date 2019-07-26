library(MASS)
library(tidyverse)
library(car)
library(dunn.test)
library(lme4)
library(lmerTest)
library(gridExtra)


wide <- read_csv("data_input(4)/mindful_clean.csv")
long <- read_csv("data_input(4)/mindful_long.csv")

wide <- wide %>% dplyr::select(-X1)
long <- long %>% dplyr::select(-X1)

### Cleaning
long$timepoint <- factor(long$timepoint, levels = c("base", "post", "fu1", "fu2", "fu3"))


### Create color palette (darkest)
cols <- c("#e47e32", "#ff9e1f", "#ae9764", "#719f8d", "#509094", "#d2c078")
col1 <- cols[1]
col2 <- cols[2]
col3 <- cols[3]
col4 <- cols[4]
col5 <- cols[5]
col6 <- cols[6]

# Create function to round numeric values in a heterogenic data frame to x digits
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


### Check for equal variances.

# Summarize data
aggregate(core_score ~ timepoint, data = long, summary)
aggregate(wb_score ~ timepoint, data = long, summary)
# We can already see that the range of scores for the later timepoints is smaller.



### Check what determines how the scores develop over time
## Correcting for gender, age, arm, and disability.
# Remove timepoints base and post from the model
long_followup <- long %>% filter(timepoint != "base" & timepoint != "post")

lmer.att <- lmer(core_score ~ timepoint*sessions_attended + age + gender + arm + disability +
                   core_post + (1|id), data = long_followup)
summary(lmer.att)

# Make dataset and predict numbers, check whether they match your calculation
test.data <- with(long_effect, data.frame(timepoint = rep(c("post", "fu1", "fu2", "fu3"), 9),
                                          age = 25, gender = "Female", arm = "control",
                                          disability = TRUE, core_post = 1,
                                          sessions_attended = c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,
                                                                4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,
                                                                8,8,8,8)))

test.result <- cbind(test.data, predict(lmer.att, newdata = test.data, type = "response", SE = TRUE))
test.data$predicted <-  predict(lmer.att, newdata = test.data, type = "response")

# Same from baseline
long_effect <- long %>% filter(timepoint != "base")

lmer.att2 <- lmer(core_score ~ timepoint*sessions_attended + age + gender + arm + disability +
                   core_base + (1|id), data = long_effect)
summary(lmer.att2)

stargazer(cbind(Estimate = coef(summary(lmer.att2))[,1], Std.Error = coef(summary(lmer.att2))[,2],
                p_value = coef(summary(lmer.att2))[,5]), type = "text", style = "qje", digits = 3)



### Look at data compared to baseline

# CORE without interaction
lmer.att3 <- lmer(core_score ~ timepoint + sessions_attended + age + gender + arm + disability +
                    core_base + (1|id), data = long)
summary(lmer.att3)
# CORE with interaction
lmer.att4 <- lmer(core_score ~ timepoint * sessions_attended + age + gender + arm + disability +
                    core_base + (1|id), data = long)
summary(lmer.att4)

# WEMWBS without interaction
lmer.att5 <- lmer(wb_score ~ timepoint + sessions_attended + age + gender + arm + disability +
                    wb_base + (1|id), data = long)
summary(lmer.att5)
# WEMWBS with interaction
lmer.att6 <- lmer(wb_score ~ timepoint * sessions_attended + age + gender + arm + disability +
                    wb_base + (1|id), data = long)
summary(lmer.att6)
##### NICEEEE


# Export. Confidence interval has to be computed extra since it has two more rows.....

# CORE

# No interaction between timepoint and sessions
core.noint <- round_df(cbind(Estimate = coef(summary(lmer.att3))[,1],
                             Std.Error = coef(summary(lmer.att3))[,2],
                             p_value = coef(summary(lmer.att3))[,5]), 3)
core.noint.conf <- round(confint(lmer.att3), 3)

write.csv(core.noint, file = "model_output/5_core_noint.csv")
write.csv(core.noint.conf, file = "model_output/5_core_noint_conf.csv")


# With interaction
core.int <- round_df(cbind(Estimate = coef(summary(lmer.att4))[,1],
                             Std.Error = coef(summary(lmer.att4))[,2],
                             p_value = coef(summary(lmer.att4))[,5]), 3)
core.int.conf <- round(confint(lmer.att4), 3)

write.csv(core.int, file = "model_output/5_core_int.csv")
write.csv(core.int.conf, file = "model_output/5_core_int_conf.csv")


# WEMWBS

# No interaction
wemwbs.noint <- round_df(cbind(Estimate = coef(summary(lmer.att5))[,1],
                             Std.Error = coef(summary(lmer.att5))[,2],
                             p_value = coef(summary(lmer.att5))[,5]), 3)
wemwbs.noint.conf <- round(confint(lmer.att5), 3)

write.csv(wemwbs.noint, file = "model_output/5_wemwbs_noint.csv")
write.csv(wemwbs.noint.conf, file = "model_output/5_wemwbs_noint_conf.csv")


# With interaction
wemwbs.int <- round_df(cbind(Estimate = coef(summary(lmer.att6))[,1],
                               Std.Error = coef(summary(lmer.att6))[,2],
                               p_value = coef(summary(lmer.att6))[,5]), 3)
wemwbs.int.conf <- round(confint(lmer.att5), 3)

write.csv(wemwbs.int, file = "model_output/5_wemwbs_int.csv")
write.csv(wemwbs.int.conf, file = "model_output/5_wemwbs_int_conf.csv")






# Use a linear mixed effects model since we have paired data
# Have 2 analyses - one checking whether or not it makes a difference if you have attended
# at all and one whether the amount of attendance makes a difference.

# Create a variable stating whether or not people have attended
long <- long %>% 
  mutate(attended = ifelse(sessions_attended == 0, 0, 1))

# Try something: Make time a continuous variable (weeks)
long <- long %>% 
  mutate(week_post_int = ifelse(timepoint == "base", 0,
                       ifelse(timepoint == "post", 8,
                              ifelse(timepoint == "fu1", 52,
                                     ifelse(timepoint == "fu2", 104,
                                            ifelse(timepoint == "fu3", 156, NA))))))

# Need to remove core score at baseline
long_nobase <- long %>% filter(timepoint != "base")

# Model 1 - with influence of attendance - if we want to see whether or not something influences
# the scores over time, we need to add an interaction between time and that variable.
# Controlling should also be done for the score at post-intervention rather than
# baseline, since those scores account for whether or not people have attended the course
# or not, i.e. they make sure we consider where people start after the course.
lmer.1 <- lmer(core_score ~ core_post + week_post_int*attended + disability +
                 gender + age + arm + (1|id), data = long_nobase)
anova(lmer.1)
summary(lmer.1)
# Time does not really change anything after post-intervention.
# No variable seems to affect the effect of time on the scores.


# Now let's check whether the number of sessions mattered over time.
# Create a dataset with only those people who have attended.
long_attended <- long_nobase %>% filter(sessions_attended != 0)

lmer.2 <- lmer(core_score ~ core_post + week_post_int*sessions_attended + disability +
                 gender + age + arm + (1|id), data = long_attended)

summary(lmer.2)



### Normal model time
lmer.3 <- lmer(core_score ~ core_post + week_post_int + sessions_attended + disability +
                 gender + age + arm + (1|id), data = long_attended)

summary(lmer.3)


### Normal model timepoint

lmer.3 <- lmer(core_score ~ core_post + timepoint + sessions_attended + disability +
                 gender + age + arm + (1|id), data = long_attended)

summary(lmer.3)




##### PLOTTING

## Trying to plot a model which contains the variables of interest: sessions_attended and time
long_reduced <- long_nobase %>% filter(!is.na(core_score))
long_reduced$attended <- as.logical(long_reduced$attended)
lmer.plot1 <- lmer(core_score ~ week_post_int*attended + (1|id), data = long_reduced)
summary(lmer.plot1)

long_reduced$predicted_core <- predict(lmer.plot1, type = "response")

ggplot(long_reduced, aes(x = week_post_int, y = predicted_core, colour = factor(attended))) +
  geom_jitter(aes(y = core_score, shape = factor(attended)), alpha=.5, width = 10) +
  geom_line(size = 1) +
  theme_bw()
# Not really working

long_attended <- long_attended %>% filter(!is.na(core_score))
lmer.plot2 <- lmer(core_score ~ week_post_int*sessions_attended + (1|id), data = long_attended)
summary(lmer.plot2)
long_attended$sessions_attended <- as.factor(long_attended$sessions_attended)

long_attended$predicted_core <- predict(lmer.plot2, type = "response")

ggplot(long_attended, aes(x = week_post_int, y = predicted_core, colour = sessions_attended)) +
  geom_jitter(aes(y = core_score, shape = sessions_attended), alpha=.5, width = 10) +
  geom_line(size = 1) +
  theme_bw()
# Not working at all



# Let's plot this in an easier way.
long_att <- long %>%
  mutate(attendance = ifelse(sessions_attended < 4, "0-3 sessions",
                             ifelse(sessions_attended >= 4, "4+ sessions", NA)))
long_att$attendance <- factor(long_att$attendance, levels = c("0-3 sessions", "4+ sessions"))

long_att <- long_att %>% filter(!is.na(core_score), timepoint != "base")
lm.plot <- lm(core_score ~ attendance * week_post_int, data = long_att)
summary(lm.plot)


# Predict probability and standard error
core.plot <- cbind(long_att, predict(lm.plot, type = "response", se = TRUE))
core.plot <- within(core.plot, {
  PredictedCore <- fit
  LL <- fit - (1.96 * se.fit)
  UL <- fit + (1.96 * se.fit)
})

long_att$predicted_core <- predict(lmer.plot, type = "response")

# Plot
ggplot(core.plot, aes(x = week_post_int, y = PredictedCore)) +
  geom_point(aes(y = core_score, color = attendance),
             position = position_jitter(w = 3), alpha = 0.2) +
  geom_line(aes(color = attendance), size = 1) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = attendance), alpha = 0.2) +
  theme_bw() +
  labs(x = "Time (weeks)",
       y = "CORE-OM score") +
  theme(
    axis.text = element_text(size = 12, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 14, face = "bold", margin=margin(10,0,0,0)),
    axis.title.y = element_text(size = 14, face = "bold", margin=margin(0,10,0,5))
  )
# cant do this bc its paired data


## Plot with individual lines connecting the same person
# CORE
long_plot <- long %>%
  mutate(attendance = ifelse(sessions_attended < 4, "0-3 sessions",
                             ifelse(sessions_attended >= 4, "4+ sessions", NA)))
long_plot$attendance <- factor(long_plot$attendance, levels = c("0-3 sessions", "4+ sessions"))

long_plot <- long_plot %>%
  mutate(time = ifelse(timepoint == "post", "Post course",
                       ifelse(timepoint == "fu1", "1-year FU",
                              ifelse(timepoint == "fu2", "2-year FU",
                                     ifelse(timepoint == "fu3", "3-year FU",
                                            ifelse(timepoint == "base", "Baseline", NA))))))
long_plot$time <- factor(long_plot$time, levels = c("Baseline", "Post course", "1-year FU",
                                                     "2-year FU", "3-year FU"))
long_plot %>% 
  ggplot(aes(time, core_score, color = attendance, group = id)) +
  geom_point(position=position_jitter(w=0.15), alpha = 0.5) +
  geom_line(alpha = 0.2) +
  scale_color_manual(values = c("#e44932", col4)) +
  theme_bw()

# WEMWBS
long_plot %>% 
  ggplot(aes(time, wb_score, color = attendance, group = id)) +
  geom_point(position = position_jitter(w = 0.15), alpha = 0.5) +
  geom_line(alpha = 0.2) +
  scale_color_manual(values = c("#e44932", col4)) +
  theme_bw()
# Whoever attended obviously started lower at post-intervention, but got slightly worse again over time.
# We need a boxplot, comparing everyone with only the people who attended a lot.




### Plot boxplot
# CORE-OM

long_plot %>%
  ggplot(aes(time, core_score, color = attendance)) +
  scale_color_manual(values = c(col2, col5)) +
  geom_boxplot(outlier.color = "gray60") +
  ylim(0,3.4) +
  theme_bw() +
  labs(x = "Timepoint", y = "CORE-OM score") +
  theme(
    text = element_text(family = "Leelawadee UI Semilight"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 12, margin=margin(5,0,8,0)),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold", margin=margin(7,0,0,0)),
    axis.title.y = element_text(size = 16, face = "bold", margin=margin(0,10,0,5))
  ) +
  labs(
    color = "Attendance"
  ) -> a

ggsave(a, filename = "plots_thesis/5_time_core.png", device = "png",
       width = 7, height= 5, units = "in")


# Numbers for this plot
long_plot %>% 
  filter(!is.na(wb_score)) %>% 
  group_by(time, attendance) %>% 
  summarize(n_obs = n())

long_plot %>% 
  filter(!is.na(core_score)) %>% 
  group_by(time) %>% 
  summarize(n_obs = n())


ggsave(COREposter, filename = "plots_poster/1_CORE_time_color.png", device = "png",
       width = 7, height= 5, units = "in")


### WEMWBS

long_plot %>%
  ggplot(aes(time, wb_score, color = attendance)) +
  scale_color_manual(values = c(col2, col5)) +
  geom_boxplot(outlier.color = "gray60") +
  theme_bw() +
  ylim(8,70) +
  labs(x = "Timepoint", y = "WEMWBS score") +
  theme(
    text = element_text(family = "Leelawadee UI Semilight"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 12, margin=margin(5,0,8,0)),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold", margin=margin(7,0,0,0)),
    axis.title.y = element_text(size = 16, face = "bold", margin=margin(0,7,0,5))
  ) +
  labs(
    color = "Attendance"
  ) -> b

ggsave(b, filename = "plots_thesis/5_time_wemwbs.png", device = "png",
       width = 7, height= 5, units = "in")



# Plot everyone
long_plot %>%
  ggplot(aes(time, core_score)) +
  geom_boxplot(outlier.color = "gray67") +
  annotate("text",
           x = c(1, 2, 3, 4, 5),
           y = c(3,3,3,3,3),
           label = c(table((long %>% filter(!is.na(core_score)))$timepoint)),
           fontface = 1, size=4, color = "red") +
  ylim(0,3.4) +
  theme_bw() +
  labs(x = "Timepoint", y = "CORE-OM score") +
  theme(
    axis.text.x = element_text(size = 10, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 12, face = "bold", margin=margin(0,0,8,0)),
    axis.title.y = element_text(size = 12, face = "bold", margin=margin(0,7,0,5))
  ) 





##### Won't need

# Plot CORE over time
long_plot %>%
  ggplot(aes(time, core_score)) +
  geom_boxplot(outlier.color = "gray67") +
  annotate("text",
           x = c(1, 2, 3, 4, 5),
           y = c(3,3,3,3,3),
           label = c(table((long %>% filter(!is.na(core_score)))$timepoint)),
           fontface = 1, size=4, color = "red") +
  ylim(0,3.4) +
  theme_bw() +
  labs(x = "Timepoint", y = "CORE-OM score") +
  theme(
    axis.text.x = element_text(size = 10, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 12, face = "bold", margin=margin(0,0,8,0)),
    axis.title.y = element_text(size = 12, face = "bold", margin=margin(0,7,0,5))
  ) -> COREall


## Plot for people who have attended 0 sessions
long_plot %>%
  filter(attendance == "0 sessions") %>% 
  ggplot(aes(time, core_score)) +
  geom_boxplot(outlier.color = "gray67") +
  annotate("text",
           x = c(1, 2, 3, 4, 5),
           y = c(3,3,3,3,3),
           label = c(table((long_plot %>% filter(!is.na(core_score) &
                                              attendance == "0 sessions"))$timepoint)),
           fontface = 1, size=4, color = "red") +
  ylim(0,3.4) +
  theme_bw() +
  labs(x = "Timepoint", y = "CORE-OM score") +
  theme(
    axis.text.x = element_text(size = 10, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 12, face = "bold", margin=margin(0,0,8,0)),
    axis.title.y = element_text(size = 12, face = "bold", margin=margin(0,7,0,5))
  ) -> CORE0

## Plot for people who have attended the minimum dose
long_plot %>%
  filter(attendance == "4+ sessions") %>% 
  ggplot(aes(time, core_score)) +
  geom_boxplot(outlier.color = "gray67") +
  annotate("text",
           x = c(1, 2, 3, 4, 5),
           y = c(3,3,3,3,3),
           label = c(table((long_plot %>% filter(!is.na(core_score) &
                                                   attendance == "4+ sessions"))$timepoint)),
           fontface = 1, size=4, color = "red") +
  ylim(0,3.4) +
  theme_bw() +
  labs(x = "Timepoint", y = "CORE-OM score") +
  theme(
    axis.text.x = element_text(size = 10, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 12, face = "bold", margin=margin(0,0,8,0)),
    axis.title.y = element_text(size = 12, face = "bold", margin=margin(0,7,0,5))
  ) -> CORE4


### ALL TOGETHER
long_plot %>%
  ggplot(aes(time, core_score, color = attendance)) +
  scale_color_manual(values = c("#e44932", col2, col5)) +
  geom_boxplot(outlier.color = "gray67") +
  ylim(0,3.4) +
  theme_bw() +
  labs(x = "Timepoint", y = "CORE-OM score") +
  theme(
    axis.text.x = element_text(size = 10, margin=margin(5,0,8,0)),
    axis.title.x = element_text(size = 12, face = "bold", margin=margin(0,0,8,0)),
    axis.title.y = element_text(size = 12, face = "bold", margin=margin(0,7,0,5)),
    legend.position = "bottom"
  ) +
  labs(
    color = "Attendance"
  ) -> COREcolor


### ALL TOGETHER WITHOUT 1-3 SESSIONS
long_plot %>%
  filter(attendance != "1-3 sessions") %>% 
  ggplot(aes(time, core_score, color = attendance)) +
  scale_color_manual(values = c(col2, col5)) +
  geom_boxplot(outlier.color = "gray60") +
  ylim(0,3.4) +
  theme_bw() +
  labs(x = "Timepoint", y = "CORE-OM score") +
  theme(
    legend.title = element_text(size = 14),
    axis.text.x = element_text(size = 12, margin=margin(5,0,8,0)),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin=margin(7,0,0,0)),
    axis.title.y = element_text(size = 14, face = "bold", margin=margin(0,7,0,5))
  ) +
  labs(
    color = "Attendance"
  ) -> COREposter

# Numbers for this plot
long_plot %>% 
  filter(!is.na(core_score) & attendance != "1-3 sessions") %>% 
  group_by(timepoint, attendance) %>% 
  summarize(n_obs = n())



grid.arrange(CORE4, CORE0, COREall, ncol = 2)

ggsave(COREall, filename = "plots_2019-06-06/1_CORE_time.png", device = "png",
       width = 5, units = "in")
ggsave(CORE0, filename = "plots_2019-06-06/1_CORE_0sess_time.png", device = "png",
       width = 5, height = 4, units = "in")
ggsave(CORE4, filename = "plots_2019-06-06/1_CORE_4+sess_time.png", device = "png",
       width = 5, height = 4, units = "in")
ggsave(COREcolor, filename = "plots_2019-06-06/1_CORE_time_color.png", device = "png",
       width = 5, units = "in")
ggsave(COREposter, filename = "plots_poster/1_CORE_time_color.png", device = "png",
       width = 7, height= 5, units = "in")

# Even people who have not practiced at all feel better at the 1-year FU timepoint.
### BOTTOM LINE: We cannot conclude that the long-term development that we see (no rise in CORE score)
### is really due to the intervention, since everyone tends to feel better after 1,2 and 3 years.






