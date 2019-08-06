library(MASS)
library(tidyverse)
library(car)
library(dunn.test)
library(lme4)
library(lmerTest)
library(gridExtra)

# Read in datasets. This time, we have a long (gathered) dataset, with another column per timepoint.
wide <- read_csv("data_input(4)/mindful_clean.csv")
long <- read_csv("data_input(4)/mindful_long.csv")


### Cleaning - ensure that the timepoints are displayed in the correct order (as they are treated as
### categorical variables).
long$timepoint <- factor(long$timepoint, levels = c("base", "post", "fu1", "fu2", "fu3"))


### Create color palette
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



##### Stats

### Create linear mixed effects models with participant id as random effect. Create models with and
### without an interaction between timepoint and sessions, in order to account for the general score
### development over time and the effect of the course over time.

# CORE-OM without interaction
lmer.c <- lmer(core_score ~ timepoint + sessions_attended + age + gender + intake + disability +
                    core_base + (1|id), data = long)
summary(lmer.c)
# CORE-OM with interaction
lmer.c.int <- lmer(core_score ~ timepoint * sessions_attended + age + gender + intake + disability +
                    core_base + (1|id), data = long)
summary(lmer.c.int)

# WEMWBS without interaction
lmer.w <- lmer(wb_score ~ timepoint + sessions_attended + age + gender + intake + disability +
                    wb_base + (1|id), data = long)
summary(lmer.w)
# WEMWBS with interaction
lmer.w.int <- lmer(wb_score ~ timepoint * sessions_attended + age + gender + intake + disability +
                    wb_base + (1|id), data = long)
summary(lmer.w.int)



# Export. Confidence interval has to be computed extra since it has two more rows for linear mixed models.....

# CORE
# No interaction between timepoint and sessions
core.noint <- round_df(cbind(Estimate = coef(summary(lmer.c))[,1],
                             Std.Error = coef(summary(lmer.c))[,2],
                             p_value = coef(summary(lmer.c))[,5]), 3)
core.noint.conf <- round(confint(lmer.c), 3)

# With interaction
core.int <- round_df(cbind(Estimate = coef(summary(lmer.c.int))[,1],
                             Std.Error = coef(summary(lmer.c.int))[,2],
                             p_value = coef(summary(lmer.c.int))[,5]), 3)
core.int.conf <- round(confint(lmer.c.int), 3)

# WEMWBS
# No interaction
wemwbs.noint <- round_df(cbind(Estimate = coef(summary(lmer.w))[,1],
                             Std.Error = coef(summary(lmer.w))[,2],
                             p_value = coef(summary(lmer.w))[,5]), 3)
wemwbs.noint.conf <- round(confint(lmer.w), 3)

# With interaction
wemwbs.int <- round_df(cbind(Estimate = coef(summary(lmer.w.int))[,1],
                               Std.Error = coef(summary(lmer.w.int))[,2],
                               p_value = coef(summary(lmer.w.int))[,5]), 3)
wemwbs.int.conf <- round(confint(lmer.w.int), 3)




### Plotting - only plot boxplots comparing people who attended 0-3 sessions and people who
### attended 4+ sessions.


## CORE-OM

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

# Save
ggsave(a, filename = "plots_thesis/5_time_core.png", device = "png",
       width = 7, height= 5, units = "in")

# Number of participants for this plot (to be added onto the plot after exporting)
long_plot %>% 
  filter(!is.na(core_score)) %>% 
  group_by(time) %>% 
  summarize(n_obs = n())


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

# Save
ggsave(b, filename = "plots_thesis/5_time_wemwbs.png", device = "png",
       width = 7, height= 5, units = "in")

# Number of participants for this plot (to be added onto the plot after exporting)
long_plot %>% 
  filter(!is.na(wb_score)) %>% 
  group_by(time) %>% 
  summarize(n_obs = n())
