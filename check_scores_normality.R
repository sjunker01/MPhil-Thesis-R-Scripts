library(tidyverse)
library(gridExtra)
library(moments)
library(GGally)
library(Amelia)
library(corrplot)

### Read in file
wide <- read_csv("data_input(4)/mindful_clean.csv")


### Check data set for missing data
missmap(wide, col=c("red", "green"), legend=FALSE)


### Create diagnostic plots
# Create a data frame which only contains the variables I am interested in (CORE and WEMWBS scores).
wide_scores <- wide[,15:24]
# Make list of variable names to loop over.
variable_list <- names(wide_scores)


## Create histograms
# Make histograms
hist_list = list()
for(i in 1:10) {
  histograms <- ggplot(wide_scores, aes_string(variable_list[i])) +
    geom_histogram(bins = 18) +
    theme_bw()
  hist_list[[i]] <- histograms
}
# Plot all histograms
grid.arrange(hist_list[[1]], hist_list[[2]], hist_list[[3]], hist_list[[4]], hist_list[[5]],
             hist_list[[6]], hist_list[[7]], hist_list[[8]], hist_list[[9]], hist_list[[10]],
             ncol = 5, nrow = 2)


## Check for normality
# Create normal Q-Q plots
qq_list <- list()
for(i in 1:10) {
  qq <- ggplot(wide_scores, aes_string(sample = variable_list[[i]])) +
    stat_qq(na.rm = TRUE, shape = 1) +
    stat_qq_line(na.rm = TRUE) +
    theme_bw()
  qq_list[[i]] <- qq
}

# Plot all normal Q-Q plots
grid.arrange(qq_list[[1]], qq_list[[2]], qq_list[[3]], qq_list[[4]], qq_list[[5]],
             qq_list[[6]], qq_list[[7]],qq_list[[8]], qq_list[[9]], qq_list[[10]],
             ncol = 5, nrow = 2)

# Do shapiro test for all scores
statistic <- vector()
p.value <- vector()
for(i in 1:10) {
  shap <- shapiro.test(wide_scores[[i]])
  statistic[i] <- shap$statistic
  p.value[i] <- shap$p.value
}

shapiro_results <- data.frame(score = variable_list, statistic, p.value)
shapiro_results
# The shapiro tests indicate that the CORE scores are definitely not normally distributed.
# Looking at the normal Q-Q plots, there seems to be a floor-effect.
# Most WEMWBS scores are not really normally distributed either. Slight ceiling effect.


# Compare skewness and kurtosis
skew <- vector()
kurt <- vector()
for(i in 1:10) {
  sk <- skewness(wide_scores[[i]], na.rm = TRUE)
  ku <- kurtosis(wide_scores[[i]], na.rm = TRUE)
  skew[i] <- sk
  kurt[i] <- ku
}

normality <- data.frame(score = variable_list, skewness = skew, kurtosis = kurt)
normality


### Check correlation of CORE and WEMWBS overall
cor(wide_scores, use = "pairwise.complete.obs", method = "pearson")
pairs(wide_scores, lower.panel = NULL)
# Usually the CORE and WEMWBS scores for the same timepoint seem to be strongly negatively
# correlated. You can also see some positive correlation between consecutive timepoints.

## By the way, you can also do it like this (takes longer but is prettier):
ggpairs(wide_scores[,1:10])

## Another way to investigate correlations:
correlations <- cor(wide_scores, use = "pairwise.complete.obs", method = "pearson")
corrplot(correlations, method="circle")


