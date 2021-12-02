library(tidyverse)
names(EvansCounty)
summary(EvansCounty)

# 95% CI for Age
mean_Age <- mean(EvansCounty$AGE) #point estimate
sd_Age <- sd(EvansCounty$AGE)
n_Age <- sum(table(EvansCounty$AGE))

# Critical Value
#crit_val <- qt(0.975, 609, lower.tail = T)
crit_val <- qt((1-0.95)/2, n_Age - 1, lower.tail = F)

ME_Age <- crit_val * sd_Age/sqrt(n_Age)

lower_Age <- mean_Age - ME_Age
upper_Age <- mean_Age + ME_Age

# Finding Confidence Interval for Mean by Writing a Function

CI_mean <- function(var1, ci_lev) {
  mean_var1 <- mean(var1, na.rm = T)
  sd_var1 <- sd(var1, na.rm = T)
  n_var1 <- sum(table(var1))
  crit_value <- qt((1-ci_lev)/2, n_var1-1, lower.tail = F)
  ME_var1 <- crit_value * sd_var1/sqrt(n_var1)
  l_var1 <- mean_var1 - ME_var1
  u_var1 <- mean_var1 + ME_var1
  return(data.frame(mean = mean_var1, lower_limit = l_var1, upper_limit = u_var1))
}
CI_mean(EvansCounty$AGE, 0.95)

# Confidence Interval for Proportions

CI_prop <- function(var1, ci_lev) {
  n_var1 <- sum(table(var1))
  prop_var1 <- sum(var1)/n_var1
  crit_val <- qnorm((1-ci_lev)/2, lower.tail = F)
  ME_var1 <- crit_val * sqrt(prop_var1 * (1-prop_var1) / n_var1)
  l_var1 <- prop_var1 - ME_var1
  u_var1 <- prop_var1 + ME_var1
  return(data.frame(samp.prop = prop_var1, lower_limit = l_var1, upper_limit = u_var1))
}
CI_prop(EvansCounty$SMK, 0.95)
CI_prop(EvansCounty$CHD, 0.95)
round(100 * CI_prop(EvansCounty$SMK, 0.95),1)
