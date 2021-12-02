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
