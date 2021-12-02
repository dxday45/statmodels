library(survival)
library(tidyverse)
library(psych)
library(gmodels)
install.packages('gmodels')
head(pbc)


pbc.data <- pbc_1_
pbc.data$event[pbc.data$status == "Dead"] <- 1
pbc.data$event[pbc.data$status == "Censored"] <- 0
pbc.data <- pbc.data %>%
  mutate(months = years * 12)
describe(pbc.data$months)
describe(pbc.data$years)
CrossTable(pbc.data$rx, pbc.data$event, digits = 3, dnn = c('Treatment Group', 'Event'))

pbc.surv_yrs <- with(pbc.data, Surv(years, event))

km.all <- survfit(pbc.surv_yrs ~ 1, data = pbc.data)
summary(km.all)
plot(km.all, main = 'Kaplan-Meier Estimator', xlab = 'Years', ylab = 'Survival %', yscale = 100, 
     col = 'indianred4', las = 1, bty = "]")
grid()

km.rx <- survfit(formula = pbc.surv_yrs ~ rx, data = pbc.data)
summary(km.rx)

plot(km.rx, main = 'Kaplan-Meier Estimator by Treatment Group', xlab = 'Years', ylab = 'Survival %',
     yscale = 100, col = c("springgreen4", "deeppink4"), las = 1, bty = "]")
legend("topright", title = "Treatment Group", c("Placebo", "DPCA"), fill = c('springgreen4', 'deeppink3'))
grid()

survdiff(formula = pbc.surv_yrs ~ rx, data = pbc.data)
survfit(oa.surv ~ pbc.data$rx)
summary(survfit(oa.surv ~ pbc.data$rx))
plot(survfit(oa.surv ~ pbc.data$rx), main = "Kaplan-Meier Estimator by Treatment Group", xlab = 'Survival',
     ylab = 'Kaplan-Meier Estimator', col = 'blue', las = 1, bty = "]")

autoplot(survfit(oa.surv ~ pbc.data$rx), main = "Comparing Treatment and Placebo Groups", xlab = 'Survival',
         ylab = 'Kaplan-Meier Estimator', las = 1)

survdiff(oa.surv ~ pbc.data$rx)
plot(survdiff(oa.surv ~ pbc.data$rx))


cox.surv <- Surv(pbc.data$years, pbc.data$event)
cox.pbc <- coxph(oa.surv ~ rx + trigli, model = TRUE, data = pbc.data)
cox.pbc
summary(cox.pbc)

plot(residuals(cox.pbc, type = c("schoenfeld")))
