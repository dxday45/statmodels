library(survival)
library(tidyverse)
library(psych)
library(gmodels)
library(survminer)
library(ggsci)
library(stargazer)
library(broom)
library(kableExtra)
library(gtsummary)
library(KMsurv)
library(ggfortify)
library(haven)

# Altering the dataset
pbc_1_ <- read_csv("pbc(1).csv")
pbc.data <- pbc_1_
pbc.data$event[pbc.data$status == "Dead"] <- 1
pbc.data$event[pbc.data$status == "Censored"] <- 0
pbc.data <- pbc.data %>%
  mutate(months = years * 12)


# Summary data for continuous variables
describe(pbc.data$years)
describe(pbc.data$trigli)

#Breakdown table broken down by treatment group
CrossTable(pbc.data$rx, pbc.data$event, digits = 3, dnn = c('Treatment Group', 'Event'))


# Kaplan-Meier Estimation for every subject
pbc.surv_yrs <- Surv(pbc.data$years, pbc.data$event)

km.all <- survfit(pbc.surv_yrs ~ 1, data = pbc.data)
summary(km.all)
summary(km.all)$table

plot(km.all, main = 'Kaplan-Meier Estimator', xlab = 'Years', ylab = 'Survival %', yscale = 100, 
     col = 'indianred4', las = 1, bty = "]")
grid()

# Same plot using ggplot themes
ggsurvplot(fit = survfit(pbc.surv_yrs ~ 1, data = pbc.data),
           title = 'Kaplan-Meier Estimator', xlab = 'years', ylab = 'Survival %',
           surv.scale = 'percent', theme = theme_bw(), palette = 'uchicago',
           legend = 'none', legend.title = "All Patients", risk.table = TRUE)

# Kaplan-Meier broken down by treatment group
km.rx <- survfit(formula = pbc.surv_yrs ~ rx, data = pbc.data)
summary(km.rx)
summary(km.rx)$table
plot(km.rx, main = 'Kaplan-Meier Estimator by Treatment Group', xlab = 'Years', ylab = 'Survival %',
     yscale = 100, col = c("springgreen4", "deeppink4"), las = 1, bty = "]")
legend("topright", title = "Treatment Group", c("Placebo", "DPCA"),
       fill = c('springgreen4', 'deeppink3'), cex = .75)
grid()

autoplot(survfit(pbc.surv_yrs ~ as.factor(pbc.data$rx)), 
         main = "Comparing Treatment and Placebo Groups", xlab = 'Survival',
         ylab = 'Kaplan-Meier Estimator', las = 1)

# Same Plot using ggplot themes
ggsurvplot(fit = survfit(pbc.surv_yrs ~ pbc.data$rx, data = pbc.data),
           title = 'Kaplan-Meier Estimator', xlab = 'years', ylab = 'Survival %',
           surv.scale = 'percent', theme = theme_bw(), palette = 'simpsons',
           conf.int = TRUE, pval = TRUE, risk.table = TRUE, legend.title = 'Treatment Groups',
           legend.labs = c("DPCA", "Placebo"))



# Log Rank Test
survdiff(formula = pbc.surv_yrs ~ rx, data = pbc.data)

# Cox Model
cox.surv_yrs <- coxph(pbc.surv_yrs ~ event, data = pbc.data)
cox.surv_yrs
summary(cox.surv_yrs)

cox.surv_yrs %>%
  tbl_regression()
ggcoxdiagnostics(cox.surv_yrs, type = "deviance", ox.scale = 'time')

# By treatment group
cox.rx <- coxph(pbc.surv_yrs ~ rx, data = pbc.data)
summary(cox.rx)

ggcoxdiagnostics(cox.rx, type = "deviance", ox.scale = 'time')

# By triglicerides
cox.trigli <- coxph(pbc.surv_yrs ~ trigli, data = pbc.data)
cox.trigli
summary(cox.trigli)
ggcoxdiagnostics(cox.trigli, type = "deviance", ox.scale = 'time')

#By Severity of liver damage
cox.histol <- coxph(pbc.surv_yrs ~ histol, data = pbc.data)
cox.histol
summary(cox.histol)
ggcoxdiagnostics(cox.histol, type = "deviance", ox.scale = 'time')

# Combining all variables
cox.combined <- coxph(pbc.surv_yrs ~ rx + trigli + histol, data = pbc.data)
cox.combined
summary(cox.combined)
ggcoxdiagnostics(cox.combined, type = "deviance", ox.scale = 'time')
ggforest(cox.combined)
ggadjustedcurves(cox.combined, data = pbc.data, legend = 'none')
stargazer(cox.rx, cox.trigli, cox.combined, type = 'text')
anova(cox.combined)

# years converted to months
# Summary data for continuous variables
describe(pbc.data$months)


# Kaplan-Meier Estimation for every subject
pbc.surv_months <- Surv(pbc.data$months, pbc.data$event)

km.all_m <- survfit(pbc.surv_months ~ 1, data = pbc.data)
summary(km.all_m)


# Kaplan-Meier broken down by treatment group
km.rx_m <- survfit(formula = pbc.surv_months ~ rx, data = pbc.data)
summary(km.rx_m)

