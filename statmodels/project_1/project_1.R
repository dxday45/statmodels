library(tidyverse)
library(NHANES)
library(lindia)
library(extrafont)
library(gtsummary)
library(car)
library(tableone)
install.packages('tinytex')
tinytex::install_tinytex()
names(NHANES)
str(NHANES)
# Remove Missing BMIs
nhanes <- NHANES %>% drop_na(BMI)
nhanes <- nhanes %>% filter(Age >= 18 & Age <= 80)
nhanes <- nhanes %>% select(Age, Gender, Race1, BMI, Smoke100, SleepHrsNight, MaritalStatus)

nhanes$BMIcat <- cut(nhanes$BMI, c(0, 18.5, 24.9, 29.9, 100))
levels(nhanes$BMIcat) <- c("Underweight","Normal","Overweight","Obese")

#Creating table with BMI Category Breakdown
nhanes %>%
  group_by(BMIcat) %>%
  summarize(mean=mean(Age),st.dev=sd(Age))
nhanes %>%
  summarize(mean=mean(Age),st.dev=sd(Age))

with(nhanes, table(BMIcat))
aov.ex <- aov(Age ~ BMIcat,data = nhanes)
summary(aov.ex)

with(nhanes, table(Gender, BMIcat))
with(nhanes, prop.table(table(Gender, BMIcat), 2))
chisq.test(with(nhanes, table(Gender, BMIcat)))

with(nhanes, table(Race1, BMIcat))
with(nhanes, prop.table(table(Race1, BMIcat), 2))
aov.ex <- aov(as.integer(Race1) ~ BMIcat,data = nhanes)
summary(aov.ex)

with(nhanes, table(Smoke100, BMIcat))
with(nhanes, prop.table(table(Smoke100, BMIcat), 2))
aov.ex <- aov(as.integer(Smoke100) ~ BMIcat,data = nhanes)
summary(aov.ex)

with(nhanes, table(MaritalStatus, BMIcat))
with(nhanes, prop.table(table(MaritalStatus, BMIcat), 2))
aov.ex <- aov(as.integer(MaritalStatus) ~ BMIcat,data = nhanes)
summary(aov.ex)

with(nhanes, table(SleepHrsNight, BMIcat))
with(nhanes, prop.table(table(SleepHrsNight, BMIcat), 2))
aov.ex <- aov(SleepHrsNight ~ BMIcat,data = nhanes)
summary(aov.ex)
catvars <- c("Gender", "Race1", "Smoke100", "MaritalStatus")
rm(vars)
vars <- c("Age", "SleepHrsNight", "Gender", "Race1", "Smoke100", "MaritalStatus")
bmi_table <- CreateTableOne(vars = vars, strata = "BMIcat", data = nhanes, factorVars = catvars)
bmi_table 

################################################################################
################################# Marital Status ###############################
################################################################################

# Regression model with marital status as the main explanatory variable
reg.marital <- lm(log(BMI) ~ as.factor(MaritalStatus), data = nhanes)
summary(reg.marital)
confint(reg.marital)

reg.full <- lm(log(BMI) ~ as.factor(MaritalStatus) + SleepHrsNight, nhanes)
summary(reg.full)
confint(reg.full)
plot(reg.full)

# Regression Tables for Marital Status and BMI
tbl_regression(reg.marital, label = ~ "Marital Status") %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

tbl_regression(reg.full, label = ~ "Marital Status") %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Boxplot of Marital Status and the Log of BMI
nhanes %>%
  ggplot(aes(MaritalStatus, log(BMI), fill = MaritalStatus)) +
  geom_boxplot() +
  labs(title = "Comparing BMI Model", subtitle = "Marital Status VS Log of BMI",
       x = "Marital Status", y = "log of BMI") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_blank(),
        plot.background = element_rect(fill = 'beige'),
        panel.grid.major = element_blank(),
        legend.position = 'None')

# Scatter Plot of the Residuals and the Fitted Values
reg.full %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  theme_bw() +
  labs(title = "Residuals VS Fitted Values", 
       subtitle = "Marital Status/Hour of Sleep Per Night VS Log of BMI",
       x = "Fitted Values", y = "Residuals") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_rect(fill = 'ivory'),
        panel.grid = element_line(color = 'grey70'),
        plot.background = element_rect(fill = 'steelblue')) +
  geom_hline(yintercept = 0, color = 'indianred')

# Normal Quartile-Quartile Plot
reg.full %>%
  gg_qqplot() +
  theme_bw() +
  labs(title = "Normal Quantile-Quantile Plot", 
       subtitle = "Marital Status/Hour of Sleep Per Night VS Log of BMI",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_rect(fill = 'ivory'),
        panel.grid = element_line(color = 'grey70'),
        plot.background = element_rect(fill = 'steelblue'))

# Scale-Location Plot
reg.full %>%
  gg_scalelocation() +
  theme_bw() +
  labs(title = "Scale-Location Plot", 
       subtitle = "Marital Status/Hour of Sleep Per Night VS Log of BMI",
       x = "Fitted Values", y = "Square Root of the Standardized Residuals") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_rect(fill = 'ivory'),
        panel.grid = element_line(color = 'grey70'),
        plot.background = element_rect(fill = 'steelblue'))

# Residuals vs Leverage Plot
reg.full %>%
  gg_resleverage() +
  theme_bw() +
  labs(title = "Residuals VS Leverage Plot", 
       subtitle = "Marital Status/Hour of Sleep Per Night VS Log of BMI",
       x = "Residuals", y = "Standardized Residuals") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_rect(fill = 'ivory'),
        panel.grid = element_line(color = 'grey70'),
        plot.background = element_rect(fill = 'steelblue'))


################################################################################
####################### Hours of Sleep Per Night ###############################
################################################################################

#Regression with Hours of Sleep Per Night as the main explanatory variable
reg.sleep <- lm(BMI ~ SleepHrsNight, data = nhanes)
summary(reg.sleep)
confint(reg.sleep)

reg.full.sleep <- lm(log(BMI) ~ SleepHrsNight + as.factor(MaritalStatus) + Age, data = nhanes)
summary(reg.full.sleep)
confint(reg.full.sleep)
plot(reg.full.sleep)

# Regression Tables for Marital Status and BMI
tbl_regression(reg.sleep, label = ~"Nightly Sleep") %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

tbl_regression(reg.full.sleep, label = ~ "Marital Status") %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Scatter Plot of Hours Slept Per Night and the Log of BMI
nhanes %>%
  ggplot(aes(SleepHrsNight, log(BMI))) +
  geom_point() +
  labs(title = "Regression Model", subtitle = "Hours of Sleep Per Night/Marital Status VS Log of BMI",
       x = "Hours of Sleep Per Night", y = "Log of BMI") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_rect(fill = 'ivory'),
        panel.grid = element_line(color = 'grey70'),
        plot.background = element_rect(fill = 'steelblue'))

# Scatter Plot of the Residuals and the Fitted Values
reg.full %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  theme_bw() +
  labs(title = "Residuals VS Fitted Values", 
       subtitle = "Hours of Sleep Per Night/Marital Status VS Log of BMI",
       x = "Fitted Values", y = "Residuals") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_rect(fill = 'ivory'),
        panel.grid = element_line(color = 'grey70'),
        plot.background = element_rect(fill = 'steelblue')) +
  geom_hline(yintercept = 0, color = 'indianred', size = 1)

# Norml Quantile-Quantile Plot
reg.full.sleep %>%
  gg_qqplot() +
  theme_bw() +
  labs(title = "Normal Quantile-Quantile Plot", 
       subtitle = "Hours of Sleep Per Night/Marital Status VS Log of BMI",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_rect(fill = 'ivory'),
        panel.grid = element_line(color = 'grey70'),
        plot.background = element_rect(fill = 'steelblue'))

# Scale-Location Plot
reg.full.sleep %>%
  gg_scalelocation() +
  theme_bw() +
  labs(title = "Scale-Location Plot", 
       subtitle = "Hours of Sleep Per Night/Marital Status VS Log of BMI",
       x = "Fitted Values", y = "Square Root of the Standardized Residuals") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_rect(fill = 'ivory'),
        panel.grid = element_line(color = 'grey70'),
        plot.background = element_rect(fill = 'steelblue'))

# Residuals vs Leverage Plot
reg.full.sleep %>%
  gg_resleverage() +
  theme_bw() +
  labs(title = "Residuals VS Leverage Plot", 
       subtitle = "Hours of Sleep Per Night/Marital Status VS Log of BMI",
       x = "Residuals", y = "Standardized Residuals") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_rect(fill = 'ivory'),
        panel.grid = element_line(color = 'grey70'),
        plot.background = element_rect(fill = 'steelblue'))

################################################################################
################################## Smoking #####################################
################################################################################

#Regression with Smoking Status as the main explanatory variable
reg.smoke <- lm(log(BMI) ~ Smoke100, data = nhanes)
summary(reg.smoke)
confint(reg.smoke)

reg.full.smoke <- lm(log(BMI) ~ Smoke100 + MaritalStatus + Age + Gender, data = nhanes)
summary(reg.full.smoke)
confint(reg.full.smoke)
plot(reg.full.smoke)

# Regression Tables for Marital Status and BMI
tbl_regression(reg.smoke, label = ~"Smoker") %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

 tbl_regression(reg.full.smoke) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# Box Plot of Smoking Status and the Log of BMI
nhanes %>%
  ggplot(aes(Smoke100, log(BMI), fill = Smoke100)) +
  geom_boxplot() +
  labs(title = "Smoking Status", subtitle = "Smoking Status VS Log of BMI",
       x = "Smoker", y = "Log of BMI") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_blank(),
        plot.background = element_rect(fill = 'beige'),
        panel.grid.major = element_blank(),
        legend.position = 'None')

# Scatter Plot of the Residuals and the Fitted Values
reg.full.smoke %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  theme_bw() +
  labs(title = "Residuals VS Fitted Values", 
       subtitle = "Smoking Status , et al. VS Log of BMI",
       x = "Fitted Values", y = "Residuals") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_rect(fill = 'ivory'),
        panel.grid = element_line(color = 'grey70'),
        plot.background = element_rect(fill = 'steelblue')) +
  geom_hline(yintercept = 0, color = 'indianred', size = 1)

# Normal Quantile-Quantile Plot
reg.full.smoke %>%
  gg_qqplot() +
  theme_bw() +
  labs(title = "Normal Quantile-Quantile Plot", 
       subtitle = "Smoking Status , et al. VS Log of BMI",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 13, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_rect(fill = 'ivory'),
        panel.grid = element_line(color = 'grey70'),
        plot.background = element_rect(fill = 'steelblue'))

# Scale-Location Plot
reg.full.smoke %>%
  gg_scalelocation() +
  theme_bw() +
  labs(title = "Scale-Location Plot", 
       subtitle = "Smoking Status , et al. VS Log of BMI",
       x = "Fitted Values", y = "Square Root of the Standardized Residuals") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_rect(fill = 'ivory'),
        panel.grid = element_line(color = 'grey70'),
        plot.background = element_rect(fill = 'steelblue'))

# Residuals vs Leverage Plot
reg.full.smoke %>%
  gg_resleverage() +
  labs(title = "Residuals VS Leverage Plot", 
       subtitle = "Smoking Status , et al. VS Log of BMI",
       x = "Leverage", y = "Standardized Residuals") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 13, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_rect(fill = 'ivory'),
        panel.grid = element_line(color = 'grey70'),
        plot.background = element_rect(fill = 'steelblue'))

