library(tidyverse)
library(tableone)
library(vcd)
library(questionr)
copd <- COPD_LC_CBMN_data


copd <- copd %>% filter(COPD == 'COPD' | COPD == 'Control')
copd <- copd %>% select('COPD', 'Age', 'Sex', 'smkstatus', 'asbestos_new', 'EMPHYS',
                        'BRONCH', 'AGECAT', 'RACE', 'Apoptosis', 'Necrosis')

#Creating table with COPD Breakdown

# Summarizing the quantitative variables apoptosis and necrosis
# Using the ranges from a study from the NIH and then spreading across the dataset range
copd1$ApoptosisCat <- cut(copd1$Apoptosis, c(0, 26, 68, 100))
# Poorly Differentiated, Moderately Differentiated, Well Differentiated
levels(copd$ApoptosisCat) <- c("Poorly Diff", "Moderately Diff", "Well Diff")

copd %>%
  group_by(COPD, Sex, ApoptosisCat) %>%
  summarize(mean=mean(Apoptosis),st.dev=sd(Apoptosis))

with(copd, table(COPD, Sex, ApoptosisCat))
with(copd, prop.table(table(COPD, Sex, ApoptosisCat)))
chisq.test(with(copd, table(ApoptosisCat, COPD)))
aov.ex <- aov(Apoptosis ~ COPD,data = copd)
summary(aov.ex)



copd %>%
  group_by(COPD, Sex) %>%
  summarize(mean=mean(Necrosis),st.dev=sd(Necrosis))
with(copd, table(COPD, Necrosis))
aov.ex <- aov(Necrosis ~ COPD,data = copd)
summary(aov.ex)

# Summarizing Categorical Variables Sex: 1 = Female, 2 = Male
# smkstatus: 0 = never smoked, 1 = former smoker, 2 = current smoker

with(copd, table(Sex, smkstatus, COPD))
with(copd, prop.table(table(Sex, smkstatus, COPD), 2))
chisq.test(with(copd, table(smkstatus, COPD)))

# Sex: 1 = Female, 2 = Male; RACE: 1 = white, 2 = hispanic, 3 = black, 4 = other
with(copd, table(Sex, RACE, COPD))
with(copd, prop.table(table(Sex, RACE, COPD), 2))
aov.ex <- aov(as.integer(RACE) ~ COPD,data = copd)
summary(aov.ex)
chisq.test(with(copd, table(RACE, COPD)))

# Sex: 1 = Female, 2 = Male; AGECAT: 1 = 40 and under, 2 = 41-50, 3 = 51-60, 4 = 61-70, 5 = 71 +
with(copd, table(Sex, AGECAT, COPD))
with(copd, prop.table(table(Sex, AGECAT, COPD), 2))
aov.ex <- aov(as.integer(AGECAT) ~ COPD,data = copd)
summary(aov.ex)
chisq.test(with(copd, table(AGECAT, COPD)))

# Sex: 1 = Female, 2 = Male; asbestos_new: 0 = no, 1 = yes
with(copd, table(Sex, asbestos_new, COPD))
with(copd, prop.table(table(Sex, asbestos_new, COPD), 2))
aov.ex <- aov(as.integer(asbestos_new) ~ COPD,data = copd)
summary(aov.ex)
chisq.test(with(copd, table(asbestos_new, COPD)))

# Sex: 1 = Female, 2 = Male; EMPHYS: 1 = no, 2 = yes
with(copd, table(Sex, EMPHYS, COPD))
with(copd, prop.table(table(Sex, EMPHYS, COPD), 2))
aov.ex <- aov(as.integer(EMPHYS) ~ COPD,data = copd)
summary(aov.ex)
chisq.test(with(copd, table(EMPHYS, COPD)))

# Sex: 1 = Female, 2 = Male; BRONCH: 1 = no, 2 = yes
with(copd, table(Sex, BRONCH, COPD))
with(copd, prop.table(table(Sex, BRONCH, COPD), 2))
aov.ex <- aov(as.integer(BRONCH) ~ COPD,data = copd)
summary(aov.ex)
chisq.test(with(copd, table(BRONCH, COPD)))

# Breakdown of each variable stratified by Apoptosis Categories (Poor, Moderate, Well)
catvars <- c("Sex", "RACE", "smkstatus", "asbestos_new", "EMPHYS", "BRONCH")
rm(vars)
vars <- c("Age", "Apoptosis", "Necrosis", "Sex", "RACE", "smkstatus", "EMPHYS", "BRONCH")
apop_table <- CreateTableOne(vars = vars, strata = "ApoptosisCat", data = copd, factorVars = catvars)
apop_table 

copd1 <- copd
copd1$copd_num[copd1$COPD == "COPD"] <- 1
copd1$copd_num[copd1$COPD == "Control"] <- 0


#check for linearity of quantitative variables
glm.1 <- glm(as.factor(COPD) ~ Apoptosis,data = copd1,family=binomial)
exp(coefficients(glm.1))
apop.emp.odds <- copd1 %>% 
  group_by(Apoptosis) %>%
  summarize(emp.prob = (sum(copd_num, na.rm = TRUE)+.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))
apop.emp.odds
summary(odds.ratio(glm.1))
ggplot(data = data.frame(Apoptosis=copd1$Apoptosis,pred=predict(glm.1)),aes(Apoptosis,pred))+
  geom_line(lty=2)+
  geom_point(data=apop.emp.odds,aes(Apoptosis,emp.odds),col="red")+
  geom_smooth(data=apop.emp.odds,aes(Apoptosis,emp.odds),col="blue",se=F)+
  theme_bw()+
  labs(x="Apoptosis Count",
       y="Logit COPD Risk")
copda <- copd1
copda$.cat <- cut(WCGS$bmi,c(0,18.5,24.99,29.99,1000),right=F)
table(ApoptosisCat)
a.emp.odds <- copd1 %>% 
  group_by(ApoptosisCat) %>% 
  summarize(mean.apoptosis = mean(Apoptosis,na.rm=T),emp.prob = (sum(copd_num, na.rm = TRUE)+.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))



glm.a <- glm(as.factor(COPD) ~ Apoptosis + smkstatus + RACE + AGECAT, data = copd1,family=binomial)
exp(coefficients(glm.a))
ggplot(data=data.frame(Apoptosis = glm.a$model$Apoptosis,pred=predict(glm.a)),aes(Apoptosis,pred))+
  geom_point(alpha=0.1)+
  geom_point(data=a.emp.odds,aes(mean.apoptosis,emp.odds),col="red",size=3)+
  geom_smooth(data=a.emp.odds,aes(mean.apoptosis,emp.odds),col="blue",se=F,size=1.5)+
  theme_bw()+
  labs(x="Apoptosis",
       y="Logit COPD Risk")


copd1$Apoptosis_10<-scale(copd1$Apoptosis, center=TRUE, scale=FALSE)/10
copd1$smkstatus_50<-scale(copd1$smkstatus, center=TRUE, scale=FALSE)/50
copd1$RACE_50 <-scale(copd1$RACE, center=TRUE, scale=FALSE)/50
copd1$AGECAT_10<-scale(copd1$AGECAT, center=TRUE, scale=FALSE)/10

WCGS$dibpat_rel <- relevel(WCGS$dibpat,ref="Type B")

WCGS$bmisbp <- WCGS$bmi_10*WCGS$sbp_50
WCGS$bmichol <- WCGS$bmi_10*WCGS$chol_50

mod.5.18 <- glm(chd69~age_10+chol_50+sbp_50+bmi_10+smoke+dibpat_rel+bmichol+bmisbp,
                data=WCGS[WCGS$chol<645,],
                family=binomial)

plot(mod.5.18,which=4,id.n=3)

#Goodness of Fit test
library(ResourceSelection)
hoslem.test(mod.5.18$model$chd69, fitted(mod.5.18), g=10)

#prediction accuracy
library(caret)
prob <- predict(mod.5.18,type="response")
pred.val <- ifelse(prob > .5, 1,0)
confusionMatrix(factor(pred.val),factor(mod.5.18$model$chd69))

library(broom)
d <- augment(mod.5.18)


#check for linearity of quantitative variables
mod.5.2 <- glm(copd_num ~ Apoptosis,data = copd1)
summary(mod.5.2)
age.emp.odds <- copd1 %>% 
  summarize(emp.prob = (sum(as.numeric(copd_num))+.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))

ggplot(data= data.frame(Apoptosis = copd1$Apoptosis,pred=predict(mod.5.2)),aes(Apoptosis,pred))+
  geom_line(lty=2)+
  geom_point(data=age.emp.odds,aes(age,emp.odds),col="red")+
  geom_smooth(data=age.emp.odds,aes(age,emp.odds),col="blue",se=F)+
  theme_bw()+
  labs(x="Age (Years)",
       y="Logit CHD Risk")
copd_sex <- as.data.frame(copd)

copd <- as.numeric(as.character(copd[[COPD]]))
unclass(copd1$COPD)
copd1$COPD <- unclass(copd1$COPD)
copd_sex <- copd %>% group_by(COPD)
# Plots for the variable: Sex
copd_sex <- copd$Sex[copd$Sex == 0] <- "male"
copd_sex <- copd$Sex[copd$Sex == 1] <- 'female'
copd_sex <- copd$Sex[copd$Sex == 'Control'] <- 1
copd_sex <- copd$Sex[copd$Sex == 'COPD'] <- 2
copd_sex
copd <- copd %>% drop_na(Sex)
copd_sex %>%
  ggplot(aes(Sex, fill = COPD)) +
  geom_bar(position = position_dodge()) +
  labs(title = "Comparing BMI Model", subtitle = "Marital Status VS Log of BMI",
       x = "Marital Status", y = "log of BMI") +
  theme(plot.title = element_text(size = 20, face = 'bold', family = 'Times'),
        plot.subtitle = element_text(size = 15, family = 'Times'),
        axis.title = element_text(size = 12, family = 'Times', face = 'bold.italic'),
        axis.text = element_text(size = 10, family = 'Times', face = 'bold'),
        panel.background = element_blank(),
        plot.background = element_rect(fill = 'grey100'),
        panel.grid.major = element_blank(),
        legend.position = 'None')


sex.emp.odds <- copd_sex %>% 
  group_by(COPD) %>% 
  summarize(emp.prob = (sum(Sex)+.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))

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

age.emp.odds <- wcgs2 %>% 
  group_by(age) %>% 
  summarize(emp.prob = (sum(chd69)+.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))



#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(copd), replace=TRUE, prob=c(0.7,0.3))
train <- copd[sample, ]
test <- copd[!sample, ]  
library(car)
library(caret)
library(pscl)
#fit logistic regression model
model <- glm(copd_num ~ smkstatus, family="binomial", data=train)
summary(model)
pscl::pR2(model)["McFadden"]
caret::varImp(model)
#define two individuals
new <- data.frame(smkstatus = 3, copd_num = c("Yes", "No"))

#predict probability of defaulting
predict(model, new, type="response")
#calculate probability of default for each individual in test dataset
predicted <- predict(model, test, type="response")
predicted
install.packages('InformationValue')
library(InformationValue)

#convert defaults from "Yes" and "No" to 1's and 0's
test$copd_num <- ifelse(test$copd_num == "Yes", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$copd_num, predicted)[1]
optimal
confusionMatrix(test$COPD, predicted)

#calculate sensitivity
sensitivity(test$copd_num, predicted)
#calculate specificity
specificity(test$copd_num, predicted)
#calculate total misclassification error rate
misClassError(test$copd_num, predicted, threshold=optimal)
#plot the ROC curve
plotROC(test$copd_num, predicted)

car::vif(model)

WCGS$bmi.cat <- cut(WCGS$bmi,c(0,18.5,24.99,29.99,1000),right=F)
table(WCGS$bmi.cat)
copd.emp.odds <- copd %>% 
  group_by(smkstatus) %>% 
  summarize(smkyrs.mean = mean(SmkYrs),emp.prob = (sum(copd_num)+.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))

glm.copd <- glm(as.factor(COPD) ~ smkstatus + SmkYrs + Age + Sex + RACE + Apoptosis +
                  Necrosis, data = copd,family=binomial)

ggplot(data=data.frame(copd.result = glm.copd$model$copd_num, pred = predict(glm.copd)), aes(copd_num,pred))+
  geom_point(alpha=0.1)+
  geom_point(data=smkyrs.emp.odds,aes(smkyrs.mean,emp.odds),col="red",size=3)+
  geom_smooth(data=smkyrs.emp.odds,aes(smkyrs.mean,emp.odds),col="blue",se=F,size=1.5)+
  theme_bw()+
  labs(x="MultiVariable Analysis",
       y="Logit COPD Risk")


WCGS$age_10<-scale(WCGS$age, center=TRUE, scale=FALSE)/10
WCGS$chol_50<-scale(WCGS$chol, center=TRUE, scale=FALSE)/50
WCGS$sbp_50<-scale(WCGS$sbp, center=TRUE, scale=FALSE)/50
WCGS$bmi_10<-scale(WCGS$bmi, center=TRUE, scale=FALSE)/10

WCGS$dibpat_rel <- relevel(WCGS$dibpat,ref="Type B")

WCGS$bmisbp <- WCGS$bmi_10*WCGS$sbp_50
WCGS$bmichol <- WCGS$bmi_10*WCGS$chol_50

mod.5.18 <- glm(chd69~age_10+chol_50+sbp_50+bmi_10+smoke+dibpat_rel+bmichol+bmisbp,
                data=WCGS[WCGS$chol<645,],
                family=binomial)

plot(mod.5.18,which=4,id.n=3)

#Goodness of Fit test
library(ResourceSelection)
hoslem.test(mod.5.18$model$chd69, fitted(mod.5.18), g=10)

#prediction accuracy
library(caret)
prob <- predict(mod.5.18,type="response")
pred.val <- ifelse(prob > .5, 1,0)
confusionMatrix(factor(pred.val),factor(mod.5.18$model$chd69))

library(broom)
d <- augment(mod.5.18)