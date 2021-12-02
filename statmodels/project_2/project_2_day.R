### Packages ###
library(tidyverse)
library(tableone)
library(questionr)
library(car)
library(caret)
library(pscl)
library(InformationValue)
library(pROC)
library(ResourceSelection)
library(broom)

### Filtering and Selecting the Data ###
copd <- COPD_data %>% filter(COPD == 'COPD' | COPD == 'Control') %>% 
  select('COPD', 'Age', 'Sex', 'smkstatus', 'SmkYrs', 'AGECAT', 'RACE', 'Apoptosis', 'Necrosis')
glimpse(copd)

### Changing NAs for years smoked to 0 and creating a new column labeling "COPD" as 1 and "Control" as 0 ###
copd$SmkYrs[is.na(copd$SmkYrs)] <- 0
copd$copd_num[copd$COPD == "Control"] <- 0
copd$copd_num[copd$COPD == "COPD"] <- 1

### Looking at some relationships of variables ###

##################################
### Years Smoked and Apoptosis ###
##################################

lm(Apoptosis ~ SmkYrs, data = copd) %>%
  summary()
cor(copd$Apoptosis, copd$SmkYrs)
ggplot(copd, aes(SmkYrs, Apoptosis))+
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE)+
  geom_smooth(color = 'red') +
  labs(title = "Apoptosis and Years Smoked", subtitle = 'Correlation = 0.08211263', x = 'Apoptosis',
       y = 'Years Smoked') +
  theme(title = element_text(family = 'Garamond', face = 'bold', size = 20),
        plot.subtitle = element_text(family = 'Roboto', face = 'bold.italic', size = 13),
        axis.title = element_text(family = 'Roboto', face = 'italic', size = 10),
        axis.text = element_text(family = 'Roboto', face = 'bold.italic', size = 7)) +
  theme_bw()

#################################
### Years Smoked and Necrosis ###
#################################

lm(Necrosis ~ SmkYrs, data = copd) %>%
  summary()
cor(copd$Necrosis, copd$SmkYrs)
ggplot(copd, aes(SmkYrs, Necrosis))+
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE)+
  geom_smooth(color = 'red') +
  labs(title = "Necrosis and Years Smoked", subtitle = 'Correlation = 0.0305', x = 'Necrosis',
       y = 'Years Smoked') +
  theme(title = element_text(family = 'Garamond', face = 'bold', size = 20),
        plot.subtitle = element_text(family = 'Roboto', face = 'bold.italic', size = 13),
        axis.title = element_text(family = 'Roboto', face = 'italic', size = 10),
        axis.text = element_text(family = 'Roboto', face = 'bold.italic', size = 7)) +
  theme_bw()

### Smoking Status and Years Smoked ###
lm(smkstatus ~ SmkYrs, data = copd) %>%
  summary()
cor(copd$smkstatus, copd$SmkYrs)
ggplot(copd, aes(SmkYrs, smkstatus))+
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE)+
  geom_smooth(color = 'red') +
  labs(title = "Smoking Status and Years Smoked", subtitle = 'Correlation = 0.7427427', x = 'Years Smoked',
       y = 'Smoking Status 0 = Never, 1 = Former, 2 = Current') +
  theme(title = element_text(family = 'Garamond', face = 'bold', size = 20),
        plot.subtitle = element_text(family = 'Roboto', face = 'bold.italic', size = 13),
        axis.title = element_text(family = 'Roboto', face = 'italic', size = 10),
        axis.text = element_text(family = 'Roboto', face = 'bold.italic', size = 7)) +
  theme_bw()


### Smoking Status and All Variables ###
lm(smkstatus ~ SmkYrs + Sex + AGECAT + RACE + Apoptosis + Necrosis, data = copd) %>%
  summary()
cor(copd$smkstatus, copd$SmkYrs + copd$Age + copd$Sex + copd$AGECAT + copd$RACE +
      copd$Apoptosis + copd$Necrosis)
lm.1 <- lm(smkstatus ~ SmkYrs + Sex + Age + AGECAT + RACE + Apoptosis + Necrosis, data = copd)
summary(lm.1)
reg.lm.1 <- lm(smkstatus ~ log(SmkYrs + Age + Sex + SmkYrs + AGECAT + RACE +
  Apoptosis + Necrosis), data = copd)
summary(reg.lm.1)
confint(reg.lm.1)
plot(reg.lm.1)

##########################################
### Creating table with COPD Breakdown ###
##########################################

### Summarizing the quantitative variables apoptosis and necrosis ###

### Using the ranges from a study from the NIH and then spreading across the dataset range ###
copd$ApoptosisCat <- cut(copd$Apoptosis, c(0, 26, 68, 100))

### Poorly Differentiated, Moderately Differentiated, Well Differentiated ###
levels(copd$ApoptosisCat) <- c("Poorly Diff", "Moderately Diff", "Well Diff")

copd %>%
  group_by(COPD, Sex, ApoptosisCat) %>%
  summarize(mean=mean(Apoptosis),st.dev=sd(Apoptosis))

with(copd, table(COPD, Sex, ApoptosisCat))
with(copd, prop.table(table(COPD, Sex, ApoptosisCat)))
chisq.test(with(copd, table(ApoptosisCat, COPD)))
aov.apop <- aov(Apoptosis ~ COPD,data = copd)
summary(aov.apop)
par(mfrow = c(2,2))
plot(aov.apop)

copd %>%
  group_by(COPD, Sex) %>%
  summarize(mean=mean(Necrosis),st.dev=sd(Necrosis))
with(copd, table(COPD, Necrosis))
aov.necr <- aov(Necrosis ~ COPD,data = copd)
summary(aov.necr)
par(mfrow = c(2,2))
plot(aov.necr)

### Summarizing Categorical Variables Sex: 1 = Female, 2 = Male ###
### smkstatus: 0 = never smoked, 1 = former smoker, 2 = current smoker ###

with(copd, table(Sex, smkstatus, COPD))
with(copd, prop.table(table(Sex, smkstatus, COPD), 2))
chisq.test(with(copd, table(smkstatus, COPD)))

### Sex: 1 = Female, 2 = Male; RACE: 1 = white, 2 = hispanic, 3 = black, 4 = other ###
with(copd, table(Sex, RACE, COPD))
with(copd, prop.table(table(Sex, RACE, COPD), 2))
aov.ex <- aov(as.integer(RACE) ~ COPD,data = copd)
summary(aov.ex)
chisq.test(with(copd, table(RACE, COPD)))

### Sex: 1 = Female, 2 = Male; AGECAT: 1 = 40 and under, 2 = 41-50, 3 = 51-60, 4 = 61-70, 5 = 71 + ###
with(copd, table(Sex, AGECAT, COPD))
with(copd, prop.table(table(Sex, AGECAT, COPD), 2))
aov.ex <- aov(as.integer(AGECAT) ~ COPD,data = copd)
summary(aov.ex)
chisq.test(with(copd, table(AGECAT, COPD)))

### Sex: 1 = Female, 2 = Male; Years Smoking ###
with(copd, table(Sex, SmkYrs, COPD))
with(copd, prop.table(table(Sex, SmkYrs, COPD), 2))
aov.smkyrs <- aov(as.integer(SmkYrs) ~ COPD,data = copd)
summary(aov.smkyrs)
par(mfrow = c(2,2))
plot(aov.smkyrs)

### Breakdown of each variable stratified by Apoptosis Categories (Poor, Moderate, Well) ###
catvars <- c("Sex", "RACE", "smkstatus", "AGECAT")
rm(vars)
vars <- c("Age", "Apoptosis", "Necrosis", "Sex", "RACE", "smkstatus", "SmkYrs", "AGECAT")
apop_table <- CreateTableOne(vars = vars, strata = "COPD", data = copd, factorVars = catvars)
apop_table

#######################################################
### Looking at Logistic Regression of the Variables ###
#######################################################

###############################
### COPD and Smoking Status ###
###############################

glm.smkstatus <- glm(as.factor(COPD) ~ smkstatus,data = copd,family=binomial)
exp(coefficients(glm.smkstatus))
smkstatus.emp.odds <- copd %>% 
  group_by(smkstatus) %>%
  summarize(emp.prob = (sum(copd_num, na.rm = TRUE) +.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))
smkstatus.emp.odds
summary(odds.ratio(glm.smkstatus))
odds.ratio(glm.smkstatus)

copd.smkstatus.f <- copd %>%
  filter(Sex == 1)
glm.smkstatus.f <- glm(as.factor(COPD) ~ smkstatus,data = copd.smkstatus.f,family=binomial)
odds.ratio(glm.smkstatus.f)

copd.smkstatus.m <- copd %>%
  filter(Sex == 2)
glm.smkstatus.m <- glm(as.factor(COPD) ~ smkstatus,data = copd.smkstatus.m,family=binomial)
odds.ratio(glm.smkstatus.m)

ggplot(data=data.frame(smkstatus = copd$smkstatus, pred = predict(glm.smkstatus)),aes(smkstatus,pred))+
  geom_line(lty=2)+
  geom_point(data=smkstatus.emp.odds,aes(smkstatus,emp.odds),col="red")+
  geom_smooth(data=smkstatus.emp.odds,aes(smkstatus,emp.odds),col="blue",se=F)+
  theme_bw()+
  labs(title = "COPD by Smoking Status",x="Smoking Status 1: Never, 2: Former, 3: Current",
       y="Logit COPD Risk")
  
#Use 80% of dataset as training set and remaining 20% as testing set
sample.smkstatus <- sample(c(TRUE, FALSE), nrow(copd), replace=TRUE, prob=c(0.8,0.2))
train.smkstatus <- copd[sample.smkstatus, ]
test.smkstatus <- copd[!sample.smkstatus, ]  

#fit logistic regression model
model.smkstatus <- glm(copd_num ~ smkstatus, family="binomial", data=train)
summary(model.smkstatus)
pscl::pR2(model.smkstatus)["McFadden"]
caret::varImp(model.smkstatus, scale = FALSE)
#define two individuals
new.smkstatus <- data.frame(smkstatus = 3, COPD = c(1, 0))

#predict probability of COPD
predict(model.smkstatus, new.smkstatus, type="response")
#calculate probability of COPD for each individual in test dataset
predicted <- predict(model, test, type="response")
predicted

#convert COPD from "Yes" and "No" to 1's and 0's
test.smkstatus$copd_num <- ifelse(test.smkstatus$COPD == "COPD", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal.smkstatus <- optimalCutoff(test.smkstatus$COPD, predicted)[1]
optimal
confusionMatrix(test.smkstatus$COPD, predicted)

# calculate total misclassification error rate
misClassError(test.smkstatus$copd_num, predicted, threshold=optimal)
#plot the ROC curve
plotROC(test.smkstatus$copd_num, predicted)

roc.smkstatus = roc(as.factor(COPD) ~ smkstatus, data=train.smkstatus) 
plot(roc.smkstatus, col="red")

#############################
### COPD and Years Smoked ###
#############################

glm.smkyrs <- glm(as.factor(COPD) ~ SmkYrs,data = copd,family=binomial)
exp(coefficients(glm.smkyrs))
smkyrs.emp.odds <- copd %>% 
  group_by(SmkYrs) %>%
  summarize(emp.prob = (sum(copd_num, na.rm = TRUE) +.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))
smkyrs.emp.odds
summary(odds.ratio(glm.smkyrs))
odds.ratio(glm.smkyrs)

copd.smkyrs.f <- copd %>%
  filter(Sex == 1)
glm.smkyrs.f <- glm(as.factor(COPD) ~ SmkYrs,data = copd.smkyrs.f,family=binomial)
odds.ratio(glm.smkyrs.f)

copd.smkyrs.m <- copd %>%
  filter(Sex == 2)
glm.smkyrs.m <- glm(as.factor(COPD) ~ SmkYrs,data = copd.smkyrs.m,family=binomial)
odds.ratio(glm.smkyrs.m)

set.seed(12345)
#Use 80% of dataset as training set and remaining 20% as testing set
sample.smkyrs <- sample(c(TRUE, FALSE), nrow(copd), replace=TRUE, prob=c(0.8,0.2))
train.smkyrs <- copd[sample.smkyrs, ]
test.smkyrs <- copd[!sample.smkyrs, ]  

#fit logistic regression model
model.smkyrs <- glm(copd_num ~ SmkYrs, family="binomial", data=train)
summary(model.smkyrs)
pscl::pR2(model.smkyrs)["McFadden"]
caret::varImp(model.smkyrs, scale = FALSE)

#define two individuals
new.smkyrs <- data.frame(SmkYrs=5, COPD = c(1, 0))

#predict probability of COPD
predict(model.smkyrs, new.smkyrs, type="response")

#calculate probability of COPD for each individual in test dataset
predicted <- predict(model, test, type="response")
predicted

#convert COPD from "Yes" and "No" to 1's and 0's
test.smkyrs$copd_num <- ifelse(test.smkyrs$COPD == "COPD", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal.smkyrs <- optimalCutoff(test$COPD, predicted)[1]
optimal
confusionMatrix(test$COPD, predicted)

#calculate total misclassification error rate
misClassError(test.smkyrs$copd_num, predicted, threshold=optimal)

#plot the ROC curve
plotROC(test.smkyrs$copd_num, predicted)
roc.smkyrs = roc(as.factor(COPD) ~ SmkYrs, data=train.smkyrs) 
plot(roc.smkyrs, col="red")

####################
### COPD and Age ###
####################

glm.age <- glm(as.factor(COPD) ~ Age,data = copd,family=binomial)
exp(coefficients(glm.age))
age.emp.odds <- copd %>% 
  group_by(Age) %>%
  summarize(emp.prob = (sum(copd_num, na.rm = TRUE) +.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))
age.emp.odds
summary(odds.ratio(glm.age))
odds.ratio(glm.age)

copd.age.f <- copd %>%
  filter(Sex == 1)
glm.age.f <- glm(as.factor(COPD) ~ Age,data = copd.age.f,family=binomial)
odds.ratio(glm.age.f)

copd.age.m <- copd %>%
  filter(Sex == 2)
glm.age.m <- glm(as.factor(COPD) ~ Age,data = copd.age.m,family=binomial)
odds.ratio(glm.age.m)

set.seed(6325981)
#Use 80% of dataset as training set and remaining 20% as testing set
sample.age <- sample(c(TRUE, FALSE), nrow(copd), replace=TRUE, prob=c(0.8,0.2))
train.age <- copd[sample.age, ]
test.age <- copd[!sample.age, ]  

#fit logistic regression model
model.age <- glm(copd_num ~ Age, family="binomial", data=train.age)
summary(model.age)
pscl::pR2(model.age)["McFadden"]
caret::varImp(model.age, scale = FALSE)
#define two individuals
new.age <- data.frame(age = 4, COPD = c(1, 0))

#calculate probability of COPD for each individual in test dataset
predicted <- predict(model.age, test.age, type="response")
predicted

#convert COPD from "Yes" and "No" to 1's and 0's
test.age$copd_num <- ifelse(test.age$COPD == "COPD", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal.age <- optimalCutoff(test.age$COPD, predicted)[1]
optimal
confusionMatrix(test.age$COPD, predicted)

#calculate total misclassification error rate
misClassError(test.age$copd_num, predicted, threshold=optimal)

#plot the ROC curve
plotROC(test.age$copd_num, predicted)
roc.age = roc(as.factor(COPD) ~ Age, data=train.age) 
plot(roc.age, col="red")

### Multivariable Prediction ###
copd.emp.odds <- copd %>% 
  group_by(smkstatus) %>% 
  summarize(smkyrs.mean = mean(SmkYrs),emp.prob = (sum(copd_num)+.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))

glm.copd <- glm(copd_num ~ smkstatus + SmkYrs + Age, data = copd,family=binomial)
plot(glm.copd)
odds.ratio(glm.copd)

copd.multi.f <- copd %>%
  filter(Sex == 1)
glm.multi.f <- glm(as.factor(COPD) ~ smkstatus + SmkYrs + Age,data = copd.multi.f,family=binomial)
odds.ratio(glm.multi.f)

copd.multi.m <- copd %>%
  filter(Sex == 2)
glm.multi.m <- glm(as.factor(COPD) ~ smkstatus + SmkYrs + Age,data = copd.multi.m,family=binomial)
odds.ratio(glm.multi.m)

set.seed(2356410)
#Use 80% of dataset as training set and remaining 20% as testing set
sample.multi <- sample(c(TRUE, FALSE), nrow(copd), replace=TRUE, prob=c(0.8,0.2))
train.multi <- copd[sample.multi, ]
test.multi <- copd[!sample.multi, ]  

#fit logistic regression model
model.multi <- glm(copd_num ~ smkstatus + SmkYrs + Age,family="binomial", data=train.multi)
summary(model.multi)
pscl::pR2(model.multi)["McFadden"]
caret::varImp(model.multi, scale = FALSE)

#define two individuals
new.multi <- data.frame(smkstatus = 3, COPD = c(1, 0))

#predict probability of COPD
predict(model.multi, new.multi, type="response")

#calculate probability of COPD for each individual in test dataset
predicted <- predict(model.multi, test.multi, type="response")
predicted

#convert COPD from "Yes" and "No" to 1's and 0's
test.multi$copd_num <- ifelse(test.multi$COPD == "COPD", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal.multi <- optimalCutoff(test.multi$COPD, predicted)[1]
optimal
confusionMatrix(test.multi$COPD, predicted)

#calculate total misclassification error rate
misClassError(test.multi$copd_num, predicted, threshold=optimal)

#plot the ROC curve
plotROC(test.multi$copd_num, predicted)
roc.multi = roc(as.factor(COPD) ~ smkstatus + SmkYrs + Age, data=train.multi) 
plot(roc.Sex, col="red")

copd$smkyrs_10<-scale(copd$SmkYrs, center=TRUE, scale=FALSE)/10
copd$smkstatus_50<-scale(copd$smkstatus, center=TRUE, scale=FALSE)/50
copd$apop_50<-scale(copd$Apoptosis, center=TRUE, scale=FALSE)/50
copd$age_10<-scale(copd$Age, center=TRUE, scale=FALSE)/10

copd$smkyrs.apop <- copd$smkyrs_10*copd$apop_50
copd$smkstatus.age <- copd$age_10*copd$smkstatus_50

glm.copd.1 <- glm(copd_num ~ smkyrs_10+smkstatus_50 + apop_50 + age_10+ smkyrs.apop+smkstatus.age,
                  data=copd,
                  family=binomial)

plot(glm.copd.1,which=4,id.n=3)

#####################
### COPD and RACE ###
#####################

glm.race <- glm(as.factor(COPD) ~ RACE,data = copd,family=binomial)
exp(coefficients(glm.race))
race.emp.odds <- copd %>% 
  group_by(RACE) %>%
  summarize(emp.prob = (sum(copd_num, na.rm = TRUE) +.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))
race.emp.odds
summary(odds.ratio(glm.1))
odds.ratio(glm.smkyrs)

glm.race <- glm(as.factor(COPD) ~ RACE,data = copd,family=binomial)
exp(coefficients(glm.race))
race.emp.odds <- copd %>% 
  group_by(RACE) %>%
  summarize(emp.prob = (sum(copd_num, na.rm = TRUE) +.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))
race.emp.odds
summary(odds.ratio(glm.race))
odds.ratio(glm.race)

set.seed(478689)

#Use 80% of dataset as training set and remaining 20% as testing set
sample.race <- sample(c(TRUE, FALSE), nrow(copd), replace=TRUE, prob=c(0.8,0.2))
train.race <- copd[sample.race, ]
test.race <- copd[!sample.race, ]  

#fit logistic regression model
model.race <- glm(copd_num ~ RACE, family="binomial", data=train.race)
summary(model.race)
pscl::pR2(model.race)["McFadden"]
caret::varImp(model.race, scale = FALSE)

#define two individuals
new.race <- data.frame(RACE = 4, COPD = c(1, 0))

#predict probability of COPD
predict(model.race, new.race, type="response")

#calculate probability of COPD for each individual in test dataset
predicted <- predict(model.race, test.race, type="response")
predicted

#convert COPD from "Yes" and "No" to 1's and 0's
test.race$copd_num <- ifelse(test.race$COPD == "COPD", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal.race <- optimalCutoff(test.race$COPD, predicted)[1]
optimal
confusionMatrix(test.race$COPD, predicted)

#calculate total misclassification error rate
misClassError(test.race$copd_num, predicted, threshold=optimal)

#plot the ROC curve
plotROC(test.race$copd_num, predicted)
roc.race = roc(as.factor(COPD) ~ RACE, data=train.race) 
plot(roc.race, col="red")

####################
### COPD and Sex ###
####################

glm.sex <- glm(as.factor(COPD) ~ Sex,data = copd,family=binomial)
exp(coefficients(glm.sex))
sex.emp.odds <- copd %>% 
  group_by(Sex) %>%
  summarize(emp.prob = (sum(copd_num, na.rm = TRUE) +.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))
sex.emp.odds
summary(odds.ratio(glm.sex))
odds.ratio(glm.sex)

set.seed(478689)

#Use 80% of dataset as training set and remaining 20% as testing set
sample.sex <- sample(c(TRUE, FALSE), nrow(copd), replace=TRUE, prob=c(0.8,0.2))
train.sex <- copd[sample.sex, ]
test.sex <- copd[!sample.sex, ]  

#fit logistic regression model
model.sex <- glm(copd_num ~ sex, family="binomial", data=train.sex)
summary(model.sex)
pscl::pR2(model.sex)["McFadden"]
caret::varImp(model.sex, scale = FALSE)

#define two individuals
new.sex <- data.frame(sex = 4, COPD = c(1, 0))

#predict probability of COPD
predict(model.sex, new.sex, type="response")
#calculate probability of COPD for each individual in test dataset
predicted <- predict(model.sex, test.sex, type="response")
predicted

#convert COPD from "Yes" and "No" to 1's and 0's
test.sex$copd_num <- ifelse(test.sex$COPD == "COPD", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal.sex <- optimalCutoff(test.sex$COPD, predicted)[1]
optimal
confusionMatrix(test.sex$COPD, predicted)

#calculate total misclassification error rate
misClassError(test.sex$copd_num, predicted, threshold=optimal)

#plot the ROC curve
plotROC(test.sex$copd_num, predicted)
roc.Sex = roc(as.factor(COPD) ~ Sex, data=train.sex) 
plot(roc.Sex, col="red")

##########################
### COPD and Apoptosis ###
##########################

glm.apop <- glm(as.factor(COPD) ~ Apoptosis,data = copd,family=binomial)
exp(coefficients(glm.apop))
apop.emp.odds <- copd %>% 
  group_by(Apoptosis) %>%
  summarize(emp.prob = (sum(copd_num, na.rm = TRUE) +.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))
apop.emp.odds
summary(odds.ratio(glm.apop))
odds.ratio(glm.apop)

set.seed(236559)
#Use 80% of dataset as training set and remaining 20% as testing set
sample.apop <- sample(c(TRUE, FALSE), nrow(copd), replace=TRUE, prob=c(0.8,0.2))
train.apop <- copd[sample.apop, ]
test.apop <- copd[!sample.apop, ]  

#fit logistic regression model
model.apop <- glm(copd_num ~ Apoptosis, family="binomial", data=train.apop)
summary(model.apop)
pscl::pR2(model.apop)["McFadden"]
caret::varImp(model.apop, scale = FALSE)

#define two individuals
new.apop <- data.frame(Apoptosis = 60, COPD = c(1, 0))

#predict probability of COPD
predict(model.apop, new.apop, type="response")

#calculate probability of COPD for each individual in test dataset
predicted <- predict(model.apop, test.apop, type="response")
predicted

#convert COPD from "Yes" and "No" to 1's and 0's
test.apop$copd_num <- ifelse(test.apop$COPD == "COPD", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal.apop <- optimalCutoff(test.apop$COPD, predicted)[1]
optimal
confusionMatrix(test.apop$COPD, predicted)

#calculate total misclassification error rate
misClassError(test.apop$copd_num, predicted, threshold=optimal)

#plot the ROC curve
plotROC(test.apop$copd_num, predicted)
roc.apop = roc(as.factor(COPD) ~ Apoptosis, data=train.apop) 
plot(roc.apop, col="red")

#########################
### COPD and Necrosis ###
#########################

glm.necr <- glm(as.factor(COPD) ~ Necrosis,data = copd,family=binomial)
glm.necr
exp(coefficients(glm.necr))
necr.emp.odds <- copd %>% 
  group_by(Necrosis) %>%
  summarize(emp.prob = (sum(copd_num, na.rm = TRUE) +.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))
necr.emp.odds
summary(odds.ratio(glm.necr))
odds.ratio(glm.necr)

set.seed(746341)
#Use 80% of dataset as training set and remaining 20% as testing set
sample.necr <- sample(c(TRUE, FALSE), nrow(copd), replace=TRUE, prob=c(0.8,0.2))
train.necr <- copd[sample.necr, ]
test.necr <- copd[!sample.necr, ]  

#fit logistic regression model
model.necr <- glm(copd_num ~ Necrosis, family="binomial", data=train.necr)
summary(model.necr)
pscl::pR2(model.necr)["McFadden"]
caret::varImp(model.necr, scale = FALSE)
#define two individuals
new.necr <- data.frame(Necrosis = 1, COPD = c(1, 0))

#predict probability of COPD
predict(model.necr, new.necr, type="response")

#calculate probability of COPD for each individual in test dataset
predicted <- predict(model.necr, test.necr, type="response")
predicted

#convert COPD from "Yes" and "No" to 1's and 0's
test.necr$copd_num <- ifelse(test.necr$COPD == "COPD", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal.necr <- optimalCutoff(test.necr$COPD, predicted)[1]
optimal
confusionMatrix(test.necr$COPD, predicted)


#calculate total misclassification error rate
misClassError(test.necr$copd_num, predicted, threshold=optimal)

#plot the ROC curve
plotROC(test.necr$copd_num, predicted)
roc.necr = roc(as.factor(COPD) ~ Necrosis, data=train.necr) 
plot(roc.necr, col="red")

############################
### Goodness of Fit test ###
############################

hoslem.test(glm.copd.1$model$copd_num, fitted(glm.copd.1), g=10)

d <- augment(glm.copd)
d
