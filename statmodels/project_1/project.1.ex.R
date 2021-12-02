library(tidyverse)
data.ex.na <- data.ex %>% drop_na(BMI) #remove missing BMI records
data.ex.na <- data.ex.na %>% select(ID,BMI,age,race,smoke) #select variables
data.ex.na <- data.ex.na %>% filter(age >= 50 & age <= 70) #remove ind.

data.ex.na$bmicat <- cut(data.ex.na$BMI,c(0,18.5,24.99,29.99,1000)) #create bmi catgeories
levels(data.ex.na$bmicat) <- c("Underweight","Normal","Overweight","Obese")

#table 1
data.ex.na %>%
  group_by(bmicat) %>%
  summarize(mean=mean(age),st.dev=sd(age))
data.ex.na %>%
  summarize(mean=mean(age),st.dev=sd(age))
with(data.ex.na,table(smoke,bmicat))
with(data.ex.na,prop.table(table(smoke,bmicat),2))
with(data.ex.na,table(race,bmicat))
with(data.ex.na,prop.table(table(race,bmicat),2))

aov.ex <- aov(age~bmicat,data=data.ex.na)
summary(aov.ex)

#table 2
reg.age <- lm(BMI~age,data=data.ex.na)
summary(reg.age)
confint(reg.age)
reg.smoke <- lm(BMI~smoke,data.ex.na)
summary(reg.smoke)

reg.race <- lm(BMI~as.factor(race),data.ex.na)
summary(reg.race)
confint(reg.race)

reg.full <- lm(BMI~age+smoke+as.factor(race),data.ex.na)
summary(reg.full)
confint(reg.full)