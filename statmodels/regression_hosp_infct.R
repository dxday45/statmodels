library(tidyverse)
library(broom)
library(ggplot2)
install.packages('ggpubr')
library(rstatix)
glimpse(hospital_infct)
lm(InfctRsk ~ Stay, data = hospital_infct) %>%
  summary()
# As the length of days stayed at the hospital increases by one day, the infection rate increases by 0.3742 
ggplot(hospital_infct, aes(Stay, InfctRsk))+
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE)+
  geom_smooth(color = 'red')
cor(hospital_infct$Stay, hospital_infct$InfctRsk)

nurses_xrays <- lm(InfctRsk ~ Stay + Nurses + Xray, data = hospital_infct)
ggplot(hospital_infct, aes(Stay Nurses Xray, InfctRsk)) +
  geom_point() +
  geom_smooth()

nurses_xrays %>%
  summary()
ggplotregression <- lm
}
ggplotregression(lm(InfctRsk ~ Stay + Nurses + Xray, data = hospital_infct))+
  geom_point()



xxx <- lm(InfctRsk**2 ~ (Stay + Nurses + Xray + as.factor(Region))**2, data = hospital_infct)
summary(xxx)
x2 <- lm(InfctRsk ~ log(Stay + Nurses + Xray + as.factor(Region)), data = hospital_infct, na.action = FALSE)
infct <- hospital_infct %>% log(hospital_infct$InfctRsk)
infct
infct <-
  hospital_infct %>%
  mutate(risk = log(InfctRsk), stay = log(Stay), nurse = log(Nurses), xray = log(Xray))
summary(lm(risk ~ stay + nurse + xray + as.factor(Region), data = infct))
         