library(tidyverse)

#check for linearity of quantitative variables
mod.5.2 <- glm(chd69~age,data=wcgs2,family="binomial")

age.emp.odds <- wcgs2 %>% 
  group_by(age) %>% 
  summarize(emp.prob = (sum(chd69)+.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))

ggplot(data=data.frame(age=wcgs2$age,pred=predict(mod.5.2)),aes(age,pred))+
  geom_line(lty=2)+
  geom_point(data=age.emp.odds,aes(age,emp.odds),col="red")+
  geom_smooth(data=age.emp.odds,aes(age,emp.odds),col="blue",se=F)+
  theme_bw()+
  labs(x="Age (Years)",
       y="Logit CHD Risk")

WCGS <- wcgs2

WCGS$bmi.cat <- cut(WCGS$bmi,c(0,18.5,24.99,29.99,1000),right=F)
table(WCGS$bmi.cat)
bmi.emp.odds <- WCGS %>% 
  group_by(bmi.cat) %>% 
  summarize(mean.bmi=mean(bmi,na.rm=T),emp.prob = (sum(chd69)+.5)/(n())) %>% 
  mutate(emp.odds=log((emp.prob/(1-emp.prob))))

mod.5.18.ex <- glm(chd69~age+chol+bmi+sbp+smoke+dibpat,data=WCGS,family=binomial)

ggplot(data=data.frame(bmi=mod.5.18.ex$model$bmi,pred=predict(mod.5.18.ex)),aes(bmi,pred))+
  geom_point(alpha=0.1)+
  geom_point(data=bmi.emp.odds,aes(mean.bmi,emp.odds),col="red",size=3)+
  geom_smooth(data=bmi.emp.odds,aes(mean.bmi,emp.odds),col="blue",se=F,size=1.5)+
  theme_bw()+
  labs(x="BMI",
       y="Logit CHD Risk")


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

