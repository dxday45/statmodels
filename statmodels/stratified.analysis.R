#Stratified Analysis with R
#load mosaic package
library(tidyverse)
library(mosaic)

#look at names of variables in data set
names(wcgs2)

#calculate means
mean(wcgs2$bmi)
mean(wcgs2$chol)
mean(wcgs2$chol,na.rm=T)

#stratified analyses
with(wcgs2,aggregate(bmi,by=list(behpat),mean))

with(wcgs2,mean(bmi~behpat))
with(wcgs2,mean(chol~behpat))
with(wcgs2,mean(chol~behpat,na.rm=T))
with(wcgs2,sd(chol~behpat,na.rm=T))

wcgs2 %>% summarize(mean(bmi),sd(bmi),sum(table(bmi)))
wcgs2 %>% summarize(mean=mean(bmi),stdev=sd(bmi),count=sum(table(bmi)))
wcgs2 %>% group_by(behpat) %>% summarize(mean=mean(bmi),stdev=sd(bmi),count=sum(table(bmi)))
wcgs2 %>% group_by(behpat,smoke) %>% summarize(mean=mean(bmi),stdev=sd(bmi),count=sum(table(bmi)))

#scatterplot
plot(wcgs2$bmi,wcgs2$chol)

#scatterplot with regression line
plot(wcgs2$bmi,wcgs2$chol)
abline(lm(wcgs2$chol~wcgs2$bmi),col="red")

#adding labels to scatterplots
plot(wcgs2$bmi,wcgs2$chol,main="bmi vs chol")
abline(lm(wcgs2$chol~wcgs2$bmi),col="red")

plot(wcgs2$bmi,wcgs2$chol,main="bmi vs chol",ylab="cholesterol",xlab="body mass index")

