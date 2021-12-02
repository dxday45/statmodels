library(survival)
library(KMsurv)
library(ggfortify)

#use 6mp data

my.surv <- Surv(X6mp$time,X6mp$event) #set survival data
survfit(my.surv~1) #descriptives
summary(survfit(my.surv~1)) #survivor function
plot(my.surv) #KM survivor function graph

survfit(my.surv~X6mp$group) # descriptives by tx group
summary(survfit(my.surv~X6mp$group)) #S(t) by tx group
plot(survfit(my.surv~X6mp$group),main="KM curves by group")

autoplot(survfit(my.surv~X6mp$group))

survdiff(my.surv~X6mp$group) #log rank test

#unos data
unos.surv <- Surv(unos$fu,unos$death)
survfit(unos.surv~unos$txtype)
summary(survfit(unos.surv~unos$txtype))
plot(survfit(unos.surv~unos$txtype))
survdiff(unos.surv~unos$txtype)

cox.unos.txtype <- coxph(unos.surv~txtype,data=unos)
summary(cox.unos.txtype)

cox.age <- coxph(unos.surv~age,data=unos)
summary(cox.age)


#pbc data

pbc.surv <- Surv(pbc$years,pbc$status)
cox.pbc <- coxph(pbc.surv~rx+bilirubin,data=pbc)
summary(cox.pbc)

cox.pbc <- coxph(pbc.surv~factor(pbc$histol))
summary(cox.pbc)




