sac <- south_africa_chd
summary(sac)
table(sac$obesity)
prop.table(table(sac$obesity))
mean(sac$sbp)
median(sac$sbp)
sd(sac$sbp)
fivenum(sac$sbp)
barplot(table(sac$obesity), col = "darkred", border = "gray10", main = "Obesity Rates", ylab = "Frequency")
hist(sac$sbp, col = "blue", border = "ivory", main = "Systolic Blood Pressure (SBP)", xlab = "SBP (ml of HG)", freq = TRUE)
hist(sac$sbp, main = "Systolic Blood Pressure (SBP)", xlab = "SBP (ml of HG)", breaks = c(100, 130,160, 190, 220, 250), labels = TRUE)
hist(log(sac$sbp))     
boxplot(sac$sbp ~ sac$obesity, col = "cyan", main = "Systolic Blood Pressure (SBP)", xlab = "Weight Categories",
        ylab = "SBP (ml of HG)")
stem(sac$sbp, scale = 2)
names(sac)
attach(sac)
cor(sac$sbp, sac$ldl)
plot(sbp, ldl)
plot(sbp, bmi)
cor(sbp, bmi, method = "spearman")
cor(bmi, bmi2)
plot(bmi, bmi2)
table(race, obesity)
prop.table(table(chd, obesity),1)
prop.table(table(chd, obesity),2)

aggregate(sbp,by = list(obesity), mean)
aggregate(sbp, by = list(obesity), median)
aggregate(sbp, by = list(obesity, race), summary)
aggregate(chd, by = list(obesity, race), table)
aggregate(chd, by = list(obesity, race, famhist), table)


#EDA example multivariate 2
barplot(table(famhist, obesity), col = c("darkred", "cyan"))
barplot(table(famhist, obesity), col = c("darkred", "cyan"), legend = T, main = "Family History of CHD by BMI category",
        beside = T)
mosaicplot(table(famhist, obesity), color = c("darkred", "cyan"), main = "Family History of CHD by BMI Category",
                                              xlab = "Family History", ylab = "BMI Levels")
boxplot(sbp ~ obesity, col = rainbow(4), main = "SBP by BMI Category")
boxplot(sbp ~ obesity, col = rainbow(4), main = "SBP by BMI Category", notch = T)
plot(age, bmi,col=famhist)
plot(age, sbp, col = obesity)




