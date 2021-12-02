####Regression Assignment####
library(tidyverse)
detach(acupuncture_1_)

acu <- acupuncture_1_ 

acu%>%
  summarize(age_pk1 = cor(age,pk1),age_pk5 = cor(age,pk5, use = "complete.obs"),
            age_f1 = cor(age,f1), age_f5 = cor(age,f5), age_age = cor(age,age),
            age_chronicity = cor(age, chronicity))
acu %>%
  summarize(chronicity_pk1 = cor(chronicity,pk1),
            chronicity_pk5 = cor(chronicity,pk5, use = 'complete.obs'),
            chronicity_f1 = cor(chronicity,f1), chronicity_f5 = cor(chronicity,f5),
            chronicity_chronicity = cor(chronicity, chronicity))

## Age vs pk1
acu %>% 
  ggplot(aes(age, pk1)) +
  geom_point()+
  theme_bw()+
  labs(title = 'Age vs Headache Severity Score Baseline', subtitle = 'correlation = 0.013',
       y = 'Severity Score: Baseline', x = 'Age of Subject (In Years)')+
  theme(plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 13, face = 'bold.italic'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'slategray'),
        panel.background = element_rect(fill = 'slategray2'),
        axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
        axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
        axis.title.x = element_text(face = 'bold', size = 13, color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 13, color = 'black'))
cor.test(acu$age, acu$pk1)
fit = lm(formula = acu$pk1~acu$age)
summary(fit)
################################################################################
################################################################################
###### The correlation, 0.013, shows a weak, positive, linear relationship ######
#### between the age of the subject and the baseline headache severity score ###
################################################################################
################ LSRL: output of pk1 = 25.6741 + 0.01908(age) ##################
################################################################################
##### Since the p-value for the slope 0.7944 > 0.05, we fail to reject the #####
###### there is insufficient evidence that there is a linear association #######
################ between age and baseline headache severity score ##############
################################################################################
## Age vs pk5
acu %>% 
  ggplot(aes(age, pk5)) +
  geom_point()+
  theme_bw()+
  labs(title = 'Age vs Headache Severity Score 1 Year', subtitle = 'correlation = 0.0881',
       y = 'Severity Score: 1 Year', x = 'Age of Subject (In Years)')+
  theme(plot.title = element_text(size = 25, face = 'bold'),
        plot.subtitle = element_text(size = 13, face = 'bold.italic'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'slategray'),
        panel.background = element_rect(fill = 'slategray2'),
        axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
        axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
        axis.title.x = element_text(face = 'bold', size = 13, color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 13, color = 'black'))
cor.test(acu$age, acu$pk5)
fit = lm(formula = acu$pk5~acu$age)
summary(fit)

################################################################################
################################################################################
##### The correlation, 0.0881, shows a weak, positive, linear relationship #####
#### between the age of the subject and the one year headache severity score ###
################################################################################
################# LSRL: output of pk5 = 12.954 + 0.08652(age) ##################
################################################################################
##### Since the p-value for the slope 0.1274 > 0.05, we fail to reject the #####
######## null hypothesis. There is insufficient evidence that there is a #######
######linear association between age and baseline headache severity score ######
################################################################################

## Age vs f1
acu %>% 
  ggplot(aes(age, f1)) +
  geom_point()+
  theme_bw()+
  labs(title = 'Age vs Headache Frequency: Baseline', subtitle = 'correlation = -0.0579',
       x = 'Age of Subjects (In Years)', y = 'Frequency: Baseline')+
  theme(plot.title = element_text(size = 25, face = 'bold'),
        plot.subtitle = element_text(size = 13, face = 'bold.italic'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'slategray'),
        panel.background = element_rect(fill = 'slategray2'),
        axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
        axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
        axis.title.x = element_text(face = 'bold', size = 13, color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 13, color = 'black'))

cor.test(acu$age, acu$f1)
fit = lm(acu$f1~acu$age)
summary(fit)

################################################################################
################################################################################
## The correlation, -0.0579, shows a very weak, negative, linear relationship ##
#### between the age of the subject and the baseline frequency of headaches ####
################################################################################
############### LSRL: output of f1 = 17.78023 - 0.03513(age) ##################
################################################################################
##### Since the p-value for the slope 0.2473 > 0.05, we fail to reject the #####
######## null hypothesis, there is insufficient evidence that there is a #######
###### linear association between age and baseline frequency of headaches ######
################################################################################

## Age vs f5
acu %>% 
  ggplot(aes(age, f5)) +
  geom_point()+
  theme_bw()+
  labs(title = 'Age vs Headache Frequency: 1 year', subtitle = 'correlation = 0.101',
       x = 'Age of Subjects (In Years)', y = 'Frequency: 1 Year')+
  theme(plot.title = element_text(size = 25, face = 'bold'),
        plot.subtitle = element_text(size = 13, face = 'bold.italic'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'slategray'),
        panel.background = element_rect(fill = 'slategray2'),
        axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
        axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
        axis.title.x = element_text(face = 'bold', size = 13, color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 13, color = 'black'))

cor.test(acu$age, acu$f5)
fit = lm(acu$f5~acu$age)
summary(fit)

################################################################################
################################################################################
##### The correlation, 0.1013, shows a weak, positive, linear relationship #####
#### between the age of the subject and frequency of headaches after 1 year ####
################################################################################
################ LSRL: output of f5 = 5.7892 + 0.03811(age) ####################
################################################################################
###### Since the p-value for the slope 0.04267 < 0.05, we reject the null ######
####### hypothesis, there is sufficient evidence that there is a linear ########
####### association between age and frequency of headaches after 1 year ########
################################################################################

##Age vs Age
acu %>% 
  ggplot(aes(age, age)) +
  geom_point()+
  theme_bw()+
  labs(title = 'Age vs Age', subtitle = 'correlation = 1', x = 'Age of Subject (In Years)',
       y = 'Age of Subject (In Years)')+
  theme(plot.title = element_text(size = 25, face = 'bold'),
        plot.subtitle = element_text(size = 13, face = 'bold.italic'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'slategray'),
        panel.background = element_rect(fill = 'slategray2'),
        axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
        axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
        axis.title.x = element_text(face = 'bold', size = 13, color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 13, color = 'black'))

cor.test(acu$age, acu$age)
fit = lm(acu$age~acu$age)
summary(fit)

################################################################################
################################################################################
########## The correlation, 1.0, shows a very strong, positive, linear #########
#### relationship between the age of the subject and the age of the subject ####
################################################################################

##Age vs Chronicity
acu %>% 
  ggplot(aes(age, chronicity)) +
  geom_point()+
  theme_bw()+
  labs(title = 'Age vs Number of Years with Headache Disorder',
       subtitle = 'correlation = 0.488', x = 'Age of Subject (In Years)',
       y = 'Years With Disorder')+
  theme(plot.title = element_text(size = 25, face = 'bold'),
        plot.subtitle = element_text(size = 13, face = 'bold.italic'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'slategray'),
        panel.background = element_rect(fill = 'slategray2'),
        axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
        axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
        axis.title.x = element_text(face = 'bold', size = 13, color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 13, color = 'black'))

cor.test(acu$age, acu$chronicity)
fit = lm(acu$chronicity~acu$age)
summary(fit)

################################################################################
################################################################################
######### The correlation, 0.4876, shows a moderate, positive, linear ##########
#### relationship between the age of the subject and the number of years of ####
######################## having the headache disorder ##########################
################################################################################
############ LSRL: output of chronicity = -6.2025 + 0.60743(age) ###############
################################################################################
####### Since the p-value for the slope 0.00 < 0.05, we  reject the null #######
###### hypothesis, there is insufficient evidence that there is a linear #######
#### association between age and number of years with the headache disorder ####
################################################################################

##Chronicity vs pk1
acu %>% 
  ggplot(aes(chronicity, pk1)) +
  geom_point()+
  theme_bw()+
  labs(title = 'Number of Years with Headache Disorder vs Headache Severity Score: Baseline',
       subtitle = 'correlation = 0.0783', x = 'Years with Disorder', y = 'Severity Score: Baseline')+
  theme(plot.title = element_text(size = 25, face = 'bold'),
        plot.subtitle = element_text(size = 13, face = 'bold.italic'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'slategray'),
        panel.background = element_rect(fill = 'slategray2'),
        axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
        axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
        axis.title.x = element_text(face = 'bold', size = 13, color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 13, color = 'black'))

cor.test(acu$chronicity, acu$pk1)
fit = lm(acu$pk1~acu$chronicity)
summary(fit)
################################################################################
################################################################################
##### The correlation, 0.07832, shows a weak, positive, linear relationship ####
####### between the chronicity  and the baseline headache severity score #######
################################################################################
############ LSRL: output of pk1 = 24.5367 + 0.09196(chronicity) ###############
################################################################################
##### Since the p-value for the slope 0.11744 > 0.05, we fail to reject the ####
####### null hypothesis, there is insufficient evidence that there is a ########
## linear relationship between chronicity and baseline headache severity score #
################################################################################

##Chronicity vs pk5
acu %>% 
  ggplot(aes(chronicity, pk5)) +
  geom_point()+
  theme_bw()+
  labs(title = 'Number of Years with Headache Disorder vs Headache Severity Score: 1 Year',
       subtitle = 'correlation = 0.133', x = 'Years with Disorder', y = 'Severity Score: 1 Year')+
  theme(plot.title = element_text(size = 25, face = 'bold'),
        plot.subtitle = element_text(size = 13, face = 'bold.italic'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'slategray'),
        panel.background = element_rect(fill = 'slategray2'),
        axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
        axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
        axis.title.x = element_text(face = 'bold', size = 13, color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 13, color = 'black'))

cor.test(acu$chronicity, acu$pk5)
fit = lm(acu$pk5~acu$chronicity)
summary(fit)

################################################################################
################################################################################
##### The correlation, 0.13335, shows a weak, positive, linear relationship ####
####### between the chronicity  and the one year headache severity score #######
################################################################################
############ LSRL: output of pk5 = 15.8598 + 0.12492(chronicity) ###############
################################################################################
###### Since the p-value for the slope 0.02066 < 0.05, we reject the null ######
###### hypothesis, there is insufficient evidence that there is a linear #######
### relationship between chronicity and the one year headache severity score ###
################################################################################

##Chronicity vs f1
acu %>% 
  ggplot(aes(chronicity, f1)) +
  geom_point()+
  theme_bw()+
  labs(title = 'Number of Years with Headache Disorder vs Frequency of Headaches: Baseline',
       subtitle = 'correlation = 0.0318', x = 'Years with Disorder', y = 'Headache Frequency: Baseline')+
  theme(plot.title = element_text(size = 25, face = 'bold'),
        plot.subtitle = element_text(size = 13, face = 'bold.italic'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'slategray'),
        panel.background = element_rect(fill = 'slategray2'),
        axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
        axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
        axis.title.x = element_text(face = 'bold', size = 13, color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 13, color = 'black'))

cor.test(acu$chronicity, acu$f1)
fit = lm(acu$f1~acu$chronicity)
summary(fit)

################################################################################
################################################################################
### The correlation, 0.0318, shows a very weak, positive, linear relationship ##
######### between the chronicity  and the baseline headache frequency ##########
################################################################################
############ LSRL: output of f1 = 15.84771 + 0.01550(chronicity) ###############
################################################################################
##### Since the p-value for the slope 0.5249 > 0.05, we fail to reject the #####
####### null hypothesis, there is insufficient evidence that there is a ########
#### linear relationship between chronicity and baseline headache frequency ####
################################################################################

##Chronicity vs f5
acu %>% 
  ggplot(aes(chronicity, f5)) +
  geom_point()+
  theme_bw()+
  labs(title = 'Number of Years with Headache Disorder vs Frequency of Headaches: 1 Year',
       subtitle = 'correlation = 0.0894', x = 'Years with Disorder', y = 'Headache Frequency: 1 Year')+
  theme(plot.title = element_text(size = 25, face = 'bold'),
        plot.subtitle = element_text(size = 13, face = 'bold.italic'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'slategray'),
        panel.background = element_rect(fill = 'slategray2'),
        axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
        axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
        axis.title.x = element_text(face = 'bold', size = 13, color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 13, color = 'black'))

cor.test(acu$chronicity, acu$f5)
fit = lm(acu$f5~acu$chronicity)
summary(fit)
################################################################################
################################################################################
##### The correlation, 0.08939, shows a weak, positive, linear relationship ####
######### between the chronicity  and the one year headache frequency ##########
################################################################################
############# LSRL: output of f5 = 8.14022 + 0.05491(chronicity) ###############
################################################################################
##### Since the p-value for the slope 0.07377 > 0.05, we fail to reject to #####
####### reject the null hypothesis, there is insufficient evidence that ########
######## there is a linear relationship between chronicity and one year ########
############################ headache frequency ################################
################################################################################

##Chronicity vs Age
acu %>% 
  ggplot(aes(chronicity, age)) +
  geom_point()+
  theme_bw()+
  labs(title = 'Number of Years with Headache Disorder vs Age of Subject (In Years)',
       subtitle = 'correlation = 0.488', x = 'Years with Disorder', y = 'Age of Subject (In Years)')+
  theme(plot.title = element_text(size = 25, face = 'bold'),
        plot.subtitle = element_text(size = 13, face = 'bold.italic'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'slategray'),
        panel.background = element_rect(fill = 'slategray2'),
        axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
        axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
        axis.title.x = element_text(face = 'bold', size = 13, color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 13, color = 'black'))

cor.test(acu$chronicity, acu$age)
fit = lm(acu$age~acu$chronicity)
summary(fit)

################################################################################
################################################################################
####### The correlation, 0.4875809, shows a very weak, positive, linear ########
####### relationship between the chronicity  and the age of the subject ########
################################################################################
############ LSRL: output of age = 37.1401 + 0.39138(chronicity) ###############
################################################################################
######## Since the p-value for the slope 0.00 < 0.05, we reject the null #######
####### hypothesis, there is sufficient evidence that there is a linear ########
########## relationship between chronicity and the age of the subject ##########
################################################################################

##Chronicity vs Chronicity
acu %>% 
  ggplot(aes(chronicity, chronicity)) +
  geom_point()+
  theme_bw()+
  labs(title = 'Number of Years with Headache Disorder vs Number of Years with Headache Disorder',
       subtitle = 'correlation = 1.0', x = 'Years with Disorder', y = 'Years with Disorder')+
  theme(plot.title = element_text(size = 25, face = 'bold'),
        plot.subtitle = element_text(size = 13, face = 'bold.italic'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'slategray'),
        panel.background = element_rect(fill = 'slategray2'),
        axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
        axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
        axis.title.x = element_text(face = 'bold', size = 13, color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 13, color = 'black'))

cor.test(acu$chronicity, acu$chronicity)
fit = lm(acu$chronicity~acu$chronicity)
summary(fit)

################################################################################
################################################################################
### The correlation, 1.0, shows a very strong, positive, linear relationship ###
########## between the chronicity  and the chronicity of the subject ###########
################################################################################
####### Since the p-value for the slope 0.00 < 0.05, we  reject the null #######
###### hypothesis, there is insufficient evidence that there is a linear #######
######## relationship between chronicity and chronicity of the subject #########
################################################################################
################################################################################

## pk5 and group
acu %>%
  ggplot(aes(group, pk5))+
  geom_point() +theme_bw()+
  labs(title = 'Severity Score of Headaches After 1 Year by Treatment and Non-Treatment Group',
       subtitle = 'correlation = -0.19512', x = 'Groups', y = 'Headache Severity Score')+
  theme(plot.title = element_text(size = 25, face = 'bold'),
        plot.subtitle = element_text(size = 13, face = 'bold.italic'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'slategray'),
        panel.background = element_rect(fill = 'slategray2'),
        axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
        axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
        axis.title.x = element_text(face = 'bold', size = 13, color = 'black'),
        axis.title.y = element_text(face = 'bold', size = 13, color = 'black'))

fit = lm(acu$pk5~acu$group)
summary(fit)
cor.test(acu$group, acu$pk5)

################################################################################
################################################################################
###### The scatterplot is not really informative since we are comparing a ######
########## quantitative value of pk5 to a categorical variable group ###########
################################################################################
##################### LSRL pk5 = 22.343 - 6.097(group) #########################
################################################################################
####### Since the p-value, 0.00066 < 0.05, we reject the null hypothesis #######
###### there is sufficient evidence that severity of headache score after ######
######## one year and treatment/control group has a linear relationship ########
################################################################################