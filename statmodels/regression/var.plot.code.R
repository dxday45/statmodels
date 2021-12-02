#create a regression diagram
library(tidyverse)
library(ggtext)

ex.data= data.frame(x = c(1,3,6,8,12,15),
                    y = c(3,7,8,16,13,16))
ex.data.mod=lm(y~x,data=ex.data)
ex.data$pred = predict(ex.data.mod)

ggplot(ex.data,aes(x,y)) +
  geom_point(aes(col="Actual\nValue"),size=3)+
  geom_smooth(method="lm",se=F,col="black")+
  geom_hline(yintercept=mean(ex.data$y),col="black",lty=2)+
  geom_point(aes(x=x,y=pred,col="Predicted\nValue"),size=3)+
  geom_point(aes(x=6,y=mean(y),col="mean y"),size=3)+
  geom_point(aes(x=12,y=mean(y),col="mean y"),size=3)+
  #geom_segment(aes(x=7,y=13,xend=7.7,yend=13),
               #arrow=arrow(),size=.25)+
  geom_segment(data=ex.data[4,],aes(x=x,y=pred,xend=x,yend=y),
               size=.5,lty=2)+
  geom_segment(data=ex.data[3,],aes(x=x,y=pred,xend=x,yend=mean(ex.data$y)),
               size=.5,lty=2)+
  geom_segment(data=ex.data[5,],aes(x=x,y=y,xend=x,yend=mean(ex.data$y)),
               size=.5,lty=2)+
  #geom_label(aes(x=5,y=8,label="Actual\nValue Y"))+
  #annotate(geom="text",x=6.9,y=13,label=expression(hat(y)[i] == beta[0] + paste(beta[1],x[i])),
           #hjust="right",size=8)+
  annotate(geom="text",x=6.5,y=15.5,label=expression(y-hat(y)),
           hjust="right",size=7)+
  annotate(geom="curve",x=6,y=15,xend=7.7,yend=13.5,
           curvature=.3,arrow=arrow(), size=.5)+
  annotate(geom="text",x=14.1,y=13.5,label=expression(y-bar(y)),
           hjust="right",size=7)+
  annotate(geom="curve",x=13.5,y=13,xend=12.3,yend=11.7,
           curvature=-.3,arrow=arrow(),size=.5)+
  annotate(geom="text",x=4.5,y=12,
           label=expression(hat(y)-bar(y)),
           hjust="right",size=7)+
  annotate(geom="curve",x=4,y=11.5,xend=5.8,yend=9.8,
           curvature=.3,arrow=arrow(),size=.5)+
  scale_color_manual(name="Points",
                     breaks=c("Actual\nValue","Predicted\nValue","mean y"),
                     values=c("Actual\nValue"="blue","Predicted\nValue"="red",
                              "mean y"="black"),
                     labels=c(expression(paste("Actual y")),
                              expression(paste("Predicted ",hat(y))),
                              expression(paste("Mean ",bar(y)))
                              )
                     )+
  labs(title="Variance Components for Regression",
       subtitle=expression(paste("Variance: ", sum((y[i]-bar(y))^2,i==1,n) == 
                                   sum((y[i]-hat(y)[i])^2,i==1,n) - 
                                   sum((hat(y)[i]-bar(y))^2,i==1,n))),
       x="Independent Variable x",
       y="Dependent Variable y")+
  theme_bw()+
  theme(legend.title=element_text(hjust=.5),
        legend.text=element_text(size=10),
        legend.background = element_rect(color="black"),
        legend.box.just = "left",
        legend.position=c(.8,.2),
        axis.text=element_blank(),
        plot.title=element_text(hjust=.5,size=16),
        plot.subtitle=element_text(hjust=.5,size=12))
        
        
