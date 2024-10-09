#UNTL Code

#Upload data
library(readxl)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(car)
library(tidyverse)
library(wesanderson)
test<-read.csv(file.choose(),header=T,sep=",")
test


Test1<-ggplot(test, aes(Site, Density_m2,))+
  geom_boxplot(position=position_dodge(0.9), color="Sea Green")+geom_jitter(colour="lightblue")+
  stat_summary(fun.y = "mean", colour="darkblue", size = 1, shape="diamond")+
  labs(y = "Snapper density (m2)")+
  theme(legend.title = element_blank(),legend.text = element_text(size=13, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=13, vjust=0.3),axis.title.x=element_blank(),
        axis.text.y= element_text(size=13), axis.title.y= element_text(size=13))
Test1

summarise(group_by(test, Site),mean=mean(Density_m2), sd=sd(Density_m2))

Test2<-ggplot(test, aes(Moon_Phase, Density_m2,))+
  geom_boxplot(position=position_dodge(0.9), color="Sea Green")+geom_jitter(colour="lightblue")+
  stat_summary(fun.y = "mean", colour="darkblue", size = 1, shape="diamond")+
  labs(y = "Snapper density (m2)")+
  theme(legend.title = element_blank(),legend.text = element_text(size=13, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=13, vjust=0.3),axis.title.x=element_blank(),
        axis.text.y= element_text(size=13), axis.title.y= element_text(size=13))
Test2

