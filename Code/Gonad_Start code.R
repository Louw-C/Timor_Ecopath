library(readxl)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(car)
library(tidyverse)
library(wesanderson)


#Gonad data
#Load data
PNMS_Gonad<-read.csv(file.choose(),header=T,sep=",")
names(PNMS_Gonad)
PNMS_Gonad<-subset(PNMS_Gonad, Fork_length_cm!="NA")
PNMS_Gonad<-subset(PNMS_Gonad, Sex!="M")

#summary statistics
# run the script called "SummarySE()_functon.R" then run the code below to get your mean + standard errors
library(plyr)
library(Rmisc)

#Fork length
data_sum_Length <- summarySE(PNMS_Gonad, measurevar="Fork_length_cm", groupvars=c("Sex","Species"))
data_sum_Length 
#Weight
data_sum_Weight<- summarySE(PNMS_Gonad, measurevar="Fish_weight_kg", groupvars=c("Sex","Species"))
data_sum_Weight


#Make basic figures

require(Rmisc)
require(ggplot2)
PNMS_Gonad$Month <- factor(PNMS_Gonad$Month, levels = c("January", "February", "March",
                                                        "September","October","November","December"))
#Tuna length
Gonad_Length<-summarySE(PNMS_Gonad, measurevar="Fork_length_cm", groupvars=c("Month","Species"))
Gonad1<-ggplot(Gonad_Length, aes(x=factor(Month), y=Fork_length_cm,fill=factor(Species)))+
  geom_col(position=position_dodge(0.9))+
  scale_fill_manual(values = wes_palette("Zissou1", 2))+
  labs(y = "Tuna fork length (cm)")+
  geom_errorbar(aes(ymin=Fork_length_cm-se, ymax=Fork_length_cm+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11,angle=90, vjust=0.3),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))

Gonad1

GonadBox<-ggplot(PNMS_Gonad, aes(x=factor(Month), y=Fork_length_cm,fill=factor(Species)))+
  geom_boxplot(position=position_dodge(0.9))+
  scale_fill_manual(values = wes_palette("Moonrise2", 2))+
  labs(y = "Tuna fork length (cm)")+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, vjust=0.3),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))

GonadBox


#Tuna weight
Gonad_weight<-summarySE(PNMS_Gonad, measurevar="Fish_weight_kg", groupvars=c("Month","Species"))
Gonad2<-ggplot(Gonad_weight, aes(x=factor(Month), y=Fish_weight_kg,fill=factor(Species)))+
  geom_col(position=position_dodge(0.9))+
  scale_fill_manual(values = wes_palette("Moonrise2", 2))+
  labs(y = "Tuna weight (kg)")+
  geom_errorbar(aes(ymin=Fish_weight_kg-se, ymax=Fish_weight_kg+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11,angle=90, vjust=0.3),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))

Gonad2

GonadBox2<-ggplot(PNMS_Gonad, aes(x=factor(Month), y=Fish_weight_kg,fill=factor(Species)))+
  geom_boxplot(position=position_dodge(0.9))+
  scale_fill_manual(values = wes_palette("Moonrise2", 2))+
  labs(y = "Tuna weight (kg)") +
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, vjust=0.3),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))

GonadBox2


##########Look at overal tuna data from BOFI
PNMS_BOFI<-read.csv(file.choose(),header=T,sep=",")
names(PNMS_BOFI)

###Plot numbers of tuna per month for BOFI

require(Rmisc)
require(ggplot2)

PNMS_BOFI$Month <- factor(PNMS_BOFI$Month, levels = c("January", "February", "March", "April","May","June",
                                                      "July", "August","September","October","November","December"))
#Tuna Abundance
BOFI_Number<-summarySE(PNMS_BOFI, measurevar="Total_number", groupvars=c("Month","Year"))
BOFI1<-ggplot(BOFI_Number, aes(x=factor(Month), y=Total_number,fill=factor(Year)))+
  geom_col(position=position_dodge(0.9))+
  scale_fill_manual(values = wes_palette("Zissou1", 3))+
  labs(y = "Average number of fish caught per month")+
  geom_errorbar(aes(ymin=Total_number-se, ymax=Total_number+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),legend.text = element_text(size=11),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11,angle=90, vjust=0.3),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))

BOFI1

#Tuna weight
BOFI_Weight<-summarySE(PNMS_BOFI, measurevar="Total.weight_kg", groupvars=c("Month","Year"))
BOFI2<-ggplot(BOFI_Weight, aes(x=factor(Month), y=Total.weight_kg,fill=factor(Year)))+
  geom_col(position=position_dodge(0.9))+
  scale_fill_manual(values = wes_palette("Zissou1", 3))+
  labs(y = "Average number of fish caught per month")+
  geom_errorbar(aes(ymin=Total.weight_kg-se, ymax=Total.weight_kg+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),legend.text = element_text(size=11) ,panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11,angle=90, vjust=0.3),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))

BOFI2


#Average GSI across time/species
Gonad_GSI<-summarySE(PNMS_Gonad, measurevar="GSI", groupvars=c("Month", "Species", "Sex"))
Gonad3<-ggplot(Gonad_GSI, aes(x=factor(Month), y=GSI,fill=factor(Sex)))+
  facet_grid(Species~.)+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Mean GSI")+
  scale_fill_manual(values=c("#66a182","#2e4057"))+
  geom_errorbar(aes(ymin=GSI-se, ymax=GSI+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),legend.text = element_text(size=15),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=15),axis.title.x=element_blank(), strip.text.y = element_text(size = 15,face="italic"),
        axis.text.y= element_text(size=13), axis.title.y= element_text(size=15))
Gonad3
Gonad_GSI$Month <- factor(Gonad_GSI$Month, levels = c( "September","October", "November","December","January","February", "March"))


#Plot fish weight and gonad weight (g)
plot(PNMS_Gonad$Gonad_weight_g~PNMS_Gonad$Total_weight_g)


#Separate YFT and BET

YFT<-subset(PNMS_Gonad, Species_Common!="Big Eye")
BET<-subset(PNMS_Gonad, Species_Common!="Yellowfin Tuna")

#Plot a histogram for each - sizes

YFT_Sizes<-ggplot(YFT, aes(x = FL_cm, fill=Sex)) +
  geom_histogram(position = "identity", alpha = 0.4, binwidth=5)

YFT_Sizes

BET_Sizes<-ggplot(BET, aes(x = FL_cm, fill=Sex)) +
  geom_histogram(position = "identity", alpha = 0.4, binwidth=5)

BET_Sizes


#Look at GSI per species
YFT_GSI<-summarySE(YFT, measurevar="GSI", groupvars=c("Month", "Sex"))
YFT_Plot<-ggplot(YFT_GSI, aes(x=factor(Month), y=GSI,fill=factor(Sex)))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Average YFT GSI per month")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  geom_errorbar(aes(ymin=GSI-se, ymax=GSI+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
YFT_Plot
YFT_GSI$Month <- factor(YFT_GSI$Month , levels = c( "September","October", "November","December","January","February", "March"))


BET_GSI<-summarySE(BET, measurevar="GSI", groupvars=c("Month", "Sex"))
BET_Plot<-ggplot(BET_GSI, aes(x=factor(Month), y=GSI,fill=factor(Sex)))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Average BET GSI per month")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  geom_errorbar(aes(ymin=GSI-se, ymax=GSI+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
BET_Plot
BET_GSI$Month <- factor(BET_GSI$Month , levels = c( "September","October", "November","December","January","February", "March"))


