#Alex fishbiomass data from 2016 and 2018

#Upload the 2016 working data

#Load data
Alex_Fish_16<-read.csv(file.choose(),header=T,sep=",")
names(Alex_Fish_16) 

#Only keep data from Viqueque
Alex_Viq_16<-subset(Alex_Fish_16, Location=="Adarai")
#Take out outliers - anything larger than 20kg
Alex_Viq_16<-subset(Alex_Viq_16, Total.Biomass_kg <= 20)
#To keep only Kyphosidae
Alex_Viq_16<-subset(Alex_Viq_16, Family =="Kyphosidae")
#To keep only Lutjanidae
Alex_Viq_16<-subset(Alex_Viq_16, Family =="Lutjanidae")
#To keep only Siganidae
Alex_Viq_16<-subset(Alex_Viq_16, Family =="Siganidae")
#To keep only Acanthuridae
Alex_Viq_16<-subset(Alex_Viq_16, Family =="Acanthuridae")

#We need to aggregate the data to transect level
Viq_16_Transect<-Alex_Viq_16 %>%
  group_by(Location_Site_Transect, Site,Transect, Family, Species) %>%
  summarise(transect.sum.Biomass=(sum(Total.Biomass_kg)*1000/250))
#Took the total biomass in kg and multiplied it by 1000 to get grams
#Then divided it by 250 (assuming the transect was 5*50m so 250m2) - to get grams/m2


#########If you want to subset data use the following code##########
#Exclude something
Subset_Data_Name<-subset(Data, Variable_Name_You_want_To_Leave_out!="Term you want to leave out")
#Only keep something
Subset_Data_Name<-subset(Data, Variable_Name_Keep=="Term you want to keep")

#Want to look at the contribution of different families
All_Check<-ggplot(Viq_16_Transect, aes(transect.sum.Biomass, Family))+
  geom_boxplot(position=position_dodge(0.9), color="Sea Green")+geom_jitter(colour="lightblue")+
  labs(y = "Fish families", x = "Biomass as g/m2")+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, vjust=0.3),axis.title.x=element_text(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))
All_Check

#Want to look at the contribution of different species
Species_Check<-ggplot(Viq_16_Transect, aes(transect.sum.Biomass, Species))+
  geom_boxplot(position=position_dodge(0.9), color="Sea Green")+geom_jitter(colour="lightblue")+
  labs(y = "Fish species", x = "Biomass as g/m2")+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, vjust=0.3),axis.title.x=element_text(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))
Species_Check


#Want to look at the contribution of different species of Lutjanidae
Lutjanidae_Check<-ggplot(Viq_16_Transect, aes(transect.sum.Biomass, Species))+
  geom_boxplot(position=position_dodge(0.9), color="Sea Green")+geom_jitter(colour="lightblue")+
  labs(y = "Lutjanidae", x = "Biomass as g/m2")+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, vjust=0.3),axis.title.x=element_text(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))
Lutjanidae_Check

#Want to look at the contribution of different species of Kyphosidae
Kyphosidae_Check<-ggplot(Viq_16_Transect, aes(transect.sum.Biomass, Species))+
  geom_boxplot(position=position_dodge(0.9), color="Sea Green")+geom_jitter(colour="lightblue")+
  labs(y = "Kyphosidae", x = "Biomass as g/m2")+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, vjust=0.3),axis.title.x=element_text(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))
Kyphosidae_Check
##ONLY ONE SPECIES OF Kyphosidae

#Want to look at the contribution of different species of Synganidae
Synganidae_Check<-ggplot(Viq_16_Transect, aes(transect.sum.Biomass, Species))+
  geom_boxplot(position=position_dodge(0.9), color="Sea Green")+geom_jitter(colour="lightblue")+
  labs(y = "Siganidae", x = "Biomass as g/m2")+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, vjust=0.3),axis.title.x=element_text(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))
Syganidae_Check

#Want to look at the contribution of different species of Acanthuridae
Acanthuridae_Check<-ggplot(Viq_16_Transect, aes(transect.sum.Biomass, Species))+
  geom_boxplot(position=position_dodge(0.9), color="Sea Green")+geom_jitter(colour="lightblue")+
  labs(y = "Acanthuridae", x = "Biomass as g/m2")+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, vjust=0.3),axis.title.x=element_text(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))
Acanthuridae_Check
