#Exploring fish species and tropic level ranges within different Peskas groups

#Load data
Timor_Fish<-read.csv(file.choose(),header=T,sep=",")
names(Timor_Fish)

#########If you want to subset data use the following code##########
#Exclude something
Subset_Data_Name<-subset(Data, Variable_Name_You_want_To_Leave_out!="Term you want to leave out")
#Only keep something
Subset_Data_Name<-subset(Data, Variable_Name_Keep=="Term you want to keep")

#Check different groups and TL ranges for each

TL_Check<-ggplot(Timor_Fish, aes(Trophic_Level, Peskas_Group))+
  geom_boxplot(position=position_dodge(0.9), color="Sea Green")+
  labs(y = "Peskas Groups")+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, vjust=0.3),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))
TL_Check

#Check Snappers and seaperches only
Snappers<-subset(Timor_Fish, Peskas_Group=="Snappers and seaperches")

Snappers_TL<-ggplot(Snappers, aes(Trophic_Level,Binomial, color=Estuarine_Connection))+
  geom_jitter(position=position_dodge(0.9), size=3)+
  labs(y = "Species")+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, face="italic"), axis.title.y= element_text(size=11))
Snappers_TL

#Check Mackerel scad only
Mackerel_scad<-subset(Timor_Fish, Peskas_Group=="Mackerel scad")

Mackerel_scad_TL<-ggplot(Mackerel_scad, aes(Trophic_Level,Binomial))+
  geom_jitter(position=position_dodge(0.9), size=3, color="Light Blue")+
  labs(y = "Species")+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, face="italic"), axis.title.y= element_text(size=11))
Mackerel_scad_TL

#Check Jacks and trevallies only
Jacks_trevallies<-subset(Timor_Fish, Peskas_Group=="Jacks and trevallies")

Jacks_trevallies_TL<-ggplot(Jacks_trevallies, aes(Trophic_Level,Binomial, color=Estuarine_Connection))+
  geom_jitter(position=position_dodge(0.9), size=3)+
  labs(y = "Species")+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, face="italic"), axis.title.y= element_text(size=11))
Jacks_trevallies_TL
