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
  geom_boxplot(position=position_dodge(0.9), color="Sea Green")+geom_jitter(colour="lightblue")+
  labs(y = "Peskas Groups")+
  theme(legend.title = element_blank(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, vjust=0.3),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11), axis.title.y= element_text(size=11))
TL_Check

#Check Snappers and seaperches only
Snappers<-subset(Timor_Fish, Peskas_Group=="Snappers and seaperches")

#Check for range in TL and connection with the estuary
Snappers_TL<-ggplot(Snappers, aes(Trophic_Level,Binomial, color=Estuarine_connection))+
  geom_jitter(position=position_dodge(0.9), size=3)+
  labs(y = "Species")+
  ggtitle("Snappers and seaperches trophic levels")+
  theme(legend.title = element_text(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, face="italic"), axis.title.y= element_text(size=11))
Snappers_TL

#Check for range in length and connection with the estuary
Snappers_Length<-ggplot(Snappers, aes(Common_length_cm,Binomial, color=Estuarine_connection))+
  geom_jitter(position=position_dodge(0.9), size=3)+
  labs(y = "Species")+
  ggtitle("Snappers and seaperches common length (cm)")+
  theme(legend.title = element_text(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, face="italic"), axis.title.y= element_text(size=11))
Snappers_Length


#Check for range in depth and connection with the estuary
Snappers_Depth<-ggplot(Snappers, aes(Common_depth_range_m,Binomial, color=Estuarine_connection))+
  geom_jitter(position=position_dodge(0.9), size=3)+
  labs(y = "Species")+
  ggtitle("Snappers and seaperches Common depth range (m)")+
  theme(legend.title = element_text(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, angle=90),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, face="italic"), axis.title.y= element_text(size=11))
Snappers_Depth


#Plot the different species versus Food 1
Snappers_Food1<-ggplot(Snappers, aes(Trophic_Level,Binomial, color=Food.1))+
  geom_jitter(position=position_dodge(0.9), size=3)+
  labs(y = "Species")+
  ggtitle("Snappers and seaperches trophic levels")+
  theme(legend.title = element_text(),legend.text = element_text(size=11),legend.position = "top", panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, face="italic"), axis.title.y= element_text(size=11))
Snappers_Food1

#Check Mackerel scad only
Mackerel_scad<-subset(Timor_Fish, Peskas_Group=="Mackerel scad")

Mackerel_scad_TL<-ggplot(Mackerel_scad, aes(Trophic_Level,Binomial))+
  geom_jitter(position=position_dodge(0.9), size=3, color="Light Blue")+
  labs(y = "Species")+
  ggtitle("Mackerel scad trophic levels")+
  theme(legend.title = element_text(),legend.text = element_text(size=11), panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, face="italic"), axis.title.y= element_text(size=11))
Mackerel_scad_TL

#Plot Mackerel_Scad_Food 1
Mackerel_scad_Food1<-ggplot(Mackerel_scad, aes(Trophic_Level,Binomial,color=Food.1))+
  geom_jitter(position=position_dodge(0.9), size=3)+
  labs(y = "Species")+
  ggtitle("Mackerel scad food 1")+
  theme(legend.title = element_text(),legend.text = element_text(size=11),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, face="italic"), axis.title.y= element_text(size=11))
Mackerel_scad_Food1

#Check Jacks and trevallies only
Jacks_trevallies<-subset(Timor_Fish, Peskas_Group=="Jacks and trevallies")

Jacks_trevallies_TL<-ggplot(Jacks_trevallies, aes(Trophic_Level,Binomial, color=Estuarine_Connection))+
  geom_jitter(position=position_dodge(0.9), size=3)+
  labs(y = "Species")+
  ggtitle("Jacks and trevallies trophic levels")+
  theme(legend.title = element_text(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, face="italic"), axis.title.y= element_text(size=11))
Jacks_trevallies_TL


#Plot Jacks_Trevallies_Food 1
Jacks_trevallies_Food1<-ggplot(Jacks_trevallies, aes(Trophic_Level,Binomial,color=Food.1))+
  geom_jitter(position=position_dodge(0.9), size=3)+
  labs(y = "Species")+
  ggtitle("Jacks and Trevallies food 1")+
  theme(legend.title = element_text(),legend.text = element_text(size=11), legend.position = "top",panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, face="italic"), axis.title.y= element_text(size=11))
Jacks_trevallies_Food1


#Check Sardines and Herring
Sardines_herring<-subset(Timor_Fish, Peskas_Group=="Sardines and Herring")

Sardines_herring_TL<-ggplot(Sardines_herring, aes(Trophic_Level,Binomial, color=Estuarine_connection))+
  geom_jitter(position=position_dodge(0.9), size=3)+
  labs(y = "Species")+
  ggtitle("Sardines and herring trophic levels")+
  theme(legend.title = element_text(),legend.text = element_text(size=11, face="italic"),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, face="italic"), axis.title.y= element_text(size=11))
Sardines_herring_TL

#Check food 1
Sardines_herring_Food1<-ggplot(Sardines_herring, aes(Trophic_Level,Binomial,color=Food.1))+
  geom_jitter(position=position_dodge(0.9), size=3)+
  labs(y = "Species")+
  ggtitle("Sardines and herring Food 1")+
  theme(legend.title = element_text(),legend.text = element_text(size=11), legend.position = "top",panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, face="italic"), axis.title.y= element_text(size=11))
Sardines_herring_Food1
