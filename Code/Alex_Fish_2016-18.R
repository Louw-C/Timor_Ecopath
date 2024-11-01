#Alex fishbiomass data from 2016 and 2018

#Upload the 2016 working data

#Load data
Alex_Fish_16<-read.csv(file.choose(),header=T,sep=",")
names(Alex_Fish_16) 

#Only keep data from Viqueque
Alex_Viq_16<-subset(Alex_Fish_16, Location=="Adarai")
Alex_Viq_16<-subset(Alex_Viq_16, Family =="Lutjanidae")


#########If you want to subset data use the following code##########
#Exclude something
Subset_Data_Name<-subset(Data, Variable_Name_You_want_To_Leave_out!="Term you want to leave out")
#Only keep something
Subset_Data_Name<-subset(Data, Variable_Name_Keep=="Term you want to keep")