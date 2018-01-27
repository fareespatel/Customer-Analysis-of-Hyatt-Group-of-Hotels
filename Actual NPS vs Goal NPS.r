# Read the raw dataset: february 2010 - january 2011, and record the loading time.
inputFilePath <- "C:\\Users\\Amol\\Desktop\\Syracuse\\IST 687\\Project Hyatt\\final.csv"
beginTime <- Sys.time()
hotelData <- read.csv(inputFilePath, header=TRUE, sep=",")
hotelData <- data.frame(hotelData)
endTime <- Sys.time()
loadTime <- endTime-beginTime

library(plyr)
#Find the unique countries being considered and the count of each NPS type for them
Unique_Ct<-unique(hotelData$Country_PL)
freq<-count(hotelData,"Country_PL")
Ctdata<-data.frame(table(hotelData$Country_PL, hotelData$NPS_Type ) [,] ) 

#Calculate nps score for each country
#Create different matrices for detractors and promoters
DTmatrix<-subset(Ctdata,Ctdata$Var2=="Detractor")

PTmatrix<-subset(Ctdata,Ctdata$Var2=="Promoter")
rownames(PTmatrix) <- 1:nrow(PTmatrix)

PSmatrix<-subset(Ctdata,Ctdata$Var2=="Passive")
rownames(PSmatrix) <- 1:nrow(PSmatrix)

FinalMT<-PTmatrix
FinalMT$NPS_Score<- 0
for(i in 1:56)
{
  FinalMT[i,4]<-(((PTmatrix[i,3] - DTmatrix[i,3])/(PTmatrix[i,3] + DTmatrix[i,3] + PSmatrix[i,3]))*100)
}

FinalMT$Var2<-NULL
FinalMT$Freq<-NULL
FinalMT

str(hotelData)
temp<-hotelData
Finaltemp<-FinalMT
merge(temp,Finaltemp,by.x='Country_PL',by.y ='Var1')
head(temp)
tail(temp)
temp$NPS_Score<-0
library(sqldf)
newdataset<- sqldf('Select t.*,mt.NPS_Score from temp t INNER JOIN FinalMT mt ON t.Country_PL=mt.Var1')
tail(newdataset)
str(newdataset)
newdataset[,24]<-NULL

write.csv(newdataset, "Final-NPS.csv")

library(ggplot2)
temp<-MergedFile
temp<-subset(temp, !temp$Likelihood_Recommend_H == "NA")
temp<-na.omit(temp$Likelihood_Recommend_H)
head(temp)
#Gives us an overall idea as to whether the customers would recommend the Brand Hyatt to others
hist(Final-NPS$NPS_Score)

ggplot(data=FinalNPS,aes(x=,y=value,group=ndata$variable, colour = ndata$variable)) + geom_tile(aes(fill = ndata$Day)) 
ggplot(aes(x=DTmatrix$Var1,y=MergedFile$Likelihood_Recommend_H, colour = MergedFile$Region_PL))+ geom_tile(aes(fill = MergedFile$POV_CODE_C))

ggplot(data=MergedFile,aes(x=Likelihood_Recommend_H,y=Region_PL)) + geom_tile(aes(fill = MergedFile$POV_CODE_C)) 

####################################################################################################
"All.Suites_PL", "Bell.Staff_PL","Boutique_PL", "Business.Center_PL","Casino_PL",
"Conference_PL", "Convention_PL","Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL",
"Fitness.Trainer_PL", "Golf_PL","Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL",
"Mini.Bar_PL", "Pool.Indoor_PL","Pool.Outdoor_PL", "Regency.Grand.Club_PL",
"Resort_PL","Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL"
#####################################################################################################

temp<-subset(MergedFile, select = c("All.Suites_PL", "Bell.Staff_PL","Boutique_PL", "Business.Center_PL","Casino_PL",
                                    "Conference_PL", "Convention_PL","Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL",
                                    "Fitness.Trainer_PL", "Golf_PL","Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL",
                                    "Mini.Bar_PL", "Pool.Indoor_PL","Pool.Outdoor_PL", "Regency.Grand.Club_PL",
                                    "Resort_PL","Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL","NPS_Type"))
tail(temp)
sum(temp$All.Suites_PL == 'Y' || temp$Bell.Staff_PL == 'Y' || temp$Boutique_PL == 'Y' || temp$Business.Center_PL == 'Y' || temp$Casino_PL == 'Y' || temp$Conference_PL == 'Y' || temp$Convention_PL == 'Y' || temp$Dry.Cleaning_PL == 'Y' || temp$Elevators_PL == 'Y' || temp$Fitness.Center_PL == 'Y' || temp$Fitness.Trainer_PL == 'Y' || temp$Golf_PL == 'Y' || temp$Indoor.Corridors_PL == 'Y' || temp$Laundry_PL == 'Y' || temp$Limo.Service_PL == 'Y' || temp$Mini.Bar_PL == 'Y' || temp$Pool.Indoor_PL == 'Y' || temp$Pool.Outdoor_PL == 'Y' || temp$Regency.Grand.Club_PL == 'Y' || temp$Resort_PL=='Y' || temp$Restaurant_PL == 'Y' || temp$Self.Parking_PL == 'Y' || temp$Shuttle.Service_PL == 'Y' || temp$Ski_PL == 'Y' || temp$Spa_PL == 'Y',na.rm = TRUE)
nrow(temp[temp$All.Suites_PL == 'N' & temp$Bell.Staff_PL == 'N',])

nrow(temp[temp$All.Suites_PL == 'Y' | temp$Bell.Staff_PL == 'Y' | temp$Boutique_PL == 'Y' | temp$Business.Center_PL == 'Y' | temp$Casino_PL == 'Y' | temp$Conference_PL == 'Y' | temp$Convention_PL == 'Y' | temp$Dry.Cleaning_PL == 'Y' | temp$Elevators_PL == 'Y' | temp$Fitness.Center_PL == 'Y' | temp$Fitness.Trainer_PL == 'Y' | temp$Golf_PL == 'Y' | temp$Indoor.Corridors_PL == 'Y' | temp$Laundry_PL == 'Y' | temp$Limo.Service_PL == 'Y' | temp$Mini.Bar_PL == 'Y' | temp$Pool.Indoor_PL == 'Y' | temp$Pool.Outdoor_PL == 'Y' | temp$Regency.Grand.Club_PL == 'Y' | temp$Resort_PL=='Y' | temp$Restaurant_PL == 'Y' | temp$Self.Parking_PL == 'Y' | temp$Shuttle.Service_PL == 'Y' | temp$Ski_PL == 'Y' | temp$Spa_PL == 'Y' & temp$NPS_Type == "Promoter",])

nrow(temp[temp$All.Suites_PL == 'N' & temp$Bell.Staff_PL == 'N' & temp$Boutique_PL == 'N' & temp$Business.Center_PL == 'N' & temp$Casino_PL == 'N' & temp$Conference_PL == 'N' & temp$Convention_PL == 'N' & temp$Dry.Cleaning_PL == 'N' & temp$Elevators_PL == 'N' & temp$Fitness.Center_PL == 'N' & temp$Fitness.Trainer_PL == 'N' & temp$Golf_PL == 'N' & temp$Indoor.Corridors_PL == 'N' & temp$Laundry_PL == 'N' & temp$Limo.Service_PL == 'N' & temp$Mini.Bar_PL == 'N' & temp$Pool.Indoor_PL == 'N' & temp$Pool.Outdoor_PL == 'N' & temp$Regency.Grand.Club_PL == 'N' & temp$Resort_PL=='N' & temp$Restaurant_PL == 'N' & temp$Self.Parking_PL == 'N' & temp$Shuttle.Service_PL == 'N' & temp$Ski_PL == 'N' & temp$Spa_PL == 'N',])

nrow(temp[temp$All.Suites_PL == 'N' & temp$Bell.Staff_PL == 'N' & temp$Boutique_PL == 'N' & temp$Business.Center_PL == 'N' & temp$Casino_PL == 'N' & temp$Conference_PL == 'N' & temp$Convention_PL == 'N' & temp$Dry.Cleaning_PL == 'N' & temp$Elevators_PL == 'N' & temp$Fitness.Center_PL == 'N' & temp$Fitness.Trainer_PL == 'N' & temp$Golf_PL == 'N' & temp$Indoor.Corridors_PL == 'N' & temp$Laundry_PL == 'N' & temp$Limo.Service_PL == 'N' & temp$Mini.Bar_PL == 'N' & temp$Pool.Indoor_PL == 'N' & temp$Pool.Outdoor_PL == 'N' & temp$Regency.Grand.Club_PL == 'N' & temp$Resort_PL=='N' & temp$Restaurant_PL == 'N' & temp$Self.Parking_PL == 'N' & temp$Shuttle.Service_PL == 'N' & temp$Ski_PL == 'N' & temp$Spa_PL == 'N',])


nrow(temp[temp$All.Suites_PL == 'N' | temp$Bell.Staff_PL == 'N' | temp$Boutique_PL == 'N' | temp$Business.Center_PL == 'N' | temp$Casino_PL == 'N' | temp$Conference_PL == 'N' | temp$Convention_PL == 'N' | temp$Dry.Cleaning_PL == 'N' | temp$Elevators_PL == 'N' | temp$Fitness.Center_PL == 'N' | temp$Fitness.Trainer_PL == 'N' | temp$Golf_PL == 'N' | temp$Indoor.Corridors_PL == 'N' | temp$Laundry_PL == 'N' | temp$Limo.Service_PL == 'N' | temp$Mini.Bar_PL == 'N' | temp$Pool.Indoor_PL == 'N' | temp$Pool.Outdoor_PL == 'N' | temp$Regency.Grand.Club_PL == 'N' | temp$Resort_PL=='N' | temp$Restaurant_PL == 'N' | temp$Self.Parking_PL == 'N' | temp$Shuttle.Service_PL == 'N' | temp$Ski_PL == 'N' | temp$Spa_PL == 'N' & temp$NPS_Type == 'Detractor',])

length(temp$All.Suites_PL)

sum(temp$All.Suites_PL == 'Y' | temp$Bell.Staff_PL == 'Y' | temp$Boutique_PL == 'Y' | temp$Business.Center_PL == 'Y' | temp$Casino_PL == 'Y' | temp$Conference_PL == 'Y' | temp$Convention_PL == 'Y' | temp$Dry.Cleaning_PL == 'Y' | temp$Elevators_PL == 'Y' | temp$Fitness.Center_PL == 'Y' | temp$Fitness.Trainer_PL == 'Y' | temp$Golf_PL == 'Y' | temp$Indoor.Corridors_PL == 'Y' | temp$Laundry_PL == 'Y' | temp$Limo.Service_PL == 'Y' | temp$Mini.Bar_PL == 'Y' | temp$Pool.Indoor_PL == 'Y' | temp$Pool.Outdoor_PL == 'Y' | temp$Regency.Grand.Club_PL == 'Y' | temp$Resort_PL=='Y' | temp$Restaurant_PL == 'Y' | temp$Self.Parking_PL == 'Y' | temp$Shuttle.Service_PL == 'Y' | temp$Ski_PL == 'Y' | temp$Spa_PL == 'Y' & temp$NPS_Type == "Promoter", na.rm=TRUE)
library(plyr)
tempFile<-temp[1:1000,]
C<-0
for(i in 1:1000)
{
  if(temp$All.Suites_PL == "N" & MergedFile$Bell.Staff_PL == "N" & MergedFile$Boutique_PL == "N" & MergedFile$Business.Center_PL == "N" & MergedFile$Casino_PL == "N" & MergedFile$Conference_PL == "N" & MergedFile$Convention_PL == "N" & MergedFile$Dry.Cleaning_PL == "N" & MergedFile$Elevators_PL == "N" & MergedFile$Fitness.Center_PL == "N" & MergedFile$Fitness.Trainer_PL == "N" & MergedFile$Golf_PL == "N" & MergedFile$Indoor.Corridors_PL == "N" & MergedFile$Laundry_PL == "N" & MergedFile$Limo.Service_PL == "N" & MergedFile$Mini.Bar_PL == "N" & MergedFile$Pool.Indoor_PL == "N" & MergedFile$Pool.Outdoor_PL == "N" & MergedFile$Regency.Grand.Club_PL == "N" & MergedFile$Resort_PL=="N" & MergedFile$Restaurant_PL == "N" & MergedFile$Self.Parking_PL == "N" & MergedFile$Shuttle.Service_PL == "N" & MergedFile$Ski_PL == "N" & MergedFile$Spa_PL == "N")
  {
    C<-C+1
  }
}
C
count(tempFile,"All.Suites_PL"=="Y")
if(MergedFile$All.Suites_PL == "Y" | MergedFile$Bell.Staff_PL == "Y" | MergedFile$Boutique_PL == "Y" | MergedFile$Business.Center_PL == "Y" | MergedFile$Casino_PL == "Y" | MergedFile$Conference_PL == "Y" || MergedFile$Convention_PL == "Y" || MergedFile$Dry.Cleaning_PL == "Y" || MergedFile$Elevators_PL == "Y" || MergedFile$Fitness.Center_PL == "Y" || MergedFile$Fitness.Trainer_PL == "Y" || MergedFile$Golf_PL == "Y" || MergedFile$Indoor.Corridors_PL == "Y" || MergedFile$Laundry_PL == "Y" || MergedFile$Limo.Service_PL == "Y" || MergedFile$Mini.Bar_PL == "Y" || MergedFile$Pool.Indoor_PL == "Y" || MergedFile$Pool.Outdoor_PL == "Y" || MergedFile$Regency.Grand.Club_PL == "Y" || MergedFile$Resort_PL=="Y" || MergedFile$Restaurant_PL == "Y" || MergedFile$Self.Parking_PL == "Y" || MergedFile$Shuttle.Service_PL == "Y" || MergedFile$Ski_PL == "Y" || MergedFile$Spa_PL == "Y" && MergedFile$NPS_Type == "Promoter")
{
  C<-C+1
}

temp<-subset(Final.NPS,select=c("Country_PL","Guest.NPS.Goal_PL","NPS_Score"))
temp1<-unique.data.frame(temp)
temp1<-temp1[!duplicated(temp1$Country_PL), ]
c<-0
for(i in 1:length(temp1$Country_PL))
{
  count<-ifelse(temp1$Guest.NPS.Goal_PL>temp1$NPS_Score,c+1,c+0)
}
count
a<-table(count)
sum(count)
Unique_Ct

library(ggplot2)
temp1[is.na(temp1)] <- 0
str(temp1)
#ggplot<-(temp1,aes(x=Country_PL,y=)
library(reshape2)
t1<-melt(temp1,id.vars="Country_PL")
ggplot(t1,aes(x=variable,y=value,fill=factor(Country_PL)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Country_PL")

ggplot(temp1, aes(Country_PL)) + 
  geom_line(aes(y = Guest.NPS.Goal_PL , colour = "Guest.NPS.Goal_PL")) + 
  geom_line(aes(y = NPS_Score, colour = "NPS_Score"))

ggplot(test_data, aes(date)) + 
  geom_line(aes(y = var0, colour = "var0")) + 
  geom_line(aes(y = var1, colour = "var1"))

#FOr nps goal vs nps score
library(lattice)
barchart(Country_PL ~ value,data=t1,groups=variable, 
         scales=list(x=list(cex=0.8)))
mean(Final.NPS$Guest.NPS.Goal_PL,na.rm = TRUE)