#Final Data Set to be loaded in R

datadf <- data.frame(Final_Data_Set_Merged)



str(datadf)
head(datadf)
install.packages("sqldf")
install.packages("ggplot2")
install.packages("ggmap")
library(sqldf)
library(ggplot2)
library(ggmap)
library(reshape2)
library(stringr)


head(datadf)

#Code to Compute the NPS of each Country 

NPS_Dummy <- sqldf('select count(*) as No_of_promoters ,Country_PL from datadf where NPS_Type = "Promoter" Group By Country_PL')
str(NPS_Dummy)
NPS_Dummy1 <- sqldf('select count(*) as No_of_detractors ,Country_PL from datadf where NPS_Type = "Detractor" Group By Country_PL')
NPS_Dummy2 <- sqldf('Select No_of_promoters,No_of_detractors,a.Country_PL from NPS_Dummy a , NPS_Dummy1 b where a.Country_PL = b.Country_PL')
sqldf('insert into NPS_Dummy2  select No_of_promoters, 0, Country_PL from NPS_Dummy a where not exists (select * from NPS_Dummy1 b where a.Country_PL = b.Country_PL)')
NPS_Dummy3 <- sqldf('select a.No_of_promoters, b.No_of_detractors, a.Country_PL from NPS_Dummy a LEFT JOIN NPS_Dummy1 b ON a.Country_PL = b.Country_PL')
NPS_Dummy3
NPS_Dummy4 <- sqldf('select count(*) as No_of_Passive ,Country_PL from datadf where NPS_Type = "Passive" Group By Country_PL')
NPS_Dummy4
NPS_Dummy5 <- sqldf('select a.No_of_promoters, a.No_of_detractors, a.Country_PL,b.No_of_Passive from NPS_Dummy3 a LEFT JOIN NPS_Dummy4 b ON a.Country_PL = b.Country_PL')
NPS_Dummy5
NPS_Dummy5$No_of_promoters[is.na(NPS_Dummy5$No_of_promoters)] <- 0
NPS_Dummy5$No_of_detractors[is.na(NPS_Dummy5$No_of_detractors)] <- 0
NPS_Dummy5$No_of_Passive[is.na(NPS_Dummy5$No_of_Passive)] <- 0

sqldf('select * from NPS_Dummy1 a where not exists (select * from NPS_Dummy b where a.Country_PL = b.Country_PL)')

NPS_Dummy5$Total_Voters <- NPS_Dummy5$No_of_promoters + NPS_Dummy5$No_of_detractors + NPS_Dummy5$No_of_Passive
NPS_Dummy5$Promoter_Perc <- NPS_Dummy5$No_of_promoters/ NPS_Dummy5$Total_Voters * 100 
NPS_Dummy5$Detractor_Perc <- NPS_Dummy5$No_of_detractors/ NPS_Dummy5$Total_Voters * 100 
NPS_Dummy5$NPS <- NPS_Dummy5$Promoter_Perc - NPS_Dummy5$Detractor_Perc 
NPS_Dummy5
sqldf('select * from  NPS_Dummy5 order by Total_Voters ')

#Plot to check the the frequency of NPS value for different countries
ggplot(NPS_Dummy5, aes(x=NPS)) + geom_histogram(binwidth=5, color="white", fill="black")

#Line Plot of Total Voters of each country Vs NPS 
ggplot(NPS_Dummy5, aes(x=Total_Voters, y= NPS)) + geom_line()

#Scatter Plot of NPS for each country 
ggplot(NPS_Dummy5, aes(x= Country_PL, y = NPS)) + geom_point() + theme(axis.text.x = element_text(angle = 90,hjust = 1))

#Bar Plot of NPS for each Country
ggplot(NPS_Dummy5,aes(x = Country_PL, y = NPS )) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90,hjust = 1))
min(NPS_Dummy5$NPS)
sqldf('select min(NPS) from NPS_Dummy5 ')
sqldf('select count(distinct NPS)  from Final_Data_Set ')

head(NPS_Dummy5)
NPS_Country <- NPS_Dummy5

datadf$NPS

#Plot NPS of each country on the world map
location_details <- geocode(NPS_Country$Country_PL)

NPS_Country$long <- location_details$lon
NPS_Country$lat <- location_details$lat
NPS_Country$long
NPS_Country$NPS
mp <- NULL
mapWorld <- borders("world", colour="black", fill="grey") # create a layer of borders

mp <- ggplot() +   mapWorld
mp <- mp+ geom_point(data = NPS_Country, aes(x=long, y=lat,color=NPS_Country$NPS),  size=3) + ggtitle("Worldwide NPS") + theme(plot.title = element_text(size = rel(3))) + labs(colour = "NPS")
mp


hotelData <- datadf
#Store the columns Required in a vector
columns <- c("X1", "Brand_PL", "Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R", 
             "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",
             "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type",
             "All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL",
             "Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL",
             "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL",
             "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL", "Resort_PL",
             "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL")
mhotelData <- hotelData[, columns]

colnames(hotelData)
# Remove missing data
mhotelData <- na.omit(mhotelData)

nrow(mhotelData)

# Format columns: Region_PL

mhotelData$Region_PL <- as.character(mhotelData$Region_PL)
mhotelData$Region_PL[str_trim(mhotelData$Region_PL)==""] <- "Other"




# Average grade of Likelihood to Recommendation distribution by hotel brand

barplot(tapply(hotelData$Likelihood_Recommend_H,hotelData$Brand_PL,mean),main = "likelihood of Recommendation") + theme(axis.text.x=element_text(angle=90, hjust=1))


#Number of Sample Distribution in the data set per brand of Hyatt
hotelBrand <- data.frame(table(hotelData$Brand_PL))
colnames(hotelBrand) <- c("Hotel_Brand", "Number_of_Sample")
hotelBrand
g <- ggplot(data=hotelBrand, aes(x=Hotel_Brand, y=Number_of_Sample))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90, hjust=1))
g

# Regional distribution: Sample number
hotelRegion <- data.frame(table(hotelData$Region_PL), stringsAsFactors=FALSE)
colnames(hotelRegion) <- c("Hotel_Region", "Number_of_Sample")
hotelRegion$Hotel_Region <- as.character(hotelRegion$Hotel_Region)
hotelRegion$Number_of_Sample <- as.numeric((hotelRegion$Number_of_Sample))
hotelRegion



g <- ggplot(data=hotelRegion, aes(x=Hotel_Region, y=Number_of_Sample))
g <- g + geom_bar(stat="identity")
g <- g + geom_text(aes(x=Hotel_Region, y=Number_of_Sample+0.05*sum(Number_of_Sample), label=Number_of_Sample), size=5)
g <- g + ggtitle("Regional Distribution - Sample Number")
g


#-----------------------Comparison of Hyatt Regency per Region-------------------------#
#Guest Room Rating Comparison#
HP_GuestRoomComp <- ggplot(hotelData[which(hotelData$Brand_PL=="Hyatt Regency"),], aes(x=Region_PL, y=Guest_Room_H, color = Region_PL)) + geom_boxplot() + ggtitle("Guest Room") + theme(plot.title = element_text(size = rel(1.5)))
HP_GuestRoomComp
#Tranquility_H Comparison#
HP_TranquilityComp <- ggplot(hotelData[which(hotelData$Brand_PL=="Hyatt Regency"),], aes(x=Region_PL, y=Tranquility_H, color = Region_PL)) + geom_boxplot() + ggtitle("Hotel Tranquility") + theme(plot.title = element_text(size = rel(1.5)))
HP_TranquilityComp
#Condition_Hotel_H Comparison#
HP_HotelConditionComp <- ggplot(hotelData[which(hotelData$Brand_PL=="Hyatt Regency"),], aes(x=Region_PL, y=Condition_Hotel_H, color = Region_PL)) + geom_boxplot() + ggtitle("Hotel Condition") + theme(plot.title = element_text(size = rel(1.5)))
HP_HotelConditionComp
#Staff_Cared_H Comparison#
HP_StaffCareComp <- ggplot(hotelData[which(hotelData$Brand_PL=="Hyatt Place"),], aes(x=Region_PL, y=Staff_Cared_H, color = Region_PL)) + geom_boxplot() + ggtitle("Staff Performance") + theme(plot.title = element_text(size = rel(1.5)))
HP_StaffCareComp
#Customer_SVC_H Comparison#
HP_CustomerServiceComp <- ggplot(hotelData[which(hotelData$Brand_PL=="Hyatt Regency"),], aes(x=Region_PL, y=Customer_SVC_H, color = Region_PL)) + geom_boxplot() + ggtitle("Customer Service") + theme(plot.title = element_text(size = rel(1.5)))
HP_CustomerServiceComp
#Check_In_H Comparison#
HP_CheckinComp <- ggplot(hotelData[which(hotelData$Brand_PL=="Hyatt Regency"),], aes(x=Region_PL, y=Check_In_H, color = Region_PL)) + geom_boxplot() + ggtitle("Ease of Check-In") + theme(plot.title = element_text(size = rel(1.5)))
HP_CheckinComp

install.packages("gridExtra")
library(gridExtra)
grid.arrange(HP_GuestRoomComp,HP_TranquilityComp,HP_HotelConditionComp,HP_StaffCareComp,HP_CustomerServiceComp,HP_CheckinComp, ncol = 2) 
#------------------------------------------------------------------------------------#

#customer type and NPS type

data <- hotelData
colnames(data)
data2<-data[,c("POV_CODE_C","NPS_Type")]
data2_1<-na.omit(data2)
data2_2<-sqldf('select POV_CODE_C,NPS_Type,count(*) from data2_1  group by POV_CODE_C,NPS_Type')
colnames(data2_2)<-c("customer_type","NPS","Number")
data2_2_1<-data2_2[1:3,]
data2_2_2<-data2_2[4:6,]
g1<-ggplot(data=data2_2[data2_2$customer_type=='BUSINESS',],aes(x=factor(1),y=Number,fill=NPS))+geom_bar(stat="identity",width=1) + coord_polar(theta="y") +
  xlab('') +ylab('Business') +labs(fill='NPS Type')
g2 <-ggplot(data=data2_2[data2_2$customer_type=='LEISURE',],aes(x=factor(1),y=Number,fill=NPS))+geom_bar(stat="identity",width=1)+ coord_polar(theta="y")+
  xlab('') +ylab('Leisure') +labs(fill='NPS Type')
grid.arrange(g1,g2)

# Regional distribution: NPS Value
hotelRegionNPS <- data.frame(tapply(hotelData$NPS_Type, hotelData$Region_PL, 
                                    function(x){NPS = (sum(x=="Promoter")-sum(x=="Detractor"))/sum(x!="") * 100}),
                             stringsAsFactors=FALSE)
hotelRegionNPS
hotelRegionNPS$Hotel_Region <- labels(hotelRegionNPS)[[1]]
colnames(hotelRegionNPS) <- c("NPS", "Hotel_Region")
hotelRegionNPS$Hotel_Region <- as.character(hotelRegionNPS$Hotel_Region)
hotelRegionNPS$NPS <- as.numeric((hotelRegionNPS$NPS))

#Scatter Plot of NPS for each region 
ggplot(hotelRegionNPS, aes(x= Hotel_Region, y = NPS)) + geom_point() + theme(axis.text.x = element_text(angle = 90,hjust = 1))

#Bar Plot of NPS for each region 
ggplot(hotelRegionNPS,aes(x = Hotel_Region, y = NPS )) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90,hjust = 1))



# Correlation between Likelihood to Recommendation and Overall Satisfaction

cor(mhotelData$Likelihood_Recommend_H, mhotelData$Overall_Sat_H)
#High correlation
# Scatter plot: x=overall satisfaction, y=likelihood to recommendation, color=NPS type
df <- mhotelData[, c("Likelihood_Recommend_H","Overall_Sat_H","NPS_Type")]
df$Likelihood_Recommend_H <- df$Likelihood_Recommend_H + runif(nrow(df), min=0, max=1.1)
df$Overall_Sat_H <- df$Overall_Sat_H + runif(nrow(df), min=0, max=1.1)
g <- ggplot(data=df, aes(x=Overall_Sat_H))
g <- g + geom_point(aes(y=Likelihood_Recommend_H, color=NPS_Type))
g <- g + ggtitle("Likelihood to Recommendation vs. Overall Satisfaction")
g

# Regional distribution: Overall Satisfaction
hotelRegionOS <- data.frame(tapply(mhotelData$Overall_Sat_H, mhotelData$Region_PL, mean), stringsAsFactors=FALSE)
hotelRegionOS$Hotel_Region <- labels(hotelRegionOS)[[1]]
colnames(hotelRegionOS) <- c("Overall_Satisfaction", "Hotel_Region")
hotelRegionOS$Hotel_Region <- as.character(hotelRegionOS$Hotel_Region)
hotelRegionOS$Overall_Satisfaction <- as.numeric((hotelRegionOS$Overall_Satisfaction))
# Bar chart
g <- ggplot(data=hotelRegionOS, aes(x=Hotel_Region, y=Overall_Satisfaction))
g <- g + geom_bar(stat="identity")
g <- g + geom_text(aes(x=Hotel_Region, y=Overall_Satisfaction+0.05*mean(Overall_Satisfaction), label=round(Overall_Satisfaction,2)), size=5)
g <- g + ggtitle("Regional Distribution - Overall Satisfaction")
g <- g + ylim(0,10)
g


# Satisfcatio#---------------------------------------------------------------------------------------------------------------#
# Regional Analysis: Middle East & Africa region
# 1) Factors for Overall Satisfaction
# 2) Predictive Model for Overall Satisfaction
#---------------------------------------------------------------------------------------------------------------#

# Sub dataset for Middle East & Africa region
hotelMEA <- mhotelData[mhotelData$Region_PL=="Middle East & Africa", ]
nrow(hotelMEA)
df <- hotelMEA[,c( "Guest_Room_H", "Tranquility_H",
                  "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H")]
df <- na.omit(df)
df.m <- melt(df)

# Box-plot for each satisfaction factor
g <- ggplot(data=df.m)
g <- g + geom_boxplot(aes(x=variable, y=value, group=variable))
g <- g + ggtitle("Middle East & Africa Region - Hotel Factors") + theme(axis.text.x = element_text(angle = 90,hjust = 1))
g

# Satisfcatio#---------------------------------------------------------------------------------------------------------------#
# Regional Analysis: America region
# 1) Factors for Overall Satisfaction
# 2) Predictive Model for Overall Satisfaction
#---------------------------------------------------------------------------------------------------------------#

# Sub dataset for Americas region
hotelAm <- mhotelData[mhotelData$Region_PL=="Americas", ]

nrow(hotelAm)

df <- hotelAm[,c("Guest_Room_H", "Tranquility_H",
                  "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H")]
df <- na.omit(df)
df.m <- melt(df)

# Box-plot for each satisfaction factor
g <- ggplot(data=df.m)
g <- g + geom_boxplot(aes(x=variable, y=value, group=variable)) + theme(axis.text.x = element_text(angle = 90,hjust = 1))
g <- g + ggtitle("US - Hotel Factors")
g

# Bar-chart for amenity factors for Middle east and africa region
amenity <- hotelMEA[,c("All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL",
                       "Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL",
                       "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL",
                       "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL", "Resort_PL",
                       "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL")]
amenity <- na.omit(amenity)
amenity$Spa_PL[amenity$Spa_PL== "Yes"] <- "Y"
amenity
df <- data.frame(ColName=character(), Yes=character(), No=character())
df




nrow(hotelMEA)
# Correlation analysis and linear regression model
dataMapping <- function(x) {
  x[x=="Y"] <- 1
  x[x=="N"] <- 0
  return(x)
}
colnames(hotelMEA)
for (colIndex in c(16:40)) {
  hotelMEA[,colIndex] <- as.character(hotelMEA[,colIndex])
  hotelMEA[,colIndex] <- dataMapping(hotelMEA[,colIndex])
  hotelMEA[,colIndex] <- as.numeric(hotelMEA[,colIndex])
}

hotelMEA
hotelMEA$Amenity <- 
  2*  hotelMEA$All.Suites_PL +
  0*  hotelMEA$Bell.Staff_PL +
  0*  hotelMEA$Boutique_PL +
  2*  hotelMEA$Business.Center_PL +
  2*  hotelMEA$Casino_PL +
  0*  hotelMEA$Conference_PL +
  1.5*hotelMEA$Convention_PL +
  0*  hotelMEA$Dry.Cleaning_PL +
  0*  hotelMEA$Elevators_PL +
  0*  hotelMEA$Fitness.Center_PL +
  2*  hotelMEA$Fitness.Trainer_PL +
  2*  hotelMEA$Golf_PL +
  0*  hotelMEA$Indoor.Corridors_PL +
  0*  hotelMEA$Laundry_PL +
  0*  hotelMEA$Limo.Service_PL +
  0*  hotelMEA$Mini.Bar_PL +
  1.5*hotelMEA$Pool.Indoor_PL +
  2*  hotelMEA$Pool.Outdoor_PL +
  1.5*hotelMEA$Regency.Grand.Club_PL +
  0*  hotelMEA$Resort_PL +
  0*  hotelMEA$Restaurant_PL +
  0*  hotelMEA$Self.Parking_PL +
  2*  hotelMEA$Shuttle.Service_PL +
  0*  hotelMEA$Ski_PL +
  2*  hotelMEA$Spa_PL

hotelMEA$Amenity <- round(10*(hotelMEA$Amenity-min(hotelMEA$Amenity))/(max(hotelMEA$Amenity)-min(hotelMEA$Amenity)))

# Create dataset for model training
hotelMeaSat <- hotelMEA[,c("Likelihood_Recommend_H", "Guest_Room_H", "Tranquility_H",
                           "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "Amenity", "NPS_Type")]
df <- hotelMeaSat[,-9]
# Create linear regression model
cor(df)
# Linear model without Amenity
lmModel1 <- lm(Likelihood_Recommend_H ~ Guest_Room_H + Tranquility_H + Condition_Hotel_H + 
                 Customer_SVC_H + Staff_Cared_H + Check_In_H, data=hotelMeaSat)
summary(lmModel1)

# Linear model with Amenity
lmModel2 <- lm(Likelihood_Recommend_H ~ Guest_Room_H + Tranquility_H + Condition_Hotel_H + 
                 Customer_SVC_H + Staff_Cared_H + Check_In_H + Amenity, data=hotelMeaSat)
summary(lmModel2)




#-----------------------------Function declarations----------------------------------#
#Functions to create df of two attributes fpr comparison and further use
#Start#
frequency_calc <- function(Vector1, Vector2)
{
  Comparator_df <- data.frame(Vector1,Vector2)
  ##colnames(Comparator_df) <- c(Vector1,Vector2)
  Freq_comp <- as.data.frame(table(Comparator_df))
  return(Freq_comp)
}

total_surveys <- function(brnd_name)
{
  
}
Freq_comparator <- function(att_name1, att_name2) #using Hyatt_JanData data set for Jan
{
  freq_df <- frequency_calc(hotelData[att_name1], hotelData[att_name2])
  return(freq_df)
}
#-----------------------------------------------------------------------------------#

#-------------------------------Data Analysis Conducted------------------------------#

#1. NPSType vs Brand
NPSType_Breakdown <- Freq_comparator("NPS_Type","Brand_PL")
NPSType_Breakdown
total_survey <- aggregate(NPSType_Breakdown$Freq, by=list(Brand_Name = NPSType_Breakdown$Brand_PL), FUN=sum)
colnames(total_survey) <- c("Brand_Name","Total_Count")

for (brnd_nm in total_survey$Brand_Name)
{
  NPSType_Breakdown[which(NPSType_Breakdown$Brand_PL == brnd_nm),4] <- NPSType_Breakdown[which(NPSType_Breakdown$Brand_PL == brnd_nm),3]/total_survey[which(total_survey$Brand_Name == brnd_nm),2]*100
}
colnames(NPSType_Breakdown) <- c("NPSType","Brand_PL","Total_Cnt","Overall_Prcnt")
NPSType_BrandPL_Plot <- ggplot(NPSType_Breakdown, aes(x=Brand_PL, y=Overall_Prcnt)) + geom_bar(aes(fill=NPSType), stat = "identity", position = "dodge") + ggtitle("Brand Performance") + theme(plot.title = element_text(size = rel(3)))
NPSType_BrandPL_Plot

#2. NPSType vs Region
field_num1 <- ncol(NPSType_Breakdown)+1
field_num2 <- field_num1+1
for (region_nm in unique(hotelData$Region_PL, na.rm = TRUE))
{
  field_nm1 <- gsub("&","",gsub(" ","",ifelse(region_nm=="","UnspecifiedRegion",region_nm)))
  field_nm2 <- gsub(" ","",paste(field_nm1,"_Percent"))
  for (brnd_nm in unique(hotelData$Brand_PL, na.rm = TRUE))
  {
    for (npsType in unique(hotelData$NPS_Type, na.rm = TRUE))
    {
      NPSType_Breakdown[which(NPSType_Breakdown$NPSType == npsType & NPSType_Breakdown$Brand_PL == brnd_nm), field_num1] <- nrow(hotelData[which(hotelData$NPS_Type == npsType & hotelData$Brand_PL == brnd_nm & hotelData$Region_PL == region_nm),])
      NPSType_Breakdown[which(NPSType_Breakdown$NPSType == npsType & NPSType_Breakdown$Brand_PL == brnd_nm), field_num2] <- NPSType_Breakdown[which(NPSType_Breakdown$NPSType == npsType & NPSType_Breakdown$Brand_PL == brnd_nm), field_num1]/nrow(hotelData[which(hotelData$Brand_PL == brnd_nm & hotelData$Region_PL == region_nm),]) * 100
    }
  }
  colnames(NPSType_Breakdown)[field_num1] <- field_nm1
  colnames(NPSType_Breakdown)[field_num2] <- field_nm2
  field_num1 <- field_num1 + 2
  field_num2 <- field_num2 + 2
}

Plot_BrandPL_MidEastAf <- ggplot(NPSType_Breakdown, aes(x=Brand_PL, y=MiddleEastAfrica_Percent)) + geom_bar(aes(fill=NPSType), stat = "identity", position = "dodge") + ggtitle("Middle East & Africa")+ theme(axis.text.x = element_text(angle = 90,hjust = 1))

Plot_BrandPL_AsiaPacific <- ggplot(NPSType_Breakdown, aes(x=Brand_PL, y=AsiaPacific_Percent)) + geom_bar(aes(fill=NPSType), stat = "identity", position = "dodge") + ggtitle("Asia Pacific")+ theme(axis.text.x = element_text(angle = 90,hjust = 1))

Plot_BrandPL_Americas <- ggplot(NPSType_Breakdown, aes(x=Brand_PL, y=Americas_Percent)) + geom_bar(aes(fill=NPSType), stat = "identity", position = "dodge") + ggtitle("Americas")+ theme(axis.text.x = element_text(angle = 90,hjust = 1))

Plot_BrandPL_Europe <- ggplot(NPSType_Breakdown, aes(x=Brand_PL, y=Europe_Percent)) + geom_bar(aes(fill=NPSType), stat = "identity", position = "dodge") + ggtitle("Europe")+ theme(axis.text.x = element_text(angle = 90,hjust = 1))

Plot_BrandPL_UnspecifiedRegion <- ggplot(NPSType_Breakdown, aes(x=Brand_PL, y=UnspecifiedRegion_Percent)) + geom_bar(aes(fill=NPSType), stat = "identity", position = "dodge") + ggtitle("Unspecified Region")+ theme(axis.text.x = element_text(angle = 90,hjust = 1))

grid.arrange(Plot_BrandPL_MidEastAf,Plot_BrandPL_AsiaPacific,Plot_BrandPL_Americas,Plot_BrandPL_Europe)

Plot_BrandPL_MidEastAf
Plot_BrandPL_AsiaPacific
data <- mhotelData
install.packages("RWeka")
library("RWeka")
InfoGainAttributeEval(NPS_Type ~ . , data = data)
table(trainset$NPS_Type)  
set.seed(1)
randIndex <- sample(1:dim(data)[1])
cutPoint2_3 <-floor(2*dim(data)[1]/3)
trainset <- data[1:cutPoint2_3,]
testset <- data[(cutPoint2_3+1):dim(data)[1],]
#MS <- make_Weka_filter("weka/filters/unsupervised/attribute/ReplaceMissingValues") 
#trainset_nomissing <-MS(data=trainset, na.action = NULL)
#testset_nomissing <-MS(data=testset, na.action = NULL)

data <- subset(data, data$Shuttle.Service_PL != "")
data <- subset(data, data$Spa.online.booking_PL != "")
data <- data[which(data$Spa_PL != ""),]

data <- na.omit(data)

# Models for Overall condition
myVars1 <- c("Guest_Room_H","Tranquility_H","Condition_Hotel_H"
             , "Staff_Cared_H","NPS_Type")
train1 <- trainset[myVars1]
test1 <- testset[myVars1]
test1 <- na.omit(test1)
train1 <- na.omit(train1)
# nb1
library(e1071)
nb1 = naiveBayes(NPS_Type ~., data = train1, laplace = 1, na.action = na.pass)
nb1
nbPred1 = predict(nb1, newdata=test1, type=c("class"))
table(nbPred1)
nbTable <- data.frame(test1[,5], nbPred1)
nbTable
table(nbTable)    

#Condition_Hotel_H
gg_Condition_Hotel_H <-ggplot(mhotelData, aes(x=NPS_Type,Condition_Hotel_H, fill=NPS_Type)) + geom_boxplot() + ggtitle("Hotel Condition")
gg_Condition_Hotel_H

#Tranquility_H
gg_Tranquility_H <-ggplot(mhotelData, aes(x=NPS_Type,Tranquility_H, fill=NPS_Type)) + geom_boxplot() + ggtitle("Tranquility")
gg_Tranquility_H

#Staff_Cared_H
gg_Staff_Cared_H <-ggplot(mhotelData, aes(x=NPS_Type,Staff_Cared_H, color=NPS_Type)) + geom_boxplot() + ggtitle("Staff Caring")
gg_Staff_Cared_H



#Spa_Plot
NPS_Spa_Plot <-ggplot(mhotelData, aes(NPS_Type,fill=Spa_PL )) + geom_bar(position="dodge") + ggtitle("Spa")
NPS_Spa_Plot



#Laundry_PL
NPS_Laundry_Plot <-ggplot(mhotelData, aes(NPS_Type,fill=Laundry_PL)) + geom_bar(position="dodge") + ggtitle("Laundry Facilities")
NPS_Laundry_Plot 

#Fitness.Center_PL
NPS_Fitness.Center_PL_Plot <-ggplot(mhotelData, aes(NPS_Type,fill=Fitness.Center_PL)) + geom_bar(position="dodge") + ggtitle("Fitness Center Facilities")
NPS_Fitness.Center_PL_Plot 

#Customer_Services 
gg_Customer_SVC_H <-ggplot(mhotelData, aes(x=NPS_Type,Customer_SVC_H, fill=NPS_Type)) + geom_boxplot() + ggtitle("Customer Services")
gg_Customer_SVC_H

#Shuttle Services 
NPS_Shuttle.Service_PL_Plot <-ggplot(mhotelData, aes(NPS_Type,fill=Shuttle.Service_PL )) + geom_bar(position="dodge") + ggtitle("Shuttle Service")
NPS_Shuttle.Service_PL_Plot 

#Self Parking 
NPS_Self.Parking_PL_Plot <-ggplot(mhotelData, aes(NPS_Type,fill=Self.Parking_PL )) + geom_bar(position="dodge") + ggtitle("Self Parking")
NPS_Self.Parking_PL_Plot 


#Checkin Process
gg_Check_In_H <-ggplot(mhotelData, aes(x=NPS_Type,Check_In_H, fill=NPS_Type)) + geom_boxplot() + ggtitle("Check In")
gg_Check_In_H

#Limo Service
NPS_Limo.Service_PL_Plot <-ggplot(mhotelData, aes(NPS_Type,fill=Limo.Service_PL )) + geom_bar(position="dodge") + ggtitle("Limo Service")
NPS_Limo.Service_PL_Plot

#Guest Room Services 
gg_Guest_Room_H <-ggplot(mhotelData, aes(x=NPS_Type,Guest_Room_H, fill=NPS_Type)) + geom_boxplot() + ggtitle("Guest Room")
gg_Guest_Room_H

grid.arrange(NPS_Limo.Service_PL_Plot,gg_Condition_Hotel_H,NPS_Self.Parking_PL_Plot,gg_Guest_Room_H,gg_Check_In_H, ncol=2 )
grid.arrange(NPS_Shuttle.Service_PL_Plot,gg_Tranquility_H, gg_Customer_SVC_H, gg_Staff_Cared_H, NPS_Fitness.Center_PL_Plot,NPS_Laundry_Plot, ncol=2)

##NPS analysis
data <- mhotelData
head(data)
# Tranquility
Tranquility1 <- subset(data, Tranquility_H == 1)
Tranquility2 <- subset(data, Tranquility_H == 2)
Tranquility3 <- subset(data, Tranquility_H == 3)
Tranquility4 <- subset(data, Tranquility_H == 4)
Tranquility5 <- subset(data, Tranquility_H == 5)
Tranquility6 <- subset(data, Tranquility_H == 6)
Tranquility7 <- subset(data, Tranquility_H == 7)
Tranquility8 <- subset(data, Tranquility_H == 8)
Tranquility9 <- subset(data, Tranquility_H == 9)
Tranquility10 <- subset(data, Tranquility_H == 10)
a1 <- table(Tranquility1$NPS_Type) 
a1 <- as.data.frame(a1)  
head(a1)
str(a1)
b1 <- a1[3,2]/(sum(a1$Freq)) - a1[1,2]/(sum(a1$Freq)) 
a2 <- table(Tranquility2$NPS_Type) 
a2 <- as.data.frame(a2)
b2 <- a2[3,2]/(sum(a2$Freq)) - a2[1,2]/(sum(a2$Freq)) 
a3 <- table(Tranquility3$NPS_Type)
a3 <- as.data.frame(a3)
b3 <- a3[3,2]/(sum(a3$Freq)) - a3[1,2]/(sum(a3$Freq)) 
a4 <- table(Tranquility4$NPS_Type)
a4 <- as.data.frame(a4)
b4 <- a4[3,2]/(sum(a4$Freq)) - a4[1,2]/(sum(a4$Freq))  
a5 <- table(Tranquility5$NPS_Type)
a5 <- as.data.frame(a5)
b5 <- a5[3,2]/(sum(a5$Freq)) - a5[1,2]/(sum(a5$Freq)) 
a6 <- table(Tranquility6$NPS_Type)
a6 <- as.data.frame(a6)
b6 <- a6[3,2]/(sum(a6$Freq)) - a6[1,2]/(sum(a6$Freq)) 
a7 <- table(Tranquility7$NPS_Type)
a7 <- as.data.frame(a7)
b7 <- a7[3,2]/(sum(a7$Freq)) - a7[1,2]/(sum(a7$Freq)) 
a8 <- table(Tranquility8$NPS_Type)
a8 <- as.data.frame(a8)
b8 <- a8[3,2]/(sum(a8$Freq)) - a8[1,2]/(sum(a8$Freq)) 
a9 <- table(Tranquility9$NPS_Type)
a9 <- as.data.frame(a9)
b9 <- a9[3,2]/(sum(a9$Freq)) - a9[1,2]/(sum(a9$Freq)) 
a10 <- table(Tranquility10$NPS_Type)
a10 <- as.data.frame(a10)
b10 <- a10[3,2]/(sum(a10$Freq)) - a10[1,2]/(sum(a10$Freq)) 
tranquilityNPS <- c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)
tranquility <- c(1,2,3,4,5,6,7,8,9,10)
R2 <- data.frame(tranquility, tranquilityNPS)
R2

R <- replicate(10,.6)
str(R)
R3 <- cbind(R,R2)

R3$guestNPS <- R3$guestNPS/100
R3

# Condition
Condition1 <- subset(data, Condition_Hotel_H == 1)
Condition2 <- subset(data, Condition_Hotel_H == 2)
Condition3 <- subset(data, Condition_Hotel_H == 3)
Condition4 <- subset(data, Condition_Hotel_H == 4)
Condition5 <- subset(data, Condition_Hotel_H == 5)
Condition6 <- subset(data, Condition_Hotel_H == 6)
Condition7 <- subset(data, Condition_Hotel_H == 7)
Condition8 <- subset(data, Condition_Hotel_H == 8)
Condition9 <- subset(data, Condition_Hotel_H == 9)
Condition10 <- subset(data, Condition_Hotel_H == 10)
a1 <- table(Condition1$NPS_Type) 
a1 <- as.data.frame(a1)  
b1 <- a1[3,2]/(sum(a1$Freq)) - a1[1,2]/(sum(a1$Freq)) 
a2 <- table(Condition2$NPS_Type) 
a2 <- as.data.frame(a2)
b2 <- a2[3,2]/(sum(a2$Freq)) - a2[1,2]/(sum(a2$Freq))  
a3 <- table(Condition3$NPS_Type)
a3 <- as.data.frame(a3)
b3 <- a3[3,2]/(sum(a3$Freq)) - a3[1,2]/(sum(a3$Freq)) 
a4 <- table(Condition4$NPS_Type)
a4 <- as.data.frame(a4)
b4 <- a4[3,2]/(sum(a4$Freq)) - a4[1,2]/(sum(a4$Freq)) 
a5 <- table(Condition5$NPS_Type)
a5 <- as.data.frame(a5)
b5 <- a5[3,2]/(sum(a5$Freq)) - a5[1,2]/(sum(a5$Freq)) 
a6 <- table(Condition6$NPS_Type)
a6 <- as.data.frame(a6)
b6 <- a6[3,2]/(sum(a6$Freq)) - a6[1,2]/(sum(a6$Freq)) 
a7 <- table(Condition7$NPS_Type)
a7 <- as.data.frame(a7)
b7 <- a7[3,2]/(sum(a7$Freq)) - a7[1,2]/(sum(a7$Freq)) 
a8 <- table(Condition8$NPS_Type)
a8 <- as.data.frame(a8)
b8 <- a8[3,2]/(sum(a8$Freq)) - a8[1,2]/(sum(a8$Freq)) 
a9 <- table(Condition9$NPS_Type)
a9 <- as.data.frame(a9)
b9 <- a9[3,2]/(sum(a9$Freq)) - a9[1,2]/(sum(a9$Freq)) 
a10 <- table(Condition10$NPS_Type)
a10 <- as.data.frame(a10)
b10 <- a10[3,2]/(sum(a10$Freq)) - a10[1,2]/(sum(a10$Freq))  
conditionNPS <- c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)
R4 <- cbind(R3, conditionNPS)
R4


# Cared
Cared1 <- subset(data, Staff_Cared_H == 1)
Cared2 <- subset(data, Staff_Cared_H == 2)
Cared3 <- subset(data, Staff_Cared_H == 3)
Cared4 <- subset(data, Staff_Cared_H == 4)
Cared5 <- subset(data, Staff_Cared_H == 5)
Cared6 <- subset(data, Staff_Cared_H == 6)
Cared7 <- subset(data, Staff_Cared_H == 7)
Cared8 <- subset(data, Staff_Cared_H == 8)
Cared9 <- subset(data, Staff_Cared_H == 9)
Cared10 <- subset(data, Staff_Cared_H == 10)
a1 <- table(Cared1$NPS_Type) 
a1 <- as.data.frame(a1)  
b1 <- a1[3,2]/(sum(a1$Freq)) - a1[1,2]/(sum(a1$Freq))  
a2 <- table(Cared2$NPS_Type) 
a2 <- as.data.frame(a2)
b2 <- a2[3,2]/(sum(a2$Freq)) - a2[1,2]/(sum(a2$Freq))  
a3 <- table(Cared3$NPS_Type)
a3 <- as.data.frame(a3)
b3 <- a3[3,2]/(sum(a3$Freq)) - a3[1,2]/(sum(a3$Freq))  
a4 <- table(Cared4$NPS_Type)
a4 <- as.data.frame(a4)
b4 <- a4[3,2]/(sum(a4$Freq)) - a4[1,2]/(sum(a4$Freq))  
a5 <- table(Cared5$NPS_Type)
a5 <- as.data.frame(a5)
b5 <- a5[3,2]/(sum(a5$Freq)) - a5[1,2]/(sum(a5$Freq))  
a6 <- table(Cared6$NPS_Type)
a6 <- as.data.frame(a6)
b6 <- a6[3,2]/(sum(a6$Freq)) - a6[1,2]/(sum(a6$Freq))  
a7 <- table(Cared7$NPS_Type)
a7 <- as.data.frame(a7)
b7 <- a7[3,2]/(sum(a7$Freq)) - a7[1,2]/(sum(a7$Freq)) 
a8 <- table(Cared8$NPS_Type)
a8 <- as.data.frame(a8)
b8 <- a8[3,2]/(sum(a8$Freq)) - a8[1,2]/(sum(a8$Freq))  
a9 <- table(Cared9$NPS_Type)
a9 <- as.data.frame(a9)
b9 <- a9[3,2]/(sum(a9$Freq)) - a9[1,2]/(sum(a9$Freq)) 
a10 <- table(Cared10$NPS_Type)
a10 <- as.data.frame(a10)
b10 <- a10[3,2]/(sum(a10$Freq)) - a10[1,2]/(sum(a10$Freq))  
CaredNPS <- c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)
R5 <- cbind(R4, CaredNPS)
R5



# Service
Service1 <- subset(data, Customer_SVC_H == 1)
Service2 <- subset(data, Customer_SVC_H == 2)
Service3 <- subset(data, Customer_SVC_H == 3)
Service4 <- subset(data, Customer_SVC_H == 4)
Service5 <- subset(data, Customer_SVC_H == 5)
Service6 <- subset(data, Customer_SVC_H == 6)
Service7 <- subset(data, Customer_SVC_H == 7)
Service8 <- subset(data, Customer_SVC_H == 8)
Service9 <- subset(data, Customer_SVC_H == 9)
Service10 <- subset(data, Customer_SVC_H == 10)
a1 <- table(Service1$NPS_Type) 
a1 <- as.data.frame(a1)  
b1 <- a1[3,2]/(sum(a1$Freq)) - a1[1,2]/(sum(a1$Freq))  
a2 <- table(Service2$NPS_Type) 
a2 <- as.data.frame(a2)
b2 <- a2[3,2]/(sum(a2$Freq)) - a2[1,2]/(sum(a2$Freq))  
a3 <- table(Service3$NPS_Type)
a3 <- as.data.frame(a3)
b3 <- a3[3,2]/(sum(a3$Freq)) - a3[1,2]/(sum(a3$Freq))  
a4 <- table(Service4$NPS_Type)
a4 <- as.data.frame(a4)
b4 <- a4[3,2]/(sum(a4$Freq)) - a4[1,2]/(sum(a4$Freq))  
a5 <- table(Service5$NPS_Type)
a5 <- as.data.frame(a5)
b5 <- a5[3,2]/(sum(a5$Freq)) - a5[1,2]/(sum(a5$Freq))  
a6 <- table(Service6$NPS_Type)
a6 <- as.data.frame(a6)
b6 <- a6[3,2]/(sum(a6$Freq)) - a6[1,2]/(sum(a6$Freq))  
a7 <- table(Service7$NPS_Type)
a7 <- as.data.frame(a7)
b7 <- a7[3,2]/(sum(a7$Freq)) - a7[1,2]/(sum(a7$Freq))  
a8 <- table(Service8$NPS_Type)
a8 <- as.data.frame(a8)
b8 <- a8[3,2]/(sum(a8$Freq)) - a8[1,2]/(sum(a8$Freq))  
a9 <- table(Service9$NPS_Type)
a9 <- as.data.frame(a9)
b9 <- a9[3,2]/(sum(a9$Freq)) - a9[1,2]/(sum(a9$Freq))  
a10 <- table(Service10$NPS_Type)
a10 <- as.data.frame(a10)
b10 <- a10[3,2]/(sum(a10$Freq)) - a10[1,2]/(sum(a10$Freq))  
ServiceNPS <- c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)
R7 <- cbind(R5, ServiceNPS)
R7

# Check In
CheckIn1 <- subset(data, Check_In_H == 1)
CheckIn2 <- subset(data, Check_In_H == 2)
CheckIn3 <- subset(data, Check_In_H == 3)
CheckIn4 <- subset(data, Check_In_H == 4)
CheckIn5 <- subset(data, Check_In_H == 5)
CheckIn6 <- subset(data, Check_In_H == 6)
CheckIn7 <- subset(data, Check_In_H == 7)
CheckIn8 <- subset(data, Check_In_H == 8)
CheckIn9 <- subset(data, Check_In_H == 9)
CheckIn10 <- subset(data, Check_In_H == 10)
a1 <- table(CheckIn1$NPS_Type) 
a1 <- as.data.frame(a1)  
b1 <- a1[3,2]/(sum(a1$Freq)) - a1[1,2]/(sum(a1$Freq))
a2 <- table(CheckIn2$NPS_Type) 
a2 <- as.data.frame(a2)
b2 <- a2[3,2]/(sum(a2$Freq)) - a2[1,2]/(sum(a2$Freq))  
a3 <- table(CheckIn3$NPS_Type)
a3 <- as.data.frame(a3)
b3 <- a3[3,2]/(sum(a3$Freq)) - a3[1,2]/(sum(a3$Freq))  
a4 <- table(CheckIn4$NPS_Type)
a4 <- as.data.frame(a4)
b4 <- a4[3,2]/(sum(a4$Freq)) - a4[1,2]/(sum(a4$Freq))  
a5 <- table(CheckIn5$NPS_Type)
a5 <- as.data.frame(a5)
b5 <- a5[3,2]/(sum(a5$Freq)) - a5[1,2]/(sum(a5$Freq))  
a6 <- table(CheckIn6$NPS_Type)
a6 <- as.data.frame(a6)
b6 <- a6[3,2]/(sum(a6$Freq)) - a6[1,2]/(sum(a6$Freq))  
a7 <- table(CheckIn7$NPS_Type)
a7 <- as.data.frame(a7)
b7 <- a7[3,2]/(sum(a7$Freq)) - a7[1,2]/(sum(a7$Freq))  
a8 <- table(CheckIn8$NPS_Type)
a8 <- as.data.frame(a8)
b8 <- a8[3,2]/(sum(a8$Freq)) - a8[1,2]/(sum(a8$Freq))  
a9 <- table(CheckIn9$NPS_Type)
a9 <- as.data.frame(a9)
b9 <- a9[3,2]/(sum(a9$Freq)) - a9[1,2]/(sum(a9$Freq))  
a10 <- table(CheckIn10$NPS_Type)
a10 <- as.data.frame(a10)
b10 <- a10[3,2]/(sum(a10$Freq)) - a10[1,2]/(sum(a10$Freq))  
CheckInNPS <- c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)
R8 <- cbind(R7, CheckInNPS)
R8

colnames(R8) <- c("Score","Guest Room", "Tranquility", "Hotel Condition","Staff Cared", "Customer Service","Check In")

newdata <- melt(R8, id="Score")
lines <- ggplot(newdata, aes(x=Score, y=value, color=variable)) +
   ggtitle("NPS improvement through amenities") +
   geom_point(data=newdata,aes(x=Score,y=value, color=variable)) +
   geom_line()
lines
str(R8)

