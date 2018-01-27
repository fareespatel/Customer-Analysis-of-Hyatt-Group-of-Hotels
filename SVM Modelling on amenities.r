#install.packages('stringr')
library(stringr)
library(reshape2)
library(openintro)
install.packages('hexbin')
library(hexbin)

dataset<- read.csv('0210A.csv')

dataset<- dataset[,-1]
#head (dataset)
dataset<- read.csv('MergedFile.csv')

WorkingDS2<-dataset[ ,c("CHECKOUT_HEADER_ID_C","Brand_PL", "Country_PL","Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R",
                     "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",
                     "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type",
                     "All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL",
                     "Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL",
                     "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL",
                     "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL", "Resort_PL",
                     "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL")]



WorkingDS2 <- na.omit(WorkingDS2)
WorkingDS$Guest_Room_H<-gsub('\\s+', '',WorkingDS$Guest_Room_H)
WorkingDS$Condition_Hotel_H<- gsub('\\s+', '',WorkingDS$Condition_Hotel_H)
WorkingDS$Customer_SVC_H<- gsub('\\s+', '',WorkingDS$Customer_SVC_H)

#Step-1: Distributive analysis on the brands of Hotel

hotelBrand <- data.frame(table(WorkingDS$Brand_PL))
colnames(hotelBrand) <- c("Hotel_Brand", "Favorable_count")
gnew <- ggplot(data=hotelBrand, aes(x=Hotel_Brand, y=Favorable_count, fill=Favorable_count))
gnew <- gnew + geom_bar(stat="identity")
gnew <- gnew + theme(axis.text.x=element_text)
gnew<- gnew + coord_flip()
gnew

#Step-2: Number of people who gave feedback and grouping on hotel region

WorkingDS$Region_PL <- as.character(WorkingDS$Region_PL)
WorkingDS$Region_PL[str_trim(WorkingDS$Region_PL)==""] <- "Other"


hotelRegion <- data.frame(table(WorkingDS$Region_PL), stringsAsFactors=FALSE)
colnames(hotelRegion) <- c("Hotel_Region", "Number_of_feedback")
hotelRegion$Hotel_Region <- as.character(hotelRegion$Hotel_Region)
hotelRegion$Number_of_Sample <- as.numeric((hotelRegion$Number_of_feedback))
gnew2 <- ggplot(data=hotelRegion, aes(x=Hotel_Region, y=Number_of_feedback, fill=Number_of_feedback))
gnew2 <- gnew2 + geom_bar(stat="identity")
gnew2 <- gnew2 + theme(axis.text.x=element_text)
gnew2<- gnew2
gnew2



#Step-3: Plotting NPS value for every region
hotelRegionNPS <- data.frame(tapply(WorkingDS$NPS_Type, WorkingDS$Region_PL,
                                    function(x){NPS = (sum(x=="Promoter")-sum(x=="Detractor"))/sum(x!="") *100}),
                             stringsAsFactors=FALSE)
hotelRegionNPS$Hotel_Region <- labels(hotelRegionNPS)[[1]]
colnames(hotelRegionNPS) <- c("NPS", "Hotel_Region")
hotelRegionNPS$Hotel_Region <- as.character(hotelRegionNPS$Hotel_Region)
hotelRegionNPS$NPS <- as.numeric((hotelRegionNPS$NPS))
gnew3 <- ggplot(data=hotelRegionNPS, aes(x=Hotel_Region, y=NPS, fill=NPS))
gnew3 <- gnew3 + geom_bar(stat="identity")
#gnew3 <- gnew3 +coord_flip()
gnew3<- gnew3 + geom_text(aes(label=round(NPS,2)), vjust=-0.25)
gnew3

#Step-4: Plot corelation for Likelihood recommendation vs Overall Satisfaction
cor(WorkingDS$Likelihood_Recommend_H, WorkingDS$Overall_Sat_H)
# Scatter plot: x=overall satisfaction, y=likelihood to recommendation, color=NPS type
df <- WorkingDS[, c("Likelihood_Recommend_H","Overall_Sat_H","NPS_Type")]
df$Likelihood_Recommend_H <- df$Likelihood_Recommend_H + runif(nrow(df), min=0, max=1.1)
df$Overall_Sat_H <- df$Overall_Sat_H + runif(nrow(df), min=0, max=1.1)
g <- ggplot(data=df, aes(x=Overall_Sat_H))
g <- g + geom_point(aes(y=Likelihood_Recommend_H, color=NPS_Type))
g <- g + ggtitle("Likelihood to Recommendation vs. Overall Satisfaction")
g<- g+ stat_binhex()
g

#Step-5: Plot corelation for Likelihood recommendation vs average of "Condition_Hotel_H","Customer_SVC_H","Guest_Room_H"
WorkingDS$Condition_Hotel_H<- as.integer(WorkingDS$Condition_Hotel_H)
WorkingDS$Customer_SVC_H<-as.integer(WorkingDS$Customer_SVC_H)
WorkingDS$Guest_Room_H<-as.integer(WorkingDS$Guest_Room_H)

colm<-c("Condition_Hotel_H","Customer_SVC_H","Guest_Room_H")
as<- WorkingDS[,colm]
new3<- sqldf('Select (w.Condition_Hotel_H + w.Customer_SVC_H + w.Guest_Room_H)/3 from WorkingDS w')
as$avg<- new3
as$avg<- as.integer(unlist(as$avg))
names(as$avg)<- c("Average")

WorkingDS$Primary_amenities<- as$avg
cor(WorkingDS$Likelihood_Recommend_H, WorkingDS$Avgfield)
df2 <- WorkingDS[, c("Likelihood_Recommend_H","Primary_amenities","NPS_Type")]
df2$Likelihood_Recommend_H <- df2$Likelihood_Recommend_H + runif(nrow(df2), min=0, max=1.1)
df2$Primary_amenities <- df2$Primary_amenities + runif(nrow(df2), min=0, max=1.1)
g2 <- ggplot(data=df2, aes(x=Primary_amenities))
g2 <- g2 + geom_point(aes(y=Likelihood_Recommend_H, color=NPS_Type))
g2 <- g2 + ggtitle("Likelihood to Recommendation vs. Primary amenities")
g2

#Step-6: US- Overall SatisFACTION


USdataset<- WorkingDS[(WorkingDS$COUNTRY_CODE_R=='UNITED STATES'),]
USdataset$STATE_R<-gsub('\\s+', '',USdataset$STATE_R)
USdataset$StateName<- abbr2state(USdataset$STATE_R)


colm<-c("STATE_R","StateName")

f<-USdataset[,colm]
USdataset$StateName[is.na(USdataset$StateName)]<- "Others"

hotelRegionOS <- data.frame(tapply(USdataset$Overall_Sat_H, USdataset$StateName, mean),
                            stringsAsFactors=FALSE)
hotelRegionOS$Hotel_Region <- labels(hotelRegionOS)[[1]]
colnames(hotelRegionOS) <- c("Overall_Satisfaction", "StateName")
hotelRegionOS$StatesNames <- as.character(hotelRegionOS$StateName)
hotelRegionOS$Overall_Satisfaction <- as.numeric((hotelRegionOS$Overall_Satisfaction))

# Bar chart for Overall Satisfaction for US states
g3 <- ggplot(data=hotelRegionOS, aes(x=StateName, y=Overall_Satisfaction, fill=Overall_Satisfaction))
g3 <- g3 + geom_bar(stat="identity")
g3 <- g3 + ggtitle("US State Distribution - Overall Satisfaction")
#g3 <- g3 + ylim(0,10)
g3<- g3 + theme(axis.text.x=element_text(angle=90, hjust=1))
g3

#Box-plot of Primary amenities

df <- USdataset[,c("Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",
                  "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type")]
hotelMeaSat <- df[,2:8]
df <- df[,c("Overall_Sat_H", "Guest_Room_H", "Tranquility_H",
            "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H")]
df <- na.omit(df)
df.m <- melt(df)
# Box-plot for each satisfaction factor
g <- ggplot(data=df.m)
g <- g + geom_boxplot(aes(x=variable, y=value, group=variable)) + theme(axis.text.x=element_text(angle=90, hjust=1))
g <- g + ggtitle("US - Hotel Factors ")
g



#Step:7: Amenities for secondary US
amenity <- USdataset[,c("Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL",
                       "Fitness.Center_PL",
                       "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL",
                       "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL",
                       "Resort_PL",
                       "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL")]
amenity <- na.omit(amenity)
amenity$Spa_PL[amenity$Spa_PL=="Yes"] <- "Y"

df <- data.frame(ColName=character(), Yes=character(), No=character())
df
for (column in colnames(amenity)) {
  contingency <- data.frame(table(amenity[,column]))
  newrow <- data.frame(ColName=column, Yes=contingency[,2][contingency[,1]=="Y"],
                       No=contingency[,2][contingency[,1]=="N"])
  df <- rbind(df, newrow)
}
df.m <- melt(df, id.vars="ColName")

# Bar chart for secondary amenities
g <- ggplot(data=df.m, aes(x=ColName, y=value))
g <- g + geom_bar(aes(fill=variable), position="dodge", stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90, hjust=1))
g <- g + ggtitle("US Region - Secondary amenities") + xlab("Seconary amenties")
g

#Step-8: NPS value for US State

hotelRegionNPS <- data.frame(tapply(USdataset$NPS_Type, USdataset$StateName,
                                    function(x){NPS = (sum(x=="Promoter")-sum(x=="Detractor"))/sum(x!="") *100}),
                             stringsAsFactors=FALSE)
hotelRegionNPS$Hotel_Region <- labels(hotelRegionNPS)[[1]]
colnames(hotelRegionNPS) <- c("NPS", "State")
hotelRegionNPS$State <- as.character(hotelRegionNPS$State)
hotelRegionNPS$NPS <- as.numeric((hotelRegionNPS$NPS))
# Bar chart
g <- ggplot(data=hotelRegionNPS, aes(x=State, y=NPS,fill=NPS))
g <- g + geom_bar(stat="identity")
g <- g + geom_text(aes(x=State, y=NPS+0.05*mean(NPS), label=round(NPS,4)), size=5)
g <- g + ggtitle("State-wise Distribution - NPS")
g <- g + theme(axis.text.x=element_text(angle=90, hjust=1))
g
hotelRegionNPS[which.max(hotelRegionNPS$NPS),]

#Step-8: Bar plot for amenities for Mississippi 
Mississippi<- USdataset[(USdataset$StateName=="Mississippi"),]
amenity <- Mississippi[,c("Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL",
                        "Fitness.Center_PL",
                        "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL",
                        "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL",
                        "Resort_PL",
                        "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL")]
amenity <- na.omit(amenity)
amenity$Spa_PL[amenity$Spa_PL=="Yes"] <- "Y"

df <- data.frame(ColName=character(), Yes=character(), No=character())
df
for (column in colnames(amenity)) {
  contingency <- data.frame(table(amenity[,column]))
  newrow <- data.frame(ColName=column, Yes=contingency[,2][contingency[,1]=="Y"],
                       No=contingency[,2][contingency[,1]=="N"])
  df <- rbind(df, newrow)
}
df.m <- melt(df, id.vars="ColName")

# Bar chart Mississppi
g <- ggplot(data=df.m, aes(x=ColName, y=value))
g <- g + geom_bar(aes(fill=variable), position="dodge", stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90, hjust=1))
g <- g + ggtitle("Mississippi - Amenity Factors")
g

a<-data.frame(USdataset$Dry.Cleaning_PL[(USdataset$Dry.Cleaning_PL=="N")])
head(USdataset)
S<- sqldf('select * from USdataset where Dry.Cleaning_PL="Y"')
s<- sqldf('SELECT * from USdataset where (Conference_PL="Y") AND (Convention_PL="Y") AND (Dry.Cleaning_PL="Y") AND (Elevators_PL="Y") AND (Fitness.Center_PL="Y") AND (Fitness.Trainer_PL="Y") AND (Golf_PL="Y") AND (Indoor.Corridors_PL="Y") AND (Laundry_PL="Y") AND (Limo.Service_PL="Y") AND (Mini.Bar_PL="Y") AND (Pool.Indoor_PL="Y") AND (Pool.Outdoor_PL="Y") AND (Regency.Grand.Club_PL="Y") AND (Resort_PL="Y") AND (Restaurant_PL="Y") AND (Self.Parking_PL="Y") AND (Shuttle.Service_PL="Y") AND (Ski_PL="Y") AND (Spa_PL="Y")')



#Step-9:  Barchart for Primary amenities in Middle East
MEA<-WorkingDS[(WorkingDS$Region_PL=="Middle East & Africa"),]
Adataset<- WorkingDS2[(WorkingDS2$Country_PL== "United States"),]
dfA <- Adataset[,c("Country_PL", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",
                   "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H")]
colnames(dfA)[1]<- "Region_PL"

dfM <- MEA[,c("Region_PL", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",
                    "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H")]

dfM[,2:8]<- sapply(dfM[,2:8],as.integer)
dfA[,2:8]<- sapply(dfA[,2:8],as.integer)

df2<- rbind(dfA,dfM)
unique(df2$Region_PL)
sqldf('select distinct Region_PL from df2')

hotelRegionSFactors <- aggregate(df2[,-1], list(df2$Region_PL), mean)
colnames(hotelRegionSFactors)[1] <- "Hotel_Region"
hotelRegionSFactors.m <- melt(hotelRegionSFactors, id.vars="Hotel_Region")
# Bar chart
g <- ggplot(data=hotelRegionSFactors.m, aes(x=Hotel_Region, y=value))
g <- g + geom_bar(aes(fill=variable), position="dodge", stat="identity")
g <- g + ggtitle("Regional Distribution - Satisfaction Factors")
g <- g + ylim(0,10)
g


library(reshape2)
t1<- data.frame(t(hotelRegionSFactors))
colnames(t1)<- c("United States","Middle East & Africa")
t1<- t1[-1,]
t2$value<- sapply(t2$value, as.numeric)
t1$ameniy<- rownames(t1)
rownames(t1)<- NULL
str(t2)
t2<-t3
t3<- melt(t2,id.vars="ameniy")
library(lattice)
barchart(ameniy~value, data=t2, groups=variable, scales= list(x=list()))


#Step-10: Box plot for Primary amenities in Middle east
hotelMEA <- MEA
# Keep columns: "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",
# "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type"
df <- hotelMEA[,c("Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",
                  "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type")]
hotelMeaSat <- df[,2:8]
df <- df[,2:8]
df <- na.omit(df)
df.m <- melt(df, measure.vars = c("Overall_Sat_H", "Guest_Room_H", "Tranquility_H","Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H"))
# Box-plot for each satisfaction factor
g <- ggplot(data=df.m)
g <- g + geom_boxplot(aes(x=variable, y=value, group=variable))
g <- g + ggtitle("Middle East & Africa Region - Hotel Factors")
g

#Step-11: Bar plot for Secondary amenities in Middle east
# Bar-chart for amenity factors
amenity <- MEA[,c("Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL",
                       "Fitness.Center_PL",
                       "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL",
                       "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL",
                       "Resort_PL",
                       "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL")]
amenity <- na.omit(amenity)
amenity$Spa_PL[amenity$Spa_PL=="Yes"] <- "Y"
df <- data.frame(ColName=character(), Yes=character(), No=character())
for (column in colnames(amenity)) {
  contingency <- data.frame(table(amenity[,column]))
  newrow <- data.frame(ColName=column, Yes=contingency[,2][contingency[,1]=="Y"],
                       No=contingency[,2][contingency[,1]=="N"])
  df <- rbind(df, newrow)
}
df.m <- melt(df, id.vars="ColName")
# Bar chart
g <- ggplot(data=df.m, aes(x=ColName, y=value))
g <- g + geom_bar(aes(fill=variable), position="dodge", stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90, hjust=1))
g <- g + ggtitle("Middle East & Africa Region - Secondary Amenity Factors") +xlab("Secondary amenities")
g

#Step-11: Middle east correlation and plot

# Keep columns: "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",
# "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type"
df <- MEA[,c("Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",
                  "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type")]
hotelMeaSat <- df[,2:8]

af<- data.frame(sapply(df[,2:8],as.numeric))
af1<- data.frame(sapply(df[,1],as.numeric))
MEAcor<-data.frame(cor(x=af, y=af1))
colnames(MEAcor)<- c('Likelihood_Recommend_H')

g11 <- ggplot(data=MEAcor, aes(x=rownames(MEAcor), y=Likelihood_Recommend_H, fill=Likelihood_Recommend_H))
g11 <- g11 + geom_bar(stat="identity")
g11 <- g11 + theme(axis.text.x=element_text(angle=90, hjust=1))+ xlab("Amenities") +ggtitle("Primary amenities")
g11

#Modelling
#Without secondary amenities
#This will demonstrate how Likelihood is dependent on primary amentites. Explain in report using R squared value. Write, since R square value is close to 1 
#the primary amenties play an mportant role to drive Likelihood to recommendation hence improving the NPS value
lmModel <-
  lm(Likelihood_Recommend_H ~ Overall_Sat_H+Guest_Room_H+Tranquility_H+Condition_Hotel_H+Customer_SVC_H+Staff_Cared_H
     +Check_In_H, data=df)
summary(lmModel)

#With secondary amenties
# Check R square value and write since all amrnties are considered the R square value increases hence secondary amenties are important for improving liklihood to recommendation hence the NPS as well

df4<-amenity

df4<-data.frame(ifelse(df4=="Y",1,0))
df4<- data.frame(sapply(df4[,1:20],as.numeric))
df<- data.frame(sapply(df[,1:8],as.numeric))

df3<- cbind(df[,-9],df4)

LMWithA<- lm(Likelihood_Recommend_H ~. , data=df3)
summary(LMWithA)


#Step-12: # Bar-chart for secondary amenity factors for Middle east

amenity <- MEA[,c("Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL",
                       "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL",
                       "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL",
                       "Resort_PL",
                       "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL")]
amenity <- na.omit(amenity)
amenity$Spa_PL[amenity$Spa_PL==1] <- "Y"
df <- data.frame(ColName=character(), Yes=character(), No=character())
for (column in colnames(amenity)) {
  contingency <- data.frame(table(amenity[,column]))
  newrow <- data.frame(ColName=column, Yes=contingency[,2][contingency[,1]=="Y"],
                       No=contingency[,2][contingency[,1]=="N"])
  df <- rbind(df, newrow)
}
df.m <- melt(df, id.vars="ColName")
# Bar chart
g <- ggplot(data=df.m, aes(x=ColName, y=value))
g <- g + geom_bar(aes(fill=variable), position="dodge", stat="identity")
g <- g + theme(axis.text.x=element_text(angle=90, hjust=1))
g <- g + ggtitle("Middle East & Africa Region - Secondary Amenity Factors") + xlab("Secondary amenities")
g

#Step 13: svm modelling

df <- MEA[,c("Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",
             "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type")]
df[,1:8]<- data.frame(sapply(df[,1:8],as.numeric))
df5<- cbind(df,amenity)

set.seed(10)
randIndex <- sample(1:dim(df5)[1])
cutPoint2_3 <- floor(2*dim(df5)[1]/3)
trainData <- df5[randIndex[1:cutPoint2_3],]
testData <- df5[randIndex[(cutPoint2_3+1):dim(df5)[1]],]

# Create the Support Vector Machine (SVM) model
library(kernlab)
# for dataset only containing "Promoter" and "Passive", and "Detractors"
svmModel1 <- ksvm(NPS_Type~Guest_Room_H + Tranquility_H + Condition_Hotel_H +
                    Customer_SVC_H + Staff_Cared_H, data=trainData, kernel="rbfdot", kpar="automatic", C=20,
                  cross=3)
# Test the SVM model
svmPred <- round(predict(svmModel1, testData, type="votes"))
compTable <- data.frame(testData[,"NPS_Type"], svmPred[3,])
# Create a confusion matrix based on the prediction result
table(compTable) # Accuracy=81.5%

# for dataset only containing "Promoter" and "Passive", and "Detractors"
svmModel2 <- ksvm(NPS_Type~., data=trainData, kernel="rbfdot",
                  kpar="automatic", C=20, cross=3)
# Test the SVM model
svmPred <- round(predict(svmModel2, testData, type="votes"))
compTable <- data.frame(testData[,"NPS_Type"], svmPred[3,])
# Create a confusion matrix based on the prediction result
table(compTable) #Accuracy is 94%

#in the above svm model write that, if the user only uses the primary amenties than hotel is able to predict correctly 81.5% of times whether the user is either a promoter or a detractor. That is svmModel1
#BUt if the customer utilizes the secondary amenity the hotel is able to predict correctly 94%% of times whether the user is either a promoter or a detractor. That is svmModel2
# When you will display the table(compTable). You can calculate the efiiciency by [(Adding all the diagonal values)/(Adding all the nine values)]*100
