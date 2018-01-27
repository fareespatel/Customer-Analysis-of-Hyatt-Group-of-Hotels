#Cleaning Datasets individually by selecting the required attributes and eliminating those tuples having blank NPS type value
#Cleaning the first dataset
#2014 02
df02 <- subset(out.201402, select = c("CHECKOUT_HEADER_ID_C", "ROOM_TYPE_CODE_C"       , "ROOM_TYPE_DESCRIPTION_C", "WALK_IN_FLG_C", "POV_CODE_C", "STATE_R", "COUNTRY_CODE_R", "OFFER_FLG_R",              "Age_Range_H", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Country_PL", "Guest.NPS.Goal_PL", "Location_PL", "NPS_Type"))
df02<-subset(df02, !df02$NPS_Type == "")

#2014 03
df03 <- subset(out.201403, select = c("CHECKOUT_HEADER_ID_C", "ROOM_TYPE_CODE_C"       , "ROOM_TYPE_DESCRIPTION_C", "WALK_IN_FLG_C", "POV_CODE_C", "STATE_R", "COUNTRY_CODE_R", "OFFER_FLG_R",              "Age_Range_H", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Country_PL", "Guest.NPS.Goal_PL", "Location_PL", "NPS_Type"))
df03<-subset(df03, !df03$NPS_Type == "")

newdf<-Reduce(function(...) merge(..., all=TRUE), list(df02, df03))

write.csv(newdf, "0203.csv")

#2014 04
df04 <- subset(out.201404, select = c("CHECKOUT_HEADER_ID_C", "ROOM_TYPE_CODE_C"       , "ROOM_TYPE_DESCRIPTION_C", "WALK_IN_FLG_C", "POV_CODE_C", "STATE_R", "COUNTRY_CODE_R", "OFFER_FLG_R",              "Age_Range_H", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Country_PL", "Guest.NPS.Goal_PL", "Location_PL", "NPS_Type"))
df04<-subset(df04, !df04$NPS_Type == "")
ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/0203.csv")

newdf<-Reduce(function(...) merge(..., all=TRUE), list(df04, ndf))
write.csv(newdf, "0204.csv")

#2014 05
df05 <- subset(out.201405, select = c("CHECKOUT_HEADER_ID_C", "ROOM_TYPE_CODE_C"       , "ROOM_TYPE_DESCRIPTION_C", "WALK_IN_FLG_C", "POV_CODE_C", "STATE_R", "COUNTRY_CODE_R", "OFFER_FLG_R",              "Age_Range_H", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Country_PL", "Guest.NPS.Goal_PL", "Location_PL", "NPS_Type"))
df05<-subset(df05, !df05$NPS_Type == "")
ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/0206.csv")

newdf<-Reduce(function(...) merge(..., all=TRUE), list(df05, ndf))
write.csv(newdf, "0205.csv")

#2015 06
df06 <- subset(out.201406, select = c("CHECKOUT_HEADER_ID_C", "ROOM_TYPE_CODE_C"       , "ROOM_TYPE_DESCRIPTION_C", "WALK_IN_FLG_C", "POV_CODE_C", "STATE_R", "COUNTRY_CODE_R", "OFFER_FLG_R",              "Age_Range_H", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Country_PL", "Guest.NPS.Goal_PL", "Location_PL", "NPS_Type"))
df06<-subset(df06, !df06$NPS_Type == "")
ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/0205.csv")

newdf<-Reduce(function(...) merge(..., all=TRUE), list(df06, ndf))
write.csv(newdf, "0206.csv")

# 2014 07
df07 <- subset(out.201407, select = c("CHECKOUT_HEADER_ID_C", "ROOM_TYPE_CODE_C"       , "ROOM_TYPE_DESCRIPTION_C", "WALK_IN_FLG_C", "POV_CODE_C", "STATE_R", "COUNTRY_CODE_R", "OFFER_FLG_R",              "Age_Range_H", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Country_PL", "Guest.NPS.Goal_PL", "Location_PL", "NPS_Type"))
df07<-subset(df07, !df07$NPS_Type == "")
ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/0205.csv")

newdf<-Reduce(function(...) merge(..., all=TRUE), list(df07,ndf))
write.csv(newdf, "0207.csv")

#2014 08
df08 <- subset(out.201408, select = c("CHECKOUT_HEADER_ID_C", "ROOM_TYPE_CODE_C"       , "ROOM_TYPE_DESCRIPTION_C", "WALK_IN_FLG_C", "POV_CODE_C", "STATE_R", "COUNTRY_CODE_R", "OFFER_FLG_R",              "Age_Range_H", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Country_PL", "Guest.NPS.Goal_PL", "Location_PL", "NPS_Type"))
df08<-subset(df08, !df08$NPS_Type == "")
ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/0207.csv")

newdf<-Reduce(function(...) merge(..., all=TRUE), list(df08, ndf))
write.csv(newdf, "0208.csv")

#2014 09
df09 <- subset(out.201409, select = c("CHECKOUT_HEADER_ID_C", "ROOM_TYPE_CODE_C"       , "ROOM_TYPE_DESCRIPTION_C", "WALK_IN_FLG_C", "POV_CODE_C", "STATE_R", "COUNTRY_CODE_R", "OFFER_FLG_R",              "Age_Range_H", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Country_PL", "Guest.NPS.Goal_PL", "Location_PL", "NPS_Type"))
df09<-subset(df09, !df09$NPS_Type == "")
ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/0208.csv")

newdf<-Reduce(function(...) merge(..., all=TRUE), list(df09, ndf))
write.csv(newdf, "0209.csv")

#2014 10
df10 <- subset(out.201410, select = c("CHECKOUT_HEADER_ID_C", "ROOM_TYPE_CODE_C"       , "ROOM_TYPE_DESCRIPTION_C", "WALK_IN_FLG_C", "POV_CODE_C", "STATE_R", "COUNTRY_CODE_R", "OFFER_FLG_R",              "Age_Range_H", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Country_PL", "Guest.NPS.Goal_PL", "Location_PL", "NPS_Type"))
df10<-subset(df10, !df10$NPS_Type == "")
ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/0209.csv")

newdf<-Reduce(function(...) merge(..., all=TRUE), list(df10, ndf))
write.csv(newdf, "0210.csv")

#Cleaning the second dataset
#2014 02
df02 <- subset(out.201402, select = c("CHECKOUT_HEADER_ID_C","Brand_PL", "Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R",                      "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",                      "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type",                      "All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL",                      "Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL",                      "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL",                      "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL", "Resort_PL",                      "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL"))
df02<-subset(df02, !df02$NPS_Type == "")
#2014 03
df03 <- subset(out.201403, select = c("CHECKOUT_HEADER_ID_C","Brand_PL", "Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R",                      "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",                      "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type",                      "All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL",                      "Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL",                      "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL",                      "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL", "Resort_PL",                      "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL"))
df03<-subset(df03, !df03$NPS_Type == "")
newdf<-Reduce(function(...) merge(..., all=TRUE), list(df02, df03))
write.csv(newdf, "0203A.csv")
#2014 04
df04 <- subset(out.201404, select = c("CHECKOUT_HEADER_ID_C","Brand_PL", "Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type", "All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL", "Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL", "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL", "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL", "Resort_PL", "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL"))
df04<-subset(df04, !df04$NPS_Type == "")
ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/amenities/0203A.csv")
newdf<-Reduce(function(...) merge(..., all=TRUE), list(df04, ndf))
write.csv(newdf, "0204A.csv")
#2014 05
df05 <- subset(out.201405, select = c("CHECKOUT_HEADER_ID_C","Brand_PL", "Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H","Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type","All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL",  "Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL", "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL", "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL", "Resort_PL", "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL"))
df05<-subset(df05, !df05$NPS_Type == "")
ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/amenities/0204A.csv")
newdf<-Reduce(function(...) merge(..., all=TRUE), list(df05, ndf))
write.csv(newdf, "0205A.csv")
#2015 06
df06 <- subset(out.201406, select = c("CHECKOUT_HEADER_ID_C","Brand_PL", "Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R",                      "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",                      "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type",                      "All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL",                      "Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL",                      "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL",                      "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL", "Resort_PL",                      "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL"))
df06<-subset(df06, !df06$NPS_Type == "")
ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/amenities/0205A.csv")
newdf<-Reduce(function(...) merge(..., all=TRUE), list(df06, ndf))
write.csv(newdf, "0206A.csv")



# 2014 07
df07 <- subset(out.201407, select = c("CHECKOUT_HEADER_ID_C","Brand_PL", "Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type", "All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL",  "Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL","Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL","Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL", "Resort_PL", "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL"))
df07<-subset(df07, !df07$NPS_Type == "")
ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/amenities/0206A.csv")
newdf<-Reduce(function(...) merge(..., all=TRUE), list(df07,ndf))
write.csv(newdf, "0207A.csv")
#2014 08
df08 <- subset(out.201408, select = c("CHECKOUT_HEADER_ID_C","Brand_PL", "Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type", "All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL", "Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL", "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL", "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL", "Resort_PL","Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL"))
df08<-subset(df08, !df08$NPS_Type == "")
                                      ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/amenities/0207A.csv")
                                      newdf<-Reduce(function(...) merge(..., all=TRUE), list(df08, ndf))
                                      write.csv(newdf, "0208A.csv")
                                      #2014 09
                                      df09 <- subset(out.201409, select = c("CHECKOUT_HEADER_ID_C","Brand_PL", "Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H","Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type","All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL", "Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL","Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL","Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL", "Resort_PL", "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL"))
                                      df09<-subset(df09, !df09$NPS_Type == "")
                                      ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/amenities/0208A.csv")
                                      
                                      newdf<-Reduce(function(...) merge(..., all=TRUE), list(df09, ndf))
                                      write.csv(newdf, "0209A.csv")
                                      #2014 10
                                      df10 <- subset(out.201410, select = c("CHECKOUT_HEADER_ID_C","Brand_PL", "Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R","Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H","Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type","All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL", "Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL", "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL","Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL", "Resort_PL","Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL"))
                                      df10<-subset(df10, !df10$NPS_Type == "")
                                      ndf<-read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/amenities/0209A.csv")
                                      newdf<-Reduce(function(...) merge(..., all=TRUE), list(df10, ndf))
                                      write.csv(newdf, "0210A.csv")
                                      
                                      
                                      #Merging the two different datasets created by us on the basis of "CHECKOUT_HEADER_ID_C" attribute so as to get a final dataset which we would be working on
                                      The second dataset was created as additional attributes were needed for the analysis
                                      #Reading the two datasets into R
                                      myfile<- read.csv('C:\\Users\\Amol\\Desktop\\Syracuse\\IST 687\\Project Hyatt\\out-201402.csv')
                                      mycreatedDS<- read.csv('C:/Users/Farees Patel/Downloads/out-201501.csv')
                                      dataset<- myfile
                                      newdataset<- mycreatedDS
                                      
                                      dataset<-dataset[ ,c("CHECKOUT_HEADER_ID_C","Brand_PL", "Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type","All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL","Conference_PL", "Convention_PL", "Dry.Cleaning_PL", "Elevators_PL", "Fitness.Center_PL","Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL", "Laundry_PL", "Limo.Service_PL","Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL", "Regency.Grand.Club_PL", "Resort_PL","Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL", "Ski_PL", "Spa_PL")]
                                      
                                      newdataset<-data.frame(read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/0210.csv", header=FALSE))
                                      dataset<-data.frame(read.csv("C:/Users/Amol/Desktop/Syracuse/IST 687/Project Hyatt/amenities/0210A.csv", header=FALSE))
                                      
                                      colnames(dataset)<-c("CHECKOUT_HEADER_ID_C","Brand_PL", "Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R","Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type","All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL", "Conference_PL", "Convention_PL", "Dry.Cleaning_PL","Elevators_PL", "Fitness.Center_PL",                        "Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL","Laundry_PL", "Limo.Service_PL",                        "Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL","Regency.Grand.Club_PL", "Resort_PL",                        "Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL" ,"Ski_PL", "Spa_PL")
                                      
                                      colnames(newdataset)<-c("CHECKOUT_HEADER_ID_C","ROOM_TYPE_CODE_C"             , "ROOM_TYPE_DESCRIPTION_C", "WALK_IN_FLG_C", "POV_CODE_C", "STATE_R", "COUNTRY_CODE_R", "OFFER_FLG_R",              "Age_Range_H", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Country_PL", "Guest.NPS.Goal_PL", "Location_PL", "NPS_Type")
                                      #Eliminating the redundant column
                                      dataset<-dataset[,-42]
                                      #SQL code to merge the two datasets based on the attribute CHECKOUT_HEADER_ID_C
                                      finaldataset<- sqldf('Select  d.*, h.*  from newdataset h INNER JOIN dataset d ON h.CHECKOUT_HEADER_ID_C=d.CHECKOUT_HEADER_ID_C')
                                      m=merge(dataset,newdataset,by="V2")
                                      write.csv(finaldataset,"MergedFile.csv")
                                      temp<-finaldataset
                                      temp<-temp[,-1]
                                      #Changing the column names
                                      colnames(temp)<-c("ID","CHECKOUT_HEADER_ID_C","Brand_PL", "Region_PL", "G.Region_PL", "COUNTRY_CODE_R", "STATE_R","Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H","Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Check_In_H", "NPS_Type","All.Suites_PL", "Bell.Staff_PL", "Boutique_PL", "Business.Center_PL", "Casino_PL","Conference_PL", "Convention_PL", "Dry.Cleaning_PL","Elevators_PL", "Fitness.Center_PL","Fitness.Trainer_PL", "Golf_PL", "Indoor.Corridors_PL","Laundry_PL", "Limo.Service_PL","Mini.Bar_PL", "Pool.Indoor_PL", "Pool.Outdoor_PL","Regency.Grand.Club_PL", "Resort_PL","Restaurant_PL", "Self.Parking_PL", "Shuttle.Service_PL" ,"Ski_PL", "Spa_PL","ID","CHECKOUT_HEADER_ID_C","ROOM_TYPE_CODE_C"      , "ROOM_TYPE_DESCRIPTION_C", "WALK_IN_FLG_C", "POV_CODE_C", "STATE_R", "COUNTRY_CODE_R", "OFFER_FLG_R",              "Age_Range_H", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Country_PL", "Guest.NPS.Goal_PL", "Location_PL", "NPS_Type")
                                      #Storing it as a csv file
                                      write.csv(temp,"MergedFile.csv")