#### Installing and opending packages ####
install.packages("rgeos")
install.packages("rgdal")
install.packages("reshape2")
install.packages("ggmap")
install.packages("proj4")
install.packages("spatialEco")
install.packages("dplyr")
#install.packages("hms")
library(foreign)
library(rgeos)
library(rgdal)
library(reshape2)
library(ggmap)
library(sp)
library(proj4)
library(spatialEco)
library(dplyr)
library(data.table)
library(hms)


#### Reading in Original Shapefile of LIHTC Housing Projects from HUD's geospatial data website ####
LIHTC <- readOGR(dsn = "C:/Users/cwonderly/Downloads/LIHTC", layer = "LowIncome_Housing_Tax_Credit_Properties", stringsAsFactors = FALSE)
LIHTC$YR_PIS <- as.numeric(LIHTC$YR_PIS)
LIHTC$N_UNITS <- as.numeric(LIHTC$N_UNITS)
LIHTC_2000 <- subset(LIHTC, YR_PIS >= 2000)
LIHTC_2000 <- subset(LIHTC_2000, YR_PIS < 2017)

#### Reading in Original Shapefile of Public Housing Projects from HUD's geospatial data website ####
PUB <- readOGR(dsn = "C:/Users/cwonderly/Documents/Housing/PUB", layer = "Public_Housing_Buildings", stringsAsFactors = FALSE)
construction_date <- data.frame(PUB$OBJECTID, PUB$DOFA_ACTUA, stringsAsFactors =  FALSE)
construction_date$PUB.DOFA_ACTUA <- colsplit(construction_date$PUB.DOFA_ACTUA, "-", 
                                    c("Year","Month","other"))

colnames(construction_date)[1] <- "OBJECTID"
colnames(construction_date)[2] <- "YR_PIS"
construction_date$YR_PIS <- construction_date$YR_PIS[-c(2,3)]
construction_date$YR_PIS <- as.character(construction_date$YR_PIS)
construction_date$YR_PIS <- as.integer(construction_date$YR_PIS)

#### Cleaning both datasets ahnd merging ####

PUB_new <- merge(PUB, construction_date, by = "OBJECTID")
PUB_new$YR_PIS
PUB_2000 <- subset(PUB_new, YR_PIS >= 2000)
PUB_2000 <- subset(PUB_2000, YR_PIS < 2017)

PUB_2000$HOUSING_TYPE <- "PUB"
LIHTC_2000$HOUSING_TYPE <- "LIHTC"

LIHTC_2000 <- LIHTC_2000[c(1,4,5,6,7,18,27,130)]
PUB_2000 <- PUB_2000[-c(2:12,14:101,103,107:140)]
PUB_2000 <- PUB_2000[c(1,13,20:80)]
PUB_2000 <- PUB_2000[c(1,2,22,24,25,26,63)]

LIHTC_2000$Units <- LIHTC_2000$LI_UNITS
PUB_2000$Units <- PUB_2000$TOTAL_DWEL
PUB_2000$PROJ_ADD <- PUB_2000$STD_ADDR
PUB_2000$PROJ_CTY <- PUB_2000$STD_CITY
PUB_2000$PROJ_ST <- PUB_2000$STD_ST
PUB_2000$PROJ_ZIP <- PUB_2000$STD_ZIP5
PUB_2000 <- PUB_2000[-c(2:6)]
LIHTC_2000 <- LIHTC_2000[-c(6)]

LIHTC_2000$ID <- paste0(LIHTC_2000$HOUSING_TYPE,LIHTC_2000$OBJECTID)
PUB_2000$ID <- paste0(PUB_2000$HOUSING_TYPE, PUB_2000$OBJECTID)

Housing <- rbind(LIHTC_2000, PUB_2000)
Housing <- Housing[-c(1,9)]
Housing$Units <- as.numeric(Housing$Units)

MF_Housing_2 <- subset(Housing, Units >= 2)
MF_Housing_5 <- subset(Housing, Units >= 5)

WA_MF_2 <- subset(MF_Housing_2, PROJ_ST == "WA") 
WA_MF_5 <- subset(MF_Housing_5, PROJ_ST == "WA")

### Saving files as shapefiles to merge with school districts in ArcMap

writeOGR(WA_MF_2, dsn = "C:/Users/cwonderly/Documents/Housing/WA_MUltifamily_Two_Units", layer = "WA_MF_Two", driver = "ESRI Shapefile")
writeOGR(WA_MF_5, dsn = "C:/Users/cwonderly/Documents/Housing/WA_MUltifamily_Five_Units", layer = "WA_MF_Five", driver = "ESRI Shapefile")

#### Combining Housing and Test data into one dataset ####

## Reading in new shapefile with housing data and school district data and csv with district test data 

Two_Unit_MF_SD <- readOGR(dsn = "C:/Users/cwonderly/Documents/Housing/Housing_in_Districts", layer = "Housing_District_Join", stringsAsFactors = FALSE)

Test_All_Years <- read.csv("C:/Users/cwonderly/Documents/Housing/Test_Data/All_Years_Final_2.csv", stringsAsFactors = FALSE)

colnames(Test_All_Years)[c(7)] <- "Share_Meeting_Standard"
colnames(Test_All_Years)[c(6)] <- "Total_Meeting_Standard"
colnames(Test_All_Years)[c(8)] <- "Level"
Test_All_Years <- Test_All_Years[c(3,4,1:2,5:9)]
Test_All_Years$Year <- as.character(Test_All_Years$Year)

write.csv(Test_All_Years, "C:/Users/cwonderly/Documents/Housing/Housing_Education_Project/Test_All_Years.csv")

Two_unit_df <- as.data.frame(Two_Unit_MF_SD)
Two_unit_df <- Two_unit_df[c(1:13)]
Two_unit_df <- Two_unit_df[c(12,6,1:5,7:11,13)]

colnames(Two_unit_df)[c(2)] <- "Year"
Two_unit_df$Year <- as.character(Two_unit_df$Year)
colnames(Two_unit_df)[c(1)] <- "GEOID"

write.csv(Two_unit_df,"C:/Users/cwonderly/Documents/Housing/Housing_Education_Project/Two_Unit_df.csv")

#### Creating full data sets #### 

MF_with_Performance <- merge(Two_unit_df, Test_All_Years,by = c("Year","GEOID"), all = TRUE)
MF_with_Performance <- subset(MF_with_Performance, Year != "2000" & Year != "2001" & Year != "2016" & GEOID != "#N/A" & District != "#N/A") #Removing years without test data or school district matches

### Saving files to repository

write.csv(MF_with_Performance, "C:/Users/cwonderly/Documents/Housing/Housing_Education_Project/MF_with_Performance.csv")

#### Creating District Level Dataset ####

Trial_Dataset <- MF_with_Performance

Trial_Dataset <- Trial_Dataset[-c(3:7,10:13,16,17)]
Trial_Dataset[is.na(Trial_Dataset)] <- "NULL"

#Creating Dummy Variables
Trial_Dataset$LIHTC <- ifelse(Trial_Dataset$HOUSING == "LIHTC", 1,0)
Trial_Dataset$PUB <- ifelse(Trial_Dataset$HOUSING == "PUB", 1,0)
Trial_Dataset$P_02 <- ifelse(Trial_Dataset$Year == "2002" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$P_03 <- ifelse(Trial_Dataset$Year == "2003" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$P_04 <- ifelse(Trial_Dataset$Year == "2004" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$P_05 <- ifelse(Trial_Dataset$Year == "2005" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$P_06 <- ifelse(Trial_Dataset$Year == "2006" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$P_07 <- ifelse(Trial_Dataset$Year == "2007" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$P_08 <- ifelse(Trial_Dataset$Year == "2008" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$P_09 <- ifelse(Trial_Dataset$Year == "2009" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$P_10 <- ifelse(Trial_Dataset$Year == "2010" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$P_11 <- ifelse(Trial_Dataset$Year == "2011" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$P_12 <- ifelse(Trial_Dataset$Year == "2012" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$P_13 <- ifelse(Trial_Dataset$Year == "2013" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$P_14 <- ifelse(Trial_Dataset$Year == "2014" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$P_15 <- ifelse(Trial_Dataset$Year == "2015" & Trial_Dataset$HOUSING != "NULL", 1,0)
Trial_Dataset$WASL <- ifelse(Trial_Dataset$Test == "WASL",1,0)
Trial_Dataset$HSPE <- ifelse(Trial_Dataset$Test == "HSPE",1,0)
Trial_Dataset$SBA <- ifelse(Trial_Dataset$Test == "SBA",1,0)
Trial_Dataset$LIHTC_Units <- ifelse(Trial_Dataset$LIHTC == 1, Trial_Dataset$Units, 0) 
Trial_Dataset$PUB_Units <- ifelse(Trial_Dataset$PUB == 1, Trial_Dataset$Units,0)

## Changing all variables to numeric for aggregation

Trial_Dataset$Share_Meeting_Standard <- as.numeric(Trial_Dataset$Share_Meeting_Standard)
Trial_Dataset$Level <- as.numeric(Trial_Dataset$Level)
Trial_Dataset$Units <- as.numeric(Trial_Dataset$Units)
Trial_Dataset$LIHTC_Units <- as.numeric(Trial_Dataset$LIHTC_Units)
Trial_Dataset$PUB_Units <- as.numeric(Trial_Dataset$PUB_Units)
Trial_Dataset[is.na(Trial_Dataset)] <- 0
Trial_Dataset$Count <- 1 #this is so the share and level variables can be brought back after aggregating 
Trial_Dataset$Year <- as.numeric(Trial_Dataset$Year)
Trial_Dataset$GEOID <- as.numeric(Trial_Dataset$GEOID)

Trial_Dataset <- Trial_Dataset[-c(3,5,6,9)] #unncessary charcater variables 

District_Level <- aggregate(Trial_Dataset, by = list(Trial_Dataset$Year,Trial_Dataset$GEOID), FUN = sum)

##Reassigning the correct values to dummy variables after aggregation

District_Level$LIHTC <- ifelse(District_Level$LIHTC > 0, 1,0)
District_Level$PUB <- ifelse(District_Level$PUB> 0, 1,0)
District_Level$P_02 <- ifelse(District_Level$P_02 > 0, 1,0)
District_Level$P_03 <- ifelse(District_Level$P_03  > 0, 1,0)
District_Level$P_04 <- ifelse(District_Level$P_04  > 0, 1,0)
District_Level$P_05 <- ifelse(District_Level$P_05  > 0, 1,0)
District_Level$P_06 <- ifelse(District_Level$P_06  > 0, 1,0)
District_Level$P_07 <- ifelse(District_Level$P_07  > 0, 1,0)
District_Level$P_08 <- ifelse(District_Level$P_08  > 0, 1,0)
District_Level$P_09 <- ifelse(District_Level$P_09  > 0, 1,0)
District_Level$P_10 <- ifelse(District_Level$P_10  > 0, 1,0)
District_Level$P_11 <- ifelse(District_Level$P_11 > 0, 1,0)
District_Level$P_12 <- ifelse(District_Level$P_12 > 0, 1,0)
District_Level$P_13 <- ifelse(District_Level$P_13 > 0, 1,0)
District_Level$P_14 <- ifelse(District_Level$P_14 > 0, 1,0)
District_Level$P_15 <- ifelse(District_Level$P_15 > 0, 1,0)
District_Level$WASL <- ifelse(District_Level$WASL > 0, 1,0)
District_Level$HSPE <- ifelse(District_Level$HSPE > 0, 1,0)
District_Level$SBA <- ifelse(District_Level$SBA > 0, 1,0)
#Returning share and level variables to the correct value
District_Level$Share_Meeting_Standard <- (District_Level$Share_Meeting_Standard/District_Level$Count)
District_Level$Level <- (District_Level$Level/District_Level$Count)

#Removing the year and geoid variables that were aggregated 
District_Level <- District_Level[-c(3,4)]
#renaming the year and geoid variables are created when the orginials were aggregated
colnames(District_Level)[1] <- "Year"
colnames(District_Level)[2] <- "GEOID"

#reorganizing data
District_Level <- District_Level[c(1,2,4:26,3,27)]

#saving district level data as csv to the repository
write.csv(District_Level, "C:/Users/cwonderly/Documents/Housing/Housing_Education_Project/District_Level.csv")
