#### Installing and opending packages ####
#install.packages("rgeos")
#install.packages("rgdal")
#install.packages("reshape2")
#install.packages("ggmap")
#install.packages("proj4")
#install.packages("spatialEco")
#install.packages("dplyr")
library(foreign)
library(rgeos)
library(rgdal)
library(reshape2)
library(ggmap)
library(sp)
library(proj4)
library(spatialEco)
library(dplyr)

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

Two_unit_df <- as.data.frame(Two_Unit_MF_SD)
Two_unit_df <- Two_unit_df[c(1:13)]
Two_unit_df <- Two_unit_df[c(12,6,1:5,7:11,13)]

colnames(Two_unit_df)[c(2)] <- "Year"
Two_unit_df$Year <- as.character(Two_unit_df$Year)
colnames(Two_unit_df)[c(1)] <- "GEOID"

### Creating full data sets

MF_with_Performance <- merge(Two_unit_df, Test_All_Years,by = c("Year","GEOID"), all.x = TRUE)
#MF_with_Performance_full_match<- merge(Two_unit_df, Test_All_Years,by = c("Year","GEOID"), all = FALSE)
#MF_with_Performance_all_districts <- merge(Two_unit_df, Test_All_Years,by = c("Year","GEOID"), all.y = TRUE, all.x = FALSE)
#MF_with_Performance_all_districts[is.na(MF_with_Performance_all_districts)] <- "NULL"
#MF_with_Performance_control <- subset(MF_with_Performance_all_districts, FID_1 == "NULL")

write.csv(MF_with_Performance, "C:/Users/cwonderly/Documents/Housing/Housing_Education_Project/MF_with_Performance.csv")
write.csv(Test_All_Years, "C:/Users/cwonderly/Documents/Housing/Housing_Education_Project/Test_All_Years.csv")
write.csv(Two_unit_df,"C:/Users/cwonderly/Documents/Housing/Housing_Education_Project/Two_Unit_df.csv")
