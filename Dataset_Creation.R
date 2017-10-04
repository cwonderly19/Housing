#### Installing Packages ####
#install.packages("gsynth")
#install.packages("arm")
#install.packages("zoo")
#install.packages("splitstackshape")
#### Loading Packages ####
library(gsynth)
library(arm)
library(zoo)
library(plyr)
library(splitstackshape)
data(gsynth)
head(simdata)

#### Creating Data for Synthetic Controls ####
# If you uploaded district level from Git you will need to remove the attributes at the end
District_Level <- as.matrix(District_Level)
District_Level <- as.data.frame(District_Level)
Synth_Practice <- (District_Level)
Synth_Practice <- Synth_Practice[c(2,27,3:26)] # Rearranging columns 

# Removing or interpolating N/As within Level varible
levels <- dcast(Synth_Practice, Synth_Practice$Year ~ Synth_Practice$GEOID, value.var = "Level") #converting to short format
levels <- levels[-c(16,18,22,26,30,41,53,63,79,80,89,90,96,98)] # If a district had more than 2 years of assessment data missing in a row they were excluded
levels <- levels[-c(91,93,98)]
levels <- levels[-c(99,100)]
levels_2 <- levels[c(1,101:262)]
levels <- levels[c(1:100)]
levels_2 <- levels_2[-c(4,15,23,30,38,43,52,54,55,60,63,74,96,99,100)]
levels_2 <- levels_2[-c(96,97,99)]
levels_2 <- levels_2[-c(99)]
levels_3 <- levels_2[c(1,101:144)]
levels_2 <- levels_2[c(1:100)]
levels_3 <- levels_3[-c(27,38,41)]
levels <- cbind(levels, levels_2, levels_3)

# Linear approximation of remaining N/A values, except for any missing from 2002
levels_approx <- as.data.frame(lapply(levels,
                                      function(x) approx(seq_along(x),x,seq_along(x), method = "linear")$y
))
# NOCB (Next observation carried backwards) approximation of any missing 2002 values
levels_approx <- na.locf(levels_approx, na.rm = FALSE, fromLast = TRUE)

# Converting back to long format
levels_approx <- melt(levels_approx, id.vars = "Synth_Practice.Year", value.name = "Level", variable.name = "GEOID", na.rm = TRUE)
levels_approx$GEOID <- as.character(levels_approx$GEOID)
levels_approx <- cSplit(levels_approx, "GEOID", "X")
levels_approx <- as.matrix(levels_approx)
levels_approx <- as.data.frame(levels_approx, stringsAsFactors = FALSE)
levels_approx <- levels_approx[c(1,4,2)]
colnames(levels_approx)[c(1)] <- "Year"
colnames(levels_approx)[c(2)] <- "GEOID"
colnames(levels_approx)[c(3)] <- "Level"
levels_approx$Year <- as.numeric(levels_approx$Year)
levels_approx$GEOID <- as.numeric(levels_approx$GEOID)
levels_approx$Level <- as.numeric(levels_approx$Level)
# Merging back with original data
Synth_Practice <- merge(Synth_Practice, levels_approx, by = c("Year", "GEOID"))

##### Creating All_Housing Variable ####
practice <- Synth_Practice
practice$Placement = (practice$P_02 + practice$P_03 + practice$P_04 + practice$P_05 + practice$P_06 + practice$P_07 + 
                        practice$P_08 + practice$P_09 + practice$P_10 + practice$P_11 + practice$P_12 + practice$P_13 + 
                        practice$P_14 +  + practice$P_15)

practice_dcast <- dcast(practice, practice$GEOID ~ practice$Year, value.var = "Placement")

practice_dcast$'2003' <- ifelse(practice_dcast$'2002'  == 1, 1, practice_dcast$'2003')
practice_dcast$'2004' <- ifelse(practice_dcast$'2003'  == 1, 1, practice_dcast$'2004')
practice_dcast$'2005' <- ifelse(practice_dcast$'2004'  == 1, 1, practice_dcast$'2005')
practice_dcast$'2006' <- ifelse(practice_dcast$'2005'  == 1, 1, practice_dcast$'2006')
practice_dcast$'2007' <- ifelse(practice_dcast$'2006'  == 1, 1, practice_dcast$'2007')
practice_dcast$'2008' <- ifelse(practice_dcast$'2007'  == 1, 1, practice_dcast$'2008')
practice_dcast$'2009' <- ifelse(practice_dcast$'2008'  == 1, 1, practice_dcast$'2009')
practice_dcast$'2010' <- ifelse(practice_dcast$'2009'  == 1, 1, practice_dcast$'2010')
practice_dcast$'2011' <- ifelse(practice_dcast$'2010'  == 1, 1, practice_dcast$'2011')
practice_dcast$'2012' <- ifelse(practice_dcast$'2011'  == 1, 1, practice_dcast$'2012')
practice_dcast$'2013' <- ifelse(practice_dcast$'2012'  == 1, 1, practice_dcast$'2013')
practice_dcast$'2014' <- ifelse(practice_dcast$'2013'  == 1, 1, practice_dcast$'2014')
practice_dcast$'2015' <- ifelse(practice_dcast$'2014'  == 1, 1, practice_dcast$'2015')

practice<- melt(practice_dcast, id.vars = "practice$GEOID", value.name = "All_Housing", variable.name = "Year")

colnames(practice)[1] <- "GEOID"
Synth_Practice <- merge(Synth_Practice, practice, by = c("GEOID", "Year"))
Synth_Data <- Synth_Practice[-c(4:21)]
colnames(Synth_Data)[9] <- "Test_Level"
colnames(Synth_Data)[10] <- "Treated"

Synth_Data$Treatment <- ifelse(Synth_Data$Units >= 1,1,0)
Synth_Data$Control <- ifelse(Synth_Data$Treatment == 0 & Synth_Data$All_Housing == 0, 1,0)
Synth_Data <- make_balanced(Synth_Practice)

post_treatment_dcast <- dcast(Synth_Data, Synth_Data$GEOID ~ Synth_Data$Year, value.var = "Treated")
post_treatment_dcast$'2015_2' <- ifelse(post_treatment_dcast$'2015'  == 1, 0, 0)
post_treatment_dcast$'2014_2' <- ifelse(post_treatment_dcast$'2014'  == 0 & post_treatment_dcast$'2015' == 1, 1, 0)
post_treatment_dcast$'2013_2' <- ifelse(post_treatment_dcast$'2013'  == 0 & post_treatment_dcast$'2014' == 1, 1, 0)
post_treatment_dcast$'2012_2' <- ifelse(post_treatment_dcast$'2012'  == 0 & post_treatment_dcast$'2013' == 1, 1, 0)
post_treatment_dcast$'2011_2' <- ifelse(post_treatment_dcast$'2011'  == 0 & post_treatment_dcast$'2012' == 1, 1, 0)
post_treatment_dcast$'2010_2' <- ifelse(post_treatment_dcast$'2010'  == 0 & post_treatment_dcast$'2011' == 1, 1, 0)
post_treatment_dcast$'2009_2' <- ifelse(post_treatment_dcast$'2009'  == 0 & post_treatment_dcast$'2010' == 1, 1, 0)
post_treatment_dcast$'2008_2' <- ifelse(post_treatment_dcast$'2008'  == 0 & post_treatment_dcast$'2009' == 1, 1, 0)
post_treatment_dcast$'2007_2' <- ifelse(post_treatment_dcast$'2007'  == 0 & post_treatment_dcast$'2008' == 1, 1, 0)
post_treatment_dcast$'2006_2' <- ifelse(post_treatment_dcast$'2006'  == 0 & post_treatment_dcast$'2007' == 1, 1, 0)
post_treatment_dcast$'2005_2' <- ifelse(post_treatment_dcast$'2005'  == 0 & post_treatment_dcast$'2006' == 1, 1, 0)
post_treatment_dcast$'2004_2' <- ifelse(post_treatment_dcast$'2004'  == 0 & post_treatment_dcast$'2005' == 1, 1, 0)
post_treatment_dcast$'2003_2' <- ifelse(post_treatment_dcast$'2003'  == 0 & post_treatment_dcast$'2004' == 1, 1, 0)
post_treatment_dcast$'2002_2' <- ifelse(post_treatment_dcast$'2002'  == 0 & post_treatment_dcast$'2003' == 1, 1, 0)

post_treatment_dcast <- post_treatment_dcast[-c(2:15)]
post_treatment_dcast <- post_treatment_dcast[c(1,15,14,13,12,11,10,9,8,7,6,5,4,3,2)]
colnames(post_treatment_dcast)[2] <- "2002"
colnames(post_treatment_dcast)[3] <- "2003"
colnames(post_treatment_dcast)[4] <- "2004"
colnames(post_treatment_dcast)[5] <- "2005"
colnames(post_treatment_dcast)[6] <- "2006"
colnames(post_treatment_dcast)[7] <- "2007"
colnames(post_treatment_dcast)[8] <- "2008"
colnames(post_treatment_dcast)[9] <- "2009"
colnames(post_treatment_dcast)[10] <- "2010"
colnames(post_treatment_dcast)[11] <- "2011"
colnames(post_treatment_dcast)[12] <- "2012"
colnames(post_treatment_dcast)[13] <- "2013"
colnames(post_treatment_dcast)[14] <- "2014"
colnames(post_treatment_dcast)[15] <- "2015" 

post_treatment_dcast[is.na(post_treatment_dcast)] <- 0
post_treatment_dcast$'2014' <- ifelse(post_treatment_dcast$'2015'  == 1, 1, post_treatment_dcast$'2014')
post_treatment_dcast$'2013' <- ifelse(post_treatment_dcast$'2014'  == 1, 1, post_treatment_dcast$'2013')
post_treatment_dcast$'2012' <- ifelse(post_treatment_dcast$'2013'  == 1, 1, post_treatment_dcast$'2012')
post_treatment_dcast$'2011' <- ifelse(post_treatment_dcast$'2012'  == 1, 1, post_treatment_dcast$'2011')
post_treatment_dcast$'2010' <- ifelse(post_treatment_dcast$'2011'  == 1, 1, post_treatment_dcast$'2010')
post_treatment_dcast$'2009' <- ifelse(post_treatment_dcast$'2010'  == 1, 1, post_treatment_dcast$'2009')
post_treatment_dcast$'2008' <- ifelse(post_treatment_dcast$'2009'  == 1, 1, post_treatment_dcast$'2008')
post_treatment_dcast$'2007' <- ifelse(post_treatment_dcast$'2008'  == 1, 1, post_treatment_dcast$'2007')
post_treatment_dcast$'2006' <- ifelse(post_treatment_dcast$'2007'  == 1, 1, post_treatment_dcast$'2006')
post_treatment_dcast$'2005' <- ifelse(post_treatment_dcast$'2006'  == 1, 1, post_treatment_dcast$'2005')
post_treatment_dcast$'2004' <- ifelse(post_treatment_dcast$'2005'  == 1, 1, post_treatment_dcast$'2004')
post_treatment_dcast$'2003' <- ifelse(post_treatment_dcast$'2004'  == 1, 1, post_treatment_dcast$'2003')
post_treatment_dcast$'2002' <- ifelse(post_treatment_dcast$'2003'  == 1, 1, post_treatment_dcast$'2002')

pre_treat <- melt(post_treatment_dcast, id.vars = "Synth_Data$GEOID", value.name = "Pre_Treat", variable.name = "Year")
colnames(pre_treat)[1] <- "GEOID"
pre_treat$Year <- as.character(pre_treat$Year)
pre_treat$Year <- as.numeric(pre_treat$Year)
Synth_Data <- merge(Synth_Data, pre_treat, by = c("GEOID", "Year"))

Synth_Data$Control <- ifelse(Synth_Data$Pre_Treat == 0 & Synth_Data$Treated == 0 & Synth_Data$Treatment == 0, 1,0)
Synth_Data$Post_Treatment <- ifelse(Synth_Data$Treated == 1 & Synth_Data$Treatment == 0, 1,0)

write.csv(Synth_Data, "C:/Users/cwond/Documents/Housing_Project/Synth_Data.csv", row.names = FALSE)

