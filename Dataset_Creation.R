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

Synth_Data <- make_balanced(Synth_Practice)

write.csv(Synth_Data, "C:/Users/cwonderly/Documents/Housing/Housing_Education_Project/Synth_Data.csv", row.names = FALSE)

