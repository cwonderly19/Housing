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

# Repeating again with share variable (see comments above)
share <- dcast(Synth_Practice, Synth_Practice$Year ~ Synth_Practice$GEOID, value.var = "Share_Meeting_Standard")
share <- share[-c(13,15,18,24,36,53,65,70,72,77,80,90)]
share <- share[-c(89,95,99)]
share <- share[-c(102,104,105,110,118,135,136,146,176,179)]


share_approx <- as.data.frame(lapply(share,
                                      function(x) approx(seq_along(x),x,seq_along(x), method = "linear")$y
))

share_approx <- na.locf(share_approx, na.rm = FALSE, fromLast = TRUE)

share_approx <- melt(share_approx, id.vars = "Synth_Practice.Year", value.name = "Level", variable.name = "GEOID", na.rm = TRUE)
share_approx$variable <- as.character(share_approx$variable)
share_approx <- cSplit(share_approx, "variable", "X")
share_approx <- as.matrix(share_approx)
share_approx <- as.data.frame(share_approx)
share_approx <- share_approx[c(1,4,2)]
colnames(share_approx)[c(1)] <- "Year"
colnames(share_approx)[c(2)] <- "GEOID"
colnames(share_approx)[c(3)] <- "Share_Meetings_Standard"

Synth_Practice <- merge(Synth_Practice, share_approx, by = c("Year", "GEOID"), all = TRUE)



# Removing any data from early years that had housing placed 
Synth_Data <- Synth_Practice[!Synth_Practice$Year == 2002 | Synth_Practice$All_Housing != 1,]
Synth_Data <- Synth_Data[!Synth_Data$Year == 2003 | Synth_Data$All_Housing != 1,]
Synth_Data <- Synth_Data[!Synth_Data$Year == 2004 | Synth_Data$All_Housing != 1,]
Synth_Data <- Synth_Data[!Synth_Data$Year == 2005 | Synth_Data$All_Housing != 1,]
Synth_Data <- Synth_Data[!Synth_Data$Year == 2006 | Synth_Data$All_Housing != 1,]

# Balancing the data after removing earlier observations
Synth_Data <- make_balanced(Synth_Data)

## Rearranging and cleaning data 
Synth_Data <- Synth_Data[-c(4,5)]
Synth_Data <- Synth_Data[c(1,2,29,28,3:27)]
colnames(Synth_Data)[3] <- "Share_Meeting_Standard"
colnames(Synth_Data)[4] <- "Level"

## Saving to Git as CSV
write.csv(Synth_Data, "C:/Users/cwonderly/Documents/Housing/Housing_Education_Project/Synth_Data.csv", row.names = FALSE)


#### Descriptive Statistics ####
stargazer(Synth_Data, type = "text", title = "Descriptive Statistics", digit.separate = 3, out = "descriptive_stats_2.txt")
# Determing how many treatment districts there are 
treatment <- subset(Synth_Data, Year != 2002 & Year != 2003 & Year != 2004 & Year != 2005 & Year != 2006 & All_Housing == 1)
total_treatment <- unique(treatment$GEOID)

<<<<<<< HEAD
#### Generalized Synthetic Controls ####
=======
#Synth_Data <- as.matrix(Synth_Data)
#Synth_Data <- as.data.frame(Synth_Data)

>>>>>>> f6a818d137f9259b2fc60631bf51750239737ca4
out <- gsynth(Share_Meeting_Standard ~ All_Housing, data = Synth_Data, 
              index = c("GEOID","Year"), force = "unit",
              CV = FALSE, r = 0 , se = TRUE, 
              inference = "parametric", nboots = 1000,
              parallel = FALSE)


