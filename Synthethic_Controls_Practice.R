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

# Removing any data from early years that had housing placed 

Synth_Data <- Synth_Data[!(Synth_Data$Year == 2002 & Synth_Data$Treated == 1),]
Synth_Data <- Synth_Data[!(Synth_Data$Year == 2003 & Synth_Data$Treated == 1),]
Synth_Data <- Synth_Data[!(Synth_Data$Year == 2004 & Synth_Data$Treated == 1),]
Synth_Data <- Synth_Data[!(Synth_Data$Year == 2005 & Synth_Data$Treated == 1),]
Synth_Data <- Synth_Data[!(Synth_Data$Year == 2006 & Synth_Data$Treated == 1),]

# Balancing the data after removing earlier observations
Synth_Data <- balance(Synth_Data, 14)

write.csv(Synth_Data, "C:/Users/cwond/Documents/Housing_Project/Restricted_Balanced.csv", row.names = FALSE)

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
out <- gsynth(Test_Level ~ Treated, data = Synth_Data, 
              index = c("GEOID","Year"), force = "unit",
              CV = FALSE, r = 0 , se = TRUE, 
              inference = "parametric", nboots = 1000,
              parallel = FALSE)


