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
out <- gsynth(Test_Level ~ Treated, data = Synth_Data, 
              index = c("GEOID","Year"), force = "unit",
              CV = FALSE, r = 0 , se = TRUE, 
              inference = "parametric", nboots = 1000,
              parallel = FALSE)


