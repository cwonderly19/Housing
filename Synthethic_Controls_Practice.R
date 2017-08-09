#install.packages("gsynth")
#install.packages("arm")
#install.packages("zoo")
install.packages("splitstackshape")
library(gsynth)
library(arm)
library(zoo)
library(plyr)
library(splitstackshape)
data(gsynth)
head(simdata)

District_Level <- as.matrix(District_Level)
District_Level <- as.data.frame(District_Level)
Synth_Practice <- (District_Level)
Synth_Practice <- Synth_Practice[c(1,2,29,3:28)]


levels <- dcast(Synth_Practice, Synth_Practice$Year ~ Synth_Practice$GEOID, value.var = "Level")
levels <- levels[-c(13,15,18,24,36,53,65,70,72,77,80,90)]
levels <- levels[-c(89,95,99)]
levels <- levels[-c(102,104,105,110,118,135,136,146,176,179)]


levels_approx <- as.data.frame(lapply(levels,
                             function(x) approx(seq_along(x),x,seq_along(x), method = "linear")$y
))

levels_approx <- na.locf(levels_approx, na.rm = FALSE, fromLast = TRUE)

levels_approx <- melt(levels_approx, id.vars = "Synth_Practice.Year", value.name = "Level", variable.name = "GEOID", na.rm = TRUE)
levels_approx$variable <- as.character(levels_approx$variable)
levels_approx <- cSplit(levels_approx, "variable", "X")
levels_approx <- as.matrix(levels_approx)
levels_approx <- as.data.frame(levels_approx)
levels_approx <- levels_approx[c(1,4,2)]
colnames(levels_approx)[c(1)] <- "Year"
colnames(levels_approx)[c(2)] <- "GEOID"
colnames(levels_approx)[c(3)] <- "Level"

Synth_Practice <- merge(Synth_Practice, levels_approx, by = c("Year", "GEOID"), all = TRUE)


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

Synth_Practice <- na.omit(Synth_Practice)
Synth_Practice <- as.matrix(Synth_Practice)
Synth_Practice <- as.data.frame(Synth_Practice)

Synth_Data <- Synth_Practice[!Synth_Practice$Year == 2002 | Synth_Practice$All_Housing != 1,]
Synth_Data <- Synth_Data[!Synth_Data$Year == 2003 | Synth_Data$All_Housing != 1,]
Synth_Data <- Synth_Data[!Synth_Data$Year == 2004 | Synth_Data$All_Housing != 1,]
Synth_Data <- Synth_Data[!Synth_Data$Year == 2005 | Synth_Data$All_Housing != 1,]
Synth_Data <- Synth_Data[!Synth_Data$Year == 2006 | Synth_Data$All_Housing != 1,]


Synth_Data <- make_balanced(Synth_Data)

Synth_Data <- Synth_Data[-c(4,5)]
Synth_Data <- Synth_Data[c(1,2,29,28,3:27)]
colnames(Synth_Data)[3] <- "Share_Meeting_Standard"
colnames(Synth_Data)[4] <- "Level"

write.csv(Synth_Data, "C:/Users/cwonderly/Documents/Housing/Housing_Education_Project/Synth_Data.csv", row.names = FALSE)



stargazer(Synth_Data, type = "text", title = "Descriptive Statistics", digit.separate = 3, out = "descriptive_stats_2.txt")

#Synth_Data <- as.matrix(Synth_Data)
#Synth_Data <- as.data.frame(Synth_Data)

out <- gsynth(Share_Meeting_Standard ~ All_Housing, data = Synth_Data, 
              index = c("GEOID","Year"), force = "unit",
              CV = FALSE, r = 0 , se = TRUE, 
              inference = "parametric", nboots = 1000,
              parallel = FALSE)

treatment <- subset(Synth_Data, Year != 2002 & Year != 2003 & Year != 2004 & Year != 2005 & Year != 2006 & All_Housing == 1)
total_treatment <- unique(treatment$GEOID)
