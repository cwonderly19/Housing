#install.packages("gsynth")
#install.packages("arm")
library(gsynth)
library(arm)
data(gsynth)
head(simdata)


Synth_Practice <- na.omit(District_Level)
Synth_Practice <- Synth_Practice[c(1,2,29,3:28)]
Synth_Practice <- Synth_Practice[!Synth_Practice$Year == 2002 | Synth_Practice$All_Housing != 1,]
Synth_Practice <- Synth_Practice[!Synth_Practice$Year == 2003 | Synth_Practice$All_Housing != 1,]
Synth_Practice <- Synth_Practice[!Synth_Practice$Year == 2004 | Synth_Practice$All_Housing != 1,]
Synth_Practice <- Synth_Practice[!Synth_Practice$Year == 2005 | Synth_Practice$All_Housing != 1,]
Synth_Practice <- Synth_Practice[!Synth_Practice$Year == 2006 | Synth_Practice$All_Housing != 1,]


Synth_Practice <- make_balanced(Synth_Practice)
write.csv(Synth_Practice, "C:/Users/cwonderly/Documents/Housing/Housing_Education_Project/Synth_Practice.csv", row.names = FALSE)


stargazer(Synth_Practice, type = "text", title = "Descriptive Statistics", digit.separate = 3, out = "descriptive_stats_2.txt")


out <- gsynth(Share_Meeting_Standard ~ All_Housing, data = Synth_Practice, 
              index = c("GEOID","Year"), force = "unit",
              CV = FALSE, r = 0 , se = TRUE, 
              inference = "parametric", nboots = 1000,
              parallel = FALSE)

pretreatment_control <- subset(Synth_Practice, Year == 2002 & Year == 2003 & Year == 2004 & Year == 2005 & Year == 2006)
total_pretreatment_control <- unique(pretreatment_control$GEOID)

treatment <- subset(Synth_Practice, Year != 2002 & Year != 2003 & Year != 2004 & Year != 2005 & Year != 2006 & All_Housing == 1)
total_treatment <- unique(treatment$GEOID)
