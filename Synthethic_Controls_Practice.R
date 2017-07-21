install.packages("gsynth")
install.packages("arm")
library(gsynth)
library(arm)
data(gsynth)
head(simdata)


Synth_Practice <- District_Level[-c(1)]
Synth_Practice <- as.data.frame(Synth_Practice)
Synth_Practice$P_02 <- ifelse(Synth_Practice$P_02 > 0, 0,0)
Synth_Practice$P_03 <- ifelse(Synth_Practice$P_03 > 0, 0,0)
Synth_Practice$P_04 <- ifelse(Synth_Practice$P_04 > 0, 0,0)
Synth_Practice$P_05 <- ifelse(Synth_Practice$P_05 > 0, 0,0)
Synth_Practice$P_06 <- ifelse(Synth_Practice$P_06 > 0, 0,0)
Synth_Practice$D <- (Synth_Practice$P_02 + Synth_Practice$P_03 + Synth_Practice$P_04 + Synth_Practice$P_05 + 
                       Synth_Practice$P_06 + Synth_Practice$P_07 + Synth_Practice$P_08 + Synth_Practice$P_09 + 
                       Synth_Practice$P_10 + Synth_Practice$P_11 + Synth_Practice$P_12 + Synth_Practice$P_13 + 
                       Synth_Practice$P_14 + Synth_Practice$P_15)
Synth_Practice <- Synth_Practice[-c(7:20)]


make_balanced <- function(x, years=14, id="GEOID", year="Year") {
  xlist <- split(x, x[,id])
  new <- list()
  dat <- lapply(xlist, function(z) if(length(unique(z[,year])) == years) {new <- z} )
  dat_ <- do.call(rbind, dat)
  return(dat_)
}


synth_balanced <- make_balanced(Synth_Practice)

system.time(
  out <- gsynth(Level ~ D + LIHTC_Units + PUB_Units, data = synth_balanced, 
                index = c("GEOID","Year"), force = "unit",
                CV = FALSE, r = 0 , se = TRUE, 
                inference = "parametric", nboots = 1000,
                parallel = FALSE)
)
