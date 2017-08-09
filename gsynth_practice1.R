# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load installr

# using the package:
updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.



install.packages("gsynth")


####load libraries####
library(gsynth)
library(arm)

## for processing C++ code
require(Rcpp) 
## for plotting
require(ggplot2)  
require(GGally) 
## for parallel computing 
require(foreach)  
require(doParallel) 
require(abind) 

data(gsynth)

out <- gsynth(Y ~ D + X1 + X2, data = simdata, 
              index = c("id","time"), force = "two-way",
              CV = TRUE, r = c(0, 5), se = TRUE, 
              inference = "parametric", nboots = 1000,
              parallel = FALSE)