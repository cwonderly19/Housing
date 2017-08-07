ss11hwa$GEOID10 <- paste(ss11hwa$ST,ss11hwa$PUMA, sep = "") 
ss11hwa <- ss11hwa[c(207,1:206)]
write.csv(ss11hwa, "C:/Users/cwonderly/Downloads/pumshh11.csv")

ss11pwa <- read_csv("~/Housing/PUMSdata/ss11pwa.csv")
ss11pwa$GEOID10 <- paste(ss11pwa$ST,ss11pwa$PUMA, sep = "") 
ss11pwa <- ss11pwa[c(285,1:284)]
write.csv(ss11pwa, "~/Housing/PUMSdata/ss11pwa_geoid.csv")

ss10hwa <- read_csv("~/Housing/PUMSdata/ss10hwa.csv")
ss10hwa$GEOID10 <- paste(ss10hwa$ST,ss10hwa$PUMA, sep = "") 
ss10hwa <- ss10hwa[c(203,1:202)]
write.csv(ss10hwa, "~/Housing/PUMSdata/ss10hwa_geoid.csv")

ss10pwa <- read_csv("~/Housing/PUMSdata/ss10pwa.csv")
ss10pwa$GEOID10 <- paste(ss10pwa$ST,ss10pwa$PUMA, sep = "") 
ss10pwa <- ss10pwa[c(280,1:279)]
write.csv(ss10pwa, "~/Housing/PUMSdata/ss10pwa_geoid.csv")

ss09hwa <- read_csv("~/Housing/PUMSdata/ss09hwa.csv")
ss09hwa$GEOID10 <- paste(ss09hwa$ST,ss09hwa$PUMA, sep = "") 
ss09hwa <- ss09hwa[c(203,1:202)]
write.csv(ss09hwa, "~/Housing/PUMSdata/ss09hwa_geoid.csv")

ss09pwa <- read_csv("~/Housing/PUMSdata/ss09pwa.csv")
ss09pwa$GEOID10 <- paste(ss09pwa$ST,ss09pwa$PUMA, sep = "") 
ss09pwa <- ss09pwa[c(280,1:279)]
write.csv(ss09pwa, "~/Housing/PUMSdata/ss09pwa_geoid.csv")

ss08hwa <- read_csv("~/Housing/PUMSdata/ss08hwa.csv")
ss08hwa$GEOID10 <- paste(ss08hwa$ST,ss08hwa$PUMA, sep = "") 
ss08hwa <- ss08hwa[c(203,1:202)]
write.csv(ss08hwa, "~/Housing/PUMSdata/ss08hwa_geoid.csv")

ss08pwa <- read_csv("~/Housing/PUMSdata/ss08pwa.csv")
ss08pwa$GEOID10 <- paste(ss08pwa$ST,ss08pwa$PUMA, sep = "") 
ss08pwa <- ss08pwa[c(271,1:270)]
write.csv(ss08pwa, "~/Housing/PUMSdata/ss08pwa_geoid.csv")

ss07hwa <- read_csv("~/Housing/PUMSdata/ss07hwa.csv")
ss07hwa$GEOID10 <- paste(ss07hwa$ST,ss07hwa$PUMA, sep = "") 
ss07hwa <- ss07hwa[c(189,1:188)]
write.csv(ss07hwa, "~/Housing/PUMSdata/ss07hwa_geoid.csv")

ss07pwa <- read_csv("~/Housing/PUMSdata/ss07pwa.csv")
ss07pwa$GEOID10 <- paste(ss07pwa$ST,ss07pwa$PUMA, sep = "") 
ss07pwa <- ss07pwa[c(240,1:239)]
write.csv(ss07pwa, "~/Housing/PUMSdata/ss07pwa_geoid.csv")

ss06hwa <- read_csv("~/Housing/PUMSdata/ss06hwa.csv")
ss06hwa$GEOID10 <- paste(ss06hwa$ST,ss06hwa$PUMA, sep = "") 
ss06hwa <- ss06hwa[c(189,1:188)]
write.csv(ss06hwa, "~/Housing/PUMSdata/ss06hwa_geoid.csv")

ss06pwa <- read_csv("~/Housing/PUMSdata/ss06pwa.csv")
ss06pwa$GEOID10 <- paste(ss06pwa$ST,ss06pwa$PUMA, sep = "") 
ss06pwa <- ss06pwa[c(240,1:239)]
write.csv(ss06pwa, "~/Housing/PUMSdata/ss06pwa_geoid.csv")

ss05hwa <- read_csv("~/Housing/PUMSdata/ss05hwa.csv")
ss05hwa$GEOID10 <- paste(ss05hwa$ST,ss05hwa$PUMA, sep = "") 
ss05hwa <- ss05hwa[c(189,1:188)]
write.csv(ss05hwa, "~/Housing/PUMSdata/ss05hwa_geoid.csv")

ss05pwa <- read_csv("~/Housing/PUMSdata/ss05pwa.csv")
ss05pwa$GEOID10 <- paste(ss05pwa$ST,ss05pwa$PUMA, sep = "") 
ss05pwa <- ss05pwa[c(239,1:238)]
write.csv(ss05pwa, "~/Housing/PUMSdata/ss05pwa_geoid.csv")

ss04hwa <- read_csv("~/Housing/PUMSdata/ss04hwa.csv")
ss04hwa$GEOID10 <- paste(ss04hwa$ST,ss04hwa$PUMA, sep = "") 
ss04hwa <- ss04hwa[c(102,1:101)]
write.csv(ss04hwa, "~/Housing/PUMSdata/ss04hwa_geoid.csv")
