

rm(list=ls()); gc()

setwd("/data2/morgante_lab/ukbiobank_projects/GxE")

library(data.table)





##### Load variable description #####

dat_codes <- fread("/data2/morgante_lab/data/ukbiobank/variables_processed/UKB_selected_variables.csv", data.table = FALSE)
dat_codes <- subset(dat_codes, to_be_kept=="Yes")





##### Load data #####

dat <- fread("/data2/morgante_lab/data/ukbiobank/variables_processed/ukb45105_selected.csv", data.table = FALSE)
dim(dat)
colnames(dat) <- paste("V", colnames(dat), sep='')
ncol(dat)==nrow(dat_codes)




# 1230	6177-0.0	226928	Categorical (multiple)





#############
##### P #####
#############


dat$PR0 <- dat$'V102-0.0'
dat$PR1 <- dat$'V102-0.1'

dat$DP0 <- dat$DP0a <- dat$'V4079-0.0'
dat$DP1 <- dat$DP1a <- dat$'V4079-0.1'
dat$SP0 <- dat$SP0a <- dat$'V4080-0.0'
dat$SP1 <- dat$SP1a <- dat$'V4080-0.1'
dat$PP0 <- dat$SP0-dat$DP0
dat$PP1 <- dat$SP1-dat$DP1

# Compute mean of 2 blood pressure measurements #
dat$PPm <- (dat$PP0+dat$PP1)/2

phen <- c("pulse_rate_automatic_00", "pulse_rate_automatic_01", 
          "diast_blood_00", "diast_blood_01",
          "sist_blood_00", "sist_blood_01",
          "pulse_pressure_00", "pulse_pressure_01",
          "pulse_pressure_mean")
phen <- c("PR0", "PR1",   "DP0", "DP1",   "SP0", "SP1",   "PP0", "PP1",   "PPm",    "DP0a", "DP1a",   "SP0a", "SP1a")
data.frame(phen,phen%in%colnames(dat))







#############
##### I #####
#############

# ID  # eid
dat$ID <- dat$Veid

# Sex # V31-0.0
dat$Sex_SI <- dat$'V31-0.0'
dat$Sex_gen <- dat$'V22001-0.0'

# Year of Birth # V34-0.0
dat$YOB <- dat$'V34-0.0'

# Month of Birth # V52-0.0
dat$MOB <- dat$'V52-0.0'

# Date of phenotyping # V53-0.0
dat$DOF <- dat$'V53-0.0'

# Phenotyping center # V54-0.0
dat$COF <- dat$'V54-0.0'

# Ethnicity # 'V21000-0.0' (ethnicity) 'V22006-0.0' (grouped as Caucasian)
dat$ethn1 <- dat$'V21000-0.0'
dat$ethn1 <- replace(dat$ethn1, dat$ethn1%in%c(-1,-3), NA)
dat$ethn2 <- dat$'V22006-0.0'
dat$ethn1_whbri <- ifelse(dat$ethn1%in%c(   1001                  ), 1, 0)
dat$ethn1_white <- ifelse(dat$ethn1%in%c(1, 1001, 2001, 3001, 4001), 1, 0)
dat$ethn1_mixed <- ifelse(dat$ethn1%in%c(2, 1002, 2002, 3002, 4002), 1, 0)
dat$ethn1_asian <- ifelse(dat$ethn1%in%c(3, 1003, 2003, 3003, 4003), 1, 0)
dat$ethn1_black <- ifelse(dat$ethn1%in%c(4, 1004, 2004, 3004, 4004), 1, 0)


# Age phenotyping, recruitment # V21003-0.0 V21022-0.0
dat$AOP <- dat$'V21003-0.0'
dat$AOR <- dat$'V21022-0.0'

Ivars <- c("ID", "Sex_SI", "Sex_gen", "YOB", "MOB", "DOF", "COF", "ethn1", "ethn1_white", "ethn1_whbri", "ethn1_mixed", "ethn1_asian", "ethn1_black", "ethn2", "AOP", "AOR")
data.frame(Ivars,Ivars%in%colnames(dat))












#############
##### E #####
#############


##### Body #####

# Body Mass Index # V21001-0.0
dat$BMI1 <- dat$'V21001-0.0'
dat$BMI2 <- dat$'V23104-0.0'

# Weight # V21002-0.0 V23098-0.0
dat$weight1 <- dat$'V21002-0.0'
dat$weight2 <- dat$'V23098-0.0'

# Waist circumference # V48-0.0
dat$waist <- dat$'V48-0.0'

# Hip circumference # V49-0.0
dat$hip <- dat$'V49-0.0'

#Standing, seating height # V50-0.0 V51-0.0
dat$height_stan <- dat$'V50-0.0'
#dat$height_seat <- dat$'V51-0.0'

# Knowledge about birth weight # V120-0.0

# weight change 1 year # V2306-0.0
dat$wei_cha <- dat$'V2306-0.0'
dat$wei_cha <- replace(dat$wei_cha, dat$wei_cha%in%c(-3,-1), NA)
dat$wei_cha_same <- ifelse(dat$wei_cha==0, 1, 0)
dat$wei_cha_gain <- ifelse(dat$wei_cha==2, 1, 0)
dat$wei_cha_lost <- ifelse(dat$wei_cha==3, 1, 0)

# Body fat percentage # V23099-0.0
dat$BFP <- dat$'V23099-0.0'

# Body fat mass # V23100-0.0
dat$BFM <- dat$'V23100-0.0'

# Body fat-free mass # V23101-0.0
dat$BLM <- dat$'V23101-0.0'

# Body water mass # V23102-0.0
dat$BWM <- dat$'V23102-0.0'

# Basal metabolic rate # V23105-0.0
dat$BMR <- dat$'V23105-0.0'

# Impedance of whole body # V23106-0.0
dat$IWB <- dat$'V23106-0.0'

# Trunk fat percentage # V23127-0.0
dat$TFP <- dat$'V23127-0.0'

# Trunk fat mass # V23128-0.0
dat$TFM <- dat$'V23128-0.0'


Ebody <- c("BMI1", "BMI2", "weight1", "weight2", "waist", "hip", "height_stan", "wei_cha", "wei_cha_same", "wei_cha_gain", "wei_cha_lost", "BFP", "BFM", "BLM", "BWM", "BMR", "IWB", "TFP", "TFM") # "height_seat", 
data.frame(Ebody,Ebody%in%colnames(dat))







##### Illness #####

# Cancer year/age first occurred # table(dat$'V87-0.0')

# Number of cancer, non-cancer, medications self-reported # V134-0.0 V135-0.0 V137-0.0
dat$cancer1 <- ifelse(dat$'V134-0.0'==0, 0, 1)
dat$cancer2 <- dat$'V2453-0.0'; dat$cancer2 <- replace(dat$cancer2, dat$'V2453-0.0'<0, NA)
dat$disease <- ifelse(dat$'V135-0.0'==0, 0, 1)
dat$medicat1 <- dat$'V137-0.0'
dat$medicat2 <- dat$'V2492-0.0'; dat$medicat2 <- replace(dat$medicat2, dat$'V2492-0.0'<0, NA)

# medications for pain, constipation, heartburn # V6154-0.0
dat$medicat3 <- 0
dat$medicat3 <- replace(dat$medicat3, dat$'V6154-0.0'%in%c(-7), 0)
dat$medicat3 <- replace(dat$medicat3, dat$'V6154-0.0'%in%c(-1, -3), NA)
dat$medicat3 <- replace(dat$medicat3, dat$'V6154-0.0'%in%c(1:6), 1)

# Falls in the last year # V2296-0.0
dat$falls <- 0
dat$falls <- replace(dat$falls, dat$'V2296-0.0'%in%c(-3), NA)
dat$falls <- replace(dat$falls, dat$'V2296-0.0'%in%c(1), 0)
dat$falls <- replace(dat$falls, dat$'V2296-0.0'%in%c(2,3), 1)



# Medication for cholesterol, blood pressure or diabetes

# 6153-0.0	270845	Categorical (multiple)	Medication for cholesterol, blood pressure, diabetes, or take exogenous hormones
dat$mediblood1 <- 0
dat$mediblood1 <- replace(dat$mediblood1, dat$'V6153-0.0'%in%c(-7,1,3), 0)
dat$mediblood1 <- replace(dat$mediblood1, dat$'V6153-0.0'%in%c(-1, -3), NA)
dat$mediblood1 <- replace(dat$mediblood1, dat$'V6153-0.0'%in%c(2), 1)

# 6177-0.0	226928	Categorical (multiple)
dat$mediblood2 <- 0
dat$mediblood2 <- replace(dat$mediblood2, dat$'V6177-0.0'%in%c(-7,1,3), 0)
dat$mediblood2 <- replace(dat$mediblood2, dat$'V6177-0.0'%in%c(-1, -3), NA)
dat$mediblood2 <- replace(dat$mediblood2, dat$'V6177-0.0'%in%c(2), 1)





# diabetes diagnoses # V2443-0.0
dat$diab <- dat$'V2443-0.0'; dat$diab <- replace(dat$diab, dat$diab<0, NA)

# vascular problems # 
dat$vascul <- 0
dat$vascul <- replace(dat$vascul, dat$'V6150-0.0'%in%c(-3), NA)
dat$vascul <- replace(dat$vascul, dat$'V6150-0.0'%in%c(-7), 0)
dat$vascul <- replace(dat$vascul, dat$'V6150-0.0'%in%c(1,2,3,4), 1)

# doctor diagnosis # V6152-0.0
dat$doctor <- 0
dat$doctor <- replace(dat$doctor, dat$'V6152-0.0'%in%c(-3), NA)
dat$doctor <- replace(dat$doctor, dat$'V6152-0.0'%in%c(-7), 0)
dat$doctor <- replace(dat$doctor, dat$'V6152-0.0'%in%c(1:9), 1)

# Non cancer illness, treatments, age at # V20002-0.0 V20003-0.0 V20009-0.0
dat$illness <- dat$'V20002-0.0'
dat$treat <- dat$'V20003-0.0'
dat$ageill <- dat$'V20009-0.0'
dat$ageill <- replace(dat$ageill, dat$ageill%in%c(-1,-3), NA)

# FEV1, FVC #  V20150-0.0 V20151-0.0
dat$FEV1 <- dat$'V20150-0.0'
dat$FVC <- dat$'V20151-0.0'

Eill <- c("cancer1", "cancer2", "disease", "medicat1", "medicat2", "medicat3", "mediblood1", "mediblood2", "falls", "diab", "vascul", "doctor", "illness", "treat", "ageill", "FEV1", "FVC")
data.frame(Eill,Eill%in%colnames(dat))







##### Life #####

# Latitude and longitude of place of birth # V129-0.0 V130-0.0
dat$latit <- dat$'V129-0.0'
dat$latit <- replace(dat$latit, dat$latit%in%c(-1), NA)
dat$longi <- dat$'V130-0.0'
dat$longi <- replace(dat$longi, dat$longi%in%c(-1), NA)

# Townsend deprivation index at recruitment # V189-0.0
dat$Townsend <- dat$'V189-0.0'

# Age completed full time education # V845-0.0
dat$education_age <- dat$'V845-0.0'
dat$education_age <- replace(dat$education_age, dat$education_age<0, NA)

#Country of birth (UK/elsewhere) # V1647-0.0 V1677-0.0 V1717-0.0 V1727-0.0 V1737-0.0 V1747-0.0 V1767-0.0 V1777-0.0 V1787-0.0
dat$country <- dat$'V1647-0.0'
dat$country <- replace(dat$country, dat$country%in%c(-1,-3), NA)
dat$bornEng <- ifelse(dat$'V1647-0.0'==1, 1, 0)

#Breastfed as a baby
dat$breastfed <- dat$'V1677-0.0'
dat$breastfed <- replace(dat$breastfed, dat$'V1677-0.0'%in%c(-1,-3), NA)


#Skin colour
dat$skincolour <- dat$'V1717-0.0'
dat$skincolour <- replace(dat$skincolour, dat$'V1717-0.0'%in%c(-1,-3), NA)

#Ease of skin tanning
dat$skintanning <- dat$'V1727-0.0'
dat$skintanning <- replace(dat$skintanning, dat$'V1727-0.0'%in%c(-1,-3), NA)

#Childhood sunburn occasions
dat$childsunburn <- dat$'V1737-0.0'
dat$childsunburn <- replace(dat$childsunburn, dat$'V1737-0.0'%in%c(-1,-3), NA)

#Hair colour (natural, before greying)
dat$haircolour <- dat$'V1747-0.0'  
dat$haircolour <- replace(dat$haircolour, dat$'V1747-0.0'%in%c(-1,-3), NA)

#Adopted as a child
dat$adoptedchild <- dat$'V1767-0.0'
dat$adoptedchild <- replace(dat$adoptedchild, dat$'V1767-0.0'%in%c(-1,-3), NA)

#Part of a multiple birth
dat$multiplebirth <- dat$'V1777-0.0'
dat$multiplebirth <- replace(dat$multiplebirth, dat$'V1777-0.0'%in%c(-1,-3), NA)

#Maternal smoking around birth
dat$mothersmoking <- dat$'V1787-0.0'
dat$mothersmoking <- replace(dat$mothersmoking, dat$'V1787-0.0'%in%c(-1,-3), NA)

# Overall health rating, # V2178-0.0
dat$OHR <- dat$'V2178-0.0'
dat$OHR1 <- ifelse(dat$OHR==1, 1, 0)
dat$OHR2 <- ifelse(dat$OHR==2, 1, 0)
dat$OHR3 <- ifelse(dat$OHR==3, 1, 0)
dat$OHR4 <- ifelse(dat$OHR==4, 1, 0)
dat$OHR <- replace(dat$OHR, dat$OHR%in%c(-1,-3), NA)

# Long stadning illness # V2188-0.0
dat$LSI <- dat$'V2188-0.0'
dat$LSI <- replace(dat$LSI, dat$'V2188-0.0'%in%c(-1,-3), NA)

# use of sun protection # V2267-0.0
dat$USP <- dat$'V2267-0.0'
dat$USP1 <- ifelse(dat$USP==1, 1, 0)
dat$USP2 <- ifelse(dat$USP==2, 1, 0)
dat$USP3 <- ifelse(dat$USP==3, 1, 0)
dat$USP4 <- ifelse(dat$USP==4, 1, 0)
dat$USP5 <- ifelse(dat$USP==5, 1, 0)
dat$USP <- replace(dat$USP, dat$USP%in%c(-1,-3), NA)

# Frequency of solarium use # V2277-0.0
dat$FSU <- dat$'V2277-0.0'   # ?????????
dat$FSU <- replace(dat$FSU, dat$FSU%in%c(-1,-3), NA)

# degree, qualification # V6138-0.0
dat$collegedegree <- ifelse(dat$'V6138-0.0'==1, 1, 0)
dat$collegedegree <- replace(dat$collegedegree, dat$'V6138-0.0'%in%c(-3), NA)


Elife <- c("latit", "longi", "Townsend", "education_age", "country", "bornEng", "breastfed", "skincolour", "skintanning", "childsunburn", "haircolour", "adoptedchild", "multiplebirth", "mothersmoking", "OHR", "OHR1", "OHR2", "OHR3", "OHR4", "LSI", "USP", "USP1", "USP2", "USP3", "USP4", "USP5", "FSU", "collegedegree")
data.frame(Elife,Elife%in%colnames(dat))





##### Environmental pollution #####

# home area population density # V20118-0.0
dat$HAPD <- dat$'V20118-0.0'   # ????????

# Nitrogen dioxide air pollution; 2010
dat$NO2_2010 <- dat$'V24003-0.0'
# Nitrogen dioxide air pollution; 2005
dat$NO2_2005 <- dat$'V24016-0.0'
# Nitrogen dioxide air pollution; 2006
dat$NO2_2006 <- dat$'V24017-0.0'
# Nitrogen dioxide air pollution; 2007
dat$NO2_2007 <- dat$'V24018-0.0'

# Nitrogen oxides air pollution; 2010
dat$NO1_2010 <- dat$'V24004-0.0'

# Particulate matter air pollution (pm10); 2010
dat$PM10_2010 <- dat$'V24005-0.0'
# Particulate matter air pollution (pm10); 2007
dat$PM10_2007 <- dat$'V24019-0.0'

# Particulate matter air pollution (pm2.5); 2010
dat$PM25_2010 <- dat$'V24006-0.0'

# Particulate matter air pollution (pm2.5) absorbance; 2010
dat$PM25A_2010 <- dat$'V24007-0.0'

# Particulate matter air pollution 2.5-10um; 2010
dat$PM2510_2010 <- dat$'V24008-0.0'

# Traffic intensity on the nearest road
dat$TINR <- dat$'V24009-0.0'   # ??????????????

# Inverse distance to the nearest road
dat$IDNR <- dat$'V24010-0.0'

# Traffic intensity on the nearest major road
dat$TINmR <- dat$'V24011-0.0'

# Inverse distance to the nearest major road
dat$IDNmR <- dat$'V24012-0.0'

# Total traffic load on major roads
dat$TTLmR <- dat$'V24013-0.0'   # ?????????????????

# Close to major road
dat$CmR <- dat$'V24014-0.0'

# Sum of road length of major roads within 100m
dat$SRL100m <- dat$'V24015-0.0'  # ????????????????


# Average daytime sound level of noise pollution
dat$noise1 <- dat$'V24020-0.0'

# Average evening sound level of noise pollution
dat$noise2 <- dat$'V24021-0.0'

# Average night-time sound level of noise pollution
dat$noise3 <- dat$'V24022-0.0'

# Average 16-hour sound level of noise pollution
dat$noise4 <- dat$'V24023-0.0'

# Average 24-hour sound level of noise pollution
dat$noise5 <- dat$'V24024-0.0'


Epoll <- c("HAPD", "NO2_2010", "NO2_2005", "NO2_2006", "NO2_2007", "NO1_2010", "PM10_2007", "PM10_2010", "PM25_2010", "PM25A_2010", "PM2510_2010", "TINR", "IDNR", "TINmR", "IDNmR", "TTLmR", "CmR", "SRL100m", "noise1", "noise2", "noise3", "noise4", "noise5")
data.frame(Epoll,Epoll%in%colnames(dat))









##### Physical activity #####


# Number of days/week walked 10+ minutes
dat$walk_d <- dat$'V864-0.0'
dat$walk_d <- replace(dat$walk_d, dat$walk_d<0, NA)

# Duration of walks
dat$walk_t1 <- dat$'V874-0.0'
dat$walk_t1 <- replace(dat$walk_t1, dat$walk_t1<0, NA)

# Number of days/week of moderate physical activity 10+ minutes
dat$act0_d <- dat$'V884-0.0'
dat$act0_d <- replace(dat$act0_d, dat$act0_d<0, NA)

# Duration of moderate activity
dat$act0_t <- dat$'V894-0.0'
dat$act0_t <- replace(dat$act0_t, dat$act0_t<0, NA)

# Number of days/week of vigorous physical activity 10+ minutes
dat$act1_d <- dat$'V904-0.0'
dat$act1_d <- replace(dat$act1_d, dat$act1_d<0, NA)

# Duration of vigorous activity
dat$act1_t <- dat$'V914-0.0'
dat$act1_t <- replace(dat$act1_t, dat$act1_t<0, NA)

# Usual walking pace
dat$walk_p <- dat$'V924-0.0'
dat$walk_p <- replace(dat$walk_p, dat$walk_p<0, NA)

# Frequency of stair climbing in last 4 weeks
dat$stair_climb <- dat$'V943-0.0'
dat$stair_climb <- replace(dat$stair_climb, dat$stair_climb<0, NA)

# Frequency of walking for pleasure in last 4 weeks
dat$walk_f <- dat$'V971-0.0'
dat$walk_f <- replace(dat$walk_f, dat$walk_f<0, NA)

# Duration walking for pleasure
dat$walk_t2 <- dat$'V981-0.0'
dat$walk_t2 <- replace(dat$walk_t2, dat$walk_t2<0, NA)

# Time spent outdoors in the summer
dat$outdoor_s <- dat$'V1050-0.0'
dat$outdoor_s <- replace(dat$outdoor_s, dat$outdoor_s<0, NA)
dat$outdoor_s <- replace(dat$outdoor_s, dat$outdoor_s==(-10), 0)

# Time spent outdoors in the winter
dat$outdoor_w <- dat$'V1060-0.0'
dat$outdoor_w <- replace(dat$outdoor_w, dat$outdoor_w<0, NA)
dat$outdoor_w <- replace(dat$outdoor_w, dat$outdoor_w==(-10), 0)

# Time spent watching TV
dat$TVtime <- dat$'V1070-0.0'
dat$TVtime <- replace(dat$TVtime, dat$TVtime<0, NA)
dat$TVtime <- replace(dat$TVtime, dat$TVtime==(-10), 0)

# Time spent using computer
dat$PCtime <- dat$'V1080-0.0'
dat$PCtime <- replace(dat$PCtime, dat$PCtime<0, NA)
dat$PCtime <- replace(dat$PCtime, dat$PCtime==(-10), 0)

# Time spent driving
dat$DRtime <- dat$'V1090-0.0'
dat$DRtime <- replace(dat$DRtime, dat$DRtime<0, NA)
dat$DRtime <- replace(dat$DRtime, dat$DRtime==(-10), 0)

# Type of transport
dat$transp <- dat$'V6162-0.0'
dat$transp_car <- ifelse(dat$transp==1, 1, 0)
dat$transp_wlk <- ifelse(dat$transp==2, 1, 0)
dat$transp_pub <- ifelse(dat$transp==3, 1, 0)
dat$transp_cyc <- ifelse(dat$transp==4, 1, 0)
dat$transp <- replace(dat$transp, dat$transp%in%c(-3,-7), NA)

# Type of physical activity
dat$phys1 <- dat$'V6164-0.0'
dat$phys1_wlk <- ifelse(dat$phys1==1, 1, 0)
dat$phys1_oth <- ifelse(dat$phys1==2, 1, 0)
dat$phys1_str <- ifelse(dat$phys1==3, 1, 0)
dat$phys1_lig <- ifelse(dat$phys1==4, 1, 0)
dat$phys1_hea <- ifelse(dat$phys1==5, 1, 0)
dat$phys2 <- dat$'V6164-0.1'
dat$phys2_wlk <- ifelse(dat$phys2==1, 1, 0)
dat$phys2_oth <- ifelse(dat$phys2==2, 1, 0)
dat$phys2_str <- ifelse(dat$phys2==3, 1, 0)
dat$phys2_lig <- ifelse(dat$phys2==4, 1, 0)
dat$phys2_hea <- ifelse(dat$phys2==5, 1, 0)



Ephys <- c("walk_d", "walk_t1", "act0_d", "act0_t", "act1_d", "act1_t", "walk_p", "stair_climb", "walk_f", "walk_t2", "outdoor_s", "outdoor_w", "TVtime", "PCtime", "DRtime", "transp", "transp_car", "transp_wlk", "transp_pub", "transp_cyc", "phys1", "phys1_wlk", "phys1_oth", "phys1_str", "phys1_lig", "phys1_hea", "phys2", "phys2_wlk", "phys2_oth", "phys2_str", "phys2_lig", "phys2_hea")
data.frame(Ephys,Ephys%in%colnames(dat))





##### Resting habit #####

# Sleep duration
dat$sleep_d <- dat$'V1160-0.0'
dat$sleep_d <- replace(dat$sleep_d, dat$sleep_d%in%c(-3,-1), NA)

# Getting up in the morning, 6 classes
dat$getup <- dat$'V1170-0.0'
dat$getup <- replace(dat$getup, dat$getup%in%c(-3,-1), NA)

# Chronotype, 6 classes
dat$chrono <- dat$'V1180-0.0'
dat$chrono <- replace(dat$chrono, dat$chrono%in%c(-3,-1), NA)

# Nap during the day
dat$nap <- dat$'V1190-0.0'
dat$nap <- replace(dat$nap, dat$nap%in%c(-3,-1), NA)

# Insomnia
dat$insomnia <- dat$'V1200-0.0'
dat$insomnia <- replace(dat$insomnia, dat$insomnia%in%c(-3,-1), NA)

# Snoring
dat$snoring <- dat$'V1210-0.0'
dat$snoring <- replace(dat$snoring, dat$snoring%in%c(-3,-1), NA)

# narcolepsy
dat$narcolepsy <- dat$'V1220-0.0'
dat$narcolepsy <- replace(dat$narcolepsy, dat$narcolepsy%in%c(-3,-1), NA)

Esleep <- c("sleep_d", "getup", "chrono", "nap", "insomnia", "snoring", "narcolepsy")
data.frame(Esleep,Esleep%in%colnames(dat))









##### Smoking #####

# Ever smoked
dat$smoked_ever <- dat$'V20160-0.0'

# Tobacco smoking, current
dat$smoking_now <- ifelse(dat$'V1239-0.0'>0, 1, 0)
dat$smoking_now <- replace(dat$smoking_now, dat$'V1239-0.0'<0, NA)

# Tobacco smoking, past
dat$smoked_past <- ifelse(dat$'V1249-0.0'%in%c(1,2,3), 1, 0) 

# Tobacco smoking, in the household
dat$smoke_house <- ifelse(dat$'V1259-0.0'>0, 1, 0)
dat$smoke_house <- replace(dat$smoke_house, dat$'V1259-0.0'<0, NA)

# Tobacco smoking, home exposure
dat$smoke_home <- dat$'V1269-0.0'   # ??????????

# Tobacco smoking, outside exposure
dat$smoke_out <- dat$'V1279-0.0'   # ??????????

# Smoking status
dat$smoke_status <- dat$'V20116-0.0'
dat$smoke_status_past <- ifelse(dat$'V20116-0.0'==1, 1, 0) 
dat$smoke_status_now <- ifelse(dat$'V20116-0.0'==2, 1, 0) 
dat$smoke_status_never <- ifelse(dat$'V20116-0.0'==0, 1, 0) 

Esmoke <- c("smoked_ever", "smoking_now", "smoked_past", "smoke_house", "smoke_home", "smoke_out", "smoke_status", "smoke_status_past", "smoke_status_now", "smoke_status_never")
data.frame(Esmoke,Esmoke%in%colnames(dat))







##### Food #####

# Cooked vegetable intake
dat$veg_cook <- dat$'V1289-0.0'
dat$veg_cook <- replace(dat$veg_cook, dat$veg_cook<0, NA)
dat$veg_cook <- replace(dat$veg_cook, dat$veg_cook==(-10), 0)

# Raw vegetable intake
dat$veg_raw <- dat$'V1299-0.0'
dat$veg_raw <- replace(dat$veg_raw, dat$veg_raw<0, NA)
dat$veg_raw <- replace(dat$veg_raw, dat$veg_raw==(-10), 0)

# Fresh fruit intake
dat$fruit_fresh <- dat$'V1309-0.0'
dat$fruit_fresh <- replace(dat$fruit_fresh, dat$fruit_fresh<0, NA)
dat$fruit_fresh <- replace(dat$fruit_fresh, dat$fruit_fresh==(-10), 0)

# Dried fruit intake
dat$fruit_dried <- dat$'V1319-0.0'
dat$fruit_dried <- replace(dat$fruit_dried, dat$fruit_dried<0, NA)
dat$fruit_dried <- replace(dat$fruit_dried, dat$fruit_dried==(-10), 0)

# Oily fish intake
dat$fish_oily <- dat$'V1329-0.0'
dat$fish_oily <- replace(dat$fish_oily, dat$fish_oily<0, NA)
dat$fish_oily <- replace(dat$fish_oily, dat$fish_oily==(-10), 0)

# Non-Oily fish intake
dat$fish_lean <- dat$'V1339-0.0'
dat$fish_lean <- replace(dat$fish_lean, dat$fish_lean<0, NA)
dat$fish_lean <- replace(dat$fish_lean, dat$fish_lean==(-10), 0)

# Processed meat intake
dat$meat_proc <- dat$'V1349-0.0'
dat$meat_proc <- replace(dat$meat_proc, dat$meat_proc<0, NA)
dat$meat_proc <- replace(dat$meat_proc, dat$meat_proc==(-10), 0)

# Poultry intake
dat$poultry <- dat$'V1359-0.0'
dat$poultry <- replace(dat$poultry, dat$poultry<0, NA)
dat$poultry <- replace(dat$poultry, dat$poultry==(-10), 0)

# Beef intake
dat$beef <- dat$'V1369-0.0'
dat$beef <- replace(dat$beef, dat$beef<0, NA)
dat$beef <- replace(dat$beef, dat$beef==(-10), 0)

# Lamb intake
dat$lamb <- dat$'V1379-0.0'
dat$lamb <- replace(dat$lamb, dat$lamb<0, NA)
dat$lamb <- replace(dat$lamb, dat$lamb==(-10), 0)

# Pork intake
dat$pork <- dat$'V1389-0.0'
dat$pork <- replace(dat$pork, dat$pork<0, NA)
dat$pork <- replace(dat$pork, dat$pork==(-10), 0)

# Cheese intake
dat$cheese <- dat$'V1408-0.0'
dat$cheese <- replace(dat$cheese, dat$cheese<0, NA)
dat$cheese <- replace(dat$cheese, dat$cheese==(-10), 0)

# Milk type used
dat$milk <- dat$'V1418-0.0'
dat$milk <- replace(dat$milk, dat$milk<0, NA)
dat$milk <- replace(dat$milk, dat$milk==(-10), 0)

# Spread type used
dat$spread <- dat$'V1428-0.0'
dat$spread <- replace(dat$spread, dat$spread<0, NA)
dat$spread <- replace(dat$spread, dat$spread==(-10), 0)

# Bread intake
dat$bread <- dat$'V1438-0.0'
dat$bread <- replace(dat$bread, dat$bread<0, NA)
dat$bread <- replace(dat$bread, dat$bread==(-10), 0)

# Bread type
dat$bread0 <- dat$'V1448-0.0'   # ????????
dat$bread0 <- replace(dat$bread0, dat$'V1448-0.0'%in%c(-3), NA)

# Cereal intake
dat$cereal <- dat$'V1458-0.0'
dat$cereal <- replace(dat$cereal, dat$cereal<0, NA)
dat$cereal <- replace(dat$cereal, dat$cereal==(-10), 0)

# Cereal type
dat$cereal0 <- dat$'V1468-0.0'   # ????????
dat$cereal0 <- replace(dat$cereal0, dat$'V1468-0.0'%in%c(-3), NA)

# Salt added to food
dat$salt <- dat$'V1478-0.0'
dat$salt <- replace(dat$salt, dat$salt<0, NA)
dat$salt <- replace(dat$salt, dat$salt==(-10), 0)

# Tea intake
dat$tea <- dat$'V1488-0.0'
dat$tea <- replace(dat$tea, dat$tea<0, NA)
dat$tea <- replace(dat$tea, dat$tea==(-10), 0)

# Coffee intake
dat$coffee <- dat$'V1498-0.0'
dat$coffee <- replace(dat$coffee, dat$coffee<0, NA)
dat$coffee <- replace(dat$coffee, dat$coffee==(-10), 0)

# Coffee type
dat$coffee0 <- dat$'V1508-0.0'   # ????????
dat$coffee0 <- replace(dat$coffee0, dat$'V1508-0.0'%in%c(-3), NA)

# Hot drink temperature
dat$drinkhot <- dat$'V1518-0.0'
dat$drinkhot <- replace(dat$drinkhot, dat$drinkhot<0, NA)

# Water intake
dat$water <- dat$'V1528-0.0'
dat$water <- replace(dat$water, dat$water<0, NA)
dat$water <- replace(dat$water, dat$water==(-10), 0)

# Diet changes, vartiation in the diet
dat$dietCh1 <- dat$'V1538-0.0'
dat$dietCh1 <- replace(dat$dietCh1, dat$'V1538-0.0'%in%c(0), 0)
dat$dietCh1 <- replace(dat$dietCh1, dat$'V1538-0.0'%in%c(1,2), 1)
dat$dietCh1 <- replace(dat$dietCh1, dat$'V1538-0.0'%in%c(-3), NA)
dat$dietCh2 <- dat$'V1548-0.0'
dat$dietCh2 <- replace(dat$dietCh2, dat$'V1548-0.0'%in%c(-3,-1), NA)

# eggs dairy wheat sugar consumption
dat$eggsNO <- ifelse(dat$'V6144-0.0'%in%c(1), 1, 0)
dat$dairyNO <- ifelse(dat$'V6144-0.0'%in%c(2), 1, 0)
dat$wheatNO <- ifelse(dat$'V6144-0.0'%in%c(3), 1, 0)
dat$sugarNO <- ifelse(dat$'V6144-0.0'%in%c(4), 1, 0)
dat$eatEDWS <- ifelse(dat$'V6144-0.0'%in%c(5), 1, 0)


# Vitamin and mineral supplements
dat$vit0 <- 0
dat$vit0 <- replace(dat$vit0, dat$'V6155-0.0'>1, 1)
dat$vit0 <- replace(dat$vit0, dat$'V6155-0.0'==c(-7), 1)
dat$vit0 <- replace(dat$vit0, dat$'V6155-0.0'==c(-3), NA)

dat$vit1 <- 0
dat$vit1 <- replace(dat$vit1, dat$'V6179-0.0'>1, 1)
dat$vit1 <- replace(dat$vit1, dat$'V6179-0.0'==c(-7), 1)
dat$vit1 <- replace(dat$vit1, dat$'V6179-0.0'==c(-3), NA)



Efood <- c("veg_cook", "veg_raw", "fruit_fresh", "fruit_dried", "fish_oily", "fish_lean", "meat_proc", "poultry", "beef", "lamb", "pork", "cheese", "milk", "spread", "bread", "bread0", "cereal", "cereal0", "salt", "tea", "coffee", "coffee0", "drinkhot", "water", "dietCh1", "dietCh2", "eggsNO", "dairyNO", "wheatNO", "sugarNO", "eatEDWS", "vit0", "vit1")
data.frame(Efood,Efood%in%colnames(dat))











##### Alcohol #####

# Alcohol intake frequency
dat$alc1 <- dat$'V1558-0.0'
dat$alc1 <- replace(dat$alc1, dat$alc1<0, NA)

# Average weekly red wine intake
dat$alc2 <- dat$'V1568-0.0'
dat$alc2 <- replace(dat$alc2, dat$alc2%in%c(-3,-1), NA)

# Average weekly champagne plus white wine intake
dat$alc3 <- dat$'V1578-0.0'
dat$alc3 <- replace(dat$alc3, dat$alc3%in%c(-3,-1), NA)

# Average weekly beer plus cider intake
dat$alc4 <- dat$'V1588-0.0'
dat$alc4 <- replace(dat$alc4, dat$alc4%in%c(-3,-1), NA)

# Average weekly spirits intake
dat$alc5 <- dat$'V1598-0.0'
dat$alc5 <- replace(dat$alc5, dat$alc5%in%c(-3,-1), NA)

# Average weekly fortified wine intake
dat$alc6 <- dat$'V1608-0.0'
dat$alc6 <- replace(dat$alc6, dat$alc6%in%c(-3,-1), NA)

# Alcohol usually taken with meals
dat$alc7 <- dat$'V1618-0.0'
dat$alc7 <- replace(dat$alc7, dat$alc7%in%c(-3,-1), NA)
dat$alc7 <- replace(dat$alc7, dat$alc7%in%c(-6), 0.5)

# Alcohol intake versus 10 years previously
dat$alc8 <- dat$'V1628-0.0'
dat$alc8 <- replace(dat$alc8, dat$alc8%in%c(-3,-1), NA)

# alcohol drinker status
dat$alc9 <- dat$'V20117-0.0'
dat$alc9 <- replace(dat$alc9, dat$alc9%in%c(-3,-1), NA)
dat$alcoh_status_past <- ifelse(dat$'V20117-0.0'==1, 1, 0) 
dat$alcoh_status_now <- ifelse(dat$'V20117-0.0'==2, 1, 0) 
dat$alcoh_status_never <- ifelse(dat$'V20117-0.0'==0, 1, 0) 


Ealcohol <- c("alc1", "alc2", "alc3", "alc4", "alc5", "alc6", "alc7", "alc8", "alc9", "alcoh_status_past", "alcoh_status_now", "alcoh_status_never")
data.frame(Ealcohol,Ealcohol%in%colnames(dat))


allvars <- c(phen, Ivars, Gvars, Ebody, Eill, Elife, Epoll, Ephys, Esleep, Esmoke, Efood, Ealcohol)
data.frame(allvars,allvars%in%colnames(dat))

dt <- dat[c(phen, Ivars,Ebody, Eill, Elife, Epoll, Ephys, Esleep, Esmoke, Efood, Ealcohol)]
dim(dt)





save(dt, file="datasets/data1_20240209.RData")





rm(list=ls()); gc()




















