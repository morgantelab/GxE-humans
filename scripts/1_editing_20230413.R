




rm(list=ls()); gc()

setwd("/data2/morgante_lab/ukbiobank_projects/GxE")

library(data.table)




##### Load individuals to remove #####

inds_to_remove <- fread("/data2/morgante_lab/data/ukbiobank/ind_to_remove/w62347_all.txt", data.table = FALSE)$V1
length(inds_to_remove)
head(inds_to_remove)



##### Load individuals to keep #####

inds_to_keep <- read.table("/data2/morgante_lab/data/ukbiobank/genotypes/array/ukb22418_all_auto_b0_v2_s488243_caucasian_white_british_unrel.fam", header=F)$V1
length(inds_to_keep)
head(inds_to_keep)






##### Load variable assignement #####

load("datasets/data1_20230413.RData")
dtt <- dt; nrow(dtt)




### Subset by ID ###
dtt <- subset(dtt, !(ID%in%inds_to_remove)); nrow(dtt)
dtt <- subset(dtt, ID%in%inds_to_keep); nrow(dtt)





### Subset for phenotypes ###
dtt <- subset(dtt, !is.na(PR0)); nrow(dtt) # pulse rate, automatic

#dtt <- subset(dtt, !is.na(PR1)); nrow(dtt)
dtt <- subset(dtt, !is.na(DP0)); nrow(dtt) # diastolic blood pressure, first reading
dtt <- subset(dtt, !is.na(DP0a)); nrow(dtt) # diastolic blood pressure, first reading
#dtt <- subset(dtt, !is.na(DP1)); nrow(dtt)
dtt <- subset(dtt, !is.na(SP0)); nrow(dtt) # sistolic blood pressure, first reading
dtt <- subset(dtt, !is.na(SP0a)); nrow(dtt) # sistolic blood pressure, first reading
#dtt <- subset(dtt, !is.na(SP1)); nrow(dtt)
dtt <- subset(dtt, !is.na(PP0)); nrow(dtt) # pulse pressure, first reading
#dtt <- subset(dtt, !is.na(PP1)); nrow(dtt)
#dtt <- subset(dtt, !is.na(PPm)); nrow(dtt)

### Subsetting for I vars ###
dtt <- subset(dtt, !is.na(Sex_SI)); nrow(dtt) # sex, self identified
#dtt <- subset(dtt, !is.na(Sex_gen)); nrow(dtt)
dtt <- subset(dtt, !is.na(AOP)); nrow(dtt) # age at phenotyping
dtt <- subset(dtt, !is.na(AOR)); nrow(dtt) # age at recruiment
#dtt <- subset(dtt, !is.na(ethn1)); nrow(dtt)
dtt <- subset(dtt, ethn1_whbri==1); nrow(dtt) # ethnicity white british (self-reported)
dtt <- subset(dtt, ethn2==1); nrow(dtt) # Caucasian cluster by genetic grouping


# G vars
#dtt <- subset(dtt, !is.na(PC1)); nrow(dtt)







# Elife
#dtt <- subset(dtt, !is.na(latit)); nrow(dtt)
#dtt <- subset(dtt, !is.na(longi)); nrow(dtt)
dtt <- subset(dtt, !is.na(Townsend)); nrow(dtt) # Townsend deprivation index at recruitment
#dtt <- subset(dtt, !is.na(education_age)); nrow(dtt)
#dtt <- subset(dtt, !is.na(country)); nrow(dtt)
#dtt <- subset(dtt, !is.na(breastfed)); nrow(dtt)
#dtt <- subset(dtt, !is.na(skincolour)); nrow(dtt)
#dtt <- subset(dtt, !is.na(skintanning)); nrow(dtt)
#dtt <- subset(dtt, !is.na(childsunburn)); nrow(dtt)
#dtt <- subset(dtt, !is.na(haircolour)); nrow(dtt)
#dtt <- subset(dtt, !is.na(adoptedchild)); nrow(dtt)
#dtt <- subset(dtt, !is.na(multiplebirth)); nrow(dtt)
#dtt <- subset(dtt, !is.na(mothersmoking)); nrow(dtt)
#dtt <- subset(dtt, !is.na(OHR)); nrow(dtt)
#dtt <- subset(dtt, !is.na(LSI)); nrow(dtt)
#dtt <- subset(dtt, !is.na(USP)); nrow(dtt)
#dtt <- subset(dtt, !is.na(FSU)); nrow(dtt)
#dtt <- subset(dtt, !is.na(collegedegree)); nrow(dtt)


# Ephys
dtt <- subset(dtt, !is.na(walk_d)); nrow(dtt) # number of days/ week walked 10þ minutes
#dtt <- subset(dtt, !is.na(walk_t1)); nrow(dtt)
dtt <- subset(dtt, !is.na(act0_d)); nrow(dtt) # number of days/week of moderate physical activity 10þ minutes
#dtt <- subset(dtt, !is.na(act0_t)); nrow(dtt)
dtt <- subset(dtt, !is.na(act1_d)); nrow(dtt) # number of days/week of vigorous physical activity 10þ minutes
#dtt <- subset(dtt, !is.na(act1_t)); nrow(dtt)
#dtt <- subset(dtt, !is.na(walk_p)); nrow(dtt)
#dtt <- subset(dtt, !is.na(stair_climb)); nrow(dtt)
#dtt <- subset(dtt, !is.na(walk_f)); nrow(dtt) ##
#dtt <- subset(dtt, !is.na(walk_t2)); nrow(dtt)
#dtt <- subset(dtt, !is.na(outdoor_s)); nrow(dtt)
#dtt <- subset(dtt, !is.na(outdoor_w)); nrow(dtt)
dtt <- subset(dtt, !is.na(TVtime)); nrow(dtt) # time spent watching television
#dtt <- subset(dtt, !is.na(transp)); nrow(dtt)
#dtt <- subset(dtt, !is.na(phys1)); nrow(dtt)
#dtt <- subset(dtt, !is.na(phys2)); nrow(dtt)



# Esleep
dtt <- subset(dtt, !is.na(sleep_d)); nrow(dtt) # sleep duration
#dtt <- subset(dtt, !is.na(chrono)); nrow(dtt)
#dtt <- subset(dtt, !is.na(nap)); nrow(dtt)
#dtt <- subset(dtt, !is.na(insomnia)); nrow(dtt)
#dtt <- subset(dtt, !is.na(snoring)); nrow(dtt)
#dtt <- subset(dtt, !is.na(narcolepsy)); nrow(dtt)


# Esmoke
dtt <- subset(dtt, !is.na(smoking_now)); nrow(dtt) # is current smoker - smoking status
#dtt <- subset(dtt, !is.na(smoke_house)); nrow(dtt)
#dtt <- subset(dtt, !is.na(smoke_home)); nrow(dtt)
#dtt <- subset(dtt, !is.na(smoke_out)); nrow(dtt)
#dtt <- subset(dtt, !is.na(smoke_status)); nrow(dtt)


# Efood
dtt <- subset(dtt, !is.na(veg_cook)); nrow(dtt) # cooked vegetable intake
#dtt <- subset(dtt, !is.na(veg_raw)); nrow(dtt)
#dtt <- subset(dtt, !is.na(fruit_fresh)); nrow(dtt)
#dtt <- subset(dtt, !is.na(fruit_dried)); nrow(dtt)
dtt <- subset(dtt, !is.na(fish_oily)); nrow(dtt) # oily fish intake
dtt <- subset(dtt, !is.na(fish_lean)); nrow(dtt) # non-oily fish intake
dtt <- subset(dtt, !is.na(meat_proc)); nrow(dtt) # processed meat intake
dtt <- subset(dtt, !is.na(poultry)); nrow(dtt) # poultry intake
dtt <- subset(dtt, !is.na(beef)); nrow(dtt) # beef intake
dtt <- subset(dtt, !is.na(lamb)); nrow(dtt) # lamb intake
dtt <- subset(dtt, !is.na(pork)); nrow(dtt) # pork intake
dtt <- subset(dtt, !is.na(cheese)); nrow(dtt) # cheese intake
#dtt <- subset(dtt, !is.na(milk)); nrow(dtt)
#dtt <- subset(dtt, !is.na(spread)); nrow(dtt)
#dtt <- subset(dtt, !is.na(bread)); nrow(dtt)
#dtt <- subset(dtt, !is.na(cereal)); nrow(dtt)
dtt <- subset(dtt, !is.na(salt)); nrow(dtt) # salt added to food
dtt <- subset(dtt, !is.na(tea)); nrow(dtt) # tea intake
#dtt <- subset(dtt, !is.na(drinkhot)); nrow(dtt)
#dtt <- subset(dtt, !is.na(water)); nrow(dtt)
#dtt <- subset(dtt, !is.na(dietCh1)); nrow(dtt)
#dtt <- subset(dtt, !is.na(vit0)); nrow(dtt)
#dtt <- subset(dtt, !is.na(vit1)); nrow(dtt)


# Ealcohol
dtt <- subset(dtt, !is.na(alc1)); nrow(dtt) # alcohol intake frequency
#dtt <- subset(dtt, !is.na(alc2)); nrow(dtt)
#dtt <- subset(dtt, !is.na(alc3)); nrow(dtt)
#dtt <- subset(dtt, !is.na(alc4)); nrow(dtt)
#dtt <- subset(dtt, !is.na(alc5)); nrow(dtt)
#dtt <- subset(dtt, !is.na(alc6)); nrow(dtt)
#dtt <- subset(dtt, !is.na(alc7)); nrow(dtt)
#dtt <- subset(dtt, !is.na(alc8)); nrow(dtt)
#dtt <- subset(dtt, !is.na(alc9)); nrow(dtt)


# Ebody
#dtt <- subset(dtt, !is.na(BMI1)); nrow(dtt)
#dtt <- subset(dtt, !is.na(BMI2)); nrow(dtt)
#dtt <- subset(dtt, !is.na(weight1)); nrow(dtt)
#dtt <- subset(dtt, !is.na(weight2)); nrow(dtt)
dtt <- subset(dtt, !is.na(waist)); nrow(dtt) # waist circumference
#dtt <- subset(dtt, !is.na(hip)); nrow(dtt)
#dtt <- subset(dtt, !is.na(height_stan)); nrow(dtt)
#dtt <- subset(dtt, !is.na(wei_cha)); nrow(dtt)
#dtt <- subset(dtt, !is.na(BFM)); nrow(dtt)
#dtt <- subset(dtt, !is.na(BLM)); nrow(dtt)
#dtt <- subset(dtt, !is.na(BWM)); nrow(dtt)
#dtt <- subset(dtt, !is.na(IWB)); nrow(dtt)
#dtt <- subset(dtt, !is.na(TFP)); nrow(dtt)
#dtt <- subset(dtt, !is.na(TFM)); nrow(dtt)







# Eill
#dtt <- subset(dtt, !is.na(cancer1)); nrow(dtt) # 
#dtt <- subset(dtt, !is.na(cancer2)); nrow(dtt)
#dtt <- subset(dtt, !is.na(disease)); nrow(dtt)
#dtt <- subset(dtt, !is.na(medicat1)); nrow(dtt)
#dtt <- subset(dtt, !is.na(medicat2)); nrow(dtt)
#dtt <- subset(dtt, !is.na(medicat3)); nrow(dtt) ###
#dtt <- subset(dtt, !is.na(falls)); nrow(dtt) ###
#dtt <- subset(dtt, !is.na(diab)); nrow(dtt)
#dtt <- subset(dtt, !is.na(vascul)); nrow(dtt)
#dtt <- subset(dtt, !is.na(doctor)); nrow(dtt)
#dtt <- subset(dtt, !is.na(ageill)); nrow(dtt)
#dtt <- subset(dtt, !is.na(FEV1)); nrow(dtt)
#dtt <- subset(dtt, !is.na(FVC)); nrow(dtt)

#dtt <- subset(dtt, diab==0); nrow(dtt)
#dtt <- subset(dtt, vascul==0); nrow(dtt)




### Adding pressure if medication ###

table(dtt$medicat1)  
table(dtt$medicat2)  
table(dtt$medicat3)  
dtt$add.med10 <- dtt$add.med15 <- 0
dtt$add.med10 <- replace(dtt$add.med10, dtt$medicat1>0 | dtt$medicat2>0 | dtt$medicat3>0, 15)
dtt$add.med15 <- replace(dtt$add.med15, dtt$medicat1>0 | dtt$medicat2>0 | dtt$medicat3>0, 15)
summary(dtt$add.med10)
summary(dtt$add.med15)
dtt$SP0a <- dtt$SP0a+dtt$add.med10
dtt$DP0a <- dtt$DP0a+dtt$add.med15
dtt$SP1a <- dtt$SP1a+dtt$add.med10
dtt$DP1a <- dtt$DP1a+dtt$add.med15
dtt$PP0a <- dtt$SP0a-dtt$DP0a
dtt$PP1a <- dtt$SP1a-dtt$DP1a
dtt$PPam <- (dtt$PP0a+dtt$PP1a)/2





### Tiezzi suggests using additional variables ###

if(1==0){
  dtt <- subset(dtt, !is.na(PCtime)); nrow(dtt) ## PCtime - time spent using PC ##
  dtt <- subset(dtt, !is.na(DRtime)); nrow(dtt) ## DRtime - time spent driving ##
  dtt <- subset(dtt, !is.na(getup)); nrow(dtt) ## getup - ease of getting up in the morning ##
  dtt <- subset(dtt, !is.na(coffee)); nrow(dtt) ## coffee - coffee consumption ##
  dtt <- subset(dtt, !is.na(smoked_past)); nrow(dtt) ## smoked_past - smoked in the past ##
  dtt <- subset(dtt, !is.na(BFP)); nrow(dtt) ## BFP - body fat percentage ##
  dtt <- subset(dtt, !is.na(BMR)); nrow(dtt) ## BMR - body mass ratio ##
}

## only records with complete information will have Tiezzi==1 ##
dtt$tiezzi <- 0
dtt$tiezzi[which(!is.na(dtt$PCtime) & !is.na(dtt$DRtime) & !is.na(dtt$getup) & !is.na(dtt$coffee) & !is.na(dtt$smoked_past) & !is.na(dtt$BFP) & !is.na(dtt$BMR))] <- 1
nrow(dtt)
table(dtt$tiezzi)

dtt <- subset(dtt, tiezzi==1); nrow(dtt)





# Epoll
#dtt <- subset(dtt, !is.na(HAPD)); nrow(dtt)
#dtt <- subset(dtt, !is.na(NO2_2010)); nrow(dtt)
#dtt <- subset(dtt, !is.na(NO2_2005)); nrow(dtt)
#dtt <- subset(dtt, !is.na(NO2_2006)); nrow(dtt)
#dtt <- subset(dtt, !is.na(NO2_2007)); nrow(dtt)
#dtt <- subset(dtt, !is.na(NO1_2010)); nrow(dtt)
#dtt <- subset(dtt, !is.na(PM10_2010)); nrow(dtt) ## 
#dtt <- subset(dtt, !is.na(PM10_2007)); nrow(dtt) ##
#dtt <- subset(dtt, !is.na(PM25_2010)); nrow(dtt)
#dtt <- subset(dtt, !is.na(PM25A_2010)); nrow(dtt)
#dtt <- subset(dtt, !is.na(PM2510_2010)); nrow(dtt)
#dtt <- subset(dtt, !is.na(TINR)); nrow(dtt)
#dtt <- subset(dtt, !is.na(IDNR)); nrow(dtt)
#dtt <- subset(dtt, !is.na(TINmR)); nrow(dtt)
#dtt <- subset(dtt, !is.na(IDNmR)); nrow(dtt)
#dtt <- subset(dtt, !is.na(TTLmR)); nrow(dtt)
#dtt <- subset(dtt, !is.na(CmR)); nrow(dtt)
#dtt <- subset(dtt, !is.na(SRL100m)); nrow(dtt)
#dtt <- subset(dtt, !is.na(noise1)); nrow(dtt)
#dtt <- subset(dtt, !is.na(noise2)); nrow(dtt)
#dtt <- subset(dtt, !is.na(noise3)); nrow(dtt)
#dtt <- subset(dtt, !is.na(noise4)); nrow(dtt)
#dtt <- subset(dtt, !is.na(noise5)); nrow(dtt)





###################################
###  Creating design indicators ###
###################################

### Age at phenotyping classes: AOPc2 ###

## Boundary values from Kerin and Marchini, 2020 ##
summary(dtt$AOP)
x1 <- 40; x1
x2 <- 51; x2
x3 <- 58; x3
x4 <- 63; x4
x5 <- 70; x5
dtt$AOPc2 <- 0
dtt$AOPc2[which(dtt$AOP>=x1)] <- 1
dtt$AOPc2[which(dtt$AOP>=x2)] <- 2
dtt$AOPc2[which(dtt$AOP>=x3)] <- 3
dtt$AOPc2[which(dtt$AOP>=x4)] <- 4
dtt$AOPc2[which(dtt$AOP>x5)] <- 5
table(dtt$AOPc2)

## Keeping only between 40 and 70 years old at phenotyping ##
dtt <- subset(dtt, AOPc2!=0); nrow(dtt)
dtt <- subset(dtt, AOPc2!=5); nrow(dtt)

## Creating a cohort indicator, pasting Sex_SI and AOPc2 ##
dtt$coh <- paste(dtt$Sex_SI, dtt$AOPc2, sep='_')
table(dtt$coh)



table(dtt$ethn1_white)
table(dtt$ethn1_whbri)
table(dtt$ethn1_black)
table(dtt$ethn1_asian)
table(dtt$ethn1_mixed)


## Creating 8 random subsets, same number as the cohorts ##
dtt$sub <- sample(11:18, nrow(dtt), replace=T)
table(dtt$sub)

## Checking the distribution of the random samples ##
table(dtt$sub, dtt$Sex_SI)
table(dtt$sub, dtt$AOPc2)
table(dtt$sub, dtt$coh)

## Checking the distribution of the Tiezzi-approved samples ##
table(dtt$tiezzi, dtt$Sex_SI)
table(dtt$tiezzi, dtt$AOPc2)
table(dtt$tiezzi, dtt$coh)





save(dtt, file="datasets/data2_20230413.RData")












rm(list=ls())



