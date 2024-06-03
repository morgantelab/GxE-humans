




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

load("datasets/data1_20240209.RData")
dtt <- dt; nrow(dtt)




### Subset by ID ###
dtt <- subset(dtt, !(ID%in%inds_to_remove)); nrow(dtt)
dtt <- subset(dtt, ID%in%inds_to_keep); nrow(dtt)






### Subset for phenotypes ###
dtt <- subset(dtt, !is.na(PR0)); nrow(dtt) # pulse rate, automatic

dtt <- subset(dtt, !is.na(DP0)); nrow(dtt) # diastolic blood pressure, first reading
dtt <- subset(dtt, !is.na(SP0)); nrow(dtt) # sistolic blood pressure, first reading
dtt <- subset(dtt, !is.na(PP0)); nrow(dtt) # pulse pressure, first reading

### Subsetting for I vars ###
dtt <- subset(dtt, !is.na(Sex_SI)); nrow(dtt) # sex, self identified
dtt <- subset(dtt, !is.na(AOP)); nrow(dtt) # age at phenotyping
dtt <- subset(dtt, !is.na(AOR)); nrow(dtt) # age at recruiment
dtt <- subset(dtt, ethn1_whbri==1); nrow(dtt) # ethnicity white british (self-reported)
dtt <- subset(dtt, ethn2==1); nrow(dtt) # Caucasian cluster by genetic grouping




# Elife
dtt <- subset(dtt, !is.na(Townsend)); nrow(dtt) # Townsend deprivation index at recruitment


# Ephys
dtt <- subset(dtt, !is.na(walk_d)); nrow(dtt) # number of days/ week walked 10þ minutes
dtt <- subset(dtt, !is.na(act0_d)); nrow(dtt) # number of days/week of moderate physical activity 10þ minutes
dtt <- subset(dtt, !is.na(act1_d)); nrow(dtt) # number of days/week of vigorous physical activity 10þ minutes
dtt <- subset(dtt, !is.na(TVtime)); nrow(dtt) # time spent watching television



# Esleep
dtt <- subset(dtt, !is.na(sleep_d)); nrow(dtt) # sleep duration


# Esmoke
dtt <- subset(dtt, !is.na(smoking_now)); nrow(dtt) # is current smoker - smoking status


# Efood
dtt <- subset(dtt, !is.na(veg_cook)); nrow(dtt) # cooked vegetable intake
dtt <- subset(dtt, !is.na(fish_oily)); nrow(dtt) # oily fish intake
dtt <- subset(dtt, !is.na(fish_lean)); nrow(dtt) # non-oily fish intake
dtt <- subset(dtt, !is.na(meat_proc)); nrow(dtt) # processed meat intake
dtt <- subset(dtt, !is.na(poultry)); nrow(dtt) # poultry intake
dtt <- subset(dtt, !is.na(beef)); nrow(dtt) # beef intake
dtt <- subset(dtt, !is.na(lamb)); nrow(dtt) # lamb intake
dtt <- subset(dtt, !is.na(pork)); nrow(dtt) # pork intake
dtt <- subset(dtt, !is.na(cheese)); nrow(dtt) # cheese intake
dtt <- subset(dtt, !is.na(salt)); nrow(dtt) # salt added to food
dtt <- subset(dtt, !is.na(tea)); nrow(dtt) # tea intake


# Ealcohol
dtt <- subset(dtt, !is.na(alc1)); nrow(dtt) # alcohol intake frequency


# Ebody
dtt <- subset(dtt, !is.na(waist)); nrow(dtt) # waist circumference






### Adding pressure if medication ###

table(dtt$medicat1)  
table(dtt$medicat2)  
table(dtt$medicat3) 
table(dtt$mediblood1)
table(dtt$mediblood2) 
table(dtt$mediblood1,dtt$mediblood2) 
summary(dtt$mediblood1)
summary(dtt$mediblood2) 
length(which(dtt$mediblood1==1 | dtt$mediblood2==1))/nrow(dtt)
dtt$add.med10 <- dtt$add.med15 <- 0
dtt$add.med10 <- replace(dtt$add.med10, dtt$mediblood1==1 | dtt$mediblood2==1, 10)
dtt$add.med15 <- replace(dtt$add.med15, dtt$mediblood1==1 | dtt$mediblood2==1, 15)
summary(dtt$add.med10)
summary(dtt$add.med15)
dtt$SP0a <- dtt$SP0a+dtt$add.med10
dtt$DP0a <- dtt$DP0a+dtt$add.med15
dtt$PP0a <- dtt$SP0a-dtt$DP0a





### Additional variables ###

dtt <- subset(dtt, !is.na(PCtime)); nrow(dtt) ## PCtime - time spent using PC ##
dtt <- subset(dtt, !is.na(DRtime)); nrow(dtt) ## DRtime - time spent driving ##
dtt <- subset(dtt, !is.na(getup)); nrow(dtt) ## getup - ease of getting up in the morning ##
dtt <- subset(dtt, !is.na(coffee)); nrow(dtt) ## coffee - coffee consumption ##
dtt <- subset(dtt, !is.na(smoked_past)); nrow(dtt) ## smoked_past - smoked in the past ##
dtt <- subset(dtt, !is.na(BFP)); nrow(dtt) ## BFP - body fat percentage ##
dtt <- subset(dtt, !is.na(BMR)); nrow(dtt) ## BMR - body mass ratio ##







###################################
###  Creating design indicators ###
###################################

### Age at phenotyping classes: AOPc2 ###

## Boundary values taken and adjusted from Kerin and Marchini, 2020 ##
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





save(dtt, file="datasets/data2_20240209.RData")












rm(list=ls()); gc()


