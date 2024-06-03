


rm(list=ls()); gc()

#library(gtools)
#library(gdata)
#library(reshape)
library(BGLR)
#library(matrixcalc)

NAcol <- function(X){for(i in 1:ncol(X)){print(length(which(is.na(X[i]))))}}
'%!in%' <- function(x,y)!('%in%'(x,y))
censcale <- function(x,A,B){(A+(((x-mean(x)))/sd(x))*B)}



args <- as.numeric(commandArgs(TRUE))
#args <- c(14, 30, 1,  110, 10, 1,   99, 9)
args


# nohup R CMD BATCH "--args  10   30    1  60000 10000 50   99   9" 6_run_20240409.R  logs/log6_10_30_1.txt




iter <- args[4]
burnin <- args[5]
thin <- args[6]
gvar <- args[7]
verb <- T
tol <- 0
where <- paste("run",args[8],sep=''); where
home <- "/data2/morgante_lab/ukbiobank_projects/GxE/run"
scratch <- "/scratch3/fabiom/tmp/BGLR"

##################################
############ Load data ###########
##################################

setwd(home)

confirm <- read.table("../read.txt", header=F)



load("../datasets/EigVal_20230628.RData")
head(Val)
tail(Val)

nrow(Val)
Val <- subset(Val, val>0)
Val <- subset(Val, sumval<=99.9)
nrow(Val)

#load("../datasets/data4short_20240209.RData"); dt <- dts
load("../datasets/data4_20240209.RData")
nrow(dt)


dt$BMI1 <- censcale(dt$BMI1, 100, 10)





##################################
########### Variables ############
##################################


Ivars <- c("ID", "Sex_SI", "Sex_gen", "YOB", "MOB", "DOF", "COF", "ethn1", "ethn1_white", "ethn1_mixed", "ethn1_asian", "ethn1_black", "ethn2", "AOP", "AOR")
Gvars <- Val$pc; length(Gvars)
Emarc <-	c("Townsend", "walk_d", "act0_d", "act1_d", "TVtime", "sleep_d", "smoking_now", "veg_cook", "fish_oily", "fish_lean", "meat_proc", "poultry", "beef", "lamb", "pork", "cheese", "salt", "tea", "alc1", "waist")
Etiez <- c("PCtime", "DRtime", "getup", "coffee", "smoked_past", "BFP", "BMR")
Evars <- c(Emarc, Etiez); length(Evars)
Yvars <- c("PR0", "PR1",   "DP0", "DP1",   "SP0", "SP1",   "PP0", "PP1",   "PPm",    "DP0a", "DP1a",   "SP0a", "SP1a",  "PP0a", "PP1a",   "PPam",    "BMI1", "BMI2") 
cohorts <- c("coh01", "coh02", "coh03", "coh04",  "coh11", "coh12", "coh13", "coh14")





##################################
############ Matrices ############
##################################


Ymat <- as.matrix(dt[Yvars]); dim(Ymat); NAcol(Ymat)

Xmat <- as.matrix(dt[cohorts]); dim(Xmat); NAcol(Xmat)

Gmat <- as.matrix(dt[Gvars]); dim(Gmat); # NAcol(Gmat)




if(args[2]==10){}




if(args[2]==1 | args[2]==11 | args[2]==21){
	load("../datasets/e99EE_20231010trim.RData")
	EEmat <- eEE$vectors
	for(i in 1:ncol(EEmat)){EEmat[,i] <- EEmat[,i]*eEE$values[i]}
	colnames(EEmat) <- paste("EE", 1:ncol(EEmat), sep='.')
	print(length(which(dt$ID!=eEE$ids)))
	rm(eEE); gc()
}

if(args[2]==11 | args[2]==14){
	load("../datasets/eG99EE_20231002.RData")
	keep <- which(eGEE$values>0)
	GEEmat <- eGEE$vectors[,keep]
	for(i in 1:ncol(GEEmat)){GEEmat[,i] <- GEEmat[,i]*eGEE$values[i]}
	colnames(GEEmat) <- paste("GEE", 1:ncol(GEEmat), sep='.')
	print(length(which(dt$ID!=eGEE$ids)))
	rm(eGEE, keep); gc()
}





if(args[2]==2 | args[2]==12 | args[2]==22){
	load("../datasets/e99EL2EL2_20231010trim.RData")
	EEmat <- eEL2EL2$vectors
	for(i in 1:ncol(EEmat)){EEmat[,i] <- EEmat[,i]*eEL2EL2$values[i]}
	colnames(EEmat) <- paste("EL2EL2", 1:ncol(EEmat), sep='.')
	print(length(which(dt$ID!=eEL2EL2$ids)))
	rm(eEL2EL2); gc()
}

if(args[2]==12 | args[2]==15){
	load("../datasets/eG99EL2EL2_20231010.RData")
	keep <- which(eGEL2EL2$values>0)
	GEEmat <- eGEL2EL2$vectors[,keep]
	for(i in 1:ncol(GEEmat)){GEEmat[,i] <- GEEmat[,i]*eGEL2EL2$values[i]}
	colnames(GEEmat) <- paste("GEL2EL2", 1:ncol(GEEmat), sep='.')
	print(length(which(dt$ID!=eGEL2EL2$ids)))
	rm(eGEL2EL2, keep); gc()
}





if(args[2]==3 | args[2]==13 | args[2]==23){
	load("../datasets/e99Ee2Ee2_20231010trim.RData")
	EEmat <- eEe2Ee2$vectors
	for(i in 1:ncol(EEmat)){EEmat[,i] <- EEmat[,i]*eEe2Ee2$values[i]}
	colnames(EEmat) <- paste("Ee2Ee2", 1:ncol(EEmat), sep='.')
	print(length(which(dt$ID!=eEe2Ee2$ids)))
	rm(eEe2Ee2); gc()
}

if(args[2]==13 | args[2]==16){
	load("../datasets/eG99Ee2Ee2_20231010.RData")
	keep <- which(eGEe2Ee2$values>0)
	GEEmat <- eGEe2Ee2$vectors[,keep]
	for(i in 1:ncol(GEEmat)){GEEmat[,i] <- GEEmat[,i]*eGEe2Ee2$values[i]}
	colnames(GEEmat) <- paste("GEe2Ee2", 1:ncol(GEEmat), sep='.')
	print(length(which(dt$ID!=eGEe2Ee2$ids)))
	rm(eGEe2Ee2, keep); gc()
}






if(args[2]==4 | args[2]==14 | args[2]==24){
	EEmat <- as.matrix(dt[Evars]); dim(EEmat); NAcol(EEmat)
	colnames(EEmat) <- paste("EE", 1:ncol(EEmat), sep='.')
	for(i in 1:ncol(EEmat)){EEmat[,i] <- censcale(EEmat[,i], 0, 1)}
}


if(args[2]==5 | args[2]==15 | args[2]==25){
	EEmat <- as.matrix(dt[paste("L2", Evars, sep='.')]); dim(EEmat); NAcol(EEmat)
	colnames(EEmat) <- paste("EE", 1:ncol(EEmat), sep='.')
	for(i in 1:ncol(EEmat)){EEmat[,i] <- censcale(EEmat[,i], 0, 1)}
}


if(args[2]==6 | args[2]==16 | args[2]==26){
	EEmat <- as.matrix(dt[paste("e2", Evars, sep='.')]); dim(EEmat); NAcol(EEmat)
	colnames(EEmat) <- paste("EE", 1:ncol(EEmat), sep='.')
	for(i in 1:ncol(EEmat)){EEmat[,i] <- censcale(EEmat[,i], 0, 1)}
}





if(args[2]==30 | args[2]==31){
	load("../datasets/e99CC_20240409.RData")
	CCmat <- eCC$vectors
	for(i in 1:ncol(CCmat)){CCmat[,i] <- CCmat[,i]*eCC$values[i]}
	colnames(CCmat) <- paste("CC", 1:ncol(CCmat), sep='.')
	print(length(which(dt$ID!=eCC$ids)))
	rm(eCC); gc()
}


if(args[2]==31){
	load("../datasets/eG99CC_20240409.RData")
	keep <- which(eGCC$values>0)
	GCCmat <- eGCC$vectors[,keep]
	for(i in 1:ncol(GCCmat)){GCCmat[,i] <- GCCmat[,i]*eGCC$values[i]}
	colnames(GCCmat) <- paste("GCC", 1:ncol(GCCmat), sep='.')
	print(length(which(dt$ID!=eGCC$ids)))
	rm(eGCC, keep); gc()
}



#dt <- dt[1:2000,]
#Ymat <- Ymat[1:2000,]
#Xmat <- Xmat[1:2000,]
#Gmat <- Gmat[1:2000,]
#EEmat <- EEmat[1:2000,]
#CCmat <- CCmat[1:2000,]
#if(args[2]%in%c(11,12,13)){GEEmat <- GEEmat[1:2000,]}













##################################
########### Cross-val ############
##################################


dt$trn <- NA

if(args[3]==1){dt$trn <- 1}

if(args[3]==11){dt$trn <- replace(dt$trn, dt$sub%in%c(11,12,13,14), 1)} # primi 4
if(args[3]==12){dt$trn <- replace(dt$trn, dt$sub%in%c(15,16,17,18), 1)} # secondi 4
if(args[3]==13){dt$trn <- replace(dt$trn, dt$sub%in%c(11,13,15,17), 1)} # dispari
if(args[3]==14){dt$trn <- replace(dt$trn, dt$sub%in%c(12,14,16,18), 1)} # pari
if(args[3]==15){dt$trn <- replace(dt$trn, dt$sub%in%c(11,12,15,16), 1)} # primi 2 di ogni gruppo
if(args[3]==16){dt$trn <- replace(dt$trn, dt$sub%in%c(13,14,17,18), 1)} # secondi 2 di ogni gruppo
if(args[3]==17){dt$trn <- replace(dt$trn, dt$sub%in%c(11,12,17,18), 1)} # ultimi 2
if(args[3]==18){dt$trn <- replace(dt$trn, dt$sub%in%c(13,14,15,16), 1)} # 4 nel mezzo
if(args[3]==19){dt$trn <- replace(dt$trn, dt$sub%in%c(11,16,13,18), 1)} # alternati 
if(args[3]==20){dt$trn <- replace(dt$trn, dt$sub%in%c(15,12,17,14), 1)} # alternati

if(args[3]==101){dt$trn <- replace(dt$trn, dt$coh%in%c("0_1", "0_2", "0_3", "0_4"), 1)} # A
if(args[3]==102){dt$trn <- replace(dt$trn, dt$coh%in%c("1_1", "1_2", "1_3", "1_4"), 1)} # B
if(args[3]==103){dt$trn <- replace(dt$trn, dt$coh%in%c("0_1", "0_2", "1_1", "1_2"), 1)} # C
if(args[3]==104){dt$trn <- replace(dt$trn, dt$coh%in%c("0_3", "0_4", "1_3", "1_4"), 1)} # D

if(args[3]==105){dt$trn <- replace(dt$trn, dt$coh%in%c("0_1", "0_2", "1_3", "1_4"), 1)} # E
if(args[3]==106){dt$trn <- replace(dt$trn, dt$coh%in%c("0_1", "1_2", "1_3", "0_4"), 1)} # F
if(args[3]==107){dt$trn <- replace(dt$trn, dt$coh%in%c("1_1", "0_2", "0_3", "0_4"), 1)} # G
if(args[3]==108){dt$trn <- replace(dt$trn, dt$coh%in%c("1_1", "0_3", "0_4", "1_4"), 1)} # H
if(args[3]==109){dt$trn <- replace(dt$trn, dt$coh%in%c("0_1", "0_3", "1_1", "1_3"), 1)} # I
if(args[3]==110){dt$trn <- replace(dt$trn, dt$coh%in%c("0_2", "0_4", "1_2", "1_4"), 1)} # L
if(args[3]==111){dt$trn <- replace(dt$trn, dt$coh%in%c("0_1", "0_4", "1_1", "1_4"), 1)} # M
if(args[3]==112){dt$trn <- replace(dt$trn, dt$coh%in%c("0_2", "0_3", "1_2", "1_3"), 1)} # N
if(args[3]==113){dt$trn <- replace(dt$trn, dt$coh%in%c("0_1", "0_2", "1_1", "1_2"), 1)} # O
if(args[3]==114){dt$trn <- replace(dt$trn, dt$coh%in%c("0_3", "0_4", "1_3", "1_4"), 1)} # P

summary(dt$trn)
nrow(dt)

if(args[3]==1){Xmat <- Xmat[,-1]}
if(args[3]==101){Xmat <- Xmat[,which(colnames(Xmat)%in%c("coh01", "coh02", "coh03", "coh04"))]; Xmat <- Xmat[,-1]} 
if(args[3]==102){Xmat <- Xmat[,which(colnames(Xmat)%in%c("coh11", "coh12", "coh13", "coh14"))]; Xmat <- Xmat[,-1]} 
if(args[3]==103){Xmat <- Xmat[,which(colnames(Xmat)%in%c("coh01", "coh02", "coh11", "coh12"))]; Xmat <- Xmat[,-1]} 
if(args[3]==104){Xmat <- Xmat[,which(colnames(Xmat)%in%c("coh03", "coh04", "coh13", "coh14"))]; Xmat <- Xmat[,-1]} 

summary(Xmat)
gc()









##################################
############# Models #############
##################################


Phen <- phen <- Ymat[,args[1]]
print(mean(phen))
rm(Ymat)


if(args[3]>1){
	phen <- phen*dt$trn
	print(summary(phen))
}


preds <- data.frame(ID=dt$ID, sub=dt$sub, coh=dt$coh, phen=Phen)

rm(dt); gc()






# Define model #

if(args[2]%in%c(10)){
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE) 
  )
   rm(Xmat, Gmat); gc()

}


if(args[2]%in%c(1,2,3,4,5,6)){
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE), 
              E=list(X=EEmat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, EEmat, Gmat); gc()

}


if(args[2]%in%c(11,12,13,14,15,16)){

  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE), 
              E=list(X=EEmat, model="BRR", saveEffects=TRUE),
              GE=list(X=GEEmat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, EEmat, Gmat, GEEmat); gc()

}


if(args[2]%in%c(21,22,23,24,25,26)){
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              E=list(X=EEmat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, EEmat, Gmat); gc()

}


if(args[2]%in%c(30)){

  ETA <- list(X=list(X=CCmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE) 
  )
   rm(Xmat, CCmat, Gmat); gc()

}


if(args[2]%in%c(31)){

  ETA <- list(X=list(X=CCmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE), 
              GE=list(X=GCCmat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, CCmat, Gmat, GCCmat); gc()

}




# Run model #


model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))







# Collect results #

if(args[2]%in%c(10, 30)){
  
  zz0 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_varB.dat', sep=''), header=F); colnames(zz1) <- "G"
  zz2 <- data.frame(matrix(0, nrow(zz0), 1)); colnames(zz2) <- "E"
  zz9 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz9)
  
  B1 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_X_b.dat', sep=''), header=T)
  B2 <- readBinMat(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_b.bin', sep=''))
  B3 <- data.frame(matrix(0, nrow(zz0), length(Evars)))
  
  varabs <- matrix(NA, nrow(VCEm), 3); colnames(varabs) <- c("Vdes", "Vgen", "Venv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[-c(1:(burnin/thin)), 2] <- apply(ETA$G$X%*%t(B2), 2, var)
  varabs[-c(1:(burnin/thin)), 3] <- 0
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs, B1)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}


if(args[2]%in%c(1,2,3,4,5,6)){
  
  zz0 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_varB.dat', sep=''), header=F); colnames(zz1) <- "G"
  zz2 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_E_varB.dat', sep=''), header=F); colnames(zz2) <- "E"
  zz9 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz9)
  
  B1 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_X_b.dat', sep=''), header=T)
  B2 <- readBinMat(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_b.bin', sep=''))
  B3 <- readBinMat(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_E_b.bin', sep=''))
  
  varabs <- matrix(NA, nrow(VCEm), 3); colnames(varabs) <- c("Vdes", "Vgen", "Venv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[-c(1:(burnin/thin)), 2] <- apply(ETA$G$X%*%t(B2), 2, var)
  varabs[-c(1:(burnin/thin)), 3] <- apply(ETA$E$X%*%t(B3), 2, var)
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs, B1)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}


if(args[2]%in%c(11,12,13,14,15,16)){
  
  zz0 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_varB.dat', sep=''), header=F); colnames(zz1) <- "G"
  zz2 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_E_varB.dat', sep=''), header=F); colnames(zz2) <- "E"
  zz3 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_GE_varB.dat', sep=''), header=F); colnames(zz3) <- "GE"
  zz9 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz3, zz9)
  
  B1 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_X_b.dat', sep=''), header=T)
  B2 <- readBinMat(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_b.bin', sep=''))
  B3 <- readBinMat(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_E_b.bin', sep=''))
  B4 <- readBinMat(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_GE_b.bin', sep=''))
  
  varabs <- matrix(NA, nrow(VCEm), 4); colnames(varabs) <- c("Vdes", "Vgen", "Venv", "Vgenenv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[-c(1:(burnin/thin)), 2] <- apply(ETA$G$X%*%t(B2), 2, var)
  varabs[-c(1:(burnin/thin)), 3] <- apply(ETA$E$X%*%t(B3), 2, var)
  varabs[-c(1:(burnin/thin)), 4] <- apply(ETA$GE$X%*%t(B4), 2, var)
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs, B1)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz3, zz9, B1, B2, B3, B4, varabs, VCE, VCEm)
  
}


if(args[2]%in%c(21,22,23,24,25,26)){
  
  zz0 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- data.frame(matrix(0, nrow(zz0), 1)); colnames(zz1) <- "G"
  zz2 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_E_varB.dat', sep=''), header=F); colnames(zz2) <- "E"
  zz9 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz9)
  
  B1 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_X_b.dat', sep=''), header=T)
  B2 <- data.frame(matrix(0, nrow(zz0), length(Gvars)))
  B3 <- readBinMat(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_E_b.bin', sep=''))
  
  varabs <- matrix(NA, nrow(VCEm), 3); colnames(varabs) <- c("Vdes", "Vgen", "Venv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[-c(1:(burnin/thin)), 2] <- 0
  varabs[-c(1:(burnin/thin)), 3] <- apply(ETA$E$X%*%t(B3), 2, var)
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs, B1)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}


if(args[2]%in%c(31)){
  
  zz0 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_varB.dat', sep=''), header=F); colnames(zz1) <- "G"
  zz2 <- data.frame(matrix(0, nrow(zz0), 1)); colnames(zz2) <- "E"
  zz3 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_GE_varB.dat', sep=''), header=F); colnames(zz3) <- "GE"
  zz9 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz3, zz9)
  
  B1 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_X_b.dat', sep=''), header=T)
  B2 <- readBinMat(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_b.bin', sep=''))
  B3 <- data.frame(matrix(0, nrow(zz0), length(Evars)))
  B4 <- readBinMat(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_GE_b.bin', sep=''))
  
  varabs <- matrix(NA, nrow(VCEm), 4); colnames(varabs) <- c("Vdes", "Vgen", "Venv", "Vgenenv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[-c(1:(burnin/thin)), 2] <- apply(ETA$G$X%*%t(B2), 2, var)
  varabs[-c(1:(burnin/thin)), 3] <- apply(ETA$E$X%*%t(B3), 2, var)
  varabs[-c(1:(burnin/thin)), 4] <- apply(ETA$GE$X%*%t(B4), 2, var)
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs, B1)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz3, zz9, B1, B2, B3, B4, varabs, VCE, VCEm)
  
}













rm(list=ls()); gc()


































