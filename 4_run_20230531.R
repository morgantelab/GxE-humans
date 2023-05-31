


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
#args <- c(14, 3, 1,  30000, 0, 5,   100)
args




iter <- args[4]
burnin <- args[5]
thin <- args[6]
gvar <- args[7]
verb <- F
tol <- 0
where <- "run3"


##################################
############ Load data ###########
##################################

setwd("/data2/morgante_lab/ukbiobank_projects/GxE/run")

confirm <- read.table("../read.txt", header=F)




load("../datasets/data3_20230511.RData")
nrow(dttt)

dttt$coh01 <- ifelse(dttt$coh=="0_1", 1, 0)
dttt$coh02 <- ifelse(dttt$coh=="0_2", 1, 0)
dttt$coh03 <- ifelse(dttt$coh=="0_3", 1, 0)
dttt$coh04 <- ifelse(dttt$coh=="0_4", 1, 0)
dttt$coh11 <- ifelse(dttt$coh=="1_1", 1, 0)
dttt$coh12 <- ifelse(dttt$coh=="1_2", 1, 0)
dttt$coh13 <- ifelse(dttt$coh=="1_3", 1, 0)
dttt$coh14 <- ifelse(dttt$coh=="1_4", 1, 0)



load("../G/eigenG_20230413.RData")
val <- eigenG[[1]][,1]
pc <- eigenG[[2]]
length(val); dim(pc)
for(i in 2:ncol(pc)){pc[,i] <- pc[,i]*val[i-1]}
dt <- merge(dttt, pc, by="ID", all.x=F, all.y=F); dim(dt)


sumval <- val
for(i in 2:length(sumval)){sumval[i] <- sumval[i]+sumval[i-1]}
sumval <- 100*(sumval/sum(val))

Val <- data.frame(pc=paste("PC",1:nrow(eigenG[[1]]),sep=''), val=val, sumval=sumval); dim(Val)

if(gvar<100){
	Val$diff <- abs(Val$sumval-gvar)
	this <- which.min(Val$diff)
	pc <- pc[,c(1:this)]
	dim(pc)     
}



if(args[3]==1){}
if(args[3]==11){dt <- subset(dt, sub%in%c(11,12,13,14))} # primi 4
if(args[3]==12){dt <- subset(dt, sub%in%c(15,16,17,18))} # secondi 4
if(args[3]==13){dt <- subset(dt, sub%in%c(11,13,15,17))} # dispari
if(args[3]==14){dt <- subset(dt, sub%in%c(12,14,16,18))} # pari
if(args[3]==15){dt <- subset(dt, sub%in%c(11,12,15,16))} # primi 2 di ogni gruppo
if(args[3]==16){dt <- subset(dt, sub%in%c(13,14,17,18))} # secondi 2 di ogni gruppo
if(args[3]==17){dt <- subset(dt, sub%in%c(11,12,17,18))} # ultimi 2
if(args[3]==18){dt <- subset(dt, sub%in%c(13,14,15,16))} # 4 nel mezzo
if(args[3]==19){dt <- subset(dt, sub%in%c(11,16,13,18))} # alternati 
if(args[3]==20){dt <- subset(dt, sub%in%c(15,12,17,14))} # alternati
if(args[3]==101){dt <- subset(dt, coh%in%c("0_1", "0_2", "0_3", "0_4"))} # A
if(args[3]==102){dt <- subset(dt, coh%in%c("1_1", "1_2", "1_3", "1_4"))} # B
if(args[3]==103){dt <- subset(dt, coh%in%c("0_1", "0_2", "1_1", "1_2"))} # C
if(args[3]==104){dt <- subset(dt, coh%in%c("0_3", "0_4", "1_3", "1_4"))} # D
if(args[3]==105){dt <- subset(dt, coh%in%c("0_1", "0_2", "1_3", "1_4"))} # E
if(args[3]==106){dt <- subset(dt, coh%in%c("0_1", "1_2", "1_3", "0_4"))} # F
if(args[3]==107){dt <- subset(dt, coh%in%c("1_1", "0_2", "0_3", "0_4"))} # G
if(args[3]==108){dt <- subset(dt, coh%in%c("1_1", "0_3", "0_4", "1_4"))} # H

nrow(dt)

rm(pc, dttt); gc()



##################################
########### Variables ############
##################################


Ivars <- c("ID", "Sex_SI", "Sex_gen", "YOB", "MOB", "DOF", "COF", "ethn1", "ethn1_white", "ethn1_mixed", "ethn1_asian", "ethn1_black", "ethn2", "AOP", "AOR")
Gvars <- Val$pc; length(Gvars)
Emarc <-	c("Townsend", "walk_d", "act0_d", "act1_d", "TVtime", "sleep_d", "smoking_now", "veg_cook", "fish_oily", "fish_lean", "meat_proc", "poultry", "beef", "lamb", "pork", "cheese", "salt", "tea", "alc1", "waist")
Etiez <- c("PCtime", "DRtime", "getup", "coffee", "smoked_past", "BFP", "BMR")
Evars <- c(Emarc, Etiez); length(Evars)
Yvars <- c("PR0", "PR1",   "DP0", "DP1",   "SP0", "SP1",   "PP0", "PP1",   "PPm",    "DP0a", "DP1a",   "SP0a", "SP1a",  "PP0a", "PP1a",   "PPam") 
cohorts <- c("coh01", "coh02", "coh03", "coh04",  "coh11", "coh12", "coh13", "coh14")






##################################
############# Models #############
##################################


if(args[1]<100){
  Ymat <- as.matrix(dt[Yvars]); dim(Ymat); NAcol(Ymat)
  phen <- Ymat[,args[1]]
  print(mean(phen))
  rm(Ymat)
}

if(args[1]>100){
  Cmat <- as.matrix(dt[Evars]); dim(Cmat); NAcol(Cmat)
  phen <- Cmat[,args[1]-100]
  print(mean(phen))
  rm(Cmat)
}


Gmat <- as.matrix(dt[Gvars]); dim(Gmat); # NAcol(Gmat)
Emat <- as.matrix(dt[Evars]); dim(Emat); NAcol(Emat)
Xmat <- as.matrix(dt[cohorts]); dim(Xmat); NAcol(Xmat)
Xmat <- Xmat[,-1]

if(args[1]>100){
  Emat[,args[1]-100] <- dt$randomvector
}



#phen <- censcale(phen, 100, 10)  
rm(eigenG, val, dt); gc()

if(args[2]==1){
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE)
  )
  rm(Xmat, Emat, Gmat); gc()
  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  
  zz0 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_varB.dat', sep=''), header=F); colnames(zz1) <- "G"
  zz2 <- data.frame(matrix(0, nrow(zz0), 1)); colnames(zz2) <- "E"
  zz9 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz9)
  
  B1 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_X_b.dat', sep=''), header=T)
  B2 <- readBinMat(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_b.bin', sep=''))
  B3 <-  data.frame(matrix(0, nrow(zz0), length(Evars)))
  
  varabs <- matrix(NA, nrow(VCEm), 3); colnames(varabs) <- c("Vdes", "Vgen", "Venv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[, 2] <- apply(ETA$G$X%*%t(B2), 2, var)
  varabs[, 3] <- 0
  VCE <- data.frame(VCEm, varabs)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  BETA <- list()
  BETA[[1]] <- B1
  BETA[[2]] <- B2
  BETA[[3]] <- B3
  save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE)
  
}







if(args[2]==2){
  
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE), 
              E=list(X=Emat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, Emat, Gmat); gc()

  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  
  zz0 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_varB.dat', sep=''), header=F); colnames(zz1) <- "G"
  zz2 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_E_varB.dat', sep=''), header=F); colnames(zz2) <- "E"
  zz9 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz9)
  
  B1 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_X_b.dat', sep=''), header=T)
  B2 <- readBinMat(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_b.bin', sep=''))
  B3 <- readBinMat(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_E_b.bin', sep=''))
  
  varabs <- matrix(NA, nrow(VCEm), 3); colnames(varabs) <- c("Vdes", "Vgen", "Venv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[, 2] <- apply(ETA$G$X%*%t(B2), 2, var)
  varabs[, 3] <- apply(ETA$E$X%*%t(B3), 2, var)
  
  VCE <- data.frame(VCEm, varabs)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  BETA <- list()
  BETA[[1]] <- B1
  BETA[[2]] <- B2
  BETA[[3]] <- B3

	  varabsmar <- list()
	  for(e in c(3)){varabsmar[[e]] <- matrix(NA, nrow(VCE), ncol(BETA[[e]]))}
#	  for(i in 1:ncol(B1)){varabsmar[[1]][,i] <- apply(B1[,i]%*%t(Xmat[,i]), 1, var)}; colnames(varabsmar[[1]]) <- colnames(Xmat)
#	  for(i in 1:ncol(B2)){varabsmar[[2]][,i] <- apply(B2[,i]%*%t(Gmat[,i]), 1, var)}; colnames(varabsmar[[2]]) <- colnames(Gmat)
	  for(i in 1:ncol(B3)){varabsmar[[3]][,i] <- apply(B3[,i]%*%t(ETA$E$X[,i]), 1, var)}; colnames(varabsmar[[3]]) <- colnames(ETA$E$X)
 	  VCEm <- data.frame(varabsmar[[3]]) # varabsmar[[1]], varabsmar[[2]], 
	  save(VCEm, file=paste(paste(where, '_results/VCEm_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))

  save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}






if(args[2]==3){
  
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              E=list(X=Emat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, Emat, Gmat); gc()

  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  
  zz0 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- data.frame(matrix(0, nrow(zz0), 1)); colnames(zz1) <- "G"
  zz2 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_E_varB.dat', sep=''), header=F); colnames(zz2) <- "E"
  zz9 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz9)
  
  B1 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_X_b.dat', sep=''), header=T)
  B2 <- data.frame(matrix(0, nrow(zz0), length(Gvars)))
  B3 <- readBinMat(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_E_b.bin', sep=''))
  
  varabs <- matrix(NA, nrow(VCEm), 3); colnames(varabs) <- c("Vdes", "Vgen", "Venv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[, 2] <- 0
  varabs[, 3] <- apply(ETA$E$X%*%t(B3), 2, var)
  
  VCE <- data.frame(VCEm, varabs)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  BETA <- list()
  BETA[[1]] <- B1
  BETA[[2]] <- B2
  BETA[[3]] <- B3

	  varabsmar <- list()
	  for(e in c(3)){varabsmar[[e]] <- matrix(NA, nrow(VCE), ncol(BETA[[e]]))}
#	  for(i in 1:ncol(B1)){varabsmar[[1]][,i] <- apply(B1[,i]%*%t(Xmat[,i]), 1, var)}; colnames(varabsmar[[1]]) <- colnames(Xmat)
#	  for(i in 1:ncol(B2)){varabsmar[[2]][,i] <- apply(B2[,i]%*%t(Gmat[,i]), 1, var)}; colnames(varabsmar[[2]]) <- colnames(Gmat)
	  for(i in 1:ncol(B3)){varabsmar[[3]][,i] <- apply(B3[,i]%*%t(ETA$E$X[,i]), 1, var)}; colnames(varabsmar[[3]]) <- colnames(ETA$E$X)
 	  VCEm <- data.frame(varabsmar[[3]]) # varabsmar[[1]], varabsmar[[2]], 
	  save(VCEm, file=paste(paste(where, '_results/VCEm_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))

  save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}






rm(list=ls()); gc()

































