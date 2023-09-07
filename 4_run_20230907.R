


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
#args <- c(14, 8, 1,  1100, 100, 10,   100, 6)
args




iter <- args[4]
burnin <- args[5]
thin <- args[6]
gvar <- args[7]
verb <- F
tol <- 0
where <- paste("run",args[8],sep=''); where
home <- "/data2/morgante_lab/ukbiobank_projects/GxE/run"
scratch <- "/scratch1/fabiom/tmp/BGLR"

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
nrow(Val)

load("../datasets/data4_20230628.RData")
nrow(dt)




if(args[2]%in%c(8,9,10)){
#	iter <- iter*2
#	burnin <- burnin*2
#	thin <- thin*2
}


if(args[2]==8){
	load("../datasets/eGEE_20230829.RData")
	GEEmat <- eGEE$vectors
	for(i in 1:ncol(GEEmat)){GEEmat[,i] <- GEEmat[,i]*eGEE$values[i]}
	eGEEvars <- colnames(GEEmat) <- paste("GEE", 1:ncol(GEEmat), sep='.')
	print(length(which(dt$ID!=eGEE$ids)))
	rm(eGEE); gc()
}


if(args[2]==9){
	load("../datasets/eGEL2EL2_20230829.RData")
	GEL2EL2mat <- eGEL2EL2$vectors
	for(i in 1:ncol(GEL2EL2mat)){GEL2EL2mat[,i] <- GEL2EL2mat[,i]*eGEL2EL2$values[i]}
	eGEL2EL2vars <- colnames(GEL2EL2mat) <- paste("GEL2EL2", 1:ncol(GEL2EL2mat), sep='.')
	print(length(which(dt$ID!=eGEL2EL2$ids)))
	rm(eGEL2EL2); gc()
}


if(args[2]==10){
	load("../datasets/eGEe2Ee2_20230829.RData")
	GEe2Ee2mat <- eGEe2Ee2$vectors
	for(i in 1:ncol(GEe2Ee2mat)){GEe2Ee2mat[,i] <- GEe2Ee2mat[,i]*eGEe2Ee2$values[i]}
	eGEe2Ee2vars <- colnames(GEe2Ee2mat) <- paste("GEe2Ee2", 1:ncol(GEe2Ee2mat), sep='.')
	print(length(which(dt$ID!=eGEe2Ee2$ids)))
	rm(eGEe2Ee2); gc()
}



#dt <- dt[1:1000,]
#GEEmat <- GEEmat[1:1000,]



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

gc()




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
  Phen <- phen <- Ymat[,args[1]]
  print(mean(phen))
  rm(Ymat)
}

if(args[1]>100){
  Cmat <- as.matrix(dt[Evars]); dim(Cmat); NAcol(Cmat)
  phen <- Cmat[,args[1]-100]
  print(mean(phen))
  rm(Cmat)
}

if(args[3]>1){
	phen <- phen*dt$trn
	print(summary(phen))
}



Gmat <- as.matrix(dt[Gvars]); dim(Gmat); # NAcol(Gmat)
Emat <- as.matrix(dt[Evars]); dim(Emat); NAcol(Emat)
Xmat <- as.matrix(dt[cohorts]); dim(Xmat); NAcol(Xmat)
EL2mat <- as.matrix(dt[paste("L2", Evars, sep='.')]); dim(EL2mat); NAcol(EL2mat)
Ee2mat <- as.matrix(dt[paste("e2", Evars, sep='.')]); dim(Ee2mat); NAcol(Ee2mat)
Xmat <- Xmat[,-1]


for(i in 1:ncol(EL2mat)){EL2mat[,i] <- censcale(EL2mat[,i], 0, 1)}
for(i in 1:ncol(Ee2mat)){Ee2mat[,i] <- censcale(Ee2mat[,i], 0, 1)}


if(args[1]>100){
  Emat[,args[1]-100] <- dt$randomvector
}

preds <- data.frame(ID=dt$ID, sub=dt$sub, coh=dt$coh, phen=Phen)

rm(dt); gc()






if(args[2]==1){
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE)
  )
  rm(Xmat, Emat, Gmat, EL2mat, Ee2mat); gc()
  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  
  zz0 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_varB.dat', sep=''), header=F); colnames(zz1) <- "G"
  zz2 <- data.frame(matrix(0, nrow(zz0), 1)); colnames(zz2) <- "E"
  zz9 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz9)
  
  B1 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_X_b.dat', sep=''), header=T)
  B2 <- readBinMat(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_b.bin', sep=''))
  B3 <-  data.frame(matrix(0, nrow(zz0), length(Evars)))
  
  varabs <- matrix(NA, nrow(VCEm), 3); colnames(varabs) <- c("Vdes", "Vgen", "Venv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[-c(1:(burnin/thin)), 2] <- apply(ETA$G$X%*%t(B2), 2, var)
  varabs[, 3] <- 0
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  BETA <- list()
  BETA[[1]] <- B1
  BETA[[2]] <- B2
  BETA[[3]] <- B3
  if(args[3]==1){save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))}

  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE)
  
}







if(args[2]==2){
  
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE), 
              E=list(X=Emat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, Emat, Gmat, EL2mat, Ee2mat); gc()

  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  
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
  
  VCE <- data.frame(VCEm, varabs)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  BETA <- list()
  BETA[[1]] <- B1
  BETA[[2]] <- B2
  BETA[[3]] <- B3

  if(args[3]==1){save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))}

  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}






if(args[2]==3){
  
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              E=list(X=Emat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, Emat, Gmat, EL2mat, Ee2mat); gc()

  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  
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
  varabs[, 2] <- 0
  varabs[-c(1:(burnin/thin)), 3] <- apply(ETA$E$X%*%t(B3), 2, var)
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  BETA <- list()
  BETA[[1]] <- B1
  BETA[[2]] <- B2
  BETA[[3]] <- B3

  if(args[3]==1){save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))}

  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))

 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}













if(args[2]==4){
  
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat[,c(1:20)], model="FIXED", saveEffects=TRUE) 
  )
   rm(Xmat, Emat, Gmat, EL2mat, Ee2mat); gc()

  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  
  zz0 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- data.frame(matrix(0, nrow(zz0), 1)); colnames(zz1) <- "G"
  zz2 <- data.frame(matrix(0, nrow(zz0), 1)); colnames(zz2) <- "E"
  zz9 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz9)
  
  B1 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_X_b.dat', sep=''), header=T)
  B2 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_b.dat', sep=''), header=T)
  B3 <- data.frame(matrix(0, nrow(zz0), length(Evars)))
  
  varabs <- matrix(NA, nrow(VCEm), 3); colnames(varabs) <- c("Vdes", "Vgen", "Venv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[, 2] <- apply(ETA$G$X%*%t(B2), 2, var)
  varabs[, 3] <- 0
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  BETA <- list()
  BETA[[1]] <- B1
  BETA[[2]] <- B2
  BETA[[3]] <- B3

  if(args[3]==1){save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))}

  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}










if(args[2]==5){
  
  ETA <- list(G=list(X=Gmat, model="BRR", saveEffects=TRUE))
  rm(Xmat, Emat, Gmat, EL2mat, Ee2mat); gc()
  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  
  zz0 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_varB.dat', sep=''), header=F); colnames(zz1) <- "G"
  zz2 <- data.frame(matrix(0, nrow(zz0), 1)); colnames(zz2) <- "E"
  zz9 <- read.table(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz9)
  
  B1 <- data.frame(matrix(0, nrow(zz0), length(cohorts)))
  B2 <- readBinMat(paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_ETA_G_b.bin', sep=''))
  B3 <- data.frame(matrix(0, nrow(zz0), length(Evars)))
  
  varabs <- matrix(NA, nrow(VCEm), 3); colnames(varabs) <- c("Vdes", "Vgen", "Venv")
  varabs[, 1] <- 0
  varabs[-c(1:(burnin/thin)), 2] <- apply(ETA$G$X%*%t(B2), 2, var)
  varabs[, 3] <- 0
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  BETA <- list()
  BETA[[1]] <- B1
  BETA[[2]] <- B2
  BETA[[3]] <- B3
  if(args[3]==1){save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))}
 
  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE)
  
}








if(args[2]==6){
  
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE), 
              E=list(X=Ee2mat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, Emat, Gmat, EL2mat, Ee2mat); gc()

  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  
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
  
  VCE <- data.frame(VCEm, varabs)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  BETA <- list()
  BETA[[1]] <- B1
  BETA[[2]] <- B2
  BETA[[3]] <- B3

  if(args[3]==1){save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))}
 
  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))

  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}








if(args[2]==7){
  
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE), 
              E=list(X=EL2mat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, Emat, Gmat, EL2mat, Ee2mat); gc()

  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  
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
  
  VCE <- data.frame(VCEm, varabs)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  BETA <- list()
  BETA[[1]] <- B1
  BETA[[2]] <- B2
  BETA[[3]] <- B3

  if(args[3]==1){save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))}

  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}







if(args[2]==8){
  
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE), 
              E=list(X=Emat, model="BRR", saveEffects=TRUE),
              GE=list(X=GEEmat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, Emat, Gmat, EL2mat, Ee2mat, GEEmat); gc()

  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  
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
  varabs[-c(1:(burnin/thin)), 3] <- apply(ETA$GE$X%*%t(B4), 2, var)
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  BETA <- list()
  BETA[[1]] <- B1
  BETA[[2]] <- B2
  BETA[[3]] <- B3
  BETA[[4]] <- B4

  if(args[3]==1){save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))}

  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz3, zz9, B1, B2, B3, B4, varabs, VCE, VCEm)
  
}








if(args[2]==9){
  
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE), 
              E=list(X=Emat, model="BRR", saveEffects=TRUE),
              GE=list(X=GEL2EL2mat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, Emat, Gmat, EL2mat, Ee2mat, GEL2EL2mat); gc()

  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  
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
  varabs[-c(1:(burnin/thin)), 3] <- apply(ETA$GE$X%*%t(B4), 2, var)
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  BETA <- list()
  BETA[[1]] <- B1
  BETA[[2]] <- B2
  BETA[[3]] <- B3
  BETA[[4]] <- B4

  if(args[3]==1){save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))}

  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz3, zz9, B1, B2, B3, B4, varabs, VCE, VCEm)
  
}








if(args[2]==10){
  
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE), 
              E=list(X=Emat, model="BRR", saveEffects=TRUE),
              GE=list(X=GEe2Ee2mat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, Emat, Gmat, EL2mat, Ee2mat, GEe2Ee2mat); gc()

  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(scratch, '/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  
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
  varabs[-c(1:(burnin/thin)), 3] <- apply(ETA$GE$X%*%t(B4), 2, var)
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs)
  save(VCE, file=paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  BETA <- list()
  BETA[[1]] <- B1
  BETA[[2]] <- B2
  BETA[[3]] <- B3
  BETA[[4]] <- B4

  if(args[3]==1){save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))}

  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz3, zz9, B1, B2, B3, B4, varabs, VCE, VCEm)
  
}











rm(list=ls()); gc()


































