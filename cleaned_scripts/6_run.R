


rm(list=ls()); gc()

library(BGLR)

NAcol <- function(X){for(i in 1:ncol(X)){print(length(which(is.na(X[i]))))}}
'%!in%' <- function(x,y)!('%in%'(x,y))
censcale <- function(x,A,B){(A+(((x-mean(x)))/sd(x))*B)}


args <- as.numeric(commandArgs(TRUE))


iter <- 60000
burnin <- 10000
thin <- 50
verb <- T
home <- ""
scratch <- ""
trait <- args[1] #DP,SP,PP
model <- args[2] #M0,M1, etc.
analysis <- args[3] #Whole data, Y-O, RND
where <- paste("run",args[4],sep=''); where



setwd(home)






##################################
############ Load data ###########
##################################


load("../datasets/EigVal_20230628.RData")
head(Val)
tail(Val)

nrow(Val)
Val <- subset(Val, val>0)
nrow(Val)

load("../datasets/data4_20240209.RData")
nrow(dt)







##################################
########### Variables ############
##################################


Ivars <- c("ID", "Sex_SI", "Sex_gen", "YOB", "MOB", "DOF", "COF", "ethn1", "ethn1_white", "ethn1_mixed", "ethn1_asian", "ethn1_black", "ethn2", "AOP", "AOR")
Gvars <- Val$pc; length(Gvars)
Emarc <- c("Townsend", "walk_d", "act0_d", "act1_d", "TVtime", "sleep_d", "smoking_now", "veg_cook", "fish_oily", "fish_lean", "meat_proc", "poultry", "beef", "lamb", "pork", "cheese", "salt", "tea", "alc1", "waist")
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



if(model%in%c("m1","m11","m21")){
	load("../datasets/eM_20231010trim.RData")
	Mmat <- eM$vectors
	for(i in 1:ncol(Mmat)){Mmat[,i] <- Mmat[,i]*eM$values[i]}
	colnames(Mmat) <- paste("M", 1:ncol(Mmat), sep='.')
	print(length(which(dt$ID!=eM$ids)))
	rm(eM); gc()
}

if(model%in%c("m21")){
	load("../datasets/eGM_20231002.RData")
	keep <- which(eGM$values>0)
	GMmat <- eGM$vectors[,keep]
	for(i in 1:ncol(GMmat)){GMmat[,i] <- GMmat[,i]*eGM$values[i]}
	colnames(GMmat) <- paste("GM", 1:ncol(GMmat), sep='.')
	print(length(which(dt$ID!=eGM$ids)))
	rm(eGM, keep); gc()
}





if(model%in%c("m2","m12","m22")){
	load("../datasets/eL_20231010trim.RData")
	Lmat <- eL$vectors
	for(i in 1:ncol(Lmat)){Lmat[,i] <- Lmat[,i]*eL$values[i]}
	colnames(Lmat) <- paste("L", 1:ncol(Lmat), sep='.')
	print(length(which(dt$ID!=eL$ids)))
	rm(eL); gc()
}

if(model%in%c("m22")){
	load("../datasets/eGL_20231010.RData")
	keep <- which(eGL$values>0)
	GLmat <- eGL$vectors[,keep]
	for(i in 1:ncol(GLmat)){GLmat[,i] <- GLmat[,i]*eGL$values[i]}
	colnames(GLmat) <- paste("GL", 1:ncol(GLmat), sep='.')
	print(length(which(dt$ID!=eGL$ids)))
	rm(eGL, keep); gc()
}





if(model%in%c("m3","m13","m23")){
	load("../datasets/e99Ee2Ee2_20231010trim.RData")
	Emat <- eE$vectors
	for(i in 1:ncol(Emat)){Emat[,i] <- Emat[,i]*eE$values[i]}
	colnames(Emat) <- paste("E", 1:ncol(Emat), sep='.')
	print(length(which(dt$ID!=eE$ids)))
	rm(eE); gc()
}

if(model%in%c("m23")){
	load("../datasets/eGE_20231010.RData")
	keep <- which(eGE$values>0)
	GEmat <- eGE$vectors[,keep]
	for(i in 1:ncol(GEmat)){GEmat[,i] <- GEmat[,i]*eGE$values[i]}
	colnames(GEmat) <- paste("GE", 1:ncol(GEmat), sep='.')
	print(length(which(dt$ID!=eGE$ids)))
	rm(eGE, keep); gc()
}















##################################
########### Cross-val ############
##################################


dt$trn <- NA

if(analysis==1){dt$trn <- 1}

if(analysis==11){dt$trn <- replace(dt$trn, dt$sub%in%c(11,12,13,14), 1)}

if(analysis==103){dt$trn <- replace(dt$trn, dt$coh%in%c("0_1", "0_2", "1_1", "1_2"), 1)} # C

summary(dt$trn)
nrow(dt)

if(analysis==1){Xmat <- Xmat[,-1]}
if(analysis==101){Xmat <- Xmat[,which(colnames(Xmat)%in%c("coh01", "coh02", "coh03", "coh04"))]; Xmat <- Xmat[,-1]} 
if(analysis==102){Xmat <- Xmat[,which(colnames(Xmat)%in%c("coh11", "coh12", "coh13", "coh14"))]; Xmat <- Xmat[,-1]} 
if(analysis==103){Xmat <- Xmat[,which(colnames(Xmat)%in%c("coh01", "coh02", "coh11", "coh12"))]; Xmat <- Xmat[,-1]} 
if(analysis==104){Xmat <- Xmat[,which(colnames(Xmat)%in%c("coh03", "coh04", "coh13", "coh14"))]; Xmat <- Xmat[,-1]} 

summary(Xmat)
gc()









##################################
############# Models #############
##################################


Phen <- phen <- Ymat[,trait]
print(mean(phen))
#rm(Ymat)


if(analysis>1){
	phen <- phen*dt$trn
	print(summary(phen))
}


preds <- data.frame(ID=dt$ID, sub=dt$sub, coh=dt$coh, phen=Phen)

rm(dt); gc()






# Define model #

if(model%in%c("m0")){
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE) 
  )
   rm(Xmat, Gmat); gc()

}


if(model%in%c("m01")){
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat[,1:20], model="BRR", saveEffects=TRUE) 
  )
   rm(Xmat, Gmat); gc()

}


if(model%in%c("m02")){
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE))
   rm(Xmat); gc()

}


if(model%in%c("m1","m2","m3")){
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              E=list(X=Emat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, Emat, Gmat); gc()

}


if(model%in%c("m11","m12","m13")){
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE), 
              E=list(X=Emat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, Emat, Gmat); gc()

}


if(model%in%c("m21","m22","m23")){

  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE), 
              E=list(X=Emat, model="BRR", saveEffects=TRUE),
              GE=list(X=GEmat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, Emat, Gmat, GEmat); gc()

}





# Run model #


model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste(scratch, '/run_', trait, '_', model, '_', analysis, '_', sep=''))







# Collect results #


if(model%in%c("m0", "m01")){
  
  zz0 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_G_varB.dat', sep=''), header=F); colnames(zz1) <- "G"
  zz2 <- data.frame(matrix(0, nrow(zz0), 1)); colnames(zz2) <- "E"
  zz9 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz9)
  
  B1 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_X_b.dat', sep=''), header=T)
  B2 <- readBinMat(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_G_b.bin', sep=''))
  B3 <- data.frame(matrix(0, nrow(zz0), length(Evars)))
  
  varabs <- matrix(NA, nrow(VCEm), 3); colnames(varabs) <- c("Vdes", "Vgen", "Venv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[-c(1:(burnin/thin)), 2] <- apply(ETA$G$X%*%t(B2), 2, var)
  varabs[-c(1:(burnin/thin)), 3] <- 0
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs, B1)
  save(VCE, file=paste(paste(where, '_results/VCE_', trait, '_', model, '_', analysis, '.RData', sep='')))
  
  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', trait, '_', model, '_', analysis, '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}


if(model%in%c("m02")){
  
  zz0 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- data.frame(matrix(0, nrow(zz0), 1)); colnames(zz1) <- "G"
  zz2 <- data.frame(matrix(0, nrow(zz0), 1)); colnames(zz2) <- "E"
  zz9 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz9)
  
  B1 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_X_b.dat', sep=''), header=T)
  B2 <- data.frame(matrix(0, nrow(zz0), length(Gvars)))
  B3 <- data.frame(matrix(0, nrow(zz0), length(Evars)))
  
  varabs <- matrix(NA, nrow(VCEm), 3); colnames(varabs) <- c("Vdes", "Vgen", "Venv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[-c(1:(burnin/thin)), 2] <- 0
  varabs[-c(1:(burnin/thin)), 3] <- 0
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs, B1)
  save(VCE, file=paste(paste(where, '_results/VCE_', trait, '_', model, '_', analysis, '.RData', sep='')))
  
  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', trait, '_', model, '_', analysis, '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}


if(model%in%c("m1","m2","m3")){
  
  zz0 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- data.frame(matrix(0, nrow(zz0), 1)); colnames(zz1) <- "G"
  zz2 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_E_varB.dat', sep=''), header=F); colnames(zz2) <- "E"
  zz9 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz9)
  
  B1 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_X_b.dat', sep=''), header=T)
  B2 <- data.frame(matrix(0, nrow(zz0), length(Gvars)))
  B3 <- readBinMat(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_E_b.bin', sep=''))
  
  varabs <- matrix(NA, nrow(VCEm), 3); colnames(varabs) <- c("Vdes", "Vgen", "Venv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[-c(1:(burnin/thin)), 2] <- 0
  varabs[-c(1:(burnin/thin)), 3] <- apply(ETA$E$X%*%t(B3), 2, var)
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs, B1)
  save(VCE, file=paste(paste(where, '_results/VCE_', trait, '_', model, '_', analysis, '.RData', sep='')))
  
  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', trait, '_', model, '_', analysis, '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}


if(model%in%c("m11","m12","m13")){
  
  zz0 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_G_varB.dat', sep=''), header=F); colnames(zz1) <- "G"
  zz2 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_E_varB.dat', sep=''), header=F); colnames(zz2) <- "E"
  zz9 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz9)
  
  B1 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_X_b.dat', sep=''), header=T)
  B2 <- readBinMat(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_G_b.bin', sep=''))
  B3 <- readBinMat(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_E_b.bin', sep=''))
  
  varabs <- matrix(NA, nrow(VCEm), 3); colnames(varabs) <- c("Vdes", "Vgen", "Venv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[-c(1:(burnin/thin)), 2] <- apply(ETA$G$X%*%t(B2), 2, var)
  varabs[-c(1:(burnin/thin)), 3] <- apply(ETA$E$X%*%t(B3), 2, var)
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs, B1)
  save(VCE, file=paste(paste(where, '_results/VCE_', trait, '_', model, '_', analysis, '.RData', sep='')))
  
  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', trait, '_', model, '_', analysis, '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}


if(model%in%c("m21","m22","m23")){
  
  zz0 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  zz1 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_G_varB.dat', sep=''), header=F); colnames(zz1) <- "G"
  zz2 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_E_varB.dat', sep=''), header=F); colnames(zz2) <- "E"
  zz3 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_GE_varB.dat', sep=''), header=F); colnames(zz3) <- "GE"
  zz9 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_varE.dat', sep=''), header=F); colnames(zz9) <- "res"
  VCEm <- data.frame(zz0, zz1, zz2, zz3, zz9)
  
  B1 <- read.table(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_X_b.dat', sep=''), header=T)
  B2 <- readBinMat(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_G_b.bin', sep=''))
  B3 <- readBinMat(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_E_b.bin', sep=''))
  B4 <- readBinMat(paste(scratch, '/run_', trait, '_', model, '_', analysis, '_ETA_GE_b.bin', sep=''))
  
  varabs <- matrix(NA, nrow(VCEm), 4); colnames(varabs) <- c("Vdes", "Vgen", "Venv", "Vgenenv")
  varabs[, 1] <- apply(ETA$X$X%*%t(B1), 2, var)
  varabs[-c(1:(burnin/thin)), 2] <- apply(ETA$G$X%*%t(B2), 2, var)
  varabs[-c(1:(burnin/thin)), 3] <- apply(ETA$E$X%*%t(B3), 2, var)
  varabs[-c(1:(burnin/thin)), 4] <- apply(ETA$GE$X%*%t(B4), 2, var)
 
  setwd(home) 
  
  VCE <- data.frame(VCEm, varabs, B1)
  save(VCE, file=paste(paste(where, '_results/VCE_', trait, '_', model, '_', analysis, '.RData', sep='')))
  
  preds	<- data.frame(preds, y=model$y, yHat=model$yHat)
  save(preds, file=paste(paste(where, '_results/PREDS_', trait, '_', model, '_', analysis, '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz3, zz9, B1, B2, B3, B4, varabs, VCE, VCEm)
  
}



### Pearson correlation ###
cor(preds$y, preds$yhat)

### Bias ###
lm(y ~ yhat, data=preds)







rm(list=ls()); gc()

































