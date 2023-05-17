


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
#args <- c(1, 2, 1,  60000, 0, 5,   100)
args




iter <- args[4]
burnin <- args[5]
thin <- args[6]
verb <- F
tol <- 0
gvar <- 100
where <- "run2"


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


sumval <- val
for(i in 2:length(sumval)){sumval[i] <- sumval[i]+sumval[i-1]}
sumval <- 100*(sumval/sum(val))

Val <- data.frame(pc=paste("PC",1:nrow(eigenG[[1]]),sep=''), val=val, sumval=sumval); dim(Val)
head(Val)
tail(Val)

if(gvar<100){
	Val$diff <- abs(Val$sumval-gvar)
	this <- which.min(Val$diff)
	pc <- pc[,c(1:this)]
	dim(pc)     
}
#Val <- subset(Val, val>=tol); nrow(Val)


table(pc$ID==dttt$ID)/nrow(dttt)
ord <- data.frame(ID=pc$ID, ord=1:nrow(pc))
dt <- merge(ord, dttt, by="ID", all.x=F, all.y=F); dim(dt)
dt <- dt[order(dt$ord),]
length(which(pc$ID!=dt$ID))
rm(ord)




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


Gmat <- as.matrix(pc[Gvars]); dim(Gmat); # NAcol(Gmat)
Emat <- as.matrix(dt[Evars]); dim(Emat); NAcol(Emat)
Xmat <- as.matrix(dt[cohorts]); dim(Xmat); NAcol(Xmat)
Xmat <- Xmat[,-1]

if(args[1]>100){
  Emat[,args[1]-100] <- dt$randomvector
}



#phen <- censcale(phen, 100, 10)  
rm(eigenG, val, pc); gc()

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
#  BETA[[2]] <- B2
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
#  BETA[[2]] <- B2
  BETA[[3]] <- B3
  save(BETA, file=paste(paste(where, '_results/BETA_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
    varabsmar <- list()
    for(e in c(1:3)){varabsmar[[e]] <- matrix(NA, nrow(VCE), ncol(BETA[[e]]))}
#    for(i in 1:ncol(B1)){varabsmar[[1]][,i] <- apply(B1[,i]%*%t(Xmat[,i]), 1, var)}; colnames(varabsmar[[1]]) <- colnames(Xmat)
#    for(i in 1:ncol(B2)){varabsmar[[2]][,i] <- apply(B2[,i]%*%t(Gmat[,i]), 1, var)}; colnames(varabsmar[[2]]) <- colnames(Gmat)
    for(i in 1:ncol(B3)){varabsmar[[3]][,i] <- apply(B3[,i]%*%t(ETA$E$X[,i]), 1, var)}; colnames(varabsmar[[3]]) <- colnames(ETA$E$X)
    VCEm <- data.frame(varabsmar[[3]]) # varabsmar[[1]], varabsmar[[2]], 
    save(VCEm, file=paste(paste(where, '_results/VCEm_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
 
  rm(ETA, model, zz0, zz1, zz2, zz9, B1, B2, B3, varabs, VCE, VCEm)
  
}






rm(list=ls()); gc()


































