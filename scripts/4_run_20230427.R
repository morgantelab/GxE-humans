


rm(list=ls()); gc()

#library(gtools)
#library(gdata)
#library(reshape)
library(BGLR)
#library(matrixcalc)

NAcol <- function(X){for(i in 1:ncol(X)){print(length(which(is.na(X[i]))))}}
'%!in%' <- function(x,y)!('%in%'(x,y))
censcale <- function(x,A,B){(A+(((x-mean(x)))/sd(x))*B)}

IncMat <- function(Var, X){
  levels <- unique(Var); nlev <- length(levels)
  X <- matrix(0, length(Var), nlev)
  for(i in 1:nlev){
    X[which(Var==levels[i]),i] <- 1
  }
  colnames(X) <- levels
  return(X)
}



args <- as.numeric(commandArgs(TRUE))
#args <- c(14,2,1,  50000, 0, 5)
args




iter <- args[4]
burnin <- args[5]
thin <- args[6]
verb <- F
tol <- 0
gvar <- 100



##################################
############ Load data ###########
##################################

setwd("/data2/morgante_lab/ukbiobank_projects/GxE/run")

confirm <- read.table("../read.txt", header=F)




load("../datasets/data3_20230413.RData")
nrow(dttt)


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








##################################
############# Models #############
##################################

system("mkdir run1_output")
system("mkdir run1_results")

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
Xmat <- IncMat(dt$coh); dim(Xmat); NAcol(Xmat)
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
  rm(Xmat, Gmat); gc()
  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste('run1_output/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))

  rm(ETA, model)
  
}







if(args[2]==2){
  
  
  ETA <- list(X=list(X=Xmat, model="FIXED", saveEffects=TRUE), 
              G=list(X=Gmat, model="BRR", saveEffects=TRUE), 
              E=list(X=Emat, model="BRR", saveEffects=TRUE)
  )
   rm(Xmat, Emat, Gmat); gc()

  
  model <- BGLR(y=phen, ETA=ETA, nIter=iter, burnIn=burnin, thin=thin, verbose=verb, saveAt=paste('run1_output/run_', args[1], '_', args[2], '_', args[3], '_', sep=''))
  rm(ETA, model)
  
}






rm(list=ls()); gc()


































