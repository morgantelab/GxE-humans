


rm(list=ls()); gc()

#library(gtools)
#library(gdata)
#library(reshape)
#library(BGLR)
#library(matrixcalc)

NAcol <- function(X){for(i in 1:ncol(X)){print(length(which(is.na(X[i]))))}}
'%!in%' <- function(x,y)!('%in%'(x,y))
censcale <- function(x,A,B){(A+(((x-mean(x)))/sd(x))*B)}






##################################
############ Load data ###########
##################################

setwd("")





load("../datasets/data3_20240209.RData")
nrow(dttt)

dttt$coh01 <- ifelse(dttt$coh=="0_1", 1, 0)
dttt$coh02 <- ifelse(dttt$coh=="0_2", 1, 0)
dttt$coh03 <- ifelse(dttt$coh=="0_3", 1, 0)
dttt$coh04 <- ifelse(dttt$coh=="0_4", 1, 0)
dttt$coh11 <- ifelse(dttt$coh=="1_1", 1, 0)
dttt$coh12 <- ifelse(dttt$coh=="1_2", 1, 0)
dttt$coh13 <- ifelse(dttt$coh=="1_3", 1, 0)
dttt$coh14 <- ifelse(dttt$coh=="1_4", 1, 0)
dttt$int <- 1
dttt$ord <- 1:nrow(dttt)

dt <- dttt

load("../G/eigenG_20230413.RData")
val <- eigenG[[1]][,1]
pc <- eigenG[[2]]
length(val); dim(pc)
for(i in 2:ncol(pc)){pc[,i] <- pc[,i]*val[i-1]}
dt <- merge(dttt, pc, by="ID", all.x=F, all.y=F); dim(dt)
dt <- dt[order(dt$ord),]


sumval <- val
for(i in 2:length(sumval)){sumval[i] <- sumval[i]+sumval[i-1]}
sumval <- 100*(sumval/sum(val))

Val <- data.frame(pc=paste("PC",1:nrow(eigenG[[1]]),sep=''), val=val, sumval=sumval); dim(Val)

if(gvar<100){
	Val$diff <- abs(Val$sumval-99.99)
	this <- which.min(Val$diff)
	pc <- pc[,c(1:this)]
	dim(pc)     
}

rm(pc, dttt); gc()





##################################
########### Load betas ###########
##################################

load(paste(where, '_results/BetasPred_run2.RData', sep=''))
for(tr in 101:127){Res[[tr]][[2]][[1]][[3]][tr-100] <- 0}




##################################
########### Variables ############
##################################


Gvars <- Val$pc; length(Gvars)
Ivars <- c("ID", "Sex_SI", "Sex_gen", "YOB", "MOB", "DOF", "COF", "ethn1", "ethn1_white", "ethn1_mixed", "ethn1_asian", "ethn1_black", "ethn2", "AOP", "AOR")
Emarc <-	c("Townsend", "walk_d", "act0_d", "act1_d", "TVtime", "sleep_d", "smoking_now", "veg_cook", "fish_oily", "fish_lean", "meat_proc", "poultry", "beef", "lamb", "pork", "cheese", "salt", "tea", "alc1", "waist")
Etiez <- c("PCtime", "DRtime", "getup", "coffee", "smoked_past", "BFP", "BMR")
Evars <- c(Emarc, Etiez); length(Evars)
Yvars <- c("PR0", "PR1",   "DP0", "DP1",   "SP0", "SP1",   "PP0", "PP1",   "PPm",    "DP0a", "DP1a",   "SP0a", "SP1a",  "PP0a", "PP1a",   "PPam") 
cohorts <- c("coh01", "coh02", "coh03", "coh04",  "coh11", "coh12", "coh13", "coh14")
Yvars[101:127] <- Evars






##################################
############## Pred ##############
##################################



N <- nrow(dt); N
P <- 27

Vars <- matrix(NA, 27, 3)
colnames(Vars) <- c("Y", "L", "e")
rownames(Vars) <- Evars

L <- e <- matrix(NA, N, P)
colnames(L) <- paste("L", Evars, sep='.')
colnames(e) <- paste("e", Evars, sep='.')

IDs <- dt$ID
Gmat <- as.matrix(dt[Gvars]); dim(Gmat); # NAcol(Gmat)
Xmat <- as.matrix(dt[c("int",cohorts)]); dim(Xmat); NAcol(Xmat)
Emat <- as.matrix(dt[Evars]); dim(Emat); NAcol(Emat)

for(tr in 101:127){
	print(tr)
	B2 <- Res[[tr]][[2]][[1]][[2]]
	B3 <- Res[[tr]][[2]][[1]][[3]]
	G2[,tr-100] <- Gmat%*%B2
	L2[,tr-100] <- Emat%*%B3
	rm(B1, B2, B3)
}


for(tr in 101:127){
	print(tr)
	e[,tr-100] <- Emat[,tr-100]-(D[,tr-100]+G[,tr-100]+L[,tr-100])
}

NewCov <- data.frame(ID=IDs, G2, L2, e2)

dt <- merge(dt, NewCov, by="ID", all.x=F, all.y=F); dim(dt)
dt <- dt[order(dt$ord),]


for(tr in 1:27){
	Vars[tr,1] <- var(Emat[,tr])
	Vars[tr,7] <- var(G2[,tr])
	Vars[tr,8] <- var(L2[,tr])
	Vars[tr,9] <- var(e2[,tr])
}



save(Vars, file="../datasets/PredVars_20240209.RData")
save(Val, file="../datasets/EigVal_20240209.RData")
save(dt, file="../datasets/data4_20240209.RData")











rm(list=ls()); gc()

































