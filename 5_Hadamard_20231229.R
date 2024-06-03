


# R CMD BATCH "--args 80 0 1 1" 5_Hadamard_20231011.R log5_80_0_1_1.txt
# R CMD BATCH "--args 99 0 1 1" 5_Hadamard_20231011.R log5_99_0_1_1.txt
# R CMD BATCH "--args 99 0 0 0 1" 5_Hadamard_20231229.R log5_99_0_0_0_1.txt





rm(list=ls()); gc()

library(matrixcalc)

'%!in%' <- function(x,y)!('%in%'(x,y))
censcale <- function(x,A,B){(A+(((x-mean(x)))/sd(x))*B)}

args <- as.numeric(commandArgs(TRUE))
#args <- c(80, 1, 1, 1)
args


home <- "/data2/morgante_lab/ukbiobank_projects/GxE/run"
setwd(home)


load("../datasets/EigVal_20230628.RData")
head(Val)
tail(Val)

nrow(Val)
Val <- subset(Val, val>0)
nrow(Val)
val <- Val$val

these <- c(1:which.min(abs(Val$sumval-args[1])))


load("../datasets/data4_20230628.RData")
nrow(dt)



##################################
########### Variables ############
##################################


Ivars <- c("ID", "Sex_SI", "Sex_gen", "YOB", "MOB", "DOF", "COF", "ethn1", "ethn1_white", "ethn1_mixed", "ethn1_asian", "ethn1_black", "ethn2", "AOP", "AOR")
Gpcv <- Val$pc; length(Gpcv)
Emarc <-	c("Townsend", "walk_d", "act0_d", "act1_d", "TVtime", "sleep_d", "smoking_now", "veg_cook", "fish_oily", "fish_lean", "meat_proc", "poultry", "beef", "lamb", "pork", "cheese", "salt", "tea", "alc1", "waist")
Etiez <- c("PCtime", "DRtime", "getup", "coffee", "smoked_past", "BFP", "BMR")
Evars <- c(Emarc, Etiez); length(Evars)
Yvars <- c("PR0", "PR1",   "DP0", "DP1",   "SP0", "SP1",   "PP0", "PP1",   "PPm",    "DP0a", "DP1a",   "SP0a", "SP1a",  "PP0a", "PP1a",   "PPam") 
cohorts <- c("coh01", "coh02", "coh03", "coh04",  "coh11", "coh12", "coh13", "coh14")
Avars <- c(Emarc, Etiez, "Sex_SI", "AOP"); length(Avars)


Xmat <- as.matrix(dt[cohorts]); dim(Xmat)
Xmat <- Xmat[,-1]



Gpc1 <- as.matrix(dt[Gpcv])[,these]; dim(Gpc1)
Gpc2 <- Gpc1; for(i in 1:ncol(Gpc2)){Gpc2[,i] <- Gpc2[,i]/val[i]}
G <- Gpc1%*%t(Gpc2); dim(G); G[1:10,1:10]






### E ###

if(args[2]==1){

	Emat <- as.matrix(dt[Evars]); dim(Emat)
	for(i in 1:ncol(Emat)){Emat[,i] <- censcale(Emat[,i], 0, 1)}
	Emat <- Emat/ncol(Emat)
	EE <- Emat%*%t(Emat); dim(EE); EE[1:10,1:10]
	summary(diag(EE))

	wei <- sum(diag(G))/sum(diag(EE)); wei
	EE <- EE*wei
	summary(diag(EE))
	EE[1:10,1:10]

	GEE <- hadamard.prod(G,EE); dim(GEE); GEE[1:10,1:10]
	summary(diag(GEE))
	dGEE <- diag(GEE)
	eGEE <- eigen(GEE)
	eGEE[["ids"]] <- dt$ID

	save(dGEE, file=paste("../datasets/dG", args[1], "EE_20231010.RData", sep=''))
	save(eGEE, file=paste("../datasets/eG", args[1], "EE_20231010.RData", sep=''))
	write.table(eGEE$values, paste("../datasets/eG", args[1], "EE_values_20231010.txt", sep=''), col.names=F, row.names=F, quote=F)

	rm(Emat, EE, wei, GEE, dGEE, eGEE)

}







### EL2 ###

if(args[3]==1){

	EL2mat <- as.matrix(dt[paste("L2", Evars, sep='.')]); dim(EL2mat)
	for(i in 1:ncol(EL2mat)){EL2mat[,i] <- censcale(EL2mat[,i], 0, 1)}
	EL2mat <- EL2mat/ncol(EL2mat)
	EL2EL2 <- EL2mat%*%t(EL2mat); dim(EL2EL2); EL2EL2[1:10,1:10]
	summary(diag(EL2EL2))

	wei <- sum(diag(G))/sum(diag(EL2EL2)); wei
	EL2EL2 <- EL2EL2*wei
	summary(diag(EL2EL2))
	EL2EL2[1:10,1:10]

	GEL2EL2 <- hadamard.prod(G,EL2EL2); dim(GEL2EL2); GEL2EL2[1:10,1:10]
	summary(diag(GEL2EL2))
	dGEL2EL2 <- diag(GEL2EL2)
	eGEL2EL2 <- eigen(GEL2EL2)
	eGEL2EL2[["ids"]] <- dt$ID

	save(dGEL2EL2, file=paste("../datasets/dG", args[1], "EL2EL2_20231010.RData", sep=''))
	save(eGEL2EL2, file=paste("../datasets/eG", args[1], "EL2EL2_20231010.RData", sep=''))
	write.table(eGEL2EL2$values, paste("../datasets/eG", args[1], "EL2EL2_values_20231010.txt", sep=''), col.names=F, row.names=F, quote=F)

	rm(EL2mat, EL2EL2, wei, GEL2EL2, dGEL2EL2, eGEL2EL2)

}





### Ee2 ###

if(args[4]==1){

	Ee2mat <- as.matrix(dt[paste("e2", Evars, sep='.')]); dim(Ee2mat)
	for(i in 1:ncol(Ee2mat)){Ee2mat[,i] <- censcale(Ee2mat[,i], 0, 1)}
	Ee2mat <- Ee2mat/ncol(Ee2mat)
	Ee2Ee2 <- Ee2mat%*%t(Ee2mat); dim(Ee2Ee2); Ee2Ee2[1:10,1:10]
	summary(diag(Ee2Ee2))

	wei <- sum(diag(G))/sum(diag(Ee2Ee2)); wei
	Ee2Ee2 <- Ee2Ee2*wei
	summary(diag(Ee2Ee2))
	Ee2Ee2[1:10,1:10]

	GEe2Ee2 <- hadamard.prod(G,Ee2Ee2); dim(GEe2Ee2); GEe2Ee2[1:10,1:10]
	summary(diag(GEe2Ee2))
	dGEe2Ee2 <- diag(GEe2Ee2)
	eGEe2Ee2 <- eigen(GEe2Ee2)
	eGEe2Ee2[["ids"]] <- dt$ID

	save(dGEe2Ee2, file=paste("../datasets/dG", args[1], "Ee2Ee2_20231010.RData", sep=''))
	save(eGEe2Ee2, file=paste("../datasets/eG", args[1], "Ee2Ee2_20231010.RData", sep=''))
	write.table(eGEe2Ee2$values, paste("../datasets/eG", args[1], "Ee2Ee2_values_20231010.txt", sep=''), col.names=F, row.names=F, quote=F)

	rm(Ee2mat, Ee2Ee2, wei, GEe2Ee2, dGEe2Ee2, eGEe2Ee2)

}






### A ###

if(args[5]==1){

	Amat <- as.matrix(dt[Avars]); dim(Amat)
	for(i in 1:ncol(Amat)){Amat[,i] <- censcale(Amat[,i], 0, 1)}
	Amat <- Amat/ncol(Amat)
	AA <- Amat%*%t(Amat); dim(AA); AA[1:10,1:10]
	summary(diag(AA))

	wei <- sum(diag(G))/sum(diag(AA)); wei
	AA <- AA*wei
	summary(diag(AA))
	AA[1:10,1:10]

	GAA <- hadamard.prod(G,AA); dim(GAA); GAA[1:10,1:10]
	summary(diag(GAA))
	dGAA <- diag(GAA)
	eGAA <- eigen(GAA)
	eGAA[["ids"]] <- dt$ID

	save(dGAA, file=paste("../datasets/dG", args[1], "AA_20231229.RData", sep=''))
	save(eGAA, file=paste("../datasets/eG", args[1], "AA_20231229.RData", sep=''))
	write.table(eGAA$values, paste("../datasets/eG", args[1], "AA_values_20231229.txt", sep=''), col.names=F, row.names=F, quote=F)

	rm(Amat, AA, wei, GAA, dGAA, eGAA)

}






rm(list=ls()); gc()

































