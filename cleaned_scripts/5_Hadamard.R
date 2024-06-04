





rm(list=ls()); gc()

library(matrixcalc)

'%!in%' <- function(x,y)!('%in%'(x,y))
censcale <- function(x,A,B){(A+(((x-mean(x)))/sd(x))*B)}


home <- ""
setwd(home)


load("../datasets/EigVal_20230628.RData")
head(Val)
tail(Val)

nrow(Val)
Val <- subset(Val, val>0)
nrow(Val)
val <- Val$val

these <- c(1:which.min(abs(Val$sumval-args[1])))


load("../datasets/data4_20240209.RData")
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




Gpc1 <- as.matrix(dt[Gpcv])[,these]; dim(Gpc1)
Gpc2 <- Gpc1; for(i in 1:ncol(Gpc2)){Gpc2[,i] <- Gpc2[,i]/val[i]}
G <- Gpc1%*%t(Gpc2); dim(G); G[1:10,1:10]



### M ###


	Mmat <- as.matrix(dt[Mvars]); dim(Mmat)
	for(i in 1:ncol(Mmat)){Mmat[,i] <- censcale(Mmat[,i], 0, 1)}
	Mmat <- Mmat/ncol(Mmat)
	M <- Mmat%*%t(Mmat); dim(M); M[1:10,1:10]
	summary(diag(M))

	wei <- sum(diag(G))/sum(diag(m)); wei
	M <- M*wei
	summary(diag(M))
	M[1:10,1:10]
	dM <- diag(M)
	eM <- eigen(M)
	eM[["ids"]] <- dt$ID

	save(dM, file=paste("../datasets/dM_20231010.RData", sep=''))
	save(eM, file=paste("../datasets/eM_20231010.RData", sep=''))
	write.table(eM$values, paste("../datasets/eM_values.txt", sep=''), col.names=F, row.names=F, quote=F)


	GM <- hadamard.prod(G,M); dim(GM); GM[1:10,1:10]
	summary(diag(GM))
	dGM <- diag(GM)
	eGM <- eigen(GM)
	eGM[["ids"]] <- dt$ID

	save(dGM, file=paste("../datasets/dGM_20231010.RData", sep=''))
	save(eGM, file=paste("../datasets/eGM_20231010.RData", sep=''))
	write.table(eGM$values, paste("../datasets/eGM_values.txt", sep=''), col.names=F, row.names=F, quote=F)

	rm(GM, dGM, eGM)

	rm(Mmat, M, wei, dM, eM)








### L ###


	Lmat <- as.matrix(dt[paste("L", Evars, sep='.')]); dim(Lmat)
	for(i in 1:ncol(Lmat)){Lmat[,i] <- censcale(Lmat[,i], 0, 1)}
	Lmat <- Lmat/ncol(Lmat)
	L <- Lmat%*%t(Lmat); dim(L); L[1:10,1:10]
	summary(diag(L))

	wei <- sum(diag(G))/sum(diag(L)); wei
	L <- L*wei
	summary(diag(EL2EL2))
	L[1:10,1:10]
	dL <- diag(L)
	eL <- eigen(L)
	eL[["ids"]] <- dt$ID

	save(dL, file=paste("../datasets/dL.RData", sep=''))
	save(eL, file=paste("../datasets/eL.RData", sep=''))
	write.table(eL$values, paste("../datasets/eL.txt", sep=''), col.names=F, row.names=F, quote=F)


	GL <- hadamard.prod(G,L); dim(GL); GL[1:10,1:10]
	summary(diag(GL))
	dGL <- diag(GL)
	eGL <- eigen(GL)
	eGL[["ids"]] <- dt$ID

	save(dGL, file=paste("../datasets/dGL.RData", sep=''))
	save(eGL, file=paste("../datasets/eGL.RData", sep=''))
	write.table(eGL$values, paste("../datasets/eGL.txt", sep=''), col.names=F, row.names=F, quote=F)

	rm(GL, dGL, eGL)

	rm(Lmat, L, wei, dL, eL)






### e ###


	Emat <- as.matrix(dt[paste("E", Evars, sep='.')]); dim(Emat)
	for(i in 1:ncol(Emat)){Emat[,i] <- censcale(Emat[,i], 0, 1)}
	Emat <- Emat/ncol(Emat)
	E <- Emat%*%t(Emat); dim(E); E[1:10,1:10]
	summary(diag(E))

	wei <- sum(diag(G))/sum(diag(E)); wei
	E <- E*wei
	summary(diag(E))
	E[1:10,1:10]
	dE <- diag(E)
	eE <- eigen(E)
	eE[["ids"]] <- dt$ID

	save(dE, file=paste("../datasets/dE.RData", sep=''))
	save(eE, file=paste("../datasets/eE.RData", sep=''))
	write.table(eE$values, paste("../datasets/eE.txt", sep=''), col.names=F, row.names=F, quote=F)


	GE <- hadamard.prod(G,E); dim(GE); GE[1:10,1:10]
	summary(diag(GE))
	dGE <- diag(GE)
	eGE <- eigen(GE)
	eGE[["ids"]] <- dt$ID

	save(dGE, file=paste("../datasets/dGE.RData", sep=''))
	save(eGE, file=paste("../datasets/eGE.RData", sep=''))
	write.table(eGE$values, paste("../datasets/eGE.txt", sep=''), col.names=F, row.names=F, quote=F)

	rm(GE, dGE, eGE)

	rm(Emat, E, wei, dE, eE)







rm(list=ls()); gc()















