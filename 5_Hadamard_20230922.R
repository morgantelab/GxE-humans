


rm(list=ls()); gc()

library(matrixcalc)

'%!in%' <- function(x,y)!('%in%'(x,y))
censcale <- function(x,A,B){(A+(((x-mean(x)))/sd(x))*B)}


home <- "/data2/morgante_lab/ukbiobank_projects/GxE/run"
setwd(home)


load("../datasets/EigVal_20230628.RData")
head(Val)
tail(Val)

nrow(Val)
Val <- subset(Val, val>0)
nrow(Val)
val <- Val$val

these <- c(1:which.min(abs(Val$sumval-80)))


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


Xmat <- as.matrix(dt[cohorts]); dim(Xmat)
Xmat <- Xmat[,-1]



Gpc1 <- as.matrix(dt[Gpcv])[,these]; dim(Gpc1)
Gpc2 <- Gpc1; for(i in 2:ncol(Gpc2)){Gpc2[,i] <- Gpc2[,i]/val[i]}
G <- Gpc1%*%t(Gpc2); dim(G); G[1:10,1:10]





### E ###

Emat <- as.matrix(dt[Evars]); dim(Emat)
for(i in 1:ncol(Emat)){Emat[,i] <- censcale(Emat[,i], 0, 1)}
Emat <- Emat/ncol(Emat)
EE <- Emat%*%t(Emat); dim(EE); EE[1:10,1:10]
summary(diag(EE))

GEE <- hadamard.prod(G,EE); dim(GEE)
summary(diag(GEE))
eGEE <- eigen(GEE)
eGEE[["ids"]] <- dt$ID

save(eGEE, file="../datasets/eG80EE_20230922.RData")
write.table(eGEE$values, "../datasets/eG80EE_values_20230922.txt", col.names=F, row.names=F, quote=F)

rm(Emat, EE, eGEE)










### EL2mat ###

EL2mat <- as.matrix(dt[paste("L2", Evars, sep='.')]); dim(EL2mat)
for(i in 1:ncol(EL2mat)){EL2mat[,i] <- censcale(EL2mat[,i], 0, 1)}
EL2mat <- EL2mat/ncol(EL2mat)
EL2EL2 <- EL2mat%*%t(EL2mat); dim(EL2EL2); EL2EL2[1:10,1:10]
summary(diag(EL2EL2))

GEL2EL2 <- hadamard.prod(G,EL2EL2); dim(GEL2EL2)
summary(diag(GEL2EL2))
eGEL2EL2 <- eigen(GEL2EL2)
eGEL2EL2[["ids"]] <- dt$ID

save(eGEL2EL2, file="../datasets/eG80EL2EL2_20230922.RData")
write.table(eGEL2EL2$values, "../datasets/eG80EL2EL2_values_20230922.txt", col.names=F, row.names=F, quote=F)

rm(EL2mat, EL2EL2, eGEL2EL2)





### Ee2mat ###

Ee2mat <- as.matrix(dt[paste("e2", Evars, sep='.')]); dim(Ee2mat)
for(i in 1:ncol(Ee2mat)){Ee2mat[,i] <- censcale(Ee2mat[,i], 0, 1)}
Ee2mat <- Ee2mat/ncol(Ee2mat)
Ee2Ee2 <- Ee2mat%*%t(Ee2mat); dim(Ee2Ee2); Ee2Ee2[1:10,1:10]
summary(diag(Ee2Ee2))

GEe2Ee2 <- hadamard.prod(G,Ee2Ee2); dim(GEe2Ee2)
summary(diag(GEe2Ee2))
eGEe2Ee2 <- eigen(GEe2Ee2)
eGEe2Ee2[["ids"]] <- dt$ID

save(eGEe2Ee2, file="../datasets/eG80Ee2Ee2_20230922.RData")
write.table(eGEe2Ee2$values, "../datasets/eG80Ee2Ee2_values_20230922.txt", col.names=F, row.names=F, quote=F)

rm(Ee2mat, Ee2Ee2, eGEe2Ee2); gc()











rm(list=ls()); gc()

































