


# R CMD BATCH "--args 80 0 1 1" 5_Hadamard_20231011.R log5_80_0_1_1.txt
# R CMD BATCH "--args 99 0 1 1" 5_Hadamard_20231011.R log5_99_0_1_1.txt
# R CMD BATCH "--args 99 0 0 0 1" 5_Hadamard_20231229.R log5_99_0_0_0_1.txt





rm(list=ls()); gc()

library(matrixcalc)

'%!in%' <- function(x,y)!('%in%'(x,y))
censcale <- function(x,A,B){(A+(((x-mean(x)))/sd(x))*B)}

args <- as.numeric(commandArgs(TRUE))
#args <- c(99, 1, 1, 1)
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



### C ###

TMPmat <- dt[c("Sex_SI", "AOP")]
TMPmat$isMale <- TMPmat$Sex_SI
TMPmat$isFemale <- -1*(TMPmat$Sex_SI-1)
TMPmat$AOP1 <- censcale(TMPmat$AOP, 0, 1)
TMPmat$AOP2 <- censcale(TMPmat$AOP, 0, 1)^2
TMPmat$AOP1_Male <- TMPmat$AOP1*TMPmat$isMale
TMPmat$AOP1_Female <- TMPmat$AOP1*TMPmat$isFemale
TMPmat$AOP2_Male <- TMPmat$AOP2*TMPmat$isMale
TMPmat$AOP2_Female <- TMPmat$AOP2*TMPmat$isFemale
head(TMPmat)


Cmat <- as.matrix(TMPmat[c("isMale", "AOP1", "AOP2", "AOP1_Male", "AOP2_Male", "AOP1_Female", "AOP2_Female")]); dim(Cmat); head(Cmat)
rm(TMPmat)


#for(i in 1:ncol(Cmat)){Cmat[,i] <- censcale(Cmat[,i], 0, 1)}
Cmat <- Cmat/ncol(Cmat); Cmat[1:10,]
CC <- Cmat%*%t(Cmat); dim(CC); CC[1:10,]
summary(diag(CC))

wei <- sum(diag(G))/sum(diag(CC)); wei
CC <- CC*wei
summary(diag(CC))
CC[1:10,1:10]
dCC <- diag(CC)
eCC <- eigen(CC)
eCC[["values"]] <- eCC[["values"]][1:5]
eCC[["vectors"]] <- eCC[["vectors"]][,1:5]
eCC[["ids"]] <- dt$ID

save(dCC, file=paste("../datasets/d", args[1], "CC_20240409.RData", sep=''))
save(eCC, file=paste("../datasets/e", args[1], "CC_20240409.RData", sep=''))
write.table(eCC$values, paste("../datasets/e", args[1], "CC_values_20240409.txt", sep=''), col.names=F, row.names=F, quote=F)


### GxC ###

GCC <- hadamard.prod(G,CC); dim(GCC); GCC[1:10,1:10]
summary(diag(GCC))
dGCC <- diag(GCC)
eGCC <- eigen(GCC)
eGCC[["ids"]] <- dt$ID

save(dGCC, file=paste("../datasets/dG", args[1], "CC_20240409.RData", sep=''))
save(eGCC, file=paste("../datasets/eG", args[1], "CC_20240409.RData", sep=''))
write.table(eGCC$values, paste("../datasets/eG", args[1], "CC_values_20240409.txt", sep=''), col.names=F, row.names=F, quote=F)

rm(GCC, dGCC, eGCC)



rm(list=ls()); gc()
















