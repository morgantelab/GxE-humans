


rm(list=ls()); gc()

setwd("")

library(data.table)

'%!in%' <- function(x,y)!('%in%'(x,y))
censcale <- function(x,A,B){(A+(((x-mean(x)))/sd(x))*B)}


##################################
########### Load data ############
##################################

load("datasets/data2_20240209.RData")
nrow(dtt)






##################################
##### Variable classification ####
##################################

Ivars <- c("ID", "Sex_SI", "Sex_gen", "YOB", "MOB", "DOF", "COF", "ethn1", "ethn1_white", "ethn1_whbri", "ethn1_mixed", "ethn1_asian", "ethn1_black", "ethn2", "AOP", "AOR")
Yvars <- c("PR0", "PR1",   "DP0", "DP1",   "SP0", "SP1",   "PP0", "PP1",   "PPm",    "DP0a", "DP1a",   "SP0a", "SP1a",  "PP0a", "PP1a",   "PPam") 
Emarc <-	c("Townsend", "walk_d", "act0_d", "act1_d", "TVtime", "sleep_d", "smoking_now", "veg_cook", "fish_oily", "fish_lean", "meat_proc", "poultry", "beef", "lamb", "pork", "cheese", "salt", "tea", "alc1", "waist")
Etiez <- c("PCtime", "DRtime", "getup", "coffee", "smoked_past", "BFP", "BMR")
tocenscale <- c(Yvars, Emarc, Etiez)





##################################
##### Descriptive statistics #####
######### Center, scale ##########
##################################




dtt$waist <- replace(dtt$waist, dtt$waist>160, 160)
dtt$BMR <- replace(dtt$BMR, dtt$BMR>14000, 14000)
dtt$TVtime <- replace(dtt$TVtime, dtt$TVtime>12, 12)
dtt$PCtime <- replace(dtt$PCtime, dtt$PCtime>12, 12)
dtt$DRtime <- replace(dtt$DRtime, dtt$DRtime>12, 12)
dtt$sleep_d <- replace(dtt$sleep_d, dtt$sleep_d>13, 13)
dtt$veg_cook <- replace(dtt$veg_cook, dtt$veg_cook>12, 12)
dtt$coffee <- replace(dtt$coffee, dtt$coffee>30, 30)
dtt$tea <- replace(dtt$tea, dtt$tea>30, 30)


dttt <- dtt




V <- ncol(dtt); V

out1 <- matrix(NA, V, 1)
colnames(out1) <- c("class")

out2 <- matrix(NA, V, 5)
colnames(out2) <- c("N", "mean", "sd", "min", "max")

out3 <- matrix(NA, V, 5)
colnames(out3) <- c("N0", "mean0", "sd0", "min0", "max0")

what <- rep("",V)
censc <- rep(0,V)

for(i in 1:V){
  
  out1[i,1] <- class(dtt[,i])[1]
  if(colnames(dtt)[i]%in%Yvars){what[i] <-  "Yvars"}
  if(colnames(dtt)[i]%in%Ivars){what[i] <-  "Ivars"}
  if(colnames(dtt)[i]%in%Emarc){what[i] <-  "Emarc"}
  if(colnames(dtt)[i]%in%Etiez){what[i] <-  "Etiez"}
  if(colnames(dtt)[i]%in%tocenscale){censc[i] <-  1}

  if(censc[i]==1 & colnames(dtt)[i]%in%Yvars){dttt[,i] <- censcale(dtt[,i], 100, 10)}
  if(censc[i]==1 & colnames(dtt)[i]%!in%Yvars){dttt[,i] <- censcale(dtt[,i], 0, 1)}
  
}

dtvr <- data.frame(Var=colnames(dtt), what=what, out1, out2, censc, out3) #c(1,4,5)



write.table(dtvr, "results/desc_20240209.csv", col.names=T, row.names=F, quote=F, sep=',')


dttt$randomvector <- rnorm(nrow(dttt), 100, 10)

save(dttt, file="datasets/data3_20240209.RData")






rm(list=ls()); gc()


























