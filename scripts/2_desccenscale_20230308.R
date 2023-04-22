


rm(list=ls()); gc()

setwd("/data2/morgante_lab/ftiezzi")

library(data.table)

'%!in%' <- function(x,y)!('%in%'(x,y))
censcale <- function(x,A,B){(A+(((x-mean(x)))/sd(x))*B)}


##################################
########### Load data ############
##################################

load("data2_20230318.RData")







##################################
##### Variable classification ####
##################################

Ivars <- c("ID", "Sex_SI", "Sex_gen", "YOB", "MOB", "DOF", "COF", "ethn1", "ethn1_white", "ethn1_whbri", "ethn1_mixed", "ethn1_asian", "ethn1_black", "ethn2", "AOP", "AOR")
Yvars <- c("pulse_rate_automatic_00", "pulse_rate_automatic_01", "diast_blood_00", "diast_blood_01", "sist_blood_00", "sist_blood_01", "pulse_pressure_00", "pulse_pressure_01", "pulse_pressure_mean") 
Emarc <-	c("Townsend", "walk_d", "act0_d", "act1_d", "TVtime", "sleep_d", "smoking_now", "veg_cook", "fish_oily", "fish_lean", "meat_proc", "poultry", "beef", "lamb", "pork", "cheese", "salt", "tea", "alc1", "waist")
Etiez <- c("PCtime", "DRtime", "getup", "coffee", "smoked_past", "BFP", "BMR")
tocenscale <- c(Yvars, Emarc, Etiez)




##################################
##### Descriptive statistics #####
######### Center, scale ##########
##################################


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
     
     
     if(class(dtt[,i])%in%c("numeric", "integer")){
       
       out2[i,1] <- round(length(which(!is.na(dtt[,i]))))
       out2[i,2] <- round(mean(dtt[,i], na.rm=T),2)
       out2[i,3] <- round(sd(dtt[,i], na.rm=T),2)
       out2[i,4] <- round(min(dtt[,i], na.rm=T),2)
       out2[i,5] <- round(max(dtt[,i], na.rm=T),2)
       
     }
     
     
     if(censc[i]==1 & colnames(dtt)[i]%in%Yvars){dttt[,i] <- censcale(dtt[,i], 100, 10)}
     if(censc[i]==1 & colnames(dtt)[i]%!in%Yvars){dttt[,i] <- censcale(dtt[,i], 0, 1)}
     
     if(class(dttt[,i])%in%c("numeric", "integer")){
       
       out3[i,1] <- round(length(which(!is.na(dttt[,i]))))
       out3[i,2] <- round(mean(dttt[,i], na.rm=T),2)
       out3[i,3] <- round(sd(dttt[,i], na.rm=T),2)
       out3[i,4] <- round(min(dttt[,i], na.rm=T),2)
       out3[i,5] <- round(max(dttt[,i], na.rm=T),2)
       
     }
     
     
}

dtvr <- data.frame(Var=colnames(dtt), what=what, out1, out2, censc, out3) #c(1,4,5)

#subset(dtvr, !(min==0 & max==1), select=c(min,max, Var, what, censcale))


write.table(dtvr, "desc_20230318.csv", col.names=T, row.names=F, quote=F, sep=',')


save(dttt, file="data3_20230318.RData")




VV <- length(Yvars); VV
tmp <- dtt[Yvars] 
dim(tmp); head(tmp)

cors <- matrix(NA, VV, VV)
colnames(cors) <- rownames(cors) <- Yvars

for(i in 1:VV){
  for(j in 1:VV){
    cors[i,j] <- cor(tmp[,i],tmp[,j])
  }
}

cors


write.table(cors, "cors_20230318.csv", col.names=T, row.names=T, quote=F, sep=',')

































