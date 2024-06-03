




rm(list=ls()); gc()


#library(gtools)
#library(gdata)
#library(reshape)
library(BGLR)
library(ggplot2)
library(grid)
library(TeachingDemos)

'%!in%' <- function(x,y)!('%in%'(x,y))
hpd1 <- function(X){x1 <- emp.hpd(X, conf=0.95)[1]; return(x1)}
hpd2 <- function(X){x2 <- emp.hpd(X, conf=0.95)[2]; return(x2)}


setwd("/data2/morgante_lab/ukbiobank_projects/GxE/run")



cohorts <- c("0_1", "0_2", "0_3", "0_4", "1_1", "1_2", "1_3", "1_4")
cohorts <- c("coh01", "coh02", "coh03", "coh04", "coh11", "coh12", "coh13", "coh14")
Yvars <- c("PR0", "PR1",   "DP0", "DP1",   "SP0", "SP1",   "PP0", "PP1",   "PPm",    "DP0a", "DP1a",   "SP0a", "SP1a",  "PP0a", "PP1a",   "PPam") 

Emarc <-	c("Townsend", "walk_d", "act0_d", "act1_d", "TVtime", "sleep_d", "smoking_now", "veg_cook", "fish_oily", "fish_lean", "meat_proc", "poultry", "beef", "lamb", "pork", "cheese", "salt", "tea", "alc1", "waist")
Etiez <- c("PCtime", "DRtime", "getup", "coffee", "smoked_past", "BFP", "BMR")
Evars <- c(Emarc, Etiez); length(Evars)
Yvars[101:127] <- Evars


where <- "run2"





tr=101
for(tr in c(10, 12, 14,   101, 102, 103, 104, 105, 106, 107, 119, 120, 121, 122, 123, 125, 126, 127,   108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 124)){
  
  args <- c(tr,2,1)
  
  
  zz0 <- read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_mu.dat', sep=''), header=F); colnames(zz0) <- "int"
  B1 <- data.frame(zero=0,read.table(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_X_b.dat', sep=''), header=T))
  colnames(B1)[1] <- cohorts[which(cohorts%!in%gsub("X","",colnames(B1)))]
  B1 <- data.frame(zz0,B1)
  B3 <- readBinMat(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_E_b.bin', sep=''))
  
  P1 <- ncol(B1); P1
  P3 <- ncol(B3); P3
  
  
  png(filename=paste(where, "_traceplots/BETAtraceplot1_", Yvars[args[1]], '_model', args[2], '_subset', args[3], ".png", sep=""), height=800*P1, width=4000,  bg="white")
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(P1, 1)))
  
  for(i in 1:P1){
    
    tmp <- data.frame(iter=1:nrow(B1), beta=B1[,i])
    
    print(
      
      ggplot(tmp, aes(x = iter, y = beta)) + 
        geom_point(size=8) + 
        labs(title = colnames(B1)[i], x="Iter", y = "Value\n") + 
        
        theme(title = element_text(face = "bold", color = "black"), axis.title = element_text(face = "bold", color = "black")) +
        guides(colour = guide_legend(override.aes = list(linetype = 1)))+
        
        theme(axis.text.x = element_text(colour="black", size=20, angle=0, hjust=.5, vjust=0, face="bold")) +
        theme(axis.text.y = element_text(colour="black", size=20, angle=0, hjust=1, vjust=0, face="plain")) +
        
        theme(panel.background = element_rect(fill='gray95', colour='black')) +
        theme(plot.background = element_rect(fill='white', colour='black')) +
        theme(panel.grid.major = element_line(colour = "white")) +
        
        theme(title = element_text(face = "bold", color = "black"), axis.title = element_text(face = "bold", color = "black")) +
        theme(axis.title.x=element_text(angle=0, vjust=-5, hjust=0.495, size=25)) +
        theme(axis.title.y=element_text(angle=90, hjust=0.52, vjust=0, size=25)) +
        
        theme(plot.margin = unit(c(2,0.75,2,1), "cm"))+
        theme(plot.title=element_text(size=30, vjust=6, hjust=0.52))+
        theme(legend.position="none")+
        
        guides(fill = guide_legend(reverse = FALSE))
      
      , vp = viewport(layout.pos.row = i, layout.pos.col = 1))
    
    rm(tmp)
    
  }
  
  dev.off()
  
  
  
  
  
  
  png(filename=paste(where, "_traceplots/BETAtraceplot3_", Yvars[args[1]], '_model', args[2], '_subset', args[3], ".png", sep=""), height=800*P3, width=4000,  bg="white")
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(P3, 1)))
  
  for(i in 1:P3){
    
    tmp <- data.frame(iter=1:nrow(B3), beta=B3[,i])
    
    print(
      
      ggplot(tmp, aes(x = iter, y = beta)) + 
        geom_point(size=8) + 
        labs(title = colnames(B3)[i], x="Iter", y = "Value\n") + 
        
        theme(title = element_text(face = "bold", color = "black"), axis.title = element_text(face = "bold", color = "black")) +
        guides(colour = guide_legend(override.aes = list(linetype = 1)))+
        
        theme(axis.text.x = element_text(colour="black", size=20, angle=0, hjust=.5, vjust=0, face="bold")) +
        theme(axis.text.y = element_text(colour="black", size=20, angle=0, hjust=1, vjust=0, face="plain")) +
        
        theme(panel.background = element_rect(fill='gray95', colour='black')) +
        theme(plot.background = element_rect(fill='white', colour='black')) +
        theme(panel.grid.major = element_line(colour = "white")) +
        
        theme(title = element_text(face = "bold", color = "black"), axis.title = element_text(face = "bold", color = "black")) +
        theme(axis.title.x=element_text(angle=0, vjust=-5, hjust=0.495, size=25)) +
        theme(axis.title.y=element_text(angle=90, hjust=0.52, vjust=0, size=25)) +
        
        theme(plot.margin = unit(c(2,0.75,2,1), "cm"))+
        theme(plot.title=element_text(size=30, vjust=6, hjust=0.52))+
        theme(legend.position="none")+
        
        guides(fill = guide_legend(reverse = FALSE))
      
      , vp = viewport(layout.pos.row = i, layout.pos.col = 1))
    
    rm(tmp)
    
  }
  
  dev.off()
  
  
  rm(args, zz0, B1, B3, P1, P3)
  
}






burnin <- 1000
thin <- 5




tr=101
#for(tr in c(10, 12, 14,   101, 102, 103, 104, 105, 106, 107, 119, 120, 121, 122, 123, 125, 126, 127,   108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 124)){

  args <- c(tr,2,1)

  B3 <- readBinMat(paste(where, '_output/run_', args[1], '_', args[2], '_', args[3], '_ETA_E_b.bin', sep=''))

nrow(B3)


















res <- matrix(NA, 127,  5)
Res <- matrix("", 127,  5)
colnames(res) <- colnames(Res) <- c("G", "E", "D", "e", "P")
model <- 1
subset <- 1

for(tr in c(10, 12, 14,   101, 102, 103, 104, 105, 106, 107, 119, 120, 121, 122, 123, 125, 126, 127,   108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 124)){
  
  load(paste(paste(where, '_results/VCE_', tr, '_', model, '_', subset, '.RData', sep='')))
  
  VCE <- VCE[seq(burnin,nrow(VCE), thin),]
  VCE$Vphen <- VCE$Vgen+VCE$Venv+VCE$Vdes+VCE$res
  
  if(tr>100){VCE <- VCE*100}
  
  Res[tr, 1] <- paste(round(mean(VCE$Vgen),2), " (", round(hpd1(VCE$Vgen),2), ", ", round(hpd2(VCE$Vgen),2), ")", sep='')
  Res[tr, 2] <- paste(round(mean(VCE$Venv),2), " (", round(hpd1(VCE$Venv),2), ", ", round(hpd2(VCE$Venv),2), ")", sep='')
  Res[tr, 3] <- paste(round(mean(VCE$Vdes),2), " (", round(hpd1(VCE$Vdes),2), ", ", round(hpd2(VCE$Vdes),2), ")", sep='')
  Res[tr, 4] <-  paste(round(mean(VCE$res),2), " (", round(hpd1(VCE$res),2),  ", ",  round(hpd2(VCE$res),2), ")", sep='')
  Res[tr, 5] <-  paste(round(mean(VCE$Vphen),2), " (", round(hpd1(VCE$Vphen),2),  ", ",  round(hpd2(VCE$Vphen),2), ")", sep='')
  
  res[tr, 1] <- mean(VCE$Vgen)
  res[tr, 2] <- mean(VCE$Venv)
  res[tr, 3] <- mean(VCE$Vdes)
  res[tr, 4] <- mean(VCE$res)
  res[tr, 5] <- mean(VCE$Vphen)
  
  rm(VCE)
  
}

Res <- data.frame(Yvars,Res)
res <- data.frame(Yvars,res)


Res <- Res[c(10,12,14,101:127),]
res <- res[c(10,12,14,101:127),]


res
Res



write.table(Res, "ResultsAmodel1_run2.csv", col.names=T, row.names=F, quote=F, sep=';')
write.table(res, "resultsBmodel1_run2.csv", col.names=T, row.names=F, quote=F, sep=';')




rm(Res, res, model, subset)








#tr=101
for(tr in c(10, 12, 14,      101, 102, 103, 104, 105, 106, 107, 119, 120, 121, 122, 123, 125, 126, 127,   108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 124)){
  
  args <- c(tr,2,1)
  
  load(paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  
  VCE <- VCE[seq(burnin,nrow(VCE), thin),]
  
  P <- ncol(VCE); P
  
  
  png(filename=paste(where, "_traceplots/VCEtraceplot_", Yvars[args[1]], '_model', args[2], '_subset', args[3], ".png", sep=""), height=800*P, width=4000,  bg="white")
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(P, 1)))
  
  for(i in 1:P){
    
    tmp <- data.frame(iter=1:nrow(VCE), vc=VCE[,i])
    
    print(
      
      ggplot(tmp, aes(x = iter, y = vc)) + 
        geom_point(size=8) + 
        labs(title = colnames(VCE)[i], x="Iter", y = "Value\n") + 
        
        theme(title = element_text(face = "bold", color = "black"), axis.title = element_text(face = "bold", color = "black")) +
        guides(colour = guide_legend(override.aes = list(linetype = 1)))+
        
        theme(axis.text.x = element_text(colour="black", size=20, angle=0, hjust=.5, vjust=0, face="bold")) +
        theme(axis.text.y = element_text(colour="black", size=20, angle=0, hjust=1, vjust=0, face="plain")) +
        
        theme(panel.background = element_rect(fill='gray95', colour='black')) +
        theme(plot.background = element_rect(fill='white', colour='black')) +
        theme(panel.grid.major = element_line(colour = "white")) +
        
        theme(title = element_text(face = "bold", color = "black"), axis.title = element_text(face = "bold", color = "black")) +
        theme(axis.title.x=element_text(angle=0, vjust=-5, hjust=0.495, size=25)) +
        theme(axis.title.y=element_text(angle=90, hjust=0.52, vjust=0, size=25)) +
        
        theme(plot.margin = unit(c(2,0.75,2,1), "cm"))+
        theme(plot.title=element_text(size=30, vjust=6, hjust=0.52))+
        theme(legend.position="none")+
        
        guides(fill = guide_legend(reverse = FALSE))
      
      , vp = viewport(layout.pos.row = i, layout.pos.col = 1))
    
    rm(tmp)
    
  }
  
  dev.off()
  
  rm(args, VCE, P)
  
}




res <- matrix(NA, 127,  5)
Res <- matrix("", 127,  5)
colnames(res) <- colnames(Res) <- c("G", "E", "D", "e", "P")
model <- 2
subset <- 1

for(tr in c(10, 12, 14,      101, 102, 103, 104, 105, 106, 107, 119, 120, 121, 122, 123, 125, 126, 127,   108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 124)){
  
  load(paste(paste(where, '_results/VCE_', tr, '_', model, '_', subset, '.RData', sep='')))
  
  VCE <- VCE[seq(burnin,nrow(VCE), thin),]
  VCE$Vphen <- VCE$Vgen+VCE$Venv+VCE$Vdes+VCE$res
  
  if(tr>100){VCE <- VCE*100}
  
  Res[tr, 1] <- paste(round(mean(VCE$Vgen),2), " (", round(hpd1(VCE$Vgen),2), ", ", round(hpd2(VCE$Vgen),2), ")", sep='')
  Res[tr, 2] <- paste(round(mean(VCE$Venv),2), " (", round(hpd1(VCE$Venv),2), ", ", round(hpd2(VCE$Venv),2), ")", sep='')
  Res[tr, 3] <- paste(round(mean(VCE$Vdes),2), " (", round(hpd1(VCE$Vdes),2), ", ", round(hpd2(VCE$Vdes),2), ")", sep='')
  Res[tr, 4] <-  paste(round(mean(VCE$res),2), " (", round(hpd1(VCE$res),2),  ", ",  round(hpd2(VCE$res),2), ")", sep='')
  Res[tr, 5] <-  paste(round(mean(VCE$Vphen),2), " (", round(hpd1(VCE$Vphen),2),  ", ",  round(hpd2(VCE$Vphen),2), ")", sep='')
  
  res[tr, 1] <- mean(VCE$Vgen)
  res[tr, 2] <- mean(VCE$Venv)
  res[tr, 3] <- mean(VCE$Vdes)
  res[tr, 4] <- mean(VCE$res)
  res[tr, 5] <- mean(VCE$Vphen)
  
  rm(VCE)
  
}

Res <- data.frame(Yvars,Res)
res <- data.frame(Yvars,res)


Res <- Res[c(10,12,14,101:127),]
res <- res[c(10,12,14,101:127),]


res
Res




write.table(Res, "ResultsAmodel2_run2.csv", col.names=T, row.names=F, quote=F, sep=';')
write.table(res, "resultsBmodel2_run2.csv", col.names=T, row.names=F, quote=F, sep=';')



















#tr=10
for(tr in c(10, 12, 14)){
  
  args <- c(tr,3,1)
  
  load(paste(paste(where, '_results/VCE_', args[1], '_', args[2], '_', args[3], '.RData', sep='')))
  
  
  VCE <- VCE[seq(burnin,nrow(VCE), thin),]
  
  P <- ncol(VCE); P
  
  
  png(filename=paste(where, "_traceplots/VCEtraceplot_", Yvars[args[1]], '_model', args[2], '_subset', args[3], ".png", sep=""), height=800*P, width=4000,  bg="white")
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(P, 1)))
  
  for(i in 1:P){
    
    tmp <- data.frame(iter=1:nrow(VCE), vc=VCE[,i])
    
    print(
      
      ggplot(tmp, aes(x = iter, y = vc)) + 
        geom_point(size=8) + 
        labs(title = colnames(VCE)[i], x="Iter", y = "Value\n") + 
        
        theme(title = element_text(face = "bold", color = "black"), axis.title = element_text(face = "bold", color = "black")) +
        guides(colour = guide_legend(override.aes = list(linetype = 1)))+
        
        theme(axis.text.x = element_text(colour="black", size=20, angle=0, hjust=.5, vjust=0, face="bold")) +
        theme(axis.text.y = element_text(colour="black", size=20, angle=0, hjust=1, vjust=0, face="plain")) +
        
        theme(panel.background = element_rect(fill='gray95', colour='black')) +
        theme(plot.background = element_rect(fill='white', colour='black')) +
        theme(panel.grid.major = element_line(colour = "white")) +
        
        theme(title = element_text(face = "bold", color = "black"), axis.title = element_text(face = "bold", color = "black")) +
        theme(axis.title.x=element_text(angle=0, vjust=-5, hjust=0.495, size=25)) +
        theme(axis.title.y=element_text(angle=90, hjust=0.52, vjust=0, size=25)) +
        
        theme(plot.margin = unit(c(2,0.75,2,1), "cm"))+
        theme(plot.title=element_text(size=30, vjust=6, hjust=0.52))+
        theme(legend.position="none")+
        
        guides(fill = guide_legend(reverse = FALSE))
      
      , vp = viewport(layout.pos.row = i, layout.pos.col = 1))
    
    rm(tmp)
    
  }
  
  dev.off()
  
  rm(args, VCE, P)
  
}




res <- matrix(NA, 127,  5)
Res <- matrix("", 127,  5)
colnames(res) <- colnames(Res) <- c("G", "E", "D", "e", "P")
model <- 3
subset <- 1

for(tr in c(10, 12, 14)){
  
  load(paste(paste(where, '_results/VCE_', tr, '_', model, '_', subset, '.RData', sep='')))
  
  VCE <- VCE[seq(burnin,nrow(VCE), thin),]
  VCE$Vphen <- VCE$Vgen+VCE$Venv+VCE$Vdes+VCE$res
  
  if(tr>100){VCE <- VCE*100}
  
  Res[tr, 1] <- paste(round(mean(VCE$Vgen),2), " (", round(hpd1(VCE$Vgen),2), ", ", round(hpd2(VCE$Vgen),2), ")", sep='')
  Res[tr, 2] <- paste(round(mean(VCE$Venv),2), " (", round(hpd1(VCE$Venv),2), ", ", round(hpd2(VCE$Venv),2), ")", sep='')
  Res[tr, 3] <- paste(round(mean(VCE$Vdes),2), " (", round(hpd1(VCE$Vdes),2), ", ", round(hpd2(VCE$Vdes),2), ")", sep='')
  Res[tr, 4] <-  paste(round(mean(VCE$res),2), " (", round(hpd1(VCE$res),2),  ", ",  round(hpd2(VCE$res),2), ")", sep='')
  Res[tr, 5] <-  paste(round(mean(VCE$Vphen),2), " (", round(hpd1(VCE$Vphen),2),  ", ",  round(hpd2(VCE$Vphen),2), ")", sep='')
  
  res[tr, 1] <- mean(VCE$Vgen)
  res[tr, 2] <- mean(VCE$Venv)
  res[tr, 3] <- mean(VCE$Vdes)
  res[tr, 4] <- mean(VCE$res)
  res[tr, 5] <- mean(VCE$Vphen)
  
  rm(VCE)
  
}

Res <- data.frame(Yvars,Res)
res <- data.frame(Yvars,res)


Res <- Res[c(10,12,14),]
res <- res[c(10,12,14),]


res
Res



write.table(Res, "ResultsAmodel3_run2.csv", col.names=T, row.names=F, quote=F, sep=';')
write.table(res, "resultsBmodel3_run2.csv", col.names=T, row.names=F, quote=F, sep=';')




rm(Res, res, model, subset)









