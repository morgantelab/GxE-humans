




rm(list=ls()); gc()


library(gtools)
#library(gdata)
#library(reshape)
library(ggplot2)
library(grid)
library(TeachingDemos)

'%!in%' <- function(x,y)!('%in%'(x,y))
hpd1 <- function(X){x1 <- emp.hpd(X, conf=0.95)[1]; return(x1)}
hpd2 <- function(X){x2 <- emp.hpd(X, conf=0.95)[2]; return(x2)}


setwd("/data2/morgante_lab/ukbiobank_projects/GxE/run")
#setwd("/Users/francescotiezzi/ARCHIVI/GxEhumans")



cohorts <- c("0_1", "0_2", "0_3", "0_4", "1_1", "1_2", "1_3", "1_4")
Yvars <- c("PR0", "PR1",   "DP0", "DP1",   "SP0", "SP1",   "PP0", "PP1",   "PPm",    "DP0a", "DP1a",   "SP0a", "SP1a",  "PP0a", "PP1a",   "PPam") 

Emarc <-	c("Townsend", "walk_d", "act0_d", "act1_d", "TVtime", "sleep_d", "smoking_now", "veg_cook", "fish_oily", "fish_lean", "meat_proc", "poultry", "beef", "lamb", "pork", "cheese", "salt", "tea", "alc1", "waist")
Etiez <- c("PCtime", "DRtime", "getup", "coffee", "smoked_past", "BFP", "BMR")
Evars <- c(Emarc, Etiez); length(Evars)
Yvars[101:127] <- Evars



where <- "keep"



tr=10
mod=7


ROW <- COL <- matrix(NA,127,7)


for(tr in c(10,12,14)){
	for(mod in c(1:7)){
		load(paste(paste(where, '_results/VCE_', tr, '_', mod, '_', 1, '.RData', sep='')))
#		COL[tr,mod] <- ncol(VCE)
#		ROW[tr,mod] <- nrow(VCE)
		rm(VCE)	
	}
}






res <- matrix(NA, 127,  5)
Res <- matrix("", 127,  5)
colnames(res) <- colnames(Res) <- c("G", "E", "D", "e", "P")

mod=1
Res1 <- Res
res1 <- res
for(tr in c(10, 12, 14,    101, 102, 103, 104, 105, 106, 107, 119, 120, 121, 122, 123, 125, 126, 127,   108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 124)){

	load(paste(paste(where, '_results/VCE_', tr, '_', mod, '_', 1, '.RData', sep='')))

	VCE <- VCE[seq(   1,nrow(VCE), 5),]

	VCE$Vphen <- VCE$Vgen+VCE$Venv+VCE$Vdes+VCE$res

	if(tr>100){VCE <- VCE*100}
	
 	Res1[tr, 1] <- paste(round(mean(VCE$Vgen),2), " (", round(hpd1(VCE$Vgen),2), ", ", round(hpd2(VCE$Vgen),2), ")", sep='')
 	Res1[tr, 2] <- paste(round(mean(VCE$Venv),2), " (", round(hpd1(VCE$Venv),2), ", ", round(hpd2(VCE$Venv),2), ")", sep='')
 	Res1[tr, 3] <- paste(round(mean(VCE$Vdes),2), " (", round(hpd1(VCE$Vdes),2), ", ", round(hpd2(VCE$Vdes),2), ")", sep='')
 	Res1[tr, 4] <-  paste(round(mean(VCE$res),2), " (", round(hpd1(VCE$res),2),  ", ",  round(hpd2(VCE$res),2), ")", sep='')
 	Res1[tr, 5] <-  paste(round(mean(VCE$Vphen),2), " (", round(hpd1(VCE$Vphen),2),  ", ",  round(hpd2(VCE$Vphen),2), ")", sep='')
  
 	res1[tr, 1] <- mean(VCE$Vgen)
	res1[tr, 2] <- mean(VCE$Venv)
 	res1[tr, 3] <- mean(VCE$Vdes)
  	res1[tr, 4] <- mean(VCE$res)
	res1[tr, 5] <- mean(VCE$Vphen)

	ROW[tr,mod] <- nrow(VCE)

	P <- ncol(VCE); P
 
	png(filename=paste(where, "_traceplots/VCEtraceplot_", tr, '_model', mod, '_subset', 1, ".png", sep=""), height=800*P, width=4000,  bg="white")

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

	rm(VCE)

}

Res1 <- data.frame(trait=Yvars, model=1, Res1)
res1 <- data.frame(trait=Yvars, model=1, res1)






mod=2
Res2 <- Res
res2 <- res
for(tr in c(10, 12, 14,    101, 102, 103, 104, 105, 106, 107, 119, 120, 121, 122, 123, 125, 126, 127,   108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 124)){

	load(paste(paste(where, '_results/VCE_', tr, '_', mod, '_', 1, '.RData', sep='')))

	VCE <- VCE[seq(   1,nrow(VCE), 5),]

	VCE$Vphen <- VCE$Vgen+VCE$Venv+VCE$Vdes+VCE$res

	if(tr>100){VCE <- VCE*100}
	
 	Res2[tr, 1] <- paste(round(mean(VCE$Vgen),2), " (", round(hpd1(VCE$Vgen),2), ", ", round(hpd2(VCE$Vgen),2), ")", sep='')
 	Res2[tr, 2] <- paste(round(mean(VCE$Venv),2), " (", round(hpd1(VCE$Venv),2), ", ", round(hpd2(VCE$Venv),2), ")", sep='')
 	Res2[tr, 3] <- paste(round(mean(VCE$Vdes),2), " (", round(hpd1(VCE$Vdes),2), ", ", round(hpd2(VCE$Vdes),2), ")", sep='')
 	Res2[tr, 4] <-  paste(round(mean(VCE$res),2), " (", round(hpd1(VCE$res),2),  ", ",  round(hpd2(VCE$res),2), ")", sep='')
 	Res2[tr, 5] <-  paste(round(mean(VCE$Vphen),2), " (", round(hpd1(VCE$Vphen),2),  ", ",  round(hpd2(VCE$Vphen),2), ")", sep='')
  
 	res2[tr, 1] <- mean(VCE$Vgen)
	res2[tr, 2] <- mean(VCE$Venv)
 	res2[tr, 3] <- mean(VCE$Vdes)
  	res2[tr, 4] <- mean(VCE$res)
	res2[tr, 5] <- mean(VCE$Vphen)

	ROW[tr,mod] <- nrow(VCE)

	P <- ncol(VCE); P
 
	png(filename=paste(where, "_traceplots/VCEtraceplot_", tr, '_model', mod, '_subset', 1, ".png", sep=""), height=800*P, width=4000,  bg="white")

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

	rm(VCE)

}

Res2 <- data.frame(trait=Yvars, model=2, Res2)
res2 <- data.frame(trait=Yvars, model=2, res2)






mod=3
Res3 <- Res
res3 <- res
for(tr in c(10, 12, 14)){

	load(paste(paste(where, '_results/VCE_', tr, '_', mod, '_', 1, '.RData', sep='')))

	VCE <- VCE[seq(1001,nrow(VCE), 5),]

	VCE$Vphen <- VCE$Vgen+VCE$Venv+VCE$Vdes+VCE$res

	if(tr>100){VCE <- VCE*100}
	
 	Res3[tr, 1] <- paste(round(mean(VCE$Vgen),2), " (", round(hpd1(VCE$Vgen),2), ", ", round(hpd2(VCE$Vgen),2), ")", sep='')
 	Res3[tr, 2] <- paste(round(mean(VCE$Venv),2), " (", round(hpd1(VCE$Venv),2), ", ", round(hpd2(VCE$Venv),2), ")", sep='')
 	Res3[tr, 3] <- paste(round(mean(VCE$Vdes),2), " (", round(hpd1(VCE$Vdes),2), ", ", round(hpd2(VCE$Vdes),2), ")", sep='')
 	Res3[tr, 4] <-  paste(round(mean(VCE$res),2), " (", round(hpd1(VCE$res),2),  ", ",  round(hpd2(VCE$res),2), ")", sep='')
 	Res3[tr, 5] <-  paste(round(mean(VCE$Vphen),2), " (", round(hpd1(VCE$Vphen),2),  ", ",  round(hpd2(VCE$Vphen),2), ")", sep='')
  
 	res3[tr, 1] <- mean(VCE$Vgen)
	res3[tr, 2] <- mean(VCE$Venv)
 	res3[tr, 3] <- mean(VCE$Vdes)
  	res3[tr, 4] <- mean(VCE$res)
	res3[tr, 5] <- mean(VCE$Vphen)

	ROW[tr,mod] <- nrow(VCE)

	P <- ncol(VCE); P
 
	png(filename=paste(where, "_traceplots/VCEtraceplot_", tr, '_model', mod, '_subset', 1, ".png", sep=""), height=800*P, width=4000,  bg="white")

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

	rm(VCE)

}

Res3 <- data.frame(trait=Yvars, model=3, Res3)
res3 <- data.frame(trait=Yvars, model=3, res3)






mod=4
Res4 <- Res
res4 <- res
for(tr in c(10, 12, 14)){

	load(paste(paste(where, '_results/VCE_', tr, '_', mod, '_', 1, '.RData', sep='')))

	VCE <- VCE[seq( 201,nrow(VCE), 1),]

	VCE$Vphen <- VCE$Vgen+VCE$Venv+VCE$Vdes+VCE$res

	if(tr>100){VCE <- VCE*100}
	
 	Res4[tr, 1] <- paste(round(mean(VCE$Vgen),2), " (", round(hpd1(VCE$Vgen),2), ", ", round(hpd2(VCE$Vgen),2), ")", sep='')
 	Res4[tr, 2] <- paste(round(mean(VCE$Venv),2), " (", round(hpd1(VCE$Venv),2), ", ", round(hpd2(VCE$Venv),2), ")", sep='')
 	Res4[tr, 3] <- paste(round(mean(VCE$Vdes),2), " (", round(hpd1(VCE$Vdes),2), ", ", round(hpd2(VCE$Vdes),2), ")", sep='')
 	Res4[tr, 4] <-  paste(round(mean(VCE$res),2), " (", round(hpd1(VCE$res),2),  ", ",  round(hpd2(VCE$res),2), ")", sep='')
 	Res4[tr, 5] <-  paste(round(mean(VCE$Vphen),2), " (", round(hpd1(VCE$Vphen),2),  ", ",  round(hpd2(VCE$Vphen),2), ")", sep='')
  
 	res4[tr, 1] <- mean(VCE$Vgen)
	res4[tr, 2] <- mean(VCE$Venv)
 	res4[tr, 3] <- mean(VCE$Vdes)
  	res4[tr, 4] <- mean(VCE$res)
	res4[tr, 5] <- mean(VCE$Vphen)

	ROW[tr,mod] <- nrow(VCE)

	P <- ncol(VCE); P
 
	png(filename=paste(where, "_traceplots/VCEtraceplot_", tr, '_model', mod, '_subset', 1, ".png", sep=""), height=800*P, width=4000,  bg="white")

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

	rm(VCE)

}

Res4 <- data.frame(trait=Yvars, model=4, Res4)
res4 <- data.frame(trait=Yvars, model=4, res4)






mod=5
Res5 <- Res
res5 <- res
for(tr in c(10, 12, 14)){

	load(paste(paste(where, '_results/VCE_', tr, '_', mod, '_', 1, '.RData', sep='')))

	VCE <- VCE[seq( 201,nrow(VCE), 1),]

	VCE$Vphen <- VCE$Vgen+VCE$Venv+VCE$Vdes+VCE$res

	if(tr>100){VCE <- VCE*100}
	
 	Res5[tr, 1] <- paste(round(mean(VCE$Vgen),2), " (", round(hpd1(VCE$Vgen),2), ", ", round(hpd2(VCE$Vgen),2), ")", sep='')
 	Res5[tr, 2] <- paste(round(mean(VCE$Venv),2), " (", round(hpd1(VCE$Venv),2), ", ", round(hpd2(VCE$Venv),2), ")", sep='')
 	Res5[tr, 3] <- paste(round(mean(VCE$Vdes),2), " (", round(hpd1(VCE$Vdes),2), ", ", round(hpd2(VCE$Vdes),2), ")", sep='')
 	Res5[tr, 4] <-  paste(round(mean(VCE$res),2), " (", round(hpd1(VCE$res),2),  ", ",  round(hpd2(VCE$res),2), ")", sep='')
 	Res5[tr, 5] <-  paste(round(mean(VCE$Vphen),2), " (", round(hpd1(VCE$Vphen),2),  ", ",  round(hpd2(VCE$Vphen),2), ")", sep='')
  
 	res5[tr, 1] <- mean(VCE$Vgen)
	res5[tr, 2] <- mean(VCE$Venv)
 	res5[tr, 3] <- mean(VCE$Vdes)
  	res5[tr, 4] <- mean(VCE$res)
	res5[tr, 5] <- mean(VCE$Vphen)

	ROW[tr,mod] <- nrow(VCE)

	P <- ncol(VCE); P
 
	png(filename=paste(where, "_traceplots/VCEtraceplot_", tr, '_model', mod, '_subset', 1, ".png", sep=""), height=800*P, width=4000,  bg="white")

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

	rm(VCE)

}

Res5 <- data.frame(trait=Yvars, model=5, Res5)
res5 <- data.frame(trait=Yvars, model=5, res5)






mod=6
Res6 <- Res
res6 <- res
for(tr in c(10, 12, 14)){

	load(paste(paste(where, '_results/VCE_', tr, '_', mod, '_', 1, '.RData', sep='')))

	VCE <- VCE[seq( 201,nrow(VCE), 1),]

	VCE$Vphen <- VCE$Vgen+VCE$Venv+VCE$Vdes+VCE$res

	if(tr>100){VCE <- VCE*100}
	
 	Res6[tr, 1] <- paste(round(mean(VCE$Vgen),2), " (", round(hpd1(VCE$Vgen),2), ", ", round(hpd2(VCE$Vgen),2), ")", sep='')
 	Res6[tr, 2] <- paste(round(mean(VCE$Venv),2), " (", round(hpd1(VCE$Venv),2), ", ", round(hpd2(VCE$Venv),2), ")", sep='')
 	Res6[tr, 3] <- paste(round(mean(VCE$Vdes),2), " (", round(hpd1(VCE$Vdes),2), ", ", round(hpd2(VCE$Vdes),2), ")", sep='')
 	Res6[tr, 4] <-  paste(round(mean(VCE$res),2), " (", round(hpd1(VCE$res),2),  ", ",  round(hpd2(VCE$res),2), ")", sep='')
 	Res6[tr, 5] <-  paste(round(mean(VCE$Vphen),2), " (", round(hpd1(VCE$Vphen),2),  ", ",  round(hpd2(VCE$Vphen),2), ")", sep='')
  
 	res6[tr, 1] <- mean(VCE$Vgen)
	res6[tr, 2] <- mean(VCE$Venv)
 	res6[tr, 3] <- mean(VCE$Vdes)
  	res6[tr, 4] <- mean(VCE$res)
	res6[tr, 5] <- mean(VCE$Vphen)

	ROW[tr,mod] <- nrow(VCE)

	P <- ncol(VCE); P
 
	png(filename=paste(where, "_traceplots/VCEtraceplot_", tr, '_model', mod, '_subset', 1, ".png", sep=""), height=800*P, width=4000,  bg="white")

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

	rm(VCE)

}

Res6 <- data.frame(trait=Yvars, model=6, Res6)
res6 <- data.frame(trait=Yvars, model=6, res6)






mod=7
Res7 <- Res
res7 <- res
for(tr in c(10, 12, 14)){

	load(paste(paste(where, '_results/VCE_', tr, '_', mod, '_', 1, '.RData', sep='')))

	VCE <- VCE[seq( 201,nrow(VCE), 1),]

	VCE$Vphen <- VCE$Vgen+VCE$Venv+VCE$Vdes+VCE$res

	if(tr>100){VCE <- VCE*100}
	
 	Res7[tr, 1] <- paste(round(mean(VCE$Vgen),2), " (", round(hpd1(VCE$Vgen),2), ", ", round(hpd2(VCE$Vgen),2), ")", sep='')
 	Res7[tr, 2] <- paste(round(mean(VCE$Venv),2), " (", round(hpd1(VCE$Venv),2), ", ", round(hpd2(VCE$Venv),2), ")", sep='')
 	Res7[tr, 3] <- paste(round(mean(VCE$Vdes),2), " (", round(hpd1(VCE$Vdes),2), ", ", round(hpd2(VCE$Vdes),2), ")", sep='')
 	Res7[tr, 4] <-  paste(round(mean(VCE$res),2), " (", round(hpd1(VCE$res),2),  ", ",  round(hpd2(VCE$res),2), ")", sep='')
 	Res7[tr, 5] <-  paste(round(mean(VCE$Vphen),2), " (", round(hpd1(VCE$Vphen),2),  ", ",  round(hpd2(VCE$Vphen),2), ")", sep='')
  
 	res7[tr, 1] <- mean(VCE$Vgen)
	res7[tr, 2] <- mean(VCE$Venv)
 	res7[tr, 3] <- mean(VCE$Vdes)
  	res7[tr, 4] <- mean(VCE$res)
	res7[tr, 5] <- mean(VCE$Vphen)

	ROW[tr,mod] <- nrow(VCE)

	P <- ncol(VCE); P
 
	png(filename=paste(where, "_traceplots/VCEtraceplot_", tr, '_model', mod, '_subset', 1, ".png", sep=""), height=800*P, width=4000,  bg="white")

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

	rm(VCE)

}

Res7 <- data.frame(trait=Yvars, model=7, Res7)
res7 <- data.frame(trait=Yvars, model=7, res7)


ROW



Res <- smartbind(Res1, Res2, Res3, Res4, Res5, Res6, Res7)
res <- smartbind(res1, res2, res3, res4, res5, res6, res7)




write.table(Res, "ResultsCh_20230906.csv", col.names=T, row.names=F, sep=',')
write.table(res, "ResultsNu_20230906.csv", col.names=T, row.names=F, sep=',')



rm(list=ls())
gc()



















