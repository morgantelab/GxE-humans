

### Needs 
### source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64
### module load R/4.2.3



rm(list=ls()); gc()

#library(gtools)
#library(gdata)
#library(reshape)
library(ggplot2)

'%!in%' <- function(x,y)!('%in%'(x,y))



doplot <- 1



Gvars <- paste("PC", c(1:20), sep=''); length(Gvars)
Emarc <-	c("Townsend", "walk_d", "act0_d", "act1_d", "TVtime", "sleep_d", "smoking_now", "veg_cook", "fish_oily", "fish_lean", "meat_proc", "poultry", "beef", "lamb", "pork", "cheese", "salt", "tea", "alc1", "waist")
Etiez <- c("PCtime", "DRtime", "getup", "coffee", "smoked_past", "BFP", "BMR")
Evars <- c(Emarc, Etiez); length(Evars)
Yvars <- c("PR0", "PR1",   "DP0", "DP1",   "SP0", "SP1",   "PP0", "PP1",   "PPm",    "DP0a", "DP1a",   "SP0a", "SP1a",  "PP0a", "PP1a",   "PPam") 

gvar <- 100
home <- "/data2/morgante_lab/ukbiobank_projects/GxE/run/keep_results"
setwd(home)


if(1==0){

#	load("../../datasets/EigVal_20230628.RData")

	load("../../datasets/data4_20230628.RData")
	nrow(dt)


	dtt <- dt[c("ID", Yvars[c(10,12,14)], Evars, Gvars)]
	dim(dtt)
	head(dtt)

	save(dtt, file="../../datasets/data4pred_20230628.RData")

}


load("../../datasets/data4pred_20230628.RData")
nrow(dtt)




# trait, model, subset
traits <- c(10,12,14)
models <- c(1,2,3,4,5,6,7)
subsets <- c(11,12,13,14,15,16,17,18,19,20,101,102,103,104,105,106,107,108,109,110,111,112,113,114)

Ntr <- length(traits); Ntr
Nmod <- length(models); Nmod
Nsub <- length(subsets); Nsub




results <- list()
for(i in 1:99){
  results[[i]] <- list()
  for(tr in traits){
    results[[i]][[tr]] <- matrix(NA, 114, 7)
  }
}

results[[98]] <- results[[99]] <- list()
for(tr in traits){
  results[[98]][[tr]] <- results[[99]][[tr]] <- list()
  for(sub in subsets){
    results[[98]][[tr]][[sub]] <- results[[99]][[tr]][[sub]] <- list()
  }
}


tr=10
mod=1
sub=11

for(tr in traits){
  for(mod in models){
    for(sub in subsets){
      if(paste('PREDS_', tr, '_', mod, '_',sub, '.RData', sep='')%in%list.files()){results[[1]][[tr]][sub,mod] <- 1}else{}
    }
  }
}



results[[1]]

tr=14
mod=6
sub=103


allcolors <- c( '#364B9A', '#FEDA8B',  '#2166AC', '#DD3D2D', '#6EA6CD', '#C2E4EF', '#F67E4B', '#A50026')




for(tr in traits){
  for(mod in models){
    for(sub in subsets){
    
      print(c(tr, mod, sub))

      if(is.na(results[[1]][[tr]][sub,mod])){}else{
        
        load(paste('PREDS_', tr, '_', mod, '_',sub, '.RData', sep=''))
        
        if(sub==11){val <- subset(preds, sub%!in%c(11,12,13,14))} # primi 4
        if(sub==12){val <- subset(preds, sub%!in%c(15,16,17,18))} # secondi 4
        if(sub==13){val <- subset(preds, sub%!in%c(11,13,15,17))} # dispari
        if(sub==14){val <- subset(preds, sub%!in%c(12,14,16,18))} # pari
        if(sub==15){val <- subset(preds, sub%!in%c(11,12,15,16))} # primi 2 di ogni gruppo
        if(sub==16){val <- subset(preds, sub%!in%c(13,14,17,18))} # secondi 2 di ogni gruppo
        if(sub==17){val <- subset(preds, sub%!in%c(11,12,17,18))} # ultimi 2
        if(sub==18){val <- subset(preds, sub%!in%c(13,14,15,16))} # 4 nel mezzo
        if(sub==19){val <- subset(preds, sub%!in%c(11,16,13,18))} # alternati 
        if(sub==20){val <- subset(preds, sub%!in%c(15,12,17,14))} # alternati
        
        if(sub==101){val <- subset(preds, coh%!in%c("0_1", "0_2", "0_3", "0_4"))} # A
        if(sub==102){val <- subset(preds, coh%!in%c("1_1", "1_2", "1_3", "1_4"))} # B
        if(sub==103){val <- subset(preds, coh%!in%c("0_1", "0_2", "1_1", "1_2"))} # C
        if(sub==104){val <- subset(preds, coh%!in%c("0_3", "0_4", "1_3", "1_4"))} # D
        
        if(sub==105){val <- subset(preds, coh%!in%c("0_1", "0_2", "1_3", "1_4"))} # E
        if(sub==106){val <- subset(preds, coh%!in%c("0_1", "1_2", "1_3", "0_4"))} # F
        if(sub==107){val <- subset(preds, coh%!in%c("1_1", "0_2", "0_3", "0_4"))} # G
        if(sub==108){val <- subset(preds, coh%!in%c("1_1", "0_3", "0_4", "1_4"))} # H
        if(sub==109){val <- subset(preds, coh%!in%c("0_1", "0_3", "1_1", "1_3"))} # I
        if(sub==110){val <- subset(preds, coh%!in%c("0_2", "0_4", "1_2", "1_4"))} # L
        if(sub==111){val <- subset(preds, coh%!in%c("0_1", "0_4", "1_1", "1_4"))} # M
        if(sub==112){val <- subset(preds, coh%!in%c("0_2", "0_3", "1_2", "1_3"))} # N
        if(sub==113){val <- subset(preds, coh%!in%c("0_1", "0_2", "1_1", "1_2"))} # O
        if(sub==114){val <- subset(preds, coh%!in%c("0_3", "0_4", "1_3", "1_4"))} # P
        
        val$err1 <- val$phen-val$yHat
        val$err2 <- abs(val$phen-val$yHat)
       
        tmp <- lm(phen ~ yHat, data=val)

		if(doplot==1){
#        val$phen <- (val$phen-100)/10
#        val$yHat <- (val$yHat-100)/10
        	lo <- min(val$phen)
    	    up <- max(val$phen)
        
	        jpeg(filename=paste("../run5_predplots/predplot_", tr, '_', mod, '_', sub, ".jpeg", sep=''), height=800, width=1000,  bg="white")
       		 print(
        	  ggplot(val, aes(x=yHat, y=phen, colour=coh, Group=coh)) + 
        	    geom_point() + 
        	    scale_color_manual(values=allcolors[1:length(unique(val$coh))]) +
    	        geom_abline(intercept=0, slope=1)+ 
	            theme(axis.text.x = element_text(colour="black", size=15, angle=0, hjust=.5, vjust=.5, face="plain")) +
            	theme(axis.text.y = element_text(colour="black", size=15, angle=0, hjust=0, vjust=0, face="plain")) +
        	    labs(title = paste("Y=", round(tmp$coefficients[1],3), "+", round(tmp$coefficients[2],3), "Yhat", sep=''), y="Y", x="Yhat")
    	    )
	        dev.off()
        
        	rm(lo,up)
		} 

        results[[2]][[tr]][sub,mod] <- nrow(val)
        results[[3]][[tr]][sub,mod] <- round(cor(val$yHat, val$phen),6)
        results[[4]][[tr]][sub,mod] <- tmp$coefficients[1]
        results[[5]][[tr]][sub,mod] <- tmp$coefficients[2]
        results[[6]][[tr]][sub,mod] <- summary(tmp)$r.squared
        rm(tmp)



		if(sub%in%c(101,102,103,104)){

			vall <- merge(val, dtt, by="ID", all.x=T, all.y=F)
			X <- vall[c(Evars, Gvars)]
			Y <- vall[c("err1", "err2")]

			results[[98]][[tr]][[sub]][[mod]] <- matrix(NA, ncol(X)+1, 2)
			rownames(results[[98]][[tr]][[sub]][[mod]]) <- c(colnames(X), "Cohort")
			for(c in 1:ncol(X)){
				results[[98]][[tr]][[sub]][[mod]][c,1] <- summary(lm(Y[,1] ~ X[,c]))$coefficients[2,4]
				results[[98]][[tr]][[sub]][[mod]][c,2] <- summary(lm(Y[,2] ~ X[,c]))$coefficients[2,4]
			}
			results[[98]][[tr]][[sub]][[mod]][ncol(X)+1,1] <- anova(lm(err1 ~ coh, data=vall))[1,5]
			results[[98]][[tr]][[sub]][[mod]][ncol(X)+1,2] <- anova(lm(err2 ~ coh, data=vall))[1,5]
			rm(vall)



			for(c in 1:ncol(X)){
				if(results[[98]][[tr]][[sub]][[mod]][c,1]<0.00000001){

					tmp <- data.frame(y=Y[,1], x=X[,c])
	
					jpeg(filename=paste("../run5_errorplots/errorplot1_", tr, '_', mod, '_', sub, '_', colnames(X)[c], ".jpeg", sep=''), height=800, width=1000,  bg="white")
					print(
  		      			ggplot(tmp, aes(x=x, y=y)) +
        			    geom_point() + 
	        		    theme(axis.text.x = element_text(colour="black", size=15, angle=0, hjust=.5, vjust=.5, face="plain")) +
            			theme(axis.text.y = element_text(colour="black", size=15, angle=0, hjust=0, vjust=0, face="plain")) +
    	    		    labs(title = "", y="err1", x=colnames(X)[c])
    		    	)
		    	    dev.off()

				}	
			}



			for(c in 1:ncol(X)){
				if(results[[98]][[tr]][[sub]][[mod]][c,2]<0.00000001){

					tmp <- data.frame(y=Y[,2], x=X[,c])

					jpeg(filename=paste("../run5_errorplots/errorplot2_", tr, '_', mod, '_', sub, '_', colnames(X)[c], ".jpeg", sep=''), height=800, width=1000,  bg="white")
					print(
        				ggplot(tmp, aes(x=x, y=y)) +
	        		    geom_point() + 
		        	    theme(axis.text.x = element_text(colour="black", size=15, angle=0, hjust=.5, vjust=.5, face="plain")) +
        	    		theme(axis.text.y = element_text(colour="black", size=15, angle=0, hjust=0, vjust=0, face="plain")) +
        			    labs(title = "", y="err2", x=colnames(X)[c])
    		    	)
		    	    dev.off()

					}			
				}	
			}

		}



        
        cohs <- unique(val$coh); Ncohs <- length(cohs) 
        cohr <- matrix(NA, Ncohs, 5) 
        rownames(cohr) <- cohs
        colnames(cohr) <- c("N","CorrP","Inter", "Slope", "Rsq")
        for(i in 1:Ncohs){
          vall <- subset(val, coh==cohs[i])
          tmp <- lm(phen ~ yHat, data=vall)
          cohr[i,1] <- nrow(vall)
          cohr[i,2] <- round(cor(vall$yHat, vall$phen),6)
          cohr[i,3] <- tmp$coefficients[1]
          cohr[i,4] <- tmp$coefficients[2]
          cohr[i,5] <- summary(tmp)$r.squared
          rm(vall, tmp)
        }
        results[[12]][[tr]][sub,mod] <- mean(cohr[,1])
        results[[13]][[tr]][sub,mod] <- mean(cohr[,2])
        results[[14]][[tr]][sub,mod] <- mean(cohr[,3])
        results[[15]][[tr]][sub,mod] <- mean(cohr[,4])
        results[[16]][[tr]][sub,mod] <- mean(cohr[,5])				
        results[[99]][[tr]][[sub]][[mod]] <- cohr
        rm(preds, val, cohs, Ncohs, cohr)
  
  
  
        
      			
    }
  }
}



results[[1]][[10]]
results[[1]][[12]]
results[[1]][[14]]

results[[99]][[14]][[103]][[2]]
results[[99]][[14]][[103]][[6]]
results[[99]][[14]][[103]][[7]]



results[[4]][[14]][103,c(2,6,7)]
results[[5]][[14]][103,c(2,6,7)]


save(results, file='../predictionresults_20231010.RData')











rm(list=ls()); gc()






























