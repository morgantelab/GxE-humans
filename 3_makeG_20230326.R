

rm(list=ls()); gc()

setwd("/data2/morgante_lab/ukbiobank_projects/GxE/G")



threads <- 36


#cp /data2/morgante_lab/data/ukbiobank/genotypes/array/ukb22418_c22_b0_v2_s488243.bed G2/test.bed
#cp /data2/morgante_lab/data/ukbiobank/genotypes/array/ukb22418_c22_b0_v2_s488243.bim G2/test.bim
#cp /data2/morgante_lab/data/ukbiobank/genotypes/array/ukb22418_c22_b0_v2_s488243.fam G2/test.fam




library(data.table)

'%!in%' <- function(x,y)!('%in%'(x,y))
censcale <- function(x,A,B){(A+(((x-mean(x)))/sd(x))*B)}


##################################
########### Load data ############
##################################

load("../datasets/data3_20230325.RData")
nrow(dttt)
dt <- dttt; rm(dttt)

table(dt$coh)

dt <- dt[order(dt$coh),]
#dt <- subset(dt, sub==11); nrow(dt)
#dt <- dt[seq(1,nrow(dt),10),]; nrow(dt)
keep <- dt$ID; length(keep)




system("cp /data2/morgante_lab/data/ukbiobank/genotypes/array/ukb22418_c22_b0_v2_s488243.fam test.fam")


fam <- read.table("test.fam", header=F)
dim(fam)
head(fam)

fam <- subset(fam, V1%in%keep); nrow(fam)

write.table(fam, "keep.list", sep='\t', col.names=F, row.names=F, quote=F)



chrs <- data.frame(paste("/data2/morgante_lab/data/ukbiobank/genotypes/array/ukb22418_c", 1:22, "_b0_v2_s488243", sep=''))
nrow(chrs)
chrs

write.table(chrs, "chrs.txt", col.names=F, row.names=F, quote=F)


system("cp /data2/morgante_lab/ftiezzi/software/gcta gcta")


system(paste("./gcta --mbfile chrs.txt --maf 0.01 --make-grm --out Gmatrix --thread-num ", threads, " --keep keep.list", sep=''))

system(paste("./gcta  --grm Gmatrix  --pca ", nrow(fam), "  --thread-num ", threads, "  --out PCvalues", sep=''))



eG1 <- read.table("PCvalues.eigenval", header=F); dim(eG1)
eG2 <- read.table("PCvalues.eigenvec", header=F); dim(eG2); 
eG2[1:10,1:10]
eG2$V2 <- NULL

colnames(eG2) <- c("ID", paste("PC",1:(ncol(eG2)-1),sep=''))

eigenG <- list()
eigenG[[1]] <- eG1
eigenG[[2]] <- eG2


save(eigenG, file="eigenG_20230326.RData")

system("rm Gmatrix.grm.N.bin Gmatrix.grm.bin Gmatrix.grm.id PCvalues.eigenval PCvalues.eigenvec chrs.txt keep.list test.fam")



rm(list=ls())





