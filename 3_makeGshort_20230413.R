

rm(list=ls()); gc()

setwd("/data2/morgante_lab/ukbiobank_projects/GxE/G")


threads <- 36



library(data.table)


##################################
########### Load data ############
##################################

load("../datasets/data3_20230413.RData")
nrow(dttt)
dt <- dttt; rm(dttt)

table(dt$coh)

### Obtain individuals to keep ###
dt <- dt[order(dt$coh),]
#dt <- subset(dt, sub==11); nrow(dt)
#dt <- dt[seq(1,nrow(dt),10),]; nrow(dt)
keep <- dt$ID; length(keep)

### Write down keel.txt file for plink ###
write.table(data.frame(keep,keep), "keep.txt", sep='\t', col.names=F, row.names=F, quote=F)



### Use the old fam file as format ###
system("cp /data2/morgante_lab/data/ukbiobank/genotypes/array/ukb22418_all_auto_b0_v2_s488243_caucasian_white_british_unrel.fam test.fam")
fam <- read.table("test.fam", header=F); dim(fam); head(fam)

### Define individuals to keep ###
fam <- subset(fam, V1%in%keep); nrow(fam)

### Write down keel.txt file for gcta ###
write.table(fam, "keep.list", sep='\t', col.names=F, row.names=F, quote=F)





##################################
##### Run plink for editing ######
##################################

# system(paste("/data2/morgante_lab/fabiom/software/plink19 --bfile /data2/morgante_lab/data/ukbiobank/genotypes/array/ukb22418_all_auto_b0_v2_s488243_caucasian_white_british_unrel  	--keep-allele-order    --keep keep.txt   --maf 0.01 --mac 5 --geno 0.1 --hwe 1e-10    --make-bed   --out /data2/morgante_lab/ukbiobank_projects/GxE/G/ukb22418_tmp      --threads", threads, sep=' '))






##################################
#### Run GCTA for creating G #####
##################################


### Copy GCTA executable ###

#system("cp /data2/morgante_lab/ftiezzi/software/gcta gcta")


### Make G ###

#system(paste("./gcta --bfile ukb22418_tmp --maf 0.01 --make-grm --out Gmatrix --thread-num ", threads, " --keep keep.list", sep=''))


### Extract principal components of G ###

system(paste("./gcta  --grm Gmatrix  --pca ", nrow(fam), "  --thread-num ", threads, "  --out PCvalues", sep=''))


### Read in principal components, allocate into a list ###

eG1 <- read.table("PCvalues.eigenval", header=F); dim(eG1)
eG2 <- read.table("PCvalues.eigenvec", header=F); dim(eG2); 
eG2[1:10,1:10]
eG2$V2 <- NULL

colnames(eG2) <- c("ID", paste("PC",1:(ncol(eG2)-1),sep=''))

eigenG <- list()
eigenG[[1]] <- eG1
eigenG[[2]] <- eG2


### Save the list with G principal components ###

save(eigenG, file="eigenG_20230413.RData")


### Clean up ###

system("rm Gmatrix.grm.N.bin Gmatrix.grm.bin Gmatrix.grm.id PCvalues.eigenval PCvalues.eigenvec keep.txt keep.list test.fam ukb22418_tmp.bed ukb22418_tmp.bim ukb22418_tmp.fam")




rm(list=ls())




