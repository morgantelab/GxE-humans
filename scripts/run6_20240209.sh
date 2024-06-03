#!/bin/bash

#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=48
#SBATCH --partition=fm-bigmem-1,fm-bigmem-2
#SBATCH --time=1200:00:00
#SBATCH --mem=600G
#SBATCH --array=1,10,12,14

source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=18
export OMP_NUM_THREADS=18


###Get response variable from array task id
VAR="${SLURM_ARRAY_TASK_ID}"


## Models for full dataset ##

# G + E
nohup R CMD BATCH "--args  ${VAR}  1    1  60000 10000 50   99   8" 6_run_20240209.R  logs/log6_"${VAR}"_1_1.txt
nohup R CMD BATCH "--args  ${VAR}  2    1  60000 10000 50   99   8" 6_run_20240209.R  logs/log6_"${VAR}"_2_1.txt
nohup R CMD BATCH "--args  ${VAR}  3    1  60000 10000 50   99   8" 6_run_20240209.R  logs/log6_"${VAR}"_3_1.txt

# G + E + GxE
nohup R CMD BATCH "--args  ${VAR} 11    1  60000 10000 50   99   8" 6_run_20240209.R  logs/log6_"${VAR}"_11_1.txt
nohup R CMD BATCH "--args  ${VAR} 12    1  60000 10000 50   99   8" 6_run_20240209.R  logs/log6_"${VAR}"_12_1.txt
nohup R CMD BATCH "--args  ${VAR} 13    1  60000 10000 50   99   8" 6_run_20240209.R  logs/log6_"${VAR}"_13_1.txt




## Models for predicting older people ##

# G + E
nohup R CMD BATCH "--args  ${VAR}  1  103  60000 10000 50   99   8" 6_run_20240209.R  logs/log6_"${VAR}"_1_103.txt
nohup R CMD BATCH "--args  ${VAR}  2  103  60000 10000 50   99   8" 6_run_20240209.R  logs/log6_"${VAR}"_2_103.txt
nohup R CMD BATCH "--args  ${VAR}  3  103  60000 10000 50   99   8" 6_run_20240209.R  logs/log6_"${VAR}"_3_103.txt

# G + E + GxE
nohup R CMD BATCH "--args  ${VAR} 11  103  60000 10000 50   99   8" 6_run_20240209.R  logs/log6_"${VAR}"_11_103.txt
nohup R CMD BATCH "--args  ${VAR} 12  103  60000 10000 50   99   8" 6_run_20240209.R  logs/log6_"${VAR}"_12_103.txt
nohup R CMD BATCH "--args  ${VAR} 13  103  60000 10000 50   99   8" 6_run_20240209.R  logs/log6_"${VAR}"_13_103.txt





module unload R/4.2.3





