#!/bin/bash

#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=18
#SBATCH --partition=fm-bigmem-1,fm-bigmem-2
#SBATCH --time=1200:00:00
#SBATCH --mem=600G
#SBATCH --array=10,12,14

source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=18
export OMP_NUM_THREADS=18


###Get response variable from array task id
VAR="${SLURM_ARRAY_TASK_ID}"





## Models for predicting older people ##

# D + G
nohup R CMD BATCH "--args  ${VAR} 10  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_10_103.txt

# D + G + E
#nohup R CMD BATCH "--args  ${VAR}  1  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_1_103.txt
#nohup R CMD BATCH "--args  ${VAR}  2  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_2_103.txt
#nohup R CMD BATCH "--args  ${VAR}  3  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_3_103.txt

nohup R CMD BATCH "--args  ${VAR}  4  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_4_103.txt
nohup R CMD BATCH "--args  ${VAR}  5  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_5_103.txt
nohup R CMD BATCH "--args  ${VAR}  6  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_6_103.txt

# D + G + E + GxE
#nohup R CMD BATCH "--args  ${VAR} 11  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_11_103.txt
#nohup R CMD BATCH "--args  ${VAR} 12  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_12_103.txt
#nohup R CMD BATCH "--args  ${VAR} 13  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_13_103.txt

# D + E
nohup R CMD BATCH "--args  ${VAR}  21  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_21_103.txt
nohup R CMD BATCH "--args  ${VAR}  22  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_22_103.txt
nohup R CMD BATCH "--args  ${VAR}  23  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_23_103.txt

nohup R CMD BATCH "--args  ${VAR}  24  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_24_103.txt
nohup R CMD BATCH "--args  ${VAR}  25  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_25_103.txt
nohup R CMD BATCH "--args  ${VAR}  26  103  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_26_103.txt





module unload R/4.2.3





