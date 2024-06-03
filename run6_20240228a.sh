#!/bin/bash

#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=18
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=1200:00:00
#SBATCH --mem=600G
#SBATCH --array=10,12,14

source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=18
export OMP_NUM_THREADS=18


###Get response variable from array task id
VAR="${SLURM_ARRAY_TASK_ID}"




## Models for variance components estimation ##

# D + G
nohup R CMD BATCH "--args  ${VAR} 10  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_10_1.txt

# D + G + E
#nohup R CMD BATCH "--args  ${VAR}  1  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_1_1.txt
#nohup R CMD BATCH "--args  ${VAR}  2  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_2_1.txt
#nohup R CMD BATCH "--args  ${VAR}  3  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_3_1.txt

nohup R CMD BATCH "--args  ${VAR}  4  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_4_1.txt
nohup R CMD BATCH "--args  ${VAR}  5  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_5_1.txt
nohup R CMD BATCH "--args  ${VAR}  6  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_6_1.txt

# D + G + E + GxE
#nohup R CMD BATCH "--args  ${VAR} 11  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_11_1.txt
#nohup R CMD BATCH "--args  ${VAR} 12  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_12_1.txt
#nohup R CMD BATCH "--args  ${VAR} 13  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_13_1.txt

# D + E
nohup R CMD BATCH "--args  ${VAR}  21  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_21_1.txt
nohup R CMD BATCH "--args  ${VAR}  22  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_22_1.txt
nohup R CMD BATCH "--args  ${VAR}  23  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_23_1.txt

nohup R CMD BATCH "--args  ${VAR}  24  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_24_1.txt
nohup R CMD BATCH "--args  ${VAR}  25  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_25_1.txt
nohup R CMD BATCH "--args  ${VAR}  26  1  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_26_1.txt






module unload R/4.2.3





