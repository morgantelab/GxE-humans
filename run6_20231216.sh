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


## Models for full dataset ##

nohup R CMD BATCH "--args  ${VAR} 32   1  60000 10000 50   100   6" 6_run_20231216.R  logs/log6_"${VAR}"_32_1.txt





## Models for predicting older people ##

nohup R CMD BATCH "--args  ${VAR} 32 103  60000 10000 50   100   6" 6_run_20231216.R  logs/log6_"${VAR}"_32_103.txt






## Models for random within-cohort splits ##

nohup R CMD BATCH "--args  ${VAR} 32 11  60000 10000 50   100   6" 6_run_20231216.R  logs/log6_"${VAR}"_32_11.txt
nohup R CMD BATCH "--args  ${VAR} 32 12  60000 10000 50   100   6" 6_run_20231216.R  logs/log6_"${VAR}"_32_12.txt
nohup R CMD BATCH "--args  ${VAR} 32 13  60000 10000 50   100   6" 6_run_20231216.R  logs/log6_"${VAR}"_32_13.txt
#nohup R CMD BATCH "--args  ${VAR} 32 14  60000 10000 50   100   6" 6_run_20231216.R  logs/log6_"${VAR}"_32_14.txt
#nohup R CMD BATCH "--args  ${VAR} 32 15  60000 10000 50   100   6" 6_run_20231216.R  logs/log6_"${VAR}"_32_15.txt






module unload R/4.2.3


