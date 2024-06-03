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

# D + E
nohup R CMD BATCH "--args  ${VAR}  21  11  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_21_11.txt
nohup R CMD BATCH "--args  ${VAR}  22  11  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_22_11.txt
nohup R CMD BATCH "--args  ${VAR}  23  11  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_"${VAR}"_23_11.txt

# D + G + E + GxE
nohup R CMD BATCH "--args  14 13  11  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_14_13_11.txt

module unload R/4.2.3

