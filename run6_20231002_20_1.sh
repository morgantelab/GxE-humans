#!/bin/bash

#SBATCH --job-name=GxE_VCE_20_1
#SBATCH --cpus-per-task=14
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=600:00:00
#SBATCH --mem=600G
#SBATCH --array=10,12,14


source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=14
export OMP_NUM_THREADS=14

###Get response variable from array task id
VAR="${SLURM_ARRAY_TASK_ID}"

nohup R CMD BATCH "--args  ${VAR}  20 1       60000 10000 50   100   6" 6_run_20231008.R  logs/log6_"${VAR}"_20_1.txt

module unload R/4.2.3


