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

nohup R CMD BATCH "--args  ${VAR}   31    1  60000 10000 50   99   9" 6_run_20240520.R  logs/log6_"${VAR}"_31_1.txt
nohup R CMD BATCH "--args  ${VAR}   31   11  60000 10000 50   99   9" 6_run_20240520.R  logs/log6_"${VAR}"_31_11.txt
nohup R CMD BATCH "--args  ${VAR}   31  103  60000 10000 50   99   9" 6_run_20240520.R  logs/log6_"${VAR}"_31_103.txt

module unload R/4.2.3
