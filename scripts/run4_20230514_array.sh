#!/bin/bash
#
#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=8
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=400:00:00
#SBATCH --mem=500G
#SBATCH --array=10,12,14

source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=8
export OMP_NUM_THREADS=8

###Get response variable from array task id
VAR="${SLURM_ARRAY_TASK_ID}"

nohup R CMD BATCH "--args  ${VAR} 2 1  25000 0 5   100" 4_run_20230515.R  logs/log4_"${VAR}"_2_1.txt

module unload R/4.2.3


