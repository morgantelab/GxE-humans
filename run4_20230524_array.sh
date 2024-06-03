#!/bin/bash
#
#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=14
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=400:00:00
#SBATCH --mem=450G
#SBATCH --array=101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127

source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=14
export OMP_NUM_THREADS=14

###Get response variable from array task id
VAR="${SLURM_ARRAY_TASK_ID}"

nohup R CMD BATCH "--args  ${VAR} 2 1  30000 0 5   100" 4_run_20230526.R  logs/log4_"${VAR}"_2_1.txt

module unload R/4.2.3


