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


## Models for predicting older people ##

# Lifestyle of Lifestyle #
nohup R CMD BATCH "--args  ${VAR} 121 103  60000 10000 50   100   6" 6_run_20231124.R  logs/log6_"${VAR}"_121_103.txt

# Residuals of Lifestyle #
nohup R CMD BATCH "--args  ${VAR} 122 103  60000 10000 50   100   6" 6_run_20231124.R  logs/log6_"${VAR}"_122_103.txt


module unload R/4.2.3


