#!/bin/bash

#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=21
#SBATCH --partition=fm-bigmem-4
#SBATCH --nodelist=bigmem009
#SBATCH --time=1200:00:00
#SBATCH --mem=600G
#SBATCH --array=1-6,10-13,21-26

source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=21
export OMP_NUM_THREADS=21


###Get response variable from array task id
VAR="${SLURM_ARRAY_TASK_ID}"


## Models for variance components estimation ##
nohup R CMD BATCH "--args  18 ${VAR}  103  60000 10000 50   99   9" 6_run_20240228.R  logs/log6_18_"${VAR}"_103.txt



module unload R/4.2.3

