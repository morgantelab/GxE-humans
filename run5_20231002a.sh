#!/bin/bash
# 
#SBATCH --job-name=GxE_matrices
#SBATCH --cpus-per-task=56
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=48:00:00
#SBATCH --exclusive

module load gsl/2.7
source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64
module load R/4.2.3

###Set variables
export MKL_NUM_THREADS=54
export OMP_NUM_THREADS=54

nohup R CMD BATCH 5_Hadamard_20231002a.R  logs/log5_Hadamard_20231002a.txt

module unload R/4.2.3
module unload gsl/2.7


