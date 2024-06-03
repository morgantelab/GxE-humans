#!/bin/bash
# 
#SBATCH --job-name=GxE_matrices
#SBATCH --cpus-per-task=64
#SBATCH --partition=fm-bigmem-4
#SBATCH --time=48:00:00
#SBATCH --mem=1400G

module load gsl/2.7
source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64
module load R/4.2.3

###Set variables
export MKL_NUM_THREADS=64
export OMP_NUM_THREADS=64

R CMD BATCH "--args 99" 5_Hadamard_20240409.R logs/log5c_99.txt


module unload R/4.2.3
module unload gsl/2.7


