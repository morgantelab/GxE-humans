#!/bin/bash
# 
#SBATCH --job-name=GxE_matrices
#SBATCH --cpus-per-task=56
#SBATCH --partition=fm-bigmem-1,fm-bigmem-2
#SBATCH --time=48:00:00
#SBATCH --exclusive
#SBATCH --mem=1400G

module load gsl/2.7
source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64
module load R/4.2.3

###Set variables
export MKL_NUM_THREADS=54
export OMP_NUM_THREADS=54

R CMD BATCH "--args 99 0 0 0 1" 5_Hadamard_20231229.R logs/log5_99_0_0_0_1.txt


module unload R/4.2.3
module unload gsl/2.7


