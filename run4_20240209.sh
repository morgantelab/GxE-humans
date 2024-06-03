#!/bin/bash
# 
#SBATCH --job-name=GxE_covariates
#SBATCH --cpus-per-task=4
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

R CMD BATCH 4bis_newcovariates_20230209.R logs/log4bis_newcovariates.txt


module unload R/4.2.3
module unload gsl/2.7


