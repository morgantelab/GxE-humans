#!/bin/bash

#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=36
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=1200:00:00
#SBATCH --mem=600G

source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=36
export OMP_NUM_THREADS=36

# D + G + E + GxE
nohup R CMD BATCH "--args  14 13  11  60000 10000 50   99   8" 6_run_20240228.R  logs/log6_14_13_11.txt

module unload R/4.2.3

