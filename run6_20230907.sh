#!/bin/bash
#
#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=6
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=1000:00:00
#SBATCH --mem=800G
#SBATCH --array=10,12,14


source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=6
export OMP_NUM_THREADS=6


nohup R CMD BATCH "--args  ${VAR}  8 1       60000 10000 50   100   6" 6_run_20230707.R  logs/log6_"${VAR}"_8_1.txt
nohup R CMD BATCH "--args  ${VAR}  9 1       60000 10000 50   100   6" 6_run_20230707.R  logs/log6_"${VAR}"_9_1.txt
nohup R CMD BATCH "--args  ${VAR} 10 1       60000 10000 50   100   6" 6_run_20230707.R  logs/log6_"${VAR}"_10_1.txt



module unload R/4.2.3


