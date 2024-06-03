#!/bin/bash
#
#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=6
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=1000:00:00
#SBATCH --mem=800G


source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=6
export OMP_NUM_THREADS=6


nohup R CMD BATCH "--args  10  8 1       60000 10000 50   100   6" 4_run_20230707.R  logs/log4_10_8_1.txt
nohup R CMD BATCH "--args  10  9 1       60000 10000 50   100   6" 4_run_20230707.R  logs/log4_10_9_1.txt
nohup R CMD BATCH "--args  10 10 1       60000 10000 50   100   6" 4_run_20230707.R  logs/log4_10_10_1.txt
nohup R CMD BATCH "--args  12  8 1       60000 10000 50   100   6" 4_run_20230707.R  logs/log4_12_8_1.txt
nohup R CMD BATCH "--args  12  9 1       60000 10000 50   100   6" 4_run_20230707.R  logs/log4_12_9_1.txt
nohup R CMD BATCH "--args  12 10 1       60000 10000 50   100   6" 4_run_20230707.R  logs/log4_12_10_1.txt
nohup R CMD BATCH "--args  14  8 1       60000 10000 50   100   6" 4_run_20230707.R  logs/log4_14_8_1.txt
nohup R CMD BATCH "--args  14  9 1       60000 10000 50   100   6" 4_run_20230707.R  logs/log4_14_9_1.txt
nohup R CMD BATCH "--args  14 10 1       60000 10000 50   100   6" 4_run_20230707.R  logs/log4_14_10_1.txt



module unload R/4.2.3


