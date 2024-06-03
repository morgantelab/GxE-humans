#!/bin/bash
#
#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=6
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=600:00:00
#SBATCH --mem=500G


source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=6
export OMP_NUM_THREADS=6


nohup R CMD BATCH "--args  10 7 11       30000 5000 25   100   5" 4_run_20230705.R  logs/log4_10_7_11.txt
nohup R CMD BATCH "--args  10 7 12       30000 5000 25   100   5" 4_run_20230705.R  logs/log4_10_7_12.txt
nohup R CMD BATCH "--args  10 7 13       30000 5000 25   100   5" 4_run_20230705.R  logs/log4_10_7_13.txt
nohup R CMD BATCH "--args  10 7 14       30000 5000 25   100   5" 4_run_20230705.R  logs/log4_10_7_14.txt
nohup R CMD BATCH "--args  10 7 15       30000 5000 25   100   5" 4_run_20230705.R  logs/log4_10_7_15.txt
nohup R CMD BATCH "--args  10 7 16       30000 5000 25   100   5" 4_run_20230705.R  logs/log4_10_7_16.txt



module unload R/4.2.3


