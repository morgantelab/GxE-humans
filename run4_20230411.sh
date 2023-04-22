#!/bin/bash
#
#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=36
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=01:00:00
#SBATCH --mem=256G


source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=1
export OMP_NUM_THREADS=1


nohup R CMD BATCH "--args   3 1 1  100000 0 10" 4_run_20230413.R    logs/log4_3_1_1.txt &
nohup R CMD BATCH "--args   3 2 1  100000 0 10" 4_run_20230413.R    logs/log4_3_2_1.txt &
nohup R CMD BATCH "--args 101 1 1  100000 0 10" 4_run_20230413.R  logs/log4_101_1_1.txt &
nohup R CMD BATCH "--args 101 2 1  100000 0 10" 4_run_20230413.R  logs/log4_101_2_1.txt &
nohup R CMD BATCH "--args 127 1 1  100000 0 10" 4_run_20230413.R  logs/log4_127_1_1.txt &
nohup R CMD BATCH "--args 127 2 1  100000 0 10" 4_run_20230413.R  logs/log4_127_2_1.txt


module unload R/4.2.3


