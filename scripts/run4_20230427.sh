#!/bin/bash
#
#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=4
#SBATCH --partition=fm-bigmem-1,fm-bigmem-2
#SBATCH --time=200:00:00
#SBATCH --mem=1500G


source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=1
export OMP_NUM_THREADS=1


nohup R CMD BATCH "--args  10 2 1  25000 0 5" 4_run_20230427.R  logs/log4_10_2_1.txt &
nohup R CMD BATCH "--args  12 2 1  25000 0 5" 4_run_20230427.R  logs/log4_12_2_1.txt &
nohup R CMD BATCH "--args  14 2 1  25000 0 5" 4_run_20230427.R  logs/log4_14_2_1.txt


module unload R/4.2.3


