#!/bin/bash
#
#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=6
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=400:00:00
#SBATCH --mem=1500G


source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=1
export OMP_NUM_THREADS=1


nohup R CMD BATCH "--args  101 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_101_1_1.txt &
nohup R CMD BATCH "--args  102 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_102_1_1.txt &
nohup R CMD BATCH "--args  103 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_103_1_1.txt &
nohup R CMD BATCH "--args  104 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_104_1_1.txt &
nohup R CMD BATCH "--args  105 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_105_1_1.txt 
nohup R CMD BATCH "--args  106 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_106_1_1.txt &
nohup R CMD BATCH "--args  107 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_107_1_1.txt &
nohup R CMD BATCH "--args  119 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_119_1_1.txt &
nohup R CMD BATCH "--args  120 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_120_1_1.txt &
nohup R CMD BATCH "--args  121 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_121_1_1.txt 
nohup R CMD BATCH "--args  122 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_122_1_1.txt &
nohup R CMD BATCH "--args  123 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_123_1_1.txt &
nohup R CMD BATCH "--args  125 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_125_1_1.txt &
nohup R CMD BATCH "--args  126 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_126_1_1.txt &
nohup R CMD BATCH "--args  127 1 1  25000 0 5   100" 4_run_20230502.R  logs/log4_127_1_1.txt


module unload R/4.2.3


