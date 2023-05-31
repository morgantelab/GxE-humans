#!/bin/bash
#
#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=14
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=400:00:00
#SBATCH --mem=450G
#SBATCH --array=11,12,13,14,15,16,17,18,19,20,101,102,103,104,105,106,107,108

source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=14
export OMP_NUM_THREADS=14

###Get response variable from array task id
VAR="${SLURM_ARRAY_TASK_ID}"

nohup R CMD BATCH "--args  10 1 ${VAR}  30000 0 20   100" 4_run_20230531.R  logs/log4_10_1_"${VAR}".txt
nohup R CMD BATCH "--args  12 1 ${VAR}  30000 0 20   100" 4_run_20230531.R  logs/log4_12_1_"${VAR}".txt
nohup R CMD BATCH "--args  14 1 ${VAR}  30000 0 20   100" 4_run_20230531.R  logs/log4_14_1_"${VAR}".txt
nohup R CMD BATCH "--args  10 2 ${VAR}  30000 0 20   100" 4_run_20230531.R  logs/log4_10_2_"${VAR}".txt
nohup R CMD BATCH "--args  12 2 ${VAR}  30000 0 20   100" 4_run_20230531.R  logs/log4_12_2_"${VAR}".txt
nohup R CMD BATCH "--args  14 2 ${VAR}  30000 0 20   100" 4_run_20230531.R  logs/log4_14_2_"${VAR}".txt
nohup R CMD BATCH "--args  10 3 ${VAR}  30000 0 20   100" 4_run_20230531.R  logs/log4_10_3_"${VAR}".txt
nohup R CMD BATCH "--args  12 3 ${VAR}  30000 0 20   100" 4_run_20230531.R  logs/log4_12_3_"${VAR}".txt
nohup R CMD BATCH "--args  14 3 ${VAR}  30000 0 20   100" 4_run_20230531.R  logs/log4_14_3_"${VAR}".txt


module unload R/4.2.3


