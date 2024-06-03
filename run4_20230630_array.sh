#!/bin/bash
#
#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=14
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=400:00:00
#SBATCH --mem=400G
#SBATCH --array=1,11,12,13,14,15,16,17,18,19,20,101,102,103,104,105,106,107,108,109,110,111,112,113,114

source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=14
export OMP_NUM_THREADS=14

###Get response variable from array task id
VAR="${SLURM_ARRAY_TASK_ID}"

nohup R CMD BATCH "--args  10 4 ${VAR}  30000 5000 25   100   4" 4_run_20230630.R  logs/log4_10_4_"${VAR}".txt
nohup R CMD BATCH "--args  12 4 ${VAR}  30000 5000 25   100   4" 4_run_20230630.R  logs/log4_12_4_"${VAR}".txt
nohup R CMD BATCH "--args  14 4 ${VAR}  30000 5000 25   100   4" 4_run_20230630.R  logs/log4_14_4_"${VAR}".txt
nohup R CMD BATCH "--args  10 5 ${VAR}  30000 5000 25   100   4" 4_run_20230630.R  logs/log4_10_5_"${VAR}".txt
nohup R CMD BATCH "--args  12 5 ${VAR}  30000 5000 25   100   4" 4_run_20230630.R  logs/log4_12_5_"${VAR}".txt
nohup R CMD BATCH "--args  14 5 ${VAR}  30000 5000 25   100   4" 4_run_20230630.R  logs/log4_14_5_"${VAR}".txt
nohup R CMD BATCH "--args  10 6 ${VAR}  30000 5000 25   100   4" 4_run_20230630.R  logs/log4_10_6_"${VAR}".txt
nohup R CMD BATCH "--args  12 6 ${VAR}  30000 5000 25   100   4" 4_run_20230630.R  logs/log4_12_6_"${VAR}".txt
nohup R CMD BATCH "--args  14 6 ${VAR}  30000 5000 25   100   4" 4_run_20230630.R  logs/log4_14_6_"${VAR}".txt
nohup R CMD BATCH "--args  10 7 ${VAR}  30000 5000 25   100   4" 4_run_20230630.R  logs/log4_10_7_"${VAR}".txt
nohup R CMD BATCH "--args  12 7 ${VAR}  30000 5000 25   100   4" 4_run_20230630.R  logs/log4_12_7_"${VAR}".txt
nohup R CMD BATCH "--args  14 7 ${VAR}  30000 5000 25   100   4" 4_run_20230630.R  logs/log4_14_7_"${VAR}".txt


module unload R/4.2.3







