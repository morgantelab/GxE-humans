#!/bin/bash

#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=18
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=1200:00:00
#SBATCH --mem=600G
#SBATCH --array=10,12,14

source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=18
export OMP_NUM_THREADS=18


###Get response variable from array task id
VAR="${SLURM_ARRAY_TASK_ID}"


## Models for full dataset ##

# Raw Lifestyle # Already done!
#nohup R CMD BATCH "--args  ${VAR} 20   1  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_20_1.txt

# E of Lifestyle #
#nohup R CMD BATCH "--args  ${VAR} 21   1  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_21_1.txt

# e of Lifestyle #
#nohup R CMD BATCH "--args  ${VAR} 22   1  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_22_1.txt





## Models for predicting older people ##

# Raw Lifestyle #
nohup R CMD BATCH "--args  ${VAR} 20 103  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_20_103.txt

# E of Lifestyle #
nohup R CMD BATCH "--args  ${VAR} 21 103  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_21_103.txt

# e of Lifestyle #
nohup R CMD BATCH "--args  ${VAR} 22 103  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_22_103.txt





## Models for predicting younger people ##

# Raw Lifestyle #
#nohup R CMD BATCH "--args  ${VAR} 20 104  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_20_104.txt

# E of Lifestyle #
#nohup R CMD BATCH "--args  ${VAR} 21 104  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_21_104.txt

# e of Lifestyle #
#nohup R CMD BATCH "--args  ${VAR} 22 104  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_22_104.txt


module unload R/4.2.3

