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
#nohup R CMD BATCH "--args  ${VAR} 20 103  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_20_103.txt

# E of Lifestyle #
#nohup R CMD BATCH "--args  ${VAR} 21 103  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_21_103.txt

# e of Lifestyle #
#nohup R CMD BATCH "--args  ${VAR} 22 103  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_22_103.txt







## Models for predicting younger people ##

# Raw Lifestyle #
#nohup R CMD BATCH "--args  ${VAR} 20 104  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_20_104.txt

# E of Lifestyle #
#nohup R CMD BATCH "--args  ${VAR} 21 104  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_21_104.txt

# e of Lifestyle #
#nohup R CMD BATCH "--args  ${VAR} 22 104  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_22_104.txt





## Models for random within-cohort splits ##

# Raw Lifestyle #
nohup R CMD BATCH "--args  ${VAR} 20 11  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_20_11.txt
nohup R CMD BATCH "--args  ${VAR} 20 12  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_20_12.txt
nohup R CMD BATCH "--args  ${VAR} 20 13  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_20_13.txt
#nohup R CMD BATCH "--args  ${VAR} 20 14  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_20_14.txt
#nohup R CMD BATCH "--args  ${VAR} 20 15  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_20_15.txt

# e of Lifestyle #
nohup R CMD BATCH "--args  ${VAR} 22 11  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_22_11.txt
nohup R CMD BATCH "--args  ${VAR} 22 12  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_22_12.txt
nohup R CMD BATCH "--args  ${VAR} 22 13  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_22_13.txt
#nohup R CMD BATCH "--args  ${VAR} 22 14  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_22_14.txt
#nohup R CMD BATCH "--args  ${VAR} 22 15  60000 10000 50   100   6" 6_run_20231012.R  logs/log6_"${VAR}"_22_15.txt






module unload R/4.2.3

