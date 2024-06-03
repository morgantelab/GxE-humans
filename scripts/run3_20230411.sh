#!/bin/bash
#
#SBATCH --job-name=makeG
#SBATCH --cpus-per-task=36
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=96:00:00
#SBATCH --mem=512G


source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

nohup R CMD BATCH 3_makeG_20230413.R log3_20230413.txt

module unload R/4.2.3

