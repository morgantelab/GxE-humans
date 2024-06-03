#!/bin/bash
#
#SBATCH --job-name=makeG
#SBATCH --cpus-per-task=36
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=02:00:00
#SBATCH --mem=512G


module load R/4.1.2

nohup R CMD BATCH 3_makeG_20230326.R log3_20230326.txt

module unload R/4.1.2





