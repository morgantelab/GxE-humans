#!/bin/bash
# 
#SBATCH --job-name=gc_age_dp
#SBATCH --cpus-per-task=16
#SBATCH --partition=fm-bigmem-1,fm-bigmem-2,fm-bigmem-3,compute
#SBATCH --time=24:00:00
#SBATCH --mem=500G
#SBATCH --output=rerun_feb2024/gc_age_dp_20240215.out
#SBATCH --error=rerun_feb2024/gc_age_dp_20240215.err


gcta-1.94.1 \
        --grm grm_overall \
        --pheno grouped_pheno_age.txt \
        --qcovar covar_pcs.txt \
        --reml-bivar 1 4 \
        --out gc_age_dp_20240215 \
        --thread-num 16
