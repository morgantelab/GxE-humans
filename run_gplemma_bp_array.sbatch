#!/bin/bash

#SBATCH --job-name=run_gplemma_bp
#SBATCH --ntasks=20
#SBATCH --partition=fm-bigmem-1,fm-bigmem-2,fm-bigmem-3
#SBATCH --time=12:00:00
#SBATCH --mem=200G
#SBATCH --output=run_gplemma_bp.%j.out
#SBATCH --error=run_gplemma_bp.%j.err
#SBATCH --array=10,12,14

###Load module
source /opt/intel/oneapi/mkl/2023.2.0/env/vars.sh intel64
module load lemma/1.0.0

###Get phenotype from array task id
VAR="${SLURM_ARRAY_TASK_ID}"

###Run gplemma
mpirun lemma_1_0_4 \
  --gplemma --random-seed 1 \
  --covar /data2/morgante_lab/ukbiobank_projects/GxE/datasets/LEMMA_Design_20230927.txt \
  --pheno "/data2/morgante_lab/ukbiobank_projects/GxE/datasets/LEMMA_Phen${VAR}_20230927.txt" \
  --bgen /data2/morgante_lab/data/ukbiobank/genotypes/array/ukb22418_all_auto_b0_v2_s488243_caucasian_white_british_unrel_gxe_bp_oxford.bgen \
  --environment /data2/morgante_lab/ukbiobank_projects/GxE/datasets/LEMMA_ENV_20230927.txt \
  --out "/data2/morgante_lab/ukbiobank_projects/GxE/run/run_gplemma_output/phen${VAR}.out"

###Unload modules
module unload lemma/1.0.0

