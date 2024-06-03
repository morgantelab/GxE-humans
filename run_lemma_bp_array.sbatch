#!/bin/bash

#SBATCH --job-name=run_lemma_bp
#SBATCH --ntasks=18
#SBATCH --partition=fm-bigmem-1,fm-bigmem-2,fm-bigmem-3
#SBATCH --time=24:00:00
#SBATCH --mem=1000G
#SBATCH --output=run_lemma_bp.%j.out
#SBATCH --error=run_lemma_bp.%j.err
#SBATCH --array=10

###Load module
source /opt/intel/oneapi/mkl/2023.2.0/env/vars.sh intel64
module load lemma/1.0.0

###Get phenotype from array task id
VAR="${SLURM_ARRAY_TASK_ID}"

###Run lemma
mpirun lemma_1_0_4 \
  --VB \
  --random-seed 1 \
  --covar /data2/morgante_lab/ukbiobank_projects/GxE/datasets/LEMMA_Design_20230927.txt \
  --pheno "/data2/morgante_lab/ukbiobank_projects/GxE/datasets/LEMMA_Phen${VAR}_20230927.txt" \
  --bgen /data2/morgante_lab/data/ukbiobank/genotypes/array/ukb22418_all_auto_b0_v2_s488243_caucasian_white_british_unrel_gxe_bp_oxford.bgen \
  --resume-from-state "run/run_lemma_output/lemma_interim_files/phen${VAR}_inference_dump_it0" \
  --environment /data2/morgante_lab/ukbiobank_projects/GxE/datasets/LEMMA_ENV_20230927.txt \
  --out "/data2/morgante_lab/ukbiobank_projects/GxE/run/run_lemma_output/phen${VAR}_inference.out"
  
mpirun lemma_1_0_4 \
  --RHEreg \
  --random-seed 1 \
  --covar /data2/morgante_lab/ukbiobank_projects/GxE/datasets/LEMMA_Design_20230927.txt \
  --pheno "/data2/morgante_lab/ukbiobank_projects/GxE/datasets/LEMMA_Phen${VAR}_20230927.txt" \
  --bgen /data2/morgante_lab/data/ukbiobank/genotypes/array/ukb22418_all_auto_b0_v2_s488243_caucasian_white_british_unrel_gxe_bp_oxford.bgen \
  --environment "/data2/morgante_lab/ukbiobank_projects/GxE/run/run_lemma_output/phen${VAR}_inference_converged_eta.out" \
  --out "/data2/morgante_lab/ukbiobank_projects/GxE/run/run_lemma_output/phen${VAR}_inference_pve.out"

