


#################################
####### use date: 20230413 ######
#################################




####### run3.sh #######


What it does:
- Loads R
- Calls R script 3_makeG_<date>.R
- Unloads R
In a nutshell: The R scripts creates G matrix subsetting markers for the individuals selected, then extracts principal components. 
Output: G folder


Notes: 
- The <date> needs to be replaced with the most current one, see above.
- The marker editing is done using plink before any calculation
- The memory and number of threads needs to be commensurate to the size of the problem. Recommended 36 cores and 512 Gb of memory for the 98k individuals in data3_20230413.RData.
- The number of cores needs also to be specified at the beginning of the 3_makeG_<date>.R script.


Example using R script with date 20230413:

#!/bin/bash
#
#SBATCH --job-name=makeG
#SBATCH --cpus-per-task=36
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=36:00:00
#SBATCH --mem=512G


source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

nohup R CMD BATCH 3_makeG_20230413.R log3_20230413.txt

module unload R/4.2.3






###########################
###########################
###########################



####### run4.sh #######


What it does:
- Loads R
- Calls R script 4_run_<date>.R using version 4.2.3 compiled with MKL
- Unloads R
In a nutshell: The R scripts runs the models (see below) in BGLR, then saves samples values of variance components and solutions. 
Output: run/run1_results folder


Notes:
- The <date> needs to be replaced with the most current one, see above.
- The R script 4_run_<date>.R can be put in parallel by adding multiple lines in the shell script itself.
- The multiple R jobs are controlled using R BATCH arguments, i.e. "--args 1 2 1 100000 0 10", where:
  - The first value is the 'trait' to consider as dependent variable (see below).
  - The second value is the model to run (see below).
  - The third value is the subset to apply (see below).
  - The fourth value is the total number of iterations to run.
  - The fifth value is the burn-in (this must be set to 0).
  - The sixth value is thinning number.
- If a log files for each run is requested, this needs to be specified as in the example.


Example using R script with date 20230413:

#!/bin/bash
#
#SBATCH --job-name=VCE
#SBATCH --cpus-per-task=6
#SBATCH --partition=fm-bigmem-2
#SBATCH --time=24:00:00
#SBATCH --mem=512G


source /opt/intel/oneapi/mkl/2023.0.0/env/vars.sh intel64

module load R/4.2.3

export MKL_NUM_THREADS=1
export OMP_NUM_THREADS=1

nohup R CMD BATCH "--args   3 1 1  100000 0 10" 4_run_20230413.R    logs/log4_3_1_1.txt &
nohup R CMD BATCH "--args   3 2 1  100000 0 10" 4_run_20230413.R    logs/log4_3_2_1.txt &
nohup R CMD BATCH "--args 101 1 1  100000 0 10" 4_run_20230413.R  logs/log4_101_1_1.txt &
nohup R CMD BATCH "--args 101 2 1  100000 0 10" 4_run_20230413.R  logs/log4_101_2_1.txt &
nohup R CMD BATCH "--args 127 1 1  100000 0 10" 4_run_20230413.R  logs/log4_127_1_1.txt &
nohup R CMD BATCH "--args 127 2 1  100000 0 10" 4_run_20230413.R  logs/log4_127_2_1.txt
module unload R/4.2.3


# Variables: 
value   acronym       description
1         PR0          Pulse rate, first reading
2         PR1          Pulse rate, second reading
3         DP0          Diastolic pressure, first reading
4         DP1          Diastolic pressure, second reading
5         SP0          Systolic pressure, first reading
6         SP1          Systolic pressure, second reading
7         PP0          Pulse pressure, first reading
8         PP1          Pulse pressure, second reading
9         PPm          Pulse pressure, first and second readings average
10        DP0a         Diastolic pressure, first reading, adjusted as in Kerin & Marchini, 2020
11        DP1a         Diastolic pressure, second reading, adjusted as in Kerin & Marchini, 2020
12        SP0a         Systolic pressure, first reading, adjusted as in Kerin & Marchini, 2020
13        SP1a         Systolic pressure, second reading, adjusted as in Kerin & Marchini, 2020
14        PP0a         Pulse pressure, first reading, adjusted as in Kerin & Marchini, 2020
15        PP1a         Pulse pressure, second reading, adjusted as in Kerin & Marchini, 2020
16        PPma         Pulse pressure, first and second readings average, adjusted as in Kerin & Marchini, 2020
101       Townsend     Townsend deprivation index
102       walk_d       number of days/ week walked 10þ minutes
103       act0_d       number of days/week of moderate physical activity 10þ minutes
104       act1_d       number of days/week of vigorous physical activity 10þ minutes
105       TVtime       time spent watching television
106       sleep_d      sleep duration
107       smoking_now  is current smoker
108       veg_cook     cooked vegetable intake
109       fish_oily    oily fish intake
110       fish_lean    non-oily fish intake
111       meat_proc    processed meat intake
112       poultry      poultry intake
113       beef         beef intake
114       lamb         lamb intake
115       pork         pork intake
116       cheese       cheese intake
117       salt         salt added to food
118       tea          tea intake
119       alc1         alcohol intake frequency
120       waist        waist circumference
121       PCtime       time spent using PC
122       DRtime       time spent driving
123       getup        difficulty getting up in the morning
124       coffee       coffee intake
125       smoked_past  is past smoker
126       BFP          body fat percentage
127       BMR          body mass ratio


# Models
model 1:
y = Xb + Za + e
model 2:
y = Xb + Wu + Za + e
where:
y: dependent variable
X and b: incidence matrix and vector of solutions for the design effect (8 age-sex classes), 'FIXED' non-penalised effect
W and u: incidence matrix and vector of solutions for the lifestyle effect (27 environmental covariates), 'ridge regression' penalised effect
Z and a: incidence matrix and vector of solutions for the additive genetic effect (p principal components of the G matrix, tolerance=0), 'ridge regression' penalised effect
e: residual error, 1 class
note: when y is 101:127, the corresponding column of W is replaced with a vector of random values ~N(0,1).
 

# Subsets

1: uses all records.
2: ...


