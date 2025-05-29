#!/bin/bash
#SBATCH --job-name=rsvfit
#SBATCH --nodes=1
#SBATCH --mail-type=END,FAIL          # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=clara.brigitta@lshtm.ac.uk 
#SBATCH --ntasks-per-node=5       # Number of tasks per node (cores per task)
#SBATCH --mem=16G              # Request 8 GB of RAM
#SBATCH --time=10:00:00       # Set runtime to 10 hours
#SBATCH --output=rsvfit_%A_%a_%j.log 
#SBATCH --array=17-23

source activate ~/miniconda3/envs/R

# Change to the directory where your job script is located
~/miniconda3/envs/R/bin/Rscript fit_rsvhpc.R $SLURM_ARRAY_TASK_ID