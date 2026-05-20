#!/bin/bash
#SBATCH --job-name=sample
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=haoge.chang@yale.edu
#SBATCH --partition=week
#SBATCH --time=7-00:00:00
#SBATCH --array=15-50
#SBATCH --mem=64G
echo "SLURM_JOBID: " $SLURM_JOBID
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID


cd /home/hc654/Random-Utility-Models/feasibility_analysis_binary_menus

module load Gurobi/12.0.1-GCCcore-13.3.0
module load MATLAB/2023b

#matlab -batch "main_1121_grace($SLURM_ARRAY_TASK_ID,2); exit"
#matlab -batch "main_1202_grace($SLURM_ARRAY_TASK_ID,2); exit"

matlab -batch "A_run_all_method_HG($SLURM_ARRAY_TASK_ID)"





