#!/bin/bash
#SBATCH --job-name=dom_cg
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --nodes=1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=haoge.chang@yale.edu
#SBATCH --partition=day
#SBATCH --time=1-00:00:00
#SBATCH --array=13-33
#SBATCH --mem=64G
echo "SLURM_JOBID:            $SLURM_JOBID"
echo "SLURM_ARRAY_TASK_ID:    $SLURM_ARRAY_TASK_ID"
echo "SLURM_ARRAY_JOB_ID:     $SLURM_ARRAY_JOB_ID"
echo "SLURM_CPUS_PER_TASK:    $SLURM_CPUS_PER_TASK"

# IMPORTANT: A_run_one_group.m calls helpers (C_gen_V_full, C_gen_one_ranking,
# B_QP, B_CG_heuristic_best_rand_cmex, ...) that live in
# feasibility_analysis_binary_menus/. Adding both directories to the MATLAB
# path so the Dominicks copies of B_solve_rum_CG / B_IP_pricing (which have
# the Frank-Wolfe gap termination + per-IP streaming) take precedence.
cd /home/hc654/Random-Utility-Models/Dominicks/scripts

module load Gurobi/12.0.1-GCCcore-13.3.0
module load MATLAB/2023b

matlab -batch "addpath('/home/hc654/Random-Utility-Models/feasibility_analysis_binary_menus'); addpath(pwd); A_run_one_group($SLURM_ARRAY_TASK_ID)"
