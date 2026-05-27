#!/bin/bash
# One-off SLURM job to compile cg_heuristic_rand_mex.c into
# cg_heuristic_rand_mex.mexa64 on the cluster.
#
# Submit with:   sbatch submit_compile_mex.sh
# Then verify:   ls -l cg_heuristic_rand_mex.mexa64
#
# After this succeeds, run the main array job:
#                sbatch submit_job_simulation_IP.sh

#SBATCH --job-name=mex-compile
#SBATCH --partition=devel
#SBATCH --time=00:10:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --output=mex-compile-%j.out

echo "SLURM_JOBID: $SLURM_JOBID"
echo "Host:        $(hostname)"
echo "Working dir: $(pwd)"

cd /home/hc654/Random-Utility-Models/feasibility_analysis_binary_menus_second_round

module load MATLAB/2023b

matlab -batch "mex -O cg_heuristic_rand_mex.c; \
               assert(exist('cg_heuristic_rand_mex','file')==3, 'mex build failed'); \
               fprintf('OK -> %s\n', which('cg_heuristic_rand_mex'));"

echo "--- post-build listing ---"
ls -l cg_heuristic_rand_mex.mexa64 || { echo "BUILD FAILED: mexa64 not produced"; exit 1; }
