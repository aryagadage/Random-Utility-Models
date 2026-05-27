/*  cg_heuristic_rand_mex.c
 *
 *  MEX-accelerated randomized best-insertion heuristic for column
 *  generation pricing in the DC-RUM problem.
 *
 *  Key optimizations over the pure-MATLAB version:
 *    1) rank_of[] inverse map  — O(1) position lookup instead of find()
 *    2) alt_to_sets mapping    — skip irrelevant choice sets
 *    3) Incremental present_count — O(1) "all members placed?" check
 *    4) Position-independent base_score — active sets cannot contain the
 *       unplaced alt, so their score contribution is constant across
 *       insertion positions.  Only newly-activated sets (completed by
 *       adding alt) need per-position evaluation.
 *    5) Incrementally maintained base_score — avoids rescanning all
 *       active sets at every step.
 *
 *  Syntax
 *    [ranking, v_final] = cg_heuristic_rand_mex(n, choice_sets, ...
 *                              chosen_alts, residual, insertion_order)
 *
 *  Inputs
 *    n               : (scalar) number of alternatives
 *    choice_sets     : (M x 1 cell) each cell is a row vector of
 *                      alternative indices (1-based)
 *    chosen_alts     : (M x 1 double) chosen alt for each choice set (1-based)
 *    residual        : (M x 1 double) current dual prices
 *    insertion_order : (1 x n double) random permutation of 1:n (1-based);
 *                      first element is the seed, rest give insertion order
 *
 *  Outputs
 *    ranking : (1 x n double) the constructed ranking (1-based),
 *              most-preferred first
 *    v_final : (M x 1 double) binary choice vector for this ranking
 *
 *  Compile (from MATLAB):
 *      mex -O cg_heuristic_rand_mex.c
 */

#include "mex.h"
#include <float.h>
#include <string.h>

void mexFunction(int nlhs, mxArray *plhs[],
                 int nrhs, const mxArray *prhs[])
{
    int i, j, s, step, pos;

    /* ================================================================== */
    /* Parse & validate inputs                                            */
    /* ================================================================== */
    if (nrhs != 5)
        mexErrMsgIdAndTxt("cg:nrhs",
            "Five inputs required: n, choice_sets, chosen_alts, "
            "residual, insertion_order.");

    int n = (int)mxGetScalar(prhs[0]);

    if (!mxIsCell(prhs[1]))
        mexErrMsgIdAndTxt("cg:type", "choice_sets must be a cell array.");
    int M = (int)mxGetNumberOfElements(prhs[1]);

    double *chosen_dbl = mxGetPr(prhs[2]);
    double *residual   = mxGetPr(prhs[3]);
    double *ins_dbl    = mxGetPr(prhs[4]);

    if ((int)mxGetNumberOfElements(prhs[2]) != M)
        mexErrMsgIdAndTxt("cg:dim", "chosen_alts must have M elements.");
    if ((int)mxGetNumberOfElements(prhs[4]) != n)
        mexErrMsgIdAndTxt("cg:dim", "insertion_order must have n elements.");

    /* ================================================================== */
    /* Flatten choice_sets cell array into C arrays                       */
    /* ================================================================== */
    int *cs_offsets = (int *)mxMalloc((M + 1) * sizeof(int));
    int *set_size   = (int *)mxMalloc(M * sizeof(int));

    int total_elems = 0;
    for (s = 0; s < M; s++) {
        mxArray *cell = mxGetCell(prhs[1], s);
        int sz = (int)mxGetNumberOfElements(cell);
        set_size[s]   = sz;
        cs_offsets[s]  = total_elems;
        total_elems   += sz;
    }
    cs_offsets[M] = total_elems;

    int *cs_alts = (int *)mxMalloc(total_elems * sizeof(int));
    for (s = 0; s < M; s++) {
        mxArray *cell = mxGetCell(prhs[1], s);
        double  *data = mxGetPr(cell);
        int off = cs_offsets[s];
        for (j = 0; j < set_size[s]; j++)
            cs_alts[off + j] = (int)data[j] - 1;      /* 0-based */
    }

    /* Convert chosen_alts and insertion_order to 0-based int arrays */
    int *chosen    = (int *)mxMalloc(M * sizeof(int));
    for (s = 0; s < M; s++) chosen[s] = (int)chosen_dbl[s] - 1;

    int *ins_order = (int *)mxMalloc(n * sizeof(int));
    for (i = 0; i < n; i++) ins_order[i] = (int)ins_dbl[i] - 1;

    /* ================================================================== */
    /* Precompute alt -> sets mapping                                      */
    /* ================================================================== */
    int *alt_count = (int *)mxCalloc(n, sizeof(int));
    for (s = 0; s < M; s++)
        for (j = cs_offsets[s]; j < cs_offsets[s + 1]; j++)
            alt_count[cs_alts[j]]++;

    int *alt_offsets = (int *)mxMalloc((n + 1) * sizeof(int));
    alt_offsets[0] = 0;
    for (i = 0; i < n; i++)
        alt_offsets[i + 1] = alt_offsets[i] + alt_count[i];

    int *alt_sets = (int *)mxMalloc(alt_offsets[n] * sizeof(int));
    int *fill     = (int *)mxCalloc(n, sizeof(int));
    for (s = 0; s < M; s++)
        for (j = cs_offsets[s]; j < cs_offsets[s + 1]; j++) {
            int a = cs_alts[j];
            alt_sets[alt_offsets[a] + fill[a]++] = s;
        }

    /* ================================================================== */
    /* Working arrays                                                     */
    /* ================================================================== */
    int *ranking       = (int *)mxMalloc(n * sizeof(int));
    int *rank_of       = (int *)mxMalloc(n * sizeof(int));
    int *present_count = (int *)mxCalloc(M, sizeof(int));
    int *is_active     = (int *)mxCalloc(M, sizeof(int));
    int *newly_active  = (int *)mxMalloc(M * sizeof(int));  /* reused buffer */

    for (i = 0; i < n; i++) rank_of[i] = -1;

    /* ================================================================== */
    /* Seed: place the first element of insertion_order                    */
    /* ================================================================== */
    int seed = ins_order[0];
    ranking[0]    = seed;
    rank_of[seed] = 0;
    int current_len = 1;

    /* Update present_count; activate qualifying sets; init base_score */
    double maintained_score = 0.0;
    for (j = alt_offsets[seed]; j < alt_offsets[seed + 1]; j++) {
        s = alt_sets[j];
        present_count[s]++;
        if (present_count[s] == set_size[s]) {
            is_active[s] = 1;
            /* Compute this set's contribution (all members placed) */
            int off = cs_offsets[s], sz = set_size[s];
            int min_p = n + 1, top = -1;
            for (i = 0; i < sz; i++) {
                int m = cs_alts[off + i];
                int p = rank_of[m];
                if (p < min_p) { min_p = p; top = m; }
            }
            if (top == chosen[s])
                maintained_score += residual[s];
        }
    }

    /* ================================================================== */
    /* Greedy insertion loop                                               */
    /* ================================================================== */
    for (step = 1; step < n; step++) {
        int alt = ins_order[step];

        /* -------------------------------------------------------------- */
        /* Find sets that become active ("newly active") when alt is added */
        /* These are sets containing alt where present_count == size - 1.  */
        /* Key insight: no currently-active set contains alt, because alt  */
        /* isn't placed yet.  So there is no overlap with active_list.     */
        /* -------------------------------------------------------------- */
        int n_newly = 0;
        for (j = alt_offsets[alt]; j < alt_offsets[alt + 1]; j++) {
            s = alt_sets[j];
            if (!is_active[s] && present_count[s] == set_size[s] - 1)
                newly_active[n_newly++] = s;
        }

        /* -------------------------------------------------------------- */
        /* base_score: contribution from all currently active sets.        */
        /* Since none contain alt, their top member is independent of the  */
        /* insertion position.  We maintain this incrementally.            */
        /* -------------------------------------------------------------- */
        double base_score = maintained_score;

        /* -------------------------------------------------------------- */
        /* Try each insertion position (0 = most preferred, current_len =  */
        /* least preferred)                                                */
        /* -------------------------------------------------------------- */
        double best_sc = -DBL_MAX;
        int    best_pos = 0;

        if (n_newly == 0) {
            /* No set is completed by adding alt → every position gives
               the same score.  Skip the per-position loop entirely. */
            best_sc  = base_score;
            best_pos = 0;
        } else {
            for (pos = 0; pos <= current_len; pos++) {
                double score = base_score;

                /* Evaluate only newly-active sets (all contain alt) */
                for (i = 0; i < n_newly; i++) {
                    s = newly_active[i];
                    int off = cs_offsets[s], sz = set_size[s];
                    int min_p = n + 1, top = -1;

                    for (j = 0; j < sz; j++) {
                        int m = cs_alts[off + j];
                        int p;
                        if (m == alt) {
                            p = pos;
                        } else {
                            p = rank_of[m];
                            if (p >= pos) p++;   /* shift for insertion */
                        }
                        if (p < min_p) { min_p = p; top = m; }
                    }
                    if (top == chosen[s])
                        score += residual[s];
                }

                if (score > best_sc) {
                    best_sc  = score;
                    best_pos = pos;
                }
            }
        }

        /* -------------------------------------------------------------- */
        /* Commit: insert alt at best_pos                                  */
        /* -------------------------------------------------------------- */
        for (i = current_len - 1; i >= best_pos; i--) {
            ranking[i + 1] = ranking[i];
            rank_of[ranking[i + 1]] = i + 1;
        }
        ranking[best_pos] = alt;
        rank_of[alt]      = best_pos;
        current_len++;

        /* -------------------------------------------------------------- */
        /* Update present_count and maintained_score                       */
        /* -------------------------------------------------------------- */
        for (j = alt_offsets[alt]; j < alt_offsets[alt + 1]; j++) {
            s = alt_sets[j];
            present_count[s]++;
            if (present_count[s] == set_size[s] && !is_active[s]) {
                is_active[s] = 1;
                /* Compute this set's contribution using final rank_of */
                int off = cs_offsets[s], sz = set_size[s];
                int min_p = n + 1, top = -1;
                for (i = 0; i < sz; i++) {
                    int m = cs_alts[off + i];
                    int p = rank_of[m];
                    if (p < min_p) { min_p = p; top = m; }
                }
                if (top == chosen[s])
                    maintained_score += residual[s];
            }
        }
    }

    /* ================================================================== */
    /* Output 1: ranking (1 x n, 1-based)                                 */
    /* ================================================================== */
    plhs[0] = mxCreateDoubleMatrix(1, n, mxREAL);
    double *out_rank = mxGetPr(plhs[0]);
    for (i = 0; i < n; i++)
        out_rank[i] = ranking[i] + 1;          /* back to 1-based */

    /* ================================================================== */
    /* Output 2: v_final (M x 1 binary choice vector)                     */
    /* ================================================================== */
    if (nlhs >= 2) {
        plhs[1] = mxCreateDoubleMatrix(M, 1, mxREAL);
        double *v_final = mxGetPr(plhs[1]);
        for (s = 0; s < M; s++) {
            int off = cs_offsets[s], sz = set_size[s];
            int min_p = n + 1, top = -1;
            for (j = 0; j < sz; j++) {
                int m = cs_alts[off + j];
                int p = rank_of[m];
                if (p < min_p) { min_p = p; top = m; }
            }
            v_final[s] = (top == chosen[s]) ? 1.0 : 0.0;
        }
    }

    /* ================================================================== */
    /* Free temporary arrays                                              */
    /* ================================================================== */
    mxFree(cs_offsets);  mxFree(set_size);     mxFree(cs_alts);
    mxFree(chosen);      mxFree(ins_order);
    mxFree(alt_count);   mxFree(alt_offsets);  mxFree(alt_sets);
    mxFree(fill);
    mxFree(ranking);     mxFree(rank_of);      mxFree(present_count);
    mxFree(is_active);   mxFree(newly_active);
}
