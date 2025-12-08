# Random Utility Model (RUM) Estimation in Discrete-Choice Settings

This repository implements various algorithms to estimate a **Random Utility Model (RUM)** in a discrete-choice setting. The goal is to find a mixture of deterministic preference orderings (rankings) that best fit observed choice probabilities.

We assume a universe of **n = 5 alternatives** and observed choice probabilities (`p_obs`) for every nonempty choice set.

## File Structure

### `p_observations.m`
- **Description**: Constructs an 80×1 vector of observed choice probabilities for **n = 5 alternatives** and saves it as `p_obs.mat`. This file is used as input to the solvers. It also generates `p_obs.csv` with labels A-A, B-B, ....
  
---

### `solve_rum_projection.m` (Brute Force)
- **Description**: Generates all deterministic choice vectors for `n` alternatives and solves the quadratic programming (quadprog) optimization problem. 
- **Goal**: Computes the full projection using **all** columns of `V` at once, serving as a benchmark "oracle" solution.

#### Inputs:
- `p_obs` : Vector of observed choice probabilities (stacked across choice sets)
- `n`     : Number of alternatives

#### Outputs:
- `lambda_opt` : Optimal mixture weights over rankings
- `V`          : Design matrix of deterministic choice vectors
- `rankings`   : All permutations of preferences
- `choice_sets`: All choice sets
- `error_val`  : Squared error of fit

You can use `run_solver.m` to find optimal weights for **n = 5 alternatives** with observed probabilities `p_obs.mat`.

---

### `generate_choice_vectors.m`
- **Description**: Builds the full deterministic choice matrix \( V \) for all rankings and choice sets.

#### Inputs:
- `n` — Number of alternatives (e.g., 5)

#### Outputs:
- `V` — Full design matrix (rows = choice outcomes, cols = rankings)
- `rankings` — All \( n! \) preference orderings
- `choice_sets` — All non-empty subsets of {1,…,n}

Each column of `V` encodes how a ranking would choose in every possible subset.

---
### `pricing_problem.m` - find best new column and its score

#### Inputs: (In `solve_rum_columngen.m` after  `generate_choice_vectors.m`)

-  `V_full`     - full deterministic matrix (rows x Kfull)
-   `residual`   - current residual vector (p_obs - V_sub*lambda)
-   `subset_idx` - indices of columns already in the restricted master

#### Outputs:
- `best_idx`   - index in V_full of the best column to add
- `best_score` - v' * residual value for that column

---

### `solve_rum_columngen.m` (Column Generation)
- **Description**: Implements the **Column Generation (CG)** algorithm, a discrete-choice analogue of the **KS framework**, to find a mixture of deterministic rankings that best fits the observed choice probabilities `p_obs`.

#### Inputs:
- `p_obs`    - Stacked observed choice probabilities vector (rows match V rows)
- `n`        - Number of alternatives (for you: `n = 5`)
- `init_k`   - Initial number of columns to seed the restricted master (default: 1)
- `max_iters`- Maximum column-generation iterations (default: 200)
- `tol`      - Threshold for accepting a new column (default: 1e-8)

#### Outputs:
- `lambda_full` - Optimal mixture weights for the final subset (size = #subset)
- `V_sub`       - Design matrix of chosen columns (rows x #subset)
- `subset_idx`  - Indices (into full V) of columns included in `V_sub`
- `rankings`    - Full list of rankings (permutations from `generate_choice_vectors`)
- `choice_sets` - Cell array of choice sets (same order as V row stacking)
- `error_val`   - Final squared error = \( \|V_{\text{sub}} \lambda_{\text{full}} - p_{\text{obs}}\|^2 \)
- `iter`        - Number of CG iterations performed

This method is intended for small `n` (e.g., `n = 5` is fine, since `5! = 120 rankings`). For larger `n`, replace the exhaustive search in the pricing problem with a heuristic.

Example usage:
```matlab
[lam, Vsub, idx, r, sets, err, it] = solve_rum_columngen(p_obs, 5, 1, 200, 1e-8);

 Procedure:
1. Generate the full design matrix `V_full` via `generate_choice_vectors`.
2. Start with a **small subset** of columns (e.g., 1 ranking).
3. Solve the restricted master problem (RMP):
   \[
   \min_{\lambda \ge 0,\, \mathbf{1}'\lambda = 1} \|V_{\text{sub}}\lambda - p_{\text{obs}}\|^2
   \]
4. Compute the residual \( r = p_{\text{obs}} - V_{\text{sub}}\lambda \).
5. Run the pricing problem to find a column \( v_j \) maximizing \( v_j' r \).
6. If the improvement `best_score` > tolerance, add the column** and repeat.
7. Stop when no further improvement is possible.

 Output→ `RUM_results.mat` (contains fitted weights, predicted probabilities,
                             and chosen rankings)
````

---

### `bestinsertion.m` (Best Insertion algorithm)

