
**********************
***C_gen_V_full.m:****
**********************

%Purpose:

%This program prepares the objects needed for the DC-RUM algorithm.  

%- In pricing_mode=='brute'*, it **enumerates all preference rankings over n alternatives and builds the full matrix V_full', whose columns correspond to rankings and whose rows correspond to observed choice sets.  
%- In other pricing_modes  (`'bestinsertion'` or `'randominsertion'`), it only generates the choice sets and leaves rankings (and the corresponding columns of V_full) to be constructed on the fly by the heuristic.

% Input:
%   p_obs        : observed choice probabilities (vector)
%   pricing_mode : 'brute', 'bestinsertion', or 'randominsertion' (default: 'brute')
%   choice_sets  : cell array of choice sets to use (if empty, generates all)
%   n            : number of alternatives
%   chosen_alts  : vector of actually chosen alternatives for each observation 

% Output:
% V_full: matrices of deterministic rankings projected onto the observed
% choice sets.
% all_rankings: all permutations of 1:n, each row being a ranking. In heuristic modes: returned as [].
% choice_sets: The cell array of choice sets used.


***************************
***C_gen_one_ranking.m:****
***************************


Purpose
This helper function constructs one deterministic preference rankings and the corresponding choice-probability columns 
Given observed choice data and choice sets, it produces:

a matrix V_sub whose columns are choice vectors implied by selected rankings,

the rankings themselves, and

indices of those rankings (when applicable).


% Input:
%   p_obs        : observed choice probabilities (vector)
%   n            : number of alternatives
%   choice_sets  : cell array of choice sets to use (if empty, generates all)
%   chosen_alts  : vector of actually chosen alternatives for each observation (optional)
%   mode         : random or deterministic, if deterministic, generate v
%   according to the given_ranking
%   given_ranking: user input for a determnistic ranking


% Output:
%   rankings: randomly generated rankings
%   V_sub: the corresponding choice probability vecotrs
%   sub_idx: an index set for the rankings

