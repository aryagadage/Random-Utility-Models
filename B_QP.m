function [results]=B_QP(V,p_obs,display)
%----------------------------------------------------------------------
% Solve the RUM estimation problem using Quadratic Programming (least squares).
%----------------------------------------------------------------------
% Input:
%   p_obs        : observed choice probabilities (vector)
%   V_full       : given preference rankings 

    tic;
    % Remove duplicate columns to reduce problem size
    V_unique = unique(V', 'rows', 'stable')';
    if display
        fprintf('Removing duplicate columns...\n');
        fprintf('Reduced from %d to %d unique columns\n', size(V,2), size(V_unique,2));
        fprintf('Solving optimization...\n');
    end
    
    % Set up constraints for lsqlin, solve optimization using LSQLIN
    
    Aeq = ones(1, size(V_unique,2));  % sum(lambda) = 1
    beq = 1;
    lb = zeros(size(V_unique,2), 1);  % lambda >= 0
    ub = ones(size(V_unique,2), 1);   % lambda <= 1

    %----------------------------------------------------------------------
    % Display Options 
   %----------------------------------------------------------------------

    if display
        options = optimoptions('lsqlin', ...
        'Algorithm', 'interior-point', ...
        'Display', 'iter', ...
        'MaxIterations', 500);
    else
        options = optimoptions('lsqlin', ...
        'Algorithm', 'interior-point', ...
        'Display', 'off', ...
        'MaxIterations', 500);
    end

    lambda_QP = lsqlin(V_unique, p_obs, [], [], Aeq, beq, lb, ub, [], options);
    
    time_brute = toc;
    
    % Compute results
    error = norm(p_obs - V_unique * lambda_QP)^2;
    num_active = sum(lambda_QP > 1e-8);
    
    results.QP.lambda = lambda_QP; %optimal weiights
    results.QP.V = V_unique; %columns of deterministic rankings 
    results.QP.error = error; %distance 
    results.QP.time = time_brute; %time spent
    results.QP.num_active = num_active; %number of rankings with nonzero weights
    results.QP.success = true; 
    results.QP.residual = p_obs - V_unique* lambda_QP; %residual (prices)
    results.QP.optim_p = V_unique* lambda_QP; %predicted choice probability
    
    if display
    fprintf('✓ Completed in %.2f seconds\n', time_brute);
    fprintf('  - Final error: %.6e\n', error);
    fprintf('  - Active rankings: %d (out of %d unique columns)\n', num_active, size(V_unique,2));
    fprintf('  - Sum of lambdas: %.10f\n', sum(lambda_QP));
    
       % Show top rankings by weight (most important types in the mixture)
    fprintf('  - Top 5 rankings by weight:\n');
    [sorted_lambda, sort_idx] = sort(lambda_QP, 'descend');
    for i = 1:min(5, num_active)
        if sorted_lambda(i) > 1e-8
            fprintf('    Lambda = %.6f\n', sorted_lambda(i));
        end
    end
    end
end
