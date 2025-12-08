% SOLVE_RUM_PROJECTION: Find mixture of rankings that best fits observed choice probabilities
%
% Inputs:
%   p_obs : vector of observed choice probabilities (stacked across sets)
%   n     : number of alternatives
%
% Outputs:
%   lambda_opt : optimal mixture weights over rankings
%   V          : design matrix of deterministic choice vectors
%   rankings   : all permutations of preferences
%   choice_sets: all choice sets
%   error_val  : squared error of fit


function [lambda_opt, V, rankings, choice_sets, error_val] = solve_rum_projection(p_obs, n)
    % Generate choice vectors
    [V, rankings, choice_sets] = generate_choice_vectors(n);

    % Define optimization
    k = size(V,2); % number of rankings = n!
    
    % Quadratic form: (V*lambda - p_obs)'(V*lambda - p_obs)
    %read documentation on quadprog
    %https://www.mathworks.com/help/optim/ug/quadprog.html 

    H = 2*(V'*V);      % quadratic term
    f = -2*(V'*p_obs); % linear term

    % Constraints: sum(lambda) = 1, lambda >= 0
    Aeq = ones(1,k);
    beq = 1;
    A = -eye(k);   % lambda >= 0
    b = zeros(k,1);

    % Solve quadratic program
    options = optimoptions('quadprog','Display','off');
    [lambda_opt, error_val] = quadprog(H,f,A,b,Aeq,beq,[],[],[],options);

end

function [V, rankings, choice_sets] = generate_choice_vectors(n)
% Generate all deterministic choice vectors for n alternatives

    alternatives = 1:n;
    rankings = perms(alternatives); % all preference orderings
    rankings = flipud(rankings);    % ensure standard lexicographic order

    % build all nonempty subsets
    choice_sets = {};
    for r = 1:n
        subsets = nchoosek(alternatives, r);
        for j=1:size(subsets,1)
            choice_sets{end+1} = subsets(j,:);
        end
    end

    % build V matrix
    V = [];
    for i = 1:size(rankings,1)
        v = [];
        rank = rankings(i,:);
        for c = 1:length(choice_sets)
            subset = choice_sets{c};
            % find the top-ranked element of this subset
            for alt = rank
                if ismember(alt, subset)
                    chosen = alt;
                    break
                end
            end
            % mark choices within this subset
            for alt = subset
                v(end+1,1) = double(alt==chosen);
            end
        end
        V(:,i) = v; %#ok<AGROW>
    end
end
