function [p_obs,choice_sets,chosen_alts,choice_set_list]=B_generate_fake_data_binarytenary(n)

rng(41); % For reproducibility
binary_fraction=.5;
tenary_fraction=.5;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%Binary and Tenary Choice Sets%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
binary_max_possible = nchoosek(1:n, 2);  % Maximum possible pairwise comparisons
tenary_max_possible = nchoosek(1:n, 3);  % Maximum possible pairwise comparisons
n2=nchoosek(n,2);
n3=nchoosek(n,3);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%Sample Choice Sets%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
binary_set=binary_max_possible(randsample(n2,   round(n2*binary_fraction)),:);
tenary_set=tenary_max_possible(randsample(n3,   round(n3*tenary_fraction)),:);
whole_set=1:n;
choice_set_list=[num2cell(binary_set,2);num2cell(tenary_set,2); num2cell(whole_set,2) ];
fprintf('Generating fake data for n=%d alternatives...\n', n);

length=size(binary_set,1)*2 + size(tenary_set,1)*3+n;
%% place holder
choice_sets = cell(length, 1);
chosen_alts = zeros(length, 1);
p_obs = [];


%% generate utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%Generate Uilities%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
true_utilities = randn(n, 1)*2;
fprintf('True utilities: ');
fprintf('%.3f ', true_utilities);
fprintf('\n\n');

%% generate choice probabilties: binary

for i=1:size(binary_set,1)
    
    % Randomly select 2 different alternatives
    alts = [binary_set(i,1),binary_set(i,2)];
    alt1 = alts(1);
    alt2 = alts(2);
    choice_sets{2*i-1} = alts;
    choice_sets{2*i} = alts;
    chosen_alts(2*i-1) = alt1;  % the alternative corresponding to the row
    chosen_alts(2*i) = alt2;  % the alternative corresponding to the row

    % Compute choice probability using logit model
    % P(choose alt1 | {alt1, alt2}) = exp(u1) / (exp(u1) + exp(u2))
    %u1 = true_utilities(alt1);
    %u2 = true_utilities(alt2);
        
    u1= randn(1, 1);
    u2 = randn(1, 1);
    
    prob_alt1 = 1/(1+exp(u2-u1));
    prob_alt2 = 1 - prob_alt1;

    %prob_alt1 = 1/(1+exp(u2-u1)) * 0.5 + 1/(1+exp(u1-u2))*0.5;
    %prob_alt2 = 1 - prob_alt1;    
    
    
    % Add both rows (one for each alternative's probability)
    p_obs = [p_obs ;prob_alt1 ;prob_alt2];
    
end

%% generate choice probabilties: tenary
base_iter=size(p_obs,1);
for i=1:size(tenary_set,1)
    
    % Randomly select 2 different alternatives
    alts = [tenary_set(i,1),tenary_set(i,2),tenary_set(i,3)];
    alt1 = alts(1);
    alt2 = alts(2);
    alt3 = alts(3);
    choice_sets{base_iter+3*i-2} = alts;
    choice_sets{base_iter+3*i-1} = alts;
    choice_sets{base_iter+3*i} = alts;

    chosen_alts(base_iter+3*i-2) = alt1;  % the alternative corresponding to the row
    chosen_alts(base_iter+3*i-1) = alt2;  % the alternative corresponding to the row
    chosen_alts(base_iter+3*i) = alt3;  % the alternative corresponding to the row

    % Compute choice probability using logit model
    % P(choose alt1 | {alt1, alt2}) = exp(u1) / (exp(u1) + exp(u2))
    %u1 = true_utilities(alt1);
    %u2 = true_utilities(alt2);
    %u3 = true_utilities(alt3);
    
    u1=randn(1,1);
    u2=randn(1,1);
    u3=randn(1,1);

    prob_alt1 = 1/(1+exp(u2-u1)+exp(u3-u1)) * 0.5 + 1/(1+exp(u1-u2)+exp(u1-u3)) * 0.5;
    prob_alt2 = 1/(1+exp(u1-u2)+exp(u3-u2)) * 0.5 + 1/(1+exp(u2-u1)+exp(u2-u3)) * 0.5;
    prob_alt3 = 1/(1+exp(u2-u3)+exp(u1-u3)) * 0.5 + 1/(1+exp(u3-u1)+exp(u3-u2)) * 0.5;

    %prob_alt1 = 1/(1+exp(u2-u1)+exp(u3-u1));
    %prob_alt2 = 1/(1+exp(u1-u2)+exp(u3-u2));
    %prob_alt3 = 1/(1+exp(u2-u3)+exp(u1-u3));
    
    
    % Add both rows (one for each alternative's probability)
    p_obs = [p_obs ;prob_alt1 ; prob_alt2; prob_alt3];
    
end

%% generate choice probabilties: whole set
base_iter=size(p_obs,1);
for i=1:n
    
    choice_sets{base_iter+i} = 1:n;
    chosen_alts(base_iter+i) = i ;

    % prob_temp = 1/sum(exp(true_utilities-true_utilities(i)))* 0.5 + 1/sum(exp(true_utilities(i)-true_utilities))* 0.5;
    prob_temp = 1/sum(exp(true_utilities-true_utilities(i)));

    p_obs = [p_obs; prob_temp];
    
end


end