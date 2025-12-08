function [optim_value,optimizer,V_sub,rankings]=B_IP_pricing(price,choice_sets,chosen_alts,choice_set_list,tol,V_sub,rankings)

n=max(chosen_alts); %number of alternatives

num_choice_set_size=zeros(1,n);

%% choice set size
for i=1:size(choice_set_list,1)
    
    size_temp=size(choice_set_list{i},2);

    num_choice_set_size(size_temp) = num_choice_set_size(size_temp) + 1;

end

% number of probability variables
nvars= n^2 + sum(num_choice_set_size(3:end).* (3:n));
num_D = sum(num_choice_set_size); %number of choice set

%initalize random variables
model.vtype=repmat('B', 1, nvars);
model.lb = zeros(1,nvars);
model.ub = ones(1,nvars);

%% inequality constraints
A=[];
rhs=[];
sense=[];

index = @(i,j,n) n*(i-1)+j; %index for the binary sets; note x_{ii} are free random variables
%% equality constraints: ranking variables
for i=1:n
    for j=(i+1):n
        
        row_temp = sparse(1, nvars);
        row_temp(index(i,j,n))=1;
        row_temp(index(j,i,n))=1;

        A = [A; row_temp];

        rhs = [rhs; 1];

        sense = [sense; '='];

    end
end

%% triangle inequality: ranking variables
for i=1:n
    for j=(i+1):n
        for k=(i+1):n
            row_temp = sparse(1, nvars);
            row_temp(index(i,j,n))=1;
            row_temp(index(j,k,n))=1;
            row_temp(index(k,i,n))=1;

            A = [A; row_temp];

            rhs = [rhs; 2];

            sense = [sense; '<'];            
        end
    end
end



%% 0 constraints
% p(D,j)\leq x_{jk} forall k\not j 
counter = n*n+1;
for j =1:size(choice_set_list,1) %for each choice set 
    
    choice_set_temp=choice_set_list{j} %this choice set
    choice_set_temp
    j
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%binary choice set: don't do anything variables already defined%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    if size(choice_set_temp,2)==2
        continue
    else
        
       choice_set_size_temp=size(choice_set_temp,2);

       for k=1:choice_set_size_temp

           %the probability of choosing choice_set_temp(k) is upper bounded
           %by the p(D,k)\leq x_{kj} to p(D,k) -x_{kj} \leq 0

           element_temp=choice_set_temp(k);        
           
           set_diff_temp = setdiff(choice_set_temp,element_temp); %all other elements

           for l=1:size(set_diff_temp,2)
                  
                row_temp = sparse(1, nvars);
                
                row_temp(index(element_temp,set_diff_temp(l),n))=-1;

                row_temp(counter)=1;

                A = [A; row_temp];

                rhs = [rhs; 0];

                sense = [sense; '<'];


           end
                counter=counter+1;

    end
    
    end
end

%% Probability Simplex Constraint
% p(D,j)\leq x_{jk} forall k \not = j 
counter = n*n+1;
for j =1:size(choice_set_list,1) %for each choice set 
    
    choice_set_temp=choice_set_list{j}; %this choice set
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%binary choice set: don't do anything variables already defined%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    if size(choice_set_temp,2)==2
        continue
    else
        
       choice_set_size_temp=size(choice_set_temp,2);

       row_temp = sparse(1,nvars);

       row_temp(counter:(counter+choice_set_size_temp-1))=1;
       
       A = [A; row_temp];
       rhs = [rhs; 1];
       sense = [sense; '='];

       counter = counter + choice_set_size_temp;
       
    end
end



model.A = A;
model.rhs = rhs;
model.sense = sense;
%% objective (i.e. price)
obj = zeros(1,nvars);
counter=1;
for j =1:size(choice_set_list,1) %for each choice set 

    choice_set_temp=choice_set_list{j};
    
    if size(choice_set_temp,2)==2
        
        obj(index(choice_set_temp(1),choice_set_temp(2),n))=price(counter);
        obj(index(choice_set_temp(2),choice_set_temp(1),n))=price(counter+1);
        counter = counter + 2;

    end
end


obj((n*n+1):end)=price(counter:end);

%params.BestObjStop=tol;
model.obj = obj;
model.modelsense = 'max';  
%result = gurobi(model,params);
result = gurobi(model);


fprintf('here')

%% returning the rank for the best objective function
ranking=zeros(1,n);
counter=1;
ranking_x=result.x(1:n^2);
for i=1:n
    
    ranking(i)=sum(ranking_x(counter:(counter+n-1)));

    counter = counter + n;
end
ranking=ranking+1; %start ranking from 1

%calcualte optimal value and the implied deterministic ranking of the
%optimizer
optim_value=result.objbound;
[optimizer, ~, ~]=C_gen_one_ranking(price,choice_sets,chosen_alts,n,'deterministic',ranking);

%update choice probability matrix and the rankings
V_sub = [V_sub, optimizer];
rankings = [rankings; ranking];

end