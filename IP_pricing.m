function [optim_value,optimizer]=IP_pricing(s,choice_sets,chosen_alts,n2,n3)

n=max(chosen_alts); %number of alternatives

nvar= n*(n-1) + 3*n3 + n;
num_D = n2+n3+1;
%initalize random variables
model.vtype=repmat('B', 1, nvar)
model.lb = zeros(1,nvar);
model.ub = ones(1,nvar);

%% inequality constraints
A=[];
rhs=[];
sense=[];

index = @(i,j) n*(i-1)+j; %index for the binary sets; note x_{ii} are free random variables
%% equality constraints: ranking variables
for i=1:n
    for j=i:n
        
        row_temp = sparse(1, nvars);
        row_temp(index(i,j))=1;
        row_temp(index(j,i))=1;

        A = [A; row];

        rhs = [rhs; 1];

        sense = [sense; '='];

    end
end

%% triangle inequality: ranking variables
for i=1:n
    for j=i:n
        for k=i:n
            row_temp = sparse(1, nvars);
            row_temp(index(i,j))=1;
            row_temp(index(j,k))=1;
            row_temp(index(k,i))=1;

            A = [A; row];

            rhs = [rhs; 1];

            sense = [sense; '<'];            
        end
    end
end

%% 

       







end