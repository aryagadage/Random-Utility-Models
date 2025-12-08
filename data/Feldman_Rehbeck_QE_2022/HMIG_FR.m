function [hmig_model] = HMIG_FR(choice,price)
%HMIG_FR gives the model to feed into Gurobi for the special case of
%Feldman and Rehbeck

%D is a dataset of the evaluated function. The size of D is T^2 where the element in row t and
%column v gives the value g^t(q^v)

%Generate a matrix of all constraints for the variables

%Computes the number of observations
T=size(choice,1);
D = zeros(T);

%Compute the matrix for the terms to run the test
for t=1:T
    for v=1:T
        if ~isequal(t,v)
        D(t,v)= choice(v,1)-price(t)*choice(v,2)-(choice(t,1)-price(t)*choice(t,2));
        end
    end
end

%Creates constant to be used in the program to account for largest
%difference between lotteries
if ~isempty(min(D(D>0)))
    db = min(1,min(D(D>0)));
else 
    db = 1;
end

%Gives a constant beta that satisfies beta > max(|g^t(q^v)|) + \min{1,db}
beta = max(max(abs(D)))+db+0.05;

%Pre-allocate a matrix for constraints removing redundant (t,t)
%constraints
%Order of variables in columns is (u_t)_{t \in T} , (U_{t,v})_{t,v \in T}, (A_t)_{t \in T}
A = zeros(4*T^2,T+T^2+T);

%Set the indices for the binary integer constraints
intcon = (T+1):(T+T^2+T);

%Pre-allocate variables for bounds on inequalities
b = zeros(4*T^2,1);

%Pre-allocate epsilon tolerance
e = 10^(-6);

%Define a variable to control the strict inequality tollerance
if db > e
    delta = e; 
else
    delta=db/2;
end

%All inequalities are re-written to <= b
counter=1;
for t=1:T
    for v=1:T
        %IP-1
        if ~isequal(t,v)
            A(counter,t)=1;                 %u_t
            A(counter,v)=-1;                %u_v
        end
        A(counter,T+(t-1)*T+v)=-2;      %U_{t,v}
        b(counter)=-e;
        counter=counter+1;
        
        %IP-2
        if ~isequal(t,v)
            A(counter,t)=-1;                %u_t
            A(counter,v)=1;                 %u_v
        end
        A(counter,T+(t-1)*T+v)=1;       %U_{t,v}
        b(counter)=1;
        counter=counter+1;
        
        %IP-5
        A(counter,T+(t-1)*T+v)=-beta;  %U_{t,v}
        A(counter,T+T^2+t)=beta;       %A_t
        b(counter)=D(t,v)+beta-delta;
        counter=counter+1;
        
        %IP-6
        A(counter,T+(v-1)*T+t)=beta;  %U_{v,t}
        A(counter,T+T^2+t)=beta;       %A_t
        b(counter)=D(t,v)+2*beta;
        counter=counter+1;
    end
end

hmig_model.A = sparse(A);
hmig_model.obj = [zeros(1,T+T^2) ones(1,T)];
hmig_model.rhs = b;
hmig_model.vtype = repmat('C',T+T^2+T, 1);
hmig_model.vtype(intcon) = 'B';
hmig_model.modelsense = 'max';
hmig_model.lb = zeros(1,T+T^2+T);
hmig_model.up = ones(1,T+T^2+T); 
end