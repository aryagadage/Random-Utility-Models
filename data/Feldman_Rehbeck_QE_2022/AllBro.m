%% Code that runs the Bronars style simulations

%% Reference to columns of rpNumData.mat 
%(1) session id=per session
%(2) individual id
%(3) task.time= time spent on a task in seconds
%(4) alpha=weight on option (N)umeraire
%(5) n.task=consistent numbering of tasks
%(6) r.task=real task order
%(7) ProbC1= probability of (E)xtreme lottery assigned to $2
%(8) ProbB1= probability of (E)xtreme lottery assigned to $10
%(9) ProbA1= probability of (E)xtreme lottery assigned to $30
%(10) ProbC2= probability of (N)umeraire lottery assigned to $2
%(11) ProbB2= probability of (N)umeraire lottery assigned to $10
%(12) ProbA2= probability of (N)umeraire lottery assigned to $30
%% 
clear

%Denote the number of trials each individual took part in
T=79;
S=144; %Number of different individuals

%Load the Matlab formatted dataset
load('rpNumData.mat');

%Number of simulations for bronars test
P=5000;

%% Houtman-Maks for Four types of distributions Distributions
%(1) Noisy behavior
%(2) Benchmark demand effect
%(3) Bootstrap
%(4) Conditional bootstrap
HMI=zeros(P,4);

%% Definition of Budgets

%Generate Budget Matrix of Numeraire and Extreme Lotteries:
B=rpNumData(1:T,7:12);

%Calculate Slope
price=(B(:,3)-B(:,6))./(B(:,1)-B(:,4));

%Set seed
rng(200);

%Parameter for problem tolerance in gurobi
parameter.MIPGap=10^(-6);
parameter.OutputFlag=0;

choices=zeros(T,2);

%Simulate Individuals and Recover the HMI
for n=1:P
    %HMI 1: Calculate chance as percentage
    chance=rand(79,1);
    
    %Calculate Choice
    choices(:,1)=chance.*B(:,6)+(1-chance).*B(:,3);
    choices(:,2)=chance.*B(:,4)+(1-chance).*B(:,1);
    
    %Run the houtman-maks test
    model=HMIG_FR(choices,price);
    result=gurobi(model,parameter);
    HMI(n,1)=result.objval;
    
    %HMI 2: Calculate perturbation around [45,55]
    chance2=.45+.1*chance;
    
    %Calculate Choice
    choices(:,1)=chance2.*B(:,6)+(1-chance2).*B(:,3);
    choices(:,2)=chance2.*B(:,4)+(1-chance2).*B(:,1);
    
    %Run the houtman-maks test
    model=HMIG_FR(choices,price);
    result=gurobi(model,parameter);
    HMI(n,2)=result.objval;
    
    %HMI 3: Calculate random draw from population of \alpha terms
    chance3=randi([1 T*S],T,1);
    
    %Calculate Choice
    choices(:,1)=(1/100)*rpNumData(chance3,4).*B(:,6)+(1-(1/100)*rpNumData(chance3,4)).*B(:,3);
    choices(:,2)=(1/100)*rpNumData(chance3,4).*B(:,4)+(1-(1/100)*rpNumData(chance3,4)).*B(:,1);
    
    %Run the houtman-maks test
    model=HMIG_FR(choices,price);
    result=gurobi(model,parameter);
    HMI(n,3)=result.objval;
    
    %HMI 4: Calculate random draw from population of \alpha conditional on
    %budget
    chance4pre=randi([1 S],T,1);
    chance4post=79.*(chance4pre-1)+[1:79]';
    
    %Calculate Choice
    choices(:,1)=(1/100)*rpNumData(chance4post,4).*B(:,6)+(1-(1/100)*rpNumData(chance4post,4)).*B(:,3);
    choices(:,2)=(1/100)*rpNumData(chance4post,4).*B(:,4)+(1-(1/100)*rpNumData(chance4post,4)).*B(:,1);
    
    %Run the houtman-maks test
    model=HMIG_FR(choices,price);
    result=gurobi(model,parameter);
    HMI(n,4)=result.objval;
end

filename = 'HMI8.mat';
save(filename, 'HMI');
