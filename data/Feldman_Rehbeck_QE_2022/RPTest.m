%% Code that runs the RP test on subject data

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

%Denote the number of trials
T=79;

%Load the Matlab formatted dataset
load('rpNumData.mat');

%Number of individuals
N=max(rpNumData(:,2));

%Test Results
%(1) Houtman Maks
TestResMain = zeros(N,1);
%% Definition of Budgets

%Generate D matrix:
%   col 1=Prob assigned to $2 from mixture
%   col 2=Prob assigned to $10 from mixture
%   col 3=Prob assigned to $30 from mixture
D=zeros(size(rpNumData,1),3);

%Calculate Slope
slope=(rpNumData(:,9)-rpNumData(:,12))./(rpNumData(:,7)-rpNumData(:,10));

%Calculate chance as percentage of numeraire
chance=(1/100)*rpNumData(:,4);

%Calculate Choice
D(:,1)=chance.*rpNumData(:,10)+(1-chance).*rpNumData(:,7);
D(:,2)=chance.*rpNumData(:,11)+(1-chance).*rpNumData(:,8);
D(:,3)=chance.*rpNumData(:,12)+(1-chance).*rpNumData(:,9);

%Define probability vectors used in the analysis
Prob = [D(:,3) D(:,1)];

%Parameter for problem tolerance in gurobi
parameter.MIPGap=10^(-6);
parameter.OutputFlag=0;

%Loop through individuals and perform a test Houtman Maks
for n=1:N
    un=79*(n-1); %Updates index for subject n
    %Run the houtman-maks test
    model=HMIG_FR(Prob(un+1:un+T,:), slope(un+1:un+T,:));
    result=gurobi(model,parameter);
    TestResMain(n,1)=result.objval;
end

filename = 'TestResMain8.mat';
save(filename, 'TestResMain');
