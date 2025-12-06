%Code to create the convex choices from the discrete choice tasks
%% Reference of columns in RPwDC for Revealed preference with discrete choices

%(1) id=true ID
%(2) task.time=response time
%(3) alpha=weight on numeraire lottery (N)
%(4) r.task=prefixed task number
%(5) n.task=order tasks were in
%(6) ProbC1= probability of extreme (E) lottery assigned to $2
%(7) ProbB1= probability of (E) lottery assigned to $10
%(8) ProbA1= probability of (E) lottery assigned to $30
%(9) ProbC2= probability of numeraire (N) lottery assigned to $2
%(10) ProbB2= probability of (N) lottery assigned to $10
%(11) ProbA2= probability of (N) lottery assigned to $30

%% Goal of this is to 
%   (1) separate the discrete choice tasks (e.g. tasks 80-85) from the 
%       convex choice task data to get implied mixtures

%% 
clear 

%Load dataset that includes discrete choices
load('RPwDC.mat');

%Generate discrete choice matrix
DC = zeros(2*144,3);
%Description of columns in DC
%(1) Individual id
%(2) Prefixed task id
%(3) Frequentist probability of reduced discrete choices

%Total number of tasks including the discrete choices
dcT=85;

%Number of individuals for discrete choice
N=size(RPwDC,1)/dcT;

%Loop to create smaller matrix of discrete choices
for n=1:N
    
    un=dcT*(n-1);   %counter for individual in RP dataset
    unDC=2*(n-1);   %counter for individual in the DC dataset
    DC(unDC+1:unDC+2,1)=n*ones(2,1); %Fill in the subject ID for the two tasks
    DC(unDC+1:unDC+2,2)= [32;1]; %Fill in the ID to match the convex task number
    
    %Loop to track the three discrete choices made in terms of alpha mixture
    for i=1:3
        DC(unDC+1,3)=DC(unDC+1,3)+RPwDC(un+79+i,3); %Aggregate discrete choices that match to convex task 32
        DC(unDC+2,3)=DC(unDC+2,3)+RPwDC(un+82+i,3); %Aggregate discrete choices that match to convex task 1 
    end
end
DC(:,3)=(1/3)*DC(:,3); %Takes the average choice from three discrete choices