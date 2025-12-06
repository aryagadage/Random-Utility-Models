function [ dist ] = fEU(x,p,alpha)
%% Description
%
% fEU returns the distance of choices from budgets in p, when the EU 
%parameter for three prize lotteries is x, and the realized choices are
%given by alpha in [0,100] from a convex budget
%% Instructions for input
%   p is vector of all input loteries 
%   p(:,1:3) are the numeraire lotteries [ordered (p_2 p_10 p_30) ]
%   p(:,4:6) are the extreme lotteries from experiment [ordered (p_2 p_10 p_30) ]
%
%   x = normalized utility
%
%   alpha=choices made by the individual
%% Function

%Define a tolerance for the utility comparisons
tolerance=0.0001;

%Numeraire expected utility
EU_N=p(:,3)+x*p(:,2);

%Numeraire expected utility
EU_E=p(:,6)+x*p(:,5);

%Return choice of alpha for individual with value x, sets equal to 50 if
%approximately indifferent
a=100*(EU_N>EU_E+tolerance)+50*(abs(EU_N-EU_E)<=tolerance);
dist = zeros(length(a),1);

%Gives a vector of distances for all values that are not indifferent
for k=1:length(a)
    if ~isequal(a(k),50)
        dist(k) = abs(a(k)-alpha(k));
    end
end

