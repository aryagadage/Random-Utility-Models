%% Full Results
%% Reference of columns in rpNumData.mat

%(1) session id=per session
%(2) individual id
%(3) task.time= time spent on a task in seconds
%(4) alpha=weight on option (N)umeraire
%(5) n.task=consistent numbering of tasks
%(6) r.task=order tasks were faced
%(7) ProbC1= probability of (E)xtreme lottery assigned to $2
%(8) ProbB1= probability of (E)xtreme lottery assigned to $10
%(9) ProbA1= probability of (E)xtreme lottery assigned to $30
%(10) ProbC2= probability of (N)umeraire lottery assigned to $2
%(11) ProbB2= probability of (N)umeraire lottery assigned to $10
%(12) ProbA2= probability of (N)umeraire lottery assigned to $30
%% Reference for Task Numbers using consistent ordering

%N1 tasks are 1-9
%N2 tasks are 10-28
%N3 tasks are 29-44
%N4 tasks are 45-57
%N5 tasks are 58-67
%N6 tasks are 68-74
%N7 tasks are 75-78
%N8 task is 79

%% Preallocation and data loading
clear

%Denote the number of trials each individual took part in
T=79;

%Load the Matlab formatted datasets
load('rpNumData.mat'); %Full data minus repeated discrete choices 
load('DC.mat'); %Implied \alpha from repeated discrete choices

%Generate column vector for D1 discrete task
D1dis=DC(1:2:288,3);

%Generate column vector for D2 discrete task
D2dis=DC(2:2:288,3);

%Number of individuals 
N=size(rpNumData,1)/T;

%Generate D matrix to add relative price info
D=[rpNumData zeros(size(rpNumData,1),1)];

%Calculate relative price information
price=(D(:,9)-D(:,12))./(D(:,7)-D(:,10));

%Calculate unique utility values to look at for EU test
uprice = uniquetol(price,.0001);
uval_temp = sort(uprice./(1+uprice));
nslopes2=zeros(2*length(uval_temp)+1,1);
counter =1; 
for s=1:length(uval_temp)
    if isequal(s,1)
        nslopes2(counter) = .5*uval_temp(counter);    
    else
        nslopes2(counter) = .5*(uval_temp(s-1)+uval_temp(s));
    end
    counter=counter+1;
    nslopes2(counter)=uval_temp(s);
    counter=counter+1; 
end
nslopes2(2*length(uval_temp)+1)=.5*(uval_temp(length(uval_temp))+1);

%Add prices to the (D)ata matrix
D(:,13)=price;

%Holds HEURISTIC non-EU behavior based on multiple slopes with mixing 
IndNonEU=zeros(N,1);  % Individuals who violate EU 
IndNonEU1=zeros(N,1); % Individuals who are not EU-1
IndNonEU4=zeros(N,1); % Individuals who are not EU-1

%Holds aggregate mixing by TASK for mixing (Col 1) Extreme (Col 2) Numeraire (Col 3)
Mix = zeros(T,3);  % Mixing when alpha \neq 0, 100 
Mix1 = zeros(T,3); % Stronger mixing \min{ 100-\alpha, \alpha \} >=2
Mix4 = zeros(T,3); % Strongest mixing \min{ 100-\alpha, \alpha \} >=5

%Holds aggregate mixing for INDIVIDUALS
numMix=zeros(N,1);  % Mixing when alpha \neq 0, 100
numMix1=zeros(N,1); % Stronger mixing \min{ 100-\alpha, \alpha \} >1
numMix4=zeros(N,1); % Strongest mixing \min{ 100-\alpha, \alpha \} >4

%Aggregates demands at the different budgets
AggDemand=zeros(79,1);

%Create matrix with the choices from convex tasks and discrete choice tasks
convDC = zeros(2*N,1);

%Preallocate to hold tasks based on numeraire for regression
taskN1=zeros(9*N,1);
taskN2=zeros(19*N,1);
taskN3=zeros(16*N,1);
taskN4=zeros(13*N,1);
taskN5=zeros(10*N,1);
taskN6=zeros(7*N,1);
taskN7=zeros(4*N,1);

%% Result 1 and Robustness Checks

%Count EU violations and mixing behavior for full dataset
for n=1:N
    un=79*(n-1);     %counter for individuals
    sH=zeros(T,1);   %Holds slopes of where mixing happens for given individual
    sH1=zeros(T,1);  %Holds slopes of where stronger mixing happens for given individual
    sH4=zeros(T,1);  %Holds slopes of where strongest mixing happens for given individual
    
    counter=0;      %counts number of slopes where mixing occurs
    counter1=0;     %counts number of slopes stronger mixing counter 
    counter4=0;     %strongest mixing counter
    
    %Loop through all tasks for each individual to get relevant statistics
    for t=1:79        
        AggDemand(t,1)=AggDemand(t,1)+D(un+t,4); %Update aggregate demand
        
        %Characterizing \alpha terms for mixing alpha \neq 0, 100
        if isequal(rpNumData(un+t,4),100) %All Numeraire
            Mix(t,2)=Mix(t,2)+1; %Updates numeraire 
        elseif isequal(rpNumData(un+t,4),0) %All Extreme
            Mix(t,3)=Mix(t,3)+1; %Updates extreme
        else %Mixture and Non-EU via slopes
            Mix(t,1)=Mix(t,1)+1;
            %Tracks whether individual mixes at multiple slopes
            for s=1:counter
                if abs(price(un+t,1)-sH(s,1))>=.001
                    IndNonEU(n,1)=1;
                end
            end
            sH(counter+1,1)=price(un+t,1);
            counter=counter+1;
        end
        %Record convex information for comparison with repeated discrete choices
        if isequal(t,1)
            convDC(2*(n-1)+2,1)=D(un+t,4); %Matches conv task for D2
        elseif isequal(t,32)
            convDC(2*(n-1)+1,1)=D(un+t,4); %Matches conv task for D1
        end
        
        %% Characterizing \alpha terms for 2-mixing 
        if rpNumData(un+t,4) >= 99 %All Numeraire
            Mix1(t,2)=Mix1(t,2)+1;
        elseif rpNumData(un+t,4) <= 1 %All Extreme
            Mix1(t,3)=Mix1(t,3)+1;
        else %Mixture and Non-EU check
            Mix1(t,1)=Mix1(t,1)+1;
            %Tracks whether individual mixes at multiple slopes
            for s=1:counter1
                if abs(price(un+t,1)-sH1(s,1))>=.001
                    IndNonEU1(n,1)=1;
                end
            end
            sH1(counter1+1,1)=price(un+t,1);
            counter1=counter1+1;
        end
        
        %% Characterizing \alpha terms for 5-mixing
        if rpNumData(un+t,4) >= 96 %All Numeraire
            Mix4(t,2)=Mix4(t,2)+1;
        elseif rpNumData(un+t,4) <= 4 %All Extreme
            Mix4(t,3)=Mix4(t,3)+1;
        else %Mixture and Non-EU check
            Mix4(t,1)=Mix4(t,1)+1;
            %Tracks whether individual mixes at multiple slopes
            for s=1:counter4
                if abs(price(un+t,1)-sH4(s,1))>=.001
                    IndNonEU4(n,1)=1;
                end
            end
            sH4(counter4+1,1)=price(un+t,1);
            counter4=counter4+1;
        end
    end
    
    %Get information for mixing histogram and create indexes for task
    %numbers
    
    %Saves the number of times each INDIVIDUAL mixed for various mixing
    %strengths 
    numMix(n,1)=counter; %mixing for anything not 0 or 100 
    numMix1(n,1)=counter1; %2-mix for individual
    numMix4(n,1)=counter4; %5-mix for individual
    
    %Create index of observations to use for the regressions for each
    %budget
    taskN1(9*(n-1)+1:9*(n-1)+9,1)=un+(1:9)';
    taskN2(19*(n-1)+1:19*(n-1)+19,1)=un+(10:28)';
    taskN3(16*(n-1)+1:16*(n-1)+16,1)=un+(29:44)';
    taskN4(13*(n-1)+1:13*(n-1)+13,1)=un+(45:57)';
    taskN5(10*(n-1)+1:10*(n-1)+10,1)=un+(58:67)';
    taskN6(7*(n-1)+1:7*(n-1)+7,1)=un+(68:74)';
    taskN7(4*(n-1)+1:4*(n-1)+4,1)=un+(75:78)';
end

%Get percentage of mixing in each task by dividing by N for different
%mixing strengths
Mix=Mix/N;
Mix1=Mix1/N;
Mix4=Mix4/N;

%Scale the aggregate demand by the number of subjects
AggDemand=(1/N)*AggDemand;

%% Generate Histogran for Figure 6
hold on
axis( [0 79 0 31] )
histogram(numMix,'FaceColor',[.5,.5,.5])
xlabel('Number of Mixing Choices');
ylabel('Number of Subjects');
saveas(gcf,'MixHist.png');
hold off
close


%% Table 2 + Appendix D tables

%Generate Agg Demand by Numeraire Lottery with last row for num obs
AggMixEnd = zeros(4,9);

AggMixEnd(:,1)=[ 100*mean(Mix(1:9,:))' ; 9*N];
AggMixEnd(:,2)=[ 100*mean(Mix(10:28,:))' ; 19*N];
AggMixEnd(:,3)=[ 100*mean(Mix(29:44,:))' ; 16*N];
AggMixEnd(:,4)=[ 100*mean(Mix(45:57,:))' ; 13*N];
AggMixEnd(:,5)=[ 100*mean(Mix(58:67,:))' ; 10*N];
AggMixEnd(:,6)=[ 100*mean(Mix(68:74,:))' ; 7*N];
AggMixEnd(:,7)=[ 100*mean(Mix(75:78,:))' ; 4*N];
AggMixEnd(:,8)=[ 100*Mix(79,:)' ; N];
AggMixEnd(:,9)=[ 100*mean(Mix)' ; 79*N];

%Table for the amount of mixing by each level of Reference
input.data=AggMixEnd;
input.tablePositioning='h';
input.tableColLabels = {'N1','N2','N3','N4','N5','N6','N7','N8','Total'};
input.tableRowLabels = {'Mix','Numeraire','Extreme','Num Obs'};
input.dataFormatMode = 'row';
input.dataFormat = {'%.0f'};
input.tableColumnAlignment = 'c';
MixAmt = latexTable(input);
save('mixamt_tab', 'MixAmt')

clear input

%Generate Agg Demand for 2-mixing
AggMixEnd1 = zeros(4,9);

AggMixEnd1(:,1)=[ 100*mean(Mix1(1:9,:))' ; 9*N];
AggMixEnd1(:,2)=[ 100*mean(Mix1(10:28,:))' ; 19*N];
AggMixEnd1(:,3)=[ 100*mean(Mix1(29:44,:))' ; 16*N];
AggMixEnd1(:,4)=[ 100*mean(Mix1(45:57,:))' ; 13*N];
AggMixEnd1(:,5)=[ 100*mean(Mix1(58:67,:))' ; 10*N];
AggMixEnd1(:,6)=[ 100*mean(Mix1(68:74,:))' ; 7*N];
AggMixEnd1(:,7)=[ 100*mean(Mix1(75:78,:))' ; 4*N];
AggMixEnd1(:,8)=[ 100*Mix1(79,:)' ; N];
AggMixEnd1(:,9)=[ 100*mean(Mix1)' ; 79*N];

%Table 5 for 2-mixing
input.data=AggMixEnd1;
input.tablePositioning='h';
input.tableColLabels = {'N1','N2','N3','N4','N5','N6','N7','N8','Total'};
input.tableRowLabels = {'Mix','Numeraire','Extreme','Num Obs'};
input.dataFormatMode = 'row';
input.dataFormat = {'%.0f'};
input.tableColumnAlignment = 'c';
MixAmt1 = latexTable(input);
save('mixamt_tab1', 'MixAmt1')

clear input

%Generate Agg Demand 4-mixing
AggMixEnd4 = zeros(4,9);

AggMixEnd4(:,1)=[ 100*mean(Mix4(1:9,:))' ; 9*N];
AggMixEnd4(:,2)=[ 100*mean(Mix4(10:28,:))' ; 19*N];
AggMixEnd4(:,3)=[ 100*mean(Mix4(29:44,:))' ; 16*N];
AggMixEnd4(:,4)=[ 100*mean(Mix4(45:57,:))' ; 13*N];
AggMixEnd4(:,5)=[ 100*mean(Mix4(58:67,:))' ; 10*N];
AggMixEnd4(:,6)=[ 100*mean(Mix4(68:74,:))' ; 7*N];
AggMixEnd4(:,7)=[ 100*mean(Mix4(75:78,:))' ; 4*N];
AggMixEnd4(:,8)=[ 100*Mix4(79,:)' ; N];
AggMixEnd4(:,9)=[ 100*mean(Mix4)' ; 79*N];

%Table 6 for the 4-mixing
input.data=AggMixEnd4;
input.tablePositioning='h';
input.tableColLabels = {'N1','N2','N3','N4','N5','N6','N7','N8','Total'};
input.tableRowLabels = {'Mix','Numeraire','Extreme','Num Obs'};
input.dataFormatMode = 'row';
input.dataFormat = {'%.0f'};
input.tableColumnAlignment = 'c';
MixAmt4 = latexTable(input);
save('mixamt_tab4', 'MixAmt1')

clear input

%% Plot graphs of log price vs mixing behavior Fig 7 + Appendix E

%Vector of prices for graph
vprice=price(1:79);

%Figure 7 (b)
hold on
plot(log(vprice),Mix(:,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice),Mix(:,2),'LineStyle','none','Marker','d','MarkerFaceColor',[0,0,0],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice),Mix(:,3),'LineStyle','none','Marker','s','MarkerFaceColor',[1,1,1],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
legend('Mix','Numeraire','Extreme');
xlabel('Log Relative Price of Numeraire');
ylabel('Fraction of Subjects');
ylim([0 1]);
saveas(gcf,'AllMix.png');
hold off
close

%Figure 18 (a)
hold on 
plot(log(vprice(1:9,1)),Mix(1:9,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(1:9,1)),Mix(1:9,2),'LineStyle','none','Marker','d','MarkerFaceColor',[0,0,0],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(1:9,1)),Mix(1:9,3),'LineStyle','none','Marker','s','MarkerFaceColor',[1,1,1],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
legend('Mix','Numeraire','Extreme');
xlabel('Log Relative Price of Numeraire');
ylabel('Fraction of Subjects');
ylim([0 1]);
saveas(gcf,'N1Mix.png');
hold off
close

%Figure 7 (a) 
hold on 
plot(log(vprice(10:28)),Mix(10:28,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(10:28)),Mix(10:28,2),'LineStyle','none','Marker','d','MarkerFaceColor',[0,0,0],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(10:28)),Mix(10:28,3),'LineStyle','none','Marker','s','MarkerFaceColor',[1,1,1],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
legend('Mix','Numeraire','Extreme');
xlabel('Log Relative Price of Numeraire');
ylabel('Fraction of Subjects');
ylim([0 1]);
saveas(gcf,'N2Mix.png');
hold off
close

%Figure 18 (b)
hold on 
plot(log(vprice(29:44,1)),Mix(29:44,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(29:44,1)),Mix(29:44,2),'LineStyle','none','Marker','d','MarkerFaceColor',[0,0,0],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(29:44,1)),Mix(29:44,3),'LineStyle','none','Marker','s','MarkerFaceColor',[1,1,1],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
legend('Mix','Numeraire','Extreme');
xlabel('Log Relative Price of Numeraire');
ylabel('Fraction of Subjects');
ylim([0 1]);
saveas(gcf,'N3Mix.png');
hold off
close

%Figure 18 (c)
hold on 
plot(log(vprice(45:57,1)),Mix(45:57,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(45:57,1)),Mix(45:57,2),'LineStyle','none','Marker','d','MarkerFaceColor',[0,0,0],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(45:57,1)),Mix(45:57,3),'LineStyle','none','Marker','s','MarkerFaceColor',[1,1,1],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
legend('Mix','Numeraire','Extreme');
xlabel('Log Relative Price of Numeraire');
ylabel('Fraction of Subjects');
ylim([0 1]);
saveas(gcf,'N4Mix.png');
hold off
close

%Figure 18 (d)
hold on 
plot(log(vprice(58:67,1)),Mix(58:67,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(58:67,1)),Mix(58:67,2),'LineStyle','none','Marker','d','MarkerFaceColor',[0,0,0],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(58:67,1)),Mix(58:67,3),'LineStyle','none','Marker','s','MarkerFaceColor',[1,1,1],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
legend('Mix','Numeraire','Extreme');
xlabel('Log Relative Price of Numeraire');
ylabel('Fraction of Subjects');
ylim([0 1]);
saveas(gcf,'N5Mix.png');
hold off
close

%Figure 18(e)
hold on 
plot(log(vprice(68:74,1)),Mix(68:74,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(68:74,1)),Mix(68:74,2),'LineStyle','none','Marker','d','MarkerFaceColor',[0,0,0],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(68:74,1)),Mix(68:74,3),'LineStyle','none','Marker','s','MarkerFaceColor',[1,1,1],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
legend('Mix','Numeraire','Extreme');
xlabel('Log Relative Price of Numeraire');
ylabel('Fraction of Subjects');
ylim([0 1]);
saveas(gcf,'N6Mix.png');
hold off
close

%Figure 18 (f) 
hold on 
plot(log(vprice(75:78,1)),Mix(75:78,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(75:78,1)),Mix(75:78,2),'LineStyle','none','Marker','d','MarkerFaceColor',[0,0,0],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
plot(log(vprice(75:78,1)),Mix(75:78,3),'LineStyle','none','Marker','s','MarkerFaceColor',[1,1,1],'MarkerEdgeColor',[0,0,0],'MarkerSize',7);
legend('Mix','Numeraire','Extreme');
xlabel('Log Relative Price of Numeraire');
ylabel('Fraction of Subjects');
ylim([0 1]);
saveas(gcf,'N7Mix.png');
hold off
close

%% Table 3 and Correlation results with repeated discrete choices for Section 4.2

%Generate column vector for D1 convex task
D1conv=convDC(1:2:288,1);

%Generate column vector for D2 convex task
D2conv=convDC(2:2:288,1);

%Correlation coefficient tests for D1 and D2
[corrD1,pD1]=corr(D1dis,D1conv);
[corrD2,pD2]=corr(D2dis,D2conv);

%Spearmann test for D1 and D2
[ScorrD1,SpD1]=corr(D1dis,D1conv,'Type','Spearman');
[ScorrD2,SpD2]=corr(D2dis,D2conv,'Type','Spearman');

%Find the correlation while leaving out the (1,1) findings
 counter1=1;
 counter2=1;
for n=1:N
    if ~isequal(D1dis(n),D1conv(n),100)
        D1disSub(counter1)=D1dis(n);
        D1convSub(counter1)=D1conv(n);
        counter1=counter1+1;
    end
    
    if ~isequal(D2dis(n),D2conv(n),100)
        D2disSub(counter2)=D1dis(n);
        D2convSub(counter2)=D1conv(n);
        counter2=counter2+1;
    end   
end

%Correlation coefficient tests for D1 and D2 on subset
[corrD1Sub,pD1Sub]=corr(D1disSub',D1convSub');
[corrD2Sub,pD2Sub]=corr(D2disSub',D2convSub');

%Spearmann test for D1 and D2 on subset
[ScorrD1Sub,SpD1Sub]=corr(D1disSub',D1convSub','Type','Spearman');
[ScorrD2Sub,SpD2Sub]=corr(D2disSub',D2convSub','Type','Spearman');

%Compare the convex vs DC choices
COMP=abs(DC(:,3)-convDC);

%Compute distance of choices in the two domains
NumDiffBudgets=[ sum(COMP(1:2:288)==0) sum(COMP(1:2:288)<=(50/3)) sum(COMP(1:2:288)<=(101/3)) sum(COMP(1:2:288)<=(200/3)) ;
                sum(COMP(2:2:288)==0) sum(COMP(2:2:288)<=(50/3)) sum(COMP(2:2:288)<=(101/3)) sum(COMP(2:2:288)<=(200/3)) ];            

% Table 3 Generation
input.data=NumDiffBudgets;
input.tablePositioning='h';
input.tableColLabels = {'0','<=16.6','<=33.3','<=66.6'};
input.tableRowLabels = {'D_1','D_2'};
input.dataFormatMode = 'row';
input.dataFormat = {'%.0f'};
input.tableColumnAlignment = 'c';
DifBudget = latexTable(input);
save('NumDifBudget_tab', 'DifBudget')
clear input 

%% Figure 11 and Aggregate Demand Plots with Log Prices for Appendix F and Table 7 generation 

%Figure 19 (a)
hold on
plot(vprice(1:9,1),AggDemand(1:9,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7)
xlabel('Relative Price of Numeraire')
ylabel('Average Demand for Numeraire (\alpha)')
ylim([0 100]);
saveas(gcf,'AggDemN1.png');
hold off 
close

%Figure 11 
hold on
plot(vprice(10:28),AggDemand(10:28,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7)
xlabel('Relative Price of Numeraire')
ylabel('Average Demand for Numeraire (\alpha)')
ylim([0 100]);
saveas(gcf,'AggDemN2.png');
hold off 
close

%Figure 19 (b)
hold on
plot(vprice(29:44,1),AggDemand(29:44,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7)
xlabel('Relative Price of Numeraire')
ylabel('Average Demand for Numeraire (\alpha)')
ylim([0 100]);
saveas(gcf,'AggDemN3.png');
hold off 
close

%Figure 19 (c)
hold on
plot(vprice(45:57,1),AggDemand(45:57,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7)
xlabel('Relative Price of Numeraire')
ylabel('Average Demand for Numeraire (\alpha)')
ylim([0 100]);
saveas(gcf,'AggDemN4.png');
hold off 
close

%Figure 19 (d)
hold on
plot(vprice(58:67,1),AggDemand(58:67,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7)
xlabel('Relative Price of Numeraire')
ylabel('Average Demand for Numeraire (\alpha)')
ylim([0 100]);
saveas(gcf,'AggDemN5.png');
hold off 
close

%Figure 19 (e)
hold on
plot(vprice(68:74,1),AggDemand(68:74,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7)
xlabel('Relative Price of Numeraire')
ylabel('Average Demand for Numeraire (\alpha)')
ylim([0 100]);
saveas(gcf,'AggDemN6.png');
hold off 
close

%Figure 19 (f)
hold on
plot(vprice(75:78,1),AggDemand(75:78,1),'LineStyle','none','Marker','o','MarkerFaceColor',[.7,.7,.7],'MarkerEdgeColor',[0,0,0],'MarkerSize',7)
xlabel('Relative Price of Numeraire')
ylabel('Average Demand for Numeraire (\alpha)')
ylim([0 100]);
saveas(gcf,'AggDemN7.png');
hold off 
close

%Table of log-lin fit of fundctions
mdLR1=LinearModel.fit(log(price(taskN1)),D(taskN1,4));
mdLR2=LinearModel.fit(log(price(taskN2)),D(taskN2,4));
mdLR3=LinearModel.fit(log(price(taskN3)),D(taskN3,4));
mdLR4=LinearModel.fit(log(price(taskN4)),D(taskN4,4));
mdLR5=LinearModel.fit(log(price(taskN5)),D(taskN5,4));
mdLR6=LinearModel.fit(log(price(taskN6)),D(taskN6,4));
mdLR7=LinearModel.fit(log(price(taskN7)),D(taskN7,4));

%Combine relevant information from regressions into a table 
RegTab=[mdLR1.Coefficients{1,1:2}' mdLR2.Coefficients{1,1:2}' mdLR3.Coefficients{1,1:2}' mdLR4.Coefficients{1,1:2}' mdLR5.Coefficients{1,1:2}' mdLR6.Coefficients{1,1:2}' mdLR7.Coefficients{1,1:2}' ; ...
        mdLR1.Coefficients{2,1:2}' mdLR2.Coefficients{2,1:2}' mdLR3.Coefficients{2,1:2}' mdLR4.Coefficients{2,1:2}' mdLR5.Coefficients{2,1:2}' mdLR6.Coefficients{2,1:2}' mdLR7.Coefficients{2,1:2}' ; ...
        mdLR1.Rsquared.Ordinary mdLR2.Rsquared.Ordinary mdLR3.Rsquared.Ordinary mdLR4.Rsquared.Ordinary mdLR5.Rsquared.Ordinary mdLR6.Rsquared.Ordinary mdLR7.Rsquared.Ordinary; ...
        mdLR1.Rsquared.Adjusted mdLR2.Rsquared.Adjusted mdLR3.Rsquared.Adjusted mdLR4.Rsquared.Adjusted mdLR5.Rsquared.Adjusted mdLR6.Rsquared.Adjusted mdLR7.Rsquared.Adjusted; ...
        mdLR1.NumObservations mdLR2.NumObservations mdLR3.NumObservations mdLR4.NumObservations mdLR5.NumObservations mdLR6.NumObservations mdLR7.NumObservations];

%Table 7 Generation 
input.data=RegTab;
input.tablePositioning='h';
input.tableColLabels = {'N1','N2','N3','N4','N5','N6','N7'};
input.tableRowLabels = {'Intercept','','log(r)','','$R^2$','Adjusted $R^2$','Num Obs'};
input.dataFormatMode = 'row';
input.dataFormat = {'%.2f'};
input.tableColumnAlignment = 'c';
RegTab = latexTable(input);
save('Reg_tab', 'RegTab')
clear input 

%% Figure 10 Density Plots of HMI vs Benchmarks, Appendix C figures, and relevant statistics

%Get the percentage of people who mix and are closer than random behavior
load('HMI8.mat') %Holds different benchmark behavior
load('TestResMain8.mat') %Holds population of subject behavior

%Figure 10 (b)
hold on
axis( [0 79 0 1] )
axis 'auto y' 
histogram(HMI(:,1), 'Normalization', 'probability','Binwidth',5,'FaceColor',[.1,.1,.1],'EdgeColor',[0,0,0],'FaceAlpha',.4)
histogram(TestResMain, 'Normalization', 'probability','Binwidth',5,'FaceColor',[.8,.8,.8],'EdgeColor',[0,0,0],'FaceAlpha',.8)
xlabel('Number of Consistent Choices');
ylabel('Density');
legend({'Benchmark Noise', 'Subjects'}, 'Location','northwest');
saveas(gcf,'HMIHistNB.png');
hold off
close

%Figure 10(a)
hold on
axis( [0 79 0 1] )
axis 'auto y' 
histogram(HMI(:,2), 'Normalization', 'probability','Binwidth',5,'FaceColor',[.1,.1,.1],'EdgeColor',[0,0,0],'FaceAlpha',.4)
histogram(TestResMain, 'Normalization', 'probability','Binwidth',5,'FaceColor',[.8,.8,.8],'EdgeColor',[0,0,0],'FaceAlpha',.8)
xlabel('Number of Consistent Choices');
ylabel('Density');
legend('Benchmark Demand Effects', 'Subjects','Location','northwest');
saveas(gcf,'HMIHistEDB.png');
hold off
close

%Figure 17 (a)
hold on
axis( [0 79 0 1] )
axis 'auto y' 
histogram(HMI(:,3), 'Normalization', 'probability','Binwidth',5,'FaceColor',[.1,.1,.1],'EdgeColor',[0,0,0],'FaceAlpha',.4)
histogram(TestResMain, 'Normalization', 'probability','Binwidth',5,'FaceColor',[.8,.8,.8],'EdgeColor',[0,0,0],'FaceAlpha',.8)
xlabel('Number of Consistent Choices');
ylabel('Density');
legend('Benchmark Bootstrap', 'Subjects','Location','northwest');
saveas(gcf,'HMIHistA1.png');
hold off
close

%Figure 17 (b) 
hold on
axis( [0 79 0 1] )
axis 'auto y' 
histogram(HMI(:,4), 'Normalization', 'probability','Binwidth',5,'FaceColor',[.1,.1,.1],'EdgeColor',[0,0,0],'FaceAlpha',.4)
histogram(TestResMain, 'Normalization', 'probability','Binwidth',5,'FaceColor',[.8,.8,.8],'EdgeColor',[0,0,0],'FaceAlpha',.8)
xlabel('Number of Consistent Choices');
ylabel('Density');
legend('Benchmark Conditional Bootstrap', 'Subjects','Location','northwest');
saveas(gcf,'HMIHistA2.png');
hold off
close

%Calculate number of individuals greater than the 95th percentile
NinetyFifthNB=sum(TestResMain>=quantile(HMI(:,1),.95));
NinetyFifthEDB=sum(TestResMain>=quantile(HMI(:,2),.95));
NinetyFifthA1=sum(TestResMain>=quantile(HMI(:,3),.95));
NinetyFifthA2=sum(TestResMain>=quantile(HMI(:,4),.95));

%Calculate Medians of the distributions
medSub=median(TestResMain(:,1));
medNB=median(HMI(:,1));
medEDB=median(HMI(:,2));
medA1=median(HMI(:,3));
medA2=median(HMI(:,4));

%Distributional Tests
%Kolmogorov Smirnoff
[KSNB,KSpNB]=kstest2(TestResMain,HMI(:,1));
[KSEDB,KSpEDB]=kstest2(TestResMain,HMI(:,2));
[KSA1,KSpA1]=kstest2(TestResMain,HMI(:,3));
[KSA2,KSpA2]=kstest2(TestResMain,HMI(:,4));

%Wilcoxon Rank Sum
WpNB=ranksum(TestResMain,HMI(:,1));
WpEDB=ranksum(TestResMain,HMI(:,2));
WpA1=ranksum(TestResMain,HMI(:,3));
WpA2=ranksum(TestResMain,HMI(:,4));

%% Check the Number of Individuals consistent with expected utility

%Define the best EU distance using the fEU function
EU=zeros(N,1);
EU1=zeros(N,1);
EU4=zeros(N,1);

%Expected utility comparisons (Only need to look at
%these since observational equivalence at other values)
for n=1:N
    un=79*(n-1);    %counter for individuals
    for r=1:length(nslopes2)
        %Expected utility predictions for a given slope
            dist=fEU(nslopes2(r),[rpNumData(1:T,10:12) rpNumData(1:T,7:9)],rpNumData(un+1:un+T,4));
            %Turns the EU indicator to 1 if there is a consistent EU
            if sum( dist == 0 ) == T
                EU(n)=1;
            end
            if sum( dist <= 1 ) == T
                EU1(n)=1;
            end    
            if sum( dist <=4 ) == T
                EU4(n)=1;
            end
    end
end
