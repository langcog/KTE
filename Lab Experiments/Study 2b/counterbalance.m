function [presentSet, trialCount]=counterbalance(subID)
rand('twister', subID);
numCond=8; 
numTrials=5;
% conditions = [1:numCond]
design=[1	2	8	3	7	4	6	5;
    2	3	1	4	8	5	7	6;
    3	4	2	5	1	6	8	7;
    4	5	3	6	2	7	1	8;
    5	6	4	7	3	8	2	1;
    6	7	5	8	4	1	3	2;
    7	8	6	1	5	2	4	3;
    8	1	7	2	6	3	5	4];
i=subID;
m=mod(i,8);
if m==0
    partSet=[design(:,8), design(:,1:4)]
elseif m<=4
    partSet=[design(:,(m:(m+4)))]     
else
    partSet=[design(:,(m:(m+8-m))), design(:,(1:m-4))]
end
s=Shuffle(partSet);
presentSet=[];
for i=1:size(s, 2)
    presentSet=[presentSet; s(:,i)];
end
for i=1:length(presentSet)
    if i<=numCond*1
        trialCount(i)=1;
    elseif i<=numCond*2
        trialCount(i)=2;
    elseif i<=numCond*3
        trialCount(i)=3;
    else
        trialCount(i)=4;
    end
end