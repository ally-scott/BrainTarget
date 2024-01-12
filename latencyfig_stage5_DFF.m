%% load perievent tidytables
clear all
close all
clc 

addpath('G:\Shared drives\Richard Lab\Papers and Abstracts\Papers in Progress\Alexandra Scott\GADVPFP\Figs\FP-analysis-variableReward\FP_analysis\FP-analysis\matlabVPFP\broken up code\')
cd('G:\Shared drives\Richard Lab\Papers and Abstracts\Papers in Progress\Alexandra Scott\GADVPFP\Figs\fig4\DFF');
GADVPFPperiEventTable=readtable('GADVPFP_dffperiEventTable.csv');
GADVPFPControlperiEventTable=readtable('GADVPFPControlperiEventTable');


%% make table for running correlation
% table will include data from day animal reach criteria, avg of signal z
% score of all trials from that day, from that animal

%ONLY WANT signal from -2 sec before to time of PE or by 5 sec


%select stage5 data

correxptab=GADVPFPperiEventTable(GADVPFPperiEventTable.stage==5,:);
corrcontab=GADVPFPControlperiEventTable(GADVPFPControlperiEventTable.stage==5,:);

subj=unique(correxptab.subject);
rho=[];
pval=[];
DSblue={};
DSpeLat={};
DSbluebinary={};

rhopurp=[];
pvalpurp=[];
DSpurp={};
DSpeLatpurp={};
DSpurpbinary={};


%% Add Shuffled Data to set (shuffled latencies per trial)
  %-- DS trials
  
  %initialize col
correxptab(:,"Shuffled")= table(nan);
  
  %TODO: just shuffle latencies between trials
data= correxptab;

allTrials= unique(data.DStrialIDcum);
allTrials= allTrials(~isnan(allTrials));


trialShuffletemp= [];
for i=1:20
trialShuffletemp(:,i)= allTrials(randperm(numel(allTrials)));
end

lattemp=[];
lat=[];


for trial= 1:numel(allTrials)
for i=1:size(trialShuffletemp,2)
    %for each trial get corresponding shuffled trial's latency and add in
    %new col
    
    ind= []; 
%     ind= ismember(data.DStrialIDcum, allTrials(trial)); %16256 calls, 32s
    ind= find(data.DStrialIDcum== allTrials(trial));

    
    ind2= [];
%     ind2= ismember(data.DStrialIDcum, trialShuffle(trial));
    ind2= find(data.DStrialIDcum== trialShuffletemp(trial,i));
    
    % rather slow, idk if it is the looping or the assignment
%     data(ind,'poxDSrelShuffled')= data(ind2(1), 'poxDSrel');  
    
%      correxptab(ind,'Shuffled')= data(ind2(1), 'DSpeLatency10sec');  
   lat=data(ind(1), 'DSpeLatency10sec');
   lattemp(trial,i)=lat.DSpeLatency10sec;
   
 
end
end

%% run correlation for every rand perm

DSblueshuff={};
DSpeLatshuff={};
DSbluebinaryshuff={};

for i=1:length(subj)  
for x=1:size(trialShuffletemp,2)%every rand perm
test=correxptab(ismember(correxptab.subject,subj{i}),:);
timelock5sec=unique(test.timeLock(test.timeLock<=5));
trialsshuff=unique(test.DStrialIDcum);
DSblueshuff{i}(:,:)=nan(length(trialsshuff),length(timelock5sec));
DSpeLatshuff{i}(:,:)=nan(length(trialsshuff),length(timelock5sec));
DSbluebinaryshuff{i}(:,:)=nan(length(trialsshuff),length(timelock5sec));
for trial=1:length(trialsshuff)
 trialind=find(test.DStrialIDcum==trialsshuff(trial));
 trialind=trialind(1:length(timelock5sec));
%  trialind=find(test.timeLock(trialind)<=5);
%  trialind=(trialind==1);

 sufftrials= trialShuffletemp(:,x); 
 shufflats=lattemp(:,x);
 latval= shufflats(find(trialsshuff(trial)==sufftrials));
 diffs=timelock5sec-latval;
 [lat,latind]=min(abs(diffs));% closest value in timelock to time of PE
if latval<=5 
for time=1:latind
    if ~isnan(test.DSblue(trialind(time))) && ~isnan(latval)
DSblueshuff{i}(trial,time)=test.DSblue(trialind(time));
DSpeLatshuff{i}(trial,time)=latval;
    end
end
elseif ~isnan(latval)
    
for time=1:length(timelock5sec)
     if ~isnan(test.DSblue(trialind(time))) && ~isnan(latval)
DSblueshuff{i}(trial,time)=test.DSblue(trialind(time));
DSpeLatshuff{i}(trial,time)=latval;
end
end
end
end
% exclude data where only 4 trials left
DSbluebinaryshuff{i}(:,:)=~isnan(DSblueshuff{i}(:,:));
DSpeLatminshuff(:,i)=min(min(DSpeLatshuff{i}(:,:)));
for timeStamp= 1:size(DSblueshuff{i}(:,:),2)
if sum(DSbluebinaryshuff{i}(:,timeStamp))<5
    DSblueshuff{i}(:,timeStamp)=nan(length(DSbluebinaryshuff{i}(:,timeStamp)),1);
    DSpeLatshuff{i}(:,timeStamp)=nan(length(DSbluebinaryshuff{i}(:,timeStamp)),1);
 end
end
% run correlation
for timeStamp= 1:size(DSblueshuff{i}(:,:),2)
 [rhoshuff(i,timeStamp,x),pvalshuff(i,timeStamp,x)]= corr(DSblueshuff{i}(:,timeStamp),DSpeLatshuff{i}(:,timeStamp), 'Rows', 'Complete');
 end

end
end

rhoshuffavg=nanmean(rhoshuff,3);

% for trial= 1:numel(allTrials)
%       %for each trial get corresponding shuffled trial's latency and add in
%     %new col
%     
%     ind= []; 
% %     ind= ismember(data.DStrialIDcum, allTrials(trial)); %16256 calls, 32s
%     ind= find(data.DStrialIDcum== allTrials(trial));
% 
%     
%     ind2= [];
% %     ind2= ismember(data.DStrialIDcum, trialShuffle(trial));
%     ind2= find(trialShuffletemp(trial,1)== data.DStrialIDcum);
%     
% 
%   correxptab(ind,'Shuffled')=table(repmat(trialShuffle(trial),[length(ind) 1]));  
% end

%% organize data and run correlation for data and shuffled data
for i=1:length(subj)   
test=correxptab(ismember(correxptab.subject,subj{i}),:);
timelock5sec=unique(test.timeLock(test.timeLock<=5));
trials=unique(test.DStrialIDcum);
DSblue{i}(:,:)=nan(length(trials),length(timelock5sec));
DSpeLat{i}(:,:)=nan(length(trials),length(timelock5sec));
DSbluebinary{i}(:,:)=nan(length(trials),length(timelock5sec));


DSpurp{i}(:,:)=nan(length(trials),length(timelock5sec));
DSpeLatpurp{i}(:,:)=nan(length(trials),length(timelock5sec));
DSpurpbinary{i}(:,:)=nan(length(trials),length(timelock5sec));


for trial=1:length(trials)
 trialind=find(test.DStrialIDcum==trials(trial));
 trialind=trialind(1:length(timelock5sec));
%  trialind=find(test.timeLock(trialind)<=5);
%  trialind=(trialind==1);
 latval= mean(test.DSpeLatency10sec(trialind));
 diffs=timelock5sec-latval;
 [lat,latind]=min(abs(diffs));% closest value in timelock to time of PE
if latval<=5 % need to change this so no taking out trials
for time=1:latind
    if ~isnan(test.DSblue(trialind(time))) && ~isnan(test.DSpeLatency10sec(trialind(time)))
DSblue{i}(trial,time)=test.DSblue(trialind(time));
DSpeLat{i}(trial,time)=test.DSpeLatency10sec(trialind(time));

DSpurp{i}(trial,time)=test.DSpurple(trialind(time));
DSpeLatpurp{i}(trial,time)=test.DSpeLatency10sec(trialind(time));
    end
end
elseif ~isnan(latval)
    
for time=1:length(timelock5sec)
     if ~isnan(test.DSblue(trialind(time))) && ~isnan(test.DSpeLatency10sec(trialind(time)))
DSblue{i}(trial,time)=test.DSblue(trialind(time));
DSpeLat{i}(trial,time)=test.DSpeLatency10sec(trialind(time));

DSpurp{i}(trial,time)=test.DSpurple(trialind(time));
DSpeLatpurp{i}(trial,time)=test.DSpeLatency10sec(trialind(time));
end
end
end
end
% exclude data where only 4 trials left
DSbluebinary{i}(:,:)=~isnan(DSblue{i}(:,:));
DSpeLatmin(:,i)=min(min(DSpeLat{i}(:,:)));
for timeStamp= 1:size(DSblue{i}(:,:),2)
if sum(DSbluebinary{i}(:,timeStamp))<5
    DSblue{i}(:,timeStamp)=nan(length(DSbluebinary{i}(:,timeStamp)),1);
    DSpeLat{i}(:,timeStamp)=nan(length(DSbluebinary{i}(:,timeStamp)),1);
    
    DSpurp{i}(:,timeStamp)=nan(length(DSbluebinary{i}(:,timeStamp)),1);
    DSpeLatpurp{i}(:,timeStamp)=nan(length(DSbluebinary{i}(:,timeStamp)),1);
    
 end
end
% run correlation
for timeStamp= 1:size(DSblue{i}(:,:),2)
 [rho(i,timeStamp),pval(i,timeStamp)]= corr(DSblue{i}(:,timeStamp),DSpeLat{i}(:,timeStamp), 'Rows', 'Complete');
 [rhopurp(i,timeStamp),pvalpurp(i,timeStamp)]= corr(DSpurp{i}(:,timeStamp),DSpeLatpurp{i}(:,timeStamp), 'Rows', 'Complete');
 end

end

%% plot stuff
%unshuffled
figure()
g=gramm('x',timelock5sec','y',rho,'subset',~strcmp(subj','rat8')& ~strcmp(subj','rat3'));
g.stat_summary('type','sem','geom','area')
g.draw()

figure()
g=gramm('x',timelock5sec','y',rho,'color',subj,'subset',~strcmp(subj','rat8')& ~strcmp(subj','rat3'));
g.geom_line()
g.draw()

% %405 
% 
% figure()
% g=gramm('x',timelock5sec','y',rhopurp,'subset',~strcmp(subj','rat8')& ~strcmp(subj','rat3'));
% g.stat_summary('type','sem','geom','area')
% g.draw()
% 
% figure()
% g=gramm('x',timelock5sec','y',rhopurp,'color',subj,'subset',~strcmp(subj','rat8')& ~strcmp(subj','rat3'));
% g.geom_line()
% g.draw()

%suffled
figure()
g=gramm('x',timelock5sec','y',rhoshuffavg,'subset',~strcmp(subj','rat8')& ~strcmp(subj','rat3'));
g.stat_summary('type','sem','geom','area')
g.draw()

figure()
g=gramm('x',timelock5sec','y',rhoshuffavg,'color',subj,'subset',~strcmp(subj','rat8')& ~strcmp(subj','rat3'));
g.geom_line()
g.draw()

% figure()
% g=gramm('x',timelock5sec','y',pval,'subset',~strcmp(subj','rat8')& ~strcmp(subj','rat3'));
% g.stat_summary('type','sem','geom','area')
% g.draw()
% 
% sigtest=pval<=0.05;
% sigtestresults=find(sigtest==1);
timeLockforrho=repmat(timelock5sec',[size(rho,1) 1]);
subjforrho=repmat(subj,[1 size(rho,2)]);
% figure()
% g=gramm('x',timeLockforrho (sigtestresults),'y',rho (sigtestresults),'color',subjforrho(sigtestresults),'subset',~strcmp(subjforrho(sigtestresults)','rat8')& ~strcmp(subjforrho(sigtestresults)','rat3'));
% g.stat_summary('type','sem','geom','area')
% g.draw()

%% table for R analysis
corrtab=table();

rho=rho';
rho=rho(:)';

% rhopurp=rhopurp';
% rhopurp=rhopurp(:)';

rhoshuffavg=rhoshuffavg';
rhoshuffavg=rhoshuffavg(:)';

subjforrho=subjforrho';
subjforrho=subjforrho(:)';

timeforrho=timeLockforrho';
timeforrho=timeforrho(:)';

corrtab.rho=cat(1,rho',rhoshuffavg');
corrtab.cat=cat(1,repelem({'unshuff'},size(rho,2))',repelem({'shuff'},size(rho,2))');
corrtab.subj=cat(1,subjforrho',subjforrho');
corrtab.time=cat(1,timeforrho',timeforrho');

writetable(corrtab,'PElatencycorrelationtableforanalysis_DFF.csv');

%% calculate rho means for animals

rhoavg=table();
rhoavg= groupsummary(corrtab,{'cat','subj','time'},'mean',{'rho'});
rhoavg=rhoavg(rhoavg.time<=5 & rhoavg.time>=0,:);

%% table with time bins in 0.1 sec
x=1;
ma=table();
subjects=unique(rhoavg.subj);
cons=unique(rhoavg.cat);
for t=1:length(rhoavg.mean_rho)/201
    if x==1
        b=0;
    else
        b=1;
       
    end
 ratevent= x:t*(length(rhoavg.mean_rho)/(length(subjects)*length(cons)));
%regression coefficients
 ma.rho_bins(((t-1)*50)+1:(t*50))= nanmean(reshape(rhoavg.mean_rho(x+b:ratevent(end)-1),[],50,1));
% % metadata

% time
 ma.bin(((t-1)*50)+1:(t*50))= nanmean(reshape(rhoavg.time(x+b:ratevent(end)-1),[],50,1));
 
 % rat
 binrat=(reshape(rhoavg.subj(x+b:ratevent(end)-1),[],50,1));
 ma.rat(((t-1)*50)+1:(t*50))= binrat(1,:);
% eventtype
 binevent=(reshape(rhoavg.cat(x+b:ratevent(end)-1),[],50,1));
 ma.event(((t-1)*50)+1:(t*50))= binevent(1,:);
% sex and led addded in R




x=t*((length(rhoavg.mean_rho)/(length(subjects)*length(cons))));

end


writetable(ma,'PElatencycorrelationtableforanalysis_DFF_bins.csv');
