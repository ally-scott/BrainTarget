clear all
close all
clc 
addpath('G:\Shared drives\Richard Lab\Papers and Abstracts\Papers in Progress\Alexandra Scott\GADVPFP\Figs\FP-analysis-variableReward\FP_analysis\FP-analysis\matlabVPFP\broken up code\');
GADVPFPperiEventTable=readtable('GADVPFPControl_dffperiEventTable.csv');
expname='GADVPFPControl_dff';
figureCount=1;

%% Data to run AUC

auctable=table();


%select data 
selection=  ~strcmp(GADVPFPperiEventTable.subject,'rat3')&(GADVPFPperiEventTable.stage<=5);
selection=find(selection==1);
auctable=GADVPFPperiEventTable(selection,:);

aucavgt=table();
aucavgt= groupsummary(auctable,{'sex','relday','stageday','stage','subject','timeLock'},'mean',{'DSblue','DSpurple','NSblue','NSpurple'});


%stage1-4,0-5sec
stage1to4tab=table();
stage1to4tab=aucavgt(aucavgt.stage<=4,:);
stage1to4tabauc=table();
stage1to4tabauc=stage1to4tab(stage1to4tab.timeLock<=5 & stage1to4tab.timeLock>=0,:);

%calculate AUC
for ratdaymean=1:(length(stage1to4tabauc.timeLock)/201)
    if ratdaymean==1
stage1to4tabauc.DSblueaucvalues(1:ratdaymean*201)= trapz(stage1to4tabauc.timeLock((1:ratdaymean*201)), stage1to4tabauc.mean_DSblue(~isnan(1:ratdaymean*201)));
stage1to4tabauc.DSpurpleaucvalues(1:ratdaymean*201)= trapz(stage1to4tabauc.timeLock((1:ratdaymean*201)), stage1to4tabauc.mean_DSpurple(~isnan(1:ratdaymean*201)));

    
    else
stage1to4tabauc.DSblueaucvalues((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201))= trapz(stage1to4tabauc.timeLock((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201)), stage1to4tabauc.mean_DSblue((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201)));       
stage1to4tabauc.DSpurpleaucvalues((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201))= trapz(stage1to4tabauc.timeLock((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201)), stage1to4tabauc.mean_DSpurple((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201)));           
    end
end


    
%plot auc over trainday

stage1to4plottab= groupsummary(stage1to4tabauc,{'sex','relday','stageday','stage','subject'},'mean',{'DSblueaucvalues','DSpurpleaucvalues'});

stage1to4plottab=stage1to4plottab(~isnan(stage1to4plottab.mean_DSblueaucvalues)& ~isnan(stage1to4plottab.mean_DSpurpleaucvalues),:);



writetable(stage1to4plottab,strcat(expname,'stage1to4plottab.csv'))

figure()
g=gramm('x',stage1to4plottab.relday,'y',stage1to4plottab.mean_DSblueaucvalues);
g.stat_summary('type','sem','geom','area')
g.draw()
% g.update('y',stage1to4plottab.mean_DSpurpleaucvalues);
% g.stat_summary('type','sem','geom','area')
% g.set_color_options('map','brewer_pastel')
% g.axe_property('YLim',[-10 15])
% g.draw()

% figure()
% g=gramm('x',stage1to4plottab.relday,'y',stage1to4plottab.mean_DSblueaucvalues);
% g.facet_wrap(stage1to4plottab.stage)
% g.stat_summary('type','sem','geom','area')
% g.draw()
% g.update('y',stage1to4plottab.mean_DSpurpleaucvalues);
% g.stat_summary('type','sem','geom','area')
% g.set_color_options('map','brewer_pastel')
% g.axe_property('YLim',[-5 15])
% g.draw()







%stage5
stage5tab=table();
stage5tab=aucavgt(aucavgt.stage==5,:);
stage5tabauc=table();
stage5tabauc=stage5tab(stage5tab.timeLock<=5 & stage5tab.timeLock>=0,:);

%calculate AUC
for ratdaymean=1:(length(stage5tabauc.timeLock)/201)
    if ratdaymean==1
stage5tabauc.DSblueaucvalues(1:ratdaymean*201)= trapz(stage5tabauc.timeLock((1:ratdaymean*201)), stage5tabauc.mean_DSblue(~isnan(1:ratdaymean*201)));
stage5tabauc.DSpurpleaucvalues(1:ratdaymean*201)= trapz(stage5tabauc.timeLock((1:ratdaymean*201)), stage5tabauc.mean_DSpurple(~isnan(1:ratdaymean*201)));
stage5tabauc.NSblueaucvalues(1:ratdaymean*201)= trapz(stage5tabauc.timeLock((1:ratdaymean*201)), stage5tabauc.mean_NSblue(~isnan(1:ratdaymean*201)));
stage5tabauc.NSpurpleaucvalues(1:ratdaymean*201)= trapz(stage5tabauc.timeLock((1:ratdaymean*201)), stage5tabauc.mean_NSpurple(~isnan(1:ratdaymean*201)));
    else
stage5tabauc.DSblueaucvalues((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201))= trapz(stage5tabauc.timeLock((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201)), stage5tabauc.mean_DSblue((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201)));   
stage5tabauc.DSpurpleaucvalues((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201))= trapz(stage5tabauc.timeLock((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201)), stage5tabauc.mean_DSpurple((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201))); 
stage5tabauc.NSblueaucvalues((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201))= trapz(stage5tabauc.timeLock((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201)), stage5tabauc.mean_NSblue((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201)));   
stage5tabauc.NSpurpleaucvalues((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201))= trapz(stage5tabauc.timeLock((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201)), stage5tabauc.mean_NSpurple((((ratdaymean-1)*201)+1):(((ratdaymean-1)*201)+201))); 
    end
end


%plot auc over trainday

stage5plottab= groupsummary(stage5tabauc,{'sex','relday','stage','stageday','subject'},'mean',{'DSblueaucvalues','DSpurpleaucvalues','NSblueaucvalues','NSpurpleaucvalues'});

stage5plottab=stage5plottab(~isnan(stage5plottab.mean_DSblueaucvalues) & ~isnan(stage5plottab.mean_DSpurpleaucvalues)& ~isnan(stage5plottab.mean_NSblueaucvalues)&~isnan(stage5plottab.mean_NSpurpleaucvalues),:);
rat= unique(stage5plottab.subject);

%label last 10 days of stage 5
for subj=1:size(rat,1)
    for ratID= rat(subj)
        ratdata=stage5plottab(strcmp(stage5plottab.subject,ratID),:);
        if size(ratdata.stageday,1)>10
            last10days=nan(size(ratdata.stageday,1),1);
            last10days(ratdata.stageday(end-10+1:end))=1:10;
            stage5plottab.last10days(strcmp(stage5plottab.subject,ratID))= last10days;
        else
            stage5plottab.last10days(strcmp(stage5plottab.subject,ratID))= min(ratdata.stageday):max(ratdata.stageday);
        end    
    end
end

writetable(stage5plottab,strcat(expname,'stage5plottab.csv'))

%%
figure()
g=gramm('x',stage5plottab.last10days,'y',stage5plottab.mean_DSblueaucvalues,'subset',~isnan(stage5plottab.last10days));
g.stat_summary('type','sem','geom','area')
g.draw()
g.update('y',stage5plottab.mean_NSblueaucvalues,'subset',~isnan(stage5plottab.last10days));
g.stat_summary('type','sem','geom','area')
g.set_line_options('style',{'--'})
g.axe_property('YLim',[-1 15])
g.draw()
% g.update('y',stage5plottab.mean_DSpurpleaucvalues,'subset',~isnan(stage5plottab.last10days));
% g.stat_summary('type','sem','geom','area')
% g.set_color_options('map','brewer_pastel')
% g.set_line_options('style',{'-'})
% g.draw()
% g.update('y',stage5plottab.mean_NSpurpleaucvalues,'subset',~isnan(stage5plottab.last10days));
% g.stat_summary('type','sem','geom','area')
% g.set_color_options('map','brewer_pastel')
% g.set_line_options('style',{'--'})
% g.axe_property('YLim',[-10 15])
% g.draw()




