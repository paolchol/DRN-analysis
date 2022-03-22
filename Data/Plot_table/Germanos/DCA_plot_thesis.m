%% Plotagem
clc
clear all
%% Files
% drought_event - a struct variable with the basic information of the analyzed drought events 
% %% Input 
% load('Ciclo_Seca')
% % vd_r017_nt - Volume Defict of the scenario without small reservoirs of the analyzed reservoir for the whole study period
% % vd_r017_ob - Volume Defict of the scenario with small reservoirs of the analyzed reservoir for the whole study period
% % CS - Structured variable that shows, for each drought event, in which quadrant of the DCA the pair of variables (SPI and VD) was in each month.
% % data_reserv_completa - date variable (in the format Y, M ,D)
 
load('AL_data.mat')

%% Scale the VD
%The VD is automatically scaled

%a = -2.7;
%b = 2.7;
%spi = (b-a)*(spi - min(spi))/(max(spi) - min(spi)) + a;
%vd_r017_nt = (b-a)*(vd_r017_nt - min(vd_r017_nt))/(max(vd_r017_nt) - min(vd_r017_nt)) + a;
%vd_r017_ob = (b-a)*(vd_r017_ob - min(vd_r017_ob))/(max(vd_r017_ob) - min(vd_r017_ob)) + a;

%% Drought Event 
drought_event{1,1}=[datenum([1992 01 1]) datenum([1994 08 01])];  % First and last date of each drought event
drought_event{2,1}=[datenum([1997 01 1]) datenum([2002 06 01])];  % First and last date of each drought event
drought_event{3,1}=[datenum([2010 01 1]) datenum([2018 12 01])];  % First and last date of each drought event
%% Selected dates of each drouhgt event to be ploted in the DCA with the marks (circles and stars that appear in the chart)
% First drought event
drought_event{1,2}(1,:)=[1992 04 01]; 
drought_event{1,2}(2,:)=[1993 05 01];
drought_event{1,2}(3,:)=[1994 08 01];
% Second drought event
drought_event{2,2}(1,:)=[1997 05 01];
drought_event{2,2}(2,:)=[1998 03 01];
drought_event{2,2}(3,:)=[1999 05 01];
drought_event{2,2}(4,:)=[2001 04 01];
drought_event{2,2}(5,:)=[2002 05 01];
% Third drought event 
drought_event{3,2}(1,:)=[2010 12 01];
drought_event{3,2}(2,:)=[2011 11 01];
drought_event{3,2}(3,:)=[2014 04 01];
drought_event{3,2}(4,:)=[2015 09 01];
drought_event{3,2}(5,:)=[2018 05 01];
%% Quadrant Color (Main RGB color of each Quadrant)
cq1=[0 1 1];
cq2=[1 1 0.0];
cq3=[1 0 0.0];
cq4=[0.5 0.0 0.5];
%% DCA horizontal bar chart
lr=8; % width of each rec that compose the bar chart
ar=2; % height of each rec that compose the bar chart
db=1.8;% Dist betweens bars
yi=2;
xt=1;
ntl(1,1)=3;  % Ticks percentage that shows dates in the horizontal bar chart for the first drought event
ntl(2,1)=5;  % Ticks percentage that shows dates in the horizontal bar chart for the second drought event
ntl(3,1)=10; % Ticks percentage that shows dates in the horizontal bar chart for the third drought event
% Basic configuration of the subplot
nplotx=1;
nploty=1;
espvbi=0.015;
espvbs=0.125;
espvf=0.0;
esphf=0.0;
esphbe=0.025;
esphbd=0.025;
nt=15; %Numero de tons
nc=3; %Numero de cores por quadrante
rr=3; %Correction factor (maximum of the SPI)
yx=-20;
[PLOT]=posicao3(espvbi,espvbs,espvf,esphbe,esphbd,esphf,nplotx,nploty);
% Titles of the drought events
titulo{1,1}='1992-1994 Drought Event';
titulo{2,1}='1997-2002 Drought Event';
titulo{3,1}='2010-2018 Drought Event';
close all
for k=1:3

    xi=find(datenum(data_reserv_completa)==drought_event{k,1}(1,1));
    xf=find(datenum(data_reserv_completa)==drought_event{k,1}(1,2));
    ev=xi:xf;
    II=[vd_r017_ob(ev) vd_r017_nt(ev) spi(ev)];
    II=sum(isnan(II),2);
    II=find(II==0);II=II(1);
    ss=spi(ev);
    ss(ss>rr)=rr;ss(ss<-rr)=-rr;
    xr=1;
    c=0;
    clear ee TT
    figure('color',[1 1 1],'position',[10 10 1100 400])
    subplot('position',PLOT{1,1})
    for ii=1:size(drought_event{k,2},1)
        if drought_event{k,2}(ii,1)==0
            continue
        else
            c=c+1;
            ee(c,1)=find(data_reserv_completa(:,1)==drought_event{k,2}(ii,1) & data_reserv_completa(:,2)==drought_event{k,2}(ii,2));
        end
    end


    for iii=1:length(ev)
        cc=cor_ivd2(vd_r017_ob(ev(iii))*rr,ss(iii),rr,cq1,cq2,cq3,cq4,nt,nc);
        rectangle('Position',[(xr) yi lr ar],'FaceColor',cc,'EdgeColor',[0 0 0],'linewidth',0.1),hold on
        if isnan(vd_r017_nt(ev(iii)))==1
            %             rectangle('Position',[xr yi+db lr ar],'FaceColor',[0 0 0],'EdgeColor',[0 0 0],'linewidth',0.1)
        else
            cc=cor_ivd2(vd_r017_nt(ev(iii))*rr,ss(iii),rr,cq1,cq2,cq3,cq4,nt,nc);
            rectangle('Position',[xr yi+ar+db lr ar],'FaceColor',cc,'EdgeColor',[0 0 0],'linewidth',0.1)
        end
        if isempty(find(ee==ev(iii)))==0
            scatter(xr+(lr/2),yi+(1.3*ar),200,'o','MarkerFaceColor',[0 0 0],'MarkerEdgeColor',[0 0 0])
            scatter(xr+(lr/2),yi+(ar)+(0.7*db),200,'p','MarkerFaceColor',[0 0 0],'MarkerEdgeColor',[0 0 0])

        end
        TT(iii,1)=xr;
        xr=xr+lr;
    end
    title(titulo{k,1},'fontsize',28)

    for iii=1:ntl(k):length(ev)
        c3=datenum(data_reserv_completa(ev(iii),:));
        tt=text(TT(iii)+(lr/2),yi-(xt*ar),datestr(c3,'mm/yyyy'),'fontsize',24);
        tt.HorizontalAlignment='Center';
        tt.Rotation=90;
    end
    ylim([yi-((xt*1.75)*ar) (2*ar+db+yi)])
    xlim([1 xr])
    axis off
    print(gcf,['DC_B_E_v2' num2str(k,'%4.2f'),'.png'],'-dpng','-r300');
end

%% Plot the selected dates of the drought events in the DCA color wheel
[X,Y,COR]=plot_droughwheel(3.2, cq1,cq2,cq3,cq4,200,5);% Create the DCA color wheel
% Basic configuration of the subplot
nplotx=1;
nploty=1;
espvbi=0.000;
espvbs=0.00;
espvf=0.00;
esphf=0.00;
esphbe=0.000;
esphbd=0.000;
[PLOT]=posicao3(espvbi,espvbs,espvf,esphbe,esphbd,esphf,nplotx,nploty);
for k=1:3
    figure('color',[1 1 1],'position',[10 10 700 800])
    subplot('position',PLOT{1,1})
    scatter(X,Y,20,COR,'filled'),hold on
    plot([0 0],[-3.4 3.4],'w--','linewidth',2), hold on
    plot([-3.4 3.4],[0 0],'w--','linewidth',2)
    clear ev
    c=0;
    for ii=1:size(drought_event{k,2},1)
        if drought_event{k,2}(ii,1)==0
            continue
        else
            c=c+1;
            ev(c,1)=find(data_reserv_completa(:,1)==drought_event{k,2}(ii,1) & data_reserv_completa(:,2)==drought_event{k,2}(ii,2));
        end
        ss=spi(ev);
        ss(ss>rr)=rr;ss(ss<-rr)=-rr;
        for iii=1:length(ev)
            plot([vd_r017_ob(ev(iii))*rr vd_r017_nt(ev(iii))*rr],[ss(iii) ss(iii)],'k--','linewidth',0.5)
            if k==2
                tt=text((vd_r017_nt(ev(iii))*rr)+0.1,ss(iii)+0.1,datestr(datenum(data_reserv_completa(ev(iii),:)),'yyyy'),'fontsize',25);
            else
                xx=(vd_r017_nt(ev(iii))+vd_r017_ob(ev(iii)))*rr*0.5;
                tt=text(xx-0.2,ss(iii)+0.25,datestr(datenum(data_reserv_completa(ev(iii),:)),'yyyy'),'fontsize',25);
            end
        end
        scatter(vd_r017_ob(ev)*rr,ss,70,'o','MarkerFaceColor',[0 0 0],'MarkerEdgeColor',[0 0 0])
        scatter(vd_r017_nt(ev)*rr,ss,150,'p','MarkerFaceColor',[0 0 0],'MarkerEdgeColor',[0 0 0])
        axis off
    end
    print(gcf,['DC_C_E_v2' num2str(k,'%4.2f'),'.png'],'-dpng','-r300');
end

%% Chart Percentage of Time for each drought quadrant 
m=0;
% Update of the CS file - Add the information of the whole study period
for iii=1:size(spi,1)
    if isnan(vd_r017_ob(iii))==1
        continue
    end
    m=m+1;
    if vd_r017_ob(iii)>=0 && spi(iii)>=0
        quad=1;
    elseif vd_r017_ob(iii)>=0 && spi(iii)<0
        quad=2;
    elseif vd_r017_ob(iii)<0 && spi(iii)<0
        quad=3;
    elseif vd_r017_ob(iii)<0 && spi(iii)>=0
        quad=4;
    end
    CS{4,1}(m,1)=quad;
    if vd_r017_nt(iii)>=0 && spi(iii)>=0
        quad=1;
    elseif vd_r017_nt(iii)>=0 && spi(iii)<0
        quad=2;
    elseif vd_r017_nt(iii)<0 && spi(iii)<0
        quad=3;
    elseif vd_r017_nt(iii)<0 && spi(iii)>=0
        quad=4;
    end
    CS{4,1}(m,2)=quad;
end

% Basic configuration of the subplot
nplotx=1;
nploty=1;
espvbi=0.080;
espvbs=0.025;
espvf=0.0;
esphf=0.0;
esphbe=0.24;
esphbd=0.015;
[PLOT]=posicao3(espvbi,espvbs,espvf,esphbe,esphbd,esphf,nplotx,nploty);

for k=1:4
    cs=CS{k,1};
    for iii=1:4
        xd=find(cs(:,1)==iii);
        tt_cs(iii,1)=length(xd)*100/size(cs,1);
        xd=find(cs(:,2)==iii);
        tt_cs(iii,2)=length(xd)*100/size(cs,1);
    end
    X = categorical({'ACS','TRS'});
    figure('color',[1 1 1],'position',[10 10 450 500])
    subplot('position',PLOT{1,1})
    ba=bar(X,tt_cs','stacked');
    if k==1
    ylabel('Drought Stages proportion')
    end
    ba(1).FaceColor=[0 1 1];
    ba(2).FaceColor=[1 1 0.0];
    ba(3).FaceColor=[1 0 0.0];
    ba(4).FaceColor=[0.5 0.0 0.5];
    ba(1).BarWidth=0.5;
    ba(2).BarWidth=0.5;
    ba(3).BarWidth=0.5;
    ba(4).BarWidth=0.5;
    ba(1).FaceAlpha=0.5;
    ba(2).FaceAlpha=0.5;
    ba(3).FaceAlpha=0.5;
    ba(4).FaceAlpha=0.5;
    set(gca,'ytick',[0:10:100],'fontsize',22)
    ylim([0 100])
    grid on
     print(gcf,['TEMPO_E' num2str(k,'%4.2f'),'.png'],'-dpng','-r300');
end


% %% Kmeans
% II=[volobr017santnf(1:end-1) voltar017santnf(1:end-1) SPIRSRALLCPACNF];
% nn=isnan(II);
% nn=sum(nn,2);
% nn=find(nn==0);
% II=II(nn,:);
% k=4;
% cor(1,:)=[0 1 1];
% cor(2,:)=[1 1 0];
% cor(3,:)=[1 0 0];
% cor(4,:)=[1 0 1];
% cor(5,:)=[0 1 0];
% 
% idx = kmeans(II,k);
% COR=zeros(size(II,1),3);
% for iii=1:k
%     xk=find(idx==iii);
%     for ii=1:length(xk)
%     COR(xk(ii),:)=cor(iii,:);
%     end
% end
% scatter(II(:,2),II(:,1),[],COR,'filled')