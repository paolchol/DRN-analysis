function [X,Y,COR] = plot_droughwheel(rr,cq1,cq2,cq3,cq4,nt,nc)
% %% Circulo de cores


%% Grade
a=0:0.25:360; %% Resolucao da drought wheel
st=1.0;
res=100; % Resolucao do ciclo de seca;

% rr=sqrt((lim_vd(2)-lim_vd(1))^2+(lim_spi(2)-lim_spi(1))^2);
rr=0:rr/(res-1):rr;   % Todas as posicoes R do circulo de cores
c=0;

for iii=1:length(a)
    for ii=1:length(rr)
        c=c+1;
        X(c,1)=sind(a(iii))*rr(ii);
        Y(c,1)=cosd(a(iii))*rr(ii);
        A(c,1)=a(iii);
    end
end
%% Def Quadrandes
cmx=1;
% cq1=[0 0 1];%dq1(dq1==0)=cq2(dq1==0);
% cq2=[0 1 1];%dq2(dq2==0)=cq3(dq2==0);
% cq3=[1 1 0];%dq2(dq2==0)=cq3(dq2==0);
% cq4=[1 0 0];%dq4(dq4==0)=cq5(dq4==0);


dq1=cq2-cq1;
dq2=cq3-cq2;
dq3=cq4-cq3;
dq4=cq1-cq4;
%% Primeiro Quadrante
a_q1=45:90/nc:135;  % Angulo Primeiro Quadrante
r_q1=cq1(1):dq1(1)/(length(a_q1)-1):cq2(1);r_q1=r_q1';
g_q1=cq1(2):dq1(2)/(length(a_q1)-1):cq2(2);g_q1=g_q1';
b_q1=cq1(3):dq1(3)/(length(a_q1)-1):cq2(3);b_q1=b_q1';
dq1(dq1==0)=cq2(dq1==0);
if isempty(r_q1)==1
   r_q1=ones(length(a_q1),1)*dq1(1); 
end
if isempty(g_q1)==1
   g_q1=ones(length(a_q1),1)*dq1(2); 
end
if isempty(b_q1)==1
   b_q1=ones(length(a_q1),1)*dq1(3); 
end
cb_q1=[r_q1 g_q1 b_q1];  % Cor base Quadrante 1
%% Segundo Quadrante
a_q2=135:90/nc:225; % Angulo segundo Quadrante
r_q2=cq2(1):dq2(1)/(length(a_q2)-1):cq3(1);r_q2=r_q2';
g_q2=cq2(2):dq2(2)/(length(a_q2)-1):cq3(2);g_q2=g_q2';
b_q2=cq2(3):dq2(3)/(length(a_q2)-1):cq3(3);b_q2=b_q2';
dq2(dq2==0)=cq3(dq2==0);

if isempty(r_q2)==1
   r_q2=ones(length(a_q2),1)*dq2(1); 
end
if isempty(g_q2)==1
   g_q2=ones(length(a_q2),1)*dq2(2); 
end
if isempty(b_q2)==1
   b_q2=ones(length(a_q2),1)*dq2(3); 
end
cb_q2=[r_q2 g_q2 b_q2];  % Cor base Quadrante 1
%% Terceiro Quadrante
a_q3=225:90/nc:315; % Angulo terceiro Quadrante
r_q3=cq3(1):dq3(1)/(length(a_q3)-1):cq4(1);r_q3=r_q3';
g_q3=cq3(2):dq3(2)/(length(a_q3)-1):cq4(2);g_q3=g_q3';
b_q3=cq3(3):dq3(3)/(length(a_q3)-1):cq4(3);b_q3=b_q3';
dq3(dq3==0)=cq4(dq3==0);

if isempty(r_q3)==1
   r_q3=ones(length(a_q3),1)*dq3(1); 
end
if isempty(g_q3)==1
   g_q3=ones(length(a_q3),1)*dq3(2); 
end
if isempty(b_q3)==1
   b_q3=ones(length(a_q3),1)*dq3(3); 
end
cb_q3=[r_q3 g_q3 b_q3];  % Cor base Quadrante 1
%% Quarto Quadrante
a_q4=315:90/nc:405;  % Angulo quarto Quadrante
r_q4=cq4(1):dq4(1)/(length(a_q4)-1):cq1(1);r_q4=r_q4';
g_q4=cq4(2):dq4(2)/(length(a_q4)-1):cq1(2);g_q4=g_q4';
b_q4=cq4(3):dq4(3)/(length(a_q4)-1):cq1(3);b_q4=b_q4';
dq4(dq4==0)=cq1(dq4==0);

if isempty(r_q4)==1
   r_q4=ones(length(a_q4),1)*dq4(1); 
end
if isempty(g_q4)==1
   g_q4=ones(length(a_q4),1)*dq4(2); 
end
if isempty(b_q4)==1
   b_q4=ones(length(a_q4),1)*dq4(3); 
end
cb_q4=[r_q4 g_q4 b_q4];  % Cor base Quadrante 1

AA=[a_q1 a_q2 a_q3 a_q4];
AA(AA>360)=AA(AA>360)-360;
R=[r_q1' r_q2' r_q3' r_q4'];
G=[g_q1' g_q2' g_q3' g_q4'];
B=[b_q1' b_q2' b_q3' b_q4'];
%% Mapa de Cor final
CC=[R' G' B'];

% CC(CC>=1)=CC(CC>=1)-1;

c=0;
COR=zeros(size(X,1)*size(X,2),3);
% RR=zeros(size(X));
% X=reshape(X,[size(X,1)*size(X,2) 1]);
% Y=reshape(Y,[size(Y,1)*size(Y,2) 1]);
% A=acosd(Y./sqrt((X.^2)+(Y.^2)));
for iii=1:length(X)
    c=c+1;
    a=A(iii);
%     if X(iii)<0 & Y(iii)<0
%         a=360-a;
%     elseif X(iii)<0 & Y(iii)>0
%         a=360-a;
%         %         a(a>360)=a(a>360)-360;
%     end
    %         RR(iii)=a;
    xa=find(abs(AA-a)==min(min(abs(AA-a))));
    P(iii,1)=xa(1);
end
for iii=1:length(X)
    dc=st*sqrt((X(iii)^2)+(Y(iii)^2))/max(rr);
    dc(dc>1)=1;
    cor=(CC(P(iii),:));
    oo=[1 1 1];
    nc=length(find(P==P(iii,1)));
        r=oo(1):(cor(1)-oo(1))/(nt-1):cor(1);
        r=r';
        if isempty(r)==1
            r=ones(nt,1)*cor(1);
        end
        g=oo(2):(cor(2)-oo(2))/(nt-1):cor(2);
        g=g';
        if isempty(g)==1
            g=ones(nt,1)*cor(2);
        end
        b=oo(3):(cor(3)-oo(3))/(nt-1):cor(3);
        b=b';
        if isempty(b)==1
            b=ones(nt,1)*cor(3);
        end
        int=[r g b];
        if round(nt*dc)>0
        cor=int(round(nt*dc),:);
        else
        cor=[1 1 1];    
        end
        COR(iii,:)=cor;
end

scatter(X,Y,20,COR,'filled'),hold on



end

