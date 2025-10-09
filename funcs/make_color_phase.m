%% make_color_phase
%% Maarten Buijsman
%% UCLA, 2010-3-23 
%% colormap for plotting maps of phase
%% 

function [CMAP] = make_color_phase;

%% make red blue yellow red colormap

%yel2 = [255 255 255/1.5]/255;
yel2 = [255 255 0]/255;
gr   = [34 139 34]/255;
R_c = [1    0     0];
%B_c = [0    0.0625    1.0000];
B_c = [0    0 255]/255;
cy = [0 255 255]/255;

num = 32;

r1 = linspace(R_c(1),B_c(1),num);
r2 = linspace(R_c(2),B_c(2),num);
r3 = linspace(R_c(3),B_c(3),num);
% b1 = linspace(B_c(1),gr(1),num);
% b2 = linspace(B_c(2),gr(2),num);
% b3 = linspace(B_c(3),gr(3),num);

b1 = linspace(B_c(1),cy(1),num);
b2 = linspace(B_c(2),cy(2),num);
b3 = linspace(B_c(3),cy(3),num);

% c1 = linspace(cy(1),gr(1),num);
% c2 = linspace(cy(2),gr(2),num);
% c3 = linspace(cy(3),gr(3),num);

c1 = linspace(cy(1),yel2(1),num);
c2 = linspace(cy(2),yel2(2),num);
c3 = linspace(cy(3),yel2(3),num);

% g1 = linspace(gr(1),yel2(1),num);
% g2 = linspace(gr(2),yel2(2),num);
% g3 = linspace(gr(3),yel2(3),num);

y1 = linspace(yel2(1),R_c(1),num);
y2 = linspace(yel2(2),R_c(2),num);
y3 = linspace(yel2(3),R_c(3),num);

% C1 = [r1 b1 c1 g1 y1];
% C2 = [r2 b2 c2 g2 y2];
% C3 = [r3 b3 c3 g3 y3];

C1 = [r1 b1 c1 y1];
C2 = [r2 b2 c2 y2];
C3 = [r3 b3 c3 y3];

CMAP = [C1;C2;C3]';
