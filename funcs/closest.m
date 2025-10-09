%% function Iclose = closest(vector,value)
%% finds index of value closest to value in the vector
%% Maarten Buijsman, GFDL, 2011-1-24

function Iclose = closest(vector,value)

[d,Iclose] = min(abs(vector-value));

return