function d=bandpass_new(c,flp,fhi,npts,delt,n)
% d=bandpass_new(c,flp,fhi,npts,delt,n)
%
% MCB, USM, 2017-8-28
% bandpass a time series with a n-nd order butterworth filter
%
% c = input time series
% flp = lowpass corner frequency of filter
% fhi = hipass corner frequency
% npts = samples in data
% delt = sampling interval of data
% n    = nth order butterworth filter
fnq=1/(2*delt);          % Nyquist frequency
Wn=[flp/fnq fhi/fnq];    % butterworth bandpass non-dimensional frequency
[b,a]=butter(n,Wn);      % construct the filter
d=filtfilt(b,a,c);       % zero phase filter the data
return;
