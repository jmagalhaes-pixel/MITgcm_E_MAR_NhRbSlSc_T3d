function maxmin(x)

% mcb, nrl, 2012-17-01

disp(['max is ' sprintf('%13.8f',max(rem_NaN(x(:))))])
disp(['min is ' sprintf('%13.8f',min(rem_NaN(x(:))))])
disp(['mean is ' sprintf('%13.8f',mean(rem_NaN(x(:))))])
disp(['std is ' sprintf('%13.8f',std(rem_NaN(x(:))))])
disp(['num NaN is ' sprintf('%13d',sum(isnan(x(:))))])
disp(['num cplx is ' sprintf('%13d',sum(imag(x(:))~=0))])
disp(['num 0 is ' sprintf('%13d',sum(x(:)==0))])
disp(['num +/-inf is' sprintf( '%13d',sum( isinf(x(:)) ) )])
