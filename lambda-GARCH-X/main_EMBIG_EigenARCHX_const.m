%%%%%%%%%%%%%%%%%%%
%Recreates results for the unrestricted model

%% Housekeeping
clc
clear
format longG

addpath('20201202 - Replication material/Library/')
addpath('20201202 - Replication material/Library/DERIVESTsuite/')
addpath('/Users/frederikbach/Library/CloudStorage/OneDrive-BachTeknikApS/cand.polit/Speciale/lambda-garch/data')

Returns = readtable('13102022_data.xlsx', 'Sheet', 'DATA_CLEAN');            
    
T_start = 1;                 % 01 Jan 2010
T_end   = 2249;              % 31 Dec 2018
    
clc
%% Load data and specify options
p    = 5;       % # of variables
n    = 5;       % # of time-varying eigenvalues
exo  = 18;      % column # of exogenous var. in dataset [10, 15, 17, 18] = [bcom, brent, 10yr, usd]
 
x     = table2array(Returns(T_start:T_end,2:1+p))';
z     = table2array(Returns(T_start:T_end,exo))';
X = [x' z']';
dates = table2array(Returns(T_start:T_end,1))';
T     = length(x);                                   

theta0 = 0.05*ones(p*(p-1)/2+p+2*n*p+(p-n)*n+p+p,1);   
%theta = readmatrix('estimates/theta2_constant.xlsx');
%[loglike, sigma2, lambda, persistence] = EigenARCH_loglikelihood(X, theta, n);
%[gamma, V, omega, a, b, c, phi, mu] = EigenARCH_repar(p, n, theta);

%% Estimation

%Minimize log likelihood function numerically
options = optimset('Display','iter','PlotFcns',@optimplotfval,'UseParallel', true, 'TolFun',1e-8, 'TolX',1e-10, 'MaxFunEvals',3000000, 'MaxIter', 10000000);
[theta, L_con, exit, output, grad, hess] = fminunc(@(coef) -EigenARCH_loglikelihood(X,coef(), n), theta0, options);
L_con = -T*L_con

%Fetch estimated covariances and persistence
[loglike, sigma2, lambda, persistence] = EigenARCH_loglikelihood(X, theta, n);  %Fetch log likelihood and the vector of variances

%Fetch parameter estimates
[gamma, V, omega, a, b, c, phi, mu] = EigenARCH_repar(p,n,theta);

% Optimality conditions
L_foc = output.firstorderopt   % Should be zero
L_soc = min(eig(hess))>0       % Should be 1 (only positive eigenvalues)

%% Model selections and misspecification testing
if 1==1
[aic_EigenARCH,bic_EigenARCH] = aicbic(L_con, size(theta,1), T) %Information criteria

% Compute residuals
res = zeros(p,T);
res2 = zeros(1,T);
for t = 2:T
    %compute residuals using ASYMMETRIC MATRIX SQUARE ROOT   
   [V_tmp,D]    = eig(sigma2(:,:,t));   
   res(:,t) = (diag(diag(D).^(-1/2))*V_tmp')*x(:,t);
   
   res2(:,t) = res(:,t)'*res(:,t);
end

% Misspecification tests
lag_test = [5,10,15]; %Which lags are we testing

no_aut      = zeros(p,size(lag_test,2));
pValue_aut  = zeros(p,size(lag_test,2));
stat_aut    = zeros(p,size(lag_test,2));
cValue_aut  = zeros(p,size(lag_test,2));

no_het      = zeros(p,size(lag_test,2));
pValue_het  = zeros(p,size(lag_test,2));
stat_het    = zeros(p,size(lag_test,2));
cValue_het  = zeros(p,size(lag_test,2));

no_arch      = zeros(p,size(lag_test,2));
pValue_arch  = zeros(p,size(lag_test,2));
stat_arch    = zeros(p,size(lag_test,2));
cValue_arch  = zeros(p,size(lag_test,2));

for i=1:p
    [no_aut(i,:),  pValue_aut(i,:),  stat_aut(i,:),  cValue_aut(i,:)] = lbqtest(res(i,:),    'lags', [5,10,15]);   %Ljung-Box test for residual autocorrelation (H0 is no autocorrelation)
    [no_het(i,:),  pValue_het(i,:),  stat_het(i,:),  cValue_het(i,:)] = lbqtest(res(i,:).^2, 'lags', [5,10,15]);   %Ljung-Box test for heteroscedasticity (H0 is no heteroscedasticity)
    [no_arch(i,:), pValue_arch(i,:), stat_arch(i,:), cValue_arch(i,:)] = archtest(res(i,:),    'lags', [5,10,15]); %Engle's no-arch test (H0 is no arch)
end
display(round(pValue_aut,2), 'no autocorrelation')
display(round(pValue_het,2), 'no heteroskedasticity')
display(round(pValue_arch,2), 'no ARCH')
end

%% Inference
if 1==1
%Standard errors based on Hessian
v_hess = inv(hess);                 
se_hess = sqrt(diag(v_hess)/T);  % divide by T to get s.e. for \hat\theta~N(\theta_0,E/T)

%Standard errors based on outer product of scores
jac_t  = jacobianest(@(coef) -EigenARCH_loglikelihood_cont(X,coef(), n), theta);                                                                
v_jac  = jac_t'*jac_t/T;         %divide by T to get 1/T sum dl/dp (dl/dp)'

%Standard errors based on the sandwich formula
v_sandwich  = v_hess*v_jac*v_hess; %divide by T to get s.e. for \hat\theta~N(\theta_0,E/T)
se_sandwich = sqrt(diag(v_sandwich)/T);

%Standard errors for transformed parameters based on the delta method
A = jacobianest(@(coef) EigenARCH_repar(p,n,coef()), theta);

v_delta = A*v_hess*A';
se_delta = sqrt(diag(v_delta)/T);

v_delta_sandwich = A*v_sandwich*A';
se_delta_sandwich = sqrt(diag(v_delta_sandwich)/T);

end


%% Print output
disp('Estimation results - lambda-GARCH-X with USD')

% Eigenvectors
count = 0;
display(reshape(round(gamma(1:count+25),3),5,5), 'V')
display(reshape(round(se_delta_sandwich(1:count+25),3),5,5), 'V s.e.')

% W
count=26;
display([round(gamma(count:count+4),3), round(se_delta_sandwich(count:count+4),3)], 'W')

% A
count=31;
display(reshape(round(gamma(count:count+24),3),5,5), 'A')
display(reshape(round(se_delta_sandwich(count:count+24),3),5,5), 'A s.e.')

% B
count=56;
display(reshape(round(gamma(count:count+24),3),5,5), 'B')
display(reshape(round(se_delta_sandwich(count:count+24),3),5,5), 'B s.e.')

% C
count=81;
display([round(gamma(count:count+4),3), round(se_delta_sandwich(count:count+4),3)], 'C')

% Phi
count=86;
display([round(gamma(count:count+9),3), round(se_delta_sandwich(count:count+9),3)], 'phi')

% mu
count=96;
display([round(gamma(count:count+4),3), round(se_delta_sandwich(count:count+4),3)], 'mu')


%%
display(round(phi,3),'phi')
display(round(V,3),'V')
display(round(omega,3),'omega')
%g
display(round(a,3),'A')
display(round(b,3),'B')
%alpha
%beta
display(round(persistence,3),'stationarity condition')
%lambda_var = diag((eye(p)-alpha-beta)^(-1)*omega);
%uncon_var = V*diag((eye(p)-alpha-beta)^(-1)*omega)*V';

%% GRAPHS
EigenARCH_graphics(V'*x,lambda,dates,1);

MGARCH_graphics(x,sigma2(:,:,1:T),dates, 'vol'); % 'all', 'desc', 'res', 'vol'
MGARCH_graphics(x,sigma2(:,:,1:T),dates, 'res'); % 'all', 'desc', 'res', 'vol'


%%



%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%              FUNCTIONS START HERE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [L, sigma2, lambda, persistence] = EigenARCH_loglikelihood(X, param, n)
% Log likelihood function for the EigenARCH(1,1) moodel
% Inputs: 
%   x: pxT matrix of asset returns
%   param: vector of initial parameters
%   n: the number of factors (n<=p)
% Outputs:
%   L: Log likelihood value
%   sigma2: Array of filtered covariance matrices
%   lambda: pxT matrix of time varying eigenvalues
%   persistence: scalar indicating persistence of the process.

[L_cont,sigma2, lambda, persistence] = EigenARCH_loglikelihood_cont(X, param, n);

L = mean(L_cont);
end

function [L, sigma2, lambda, persistence] = EigenARCH_loglikelihood_cont(X, param, n)
% Log likelihood contributions for the EigenARCH(1,1) moodel
% Inputs: 
%   x: pxT matrix of asset returns
%   z: 1xT matrix of exogenous covariate realizations
%   param: vector of initial parameters
%   n: the number of factors (n<=p)   
% Outputs:
%   L: Log likelihood value
%   sigma2: Array of filtered covariance matrices
%   lambda: pxT matrix of time varying eigenvalues
%   persistence: scalar indicating persistence of the process.

x = X(1:5,:);
z = X(5+1,:);

%Constants
T = length(x);
p = size(x,1);

[~, V, omega, a, b, c, ~, mu] = EigenARCH_repar(p, n, param); %Fetch (reparameterized) parameter matrices

%Rotated returns
%y               = V'*x; 
    
%Log likelihood
loglike         = zeros(1, T);         %Vector to hold log-likelihood contributions
sigma2          = zeros(p, p,  T);     %Array to contain time-varying covariance matrices
lambda          = zeros(p,T);          %\lambda (vector), contains time-varying eigenvalues
lambda(:,1)     = omega;


for i = 2:T 
    %conditional eigenvalues
    lambda(:,i) = omega+a*(V'*(x(:,i-1))).^2 + b*lambda(:,i-1) + c*(z(1,i-1)^2);            
    
    %conditional covariance matrix
    sigma2(:,:,i) = V*diag(lambda(:,i))*V'; %Save covariance estimate    
    
    %log-likelihood contributions
    loglike(i) = -p/2*log(2*pi)-0.5*sum(log(lambda(:,i)))-0.5*(V'*(x(:,i)-mu))'*diag(1./lambda(:,i))*(V'*(x(:,i)-mu));
    
end

persistence = max(eig(a+b)); %persistence of stochastic process

L = loglike; %Returns vector of loglikelihood contributions

end

function [gamma, V, omega, a, b, c, phi, mu] = EigenARCH_repar(p, n, param)
% Function to reparameterize the parameter-vector to the matrices

tal=1;
%Eigenvectors
phi = param(tal:tal + p*(p-1)/2-1);
phi = exp(phi)./(1+exp(phi))*pi/2;
tal=tal+p*(p-1)/2;
V = rotation(phi,p); %Rotation matrix

% Constant W vector
omega = exp(param(tal:tal+p-1));
tal=tal+p;

% Exogenous term C
c = exp(param(tal:tal+p-1));
tal = tal + p;

%Reduced rank matrices
a = vec2mat(param(tal:tal+p*n-1),n).^2;
tal = tal + p*n;

b = reshape(param(tal:tal+p*n-1),n,p)'.^2;
tal=tal+p*n; 

% Constant mu vector
mu = param(tal:tal+p-1);

gamma = [V(:); omega(:); a(:); b(:); c(); phi(:); mu()];   

end


