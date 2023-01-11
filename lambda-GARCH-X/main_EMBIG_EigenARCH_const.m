%%%%%%%%%%%%%%%%%%%
%Recreates results for the unrestricted model

%% Housekeeping
clc
clear
format longG


addpath('20201202 - Replication material/Library/')
addpath('20201202 - Replication material/Library/DERIVESTsuite/')
addpath('/Users/frederikbach/Library/CloudStorage/OneDrive-BachTeknikApS/cand.polit/Speciale/lambda-garch/data')

%US Banks
Returns = readtable('13102022_data.xlsx', 'Sheet', 'DATA_CLEAN'); 
res1 = readmatrix('residuals_model1.xlsx');
res5 = readmatrix('residuals_model2.xlsx');
    
T_start = 1;                 % 01 Jan 2010
T_end   = 2249;              % 31 Dec 2018
    
%% Load data and specify options
p    = 5;       % # of variables
n    = 5;       % # of time-varying eigenvalues
 
x     = table2array(Returns(T_start:T_end,2:1+p))';     
dates = table2array(Returns(T_start:T_end,1))';
T     = length(x);                                   

theta0 = 0.05*ones(p*(p-1)/2+p+2*n*p+(p-n)*n+p,1);   
theta = readmatrix('estimates/theta1_constant.xlsx');
[loglike, sigma2, lambda, persistence] = EigenARCH_loglikelihood(x, theta, n);  %Fetch log likelihood and the vector of variances

%% Estimation

%Minimize log likelihood function numerically
options = optimset('Display','iter','PlotFcns',@optimplotfval,'UseParallel', true, 'TolFun',1e-8, 'TolX',1e-10, 'MaxFunEvals',3000000, 'MaxIter', 10000000);
[theta, L_con, exit, output, grad, hess] = fminunc(@(coef) -EigenARCH_loglikelihood(x,coef(), n), theta0, options);
L_con = -T*L_con

%Fetch estimated covariances and persistence
[loglike, sigma2, lambda, persistence] = EigenARCH_loglikelihood(x, theta, n);  %Fetch log likelihood and the vector of variances

%Fetch parameter estimates
[gamma, V, omega, alpha, beta, phi, mu] = EigenARCH_repar(p,n,theta);

% Optimality conditions
%L_foc = output.firstorderopt   % Should be zero
%L_soc = min(eig(hess))>0    % Should be 1 (only positive eigenvalues)

%% Model selections and misspecification testing
if 1==1
%[aic_EigenARCH,bic_EigenARCH] = aicbic(L_con, size(theta,1), T) %Information criteria

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
display(round(pValue_aut,3), 'no autocorrelation')
display(round(pValue_het,3), 'no heteroskedasticity')
display(round(pValue_arch,3), 'no ARCH')
end

%% Multivariate normality tests
multnorm(res1);
multnorm(res5);

%% Inference
if 1==1
%Standard errors based on Hessian
v_hess = inv(hess);                 
se_hess = sqrt(diag(v_hess)/T);  % divide by T to get s.e. for \hat\theta~N(\theta_0,E/T)

%Standard errors based on outer product of scores
jac_t  = jacobianest(@(coef) -EigenARCH_loglikelihood_cont(x,coef(), n), theta);                                                                
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
diary('output1.txt')

disp('Estimation results - lambda-GARCH')

display(persistence)
display(L_con)
display(aic_EigenARCH)
display(bic_EigenARCH)


% Eigenvectors
count = 0;
display(reshape(gamma(1:count+25),5,5), 'V')
display(reshape(se_delta_sandwich(1:count+25),5,5), 'V s.e.')

% W
count=26;
display([gamma(count:count+4), se_delta_sandwich(count:count+4)], 'W')

% A
count=31;
display(reshape(gamma(count:count+24),5,5), 'A')
display(reshape(se_delta_sandwich(count:count+24),5,5), 'A s.e.')

% B
count=56;
display(reshape(gamma(count:count+24),5,5), 'B')
display(reshape(se_delta_sandwich(count:count+24),5,5), 'B s.e.')

% Phi
count=81;
display([gamma(count:count+9), se_delta_sandwich(count:count+9)], 'phi')

% mu
count=91;
display([gamma(count:count+4), se_delta_sandwich(count:count+4)], 'mu')

diary('off')

%%
EigenARCH_graphics(V'*x,lambda,dates,1);
%[~, V, omega, a, b, ~, mu] = EigenARCH_repar(p, n, theta); %Fecth (reparameterized) parameter matrices


%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%              FUNCTIONS START HERE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [L, sigma2, lambda, persistence] = EigenARCH_loglikelihood(x, param, n)
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

[L_cont,sigma2, lambda, persistence] = EigenARCH_loglikelihood_cont(x,param,n);

L = mean(L_cont);
end

function [L, sigma2, lambda, persistence] = EigenARCH_loglikelihood_cont(x, param, n)
% Log likelihood contributions for the EigenARCH(1,1) moodel
% Inputs: 
%   x: pxT matrix of asset returns
%   param: vector of initial parameters
%   n: the number of factors (n<=p)   
% Outputs:
%   L: Log likelihood value
%   sigma2: Array of filtered covariance matrices
%   lambda: pxT matrix of time varying eigenvalues
%   persistence: scalar indicating persistence of the process.

%Constants
T = length(x);
p = size(x,1);

[~, V, omega, a, b, ~, mu] = EigenARCH_repar(p, n, param); %Fecth (reparameterized) parameter matrices

%Rotated returns
%y               = V'*x; 
    
%Log likelihood
loglike         = zeros(1, T);         %Vector to hold log-likelihood contributions
sigma2          = zeros(p, p,  T);   %Array to contain time-varying covariance matrices
lambda          = zeros(p,T);        %\lambda (vector), contains time-varying eigenvalues
lambda(:,1)     = omega;


for i = 2:T 
    %conditional eigenvalues
    lambda(:,i) = omega+a*(V'*(x(:,i-1))).^2 + b*lambda(:,i-1);               
    
    %conditional covariance matrix
    sigma2(:,:,i) = V*diag(lambda(:,i))*V'; %Save covariance estimate    
    
    %log-likelihood contributions
    loglike(i) = -p/2*log(2*pi)-0.5*sum(log(lambda(:,i)))-0.5*(V'*(x(:,i)-mu))'*diag(1./lambda(:,i))*(V'*(x(:,i)-mu));
    
end

persistence = max(eig(a+b)); %persistence of stochastic process

L = loglike; %Returns vector of loglikelihood contributions

end

function [gamma, V, omega, a, b, phi, mu] = EigenARCH_repar(p, n, param)
% Function to reparameterize the parameter-vector to the matrices

tal=1;
%Eigenvectors
phi = param(tal:tal + p*(p-1)/2-1);
phi = exp(phi)./(1+exp(phi))*pi/2;
tal=tal+p*(p-1)/2;
V = rotation(phi,p); %Rotation matrix

%Constant
omega = exp(param(tal:tal+p-1));
tal=tal+p;

%Reduced rank matrices
a = vec2mat(param(tal:tal+p*n-1),n).^2;
tal = tal + p*n;
     
b = reshape(param(tal:tal+p*n-1),n,p)'.^2;
tal=tal+p*n; 

mu = param(tal:tal+p-1);

gamma = [V(:); omega(:); a(:); b(:); phi(:); mu(:)];   

end


