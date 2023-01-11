%%%%%%%%%%%%%%%%%%%
%Recreates results for the unrestricted model

%% Housekeeping
clc
clear
format longG

addpath('20201202 - Replication material/Library/')
addpath('20201202 - Replication material/Library/DERIVESTsuite/')
    
%% Model specification
p = 2;    % number of variables
n = 2;    % number of eigenvalues

% True model parameters
omega = [1; 1];
alpha = [1/3 1/3; 1/4 1/4];   % ARCH term
beta  = [0.1 0.1; 0.15 0.15]; % GARCH term
c    = [0.1; 0.1];            % Exogenous term 
gamma = 0.25;                 % autoregressive coefficient for O_t 

% Check hypothesized stationarity condition
display(round(max(abs(eig(alpha+beta))),3),'Maximum absolute eigenvalue of A + B');

phi = 0.3;
V = [cos(phi) -sin(phi); sin(phi) cos(phi)];

% moments for innovation term eta
mu = [0 0];
Sigma = [1 0; 0 1];

%% Simulation

% Monte Carlo calibration
M = 2000; % number of simulations
T = 1000; % length of time series
rng(1997) % seed number
options = optimset('Display','off','UseParallel', false, ...
                    'TolFun', 1e-20,'TolX',1e-20,'MaxFunEvals', 30000000, 'MaxIter', 100000000); % options for optimizer

theta = zeros(p*(p-1)/2+p+2*n*p+(p-n)*n+p,M); % placeholder for estimates
%O_sim = zeros(T,M);        % placeholder for simulated univariate series
%X1_sim = zeros(T,M);        % placeholder for simulated multivariate series
%X2_sim = zeros(T,M);        % placeholder for simulated multivariate series


tic
  
for i = 1:M
    clc
    disp(['Now running simulation ' num2str(i) ' out of ' num2str(M)]);
    
    % initialize MGARCH series X
    X = zeros(T,2)';        % placeholder for simulated multivariate series
    Lambda = zeros(T,2);   % placeholder for simulated eigenvalue series
    Lambda(1,:) = omega';  % set initial value of Lambda to omega'

    % initialize exogenous Student's t ARCH model J
    O = zeros(T,1);

    % draw innovations from N_2(0,I_p) and t_4
    eta = mvnrnd(mu,Sigma,T);
    v = normrnd(0,1,[T,1]);
    %v   = trnd(4,T,1) * sqrt((4-2)/4);

    for t = 2:T
        
        % simulate O_t
        O(t,1) = sqrt(1 + gamma*(O(t-1,1))^2) * v(t,1);

        % simulate X_t
        % i) conditional eigenvalues from lambda-GARCH-X
        Lambda(t,:) = omega + alpha*((V'*X(:,t-1)).^2) + beta*Lambda(t-1,:)' + c*(O(t-1,1).^2);
        
        % ii) realization of X_t based on conditional covariance matrix
        X(:,t) = V * (diag(Lambda(t,:))).^(1/2) * eta(t,:)';
    end

    % save simulations
    %O_sim(:,i) = O(:,1);
    %X1_sim(:,i) = X(1,:)';
    %X2_sim(:,i) = X(2,:)';

    % append X and O
    Z = [X; O'];

    % initial parameters for optimizer
    theta0 = 0.05*ones(p*(p-1)/2+p+2*n*p+(p-n)*n+p,1); 
    
    % estimate
    [thetahat, L_con, exit, output, grad, hess] = fminunc(@(coef) -EigenARCH_loglikelihood(Z,coef(), n), theta0, options);
    
    % save estimate 
    theta(:,i) = thetahat;
    
    % save convergence result
end

toc

writematrix(theta,'theta_sims.xlsx','Sheet',1,'Range','A1')
%% PLOT SIMULATIONS

tiledlayout(3,1)
nexttile
plot(O_sim(:,1))
title('Panel A: Simulated exogenous covariate', 'FontSize', 20)

nexttile
plot(X1_sim(:,1))
title('Panel B: Simulated X1 from multivariate GARCH-X', 'FontSize', 18)

nexttile
plot(X2_sim(:,1))
title('Panel C: Simulated X2 from multivariate GARCH-X', 'FontSize', 18)

%% LOAD SIMULATIONS FROM FILE
theta = readmatrix('theta_sims.xlsx');


%% PLOT OMEGA

tiledlayout(2,1)
nexttile

histfit(sqrt(T)*(exp(theta(2,:)) - omega(1)),100)
title('Figure 1a: $\sqrt{T}(\hat{\omega}_1 - \omega_{1,0})$', 'interpreter', 'latex', 'FontSize', 20)

nexttile
histfit(sqrt(T)*(exp(theta(3,:)) - omega(2)),100)
title('Figure 1b: $\sqrt{T}(\hat{\omega}_1 - \omega_{1,0})$', 'interpreter', 'latex', 'FontSize', 20)

%% PLOT A

tiledlayout(2,2)
nexttile
histfit(sqrt(T)*(theta(4,:).^2 - alpha(1,1)),100)
title('Figure 2a: $\sqrt{T}(\hat{A}_{11} - A_{11,0})$', 'interpreter', 'latex', 'FontSize', 20)

nexttile
histfit(sqrt(T)*(theta(5,:).^2 - alpha(2,1)),100)
title('Figure 2b: $\sqrt{T}(\hat{A}_{21} - A_{21,0})$', 'interpreter', 'latex', 'FontSize', 20)

nexttile
histfit(sqrt(T)*(theta(6,:).^2 - alpha(1,2)),100)
title('Figure 2c: $\sqrt{T}(\hat{A}_{12} - A_{12,0})$', 'interpreter', 'latex', 'FontSize', 20)

nexttile
histfit(sqrt(T)*(theta(7,:).^2 - alpha(2,2)),100)
title('Figure 2d: $\sqrt{T}(\hat{A}_{22} - A_{22,0})$', 'interpreter', 'latex', 'FontSize', 20)


%% PLOT B

tiledlayout(2,2)
nexttile
%histfit(sqrt(T)*(theta(8,:).^2 - beta(1,1)),100)
histfit(sqrt(T)*(theta(8,:).^2),100)
title('Figure 3a: $\sqrt{T}(\hat{B}_{11} - B_{11,0})$', 'interpreter', 'latex', 'FontSize', 20)

nexttile
histfit(sqrt(T)*(theta(9,:).^2 - beta(2,1)),100)
title('Figure 3b: $\sqrt{T}(\hat{B}_{21} - B_{21,0})$', 'interpreter', 'latex', 'FontSize', 20)

nexttile
histfit(sqrt(T)*(theta(10,:).^2 - beta(1,2)),100)
title('Figure 3c: $\sqrt{T}(\hat{B}_{12} - B_{12,0})$', 'interpreter', 'latex', 'FontSize', 20)

nexttile
histfit(sqrt(T)*(theta(11,:).^2 - beta(2,2)),100)
title('Figure 3d: $\sqrt{T}(\hat{B}_{22} - B_{22,0})$', 'interpreter', 'latex', 'FontSize', 20)

%% PLOT C

tiledlayout(2,1)
nexttile
histfit(sqrt(T)*(exp(theta(12,:)) - c(1)),100)
title('Figure 4a: $\sqrt{T}(\hat{C}_1 - C_{1,0})$', 'interpreter', 'latex', 'FontSize', 20)

nexttile
histfit(sqrt(T)*(exp(theta(13,:)) - c(2)),100)
title('Figure 4b: $\sqrt{T}(\hat{C}_2 - C_{2,0})$', 'interpreter', 'latex', 'FontSize', 20)
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

x = X(1:2,:);
z = X(3,:);

%Constants
T = length(x);
p = size(x,1);

[~, V, omega, ~, ~, C, ~, ~, A, B] = EigenARCH_repar(p, n, param); %Fetch (reparameterized) parameter matrices

%Rotated returns
y               = V'*x; 
    
%Log likelihood
loglike         = zeros(1, T);         %Vector to hold log-likelihood contributions
sigma2          = zeros(p, p,  T);   %Array to contain time-varying covariance matrices
lambda          = zeros(p,T);        %\lambda (vector), contains time-varying eigenvalues
lambda(:,1)     = omega;


for i = 2:T 
    %conditional eigenvalues
    lambda(:,i) = omega+A*y(:,i-1).^2 + B*lambda(:,i-1) + C*(z(1,i-1)^2);            
    
    %conditional covariance matrix
    sigma2(:,:,i) = V*diag(lambda(:,i))*V'; %Save covariance estimate    
    
    %log-likelihood contributions
    loglike(i) = -p/2*log(2*pi)-0.5*sum(log(lambda(:,i)))-0.5*y(:,i)'*diag(1./lambda(:,i))*y(:,i);
    
end

persistence = max(eig(A+B)); %persistence of stochastic process

L = loglike; %Returns vector of loglikelihood contributions

end

function [gamma, V, omega, alpha, beta, c, phi, g,a,b] = EigenARCH_repar(p, n, param)
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
     
g = zeros(p,n);
g(1:p-n,:) = vec2mat(param(tal:tal+(p-n)*n-1),n).^2; %FIRST ROW FREE
g(p-n+1:end,:)=eye(n,n);
tal=tal+(p-n)*n;   

b = reshape(param(tal:tal+p*n-1),n,p)'.^2;
tal=tal+p*n; 

c = exp(param(tal:tal+p-1));

alpha    = g*a'; 
beta    = g*b'; 

gamma = [V(:); omega(:); alpha(:); beta(:); c(); phi(:)];   

end


