EigenARCH_loglikelihood <- function(param){
  # log-likelihood function
  
  loglikelihood_fct <- EigenARCH_loglikelihood_cont(X = X, param, n = n, p = p)
  L <- mean(loglikelihood_fct$loglike)

  return(-L)
}

EigenARCH_loglikelihood_cont <- function(X, param, n, p){

  # Log likelihood contributions for the EigenARCH(1,1) moodel
    # Inputs: 
        # X: pxT matrix of asset and covariate returns returns [x, z]
        # param: vector of initial parameters
        # n: the number of factors (n<=p)   
    # Outputs:
        # L: Log likelihood value
        # sigma2: Array of filtered covariance matrices
        # lambda: pxT matrix of time varying eigenvalues
        # persistence: scalar indicating persistence of the process.
  
  # Split up data
  x <- X[1:p,]
  z <- X[p+1,]
  
  # Define constants
  N = ncol(x);
  
  # Fetch reparametrized parameter matrices
  parameter_list = EigenARCH_repar(p, n, param) 
  
  V = parameter_list$eigenvectors
  omega = parameter_list$omega %>% as.matrix()
  alpha = parameter_list$alpha %>% as.matrix()
  beta = parameter_list$beta %>% as.matrix()
  psi = parameter_list$psi %>% as.matrix()
  mu = parameter_list$mu %>% as.matrix()
  
  # Rotated returns  
  y <- t(V) %*% x
  
  #Log likelihood
  loglike         = matrix(0, nrow = 1, ncol = N)  # Vector to hold log-likelihood contributions
  sigma2          = list();                        # list to contain time-varying covariance matrices
  lambda          = matrix(0, nrow = p, ncol = N); # lambda (vector), contains time-varying eigenvalues
  lambda[,1]      = omega;
   
  for(i in 2:N){ 
    
    # conditional eigenvalues
    lambda[,i] = omega+alpha%*%(t(V)%*%x[,i-1])^2 + beta%*%lambda[,i-1] + psi%*%z[i-1]^2      
    
    # conditional covariance matrix
    sigma2[[i]] = V %*% diag(lambda[,i]) %*% t(V) # based on the spectral decomposition
    
    # log-likelihood contribution
    loglike[i] = -p/2*log(2*pi) - 1/2*sum(log(lambda[,i])) - 1/2*t(t(V)%*%(x[,i]-mu)) %*% diag(1/lambda[,i]) %*% (t(V)%*%(x[,i]-mu))
  }
  
  log_likelihood <- list(loglike = loglike, 
                         sigma2 = sigma2, 
                         lambda = lambda)
  
  return(log_likelihood)
}


EigenARCH_repar <- function(p, n, param){
# Function to reparameterize the parameter-vector to the matrices

  count <- 1
  
  # Eigenvectors
  phi <- param[count:(count + p*(p-1)/2-1)]
  phi <- exp(phi)/(1+exp(phi)) * pi/2 
  count <- count+p*(p-1)/2
  V   <- rotation(phi,p) # Rotation matrix
  
  # Constant W
  omega <- exp(param[count:(count+p-1)])
  count <- count+p
  
  # C matrix
  psi <- exp(param[count:(count+p-1)])
  count <- count + p
  
  # Reduced rank matrices
  a <- matrix(param[count:(count+p*n-1)]^2, ncol=n, byrow = TRUE)
  count <- count + p*n
  
  if(p == n){
    g = diag(1,n)
  } else{
    g <- matrix(0,p,n)
    g[1:(p-n),] <- matrix(param[count:(count+(p-n)*n-1)]^2, ncol=n, byrow = TRUE) # FIRST ROW FREE
    g[(p-n+1):end,] <- diag(n,n)
    count <- count+(p-n)*n     
  }
  
  b <- matrix(param[count:(count+p*n-1)]^2,ncol=n,nrow=p, byrow = TRUE);
  count <- count+p*n;
  
  mu = param[count:(count+p-1)]
  
  repar <- list(eigenvectors = V, 
                omega = omega, 
                alpha = a, 
                beta = b,
                psi = psi,
                mu = mu)
  
  return(repar)
}

rotation <- function(angle, p){
  
  # ROTATION Returns a pxp rotation matrix constructed in the same way as in the GO-GARCH paper by van der Weide (2002).
  
  # allocate storage for rotation matrix
  rot = diag(p)

  # Parameter counter
  tal = 1
  
  for (i in 1:(p-1)){
    for (j in (i+1):p){
      #Construct pxp matrix with 2x2 rotation spanning one dimension
      help <- diag(p)
      help[i,i] <- cos(angle[tal])
      help[j,j] <- cos(angle[tal])
      help[i,j] <- sin(angle[tal])
      help[j,i] <- -sin(angle[tal])
      
      #update rotation
      rot = rot %*% help
      tal = tal +1 # update parameter counter
    }
  }
  return(rot)
}

