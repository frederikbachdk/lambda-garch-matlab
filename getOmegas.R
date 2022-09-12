# turn off scinumbers, clear console and memory
options(scipen=999) 
cat('\014')

# import functions
source('getTheta.R') # fetches data and estimates theta

#Lambda <- readxl::read_excel("data/condEigenvalues.xlsx") %>% as.matrix()
L <- EigenARCH_loglikelihood(theta)
loglik <- EigenARCH_loglikelihood_cont(x, theta, n)
Lambda <- loglik$lambda %>% t()
colnames(Lambda) <- c('lambda1','lambda2','lambda3')

# Reparametrization of theta, save in individual matrices
parameters <- EigenARCH_repar(p, n, theta)
V <- parameters$eigenvectors
W <- parameters$omega
A <- parameters$alpha
B <- parameters$beta

#############################################################################
### create dataframes of conditional eigenvalues and covariance matrices ###
#############################################################################

# retrieve estimated lambdas
condEigenvals <- data %>% select(Date) %>% 
  filter(Date <= estimation_end) %>% 
  cbind(Lambda %>% round(10))

# estimate lambdas out of sample
for(t in (row_end+1):(ncol(x_full))){

  # set date
  condEigenvals[t,1] <- data[t,1]
  
  # calculate conditional eigenvalues
  lambda_lag <- condEigenvals[t-1,] %>% select(-Date) %>% as.matrix() %>% t()
  lambda_t <- W + A %*% (t(V) %*% x_full[,t-1])^2 + B %*% lambda_lag
  lambda_t <- lambda_t %>% t()

  # save in tibble
  condEigenvals[t,2:(2+p-1)] <- lambda_t
}

# estimate Omegas out of sample
condCovariances <- lapply(1:ncol(x_full), 
                          function(t) {
                            V %*% diag(condEigenvals[t,2:(2+p-1)]) %*% t(V)
                                      }
                          ) 

names(condCovariances) <- data$Date

# calculate sample covariance matrix
sampleCovariance <- cov(t(x))
