###############################################################################
### This script produces the full analysis of EMBIG returns with the MGARCH ###
###############################################################################

###### IMPORT AND CALCULATE ALL NUMBERS ######

# import functions and packages
source('utils/wrapperFunctions.R')
source('utils/estimationFunctions.R')
source('getWeights.R')
import_packages()

# import data and parameters
data <- import_data(estimation_start = '2010-01-01')
theta <- import_theta()

# calculate conditional covariance matrices
condEstimates <- calculate_omegas(data, theta, estimation_end = '2018-12-31')
condOmegas <- condEstimates$Omega
condLambdas <- condEstimates$Lambda

# calculate optimal portfolio weights



###### PLOT ESTIMATION RESULTS ######