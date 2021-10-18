######################################################################## 
############################### ACCEPT - adj  ##########################
######################################################################## 

library(predtools)
library(pROC)

source("functions.R")


##### All exacerbations

## 3trials => TORCH (no hist)
EstOR_compare(mu = 0.562, q = 0.341, pi_dev_var = 0.025)


##### Severe exacerbations

## 3trials => TORCH (no hist)
EstOR_compare(mu = 0.183, q = 0.079, pi_dev_var = 0.024)

