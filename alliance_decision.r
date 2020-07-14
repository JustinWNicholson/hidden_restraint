################################################################################
#Justin W. Nicholson
#Extended Deterrence Project
# Execute estimtion and bootstrap via Partial Observation Strategic Probit Model 
# Version: P.O STRATEGIC ESTIMATION, PROBIT
################################################################################

#load necessary packages
library(snow)
library(Rmpi)
source("~/nukeshare/scripts/partial_obs_stratlogit.R")

#set number of clusters
nclus=10

#create cluster
cl <-makeMPIcluster(nclus)
clusterSetupRNG(cl)
#alter either n or mc to affect run time
#set number of bootstraps
boots=2000
#calculate bootstraps per worker node
bootsperclus=round(boots/nclus)


#load necessary libraries on the worker nodes:
clusterEvalQ(cl, library(maxLik))
clusterExport(cl, "strategic_mle")


##############FOR SINGLE COMPUTER ONLY#######
dat <- nuke_share[nuke_share$ccode_patron == 2,]
test3 <-     maxLik(logLik = strategic_mle, 
                    d_1 = d_defen,
                    X.11 = X.11,
                    X.13 = X.13, 
                    X.14 = X.14,
                    X.23 = X.23,
                    X.24 = X.24, 
                    start = runif(ncol(X.11) + ncol(X.13) + ncol(X.14) + ncol(X.23) + ncol(X.24))*0.1,
                    method = "BFGS", 
                    iterlim = 3000, 
                    qac = "marquardt"
)
###########################################################
#####################CLUSTER CODE##########################
###########################################################

STRAT_SHARE <- function(inputs, reps){
  result <- NULL
  for (i in 1:reps){
    dat <- inputs[sample(nrow(inputs), nrow(inputs), replace = T), ]
    
    d_nuke <- dat$nukshare
    d_defen <- dat$defense
    
    X.11 =  cbind(
      dat$cincA,
      dat$lnars_alow,
      dat$postcw,
      dat$nptany,
      dat$icbm
    )
    
    X.13 =  cbind(rep(1,length(dat$nukshare_alt)))
    
    X.14 = cbind(1,
                 dat$Nu_lag,
                 dat$disputes_finalB,
                 dat$civcon,
                 dat$cincB,
                 #dat$exploreB,
                 dat$rivdist1000,
                 dat$borderstotalB,
                 dat$dist1000
    )
    
    
    X.23 =  cbind(rep(1,length(dat$nukshare_alt)))
    
    X.24 =  cbind(
      dat$disputes_finalB,
      #dat$civcon,
      dat$rivdist1000,
      #dat$rivalryA,
      dat$cincB,
      dat$cincA,
      dat$Nu_lag,
      dat$borderstotalB
    )
    
    nuke_estimate <- maxLik(logLik = strategic_mle, 
                            d_1 = d_nuke,
                            X.11 = X.11,
                            X.13 = X.13, 
                            X.14 = X.14,
                            X.23 = X.23,
                            X.24 = X.24, 
                            start = runif(ncol(X.11) + ncol(X.13) + ncol(X.14) + ncol(X.23) + ncol(X.24))*0.1,
                            method = "BFGS", 
                            iterlim = 3000, 
                            qac = "marquardt"
    )
    errors <- sqrt(diag(vcov(nuke_estimate)))
    
    if (length(which(is.nan(errors))) == 0){
      result <-  rbind(result, nuke_estimate$estimate)
    } else{
      result <- rbind(result)
    }
  }
  return(result)
  
}
b <- clusterCall(cl = cl,
                 fun = STRAT_SHARE,
                 dat = nuke_share,
                 reps = bootsperclus)

saveRDS(b, "~/nukeshare/output.rds")
stopCluster(cl)