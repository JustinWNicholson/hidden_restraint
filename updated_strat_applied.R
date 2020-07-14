################################################################################
#Justin W. Nicholson
#Nuclear Sharing Project
# Execute estimtion and bootstrap via Partial Observation Strategic Probit Model 
# Version: P.O STRATEGIC ESTIMATION, PROBIT
################################################################################
#housekeeping: 
rm(list=ls())
library(maxLik)
library(snow)
library(Rmpi)
source("~/nukeshare/scripts/partial_obs_stratlogit.R")

#set number of clusters
nclus=4

#create cluster
cl <-makeMPIcluster(nclus)
clusterSetupRNG(cl)
#alter either n or mc to affect run time
#set number of bootstraps
boots=500
#calculate bootstraps per worker node
bootsperclus=round(boots/nclus)


#load necessary libraries on the worker nodes:
clusterEvalQ(cl, library(maxLik))
clusterExport(cl, "strategic_mle")
#load data
nukeshare <- readRDS("~/nukeshare/data/share.rds")
#keep only cols necessary for estimation. This should be ignored after MI:
keep <- c("ccode1", "ccode2", "cname1", "cname2","year", "defense", "exploreB", "nptany", "disputesB", "icbm", "civcon", 
          "probprgm", "postcw", "dist1000", "eeriv", "rivdist1000", "nonukyrs", "nukshare_alt", "nukshare", "lnars_alow",
          "openness_finalB","dch5_finalB","disputes_finalB","rivalryB","cincB","borderstotalB")
nuke_share[ is.na(nuke_share) ] <- NA

nuke_share <- nuke_share[,keep]
nuke_share <- na.omit(nuke_share)


STRAT_SHARE <- function(inputs, reps)
{
  result <- NULL
  for (i in 1:reps){
    dat <- inputs[sample(nrow(inputs), nrow(inputs), replace = T), ]
    
    d_nuke <- dat$defense
    
    X.11 =  cbind(1,
                  dat$borderstotalB,
                  dat$disputes_finalB,
                  dat$dch5_finalB,
                  dat$civcon,
                  dat$disputesA
                  )
    
    X.13 =  cbind(rep(1,length(dat$nukshare_alt)))
    
    X.14 = cbind(
      dat$rivdist1000,
      dat$cincB,
      dat$dch5_finalA,
      dat$exploreB,
      dat$postcw
      )
    
    
    X.23 =  cbind(rep(1,length(dat$nukshare_alt)))
    
    X.24 =  cbind(
      dat$lnars_alow,
      dat$icbm,
      dat$nptany,
      dat$postcw,
      dat$dist1000,
      dat$rivdist1000,
      dat$dch5_finalA,
      dat$dch5_finalB,
      dat$disputes_finalB,
      dat$civcon,
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
                            iterlim = 3000 
                            #qac = "marquardt"
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

