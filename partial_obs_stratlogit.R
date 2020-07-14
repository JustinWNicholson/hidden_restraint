################################################################################
#Justin W. Nicholson
#Extended Deterrence Project
# Execute estimtion and bootstrap via Partial Observation Strategic Probit Model 
# P.O STRATEGIC ESTIMATION, PROBIT
# Version: 1.1
################################################################################
strategic_mle = function(b, 
                         X.11,
                         X.13,
                         X.14,
                         X.23,
                         X.24,
                         d_1,
                         sigma = 1){
  d_0 = (1-d_1)
  len.11 = ncol(X.11)
  len.13 = ncol(X.13)
  len.14 = ncol(X.14)
  len.23 = ncol(X.23)
  len.24 = ncol(X.24)
  
  #create proper betas
  b.11 = b[1:(len.11)]
  b.13 = b[(1+len.11):(len.11 + len.13)]
  b.14 = b[(len.11 + len.13 + 1):(len.11 + len.13 + len.14)]
  b.23 = b[(len.11 + len.13 + len.14 + 1):(len.11 + len.13 + len.14 + len.23)]
  b.24 = b[(len.11 + len.13 + len.14 + len.23 + 1):(length(b))]
  
  #create estimates
  xb.11 = X.11 %*% b.11
  xb.13 = X.13 %*% b.13
  xb.14 = X.14 %*% b.14
  xb.23 = X.23 %*% b.23
  xb.24 = X.24 %*% b.24

 
#Define probabilities for players -- PROBIT
#player 2
p_b = pnorm((xb.24 - xb.23) / (sqrt(2)))
#p_b = (exp(xb.24) / (exp(xb.23) + exp(xb.24)))
p_nb = (1-p_b)

#player 1
p_a = pnorm((p_b*xb.14 + p_nb*xb.13 - xb.11) / sqrt(p_b^2 + p_nb^2 + 1))
#p_na = exp(xb.11) / (exp(xb.11) + exp(p_b*xb.14 + p_nb*xb.13))
p_na = 1-p_a

  #define log-liklihood function for P.O. strat logit: 
  loglik = d_0*log(1-p_a*p_b) + d_1*(log(p_a) + log(p_b))
}



############ERROR HANDLING########################################## 
#whismall_p_a = which(p_a < .Machine$double.eps)
#whismall_p_na = which(p_na < .Machine$double.eps)
#whismall_p_b  = which(p_b < .Machine$double.eps)
#whismall_p_nb = which(p_nb < .Machine$double.eps)

#p_a[whismall_p_a] <- .Machine$double.eps
#p_na[whismall_p_na] <- .Machine$double.eps
#p_b[whismall_p_b] <- .Machine$double.eps
#p_nb[whismall_p_nb] <- .Machine$double.eps
#####################################################################
#complete model:
loglik2 = 