## Sensitivity analysis

# store an array for the 8 fixed parameters: mu, H_set, H_points,
#                                            lambda, gamma, theta,
#                                            delta, m


V <- 10   # valori di input
sensitivity_array <- array(0, c(M, V, 8))
sensitivity_array_ab <- array(0, c(M, V, 28))
hyper_list <- c(0.00001, 0.0001, 0.001, 0.002, 0.01, 0.02, 0.1, 0.2, 1, 2)



# adjustment for tie breaks
Y[Y>23]<-23
Y[deuce_index2] <- 13
O <- c()
O <- deuce
O[deuce_index] = deuce[deuce_index]-23
O[deuce_index2] = deuce[deuce_index2]-13
X = Y+O



sensitivity_routine <- function(tau.sens, par.gamma.1, par.gamma.2){
  # data
  data.jags.ita <- list(team1=team1, 
                        team2=team2,
                        nteams =nteams, 
                        Z=Z, 
                        nlength_set = length(volley_ita_frame$day), 
                        Y =Y,
                        O=O,
                        N=N,
                        TT=TT,
                        tau.sens = tau.sens,
                        par.gamma.1 = par.gamma.1,
                        par.gamma.2 = par.gamma.2)
  
  # model
  mod_volley_covariates <- "
  model{
  for (g in 1:nlength_set){
  Z[g] ~ dbern(p.set[g])
  logit(p.set[g]) <- set.home+
  set.ab[team1[g]]-set.ab[team2[g]]+
  gamma*(p.ab[team1[g]]-p.ab[team2[g]])
  X[g] <- step(TT[g]-(Y[g]+O[g]))*(Y[g])+(1-step(TT[g]-(Y[g]+O[g])))*(Y[g]+O[g])
  Y[g]  ~ dnegbin(1-pp[g], N[g] )T(0, TT[g])
  logit(pp[g]) <- mu+(1-Z[g])*p.home+(p.ab[team1[g]]-p.ab[team2[g]])*(1-2*Z[g])
  Y.rep[g] ~ dnegbin(1-pp[g], N[g] )T(0, TT[g])
  O[g] ~ dpois(lambda[g])
  lambda[g] <- (1-u[g])*lambda.star[g]+0.1
  u[g] ~ dbern(po[g])
  logit(po[g]) <- iota 
  log(lambda.star[g]) <- eta
  O.rep[g] ~ dpois(lambda[g])
  }
  
  for ( t in 1:nteams){
  p.ab[t] <- p.ab.star[t]-mean(p.ab.star[])
  p.ab.star[t]  ~ dnorm(0, tau.p)
  
  }
  
  set.ab[10] ~ dnorm(0, tau.set)
  set.ab[12] ~ dnorm(0, tau.set)
  
  for (t in 1:9){
  set.ab[t] <- 0
  }
  
  set.ab[11] <- 0
  set.ab[13] <- 0
  set.ab[14] <- 0
  
  p.home  ~ dnorm(0, tau.sens)   # H_points
  set.home ~ dnorm(0, tau.sens)  # H_set 
  mu ~ dnorm(0, tau.sens)        # mu
  iota ~ dnorm(0, tau.sens)      # m
  eta ~ dnorm(0, tau.sens)       # log(lambda)
  alpha ~ dnorm(0, tau.sens)     # delta
  beta ~ dnorm(0, tau.sens)      # gamma
  gamma ~ dnorm(0, tau.sens)     # theta

  tau.p ~ dgamma(par.gamma.1, par.gamma.2) 
  tau.set ~ dgamma(par.gamma.1, par.gamma.2)
  }"

# monitor
moni <- c("set.ab", "p.ab", "u", "lambda",
          "Y.rep", "O.rep", "pp", "p.set", "O", "Z", 
          "p.home", "set.home", "po", "eta", 
          "u", "mu", "alpha", "beta", "iota", "gamma", "dic")

# initial parameters
init_1 <- list( p.home =0.1, set.home =0.1)
init_2 <- list( p.home =0.1, set.home =0.1)
init_3 <- list( p.home =0.1, set.home =0.1)


# fitting
volley.jags <- run.jags(model=mod_volley_covariates, 
                        data=data.jags.ita, 
                        monitor=moni, 
                        inits=list(init_1, init_2, init_3), 
                        n.chains=3, 
                        method="rjags",
                        plots=FALSE, thin=2,
                        adapt=A, sample=M, burnin=B)

ris.volley <- volley.jags$mcmc[[1]]
set.ab <-ris.volley[,grep("set.ab[",colnames(ris.volley),fixed=TRUE)] 
p.ab   <-ris.volley[,grep("p.ab[",colnames(ris.volley),fixed=TRUE)]
p.home   <-ris.volley[,grep("p.home",colnames(ris.volley),fixed=TRUE)]
set.home <-ris.volley[,grep("set.home",colnames(ris.volley),fixed=TRUE)] 
mu <- ris.volley[,grep("mu",colnames(ris.volley),fixed=TRUE)]
eta <- ris.volley[,grep("eta",colnames(ris.volley),fixed=TRUE)]
delta <- ris.volley[,grep("alpha",colnames(ris.volley),fixed=TRUE)]
gamma <- ris.volley[,grep("beta",colnames(ris.volley),fixed=TRUE)]
lambda <- ris.volley[,grep("lambda[",colnames(ris.volley),fixed=TRUE)]
m <- ris.volley[,grep("iota",colnames(ris.volley),fixed=TRUE)]
theta <- ris.volley[,grep("gamma",colnames(ris.volley),fixed=TRUE)]


return(list(set.ab = set.ab,
            p.ab = p.ab,
            p.home = p.home,
            set.home = set.home,
            mu = mu,
            m = m,
            eta = eta,
            delta = delta,
            gamma = gamma,
            theta =theta))

}



# lancio la routine per i V valori degli iperparametri, tengo fermi 
# par.gamma.1 e par.gamma.2
for (j in 1:V){
  func <- sensitivity_routine(tau.sens = hyper_list[j],
                              par.gamma.1 = 0.001,
                              par.gamma.2 = 0.001)
  sensitivity_array[,j,1] <- func$p.home
  sensitivity_array[,j,2] <- func$set.home
  sensitivity_array[,j,3] <- func$mu
  sensitivity_array[,j,4] <- func$m
  sensitivity_array[,j,5] <- func$eta[,1]
  sensitivity_array[,j,6] <- func$delta
  sensitivity_array[,j,7] <- func$gamma
  sensitivity_array[,j,8] <- func$theta
}

# lancio la routine per i V valori degli iperparametri, tengo fermo tau.sens


for (j in 1:V){
  func <- sensitivity_routine(tau.sens = 1,
                              par.gamma.1 = hyper_list[j],
                              par.gamma.2 = hyper_list[j])
  sensitivity_array_ab[,j,1:14] <- func$p.ab
  sensitivity_array_ab[,j,15:28] <- func$set.ab
}





