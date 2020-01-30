# adjustment for tie-breaks
Y[Y>23]<-23
Y[deuce_index2] <- 13
O <- c()
O <- deuce
O[deuce_index] = deuce[deuce_index]-23
O[deuce_index2] = deuce[deuce_index2]-13
X = Y+O

# data
data.jags.ita <- list(team1=team1, 
                      team2=team2,
                      nteams =nteams, 
                      Z=Z, 
                      nlength_set = length(volley_ita_frame$day), 
                      Y =Y,
                      day=volley_ita_frame$day,
                      T=T,
                      O =O,
                      N = N,
                      TT = TT)

# model
mod_volley_ZIP_dynamic <- "
  model{
  for (g in 1:nlength_set){
      Z[g] ~ dbern(p.set[g])
      logit(p.set[g]) <- set.home+set.ab[team1[g]]-set.ab[team2[g]]+
                          gamma*(p.ab[team1[g], day[g]]-p.ab[team2[g], day[g]])
      X[g] <- step(TT[g]-(Y[g]+O[g]))*(Y[g])+(1-step(TT[g]-(Y[g]+O[g])))*(Y[g]+O[g])
      Y[g]  ~ dnegbin(1-pp[g], N[g] )T(0, TT[g])
      logit(pp[g]) <- mu+(1-Z[g])*p.home+(p.ab[team1[g], day[g]]-p.ab[team2[g], day[g]])*(1-2*Z[g])
      Y.rep[g] ~ dnegbin(1-pp[g], N[g] )T(0, TT[g])
      O[g] ~ dpois(lambda[g])
      lambda[g] <- (1-u[g])*lambda.star[g]+0.1
      u[g] ~ dbern(po[g])
      log(lambda.star[g]) <- eta
      logit(po[g]) <- mu + alpha*( p.ab[team1[g], day[g]]-p.ab[team2[g], day[g]])     
                         + beta*(set.ab[team1[g]]-set.ab[team2[g]])
      
      
}

  for ( t in 1:nteams){
      p.ab[t,1] <- p.ab.star[t,1]-mean(p.ab.star[,1])
      p.ab.star[t,1]  ~ dnorm(0, tau.p)
      
      
      for (h in 2:T){
        p.ab.star[t,h] ~ dnorm( p.ab.star[t, h-1], tau.p)
        p.ab[t,h] <- p.ab.star[t,h]-mean(p.ab.star[,h])
        }
  }
      set.ab[10] ~ dnorm(0, 0.5)
      set.ab[12] ~ dnorm(0, 0.5)

      for (t in 1:9){
          set.ab[t] <- 0
        }

    set.ab[11] <- 0
    set.ab[13] <- 0
    set.ab[14] <- 0
    p.home  ~ dnorm(0, 0.001)
    set.home ~ dnorm(0, 0.001)
    mu ~ dnorm(0, 0.001)
    eta ~ dnorm(0, 1) 
    alpha ~ dnorm(0, 0.001)
    beta ~ dnorm(0, 0.001) 
    tau.p ~ dgamma(0.001, 0.001) 
    gamma ~ dnorm(0, 0.001)
  }"

# monitor
moni <- c("set.ab", "p.ab",
          "Y.rep", "pp", "p.set", "O", "Z", 
          "p.home", "set.home", "po", "eta", 
          "u", "mu", "tau.p", "dic")

# initial parameters
init_1 <- list( p.home =0.1, set.home =0.1, eta =1)
init_2 <- list( p.home =0.1, set.home =0.1, eta =1)
init_3 <- list( p.home =0.1, set.home =0.1, eta =1)


# fitting
volley.jags <- run.jags(model=mod_volley_ZIP_dynamic, 
                        data=data.jags.ita, 
                        monitor=moni, 
                        inits=list(init_1, init_2, init_3), 
                        n.chains=3, 
                        method="rjags",
                        plots=FALSE, thin=2,adapt=A, sample=M, burnin=B)


# posterior extraction
ris.volley <- volley.jags$mcmc[[1]]
set.ab <-ris.volley[,grep("set.ab[",colnames(ris.volley),fixed=TRUE)] 
p.ab   <-ris.volley[,grep("p.ab[",colnames(ris.volley),fixed=TRUE)]
p.set <-ris.volley[,grep("p.set[",colnames(ris.volley),fixed=TRUE)] 
p.home   <-ris.volley[,grep("p.home",colnames(ris.volley),fixed=TRUE)]
set.home <-ris.volley[,grep("set.home",colnames(ris.volley),fixed=TRUE)] 
pp   <-ris.volley[,grep("pp[",colnames(ris.volley),fixed=TRUE)]
o   <-ris.volley[,grep("O[",colnames(ris.volley),fixed=TRUE)]
z   <-ris.volley[,grep("Z[",colnames(ris.volley),fixed=TRUE)]
y.rep <-ris.volley[,grep("Y.rep[",colnames(ris.volley),fixed=TRUE)] 
po <- ris.volley[,grep("po[",colnames(ris.volley),fixed=TRUE)]
mu <- ris.volley[,grep("mu",colnames(ris.volley),fixed=TRUE)]
tau.p <- ris.volley[,grep("tau.p",colnames(ris.volley),fixed=TRUE)]
dic <- volley.jags$dic
pd <- as.double(volley.jags$deviance.sum[2])



