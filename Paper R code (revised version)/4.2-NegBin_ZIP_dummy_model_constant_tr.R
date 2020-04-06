# adjustment for tie breaks
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
                      O=O,
                      N=N,
                      TT=TT)

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
      p.ab.star[t]  ~ dnorm(0, 0.5)   # tenere questa prior (per il DIC)!
      
  }
  
      set.ab[10] ~ dnorm(0, 0.5)
      set.ab[12] ~ dnorm(0, 0.5)
      
      for (t in 1:9){
        set.ab[t] <- 0
      }
      
      set.ab[11] <- 0
      set.ab[13] <- 0
      set.ab[14] <- 0
      
    p.home  ~ dnorm(0, 0.001)   # H_points
    set.home ~ dnorm(0, 0.001)  # H_set
    mu ~ dnorm(0, 1)            # mu
    iota ~ dnorm(0, 1)          # m
    eta ~ dnorm(0, 1)           # log(lambda)
    gamma ~ dnorm(0, 0.001)     # theta
}"

# monitor
moni <- c("set.ab", "p.ab", "u", "lambda",
          "Y.rep", "O.rep", "pp", "p.set", "O", "Z", 
          "p.home", "set.home", "po", "eta", 
          "u", "mu",  "iota", "gamma", "dic")

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
o.rep <- ris.volley[,grep("O.rep[",colnames(ris.volley),fixed=TRUE)]
po <- ris.volley[,grep("po[",colnames(ris.volley),fixed=TRUE)]
mu <- ris.volley[,grep("mu",colnames(ris.volley),fixed=TRUE)]
eta <- ris.volley[,grep("eta",colnames(ris.volley),fixed=TRUE)]
u <- ris.volley[,grep("u[",colnames(ris.volley),fixed=TRUE)]
lambda <- ris.volley[,grep("lambda[",colnames(ris.volley),fixed=TRUE)]
iota <- ris.volley[,grep("iota",colnames(ris.volley),fixed=TRUE)]
gamma <- ris.volley[,grep("gamma",colnames(ris.volley),fixed=TRUE)]
dic <- volley.jags$dic
pd <- as.double(volley.jags$deviance.sum[2])


# table for estimates
riass=function(x) c(mean(x), quantile(x, 0.5), sd(x),quantile(x,c(0.025,0.975)))
st  <- rbind(riass(set.home))
st <- rbind(st, riass(p.home))
st <- rbind(st, riass(gamma))
st <- rbind(st, riass(mu))
st <- rbind(st, riass(eta))
st <- rbind(st, riass(iota))
df  <- as.data.frame(st)
dimnames(df) <- list(c(expression(H[s]), expression(H[p]),
                       expression(theta),
                       expression(mu),   expression(eta), "m"
                       ),
                     #teams_trad),
                     c("Mean", "Median", "sd", "2.5%", "97.5%"))
x <- round(df,2)


# percentage change advantage (commented but in the paper)

# mean(exp(as.double(set.home)))
# sd(exp(as.double(set.home)))
# quantile(exp(as.double(set.home)), c(0.025, 0.5, 0.975))
# 
# mean(exp(as.double(p.home)))
# sd(exp(as.double(p.home)))
# quantile(exp(as.double(p.home)), c(0.025, 0.5, 0.975))

