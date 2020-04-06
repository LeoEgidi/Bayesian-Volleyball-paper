# data
data.jags.ita <- list(team1=team1, 
                      team2=team2,
                      nteams =nteams, 
                      Z=Z, 
                      nlength_set = length(volley_ita_frame$day), 
                      Y =Y, 
                      TT = TT)

# model
mod_volley_poisson <- "
  model{
  for (g in 1:nlength_set){
      Z[g] ~ dbern(p.set[g])
      logit(p.set[g]) <- set.home+set.ab[team1[g]]-set.ab[team2[g]]
      Y[g]  ~ dpois(lambda[g])
      log(lambda[g]) <- mu+(1-Z[g])*p.home+(p.ab[team1[g]]-p.ab[team2[g]])*(1-2*Z[g]) +log(TT[g])
      Y.rep[g] ~ dpois(lambda[g] )
      
}

  for ( t in 1:nteams){
      p.ab[t] <- p.ab.star[t]-mean(p.ab.star[])
      set.ab[t] <- set.ab.star[t]-mean(set.ab.star[])
      p.ab.star[t]  ~ dnorm(0, 0.5)
      set.ab.star[t]~ dnorm(0, 0.5)
 }
    p.home  ~ dnorm(0, 0.001)
    set.home ~ dnorm(0, 0.001)
    mu ~ dnorm(0, 0.001)
}"

# monitor
moni <- c("set.ab", "p.ab", "Y.rep", 
          "p.set", "Z", 
          "p.home", "set.home", "mu", "dic")

# initial parameters
init_1 <- list( p.home =0.1, set.home =0.1)
init_2 <- list( p.home =0.1, set.home =0.1)
init_3 <- list( p.home =0.1, set.home =0.1)

# fitting
volley.jags <- run.jags(model=mod_volley_poisson, 
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
mu   <-ris.volley[,grep("mu",colnames(ris.volley),fixed=TRUE)]
dic <- volley.jags$dic
pd <- as.double(volley.jags$deviance.sum[2])


# table for estimates
riass=function(x) c(mean(x), quantile(x, 0.5), sd(x),quantile(x,c(0.025,0.975)))

st  <- rbind(riass(set.home))
st  <- rbind(st, t(apply(set.ab,2, riass)))
df  <- as.data.frame(st)
dimnames(df) <- list(c("H", teams), c("Mean", "Median", "sd", "2.5%", "97.5%"))
x <- round(df,2)
