
# adjustment for tie-breaks
Y[Y>23]<-23
Y[deuce_index2] <- 13
O <- c()
O <- deuce
O[deuce_index] = deuce[deuce_index]-23
O[deuce_index2] = deuce[deuce_index2]-13
X = Y+O

# model
mod_volley_covariates <- "
model{
  for (g in 1:nlength_set){
      Z[g] ~ dbern(p.set[g])
      logit(p.set[g]) <- set.home+set.ab[team1[g]]-set.ab[team2[g]]+
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
      p.ab.star[t]  ~ dnorm(0, 0.001)

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
    mu ~ dnorm(0, 1)
    iota ~ dnorm(0, 1)
    eta ~ dnorm(0, 1)
    gamma ~ dnorm(0, 0.001)
}"

if (type=="mid-season"){

ngames_test <- 91
ngames_train <- 91
nset_train <- length(team1)/2
nset_test <- length(team1)/2
}else if (type=="playoff"){
  
  ## This part mimics the "volley_ita.R" file
  volley_ita_prel <- read_excel("playoff_season_2017-18_ita.xlsx")
  teams_prel <-unique(volley_ita_prel$team)
  home_team_prel <- volley_ita_prel$team[ seq(1, length(volley_ita_prel$team), by =2)]
  away_team_prel <- volley_ita_prel$team[ seq(2, length(volley_ita_prel$team), by =2)]
  match_day <- volley_ita_prel$day[ seq(1, length(volley_ita_prel$team), by =2)]
  
  
  conta_set <- c()
  for (n in seq(1, dim(volley_ita_prel)[1], by=2)){
    conta_set[n] <- sum(  !is.na(volley_ita_prel[n,4:8]))
  }
  
  conta_set <- conta_set[!is.na(conta_set)]
  
  volley_ita <- matrix(NA, 900,5)
  new_ind <- 1
  for (n in 1: length(home_team_prel)){
    volley_ita[new_ind:(new_ind+conta_set[n]-1),  1]<- rep(match_day[n], conta_set[n])
    volley_ita[new_ind:(new_ind+conta_set[n]-1),  2]<- rep(home_team_prel[n], conta_set[n])
    volley_ita[new_ind:(new_ind+conta_set[n]-1),  3 ]<- as.double(volley_ita_prel[2*n-1, 4:(3+conta_set[n])])
    volley_ita[new_ind:(new_ind+conta_set[n]-1),  4]<- rep(away_team_prel[n], conta_set[n])
    volley_ita[new_ind:(new_ind+conta_set[n]-1),  5 ]<- as.double(volley_ita_prel[2*n, 4:(3+conta_set[n])])
    
    if (conta_set[n]==5){
      volley_ita[(new_ind+conta_set[n]-1),  3 ] <- as.double(volley_ita_prel[2*n-1, (3+conta_set[n])])
      volley_ita[(new_ind+conta_set[n]-1),  5 ] <- as.double(volley_ita_prel[2*n, (3+conta_set[n])])
    }
    
    new_ind <- new_ind+length(1:conta_set[n])
    
  }
  volley_ita <- volley_ita[-c(new_ind:900),]
  
  #redefinition
  home_team <- volley_ita[,2]
  away_team <- volley_ita[,4]
  
  teams <- unique(home_team)
  team1 <- match(home_team,teams)
  team2 <- match(away_team, teams)
  nteams <- length(teams)
  cont <-1
  match_index <-c()
  match_index[1] <-1
  for (n in 2:length(volley_ita[,1])){
    if(team1[n]==team1[n-1] ){
      match_index[n]<-cont
      
    }else{
      cont <-cont+1
      match_index[n] <- cont
    }
  }
  
  
  tie_matches <- as.double(names( table(match_index))[table(match_index)==5])
  tie_index<-c()
  for (j in 1:length(tie_matches)){
    tie_index[j] <-  (1:length(team1))[match_index==tie_matches[j]][5]
  }
  
  
  volley_ita_frame= data.frame(match = match_index,
                               day = as.numeric(as.vector(volley_ita[,1])),
                               team1 = volley_ita[,2],
                               points1 = as.numeric(as.vector(volley_ita[,3])),
                               team2 = volley_ita[,4],
                               points2 = as.numeric(as.vector(volley_ita[,5])))
  
  exact_result <- cbind(volley_ita_frame$points1, volley_ita_frame$points2)
  
  #number of points for each set  for the loosing team
  Y <- apply(exact_result,1, min)  
  R <- apply(exact_result,1, max) 
  Z<- c()
  Z[volley_ita_frame$points1>volley_ita_frame$points2]<- 1
  Z[volley_ita_frame$points1 <volley_ita_frame$points2]<- 0
  point_diff <- volley_ita_frame$points1-volley_ita_frame$points2
  points_sum <- apply(exact_result, 1, sum)
  
  
  deuce_index <- (1:length(Y))[points_sum>48]
  deuce_index2 <- tie_index[(1:length(tie_index))[ points_sum[tie_index] > 28]]
  deuce_index <- c(deuce_index, deuce_index2)
  deuce_index <- sort(deuce_index)
  
  # deuce points for the loosing team
  deuce <- rep(0, length(Y))
  deuce[deuce_index] <- Y[deuce_index]
  index_loosing <- apply(exact_result,1, which.min)
  index_winning <- apply(exact_result,1, which.max)
  
  # tie breaks
  tie <- sum(conta_set==5)
  
  N <- c()
  N <- rep(25, length(team1))
  N[tie_index] <- 15 
  
  TT <- rep(23, length(team1))
  TT[tie_index] <- 13
  T <- length(unique(volley_ita_frame$day))
  home_result <- volley_ita_prel$set[seq(1,411, by=2)]
  away_result <- volley_ita_prel$set[seq(2,412, by=2)]
  binary_win <- c()
  binary_win[home_result>away_result]<-1
  binary_win[home_result<away_result]<-0
  table(binary_win)
  Y[Y>23]<-23
  Y[deuce_index2] <- 13
  O <- c()
  O <- deuce
  O[deuce_index] = deuce[deuce_index]-23
  O[deuce_index2] = deuce[deuce_index2]-13
  X = Y+O
  
  ngames_test <- 24
  ngames_train <- 182
  nset_train <- 680
  nset_test <- 98
}


# definition of training set quantities
team1_train<- team1[1:nset_train]
team1_prev<-team1[(nset_train+1):(nset_train+nset_test)]
team2_train<- team2[1:nset_train]
team2_prev<-team2[(nset_train+1):(nset_train+nset_test)]
Z_train <- Z[1:nset_train]
nlength_set_train <- nset_train
Y_train <- Y[1:nset_train]
O_train <- O[1:nset_train]
N_train <- N[1:nset_train]
TT_train <- TT[1:nset_train]


# data
data.jags.ita.train <- list(team1=team1_train, 
                      team2=team2_train,
                      nteams =nteams, 
                      Z=Z_train, 
                      nlength_set = nlength_set_train, 
                      Y = Y_train,
                      O=O_train,
                      N=N_train,
                      TT=TT_train)

# monitor
moni <- c("set.ab", "p.ab", "u", "lambda",
          "Y.rep", "O.rep", "pp", "p.set", "O", "Z", 
          "p.home", "set.home", "po", "eta", 
          "u", "mu", "iota", "gamma", "dic")

# initial parameters
init_1 <- list( p.home =0.1, set.home =0.1)
init_2 <- list( p.home =0.1, set.home =0.1)
init_3 <- list( p.home =0.1, set.home =0.1)


# fitting
volley.jags <- run.jags(model=mod_volley_covariates, 
                        data=data.jags.ita.train, 
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
eta <- ris.volley[,grep("eta",colnames(ris.volley),fixed=TRUE)][,1]
u <- ris.volley[,grep("u[",colnames(ris.volley),fixed=TRUE)]
lambda <- ris.volley[,grep("lambda[",colnames(ris.volley),fixed=TRUE)]
iota <- ris.volley[,grep("iota",colnames(ris.volley),fixed=TRUE)]
gamma <- ris.volley[,grep("gamma",colnames(ris.volley),fixed=TRUE)]

# prediction quantities definition
S <- length(gamma)
w_prev <- matrix(NA, S, nset_test)
z_prev <- matrix(NA, S, nset_test)
pp_prev <- matrix(NA, S, nset_test)
y_prev <- matrix(NA, S, nset_test)
po_prev <- matrix(NA, S, nset_test)
o_prev <- matrix(NA, S, nset_test)
l.star <- matrix(NA, S, nset_test)
lambda_prev <- matrix(NA, S, nset_test)
u_prev <- matrix(NA, S, nset_test)
tot_prev <- matrix(NA, S, nset_test)
binary_win_matrix_prev <- matrix(NA, S, ngames_test)
k_cohen <- c()
agree_prev <- c()
binary_win_test <- binary_win[(ngames_train+1):(ngames_train+ngames_test)]


team1_prev_singles <- team1_prev[cumsum(conta_set[(ngames_train+1):(ngames_train+ngames_test)])]
team2_prev_singles <- team2_prev[cumsum(conta_set[(ngames_train+1):(ngames_train+ngames_test)])]

tot_set <-list()
tot_team1_prev <-list()
tot_team2_prev <- list()
binary_win_matrix_prev <- matrix(NA, S, ngames_test)
for (i in 1:ngames_test){
 tot_set[[i]]<- c( rep(conta_set[ngames_train+i], conta_set[ngames_train+i]),
     rep(conta_set[ngames_train+i], 5-conta_set[ngames_train+i]))
 tot_team1_prev[[i]]<- c( rep(team1_prev_singles[ i]  , conta_set[ngames_train+i]),
                   rep(team1_prev_singles[i], 5-conta_set[ngames_train+i]))
 tot_team2_prev[[i]]<- c( rep(team2_prev_singles[i], conta_set[ngames_train+i]),
                          rep(team2_prev_singles[i], 5-conta_set[ngames_train+i]))
}

# tot_z_prev is 1 if home team wins, and 0 otherwise
tot_w_prev <- tot_z_prev <- array(NA, c(S, ngames_test, 5))
for (s in 1:S){
  for (nn in 1:ngames_test){
    tot_w_prev[s,nn,] <- 1/(1+exp(  -(set.home[s] + set.ab[s, tot_team1_prev[[nn]]]- set.ab[s, tot_team2_prev[[nn]]]+
                                  gamma[s]*(p.ab[s, tot_team1_prev[[nn]]]- p.ab[s, tot_team2_prev[[nn]]] )) ))
    for (h in 1:5){
    tot_z_prev[s,nn,h] <- rbinom(1, 1, tot_w_prev[s,nn,h])
      
      #vecchio metodo
      #1*(tot_w_prev[s,nn,h]>=0.5)+0*(tot_w_prev[s,nn,h]<0.5)
                         
    }
  }
  
  tot_z_prev_est <- round(apply(tot_z_prev, c(2,3), median),0)
  Z_sum_prev <- apply(tot_z_prev[s,,],1,sum)
  binary_win_matrix_prev[s,Z_sum_prev>=3]<-1
  binary_win_matrix_prev[s,Z_sum_prev <3]<-0
  
  matrix_obs_prev <- cbind(binary_win_matrix_prev[s,], binary_win_test)
  k_cohen[s] <- kappa2(matrix_obs_prev)$value
  agree_prev[s] <- agree(matrix_obs_prev)$value
}


# set agreement
set_diff <- list()
count_zero <- matrix(NA, S, ngames_test)
for(s in 1:S){
  set_diff[[1]] <- tot_z_prev[s,1,1:conta_set[ngames_train+1] ]-  Z[(nset_train+1): (nset_train+conta_set[ngames_train+1])]
  count_zero[s,1] <- sum(as.vector(set_diff[[1]])==0)
  for (nn in 2:ngames_test){
set_diff[[nn]] <-tot_z_prev[s,nn,1:conta_set[ngames_train+nn] ]-
  Z[ (nset_train+ 1  +
                        cumsum(conta_set[(ngames_train+1):(ngames_train+nn)])[nn-1]:
  (cumsum(conta_set[(ngames_train+1):(ngames_train+nn)])[nn]-1)
  )]
  
  count_zero[s,nn] <- sum(as.vector(set_diff[[nn]])==0)

}
}

sum_set<- apply(count_zero,1, sum)
nset_veri <- sum(conta_set[(ngames_train+1):(ngames_train+ngames_test)])



