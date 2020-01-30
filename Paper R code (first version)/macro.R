# Description: you should run this file to reproduce all the paper plots 
# and results. This file is linked to other R files.

# import packages
library(readxl)
library(runjags)
library(R2jags)   
library(MCMCpack) 
library(rjags)
library(R2WinBUGS)
library(readr)
library(arm)
library(matrixStats)
library(xtable)
library(ggplot2)
library(bayesplot)
library(dplyr)
library(reshape2)
library(truncdist)
library(irr)


# default values for MCMC sampling
A <- 250  # adaptation phase for jags MCMC sampling
B <- 100  # burn-in for jags MCMC sampling
M <- 650  # MCMC iterations for jags MCMC sampling


################
### FIGURE 1
################

# plots for the expected points of a truncated negative binomial
mean_func <- function(x){
  25*(1-x)/x -
    (24*dnbinom(24, size=25, prob=x))/
    (x*pnbinom(23, size=25, prob =x))
}

mean_func_untr <- function(x){25*(1-x)/x}


pdf(file="Negative_expected.pdf", width=6, height =4)
curve(mean_func, 0.1, 0.9, ylab ="Expected points for the loosing team",
      xlab ="Success point prob. for the winning team", main = "Truncated Neg Binomial",
      ylim=c(0, 25))
abline(h =23, lty =2)
dev.off()



##############
### FIGURE 2
#############

# standard deviation for the random effect model
source("volley_ita.R")
source("3.2-NegBin_ZIP_model_re_tr.R")
par(mfrow=c(1,1), mar=c(6.5,5,2,1))
pdf(file="sigma_epsilon_re.pdf", width =10, height =7)
par(mfrow=c(1,1), mar=c(6.5,5,2,1))
plot(density(sigma),  main ="",
     xlab =expression(sigma[epsilon]), cex.lab =1.5)
dev.off()


################
### TABLE 1
###############

# dic and effective parameters for basic models (section 3)
dic_vector <- c()
pd_vector <- c()

# 1) poisson with different abilities
source("volley_ita.R")
source("3.4-Poisson_model.R")
dic_vector[1] <- as.double(dic$dic)
pd_vector[1] <- pd

# 2) neg_bin with different abilities
source("volley_ita.R")
source("3.1-NegBin_model.R")
dic_vector[2] <- as.double(dic$dic)
pd_vector[2] <- pd


# 3) ZIP neg_bin with different abilities
source("volley_ita.R")
source("3.3-NegBin_ZIP_model_tr.R")
dic_vector[3] <- as.double(dic$dic)
pd_vector[3] <- pd

# 4) ZIP neg_bin with random effect
source("volley_ita.R")
source("3.2-NegBin_ZIP_model_re_tr.R")
dic_vector[4] <- as.double(dic$dic)
pd_vector[4] <- pd


dic_table_1 <- data.frame( pd = pd_vector[1:4],
                            dic= dic_vector[1:4])
colnames(dic_table_1) <- c( "# eff.par", "DIC")
rownames(dic_table_1) <- c("Poisson", "Tr. Neg. binomial",
                           "ZIP Tr. Neg. bin.",
                           "ZIP Tr. Neg. bin. r.e.")


################
### TABLE 2
###############

# dic and effective parameters extended models (section 4)
# 5) ZIP neg_bin connected abilities
source("volley_ita.R")
source("4.1-NegBin_ZIP_connected_model_tr.R")
dic_vector[5] <- as.double(dic$dic)
pd_vector[5] <- pd

# 6) ZIP neg_bin only point abilities
source("volley_ita.R")
source("4.1-NegBin_ZIP_commonpoint_model_tr.R")
dic_vector[6] <- as.double(dic$dic)
pd_vector[6] <- pd

# 7 ZIP neg_bin dummy with connected abilities
# and extra set abilities for verona and padova
dummy_index <- c(rep(0,9), 1, 0,1,0,0)
source("volley_ita.R")
source("4.1-NegBin_ZIP_dummy_model_tr.R")
dic_vector[7] <- as.double(dic$dic)
pd_vector[7] <- pd
table_est <- x

# 8) ZIP neg_bin dummy with connected abilities
# and extra set abilities for verona only
dummy_index <- c(rep(0,9), 1, 0,0,0,0)
source("volley_ita.R")
source("4.1-NegBin_ZIP_dummy_model_Verona_tr.R")
dic_vector[8] <- as.double(dic$dic)
pd_vector[8] <- pd

# 9) ZIP neg_bin dynamic with only the point ab dynamic
source("volley_ita.R")
source("4.2-NegBin_ZIP_dynamic_model_tr.R")
dic_vector[9] <- as.double(dic$dic)
pd_vector[9] <- pd


dic_table_2 <- data.frame( pd = pd_vector[5:9],
                           dic= dic_vector[5:9])
colnames(dic_table_2) <- c( "# eff.par", "DIC")
rownames(dic_table_2) <- c("ZIP Tr. Neg. bin. conn. (v1=1, v2=1)",
                           "ZIP Tr. Neg. bin. conn. (v1=0, v2=1) ",
                           "ZIP Tr. Neg. bin. conn. and set extra Verona and Padova (v1=1, v2=1)",
                           "ZIP Tr. Neg. bin. conn. and set extra Verona  (v1=1, v2=1)",
                           "ZIP Tr. Neg. bin. conn. and dynamic point abilities")



##############
### FIGURE 3
##############

# plot of dynamic abilities (model 9)
source("volley_ita.R")
source("4.2-NegBin_ZIP_dynamic_model_tr.R")
season_unique=1:T
p_med=apply(p.ab,2, median)
p_025=apply(p.ab, 2, function(x) quantile(x, 0.025))
p_975=apply(p.ab, 2, function(x)  quantile(x, 0.975))
p_025_mat=p_975_mat=p_50_mat=matrix(NA, nteams, T)

for (t in 1:T){
  p_025_mat[,t]=as.vector(p_025[  (t*nteams-((nteams-1))):(t*nteams)])
  p_50_mat[,t]=as.vector(p_med[  (t*nteams-((nteams-1))):(t*nteams)])
  p_975_mat[,t]=as.vector(p_975[  (t*nteams-((nteams-1))):(t*nteams)])
}


mt_p_025=melt(p_025_mat)
mt_p_50=melt(p_50_mat)
mt_p_975=melt(p_975_mat)
teams_fac_rep=rep(teams, T)
day_rep=rep(1:T, each=length(teams))


p_data=data.frame(
  teams=teams_fac_rep,
  day=day_rep,
  mid=mt_p_50$value,
  lo=mt_p_025$value,
  hi=mt_p_975$value
)

position_lookup <-
  p_data %>%
  group_by(teams) %>%
  summarise(pos=first(teams))

pdf(file="NegBin_ZIP_dynamic_abilities.pdf", 8,6)
ggplot() +
  geom_ribbon(
    aes(x = day, ymin = lo, ymax = hi),
    data = p_data,
    fill = color_scheme_get("red")[[1]]
  )+
  geom_line(
    aes(x = day, y = mid),
    data = p_data,
    size = 1,
    color = color_scheme_get("red")[[4]]
  )+
  scale_color_manual(values = c(color_scheme_get("blue")[[4]],
                                color_scheme_get("red")[[4]]))+
  facet_wrap("teams", scales = "free")+
  labs(x = "Day", y = "Teams' effects",
       title = "Point abilities (95% posterior bars)",
       subtitle = "for teams of Italian SuperLega 2017/2018") +
  yaxis_text(size=rel(1.2))+
  xaxis_text( size = rel(1.2))
dev.off()


##################
### FIGURE 4
##################

# posterior for standard deviation of dynamic point abilities (model 9)
sigma_beta <- 1/tau.p
par(mfrow=c(1,1), mar=c(6.5,5,2,1))
pdf(file="sigma_beta_dynamic.pdf", width =10, height =7)
par(mfrow=c(1,1), mar=c(6.5,5,2,1))
plot(density(sigma_beta),  main ="",
     xlab =expression(sigma[beta]), cex.lab =1.5)
dev.off()


###################
### TABLE 4
###################

# posterior estimates for model 7
table_est   # previously stored


####################
### FIGURE 5
###################

# posterior 95% intervals plot for abilities of model 5 (connected abilities)
source("volley_ita.R")
source("4.1-NegBin_ZIP_connected_model_tr.R")
set.ab.mean <- apply(set.ab, 2, mean)
set.ab.sd <- apply(set.ab, 2, sd)
set.ab.025 <- apply(set.ab, 2, function(x) quantile(x, 0.025))
set.ab.975 <- apply(set.ab, 2, function(x) quantile(x, 0.975))
p.ab.mean <- apply(p.ab, 2, mean)
p.ab.sd <- apply(p.ab, 2, sd)
p.ab.025 <- apply(p.ab, 2, function(x) quantile(x, 0.025))
p.ab.975 <- apply(p.ab, 2, function(x) quantile(x, 0.975))
real_teams<- c("Sir Safety Perugia", "Cucine Lube Civitanova",
               "Azimut Modena", "Diatec Trentino",
               "Calzedonia Verona", "Revivre Milano",
               "Wixo LPR Piacenza", "Bunge Ravenna",
               "Kioene Padova", "Gi Group Monza",
               "Taiwan Exc. Latina", "Callipo Vibo Valentia",
               "Biosì Sora", "BCC Castellana Grotte")
ord<-match(real_teams, teams)
par(mfrow=c(1,2), oma =c(1,1,1,1))
pdf(file= "NegBin_ZIP_connected_abilities.pdf", width =10, height =5)
par(mfrow=c(1,2), oma =c(1,1,1,1))
coefplot(rev(set.ab.mean[ord]), rev(set.ab.sd[ord]), CI=2,
         varnames=rev(real_teams), main="Set abilities (95% post. intervals)\n",
         cex.var=1, mar=c(1,7,4,2), lwd=2,
         cex.main=0.9,pch=16, col="red")
coefplot(rev(p.ab.mean[ord]), rev(p.ab.sd[ord]), CI=2,
         varnames=rev(real_teams), main="Point abilities (95% post. intervals)\n",
         cex.var=1, mar=c(1,7,4,2), lwd=2,
         cex.main=0.9,pch=16, col="red")
dev.off()


###################
### FIGURE 6
##################

# posterior 95% intervals plot for abilities of model 7 (connected abilities and set extra
# for Verona and Padova)
dummy_index <- c(rep(0,9), 1, 0,1,0,0)
source("volley_ita.R")
source("4.1-NegBin_ZIP_dummy_model_tr.R")
set.ab.mean <- apply(set.ab, 2, mean)
set.ab.sd <- apply(set.ab, 2, sd)
p.ab.mean <- apply(p.ab, 2, mean)
p.ab.sd <- apply(p.ab, 2, sd)
real_teams<- c("Sir Safety Perugia", "Cucine Lube Civitanova",
               "Azimut Modena", "Diatec Trentino",
               "Calzedonia Verona", "Revivre Milano",
               "Wixo LPR Piacenza", "Bunge Ravenna",
               "Kioene Padova", "Gi Group Monza",
               "Taiwan Exc. Latina", "Callipo Vibo Valentia",
               "Biosì Sora", "BCC Castellana Grotte")
ord<-match(real_teams, teams)
par(mfrow=c(1,2), oma =c(1,1,1,1))
pdf(file= "NegBin_ZIP_dummy_abilities.pdf", width =10, height =5)
par(mfrow=c(1,2), oma =c(1,1,1,1))
coefplot(rev(set.ab.mean[ord]), rev(set.ab.sd[ord]), CI=2,
         varnames=rev(real_teams), main="Set abilities (95% post. intervals)\n",
         cex.var=1, mar=c(1,7,4,2), lwd=2,
         cex.main=0.9,pch=16, col="red")
coefplot(rev(p.ab.mean[ord]), rev(p.ab.sd[ord]), CI=2,
         varnames=rev(real_teams), main="Point abilities (95% post. intervals)\n",
         cex.var=1, mar=c(1,7,4,2), lwd=2,
         cex.main=0.9,pch=16, col="red")
dev.off()


##################
### TABLE 5
#################

# League reconstruction from MCMC sampling
dummy_index <- c(rep(0,9), 1, 0,1,0,0)
source("volley_ita.R")
source("4.1-NegBin_ZIP_dummy_model_tr.R")

real_points <- c(70, 64, 60, 51, 50, 44, 42, 41, 35,
                  28, 25, 13, 13, 10)
real_teams<- c("Sir Safety Perugia", "Cucine Lube Civitanova",
                "Azimut Modena", "Diatec Trentino",
                "Calzedonia Verona", "Revivre Milano",
                "Wixo LPR Piacenza", "Bunge Ravenna",
                "Kioene Padova", "Gi Group Monza",
                "Taiwan Exc. Latina", "Callipo Vibo Valentia",
                "Biosì Sora", "BCC Castellana Grotte")

match_alloc <- array(0,
                     dim=c(M,
                           length(unique(match_index)),
                           2))
point_alloc = point_real_alloc_2 =
  array(0, dim=c(M, length(unique(match_index)),2))
point_real_alloc =  matrix(0, 182,2)
class <- array(0, c(M,14, length(unique(match_index))))

for ( s in 1:M){
  for (n in 1: length(unique(match_index))){

    match_alloc[s, n,1] <- sum(z[s,match_index==n ])
    match_alloc[s, n,2] <- sum(1-z[s,match_index==n])

    point_alloc[s, n,1] <- sum( (y.rep[s, match_index==n]+o.rep[s, match_index==n])%*%(1-z[s,match_index==n ])+ (25+o.rep[s, match_index==n])%*%z[s,match_index==n ])
    point_alloc[s, n,2] <- sum((y.rep[s, match_index==n]+o.rep[s, match_index==n])%*%(z[s,match_index==n ])+ (25+o.rep[s, match_index==n])%*%(1-z[s,match_index==n ]))

    point_real_alloc[ n,1] <- sum( volley_ita_frame$points1[match_index==n])
    point_real_alloc[ n,2] <- sum(volley_ita_frame$points2[match_index==n])

    point_real_alloc_2[s, n,1] <- sum( ( (y.rep[s, match_index==n]+o.rep[s, match_index==n])*(1-z[s,match_index==n ])+ (25+o.rep[s, match_index==n])*z[s,match_index==n ]-volley_ita_frame$points1[match_index==n])^2)
    point_real_alloc_2[s, n,2] <- sum((  (y.rep[s, match_index==n]+o.rep[s, match_index==n])*(z[s,match_index==n ])+ (25+o.rep[s, match_index==n])*(1-z[s,match_index==n ])-volley_ita_frame$points2[match_index==n]   )^2 )



    if (match_alloc[s, n,1] -
        match_alloc[s, n,2]>=2){
      class[s, unique(team1[match_index==n]) ,n]<-3
      class[s, unique(team2[match_index==n])  ,n]<-0
    }else if ( match_alloc[s, n,1] -
               match_alloc[s, n,2]==1  ){
      class[s, unique(team1[match_index==n]),n]<-2
      class[s, unique(team2[match_index==n]),n]<-1
    }else if ( match_alloc[s, n,1] -
               match_alloc[s, n,2]<=-2  ){
      class[s, unique(team1[match_index==n]),n]<-0
      class[s, unique(team2[match_index==n]),n]<-3

    }else if( match_alloc[s, n,1] -
              match_alloc[s, n,2]==-1  ){
      class[s, unique(team1[match_index==n]),n]<-1
      class[s, unique(team2[match_index==n]),n]<-2
    }

      }
   }

class_fin <- apply(class, c(1,2), sum)
class_fin_mean <- apply(class_fin,2, mean)
class_fin_025 <- apply(class_fin, 2,  function(x) quantile(x, 0.25))
class_fin_075 <- apply(class_fin, 2,  function(x) quantile(x, 0.75))
cbind(class_fin_mean, class_fin_025, class_fin_075)

exp.points <- sort(class_fin_mean, decreasing =TRUE, index.return=TRUE)$x
final_teams <- teams[sort(class_fin_mean, decreasing =TRUE, index.return=TRUE)$ix]
real_rank<- match(final_teams, real_teams)

df2<-as.data.frame(cbind(final_teams, exp.points,
      real_points[real_rank],
      real_rank))
dimnames(df2) <- list(c(1:14), c( "Teams", "Exp. Points",
                             "Actual points",
                             "Actual rank"))
class_table <- xtable(df2)


###################
### FIGURE 7
##################

# Marginal PP checks for the points
G <- length(volley_ita_frame$day)
diff <- volley_ita_frame$points1-volley_ita_frame$points2
diff_rep <- matrix(NA, M, G)
for (s in 1:M){
  for ( g in 1:G){
    diff_rep[s,g] <- (y.rep[s, g]+o.rep[s, g])*(1-z[s,g ])+ (25+o.rep[s, g])*z[s,g ]- (y.rep[s, g]+o.rep[s, g])*(z[s,g ]) - (25+o.rep[s, g])*(1-z[s,g ])
}}

pdf(file="ppc_densities.pdf", height = 6, width =7)
ppc_dens_overlay(diff, diff_rep, bw=0.7)+
  xaxis_text(on =TRUE, size=rel(1.9))+
  yaxis_text(on =TRUE, size=rel(1.9))+
  legend_text(size=rel(2))
dev.off()




### Here is the out-of sample part (Section 5), splitted in
# mid-season predictions and playoff predictions. Firstly, the mid-season


##################
### FIGURE 8
##################

# 8a: posterior agreemeent for the mid-season
type="mid-season"  # type of prediction
dummy_index <- c(rep(0,9), 1, 0,1,0,0)
source("volley_ita.R")
source("5.4-Out_of_sample predictions.R")
pdf(file=paste("agree_", type, ".pdf", sep=""), width =10, height =6.5)
par(mfrow=c(1,1), mar=c(5,5,3,1))
plot(density(agree_prev, bw=2.5),   xlab ="% of agreement",
     main =paste("Test set: ", type), cex.main =2.1,lwd=2, cex.lab =2.4)
dev.off()


#################
### FIGURE 9
#################

# rank plot with posterior 95% intervals and observed points
for ( s in 1:S){  # compute predictive quantities
  for (n in 1:nset_test){
    po_prev[s,n] <- 1/(1+exp(-(iota[s]
                               +alpha[s]*( p.ab[s, team1_prev[n]]-p.ab[s, team2_prev[n]])
                               + beta[s]*(set.ab[s, team1_prev[n]]-set.ab[s,team2_prev[n]]) )))
    l.star[s,n] <- exp(eta[s])
    u_prev[s,n] <- rbinom(1,1, po_prev[s,n])
    lambda_prev[s,n] <- (1-u_prev[s,n])*l.star[s,n]+0.1
    o_prev[s,n] <- rpois(1, lambda_prev[s,n])
    
    w_prev[s,n] <- 1/(1+exp(  -(set.home[s] + set.ab[s, team1_prev[n]]- set.ab[s, team2_prev[n]]+
                                  gamma[s]*(p.ab[s, team1_prev[n]]- p.ab[s, team2_prev[n]] )) ))
    z_prev[s,n] <- rbinom(1, 1,  w_prev[s,n] )
    pp_prev[s,n] <- 1/(1+exp(-  ( mu[s]+(1-z_prev[s,n])*p.home[s]+(p.ab[s,team1_prev[n]]-p.ab[s, team2_prev[n]])*(1-2*z_prev[s,n]) )  ))
    y_prev[s,n] <- rtrunc(1, spec="nbinom", size=25, prob=1-pp_prev[s,n],
                          a=0, b=23)
    tot_prev[s,n] <- y_prev[s,n]+o_prev[s,n]
    
  }
}
real_points <- c(70, 64, 60, 51, 50, 44, 42, 41, 35,
                 28, 25, 13, 13, 10)
in_sample_points <- c(70, 63, 60, 52, 50, 44, 41, 41,35, 28, 26, 13, 13,10)
real_points_andata <- c(33,32,30,22,26,21,20,20,23,12,12, 9,6,7)
real_teams<- c("Sir Safety Perugia", "Cucine Lube Civitanova",
               "Azimut Modena", "Diatec Trentino",
               "Calzedonia Verona", "Revivre Milano",
               "Wixo LPR Piacenza", "Bunge Ravenna",
               "Kioene Padova", "Gi Group Monza",
               "Taiwan Exc. Latina", "Callipo Vibo Valentia",
               "Biosì Sora", "BCC Castellana Grotte")

# compute the match outcomes for each MCMC sample
match_alloc <- array(0,
                     dim=c(M,
                           length(unique(match_index))/2,
                           2))
point_alloc = point_real_alloc_2 =
  array(0, dim=c(M, length(unique(match_index))/2,2))
point_real_alloc =  matrix(0, 91,2)
class <- array( 0, c(M,14, length(unique(match_index))/2))
match_index_test <- match_index[341:(nset_train+nset_test)] 


for ( s in 1:M){
  for (n in 1: 91){
    
    match_alloc[s, n,1] <- sum(tot_z_prev[s,n,])
    match_alloc[s, n,2] <- sum(1-tot_z_prev[s,n,])
    
    point_alloc[s, n,1] <- sum( (y_prev[s, match_index_test==n + 91 ]+o_prev[s, match_index_test==n +91])%*%(1-z[s,match_index_test==n + 91])+ (25+o_prev[s, match_index_test==n+91])%*%z[s,match_index_test==n + 91 ])
    point_alloc[s, n,2] <- sum((y_prev[s, match_index_test==n + 91]+o_prev[s, match_index_test==n + 91])%*%(z[s,match_index_test==n + 91])+ (25+o_prev[s, match_index_test==n +91])%*%(1-z[s,match_index_test==n +91 ]))
    
    point_real_alloc[ n,1] <- sum( volley_ita_frame$points1[match_index_test==n+91])
    point_real_alloc[ n,2] <- sum(volley_ita_frame$points2[match_index_test==n+91])
    
    point_real_alloc_2[s, n,1] <- sum( ( (y_prev[s, match_index_test==n+91]+o_prev[s, match_index_test==n+91])*(1-z[s,match_index_test==n+91 ])+ (25+o_prev[s, match_index_test==n+91])*z[s,match_index_test==n+91 ]-volley_ita_frame$points1[match_index_test==n+91])^2)
    point_real_alloc_2[s, n,2] <- sum((  (y_prev[s, match_index_test==n+91]+o_prev[s, match_index_test==n+91])*(z[s,match_index_test==n+91 ])+ (25+o_prev[s, match_index_test==n+91])*(1-z[s,match_index_test==n+91 ])-volley_ita_frame$points2[match_index_test==n+91]   )^2 )
    
    
    # points allocation for each match
    if (match_alloc[s, n,1] -
        match_alloc[s, n,2]>=2){
      class[s, unique(team1_prev[match_index_test==n+91]) ,n]<-3
      class[s, unique(team2_prev[match_index_test==n+91])  ,n]<-0
    }else if ( match_alloc[s, n,1] -
               match_alloc[s, n,2]==1  ){
      class[s, unique(team1_prev[match_index_test==n+91]),n]<-2
      class[s, unique(team2_prev[match_index_test==n+91]),n]<-1
    }else if (match_alloc[s, n,1] -
              match_alloc[s, n,2]==0){
      if (w_prev[s,n]>=0.5){
        class[s, unique(team1_prev[match_index_test==n+91]),n]<-2
        class[s, unique(team2_prev[match_index_test==n+91]),n]<-1
      }else{
        class[s, unique(team1_prev[match_index_test==n+91]),n]<-2
        class[s, unique(team2_prev[match_index_test==n+91]),n]<-1
      }
      
    }else if ( match_alloc[s, n,1] -
               match_alloc[s, n,2]<=-2  ){
      class[s, unique(team1_prev[match_index_test==n+91]),n]<-0
      class[s, unique(team2_prev[match_index_test==n+91]),n]<-3
      
    }else if( match_alloc[s, n,1] -
              match_alloc[s, n,2]==-1  ){
      class[s, unique(team1_prev[match_index_test==n+91]),n]<-1
      class[s, unique(team2_prev[match_index_test==n+91]),n]<-2
    }
    
  }
}

class_fin <- apply(class, c(1,2), sum)
class_fin_mean <- apply(class_fin,2, median)
class_fin_025 <- apply(class_fin, 2,  function(x) quantile(x, 0.025))
class_fin_975 <- apply(class_fin, 2,  function(x) quantile(x, 0.975))
cbind(class_fin_mean, class_fin_025, class_fin_975)

exp <- sort.int(class_fin_mean, decreasing =TRUE, index.return=TRUE)
exp.points <- exp$x
exp.points.ix <- exp$ix

final_teams <- teams[sort(class_fin_mean, decreasing =TRUE, index.return=TRUE)$ix]
real_rank<- match(final_teams, real_teams)

df2<-as.data.frame(cbind(final_teams, exp.points,
                         real_points[real_rank],
                         real_rank))
dimnames(df2) <- list(c(1:14), c( "Teams", "Exp. Points",
                                  "Actual points",
                                  "Actual rank"))
quasi_points <- sort.int(exp.points+real_points_andata[real_rank],
                         decreasing = TRUE, index.return = TRUE)

rank_frame=data.frame(
  squadre=final_teams[quasi_points$ix],
  mid=(exp.points+real_points_andata[real_rank])[quasi_points$ix],
  lo=(class_fin_025[exp.points.ix]+real_points_andata[real_rank])[quasi_points$ix],
  hi=(class_fin_975[exp.points.ix]+real_points_andata[real_rank])[quasi_points$ix],
  obs=real_points[  match(  final_teams[quasi_points$ix], real_teams) ],
  insample = in_sample_points[  match(  final_teams[quasi_points$ix], real_teams) ]
)

rank_frame$squadre=factor(rank_frame$squadre, levels=final_teams[quasi_points$ix])

ggplot()+
  geom_ribbon(aes(x=squadre, ymin=lo, ymax=hi, group=1),
              data=rank_frame,
              fill = color_scheme_get("red")[[2]]
  )+
  geom_line(aes(x=squadre, y= mid, group=1),
            data=rank_frame,
            color = color_scheme_get("red")[[4]]
  )+
  geom_point(aes(x=squadre, y=obs),
             size =2,
             data=rank_frame)+
  geom_point(aes(x=squadre, y=insample),
             color = color_scheme_get("blue")[[3]],
             size =2,
             data=rank_frame)+
  scale_color_manual(values = c(color_scheme_get("blue")[[2]], 
                                color_scheme_get("red")[[2]]))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x="Teams", y="Points")
ggsave(file="RankPlot.pdf", width=6, height=6)


##################
### FIGURE 10
#################

# rank probabilities
ordinamento_campione  = rank_finale = punti_campione = matrix(NA, M, 14)
for (h in 1:M){
  rank_finale[h,] <- (class_fin[h,]+real_points_andata[match(teams, real_teams)])
  punti_campione[h,] <- sort.int(rank_finale[h,], decreasing = TRUE, index.return =TRUE)$x
  ordinamento_campione[h,] <- sort.int(rank_finale[h,], decreasing = TRUE, index.return =TRUE)$ix
}

lista_rank<- apply(ordinamento_campione,2, table)
ordine_vero <- match(real_teams, teams)
exact_rank_prob <- matrix(0, 14, 14)

for (h in 1:14){   # scorre posizioni
  for (u in as.double(names(lista_rank[[h]]))){ # scorre squadre
    exact_rank_prob[h,u] <- as.double(lista_rank[[h]][as.double(names(lista_rank[[h]]))==u])/M
  }
  exact_rank_prob[h,] <-exact_rank_prob[h,][match(teams[ordine_vero], teams)]
}

apply(exact_rank_prob,1,sum)
colnames(exact_rank_prob) <- teams[ordine_vero]


par(mfrow=c(5,3), mar=c(3,4,2,1), oma =c(0,0,0,0))
pdf(file="ExactRankProb.pdf", width=11, height =12)
par(mfrow=c(5,3), mar=c(3,4,2,1), oma =c(0,0,0,0))
for (i in 1:14){
  cols <- rep("gray", 14)
  cols[i] <- "red"
  barplot(exact_rank_prob[,i], col= cols,
          names.arg=c(1:14), cex.names=0.9,
          main = paste(teams[ordine_vero][i]," (", i, ")", sep=""),
          cex.main = 1.7,
          border = "blue")
}
dev.off()



## Now playoff

##################
### FIGURE 8
##################

# 8b: posterior agreemeent for the playoff
type="playoff"  # type of prediction
dummy_index <- c(rep(0,9), 1, 0,1,0,0)
source("volley_ita.R")
source("5.4-Out_of_sample predictions.R")
pdf(file=paste("agree_", type, ".pdf", sep=""), width =10, height =6.5)
par(mfrow=c(1,1), mar=c(5,5,3,1))
plot(density(agree_prev, bw=2.5),   xlab ="% of agreement",
     main =paste("Test set: ", type), cex.main =2.1,lwd=2, cex.lab =2.4)
dev.off()





################
### TABLE 6
###############

# playoff posterior probabilities
for ( s in 1:S){  # compute predictive quantities
  for (n in 1:nset_test){
    po_prev[s,n] <- 1/(1+exp(-(iota[s]
                               +alpha[s]*( p.ab[s, team1_prev[n]]-p.ab[s, team2_prev[n]])
                               + beta[s]*(set.ab[s, team1_prev[n]]-set.ab[s,team2_prev[n]]) )))
    l.star[s,n] <- exp(eta[s])
    u_prev[s,n] <- rbinom(1,1, po_prev[s,n])
    lambda_prev[s,n] <- (1-u_prev[s,n])*l.star[s,n]+0.1
    o_prev[s,n] <- rpois(1, lambda_prev[s,n])
    
    w_prev[s,n] <- 1/(1+exp(  -(set.home[s] + set.ab[s, team1_prev[n]]- set.ab[s, team2_prev[n]]+
                                  gamma[s]*(p.ab[s, team1_prev[n]]- p.ab[s, team2_prev[n]] )) ))
    z_prev[s,n] <- rbinom(1, 1,  w_prev[s,n] )
    pp_prev[s,n] <- 1/(1+exp(-  ( mu[s]+(1-z_prev[s,n])*p.home[s]+(p.ab[s,team1_prev[n]]-p.ab[s, team2_prev[n]])*(1-2*z_prev[s,n]) )  ))
    y_prev[s,n] <- rtrunc(1, spec="nbinom", size=25, prob=1-pp_prev[s,n],
                          a=0, b=23)
    tot_prev[s,n] <- y_prev[s,n]+o_prev[s,n]
    
  }
}

# definition of predictive ranks
match_alloc_playoff <- array(0,dim=c(M,24,2))
match_index_test_playoff <- match_index[(nset_train+1):(nset_train+nset_test)] 
class_playoff <- array( 0, c(M,14, 24))
progress_probability <- array(0, dim = c(M, 7, 2 ))
set_index <- c(12, 11, 8, 9, 20, 18, 20)
matches_rep <- #c(1,4,7,9,11,16,20)
  cumsum(c(3,3,2,2,5,4,5))
distinct_set <- cumsum(c(4, 5, 3, 5, 3, 3, 3, 5, 5, 4, 4,5,5,3,3, 5, 4, 5, 4,
                         4,5,4, 4,3))
match_results <-matrix(NA, M, 24)
match_binary = match_teams = matrix(NA, M, 24)
overall_match_binary <- matrix(NA, M, 7)
passaggio_turno = overall_match_teams = matrix(NA, M, 7)
new_team1_prev = new_team2_prev = matrix(NA, M, nset_test)
new_team1_prev_singles = new_team2_prev_singles = matrix(NA, M, 24)


for (s in 1:M){
  match_results[s,1] <- sum(tot_z_prev[s, 1,])
  if (match_results[s,1]/length(1:distinct_set[1])==1/2){
    match_results[s,1] <- match_results[s,1]+rbinom(1,1, w_prev[s,1])
  }
  
  # quarti
  
  for (n in 2:10){
    match_results[s, n] <- sum(tot_z_prev[s, n,])
    if (match_results[s,n]/length((distinct_set[n-1]+1):distinct_set[n])==1/2){
      match_results[s,n] <- match_results[s,n]+ rbinom(1,1, w_prev[s,(distinct_set[n-1]+1)])
    }
  }
  match_binary[s, match_results[s, 1:10]>=3] <- 1
  match_teams[s, match_results[s,1:10]>=3] <- team1_prev_singles[match_results[s,1:10]>=3]
  match_binary[s, match_results[s,1:10]<3] <- 0
  match_teams[s, match_results[s,1:10]<3] <- team2_prev_singles[match_results[s,1:10]<3]
  
  overall_match_binary[s,1] <- sum(match_binary[s, 1:matches_rep[1]])
  if (overall_match_binary[s,1]>((matches_rep[1]))/2){
    passaggio_turno[s,1] <-1
    overall_match_teams[s,1] <- max(match_teams[s, 1:matches_rep[1] ])
  }else{
    passaggio_turno[s,1] <-0
    overall_match_teams[s,1] <- min(match_teams[s, 1:matches_rep[1] ])
  }
  
  for (h in 2:4){
    overall_match_binary[s,h] <- sum(match_binary[s, (matches_rep[h-1]+1):(matches_rep[h])]) 
    
    if (overall_match_binary[s,h]>((matches_rep[h])-(matches_rep[h-1]))/2){
      passaggio_turno[s,h] <-1
      overall_match_teams[s,h] <- max(match_teams[s, (matches_rep[h-1]+1):(matches_rep[h]) ])
      
    }else{
      passaggio_turno[s,h] <-0
      overall_match_teams[s,h] <- min(match_teams[s, (matches_rep[h-1]+1):(matches_rep[h]) ])
    }
  }
  
  # semi
  
  new_team1_prev[s, 43:62]<-as.numeric(as.vector(c(rep(overall_match_teams[s,1],4),
                                                   rep(overall_match_teams[s,2],5),
                                                   rep(overall_match_teams[s,1],5),
                                                   rep(overall_match_teams[s,2],3),
                                                   rep(overall_match_teams[s,1],3))))
  new_team2_prev[s, 43:62]<-as.numeric(as.vector(c(rep(overall_match_teams[s,2],4),
                                                   rep(overall_match_teams[s,1],5),
                                                   rep(overall_match_teams[s,2],5),
                                                   rep(overall_match_teams[s,1],3),
                                                   rep(overall_match_teams[s,2],3))))
  
  new_team1_prev[s, 63:78]<-as.numeric(as.vector(c(rep(overall_match_teams[s,3],3),
                                                   rep(overall_match_teams[s,4],4),
                                                   rep(overall_match_teams[s,3],5),
                                                   rep(overall_match_teams[s,4],4))))
  
  new_team2_prev[s, 63:78]<-as.numeric(as.vector(c(rep(overall_match_teams[s,4],3),
                                                   rep(overall_match_teams[s,3],4),
                                                   rep(overall_match_teams[s,4],5),
                                                   rep(overall_match_teams[s,3],4))))
  
  # soglie a 43, 63
  for (n in 43:78){
    po_prev[s,n] <- 1/(1+exp(-(iota[s]
                               +alpha[s]*( p.ab[s, new_team1_prev[s,n]]-p.ab[s, new_team2_prev[s,n]])
                               + beta[s]*(set.ab[s, new_team1_prev[s,n]]-set.ab[s,new_team2_prev[s,n]]) )))
    l.star[s,n] <- exp(eta[s])
    u_prev[s,n] <- rbinom(1,1, po_prev[s,n])
    lambda_prev[s,n] <- (1-u_prev[s,n])*l.star[s,n]+0.1
    o_prev[s,n] <- rpois(1, lambda_prev[s,n])
    
    w_prev[s,n] <- 1/(1+exp(  -(set.home[s] + set.ab[s, new_team1_prev[s,n]]- set.ab[s, new_team2_prev[s,n]]+
                                  gamma[s]*(p.ab[s, new_team1_prev[s,n]]- p.ab[s, new_team2_prev[s,n]] )) ))
    z_prev[s,n] <- rbinom(1, 1,  w_prev[s,n] )
  }
  
  for (n in 11:19){
    match_results[s, n] <- sum(tot_z_prev[s, n,])
    if (match_results[s,n]/length((distinct_set[n-1]+1):distinct_set[n])==1/2){
      match_results[s,n] <- match_results[s,n]+ rbinom(1,1, w_prev[s,(distinct_set[n-1]+1)])
    }
  }
  
  new_team1_prev_singles[s,] <- new_team1_prev[s,cumsum(conta_set[(ngames_train+1):(ngames_train+ngames_test)])]
  new_team2_prev_singles[s,] <- new_team2_prev[s,cumsum(conta_set[(ngames_train+1):(ngames_train+ngames_test)])]
  
  
  match_binary[s, match_results[s, 11:19]>=3] <- 1
  match_teams[s, match_results[s,11:19]>=3] <- new_team1_prev_singles[s,match_results[s,11:19]>=3]
  match_binary[s, match_results[s,11:19]<3] <- 0
  match_teams[s, match_results[s,11:19]<3] <- new_team2_prev_singles[s,match_results[s,11:19]<3]
  
  
  for (h in 5:6){
    overall_match_binary[s,h] <- sum(match_binary[s, (matches_rep[h-1]+1):(matches_rep[h])]) 
    
    if (overall_match_binary[s,h]>((matches_rep[h])-(matches_rep[h-1]))/2){
      passaggio_turno[s,h] <-1
      overall_match_teams[s,h] <- max(match_teams[s, (matches_rep[h-1]+1):(matches_rep[h]) ])
      
    }else{
      passaggio_turno[s,h] <-0
      overall_match_teams[s,h] <- min(match_teams[s, (matches_rep[h-1]+1):(matches_rep[h]) ])
    }
  }
  
  # final
  
  new_team1_prev[s, 79:98]<-as.numeric(as.vector(c(rep(overall_match_teams[s,5],4),
                                                   rep(overall_match_teams[s,6],5),
                                                   rep(overall_match_teams[s,5],4),
                                                   rep(overall_match_teams[s,6],4),
                                                   rep(overall_match_teams[s,5],3))))
  new_team2_prev[s, 79:98]<-as.numeric(as.vector(c(rep(overall_match_teams[s,6],4),
                                                   rep(overall_match_teams[s,5],5),
                                                   rep(overall_match_teams[s,6],4),
                                                   rep(overall_match_teams[s,5],4),
                                                   rep(overall_match_teams[s,6],3))))
  
  
  # soglie a 43, 63
  for (n in 79:98){
    po_prev[s,n] <- 1/(1+exp(-(iota[s]
                               +alpha[s]*( p.ab[s, new_team1_prev[s,n]]-p.ab[s, new_team2_prev[s,n]])
                               + beta[s]*(set.ab[s, new_team1_prev[s,n]]-set.ab[s,new_team2_prev[s,n]]) )))
    l.star[s,n] <- exp(eta[s])
    u_prev[s,n] <- rbinom(1,1, po_prev[s,n])
    lambda_prev[s,n] <- (1-u_prev[s,n])*l.star[s,n]+0.1
    o_prev[s,n] <- rpois(1, lambda_prev[s,n])
    
    w_prev[s,n] <- 1/(1+exp(  -(set.home[s] + set.ab[s, new_team1_prev[s,n]]- set.ab[s, new_team2_prev[s,n]]+
                                  gamma[s]*(p.ab[s, new_team1_prev[s,n]]- p.ab[s, new_team2_prev[s,n]] )) ))
    z_prev[s,n] <- rbinom(1, 1,  w_prev[s,n] )
  }
  
  for (n in 20:24){
    match_results[s, n] <- sum(tot_z_prev[s, n,])
    if (match_results[s,n]/length((distinct_set[n-1]+1):distinct_set[n])==1/2){
      match_results[s,n] <- match_results[s,n]+ rbinom(1,1, w_prev[s,(distinct_set[n-1]+1)])
    }
  }
  
  new_team1_prev_singles[s,] <- new_team1_prev[s,cumsum(conta_set[(ngames_train+1):(ngames_train+ngames_test)])]
  new_team2_prev_singles[s,] <- new_team2_prev[s,cumsum(conta_set[(ngames_train+1):(ngames_train+ngames_test)])]
  
  
  match_binary[s, match_results[s, 20:24]>=3] <- 1
  match_teams[s, match_results[s,20:24]>=3] <- new_team1_prev_singles[s,match_results[s,20:24]>=3]
  match_binary[s, match_results[s,20:24]<3] <- 0
  match_teams[s, match_results[s,20:24]<3] <- new_team2_prev_singles[s,match_results[s,20:24]<3]
  
  
  h=7
  overall_match_binary[s,h] <- sum(match_binary[s, (matches_rep[h-1]+1):(matches_rep[h])]) 
  
  if (overall_match_binary[s,h]>((matches_rep[h])-(matches_rep[h-1]))/2){
    passaggio_turno[s,h] <-1
    overall_match_teams[s,h] <- max(match_teams[s, (matches_rep[h-1]+1):(matches_rep[h]) ])
    
  }else{
    passaggio_turno[s,h] <-0
    overall_match_teams[s,h] <- min(match_teams[s, (matches_rep[h-1]+1):(matches_rep[h]) ])
  }
  
  
}

# table definition
progress_probability <- matrix(0, 8, 3)
nomi_squadre <- c(as.double(names(table(overall_match_teams[,7]))),2)
mancante <- setdiff(unique(team1_prev_singles), nomi_squadre)
nomi_squadre <- c(nomi_squadre, mancante)

progress_probability[(1:8)[nomi_squadre==nomi_squadre[1]],1] <- as.double(table(overall_match_teams[,4])/M)[1]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[1]],2] <- as.double(table(overall_match_teams[,6])/M)[1]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[1]],3] <- as.double(table(overall_match_teams[,7])/M)[1]

progress_probability[(1:8)[nomi_squadre==nomi_squadre[2]],1] <- as.double(table(overall_match_teams[,2])/M)[1]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[2]],2] <- as.double(table(overall_match_teams[,5])/M)[1]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[2]],3] <- as.double(table(overall_match_teams[,7])/M)[2]

progress_probability[(1:8)[nomi_squadre==nomi_squadre[3]],1] <- as.double(table(overall_match_teams[,1])/M)[1]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[3]],2] <- as.double(table(overall_match_teams[,5])/M)[2]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[3]],3] <- as.double(table(overall_match_teams[,7])/M)[3]

progress_probability[(1:8)[nomi_squadre==nomi_squadre[4]],1] <- as.double(table(overall_match_teams[,2])/M)[2]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[4]],2] <- as.double(table(overall_match_teams[,5])/M)[3]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[4]],3] <- as.double(table(overall_match_teams[,7])/M)[4]


progress_probability[(1:8)[nomi_squadre==nomi_squadre[5]],1] <- as.double(table(overall_match_teams[,3])/M)[1]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[5]],2] <- as.double(table(overall_match_teams[,6])/M)[2]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[5]],3] <- as.double(table(overall_match_teams[,7])/M)[5]

progress_probability[(1:8)[nomi_squadre==nomi_squadre[6]],1] <- as.double(table(overall_match_teams[,4])/M)[2]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[6]],2] <- as.double(table(overall_match_teams[,6])/M)[3]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[6]],3] <- as.double(table(overall_match_teams[,7])/M)[6]

progress_probability[(1:8)[nomi_squadre==nomi_squadre[7]],1] <- as.double(table(overall_match_teams[,3])/M)[2]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[7]],2] <- as.double(table(overall_match_teams[,6])/M)[4]
progress_probability[(1:8)[nomi_squadre==nomi_squadre[7]],3] <- as.double(table(overall_match_teams[,7])/M)[7]

progress_probability[(1:8)[nomi_squadre==mancante],1] <- 0
progress_probability[(1:8)[nomi_squadre==mancante],2] <- 0
progress_probability[(1:8)[nomi_squadre==mancante],3] <- 0

na_values <- (1:8)[is.na(progress_probability[,1])]
progress_probability[na_values,] <- rep(0,3)

rownames(progress_probability) <- teams[nomi_squadre]
colnames(progress_probability) <- c("Semi", "Final", "Winner")
xtable(progress_probability)



################
### FIGURE 11
###############

### Obtained in Latex, building the probabilities from Table 6.

