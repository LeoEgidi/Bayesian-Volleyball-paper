
# install.packages('readxl')
# install.packages('runjags')
# install.packages('R2jags')
# install.packages('MCMCpack')
# install.packages('rjags')
# install.packages('R2WinBUGS')
# install.packages('readr')
# install.packages("arm")
# install.packages("ggplot2")
# install.packages("matrixStats")





# import volley data

volley_ita_prel <- read_excel("Regular_season_2017-18_Set_by_set_Ita.xlsx")
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
home_result <- volley_ita_prel$set[seq(1,363, by=2)]
away_result <- volley_ita_prel$set[seq(2,364, by=2)]
binary_win <- c()
binary_win[home_result>away_result]<-1
binary_win[home_result<away_result]<-0
table(binary_win)



