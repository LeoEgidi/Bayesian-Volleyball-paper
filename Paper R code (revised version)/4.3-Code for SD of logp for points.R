# Points of the loosing team in tie breaks
z <- Y[tie_index]
# Points of the loosing team in normal sets 
z2<-Y[-tie_index]
# range(z)
#[1]  8 18
# range(z2)
#[1]  8 39
#
# Truncate points to 13/23 (extra points are modelled by the ZIP component)
z[z>13]<-13
z2[z2>23]<-23
#
#range(z)
#[1]  8 13
#range(z2)
#[1]  8 23
#
# Calculations of logits for tie breaks 
lz<-log( (15-z)/z )
# Calculations of logits for normal sets  
lz2<-log( (25-z2)/z2 )
#
# vector of logits 
lznew<-c(lz,lz2)
#mean(lznew)
#[1] -1.510211
#sd(lznew)
#[1] 0.7425043
#1/(1+exp(-1.5))
#[1] 0.8175745
#25*1/(1+exp(-1.5))
#[1] 20.43936
#15*1/(1+exp(-1.5))
#[1] 12.26362


# vector of points 
znew<-c(z,z2)


# mean(z2)
#[1] 19.92824
# mean(z)
#[1] 11.4359
# mean(z)/25
#[1] 0.4574359
# mean(z2)/25
#[1] 0.7971295
# mean(z)/15
#[1] 0.7623932

sd(z);sd(z2)
#[1] 1.774006
#[1] 3.02611
p<-c(z/15,z2/25)
mean(p); sd(p)
#[1] 0.7951373
#[1] 0.1210715

p<-c(z/15,z2/25)
mean(p); sd(p)
#[1] 0.7951373
#[1] 0.1210715
 

