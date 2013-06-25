



#anova power simulation
#groups is a vector of group sizes; means is a vector of group means; sds is a vector of group standard devs; nsims is the number of simulations to run: reccomended values between 1000 and 10k
anova.pwr.sim <- function(groups, means, sds, nsims ){
  #repeats means for each group for use in rnorm number generation
  rep(means, times=groups) -> means.vec
  
  #repeats means for each group for use in rnorm number gen
  rep(sds, times=groups) -> sds.vec
  
  #assigns factor vals for each entry based on number of groups
  factor(rep(1:length(groups), times = groups) -> IV
         
  sim <- function(){
    rnorm(sum(groups), means.vec, sds.vec) -> DV
         
    anova(lm(DV~IV))['IV', 'Pr(>F)']
  }
  
  replicate(nsims, sim()) -> p.vals
         
  (sum(p.vals < 0.05)/nsims) -> power
         
  print(power)
         
  
         
}