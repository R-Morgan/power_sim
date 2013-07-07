require(compute.es)
require(ggplot2)

tTestPwrSim <- function(grpsize, means, sds, nsims){
  
  #creates a vector to represent sample sizes of each group. assumes equal
  #sample size.  Allows user to specify only one value
  rep(10, 2) -> groups
  
  #repeats means for each group for use in rnorm number generation
  rep(means, times=(groups) -> means.vec
  
  #repeats means for each group for use in rnorm number gen
  rep(sds, times=groups) -> sds.vec
  
  #assigns factor vals for each entry based on number of groups
  factor(rep(1:2), times = groups)) -> IV
    
#this is the ANOVA simulation portion of the function
  
  rep(NA, length(groups)) -> PVals
  PVals -> tVals
  PVals -> ES
  
  sim <- function(){
        
    #generates a random normal distribution for each entry in all groups
    rnorm(sum(groups), means.vec, sds.vec) -> DV
    
    cbind(IV, DV) -> grouped
    
    
    #Runs the ANOVA portion of the function
    #lm(DV~IV) -> fit
    
    #Extracts the p-vals and F-val from the ANOVA
    #fit['IV', 'Pr(>F)'] -> append(PVals)
    #fit['IV', 'F value'] -> f.val
    
    #Extracts min and max group sizes for use in the effect size calculation. it               
    #should be noted that this ES extraction is only for the largest and 
    #smallest group size.  Clearly this is problematic as this does not examine 
    #all possible effect sizes or even necessarily the one of most substantive 
    #interest.
    min(groups) -> n.1
    max(groups) -> n.2
    
    #calculates effect size using fes() from the 'compute.es' package
    fes(f.val, n.1, n.2)-> d
      
    #extracts the first element of the MeanDifference label.  This is Cohen's d.
    d$MeanDifference[1] -> cohend.es
    
    #Creates matrix for effect size for easier manipulation
    as.matrix(cohend.es) -> cohend.mtx
    
      
    }
  
  #replicates the simulation function and outputs return value to a matrix.
  replicate(nsims, sim()) -> sim.stats
  
  #Transposes the matrix so that the list names are column names.  Forces mtx
  #to a data frame for easier manipulation with unlist() function.
  as.data.frame(t(sim.stats)) -> sim.stats.df
 
  #turns what was formerly a list into numeric vectors
  unlist(sim.stats.df$PVal) -> PVals
  unlist(sim.stats.df$ES) -> ESs
  
  #Rounds the effect sizes for plotting purposes
  round(ESs, 1) -> ESs.round
  
  #Binds the numeric vectors together and creates a dataframe
  as.data.frame(cbind(PVals, ESs, ESs.round)) -> ss.df 
  
  #Returns power of specified ANOVA by calculating proportion of p-values that
  #are less than 0.05
  sum(PVals < 0.05)/ nsims -> power
  print('ANOVA Power')
  print(power)
 
  #uses ggplot2's qplot() to create a histogramme of ES frequencies in the
  #simulation
  qplot(data = ss.df, x=ESs.round, main='Histogramme of Simulation Effect Sizes
        (d)', xlab='Cohens d', ylab='Frequency') 
         
}
