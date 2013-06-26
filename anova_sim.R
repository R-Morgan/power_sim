
#########################################################################
# Copyright, 2013, Arianna Morgan                                      ##
#########################################################################
# This program is free software: you can redistribute it and/or modify ##
# it under the terms of the GNU General Public License as published by ##
# the Free Software Foundation, either version 3 of the License, or    ##
# (at your option) any later version.                                  ##
#                                                                      ##
# This program is distributed in the hope that it will be useful,      ##
# but WITHOUT ANY WARRANTY; without even the implied warranty of       ##
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        ##
# GNU General Public License for more details.                         ##   
#########################################################################


#anova power simulation
#groups is a vector of group sizes; means is a vector of group means; sds is a vector of group standard devs; nsims is scalar corresponding to the the number of simulations to run: recommended values between 100 and 10k

#required packages -- needed for computing effect size and plotting histogramme
#of effect sizes

require(compute.es)
require(ggplot2)

anova.pwr.sim <- function(groups, means, sds, nsims, es){
  #repeats means for each group for use in rnorm number generation
  rep(means, times=groups) -> means.vec
  
  #repeats means for each group for use in rnorm number gen
  rep(sds, times=groups) -> sds.vec
  
  #assigns factor vals for each entry based on number of groups
  factor(rep(1:length(groups), times = groups)) -> IV
    
#this is the ANOVA simulation portion of the function
  
  sim <- function(){
        
    #generates a random normal distribution for each entry in all groups
    rnorm(sum(groups), means.vec, sds.vec) -> DV
    
    #Runs the ANOVA portion of the function
    anova(lm(DV~IV)) -> fit
    
    #Extracts the p-vals and F-val from the ANOVA
    fit['IV', 'Pr(>F)'] -> fit.p.val

    fit['IV', 'F value'] -> f.val
    
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
    
    #Creates a list that is the return value of the function.  This list is
    #manipulated outside the scope of the function.
    
    list(PVal = fit.p.val, FVal = f.val, ES =  cohend.mtx[1]) -> out
    return(out)
      
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

#The following function is the autonomous ANOVA power simulation function.  It takes vectors of group size, means, and standard deviations.  The intended function is to take these, run an ANOVA, and output several values.

sim <- function(groups, means, sds){
  
  #repeats means for each group for use in rnorm number generation
    rep(means, times=groups) -> means.vec
  
  #repeats means for each group for use in rnorm number gen
    rep(sds, times=groups) -> sds.vec
  
  #assigns factor vals for each entry based on number of groups
    factor(rep(1:length(groups), times = groups)) -> IV  
  
  #This sets the starting point of the pseudo-random number generation.
  #this has the function of fixing the values of the output for debugging
  #purposes.  Comment this out, to allow out other output.
    set.seed(100)
  
  #generates a random normal distribution for each entry in all groups
    rnorm(sum(groups), means.vec, sds.vec) -> DV
  
  #Runs the ANOVA portion of the function
    anova(lm(DV~IV)) -> fit
  
  #prints the p-value for the grouping factor
    print(fit['IV', 'Pr(>F)'] -> fit.p.val)
  
  #prints the F-value
    print(fit['IV', 'F value'] -> f.val)
  
  #prints min and max group sizes.  this maximises the effect size, though it     
  #does not compute all effect sizes for all groups.  will fix that in later 
  #versions.
  
    print(min(groups) -> n.1)
    print(max(groups) -> n.2)
  
  
  #computes effect sizes, using compute.es::fes
    fes(f.val, n.1, n.2)-> d
  
  #prints the Cohen's d MeanDiff effect size line
    print(d$MeanDifference[1] -> cohend.es)
  
  #reconstructs Cohen's d as a matrix
    as.matrix(cohend.es) -> cohend.mtx
  
  #makes a list of different values to display in preparation to return the 
    #value
    list(FVal = f.val, ES =  cohend.mtx[1]) -> out
    return(out)
  
}