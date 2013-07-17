  # purpose of this function is to allow for the subsetting of a matrix so
  # so that only significant results are retained.  user may specify a level
  # of p-value

RetainPVals <- function(data, pval.col.name, plevel){

  #creates a vector of significant results for use in getting ESs
  subset(data, pval.col.name < plevel) -> sig.results
  
  return(sig.results)
}
  #print(sig.results)
  

#function divides up effect sizes based on magnitude of cohen's d

DivideES <- function(data, es.col.name){
  
  #subsets data to get very small ESs
  nrow(subset(data, abs(es.col.name) < 0.2)) -> es.v.small
  
  #subsets data to get all ESes less than 0.8, resulting in vector of non-large
  #ESes
  subset(sig.results, abs(es.col.name) < 0.8) -> es.ltpe
  
  #subsets non-large ES vector for effects sizes below 0.5, resulting in
  #ESes below 0.5
  subset(es.ltpe, abs(es.col.name) < 0.5) -> es.small.ltpf
  
  #subsets data to get ESes between 0.2 and 0.5.  This is the small range.
  nrow(subset(es.small.ltpf, abs(es.col.name) >= 0.2)) -> es.small
  
  #subsets data to get ESes between 0.5 and 0.8. This is the medium range.
  nrow(subset(es.ltpe, abs(es.col.name) >= 0.5)) -> es.med
  
  #Subsets data for ESes above 0.8, for large ES size.
  nrow(subset(data, abs(es.col.name) >= 0.8)) -> es.large
}