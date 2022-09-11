#Function used to calculate Cohen's as d shown in 
# Rosenberg, M. S., Rothstein, H. R., & Gurevitch, J. (2013). Effect sizes: conventional choices and calculations. Handbook of meta-analysis in ecology and evolution, 61.
# Y1 = treatment mean 
# s1 = standard deviation of the treatment 
# n1 = sample size of the treatment
# Y2 = control mean
# s2 = standard deviation of the control 
# n2 = sample size of the control
# J = correction for small sample sizes (< 50)
# d = Cohen's d


calculate.cohens_d = function(Y1, s1, n1, Y2, s2, n2){
  
  Y1 = mean_treatment
  s1 = standard_deviation_treatment
  n1 = sample_size_treatment
  
  Y2 = mean_control
  s2 = standard_deviation_control
  n2 = sample_size_control
  
  J = 1 - (3 / (4 * (n1 + n2 - 2) - 1))
  
  denominator = sqrt( (((n1 - 1) * (s1)^2) + ((n2 - 1) * (s2)^2)) / (n1 + n2 - 2) )
  
  d = (Y1 - Y2) / denominator * J
  
  return(d)
  
}