#Hedge's d is calculated as shown in
#Rosenberg, M. S., Rothstein, H. R., & Gurevitch, J. (2013). Effect sizes: conventional choices and calculations. Handbook of meta-analysis in ecology and evolution, 61.
#The SE of Hedge's d are calculate as shown in 
#Borenstein, M., Hedges, L. V., Higgins, J. P., & Rothstein, H. R. (2009). Effect sizes based on means. Introduction to meta-analysis, 21-32.
#The 95% CI interval computation from the SE was found at:
#Hedges, L. V., & Olkin, I. (1984). Statistical methods for meta-analysis. Academic press.

calculate.hedges_d = function(Y1, # Y1 = treatment mean 
                              s1, # s1 = standard deviation of the treatment 
                              n1, # n1 = sample size of the treatment
                              Y2, # Y2 = control mean
                              s2, # s2 = standard deviation of the control 
                              n2  # n2 = sample size of the control
                              ){
  
  ### --- Calculate Hedge's d --- ###
  
  # J = correction for small sample sizes (< 50)
  J = 1 - ( 3 / ( ( 4 * ( n1 + n2 - 2) ) - 1) )
  
  denominator_of_d = sqrt( ( ( ( n1 - 1 ) * ( s1^2 )  ) + ( ( n2 - 1 ) * ( s2^2 ) ) )   /   ( n1 + n2 - 2) )
  
  # d = Hedge's d
  d = ( ( Y1 - Y2 ) / denominator_of_d ) * J
  
  ### --- Calculate Hedge's d SE --- ###
  
  Vd = ( ( n1 + n2 ) / ( n1* n2 ) ) + ( d^2 / ( 2 * ( n1 + n2 ) ) ) # Vd = variance of d 
  
  Vg = J^2 * Vd #
  
  SE = sqrt(Vg)
  
  ### --- Calculate Hedge's d 95% CI upper and lower bounds --- ###
  
  upper_CI = d + (1.96 * SE)
    
  lower_CI = d - (1.96 * SE)
  
  ### --- Put everything together --- ###
  
  hedges_d = data.frame(
    d = d,
    upper_CI = upper_CI,
    lower_CI = lower_CI
  )
  
  return(hedges_d)
  
}