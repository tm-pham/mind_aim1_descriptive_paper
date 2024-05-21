# Thi Mui Pham, t.m.pham@hsph.harvard.edu
# ============================================================================ #
# Function to get marginalized time trend coefficient and convert it to AAPC
# ============================================================================ #
# This function computes marginalized time trend coefficients, assuming a
# Poisson distribution with log-link for the outcome variable. The result is
# then converted to Average Annual Percentage Change (AAPC). 
# It also assumes that the time unit is one year with the option to change to 
# quarter if the input variable 'quarter' is set to TRUE. 
# ============================================================================ #
aapc.function <- function(model, 
                          specs = "~ time_1", 
                          var = "time_1", 
                          n_round=1, 
                          quarter = F, 
                          conf_level = 0.95,
                          p_value = T){
  if(quarter){
    res <- summary(emtrends(model, as.formula(specs), var=var, level = conf_level))[c(paste0(var, ".trend"), "asymp.LCL", "asymp.UCL")]
    colnames(res)[1] <- "time.trend"
    res <- round(100*(exp(res*4)-1), n_round)
  }else{
    res <- summary(emtrends(model, as.formula(specs), var=var, level = conf_level))[c(paste0(var, ".trend"), "asymp.LCL", "asymp.UCL")]
    colnames(res)[1] <- "time.trend"
    res <- round(100*(exp(res)-1), n_round)
  }
  
  if(p_value){
    res$p_value <- test(emtrends(model, as.formula(specs), var=var, level = conf_level))[,"p.value"]
  }
  

  return(res)
  
}
