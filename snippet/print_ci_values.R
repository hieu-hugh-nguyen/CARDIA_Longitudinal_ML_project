# print bootstrapped performance in medan (lower bound, upper bound of 95% confidence interval)
print_ci_values <- function(ci_output, digits = 2){
  return(paste0(round(ci_output[2], digits = digits),' (',
                ci_output[1] %>% round(digits = digits),', ',
                ci_output[3] %>% round(digits = digits),')'))
}
