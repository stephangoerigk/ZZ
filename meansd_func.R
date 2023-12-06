library(tidyverse)

means_sd_func = function(x){
  m = mean(x)
  s = round(sd(x), 2)
  res = paste0(m, " (", s, ")")
  return(res)
}

