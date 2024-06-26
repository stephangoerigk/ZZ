library(ggplot2)
library(report)

roundallnumerics = function(df, digits){
  for(j in 1:ncol(df)){
    if(is.numeric(df[,j])){
      df[,j] = round(df[,j], digits)
    }
  }
  return(df)
}

p_labeller = function(vec){
  vec = as.numeric(vec)
  for(i in 1:length(vec)){
    if(is.na(vec[i]) == F & vec[i] < .001){
      vec[i] = "<.001***"
    }
    if(is.na(vec[i]) == F & vec[i] >= .001 & vec[i] < .01){
      vec[i] = paste0(vec[i], "**")
    }
    if(is.na(vec[i]) == F & vec[i] > .01 & vec[i] < .05){
      vec[i] = paste0(vec[i], "*")
    }
  }
  return(vec)
}


therapy4groups = read.csv("https://raw.githubusercontent.com/stephangoerigk/WAF_Folien/master/therapy4groups.csv")

ggplot(therapy4groups, aes(x = Therapy, y = Symptoms, colour = Diagnosis)) +
  stat_summary(position = position_dodge(.95))


ttest = t.test(Symptoms ~ Diagnosis, data = therapy4groups)
ttest$statistic

tab = as.data.frame(report_table(ttest))

tab$CI = paste0(tab$CI, " (", round(tab$CI_low, 2), " to ", round(tab$CI_high, 2), ")")
tab$d = paste0(round(tab$d, 2), " (", round(tab$d_CI_low, 2), " to ", round(tab$d_CI_high, 2), ")")

tab = BBmisc::dropNamed(tab, drop = c("Parameter",   "Group", "CI_low", "CI_high", "Method", "Alternative", "d_CI_high", "d_CI_low"))

names(tab)[1:2] = c("Mean PDD", "Mean BPD")

tab = roundallnumerics(tab,2)

mod = lm(Symptoms ~ Therapy * Diagnosis, data = therapy4groups)
tab = as.data.frame(report_table(anova(mod)))

library(tidyverse)

tab = add_column(tab, Model = "Model 1", .before = 1)
tab = roundallnumerics(tab, 3)
tab$p = p_labeller(tab$p)

library(flextable)
fix_border_issues(merge_v(flextable(tab)))



