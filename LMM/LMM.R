library(ggplot2)
library(lme4)
library(lmerTest)

d = read.csv(file = "LMM/lmer_data.csv")

names(d)

ggplot(data = d, aes(x = time, y = y, linetype = treatment)) + 
  #stat_summary() +
  #stat_summary(geom = "line") +
geom_smooth(method = "lm")

model = lm(y ~ time * treatment, data = d)
summary(model)

model_simple = lmer(y ~ time * treatment + (1|subject), data = d)
model_complex = lmer(y ~ time * treatment + (time|subject), data = d, REML = F)

anova(model_simple, model_complex)

summary(model2)
anova(model2)

# chi-square-likelihood-ratio-test


