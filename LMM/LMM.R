library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(flextable)

# intention-to-treat.
# per-protocol

d = read.csv(file = "LMM/lmer_data.csv")

names(d)

d$time_discrete = factor(d$time)

ggplot(data = d, aes(x = time, y = y, linetype = treatment)) + 
   stat_summary() +
   stat_summary(geom = "line") #+
 #geom_smooth(method = "lm")

model = lm(y ~ time * treatment, data = d)
summary(model)

model_simple = lmer(y ~ time * treatment + (1|subject), data = d)
model_complex = lmer(y ~ time * treatment + (time|subject), data = d)

model_discrete = lmer(y ~ time_discrete * treatment + (1|subject), data = d)

anova(model_simple, model_complex)

anova(model_complex)
summary(model_complex)

emmeans(object = model_discrete, specs = pairwise ~ treatment | time_discrete)
emmeans(object = model_discrete, specs = pairwise ~ time_discrete | treatment, adjust = "tukey")

em = emtrends(object = model_complex, specs = pairwise ~ treatment, var = "time")

flextable(as.data.frame(em$emtrends))

em$contrasts

model_simple = lmer(y ~ time * treatment + (time|Therapist/subject), data = d)


# Übung -------------------------------------------------------------------


ChickWeight
bmi

# Gibt es innerhalb der Diet-Gruppen signfikante Gewichtszunahmen?

# Unterscheiden sich die Diet-Gruppen voneinander (nehmen einige stärker zu als andere)?

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  stat_summary() +
  geom_smooth(method = "lm", formula= y ~ x + I(x^2) ) +
geom_smooth(method = "lm", colour = "red")


mod1 = lmer(weight ~ Time * Diet + (1|Chick), data = ChickWeight)
  
mod2 = lmer(weight ~ Time * Diet + (Time|Chick), data = ChickWeight)

anova(mod1, mod2)

em = emtrends(object = mod2, specs = pairwise ~ Diet, var = "Time", adjust = "none")

anova(mod2)

effectsize::omega_squared(mod2)

eff_size(em, sigma = sigma(mod2), edf = 45)








