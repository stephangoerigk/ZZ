set.seed(1)

p <- study_parameters(n1 = 11,
                      n2 = 10,
                      n3 = 4,
                      T_end = 10,
                      fixed_intercept = 37,
                      fixed_slope = -0.25,
                      sigma_subject_intercept = 2.89,
                      sigma_cluster_intercept = 0.6,
                      icc_slope = 0.1,
                      var_ratio = 0.03,
                      sigma_error = 1.5,
                      cor_subject = -0.5,
                      cor_cluster = 0,
                      cohend = 0.4)

d <- simulate_data(p)
d = rename(d, "Therapist" = cluster)
d$treatment = factor(d$treatment, levels = 0:1, labels = c("Tx", "Waitlist"))

write.csv(d, file = "/Users/stephangoerigk/Desktop/Universität/CFH/ZZ/ZZ/LMM/lmer_data.csv")

ggplot(data = d, aes(x = time, y = y, linetype = treatment)) +
  stat_summary() +
stat_summary(geom = "line")

mod = lmer(y ~ time * treatment + (time|subject), data = d)
summary(mod)





