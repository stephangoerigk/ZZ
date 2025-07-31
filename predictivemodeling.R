library(mlr)
options(scipen = 999)
# Classification  (Kategorie) vs. Regression (Zahl)

# einfache/multiple lineare Regression

lm()

# einfache/multiple logistische Regression

glm()

iris2 = iris[iris$Species != "virginica",] 

iris2$Species = droplevels(iris2$Species)

mod = glm(Species ~ Sepal.Length, data = iris2, family = "binomial")
summary(mod)


measures = list(bac, tpr, tnr, auc)

predict(mod, newdata = iris2, type = "response")

task = makeClassifTask(target = "Species", positive = "versicolor", data = iris2)

log = makeLearner("classif.logreg", predict.type = "prob")

desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)

benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)


# Predict breastcancer ----------------------------------------------------

breastcancer = read.csv("breastcancer.csv")

breastcancer = BBmisc::dropNamed(breastcancer, drop = c("X"))

task = makeClassifTask(id = "breastcancer", target = "diagnosis", positive = "M", data = BBmisc::dropNamed(breastcancer, drop = "id"))
log = makeLearner("classif.logreg", predict.type = "prob")

r = 268 / 500 
log = makeUndersampleWrapper(log, usw.rate = r)

desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
bmr = benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)


bmr$results$breastcancer$classif.logreg$pred$data


# elastic net
y = x1 + x2 + x3 + x4...
pca -> predictors
# lasso + ridge-regression

# support vector machines





