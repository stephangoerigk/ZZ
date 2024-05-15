library(mlr)

# Classification  (Kategorie) vs. Regression (Zahl)

# einfache/multiple lineare Regression

lm()

# einfache/multiple logistische Regression

glm()

iris2 = iris[iris$Species != "virginica",] 

iris2$Species = droplevels(iris2$Species)

mod = glm(Species ~ Sepal.Length, data = iris2, family = "binomial")
summary(mod)


measures = list(bac)

predict(mod, newdata = iris2, type = "response")

task = makeClassifTask(target = "Species", positive = "versicolor", data = iris2)

log = makeLearner("classif.logreg", predict.type = "prob")

desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)

benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)


# Predict breastcancer ----------------------------------------------------

breastcancer = read.csv("breastcancer.csv")

breastcancer = BBmisc::dropNamed(breastcancer, drop = "X")


task = makeClassifTask(target = "diagnosis", positive = "M", data = breastcancer)
log = makeLearner("classif.logreg", predict.type = "prob")
desc = makeResampleDesc(method = "RepCV", folds = 5L, reps = 10L)
benchmark(learners = log, tasks = task, resamplings = desc, measures = measures)


