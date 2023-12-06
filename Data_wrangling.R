library(tidyverse)

starwars = as.data.frame(starwars)

names(starwars)
nrow(starwars)
ncol(starwars)

View(starwars)
str(starwars)

starwars[,"mass"]
starwars$mass

starwars[which(starwars$height <  100 & starwars$mass >  20), c("name","height", "mass")]
starwars[which(starwars$height <  100 | starwars$mass >  20), c("name","height", "mass")]

starwars[,grep("color$", names(starwars))]

starwars$name = gsub("Luke", "Skywalker", starwars$name)
starwars$name2 = gsub("Skywalker$", "Müller", starwars$name)

starwars$summe = starwars$height + starwars$mass

rowMeans(starwars[, c("height", "mass")])

mittelwert = mean(starwars$mass, na.rm = T)

starwars$mass

starwars$mass[is.na(starwars$mass)] = mittelwert

is.na(starwars)

recode(mtcars$gear, "3" = 5, "4" = 4, "5" = 3)

starwars$sex = factor(starwars$sex, levels = c("hermaphroditic", "female", "male", "none"))

levels(starwars$sex) = c("hermaphroditic", "female", "male", "nichts")

mtcars$gear = factor(mtcars$gear, levels = 3:5, labels = c("3 Gänge", "4 Gänge", "5 Gänge"))

