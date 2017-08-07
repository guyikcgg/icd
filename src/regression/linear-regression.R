################################################
#      Introducción a la Ciencia de Datos      #
#               REGRESIÓN LINEAR               #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Load required libraries
library(utils)
library(stats)
library(foreign)
library(ggplot2)
library(reshape)

# FUNCTIONS

run_lm_fold = function(i, file.name, model = Y~.,  tt = "test", n = 5) {
  # Open files
  file.name = paste("datasets", file.name, file.name, sep = "/")

  file = paste(file.name, "-", as.character(n), "-", i, "tra.dat", sep = "")
  x_tra = read.csv(file, comment.char = "@")
  file = paste(file.name, "-", as.character(n), "-", i, "tst.dat", sep = "")
  x_tst = read.csv(file, comment.char = "@")
  ln = length(names(x_tra))-1

  # Give names
  names(x_tra)[1:ln] = paste("X", 1:ln, sep = "")
  names(x_tra)[ln+1] = "Y"
  names(x_tst)[1:ln] = paste("X", 1:ln, sep = "")
  names(x_tst)[ln+1] = "Y"

  if (tt == "train") {
    test = x_tra
  } else {
    test = x_tst
  }

  fitMulti = lm(model, x_tra)

  # Get MSE Error
  yprime = predict(fitMulti, test)
  sum(abs(test$Y-yprime)^2)/length(yprime)
}



run_lm_fold.abalone = function(i, model = Rings~.,  Y = "Rings", tt = "test") {
  # Open files
  file.name = "abalone"
  n = 5
  file.name = paste("datasets", file.name, file.name, sep = "/")

  file = paste(file.name, "-", as.character(n), "-", i, "tra.dat", sep = "")
  x_tra = read.csv(file, comment.char = "@")
  file = paste(file.name, "-", as.character(n), "-", i, "tst.dat", sep = "")
  x_tst = read.csv(file, comment.char = "@")
  ln = length(names(x_tra))-1

  # Give names
  names.abalone = c(
    "Sex",
    "Length",
    "Diameter",
    "Height",
    "Whole_weight",
    "Shucked_weight",
    "Viscera_weight",
    "Shell_weight",
    "Rings"
  )
  names(x_tra) = names.abalone
  names(x_tst) = names.abalone
  x_tra$Sex = factor(
    x_tra$Sex,
    levels = c(1, 2, 3),
    labels = c("M", "F", "I")
  )
  x_tst$Sex = factor(
    x_tst$Sex,
    levels = c(1, 2, 3),
    labels = c("M", "F", "I")
  )

  # Additional fields
  x_tra$Shucked_weight.3 = (x_tra$Shucked_weight)^(1/3)
  x_tra$Shucked_weight.4 = (x_tra$Shucked_weight)^(1/4)
  x_tra$Shucked_weight.8 = (x_tra$Shucked_weight)^(1/8)
  x_tst$Shucked_weight.3 = (x_tst$Shucked_weight)^(1/3)
  x_tst$Shucked_weight.4 = (x_tst$Shucked_weight)^(1/4)
  x_tst$Shucked_weight.8 = (x_tst$Shucked_weight)^(1/8)


  if (tt == "train") {
    test = x_tra
  } else {
    test = x_tst
  }

  fitMulti = lm(model, x_tra)

  # Get MSE Error
  yprime = predict(fitMulti, test)
  sum(abs(test[,Y]-yprime)^2)/length(yprime)
}


# ABALONE

# Load the dataset and provide it with the
# structure from Keel
abalone = read.csv(
  "datasets/abalone/abalone.dat",
  comment.char="@"
)
names(abalone) = c(
  "Sex",
  "Length",
  "Diameter",
  "Height",
  "Whole_weight",
  "Shucked_weight",
  "Viscera_weight",
  "Shell_weight",
  "Rings"
)


abalone$Sex = factor(
  abalone$Sex,
  levels = c(1, 2, 3),
  labels = c("M", "F", "I")
)

# Analysis of preprocessing

## Correlation
abalone3 = abalone# subset(abalone, select = -Sex)
abalone3$Rings2 = abalone3$Rings^2
abalone3$Rings3 = abalone3$Rings^3
abalone3$Rings.2 = abalone3$Rings^(1/2)
abalone3$Rings.3 = abalone3$Rings^(1/3)
abalone3$Rings.log = log(abalone3$Rings)
abalone3$Shucked_weight2 = (abalone3$Shucked_weight)^2
abalone3$Shucked_weight3 = (abalone3$Shucked_weight)^3
abalone3$Shucked_weight.2 = sqrt(abalone3$Shucked_weight)
abalone3$Shucked_weight.3 = (abalone3$Shucked_weight)^(1/3)
abalone3$Shucked_weight.4 = (abalone3$Shucked_weight)^(1/4)
abalone3$Shucked_weight.8 = (abalone3$Shucked_weight)^(1/8)
abalone3$Shucked_weight.log = log(abalone3$Shucked_weight)

Y.vars = grepl("Rings", names(abalone3))
X.vars = grepl("Shucked_weight", names(abalone3))
cor.Shucked_weight = cor(
  subset(abalone3, select = X.vars),
  subset(abalone3, select = Y.vars)
)

# Plot
myData = melt.data.frame(
  abalone3[abalone3$Height<0.3,],
  id.vars=c("Sex", names(abalone3)[Y.vars])
)

selected.fields = c(
  "Shucked_weight",
  "(Shucked_weight)^(2)",
  "(Shucked_weight)^(3)",
  "(Shucked_weight)^(1/2)",
  "(Shucked_weight)^(1/3)",
  "(Shucked_weight)^(1/4)",
  "(Shucked_weight)^(1/8)",
  "log(Shucked_weight)"
)
names(selected.fields) = c(
  "Shucked_weight",
  "Shucked_weight2",
  "Shucked_weight3",
  "Shucked_weight.2",
  "Shucked_weight.3",
  "Shucked_weight.4",
  "Shucked_weight.8",
  "Shucked_weight.log"
)

ggplot(subset(myData, variable %in% names(selected.fields)), aes(y = value, x = Rings)) + geom_jitter(width = 0.8, alpha = 0.12) + facet_wrap( ~ variable, ncol = 2, scales = "free", labeller = as_labeller(selected.fields)) + ylab("")
ggplot(subset(myData, variable %in% names(selected.fields)), aes(y = value, x = Rings.log)) + geom_jitter(width = 0.8, alpha = 0.12) + facet_wrap( ~ variable, ncol = 2, scales = "free", labeller = as_labeller(selected.fields)) + ylab("") + xlab("log(Rings)")

# Logarithmical growth makes greater correlation. Cube makes more beautiful plot...





# Fit a simple model using just `Shucked_weight` and preprocessed `Shucked_weight` and `Rings`.
# Use K-fold cross validation to check which preprocessing works best.






#lmMSEtrain = mean( sapply(1:5, run_lm_fold, "abalone", model = Y~X6, tt = "train") )
#lmMSEtest = mean( sapply(1:5, run_lm_fold, "abalone", model = Y~X6, tt = "test") )
#lmMSEtest = mean( sapply(1:5, run_lm_fold, "abalone", model = Y~log(X6), tt = "test") )
#lmMSEtest = mean( sapply(1:5, run_lm_fold, "abalone", model = Y~sqrt(X6), tt = "test") )
#lmMSEtest = mean( sapply(1:5, run_lm_fold, "abalone", model = Y~(X6^1/3), tt = "test") )

mean( sapply(1:5, run_lm_fold.abalone, model = Rings~Shucked_weight.8, Y = "Rings", tt = "test") )
summary(lm(Rings.log~Shucked_weight.log, abalone3))

myFit = lm(Rings~Shucked_weight, abalone)

summary(myFit)

#summary(lm(log(Rings)~log(Shucked_weight), abalone))


ggplot(subset(myData, variable %in% "Shucked_weight"), aes(y = log(value), x = Rings)) +
  geom_jitter(width = 0.8, alpha = 0.12) +
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap( ~ variable, ncol = 2, scales = "free", labeller = as_labeller(selected.fields)) +
  ylab("")
