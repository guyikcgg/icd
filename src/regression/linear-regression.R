################################################
#      Introducción a la Ciencia de Datos      #
#               REGRESIÓN LINEAR               #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Build the workspace
source("../regression/build-workspace.R")


# Get simple linear regression models
simple.linear.fit = list()
for (i in 1:(length(abalone)-1)) {
  simple.linear.fit[[i]] = lm(abalone$Rings~abalone[,i])  
  names(simple.linear.fit)[i] = names(abalone)[i]
}

summary(simple.linear.fit$Sex)
summary(simple.linear.fit$Length)
summary(simple.linear.fit$Diameter)
summary(simple.linear.fit$Height)
summary(simple.linear.fit$Whole_weight)
summary(simple.linear.fit$Shucked_weight)
summary(simple.linear.fit$Viscera_weight)
summary(simple.linear.fit$Shell_weight)


MSE = matrix(nrow = 8, ncol = 2)
colnames(MSE) = c("train", "test")
rownames(MSE) = names(abalone)[1:8]
for (tt in colnames(MSE)) {
  for (i in rownames(MSE)) {
    MSE[i, tt] = mean(sapply(
      1:5, 
      run_lm_fold, 
      model = Rings~eval(parse(text = i)), 
      tt = tt
    ))
  }
}

# Using the shell weight seems to be the best model, 
# since it produces less error.

# Scatterplots
myData = melt.data.frame(
  abalone[abalone$Height<0.3,],
  id.vars=c("Sex", "Rings")
)
myData[myData$variable=="Height",] =
  within(myData[myData$variable=="Height",], {
    value = jitter(value, factor = 3)
  }
  )
ggplot(myData, aes(x = value, y = Rings)) + 
  geom_jitter(alpha = 0.03) + 
  geom_smooth(method = lm) + 
  facet_wrap( ~ variable, ncol = 2, scales = "free") + 
  xlab("")


# Analysis of preprocessing
abalone = add.non.linearities(abalone)
abalone.tra = lapply(abalone.tra, add.non.linearities)
abalone.tst = lapply(abalone.tst, add.non.linearities)


## Calculate correlations
Y.vars = grepl("Rings", names(abalone))
X.vars = grepl("_weight", names(abalone))
cor.Shucked_weight = cor(
  subset(abalone, select = X.vars),
  subset(abalone, select = Y.vars)
)

## New models, based on new variables
simple.linear.fit = list()
for (i in 1:(length(abalone))) {
  simple.linear.fit[[i]] = lm(abalone$Rings~abalone[,i])  
  names(simple.linear.fit)[i] = names(abalone)[i]
}

summary(simple.linear.fit$Whole_weight.log)
summary(simple.linear.fit$Shucked_weight.2)
summary(simple.linear.fit$Shucked_weight.3)
summary(simple.linear.fit$Shucked_weight.log)
summary(simple.linear.fit$Viscera_weight.log)
summary(simple.linear.fit$Shell_weight.3)
summary(simple.linear.fit$Shell_weight.log)

MSE = matrix(nrow = ncol(abalone), ncol = 2)
colnames(MSE) = c("train", "test")
rownames(MSE) = names(abalone)
for (tt in colnames(MSE)) {
  for (i in rownames(MSE)) {
    MSE[i, tt] = mean(sapply(
      1:5, 
      run_lm_fold, 
      model = Rings~eval(parse(text = i)), 
      tt = tt
    ))
  }
}


# Plot
myData = melt.data.frame(
  abalone[abalone$Height<0.3,],
  id.vars=c("Sex", names(abalone)[Y.vars])
)

selected.fields = c(
  "Whole_weight",
  "log(Whole_weight)",
  "Shucked_weight",
  "log(Shucked_weight)",
  "Viscera_weight",
  "log(Viscera_weight)",
  "Shell_weight",
  "(Shell_weight)^(1/3)"
)
names(selected.fields) = c(
  "Whole_weight",
  "Whole_weight.log",
  "Shucked_weight",
  "Shucked_weight.log",
  "Viscera_weight",
  "Viscera_weight.log",
  "Shell_weight",
  "Shell_weight.3"
)

ggplot(
  subset(myData, variable %in% names(selected.fields)),
  aes(x = value, y = Rings)
) + 
  geom_jitter(alpha = 0.03) + geom_smooth(method = lm) +
  facet_wrap( 
    ~ variable, 
    ncol = 2, 
    scales = "free", 
    labeller = as_labeller(selected.fields)
  ) + 
  xlab("")

# Linear regression using multiple variables
## Only linear models
abalone = abalone[,1:9]

myFit = lm(Rings~., abalone)
summary(myFit)

myModel = Rings ~ 
  Diameter + 
  Height + 
  Whole_weight + 
  Shucked_weight + 
  Viscera_weight + 
  Shell_weight
myFit = lm(myModel, abalone)
summary(myFit)


MSE = matrix(nrow = 1, ncol = 2)
colnames(MSE) = c("train", "test")
for (tt in colnames(MSE)) {
  MSE[1, tt] = mean(sapply(
    1:5, 
    run_lm_fold, 
    model = myModel, 
    tt = tt
  ))
}

# Add non-linearities
abalone = add.non.linearities(abalone)

myModel = Rings ~
  Length +
  Height + 
  Whole_weight.log +
  Shucked_weight.log +
  Viscera_weight.log +
  Shell_weight.3
myFit = lm(Rings~., abalone)
summary(myFit)

myFit = lm(myModel, abalone)
summary(myFit)

MSE = matrix(nrow = 1, ncol = 2)
colnames(MSE) = c("train", "test")
for (tt in colnames(MSE)) {
  MSE[1, tt] = mean(sapply(
    1:5, 
    run_lm_fold, 
    model = myModel, 
    tt = tt
  ))
}

myModel = Rings ~ 
  poly(Length, 2) +
  poly(Height, 2) + 
  poly(Whole_weight, 2) +
  poly(Shucked_weight, 2) +
  poly(Viscera_weight, 2) +
  poly(Shell_weight, 2)
myFit = lm(myModel, abalone)
summary(myFit)

MSE = matrix(nrow = 1, ncol = 2)
colnames(MSE) = c("train", "test")
for (tt in colnames(MSE)) {
  MSE[1, tt] = mean(sapply(
    1:5, 
    run_lm_fold, 
    model = myModel,
    tt = tt
  ))
}

# Add interactions
myModel = Rings ~ 
  Length +
  Height +
  Whole_weight.log *
  Shucked_weight.log *
  Viscera_weight *
  Shell_weight.3

myFit = lm(myModel, abalone)
summary(myFit)

MSE = matrix(nrow = 1, ncol = 2)
colnames(MSE) = c("train", "test")
for (tt in colnames(MSE)) {
  MSE[1, tt] = 
    mean(sapply(1:5, run_lm_fold, model = myModel, tt = tt))
}
