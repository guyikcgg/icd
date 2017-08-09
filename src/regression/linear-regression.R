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

# Using the shell weight seems to be the best model, since it produces less error.

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
ggplot(myData, aes(x = value, y = Rings)) + geom_jitter(alpha = 0.03) + geom_smooth(method = lm) + facet_wrap( ~ variable, ncol = 2, scales = "free") + xlab("")


# Analysis of preprocessing

## Correlation
add.non.linearities = function(df) {
  # Remove new fields
  df = df[,1:9]
  
  df$Whole_weight.2     = (df$Whole_weight)^(1/2)
  df$Whole_weight.3     = (df$Whole_weight)^(1/3)
  df$Whole_weight.log   = log(df$Whole_weight)
  df$Shucked_weight.2   = (df$Shucked_weight)^(1/2)
  df$Shucked_weight.3   = (df$Shucked_weight)^(1/3)
  df$Shucked_weight.log = log(df$Shucked_weight)
  df$Viscera_weight.2   = (df$Viscera_weight)^(1/2)
  df$Viscera_weight.3   = (df$Viscera_weight)^(1/3)
  df$Viscera_weight.log = log(df$Viscera_weight)
  df$Shell_weight.2     = (df$Shell_weight)^(1/2)
  df$Shell_weight.3     = (df$Shell_weight)^(1/3)
  df$Shell_weight.log   = log(df$Shell_weight)
  
  return(df)
}

abalone = add.non.linearities(abalone)
abalone.tra = lapply(abalone.tra, add.non.linearities)
abalone.tst = lapply(abalone.tst, add.non.linearities)


# Calculate correlations
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

ggplot(subset(myData, variable %in% names(selected.fields)), aes(x = value, y = Rings)) + 
  geom_jitter(alpha = 0.03) + geom_smooth(method = lm) +
  facet_wrap( ~ variable, ncol = 2, scales = "free", labeller = as_labeller(selected.fields)) + 
  ylab("")
#ggplot(subset(myData, variable %in% names(selected.fields)), aes(y = value, x = Rings.log)) + geom_jitter(width = 0.8, alpha = 0.12) + facet_wrap( ~ variable, ncol = 2, scales = "free", labeller = as_labeller(selected.fields)) + ylab("") + xlab("log(Rings)")

# Logarithmical growth makes greater correlation. Cube makes more beautiful plot...





# Fit a simple model using just `Shucked_weight` and preprocessed `Shucked_weight` and `Rings`.
# Use K-fold cross validation to check which preprocessing works best.






#lmMSEtrain = mean( sapply(1:5, run_lm_fold, "abalone", model = Y~X6, tt = "train") )
#lmMSEtest = mean( sapply(1:5, run_lm_fold, "abalone", model = Y~X6, tt = "test") )
#lmMSEtest = mean( sapply(1:5, run_lm_fold, "abalone", model = Y~log(X6), tt = "test") )
#lmMSEtest = mean( sapply(1:5, run_lm_fold, "abalone", model = Y~sqrt(X6), tt = "test") )
#lmMSEtest = mean( sapply(1:5, run_lm_fold, "abalone", model = Y~(X6^1/3), tt = "test") )

mean( sapply(1:5, run_lm_fold, model = Rings~., tt = "test") )
summary(lm(Rings.log~Shucked_weight.log, abalone))

myFit = lm(Rings~Shucked_weight, abalone)

summary(myFit)


ggplot(subset(myData, variable %in% "Shucked_weight"), aes(y = log(value), x = Rings)) +
  geom_jitter(width = 0.8, alpha = 0.12) +
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap( ~ variable, ncol = 2, scales = "free", labeller = as_labeller(selected.fields)) +
  ylab("")
