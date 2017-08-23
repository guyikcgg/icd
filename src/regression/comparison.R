################################################
#      Introducción a la Ciencia de Datos      #
#           COMPARATIVA DE ALGORITMOS          #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Build the workspace
source("../regression/build-workspace.R")

abalone.tra = lapply(abalone.tra, add.non.linearities)
abalone.tst = lapply(abalone.tst, add.non.linearities)


# Comparison of data points

myModel = Rings ~
  Length + 
  Height + 
  Whole_weight.log * 
  Shucked_weight.log * 
  Viscera_weight * 
  Shell_weight.3

myTrain = abalone.tra[[1]]
myTest  = abalone.tst[[1]][sample(1:nrow(abalone.tst[[1]]), 20), ]

## Linear regression
# Plot some points of the last model
myFit = lm(myModel, myTrain)
myResult = predict(myFit, myTest)

myData1 = myTest
myData1$Rings = myResult
myData1 = cbind(
  myData1, 
  data.type = "prediction", 
  method = "Linear Model"
)
myData1 = rbind(
  myData1, 
  cbind(
    myTest, 
    data.type = "original value", 
    method = "Linear Model"
  )
)
myData1 = melt.data.frame(
  data = myData1,
  id.vars = c("Shell_weight", "data.type", "method"),
  measure.vars = "Rings"
)

## k-NN
# Plot some points of the last model
myResult = kknn(myModel, myTrain, myTest, k = 7)

myData2 = myTest
myData2$Rings = myResult$fitted.values
myData2 = cbind(
  myData2, 
  data.type = "prediction", 
  method = "k-NN"
)
myData2 = rbind(
  myData2, 
  cbind(
    myTest, 
    data.type = "original value", 
    method = "k-NN"
  )
)
myData2 = melt.data.frame(
  data = myData2,
  id.vars = c("Shell_weight", "data.type", "method"),
  measure.vars = "Rings"
)


## Plot
myData = rbind(myData1, myData2)

ggplot(
  myData,
  aes(x = Shell_weight, y = value, color = data.type)
) + 
  geom_point() + 
  ylab("Rings") +
  xlab("Shell weight") + 
  geom_line(aes(group = Shell_weight, color = "error")) +
  labs(color = "Data type") + 
  facet_wrap( ~ method, ncol = 1)


# Error distribution
myTest = abalone.tst[[1]]
lmErrors  = 
  myTest$Rings-predict(myFit, myTest)
knnErrors = 
  myTest$Rings-kknn(myModel, myTrain, myTest)$fitted.values

### Tests
wilcox.test(lmErrors^2, knnErrors^2, paired = FALSE)
ks.test(lmErrors^2, knnErrors^2)

### Plot
myData1 = data.frame(method = "Linear Model", Err = lmErrors)
myData2 = data.frame(method = "k-NN", Err = knnErrors)
myData = melt.data.frame(
  data = rbind(myData1, myData2),
  id.vars = "method"
)
ggplot(myData, aes(x=value^2)) + 
  geom_density(aes(color = method, fill = method), alpha = 0.3) +
  xlab("Squared error") + xlim(0,20)


# General algorithm comparison (using MSE from every database)
## Read data
testResults = read.csv("../regression/regr_test_alumnos.csv")
testTable = testResults[, 2:ncol(testResults)]
rownames(testTable) = testResults[,1]

trainResults = read.csv("../regression/regr_train_alumnos.csv")
trainTable = trainResults[, 2:ncol(testResults)]
rownames(trainTable) = trainResults[,1]

## Comparison between LM and kNN 
## (kNN is assumed to provide better results)
difs = (testTable[,1] - testTable[,2])/testTable[,1]

wilc_1_2 = cbind(
  ifelse(difs<0, abs(difs)+0.1, 0.1),
  ifelse(difs>0, abs(difs)+0.1, 0.1)
)
colnames(wilc_1_2) = colnames(testTable)[1:2]

### Wikcoxon tests
LMvsKNNtst = wilcox.test(
  wilc_1_2[,1], 
  wilc_1_2[,2], 
  alternative = "two.sided", 
  paired = TRUE
)

Rplus  = LMvsKNNtst$statistic
pvalue = LMvsKNNtst$p.value

LMvsKNNtst = wilcox.test(
  wilc_1_2[,2], 
  wilc_1_2[,1], 
  alternative = "two.sided", 
  paired = TRUE
)

Rminus = LMvsKNNtst$statistic

WilcoxonTestOutput = cbind(Rplus, Rminus, pvalue)


## Comparison amongst LM, kNN and M5'
friedman.test(as.matrix(testTable))

#### p-value < 0.05 => significative difference exist

groups = rep(1:ncol(testTable), each = nrow(testTable))
pairwise.wilcox.test(
  as.matrix(testTable), 
  groups, 
  p.adjust.method = "holm", 
  paired = TRUE
)

#### M5' seems to work better (confidence: 84%)
#### LM and kNN seems not to present any difference

