
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
myData1 = cbind(myData1, data.type = "prediction", method = "Linear Model")
myData1 = rbind(myData1, cbind(myTest, data.type = "original value", method = "Linear Model"))
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
myData2 = cbind(myData2, data.type = "prediction", method = "k-NN")
myData2 = rbind(myData2, cbind(myTest, data.type = "original value", method = "k-NN"))
myData2 = melt.data.frame(
  data = myData2,
  id.vars = c("Shell_weight", "data.type", "method"),
  measure.vars = "Rings"
)


## Plot
myData = rbind(myData1, myData2)

ggplot(myData, aes(x = Shell_weight, y = value, color = data.type)) + 
  geom_point() + 
  ylab("Rings") +
  xlab("Shell weight") + 
  geom_line(aes(group = Shell_weight, color = "error")) +
  labs(color = "Data type") + 
  facet_wrap( ~ method, ncol = 1)

