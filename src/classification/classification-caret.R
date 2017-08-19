################################################
#      Introducción a la Ciencia de Datos      #
#                CLASIFICACIÓN                 #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Build the workspace
source("../classification/build-workspace.R")

# Apply k-NN algorithm to the first set
knnModel = train(
  x = tae.tra[[1]],
  y = tae.tra.cl[[1]],
  method = "knn",
  preProcess = c("center", "scale")
)

knnFit = train(
  x = tae.tra[[1]],
  y = tae.tra.cl[[1]],
  method = "knn",
  #preProcess = c("center", "scale"),
  metric = "Accuracy",
  tuneGrid = data.frame(.k=1:15)
)

knnPred = predict(knnModel, newdata = tae.tst[[1]])

ActualClass      = tae.tst.cl[[1]]
PredictClass     = knnPred
ConfusionMatrix = table(PredictClass, ActualClass)





confusionMatrix(ConfusionMatrix)


tae_test_pred = knn(
  train = tae.tra[[1]],    # Dataframe
  test  = tae.tst[[1]],    # Dataframe
  cl    = tae.tra.cl[[1]], # Vector (factor)
  k     = 21
)

table(tae_test_pred, tae.tst.cl[[1]])
