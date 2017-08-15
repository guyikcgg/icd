################################################
#      Introducción a la Ciencia de Datos      #
#               REGRESIÓN k-NN                 #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Build the workspace
source("../regression/build-workspace.R")

abalone = abalone[abalone$Height<0.3,]

# Get simple knn models
simple.knn.fit = list()
for (i in 1:(length(abalone)-1)) {
  simple.knn.fit[[i]] = kknn(abalone$Rings~abalone[,i], abalone, abalone)
  names(simple.knn.fit)[i] = names(abalone)[i]
}


## Scatterplots
myData1 = melt.data.frame(
  abalone,
  id.vars=c("Sex", "Rings")
)

myData2 = data.frame()
for (variable in names(simple.knn.fit)[2:8]) {
  Prediction = simple.knn.fit[[variable]]$fitted.values
  myData2 = rbind(myData2, data.frame(Prediction))
}

myData = cbind(myData1, myData2)

Data = "Original data"
b = "Prediction"
colorPalette <- c("#000000", "#56B4E9")

ggplot(myData) + 
  geom_point(aes(x = value, y = Rings, color = Data), alpha = 0.03) + 
  geom_point(aes(x = value, y = Prediction, color = b), alpha = 0.03) + 
  facet_wrap( ~ variable, ncol = 2, scales = "free") + 
  xlab("") +
  scale_colour_manual(values=colorPalette) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

## MSE
MSE = matrix(nrow = 8, ncol = 2)
colnames(MSE) = c("train", "test")
rownames(MSE) = names(abalone)[1:8]
for (tt in colnames(MSE)) {
  for (i in rownames(MSE)) {
    MSE[i, tt] = mean(sapply(
      1:5, 
      run_knn_fold, 
      model = Rings~eval(parse(text = i)), 
      tt = tt
    ))
  }
}

## Comparative k
MSE.Shell_weight = c()
k = 1:20
for (i in k) {
  MSE.Shell_weight[i] = mean(sapply(
    1:5, 
    run_knn_fold, 
    model = Rings~Shell_weight, 
    k = i
  ))
}
MSE.Shell_weight = data.frame(k, MSE.Shell_weight)

ggplot(MSE.Shell_weight, aes(x = k, y = MSE.Shell_weight)) + geom_point() + geom_line()




