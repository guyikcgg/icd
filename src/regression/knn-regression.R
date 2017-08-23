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
  simple.knn.fit[[i]] = 
    kknn(abalone$Rings~abalone[,i], abalone, abalone)
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
  geom_point(
    aes(x = value, y = Rings, color = Data),
    alpha = 0.03
  ) + 
  geom_point(
    aes(x = value, y = Prediction, color = b), 
    alpha = 0.03
  ) + 
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
      tt = tt,
      k = 7   # Modify k
    ))
  }
}

## Comparative k
k = 1:20
MSE.Shell_weight = data.frame(k)
for (tt in c("train", "test")) {
  for (i in k) {
    MSE.Shell_weight[i,tt] = mean(sapply(
      1:5, 
      run_knn_fold, 
      model = Rings~Shell_weight, 
      tt = tt,
      k = i
    ))
  }
}

Data = "test"
b = "train"
ggplot(MSE.Shell_weight) + 
  geom_point(aes(x = k, y = test,  color = Data)) + 
  geom_line (aes(x = k, y = test,  color = Data)) +
  geom_point(aes(x = k, y = train, color = b)) + 
  geom_line (aes(x = k, y = train, color = b)) + 
  ylab("MSE") + 
  scale_colour_manual(values=colorPalette)
  


# Non-linearities
abalone = add.non.linearities(abalone)
abalone.tra = lapply(abalone.tra, add.non.linearities)
abalone.tst = lapply(abalone.tst, add.non.linearities)

MSE = matrix(nrow = ncol(abalone), ncol = 2)
colnames(MSE) = c("train", "test")
rownames(MSE) = names(abalone)
for (tt in colnames(MSE)) {
  for (i in rownames(MSE)) {
    MSE[i, tt] = mean(sapply(
      1:5, 
      run_knn_fold, 
      model = Rings~eval(parse(text = i)), 
      tt = tt,
      k = 7   # Modify k
    ))
  }
}


# Multiple variables
abalone = abalone[,1:9]
abalone.tra = lapply(abalone.tra, function(df){return(df[,1:9])})
abalone.tst = lapply(abalone.tst, function(df){return(df[,1:9])})
MSE.record = data.frame()

knn.MSE.data.frame = function(
  model = Rigns~., 
  k = 1:20, i = 1:5, 
  tt = c("train", "test")
) {
  MSE = data.frame(k)
  tt_ = tt
  i_  = i
  for (tt in tt_) {
    for (i in k) {
      MSE[i, tt] = mean(sapply(
        i_, 
        run_knn_fold, 
        model = model, 
        tt = tt,
        k = i   # Modify k
      ))
    }
  }
  MSE$model = as.character(myModel)[3]
  
  return(MSE)
}

plot.MSE.data.frame = function(MSE) {
  myData = melt.data.frame(
    MSE,
    id.vars = c("k", "model"),
    variable_name = "Data"
  )
  
  ggplot(myData, aes(x = k, y = value, color = Data)) + 
    geom_point() + geom_line () + ylab("MSE")
}


myModel = Rings~.

MSE = knn.MSE.data.frame(myModel)
MSE.record = rbind(MSE.record, MSE)
plot.MSE.data.frame(MSE)


## No interactions
myModel = Rings ~ 
  Diameter + 
  Height + 
  Whole_weight + 
  Shucked_weight + 
  Viscera_weight + 
  Shell_weight

MSE = knn.MSE.data.frame(myModel)
MSE.record = rbind(MSE.record, MSE)
plot.MSE.data.frame(MSE)


## Non-linearities
abalone = add.non.linearities(abalone)
abalone.tra = lapply(abalone.tra, add.non.linearities)
abalone.tst = lapply(abalone.tst, add.non.linearities)

myModel = Rings ~
  Length + 
  Height + 
  Whole_weight.log + 
  Shucked_weight.log + 
  Viscera_weight.log + 
  Shell_weight.3

MSE = knn.MSE.data.frame(myModel)
MSE.record = rbind(MSE.record, MSE)
plot.MSE.data.frame(MSE)


## Non-linearities (poly)
myModel = Rings ~
  poly(Length, 2) + 
  poly(Height, 2) + 
  poly(Whole_weight, 2) + 
  poly(Shucked_weight, 2) + 
  poly(Viscera_weight, 2) + 
  poly(Shell_weight, 2)

MSE = knn.MSE.data.frame(myModel)
MSE.record = rbind(MSE.record, MSE)
plot.MSE.data.frame(MSE)

#--
myModel = Rings ~
  poly(Length, 3) + 
  poly(Height, 3) + 
  poly(Whole_weight, 3) + 
  poly(Shucked_weight, 3) + 
  poly(Viscera_weight, 3) + 
  poly(Shell_weight, 3)

MSE = knn.MSE.data.frame(myModel)
MSE.record = rbind(MSE.record, MSE)
plot.MSE.data.frame(MSE)


## Interactions
myModel = Rings ~
  Length + 
  Height + 
  Whole_weight.log * 
  Shucked_weight.log * 
  Viscera_weight * 
  Shell_weight.3

MSE = knn.MSE.data.frame(myModel)
MSE.record = rbind(MSE.record, MSE)
plot.MSE.data.frame(MSE)

## Compare k
myData = subset(MSE.record, select = -train)
myData = myData[myData$k>2,]
myData = melt.data.frame(
  myData,
  id.vars = c("k", "model"),
  variable_name = "Data"
)
myData$Data = paste(myData$Data, " in ", myData$model)

ggplot(myData, aes(x = k, y = value, color = Data)) + 
  geom_point() + geom_line () + ylab("MSE") + 
  theme(legend.position = "bottom", legend.direction = "vertical")
