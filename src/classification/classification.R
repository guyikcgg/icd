################################################
#      Introducción a la Ciencia de Datos      #
#                CLASIFICACIÓN                 #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Build the workspace
source("../classification/build-workspace.R")

# Some info about the classes
round(prop.table(table(tae$Class)) * 100, digits = 1)

# Normalize numeric data
tae$Size = normalize(tae$Size)
tae.tra.dt = lapply(tae.tra.dt, normalize.tae)
tae.tst.dt = lapply(tae.tst.dt, normalize.tae)



################################
# k Nearest Neighbourgs (k-NN) #
################################

# Apply k-NN algorithm to the first set
tae_test_pred = knn(
  train = tae.tra.dt[[1]], # Dataframe
  test  = tae.tst.dt[[1]], # Dataframe
  cl    = tae.tra.cl[[1]], # Vector (factor)
  k     = 21
)

# See the results on a contingency table (confusion matrix)
table(tae_test_pred, tae.tst.cl[[1]])

# Apply k-NN algorithm to every dataset and check the 
# overall accuracy for several values of k. Repeat this process
# m times, to get the random part working.
k.max = 35
Acc = data.frame(k = 1:k.max)
for (m in 1:9) {
  for (k in 1:k.max) {
    acc = rep(0, 10)
    for (i in 1:10) {
      tae_test_pred = knn(
        train = tae.tra.dt[[i]], # Dataframe
        test  = tae.tst.dt[[i]], # Dataframe
        cl    = tae.tra.cl[[i]], # Vector (factor)
        k     = k
      )
      acc[i] = accuracy(
        table(tae_test_pred, tae.tst.cl[[i]])
      )
    }
    Acc[k, paste("Acc", m, sep = "")] = mean(acc)
  }
}

Acc$Acc_mean = rowMeans(Acc[,-1])

myData = melt.data.frame(
  data = Acc,
  id.vars = "k"
)
myData$lineSize = 1
myData[myData$variable=="Acc_mean",]$lineSize = 2

max(Acc$Acc_mean)
which.max(Acc$Acc_mean)

ggplot(
  myData, 
  aes(x = k, y = value, color = variable, size = lineSize)
) + 
  geom_line() + scale_size(range = c(0.5, 2), guide="none") + 
  ylab("Accuracy")
## It seems that k=1 performs the best, with Acc = 0.61
# This plot could also be done with training data 
# in order to test for overfitting


# Fill in the table in the document
Acc.knn = matrix(ncol = 10, nrow = 10)
rownames(Acc.knn) = paste("K", 1:10, sep = "")
colnames(Acc.knn) = 
  c(paste("Acc", 1:9, sep = ""), "Acc_mean")
Kappa.knn = matrix(ncol = 10, nrow = 10)
rownames(Kappa.knn) = paste("K", 1:10, sep = "")
colnames(Kappa.knn) = 
  c(paste("Kappa", 1:9, sep = ""), "Kappa_mean")
for (m in 1:9) {
    myAcc   = rep(0, 10)
    myKappa = rep(0, 10)
    for (i in 1:10) {
      tae_test_pred = knn(
        train = tae.tra.dt[[i]], # Dataframe
        test  = tae.tst.dt[[i]], # Dataframe
        cl    = tae.tra.cl[[i]], # Vector (factor)
        k     = 1
      )
      cm = confusionMatrix(
        table(tae_test_pred, tae.tst.cl[[i]])
      )
      myAcc[i]   = cm$overall[1]
      myKappa[i] = cm$overall[2]
    }
    Acc.knn[,m]   = myAcc
    Kappa.knn[,m] = myKappa
}
Acc.knn[,10]   = rowMeans(Acc.knn[,1:9])
Kappa.knn[,10] = rowMeans(Kappa.knn[,1:9])


######################################
# Linear Discriminant Analysis (LDA) #
######################################

# Check for normality of (pseudo-)numeric variables
sapply(tae.original[1:5], shapiro.test)

myData = melt.data.frame(
  data = tae.original,
  measure.vars = c(
    "Native",
    "Instructor",
    "Course",
    "Semester",
    "Size"
  )
)
ggplot(myData) + 
  stat_qq(aes(sample = value)) + 
  facet_wrap(~variable, ncol = 2, scales = "free")

# LDA fit
lda.fit = lda(
  Class ~ .,
  data = tae.tra[[1]]
)

lda.pred = predict(lda.fit, tae.tst.dt[[1]])

table(lda.pred$class, tae.tst.cl[[1]])

# Do it for every dataset in k-fold CV
acc = rep(0, 10)
for (i in 1:10) {
  lda.fit = lda(
    Class ~ .,
    data = tae.tra[[i]]
  )
  lda.pred = predict(lda.fit, tae.tst.dt[[i]])
  
  acc[i] = accuracy(
    table(lda.pred$class, tae.tst.cl[[i]])
  )
}
Acc.LDA = data.frame(fold = 1:10, Acc = acc)

mean(Acc.LDA$Acc)

# LDA performs poorly if every variable is considered.
# However, there are not real numeric variables, only
# ordered factors, with no normal distribution.
# Other variables should be computed from available
# information (and pray for them to have a quasi-normal
# distribution). Otherwise, LDA will not perform correctly.
# Actually, mean(acc) equals 0.348 if the model considers 
# solely the "numeric" variables, i.e. Course+Instructor+Size,
# meaning a loss of less than 5% in accuracy.


# Fill in the table in the document
Acc.lda = matrix(ncol = 10, nrow = 10)
rownames(Acc.lda) = paste("K", 1:10, sep = "")
colnames(Acc.lda) = 
  c(paste("Acc", 1:9, sep = ""), "Acc_mean")
Kappa.lda = matrix(ncol = 10, nrow = 10)
rownames(Kappa.lda) = paste("K", 1:10, sep = "")
colnames(Kappa.lda) = 
  c(paste("Kappa", 1:9, sep = ""), "Kappa_mean")
for (m in 1:9) {
  myAcc   = rep(0, 10)
  myKappa = rep(0, 10)
  for (i in 1:10) {
    lda.fit = lda(
      Class ~ .,
      data = tae.tra[[i]]
    )
    lda.pred = predict(lda.fit, tae.tst.dt[[i]])
    
    cm = confusionMatrix(
      table(lda.pred$class, tae.tst.cl[[i]])
    )
    myAcc[i]   = cm$overall[1]
    myKappa[i] = cm$overall[2]
  }
  Acc.lda[,m]   = myAcc
  Kappa.lda[,m] = myKappa
}
Acc.lda[,10] = rowMeans(Acc.lda[,1:9])
Kappa.lda[,10] = rowMeans(Kappa.lda[,1:9])


## Some plots
partimat(
  Class~Size+Instructor, 
  data = tae.tra[[1]],
  method = "lda"
)
partimat(
  Class~Size+Course,
  data = tae.tra[[1]],
  method = "lda"
)
partimat(
  Class~Instructor+Course,
  data = tae.tra[[1]],
  method = "lda"
)



#########################################
# Quadratic Discriminant Analysis (QDA) #
#########################################
qda.fit = qda(
  Class ~ .,
  data = tae.tra[[1]]
)

qda.pred = predict(qda.fit, tae.tst.dt[[1]])

table(qda.pred$class, tae.tst.cl[[1]])

# Do it for every dataset in k-fold CV
acc = rep(0, 10)
for (i in 1:10) {
  qda.fit = qda(
    Class ~ .,
    data = tae.tra[[i]]
  )
  qda.pred = predict(qda.fit, tae.tst.dt[[i]])
  
  acc[i] = accuracy(
    table(qda.pred$class, tae.tst.cl[[i]])
  )
}
Acc.QDA = data.frame(fold = 1:10, Acc = acc)

mean(Acc.QDA$Acc)

# QDA performs poorly if every variable is considered.


# Fill in the table in the document
Acc.qda = matrix(ncol = 10, nrow = 10)
rownames(Acc.qda) = paste("K", 1:10, sep = "")
colnames(Acc.qda) = 
  c(paste("Acc", 1:9, sep = ""), "Acc_mean")
Kappa.qda = matrix(ncol = 10, nrow = 10)
rownames(Kappa.qda) = paste("K", 1:10, sep = "")
colnames(Kappa.qda) = 
  c(paste("Kappa", 1:9, sep = ""), "Kappa_mean")
for (m in 1:9) {
  myAcc   = rep(0, 10)
  myKappa = rep(0, 10)
  for (i in 1:10) {
    qda.fit = qda(
      Class ~ .,
      data = tae.tra[[i]]
    )
    qda.pred = predict(qda.fit, tae.tst.dt[[i]])
    
    cm = confusionMatrix(
      table(qda.pred$class, tae.tst.cl[[i]])
    )
    myAcc[i]   = cm$overall[1]
    myKappa[i] = cm$overall[2]
  }
  Acc.qda[,m]   = myAcc
  Kappa.qda[,m] = myKappa
}
Acc.qda[,10] = rowMeans(Acc.qda[,1:9])
Kappa.qda[,10] = rowMeans(Kappa.qda[,1:9])


## Some plots
partimat(
  Class~Size+Instructor, 
  data = tae.tra[[1]], 
  method = "qda"
)
partimat(
  Class~Size+Course, 
  data = tae.tra[[1]], 
  method = "qda"
)
partimat(
  Class~Instructor+Course, 
  data = tae.tra[[1]], 
  method = "qda"
)



########################
# Algorithm comparison #
########################

# Acc and Kappa for every k-fold
Acc = cbind(Acc.knn[,10], Acc.lda[,10], Acc.qda[,10])
Acc = rbind(Acc, mean = colMeans(Acc))
colnames(Acc) = c("kNN", "LDA", "QDA")
Kappa = cbind(Kappa.knn[,10], Kappa.lda[,10], Kappa.qda[,10])
Kappa = rbind(Kappa, mean = colMeans(Kappa))
colnames(Kappa) = c("kNN", "LDA", "QDA")

# Check whether there is a method outperforming the others
friedman.test(as.matrix(Acc))

## Low p-value clearly shows that there's a method 
## outperforming the others

groups = rep(1:ncol(Acc), each = nrow(Acc))
pairwise.wilcox.test(
  Acc, 
  groups, 
  p.adjust.method = "holm", 
  paired = TRUE
)

# There are significant differences amongst the three methods,
# a plot will clarify it

myAcc = as.data.frame(cbind(Acc, k.fold = 1:11))[1:10,]
myData = melt.data.frame(
  data = myAcc,
  id.vars = c("k.fold"),
  variable_name = "Method"
)
names(myData)[3] = "Acc"
ggplot(myData, aes(x = k.fold, y = Acc, fill = Method)) + 
  geom_col(position = "dodge")

myData = data.frame(Measure = c("Acc.mean", "Kappa.mean"))
myData = cbind(myData, rbind(t(Acc[11,]), t(Kappa[11,])))
myData = melt.data.frame(
  data = as.data.frame(myData),
  id.vars = "Measure",
  variable_name = "Method"
)
ggplot(myData, aes(x = Method, y = value, fill = Method)) + 
  geom_col(position = "dodge") + ylab("") +
  facet_wrap(~Measure, nrow = 1)
