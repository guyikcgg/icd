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

# Some plots
#plot(tae, col = tae$Class)
tae2 = tae
tae2$Instructor = as.integer(tae$Instructor)
tae2$Course = as.integer(tae$Course)
#ggpairs(tae2, aes(color = Class, alpha = 0.3))



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

ggplot(myData, aes(x = k, y = value, color = variable, size = lineSize)) + 
  geom_line() + scale_size(range = c(0.5, 2), guide="none") + ylab("Accuracy")
## It seems that k=1 performs the best, with Acc = 0.61
# This plot could also be done with training data, to test for overfitting


# Fill in the table in the document
Acc = matrix(ncol = 10, nrow = 10)
rownames(Acc) = paste("K", 1:10, sep = "")
colnames(Acc) = c(paste("Acc", 1:9, sep = ""), "Acc_mean")
Kappa = matrix(ncol = 10, nrow = 10)
rownames(Kappa) = paste("K", 1:10, sep = "")
colnames(Kappa) = c(paste("Kappa", 1:9, sep = ""), "Kappa_mean")
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
    Acc[,m]   = myAcc
    Kappa[,m] = myKappa
}
Acc[,10] = rowMeans(Acc[,1:9])
Kappa[,10] = rowMeans(Kappa[,1:9])


######################################
# Linear Discriminant Analysis (LDA) #
######################################

# Check for normality of (pseudo-)numeric variables
sapply(tae.original[1:5], shapiro.test)

myData = melt.data.frame(
  data = tae.original,
  measure.vars = c("Native", "Instructor", "Course", "Semester", "Size")
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


library(ISLR)
library(klaR)
plot(lda.fit)

partimat(Class~Size+Instructor, data = tae.tra[[1]], method = "lda")
partimat(Class~Size+Course, data = tae.tra[[1]], method = "lda")
partimat(Class~Instructor+Course, data = tae.tra[[1]], method = "lda")



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


library(ISLR)
library(klaR)
plot(qda.fit)

partimat(Class~Size+Instructor, data = tae.tra[[1]], method = "qda")
partimat(Class~Size+Course, data = tae.tra[[1]], method = "qda")
partimat(Class~Instructor+Course, data = tae.tra[[1]], method = "qda")



# Algorithm comparison


