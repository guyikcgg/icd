################################################
#      Introducción a la Ciencia de Datos      #
#                CLASIFICACIÓN                 #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Build the workspace
source("classification/build-workspace.R")

# Some info about the classes
round(prop.table(table(tae$Class)) * 100, digits = 1)

unfold.tae = function(df) {
  Instructor = list()
  Course     = list()
  
  Instructor = matrix(ncol = 25, nrow = nrow(df), data = 0)
  for (i in 1:nrow(df)) {
    j = df[i, "Instructor"]
    Instructor[i,j] = 1
  }
  Instructor = as.data.frame(Instructor)
  names(Instructor) = paste("Instructor", 1:25, sep = "")
  
  Course = matrix(ncol = 26, nrow = nrow(df), data = 0)
  for (i in 1:nrow(df)) {
    j = df[i, "Course"]
    Course[i,j] = 1
  }
  Course = as.data.frame(Course)
  names(Course) = paste("Course", 1:26, sep = "")

  df = subset(df, select = c(-Course, -Instructor))
  df = cbind(df, Course, Instructor)

  return(df)
}

tae.tra = lapply(tae.tra, unfold.tae)
tae.tst = lapply(tae.tst, unfold.tae)

tae.tra.dt = lapply(tae.tra.dt, unfold.tae)
tae.tst.dt = lapply(tae.tst.dt, unfold.tae)

tae.tra.dt$Size = normalize(tae.tra.dt$size)
tae.tst.dt$Size = normalize(tae.tst.dt$size)

# Normalize numeric data
#tae$Size = normalize(tae$Size)
#tae.tra.dt = lapply(tae.tra.dt, normalize.tae)
#tae.tst.dt = lapply(tae.tst.dt, normalize.tae)

# Some plots
#plot(tae, col = tae$Class)
tae2 = tae
tae2$Instructor = as.integer(tae$Instructor)
tae2$Course = as.integer(tae$Course)
#ggpairs(tae2, aes(color = Class, alpha = 0.3))


# Apply k-NN algorithm to the first set
tae_test_pred = knn(
  train = tae.tra.dt[[1]], # Dataframe
  test  = tae.tst.dt[[1]], # Dataframe
  cl    = tae.tra.cl[[1]], # Vector (factor)
  k     = 21
)

# See the results in a contingency table (confusion matrix)
table(tae_test_pred, tae.tst.cl[[1]])

# Apply k-NN algorithm to every dataset and check the 
# overall accuracy for several values of k
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
## It seems that k=17 performs the best, with Acc = 0.55
# This plot could also be done with training data, to test for overfitting


# Linear Discriminant Analysis (LDA)
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



# Quadratic Discriminant Analysis (QDA)
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
# However, there are not real numeric variables, only
# ordered factors, with no normal distribution.
# Other variables should be computed from available
# information (and pray for them to have a quasi-normal
# distribution). Otherwise, QDA will not perform correctly.
# Actually, mean(acc) equals 0.348 if the model considers 
# solely the "numeric" variables, i.e. Course+Instructor+Size,
# meaning a loss of less than 5% in accuracy.


library(ISLR)
library(klaR)
plot(qda.fit)

partimat(Class~Size+Instructor, data = tae.tra[[1]], method = "qda")
partimat(Class~Size+Course, data = tae.tra[[1]], method = "qda")
partimat(Class~Instructor+Course, data = tae.tra[[1]], method = "qda")



# Algorithm comparison

