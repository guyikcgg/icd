################################################
#      Introducción a la Ciencia de Datos      #
#   FUNCIONES Y VARIABLES PARA CLASIFICACIÓN   #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Load required libraries
library(utils)
library(stats)
library(foreign)
library(ggplot2)
library(reshape)
library(GGally)
library(class)
library(caret)
library(MASS)
library(plyr)

#TAE

# Load the dataset and provide it with the 
# structure from Keel
tae = read.csv(
  "datasets/tae/tae.dat", 
  comment.char="@"
)
names(tae) = c(
  "Native", 
  "Instructor", 
  "Course", 
  "Semester", 
  "Size", 
  "Class"
)


tae$Class      = factor(
  tae$Class,
  levels = c(1,2,3),
  labels = c("low", "medium", "high")
)
tae.original = tae
tae$Native     = factor(
  tae$Native, 
  levels = c(1,2), 
  labels = c(
    "English speaker", 
    "non-English speaker"
  )
)
tae$Semester   = factor(
  tae$Semester, 
  levels = c(1,2), 
  labels = c("Summer", "Regular")
)
tae$Instructor = factor(tae$Instructor)
tae$Course     = factor(tae$Course)


# Load traning data and test data
tae.tra = list()
tae.tra.cl = list()
tae.tra.dt = list()
tae.tst = list()
tae.tst.cl = list()
tae.tst.dt = list()
for (i in 1:10) {
  for (j in 1:2) {
    filename = paste(
      "datasets/tae/tae-10-",
      as.character(i),
      ifelse(j==1, "tra", "tst"),
      ".dat",
      sep = ""
    )
    x = read.csv(
      filename,
      comment.char="@"
    )
    names(x) = names(tae)
    # Everything except the Class will be 
    # treated as numeric for classification
    x$Class      = factor(
      x$Class,
      levels = c(1,2,3),
      labels = c("low", "medium", "high")
    )
    
    if(j==1) {
      tae.tra[[i]]    = x
      tae.tra.dt[[i]] = subset(x, select = -Class)
      tae.tra.cl[[i]] = x$Class
    } else {
      tae.tst[[i]]    = x
      tae.tst.dt[[i]] = subset(x, select = -Class)
      tae.tst.cl[[i]] = x$Class
    }
  }
}


# FUNCTIONS
normalize = function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

normalize.tae = function(df) {
  df$Instructor = df$Instructor*4
  df$Course     = df$Course*4
  df$Native     = df$Native*4
  df$Semester   = df$Semester
  df$Size       = normalize(df$Size)
  
  return(df)
}

# Accuracy function
accuracy = function(ConfusionMatrix) {
  return(sum(diag(ConfusionMatrix))/sum(ConfusionMatrix))
  
  meanAccuracy     = 0
  for (k in 1:ncol(ConfusionMatrix)) {
    TP = ConfusionMatrix[k,k]
    FP = sum(ConfusionMatrix[k,]) - TP
    FN = sum(ConfusionMatrix[,k]) - TP
    TN = sum(ConfusionMatrix[-k,-k])
    Accuracy = (TP+TN)/(TP+TN+FP+FN)
    
    meanAccuracy = meanAccuracy + Accuracy/ncol(ConfusionMatrix)
  }
  return(meanAccuracy)
}

set.seed(1)

rm(i, j, filename, x)