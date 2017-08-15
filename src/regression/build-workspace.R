################################################
#      Introducción a la Ciencia de Datos      #
#     FUNCIONES Y VARIABLES PARA REGRESIÓN     #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Load required libraries
library(utils)
library(stats)
library(foreign)
library(ggplot2)
library(reshape)
library(kknn)

# ABALONE

# Load the dataset and provide it with the
# structure from Keel
abalone = read.csv(
  "datasets/abalone/abalone.dat",
  comment.char="@"
)
names(abalone) = c(
  "Sex",
  "Length",
  "Diameter",
  "Height",
  "Whole_weight",
  "Shucked_weight",
  "Viscera_weight",
  "Shell_weight",
  "Rings"
)
abalone$Sex = factor(
  abalone$Sex,
  levels = c(1, 2, 3),
  labels = c("M", "F", "I")
)

# Load traning data and test data
abalone.tra = list()
abalone.tst = list()
for (i in 1:5) {
  for (j in 1:2) {
    filename = paste(
      "datasets/abalone/abalone-5-",
      as.character(i),
      ifelse(j==1, "tra", "tst"),
      ".dat",
      sep = ""
    )
    x = read.csv(
      filename,
      comment.char="@"
    )
    names(x) = names(abalone)
    x$Sex = factor(
      x$Sex,
      levels = c(1, 2, 3),
      labels = c("M", "F", "I")
    )
    if(j==1) {
      abalone.tra[[i]] = x
    } else {
      abalone.tst[[i]] = x
    }
  }
}


# FUNCTIONS

# Run k-fold cross validation on LM fit
run_lm_fold = function(i, tra = abalone.tra, tst = abalone.tst, model = Rings~.,  tt = "test") {
  x_tra = tra[[i]]
  x_tst = tst[[i]]
  
  if (tt != "test") {
    test = x_tra
  } else {
    test = x_tst
  }
  
  # Perform LM fit
  lm.fit = lm(model, x_tra)
  output.var = as.character(model[2])
  
  # Get MSE Error
  yprime = predict(lm.fit, test)
  sum(abs(test[,output.var]-yprime)^2)/length(yprime)
}


run_knn_fold = function(i, tra = abalone.tra, tst = abalone.tst, model = Rings~.,  tt = "test", ...) {
  x_tra = tra[[i]]
  x_tst = tst[[i]]
  
  if (tt != "test") {
    test = x_tra
  } else {
    test = x_tst
  }
  
  # Perform knn fit
  knn.fit = kknn(model, x_tra, test, ...)
  output.var = as.character(model[2])
  
  # Get MSE Error
  yprime = knn.fit$fitted.values
  sum(abs(test[,output.var]-yprime)^2)/length(yprime)
}

# Add non linearities to abalone training/test set
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

rm(i, j, filename, x)

