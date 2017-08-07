################################################
#      Introducción a la Ciencia de Datos      #
#               REGRESIÓN k-NN                 #
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

# FUNCTIONS

run_knn_fold = function(i, file.name, model = Y~.,  tt = "test", n = 5) {
  # Open files
  file.name = paste("datasets", file.name, file.name, sep = "/")

  file = paste(file.name, "-", as.character(n), "-", i, "tra.dat", sep = "")
  x_tra = read.csv(file, comment.char = "@")
  file = paste(file.name, "-", as.character(n), "-", i, "tst.dat", sep = "")
  x_tst = read.csv(file, comment.char = "@")
  ln = length(names(x_tra))-1

  # Give names
  names(x_tra)[1:ln] = paste("X", 1:ln, sep = "")
  names(x_tra)[ln+1] = "Y"
  names(x_tst)[1:ln] = paste("X", 1:ln, sep = "")
  names(x_tst)[ln+1] = "Y"

  if (tt == "train") {
    test = x_tra
  } else {
    test = x_tst
  }

  fitMulti = kknn(model, x_tra, x_tst)

  # Get MSE Error
  yprime = predict(fitMulti, test)
  sum(abs(test$Y-yprime)^2)/length(yprime)
}
