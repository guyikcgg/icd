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
tae.tst = list()
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
    x$Class      = factor(
      x$Class,
      levels = c(1,2,3),
      labels = c("low", "medium", "high")
    )
    x$Native     = factor(
      x$Native, 
      levels = c(1,2), 
      labels = c(
        "English speaker", 
        "non-English speaker"
      )
    )
    x$Semester   = factor(
      x$Semester, 
      levels = c(1,2), 
      labels = c("Summer", "Regular")
    )
    x$Instructor = factor(x$Instructor)
    x$Course     = factor(x$Course)
    
    if(j==1) {
      tae.tra[[i]] = x
    } else {
      tae.tst[[i]] = x
    }
  }
}


# FUNCTIONS
normalize = function(x) {
  return((x-min(x))/(max(x)-min(x)))
}