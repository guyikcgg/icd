################################################
#      Introducción a la Ciencia de Datos      #
#              ANÁLISIS DE DATOS               #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Load required libraries
library(utils)
library(stats)
library(foreign)

# TAE

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


# Get the data type and dimension
class(tae)
dim(tae)

# Get the data types of every field
lapply(tae, class)

# Get summarized information
str(tae)      # Structure
summary(tae)  # Statistical momenta

# By taking a look to the file, it is clear that 
# the output variable is Class, thus we have to 
# classify the data by this category
























# AVALONE

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


# Get the data type and dimension
class(abalone)
dim(abalone)

# Get summarized information
str(abalone)      # Structure
summary(abalone)  # Statistical momenta

# By taking a look to the file, it is clear that 
# the output variable is Class, thus we have to 
# classify the data by this category

