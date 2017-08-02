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
library(ggplot2)
library(reshape)

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

# Check if there are missing values
colSums(is.na(tae))

# Get the statistical measurements (numeric variables)
min       (tae$Size)        # Minimum
quantile  (tae$Size, 0.25)  # 1st Quartile
median    (tae$Size)        # Median
mean      (tae$Size)        # Mean
quantile  (tae$Size, 0.75)  # 3rd Quartile
max       (tae$Size)        # Max

var       (tae$Size)        # Variane
sd        (tae$Size)        # Standard deviation

# Get summarized information
str(tae)      # Structure
summary(tae)  # Statistical measurements

# By taking a look to the file, it is clear that 
# the output variable is Class, thus we have to 
# classify the data by this category
