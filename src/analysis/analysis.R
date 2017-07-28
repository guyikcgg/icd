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

# Get summarized information
str(tae)      # Structure
summary(tae)  # Statistical momenta

# By taking a look to the file, it is clear that 
# the output variable is Class, thus we have to 
# classify the data by this category
























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


# Get the data type and dimension
class(abalone)
dim(abalone)

# Get summarized information
str(abalone)      # Structure
summary(abalone)  # Statistical momenta

# By taking a look to the file, it is clear that 
# the output variable is Class, thus we have to 
# classify the data by this category

ggplot(abalone, aes(x = Sex)) + geom_bar(stat = "count")

meltData = melt.data.frame(data = abalone[,2:8])
ggplot(meltData, aes(factor(variable), value)) + geom_boxplot() + coord_flip()

ggplot(abalone, aes(Sex, Rings)) + geom_boxplot()

ggplot(abalone, aes(Rings)) + geom_histogram(bins = 29)

ggplot(abalone, aes(Shell_weight)) + geom_histogram(bins = 29)

ggplot(abalone, aes(Shell_weight,   colour = Sex)) + geom_freqpoly(bins = 29)
ggplot(abalone, aes(Viscera_weight, colour = Sex)) + geom_freqpoly(bins = 29)
ggplot(abalone, aes(Shucked_weight, colour = Sex)) + geom_freqpoly(bins = 29)
ggplot(abalone, aes(Whole_weight,   colour = Sex)) + geom_freqpoly(bins = 29)
ggplot(abalone, aes(Height,         colour = Sex)) + geom_freqpoly(bins = 29)
ggplot(abalone, aes(Diameter,       colour = Sex)) + geom_freqpoly(bins = 29)
ggplot(abalone, aes(Length,         colour = Sex)) + geom_freqpoly(bins = 29)
ggplot(abalone, aes(Rings,          colour = Sex)) + geom_freqpoly(bins = 29)
# Histograms show that Male-Female distinction is not relevant, and does not characterize any of the above variables.
# Nonetheless, Infants' dimensions are usually lower, as it is their age.
# Statistical test is needed to confirm this statement.


ggplot(abalone, aes(x = Shell_weight,   y = Rings)) + geom_jitter(alpha = 0.2)
ggplot(abalone, aes(x = Viscera_weight, y = Rings)) + geom_jitter(alpha = 0.2)
ggplot(abalone, aes(x = Shucked_weight, y = Rings)) + geom_jitter(alpha = 0.2)
ggplot(abalone, aes(x = Whole_weight,   y = Rings)) + geom_jitter(alpha = 0.2)
ggplot(abalone, aes(x = Diameter,       y = Rings)) + geom_jitter(alpha = 0.2)
ggplot(abalone, aes(x = Length,         y = Rings)) + geom_jitter(alpha = 0.2)

# Remove outlayers before plotting Height
myData = abalone[abalone$Height<0.3,]
ggplot(myData, aes(x = Height, y = Rings)) + geom_jitter(alpha = 0.2)