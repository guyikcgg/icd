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

# Get the data types of every field
lapply(abalone, class)

# Check if there are missing values
colSums(is.na(abalone))

# Get the statistical measurements (numeric vars)
# and save them into a.s.m 
# (abalone.statistical.measurements matrix)
abalone2 = subset(abalone, select = -Sex)

a.s.m = matrix(
  nrow = ncol(abalone2),
  ncol = 8
)
colnames(a.s.m) = c(
  "Min.", 
  "1st Qu.", 
  "Median", 
  "Mean", 
  "3rd Qu.", 
  "Max.", 
  "Var.", 
  "SD"
)
rownames(a.s.m) = names(abalone2)

a.s.m[,1] = 
  sapply(abalone2, min)             # Minimum
a.s.m[,2] =
  sapply(abalone2, quantile, 0.25)  # 1st Quart.
a.s.m[,3] =
  sapply(abalone2, median)          # Median
a.s.m[,4] =
  sapply(abalone2, mean)            # Mean
a.s.m[,5] =
  sapply(abalone2, quantile, 0.75)  # 3rd Quart.
a.s.m[,6] =
  sapply(abalone2, max)             # Max

a.s.m[,7] =
  sapply(abalone2, var)             # Variance
a.s.m[,8] =
  sapply(abalone2, sd)              # Std. dev.

# Get summarized information
str(abalone)      # Structure
summary(abalone)  # Statistical measurements

# Get correlation matrix
cor(abalone2)

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


abalone.reshaped = melt(abalone[abalone$Height<0.3,]) # Remove outlayers
ggplot(abalone.reshaped, aes(value, colour = Sex)) + geom_freqpoly(bins = 29) + facet_wrap( ~ variable, ncol = 2, scales = "free")
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


myData = melt.data.frame(abalone[abalone$Height<0.3,], id.vars=c("Sex", "Rings"))
myData[myData$variable=="Height",]$value = jitter(myData[myData$variable=="Height",]$value, factor = 3)
ggplot(myData, aes(x = value, y = Rings)) + geom_jitter(alpha = 0.03) + facet_wrap( ~ variable, ncol = 2, scales = "free")
