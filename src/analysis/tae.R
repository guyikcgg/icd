################################################
#      Introducción a la Ciencia de Datos      #
#              ANÁLISIS DE DATOS               #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Build the workspace
source("../classification/build-workspace.R")


# Get the data type and dimension
class(tae)
dim(tae)

# Get the data types of every field
lapply(tae, class)

# Check if there are missing values
colSums(is.na(tae))

# Get the statistical measurements (numeric variables)
attach(tae)
min       (Size)        # Minimum
quantile  (Size, 0.25)  # 1st Quartile
median    (Size)        # Median
mean      (Size)        # Mean
quantile  (Size, 0.75)  # 3rd Quartile
max       (Size)        # Max

var       (Size)        # Variane
sd        (Size)        # Standard deviation
detach(tae)

# Get summarized information
str(tae)      # Structure
summary(tae)  # Statistical measurements

# By taking a look to the file, it is clear that 
# the output variable is Class, thus we have to 
# classify the data by this category


# Visualize numerical variables
myData = melt.data.frame(
  data = subset(tae, select = c(Class, Size)),
  id.vars = "Class"
)

# Boxplots of numeric variables
ggplot(myData, aes(factor(variable), value)) +
  xlab("") + ylab("") + geom_boxplot() + coord_flip() +
  facet_wrap( ~ Class, ncol=1)

# Distributions of Size
ggplot(myData, aes(value, colour = Class)) +
  geom_freqpoly(bins = 7) + xlab("Size")


# Barplots of factors
myData = melt.data.frame(
  data = subset(tae, select = c(-Size, -Instructor, -Course)),
  id.vars = "Class"
)

ggplot(myData, aes(value, fill = Class)) +
  geom_bar(stat = "count", position = "dodge") + 
  facet_wrap(~ variable, ncol = 1, scales = "free")



myData = melt.data.frame(
  data = subset(tae, select = c(Class, Course, Instructor)),
  id.vars = "Class"
)

ggplot(myData, aes(value, fill = Class)) +
  geom_bar(stat = "count") + 
  facet_wrap(~ variable, ncol = 1, scales = "free")



myData = melt.data.frame(
  data = subset(tae, select = c(Class, Course)),
  id.vars = "Class"
)
ggplot(myData, aes(value, fill = Class)) +
  geom_bar(stat = "count", width = 0.9) + 
  facet_wrap(~ value, ncol = 7, scales = "free") +
  xlab("Course")



myData = melt.data.frame(
  data = subset(tae, select = c(Class, Instructor)),
  id.vars = "Class"
)
ggplot(myData, aes(value, fill = Class)) +
  geom_bar(stat = "count", width = 0.9) + 
  facet_wrap(~ value, ncol = 7, scales = "free") +
  xlab("Instructor")

ggplot(tae, aes(as.integer(Instructor), color = Class)) +
  geom_density()
ggplot(tae, aes(as.integer(Course), color = Class)) +
  geom_density()

# Data is heterogeneous



ggplot(tae, aes(x = Course, y = Instructor, color = Class)) +
  geom_point()
