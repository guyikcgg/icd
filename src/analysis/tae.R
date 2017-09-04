################################################
#      Introducción a la Ciencia de Datos      #
#              ANÁLISIS DE DATOS               #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Build the workspace
source("classification/build-workspace.R")


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
summary(tae.original)

# By taking a look to the file, it is clear that 
# the output variable is Class, thus we have to 
# classify the data by this category


# Visualize numerical variables
myData = melt.data.frame(
  data = subset(tae, select = c(Class, Size, Native, Semester)),
  id.vars = c("Class", "Native", "Semester")
)

# Boxplots of numeric variables
ggplot(myData, aes(factor(variable), value, fill = Class)) +
  xlab("") + ylab("Size") + geom_boxplot() + coord_flip()

# Distributions of Size
ggplot(myData, aes(value)) +
  geom_freqpoly(bins = 8, aes(colour = Class)) +
  xlab("Size") + xlim(c(0,70))

ggplot(myData, aes(value)) +
  geom_freqpoly(bins = 8, aes(colour = Semester)) +
  xlab("Size") + xlim(c(0,70))
ggplot(myData, aes(value)) +
  geom_freqpoly(bins = 8, aes(colour = Native)) +
  xlab("Size") + xlim(c(0,70))
ks.test(
  subset(tae, subset = Semester=="Summer")$Size,
  subset(tae, subset = Semester=="Regular")$Size
)
ks.test(
  subset(tae, subset = Native=="English speaker")$Size,
  subset(tae, subset = Native=="non-English speaker")$Size
)

# Visualize pseudo-numeric variables
myData = melt.data.frame(
  data = subset(
    tae.original, 
    select = c(Class, Course, Instructor)
  ),
  id.vars = "Class"
)
ggplot(myData, aes(factor(variable), value, fill = Class)) +
  xlab("") + ylab("number") + geom_boxplot() + coord_flip()
ggplot(myData, aes(value, colour = Class)) +
  geom_freqpoly(bins = 10) + xlab("number") + 
  facet_wrap(~ variable, ncol = 1)


#######################
# Barplots of factors #
#######################

## Native & Semester
myData = melt.data.frame(
  data = subset(tae, select = c(-Size, -Instructor, -Course)),
  id.vars = "Class"
)

ggplot(myData, aes(value, fill = Class)) +
  geom_bar(stat = "count", position = "dodge") + 
  xlab("") +
  facet_wrap(~ variable, ncol = 2, scales = "free")


## Class
ggplot(tae, aes(Class, fill = Class)) +
  geom_bar(stat = "count")


## Course
freq = ave(rep(1, times=nrow(tae)), tae$Course, FUN=sum)
myData = tae[order(freq, tae$Course, decreasing = TRUE), ]
myData = melt.data.frame(
  data = subset(myData, select = c(Class, Course)),
  id.vars = "Class"
)

ggplot(myData, aes(value, fill = Class)) +
  geom_bar(stat = "count", position = "dodge") + 
  xlab("Course") +
  scale_x_discrete(
    limits = as.factor(
      order(
        count(tae, "Course")$freq,
        decreasing = T
      )
    )
  )


myData = as.data.frame(table(tae$Class, tae$Course))
names(myData) = c("Class", "Course", "freq")
freq = ave(myData$freq, myData$Course, FUN = sum)
myData = myData[order(freq, myData$Course, decreasing = TRUE), ]
ggplot(myData, aes(x = Course, y = freq, color = Class)) +
  geom_point() + 
  geom_line(aes(group = Class)) +
  scale_x_discrete(limits = myData$Course) +
  ylab("Frequency")

myData$rel.freq = 
  myData$freq/freq[order(freq, myData$Course, decreasing = T)]
ggplot(myData, aes(x = Course, y = rel.freq, fill = Class)) +
  geom_col(width = 2.5) +
  scale_x_discrete(limits = myData$Course) +
  ylab("Relative frequency")


## Instructor
myData = as.data.frame(table(tae$Class, tae$Instructor))
names(myData) = c("Class", "Instructor", "freq")
freq = ave(myData$freq, myData$Instructor, FUN = sum)
myData = 
  myData[order(freq, myData$Instructor, decreasing = T), ]
myData[myData$freq==0,]$freq = 0.03
ggplot(myData, aes(x = Instructor, y = freq))  + 
  geom_col(width = 2, position = "dodge", aes(fill = Class)) +
  scale_x_discrete(limits = myData$Instructor) +
  ylab("Frequency")

myData[myData$freq==0.03,]$freq = 0
myData$rel.freq = 
  myData$freq/freq[order(freq, myData$Instructor, decreasing = T)]
ggplot(myData, aes(x = Instructor, y = rel.freq, fill = Class)) +
  geom_col(width = 2.5) +
  scale_x_discrete(limits = myData$Instructor) +
  ylab("Relative frequency")

ggplot(tae, aes(as.integer(Instructor), color = Class)) +
  geom_density()
ggplot(tae, aes(as.integer(Course), color = Class)) +
  geom_density()
