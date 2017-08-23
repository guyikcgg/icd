################################################
#      Introducción a la Ciencia de Datos      #
#              ANÁLISIS DE DATOS               #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Build the workspace (load variables and functions)
source("../regression/build-workspace.R")


# Get the data type and dimension
class(abalone)
dim(abalone)

# Get the data types of every field
lapply(abalone, class)

# Check if there are missing values
colSums(is.na(abalone))

# Get the statistical measurements
# (numeric variables) and save them into a.s.m
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

a.s.m[,1] = sapply(abalone2, min)             # Minimum
a.s.m[,2] = sapply(abalone2, quantile, 0.25)  # 1st Quart.
a.s.m[,3] = sapply(abalone2, median)          # Median
a.s.m[,4] = sapply(abalone2, mean)            # Mean
a.s.m[,5] = sapply(abalone2, quantile, 0.75)  # 3rd Quart.
a.s.m[,6] = sapply(abalone2, max)             # Max
a.s.m[,7] = sapply(abalone2, var)             # Variance
a.s.m[,8] = sapply(abalone2, sd)              # Std. dev.

# Get the statistical measurements
# (categorical variables)
a.s.m2 = as.data.frame( table(abalone$Sex) )
names(a.s.m2)[1] = "Sex"
a.s.m2$relFreq = a.s.m2$Freq/nrow(abalone)

# Get summarized information
str(abalone)      # Structure
summary(abalone)  # Statistical measurements

# Get correlation matrix
cor.matrix = cor(abalone2)

# By taking a look to the file, it is clear that
# the output variable is Class, thus we have to
# classify the data by this category

# PLOTS

# Categorical variables (barplots)
ggplot(abalone, aes(x = Sex, fill = Sex)) +
  geom_bar(stat = "count")

# Numeric variables Boxplots
meltData1 = melt.data.frame(data = abalone[,2:4])
meltData1$Measure = "length (mm)"

meltData2 = melt.data.frame(data = abalone[,5:8])
meltData2$Measure = "weight (grams)"

meltData = rbind(meltData1, meltData2)
ggplot(meltData, aes(factor(variable), value)) +
  xlab("") + ylab("") + geom_boxplot() + coord_flip() +
  facet_wrap( ~ Measure, ncol=1, scales="free")

# Distribution of rings
ggplot(abalone, aes(Rings, fill = Sex, color = Sex)) + 
  geom_freqpoly(bins = 29)
ggplot(abalone, aes(Sex, Rings, fill = Sex)) + geom_boxplot()
ggplot(abalone, aes(Rings, fill = Sex, color = Sex)) + 
  stat_ecdf() + ylab("CDF")
ggplot(abalone, aes(Rings, fill = Sex, color = Sex)) + 
  stat_density(alpha = .8)

# Distribution of numeric variables against sex
# Remove outlayers
abalone.reshaped = melt(abalone[abalone$Height<0.3,])
ggplot(abalone.reshaped, aes(value, colour = Sex)) +
  geom_freqpoly(bins = 29) +
  facet_wrap( ~ variable, ncol = 2, scales="free")

# Histograms show that Male-Female distinction is not relevant, 
# and does not characterize any of the above variables.
# Nonetheless, Infants' dimensions are usually lower,
# as is their age.
# Statistical test is needed to confirm this statement.


# Scatterplots
myData = melt.data.frame(
  abalone[abalone$Height<0.3,],
  id.vars=c("Sex", "Rings")
)
myData[myData$variable=="Height",] =
  within(myData[myData$variable=="Height",], {
    value = jitter(value, factor = 3)
  }
)
ggplot(myData, aes(x = value, y = Rings)) + 
  geom_jitter(alpha = 0.03) + 
  facet_wrap( ~ variable, ncol = 2, scales = "free")


# Check whether the Rings distributions are the same
mean(abalone[abalone$Sex=="F",]$Rings)
summary(abalone[abalone$Sex=="F",]$Rings)
mean(abalone[abalone$Sex=="M",]$Rings)
summary(abalone[abalone$Sex=="M",]$Rings)

ks.test(
  abalone[abalone$Sex=="M",]$Rings,
  abalone[abalone$Sex=="F",]$Rings
)
# In the studied sample, there are more old females than 
# old males. Actually, the distributions can be proved not
# to be the same.
# This can indicate that females usually live longer than males.
# This fact should be taken into account to test if there is 
# any secondary sex charasteristic, i.e. it does not suffice to 
# test wheter length distribution in females is the same than 
# length distribution in males, it is necessary to test this 
# taking age into account.
# From the minimum age of both female and male, it can be 
# suspected that primary sex characteristics are easier 
# detected on males.
